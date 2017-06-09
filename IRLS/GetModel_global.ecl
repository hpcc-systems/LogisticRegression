IMPORT ML_Core;
IMPORT ML_Core.Types AS Core_Types;
IMPORT PBblas;
IMPORT PBblas.Types AS PBblas_Types;
IMPORT $.^ AS LR;
IMPORT LR.Constants;
IMPORT LR.Types;
//Aliases for convenience
NumericField := Core_Types.NumericField;
DiscreteField:= Core_Types.DiscreteField;
Layout_Model := Core_Types.Layout_Model;
t_work_item  := Core_Types.t_work_item;
t_RecordID   := Core_Types.t_RecordID;
t_FieldNumber:= Core_Types.t_FieldNumber;
t_FieldReal  := Core_Types.t_FieldReal;
Cell := PBblas_Types.Layout_Cell;
Layout_Norm := PBblas_Types.Layout_Norm;
//
Ext_Cell := RECORD(Cell)
  REAL8 max_delta;
  UNSIGNED4 correct;
  UNSIGNED4 incorrect;
  UNSIGNED2 iterations;
END;
REAL8 Bernoulli_EV(REAL8 v) := 1.0/(1.0+exp(-v));
PBblas.Types.value_t u_i(PBblas.Types.value_t v,
        PBblas.Types.dimension_t r,
        PBblas.Types.dimension_t c) := Bernoulli_EV(v);
PBblas.Types.value_t w_i(PBblas.Types.value_t v,
        PBblas.Types.dimension_t r,
        PBblas.Types.dimension_t c) := u_i(v,r,c)*(1-u_i(v, r, c));
PBblas.Types.value_t abs_v(PBblas.Types.value_t v,
        PBblas.Types.dimension_t r,
        PBblas.Types.dimension_t c) := ABS(v);
PBblas.Types.value_t sig_v(PBblas.Types.value_t v,
        PBblas.Types.dimension_t r,
        PBblas.Types.dimension_t c) := IF(v>=0.5, 1, 0);
/**
 * Internal function to determine values for the model coefficients
 * and selected statistics from building the model.
 * @param independents the independent values
 * @param dependents the dependent values
 * @param max_iter maximum number of iterations to try
 * @param epsilon the minimum change in the Beta value estimate to continue
 * @param ridge a value to pupulate a diagonal matrix that is added to
 * a matrix help assure that the matrix is invertible.
 * @return coefficient matrix plus model building statistics
 */
EXPORT DATASET(Layout_Model)
      GetModel_global(DATASET(NumericField) independents,
               DATASET(DiscreteField) dependents,
               UNSIGNED max_iter=200,
               REAL8 epsilon=Constants.default_epsilon,
               REAL8 ridge=Constants.default_ridge) := FUNCTION
  // Convert data for PBblas, adding constant column to X
  ind_cells := PBblas.Converted.NFToMatrix(independents);
  X_in := PBblas.MatUtils.InsertCols(ind_cells, 1, 1);  //x_0 column
  Y_in := PBblas.Converted.DFToMatrix(dependents);
  // Y columns need to be dense and split into single column
  //work items wit independents replicated
  // work item re-map for multiple column dependents and replicate
  wi_info := RECORD
    t_work_item orig_wi;
    t_work_item wi;
    UNSIGNED4 col;
    UNSIGNED4 dep_cols;
    UNSIGNED4 dep_rows;
    UNSIGNED4 ind_cols;
    UNSIGNED4 ind_rows;
    UNSIGNED4 orig_col;
  END;
  dcols := TABLE(Y_in, {wi:=wi_id, Y, max_X:=MAX(GROUP, X), col:=1},
                 wi_id, Y, FEW, UNSORTED);
  dep_map := PROJECT(GROUP(SORT(dcols, wi, Y), wi),
                     TRANSFORM(RECORDOF(dcols),
                               SELF.col := COUNTER,
                               SELF:=LEFT));
  dep_cols := TABLE(dep_map,{wi,r:=MAX(GROUP,max_X), c:=MAX(GROUP,col)},
                    wi, FEW, UNSORTED);
  ind_cols := TABLE(X_in, {wi:=wi_id, r:=MAX(GROUP,X), c:=MAX(GROUP,Y)},
                    wi_id, FEW, UNSORTED);
  wi_info cmb(RECORDOF(ind_cols) ind, RECORDOF(dep_cols) dep):=TRANSFORM
    SELF.orig_wi := ind.wi;
    SELF.wi := ind.wi;
    SELF.col := 1;    // place holder
    SELF.dep_cols := dep.c;
    SELF.dep_rows := MAX(dep.r, ind.r);
    SELF.ind_cols := ind.c; // constant was inserted
    SELF.ind_rows := MAX(ind.r, dep.r);
    SELF.orig_col := 0;   // fill in after NORMALIZE
  END;
  cmb_cols := JOIN(ind_cols, dep_cols, LEFT.wi=RIGHT.wi,
                   cmb(LEFT, RIGHT), FEW);
  wi_info mark_wi(wi_info prev, wi_info curr) := TRANSFORM
    SELF.wi := IF(prev.wi=0, 1, prev.wi + prev.dep_cols);
    SELF := curr;
  END;
  marked_wi := ITERATE(SORT(cmb_cols,wi), mark_wi(LEFT, RIGHT));
  exp_wi := NORMALIZE(marked_wi, LEFT.dep_cols,
                      TRANSFORM(wi_info,
                                SELF.wi := LEFT.wi - 1 + COUNTER,
                                SELF.col := COUNTER,
                                SELF := LEFT));
  map_wi := JOIN(exp_wi, dep_map,
                 LEFT.orig_wi=RIGHT.wi AND LEFT.col=RIGHT.col,
                 TRANSFORM(wi_info,
                           SELF.orig_col:=RIGHT.Y,
                           SELF:=LEFT),
                 LOOKUP, FEW);
  X_rep := JOIN(X_in, map_wi, LEFT.wi_id=RIGHT.orig_wi,
               TRANSFORM(Cell, SELF.wi_id := RIGHT.wi, SELF := LEFT),
               LOOKUP, MANY, FEW);
  Y_rep := JOIN(Y_in, map_wi,
               LEFT.wi_id=RIGHT.orig_wi AND LEFT.Y=RIGHT.col,
               TRANSFORM(Cell,SELF.wi_id:=RIGHT.wi,SELF.Y:=1,SELF:=LEFT),
               LOOKUP, MANY, FEW);
  // generate initial values for Betas
  XtX := PBblas.gemm(TRUE, FALSE, 1.0, X_rep, X_rep);
  XtY := PBblas.gemm(TRUE, FALSE, 1.0, x_rep, Y_rep);
  LU_XtX := PBblas.getrf(XtX);
  inner_B := PBblas.trsm(PBblas_Types.Side.Ax,
                          PBblas_Types.Triangle.Lower,
                          FALSE,
                          PBblas_Types.Diagonal.UnitTri,
                          1.0, LU_XtX, XtY);
  outer_B := PBblas.trsm(PBblas_Types.Side.Ax,
                         PBblas_Types.Triangle.Upper,
                         FALSE,
                         PBblas_Types.Diagonal.NotUnitTri,
                         1.0, LU_XtX, inner_B);
  Ext_Cell prepBeta(PBblas_Types.Layout_Cell b) := TRANSFORM
    SELF.max_delta := 2*epsilon;
    SELF.iterations := 0;
    SELF.correct := 0;
    SELF.incorrect := 0;
    SELF := b;
  END;
  B_init := PROJECT(outer_B, prepBeta(LEFT));
  // generate an m element ridge vector, use Beta for convenience
  R_vector := PROJECT(B_init,TRANSFORM(Cell,SELF.v:=ridge,SELF:=LEFT));
  R := PBblas.Vector2Diag(R_vector);
  Identity_M := PROJECT(R, TRANSFORM(Cell,
                                    SELF.v := IF(LEFT.x=LEFT.y, 1, 0),
                                    SELF:=LEFT));
  // iterate Beta to get convergence
  DATASET(Ext_Cell) iter0(DATASET(Ext_Cell) B_SE, UNSIGNED iter):=FUNCTION
    B := B_SE(y=1);   // drop the shadow SE column
    XB := PBblas.gemm(FALSE, FALSE, 1.0, X_rep, B);
    W_vector := PBblas.Apply2Elements(XB, w_i);
    U_vector := PBblas.Apply2Elements(XB, u_i);
    W := PBblas.Vector2Diag(W_vector);
    XtW := PBblas.gemm(TRUE, FALSE, 1.0, X_rep, W);
    XtWX_R := PBblas.gemm(FALSE, FALSE, 1.0, XtW, X_rep, R, 1.0); //add ridge
    Y_U := PBblas.axpy(-1.0, U_vector, Y_rep);
    WXB_Y_U := PBblas.gemm(FALSE, FALSE, 1.0, W, XB, Y_U, 1.0);
    XtWXB_Y_U := PBblas.gemm(TRUE, FALSE, 1.0, X_rep, WXB_Y_U);
    LU_XtWX_R := PBblas.getrf(XtWX_R);
    intermediate := PBblas.trsm(PBblas_Types.Side.Ax,
                                PBblas_Types.Triangle.Lower,
                                FALSE,
                                PBblas_Types.Diagonal.UnitTri,
                                1.0, LU_XtWX_R, Identity_M);
    Inv_M := PBblas.trsm(PBblas_Types.Side.Ax,
                         PBblas_Types.Triangle.Upper,
                         FALSE,
                         PBblas_Types.Diagonal.NotUnitTri,
                         1.0, LU_XtWX_R, intermediate);
    new_B := PBblas.gemm(FALSE, FALSE, 1.0, inv_M,  XtWXB_Y_U);
    XB1 := PBblas.gemm(FALSE, FALSE, 1.0, X_rep, new_B);
    U1_vector := PBblas.Apply2Elements(XB1, u_i);
    Y1 := PBblas.Apply2Elements(U1_vector, sig_v);
    avp_vector := PBblas.axpy(-1.0, Y1, Y_rep);
    incorrect := PBblas.asum(avp_vector); // Layout_Norm
    score_card := RECORD
      t_work_item wi_id;
      UNSIGNED4 correct;
      UNSIGNED4 incorrect;
    END;
    score_card calc_scores(Layout_Norm l1, wi_info w) := TRANSFORM
      SELF.incorrect := l1.v;
      SELF.correct := w.dep_rows - l1.v;
      SELF.wi_id := L1.wi_id;
    END;
    scored := JOIN(incorrect, map_wi, LEFT.wi_id=RIGHT.wi,
                   calc_scores(LEFT,RIGHT), LOOKUP, FEW);
    delta_B := PBblas.Apply2Elements(PBblas.axpy(-1.0, B, new_B), abs_v);
    max_delta_B := TABLE(delta_b, {wi_id, max_v:=MAX(GROUP,v)},
                         wi_id, FEW, UNSORTED);
    stat0 := JOIN(scored, max_delta_B, LEFT.wi_id=RIGHT.wi_id, LOOKUP, FEW);
    Cell calc_SE(Cell se_cell) := TRANSFORM
      SELF.y := 2;
      SELF.v := SQRT(se_cell.v);
      SELF := se_cell;
    END;
    SE := PROJECT(Inv_M(x=y), calc_SE(LEFT));
    Ext_Cell decorate_cell(Cell cell, RECORDOF(stat0) s):=TRANSFORM
      SELF.iterations := iter;
      SELF.max_delta := s.max_v;
      SELF := s;
      SELF := cell;
    END;
    rslt := JOIN(new_B+SE, stat0, LEFT.wi_id=RIGHT.wi_id,
                 decorate_cell(LEFT,RIGHT), LOOKUP, FEW);
    RETURN rslt;
  END;
  B_calc := LOOP(B_init, epsilon<LEFT.max_delta AND max_iter>LEFT.iterations,
                 iter0(ROWS(LEFT), COUNTER));
  NumericField extractBeta(Ext_Cell cell, wi_info info) := TRANSFORM
    SELF.id := Constants.id_betas + cell.x - 1 + (cell.y-1)*info.ind_cols;
    SELF.number := 1;
    SELF.wi := cell.wi_id;
    SELF.value := cell.v;
  END;
  betas := JOIN(B_calc, map_wi, LEFT.wi_id=RIGHT.wi,
                extractBeta(LEFT, RIGHT), LOOKUP, FEW);  // first beta and SE
  NumericField extStats(Ext_Cell p, UNSIGNED fld) := TRANSFORM
    SELF.wi := p.wi_id;
    SELF.id := CHOOSE(fld, Constants.id_iters, Constants.id_delta,
                        Constants.id_correct, Constants.id_incorrect);
    SELF.number := 1;
    SELF.value := CHOOSE(fld, p.iterations, p.max_delta,
                              p.correct, p.incorrect);
  END;
  one_rec := DEDUP(B_calc, wi_id, HASH); //
  stats := NORMALIZE(one_rec, 4, extStats(LEFT, COUNTER));
  Layout_Model wi_reset(NumericField nf, wi_info w) := TRANSFORM
    SELF.wi := w.orig_wi;
    SELF.number := w.orig_col;
    SELF := nf;
  END;
  var_data := JOIN(betas+stats, map_wi, LEFT.wi=RIGHT.wi,
                   wi_reset(LEFT, RIGHT), LOOKUP, FEW);
  Layout_Model extBase(wi_info w, UNSIGNED f) := TRANSFORM
    SELF.wi := w.orig_wi;
    SELF.id := Constants.id_base;
    SELF.number := CHOOSE(f, Constants.base_builder,
                             Constants.base_max_iter,
                             Constants.base_epsilon,
                             Constants.base_ind_vars,
                             Constants.base_dep_vars,
                             Constants.base_obs);
    SELF.value := CHOOSE(f, 1,
                            max_iter,
                            epsilon,
                            w.ind_cols-1,
                            w.dep_cols,
                            MAX(w.dep_rows, w.ind_rows));
  END;
  base_data := NORMALIZE(cmb_cols, 6, extBase(LEFT, COUNTER));
  RETURN base_data + var_data;
END;
