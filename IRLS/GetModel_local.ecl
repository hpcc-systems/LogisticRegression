IMPORT ML_Core;
IMPORT ML_Core.Types AS Core_Types;
IMPORT $.^ AS LR;
IMPORT $.^.Constants;
IMPORT $.^.Types;
IMPORT $ AS IRLS;
IMPORT Std;
IMPORT Std.BLAS AS BLAS;
//Aliases for convenience
NumericField := Core_Types.NumericField;
DiscreteField:= Core_Types.DiscreteField;
Layout_Model := Core_Types.Layout_Model;
t_work_item  := Core_Types.t_work_item;
t_RecordID   := Core_Types.t_RecordID;
t_FieldNumber:= Core_Types.t_FieldNumber;
t_FieldReal  := Core_Types.t_FieldReal;
value_t      := BLAS.Types.value_t;
dimension_t  := BLAS.Types.dimension_t;
matrix_t     := BLAS.Types.matrix_t;
triangle     := BLAS.Types.triangle;
diagonal     := BLAS.Types.diagonal;
side         := BLAS.Types.side;
gemm         := BLAS.dgemm;
trsm         := BLAS.dtrsm;
getf2        := BLAS.dgetf2;
axpy         := BLAS.daxpy;
Apply2Cells  := BLAS.Apply2Cells;
make_diag    := BLAS.make_diag;
dimm         := LR.dimm;
make_vector  := BLAS.make_vector;
extract_diag := BLAS.extract_diag;
t_part       := UNSIGNED2;
//
Part := RECORD
  t_work_item wi;
  dimension_t part_rows;
  dimension_t part_cols;
  matrix_t mat;
  REAL8 max_delta := 0.0;
  UNSIGNED2 iterations := 0;
  UNSIGNED4 correct := 0;
  UNSIGNED4 incorrect := 0;
END;
Ext_NumericField := RECORD(NumericField)
  dimension_t part_rows;
  dimension_t part_cols;
END;
REAL8 Bernoulli_EV(REAL8 v) := 1.0/(1.0+exp(-v));
value_t u_i(value_t v,
            dimension_t r,
            dimension_t c) := Bernoulli_EV(v);
value_t w_i(value_t v,
            dimension_t r,
            dimension_t c) := u_i(v,r,c)*(1-u_i(v, r, c));
value_t abs_v(value_t v,
              dimension_t r,
              dimension_t c) := ABS(v);
value_t sig_v(value_t v,
              dimension_t r,
              dimension_t c) := IF(v>=0.5, 1, 0);

/**
 * Internal function to determine values for the model co-efficients
 * and selected stats from building the model.
 * @param independents the independent values
 * @param dependents the dependent values.
 * @param max_iter maximum number of iterations to try
 * @param epsilon the minimum change in the Beta value estimate to continue
 * @param ridge a value to populate a diagonal matrix that is added to
 * a matrix help assure that the matrix is invertible.
 * @return coefficient matrix plus model building stats
 */
EXPORT DATASET(Layout_Model)
       GetModel_local(DATASET(NumericField) independents,
               DATASET(DiscreteField) dependents,
               UNSIGNED2 max_iter=200,
               REAL8 epsilon=Constants.default_epsilon,
               REAL8 ridge=Constants.default_ridge) := FUNCTION
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
  dcols := TABLE(dependents, {wi, number, max_id:=MAX(GROUP, id), col:=1},
                 wi, number, FEW, UNSORTED);
  dep_map := PROJECT(GROUP(SORT(dcols, wi, number), wi),
                     TRANSFORM(RECORDOF(dcols),
                               SELF.col := COUNTER,
                               SELF:=LEFT));
  dep_cols := TABLE(dep_map,
                   {wi,r:=MAX(GROUP,max_id), c:=MAX(GROUP,col)},
                   wi, FEW, UNSORTED);
  ind_cols := TABLE(independents,
                    {wi, r:=MAX(GROUP,id), c:=MAX(GROUP,number)},
                    wi, FEW, UNSORTED);
  wi_info cmb(RECORDOF(ind_cols) ind, RECORDOF(dep_cols) dep):=TRANSFORM
    SELF.orig_wi := ind.wi;
    SELF.wi := ind.wi;
    SELF.col := 1;    // place holder
    SELF.dep_cols := dep.c;
    SELF.dep_rows := MAX(dep.r, ind.r);
    SELF.ind_cols := ind.c + 1; // to be inserted
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
                           SELF.orig_col:=RIGHT.number,
                           SELF:=LEFT),
                 LOOKUP, FEW);
  dist_wi := SORT(DISTRIBUTE(map_wi, wi), wi, LOCAL);
  // replicate independents, insert constant column, add zero elements
  ind_c1 := NORMALIZE(dist_wi, LEFT.ind_rows,
                      TRANSFORM(Ext_NumericField,
                                SELF.wi := LEFT.wi,
                                SELF.value := 1,
                                SELF.number := 1,
                                SELF.part_rows := LEFT.ind_rows,
                                SELF.part_cols := LEFT.ind_cols,
                                SELF.id := COUNTER));
  ind_c2 := DISTRIBUTE(JOIN(independents, map_wi,
                            LEFT.wi=RIGHT.orig_wi,
                            TRANSFORM(Ext_NumericField,
                                      SELF.wi := RIGHT.wi,
                                      SELF.number:=LEFT.number+1,
                                      SELF.part_rows := RIGHT.ind_rows,
                                      SELF.part_cols := RIGHT.ind_cols,
                                      SELF:=LEFT),
                            LOOKUP, MANY, FEW),
                       wi);
  ind_zero := NORMALIZE(dist_wi, LEFT.ind_rows*LEFT.ind_cols,
                        TRANSFORM(Ext_NumericField,
                          SELF.wi := LEFT.wi,
                          SELF.id := ((COUNTER-1)%LEFT.ind_rows)+1,
                          SELF.number:=((COUNTER-1) DIV LEFT.ind_rows)+1,
                          SELF.part_rows := LEFT.ind_rows,
                          SELF.part_cols := LEFT.ind_cols,
                          SELF.value := 0));
  ind_dist := ROLLUP(SORT(ind_c1+ind_c2+ind_zero, wi, number, id, LOCAL),
                     TRANSFORM(Ext_NumericField,
                               SELF.value := IF(LEFT.value<>0,
                                                LEFT.value,
                                                RIGHT.value),
                               SELF:=LEFT),
                     wi, number, id, LOCAL);
  ind_grp := GROUP(ind_dist, wi, LOCAL);
  ind_mat := ROLLUP(ind_grp, GROUP,
                    TRANSFORM(Part,
                              SELF.mat:=SET(ROWS(LEFT), value),
                              SELF := LEFT));
  // re-map Y matrix to multiple vectors and fluff with zeros
  dep_v := DISTRIBUTE(JOIN(dependents, map_wi,
                           LEFT.wi=RIGHT.orig_wi
                           AND LEFT.number=RIGHT.orig_col,
                           TRANSFORM(Ext_NumericField,
                                     SELF.number := 1,
                                     SELF.part_rows := RIGHT.dep_rows,
                                     SELF.part_cols := 1,
                                     SELF.wi := RIGHT.wi,
                                     SELF := LEFT),
                           LOOKUP, FEW),
                      wi);
  dep_0 := NORMALIZE(dist_wi, LEFT.dep_rows,
                     TRANSFORM(Ext_NumericField,
                               SELF.wi := LEFT.wi,
                               SELF.id := COUNTER,
                               SELF.number := 1,
                               SELF.part_rows := LEFT.dep_rows,
                               SELF.part_cols := 1,
                               SELF.value := 0));
  dep_dist := ROLLUP(SORT(dep_v+dep_0, wi, number, id, LOCAL),
                     TRANSFORM(Ext_NumericField,
                               SELF.value := IF(LEFT.value<>0,
                                                LEFT.value,
                                                RIGHT.value),
                               SELF := LEFT),
                      wi, number, id, LOCAL);
  dep_grp := GROUP(dep_dist, wi, LOCAL);
  dep_mat := ROLLUP(dep_grp, GROUP,
                    TRANSFORM(Part,
                              SELF.mat:=SET(ROWS(LEFT), value),
                              SELF := LEFT));
  // Define Ridge diagonal matrix
  Part makeRidge(wi_info wif) := TRANSFORM
    SELF.mat := make_diag(wif.ind_cols, ridge);
    SELF.wi := wif.wi;
    SELF.part_rows := wif.ind_cols;
    SELF.part_cols := wif.ind_cols;
  END;
  R_mat := PROJECT(dist_wi, makeRidge(LEFT));
  // Initial beta estimate
  Part init_beta(Part x, Part y) := TRANSFORM
    SELF.mat := make_vector(x.part_cols, 0.00000001); // make non-zero
    SELF.iterations := 0;
    SELF.max_delta := 2*epsilon;
    SELF.part_rows := x.part_cols;
    SELF.part_cols := 1;
    SELF.wi := x.wi;
  END;
  init_B := JOIN(ind_mat, dep_mat, LEFT.wi=RIGHT.wi,
                 init_beta(LEFT, RIGHT), LOCAL);
  // iterative least squares to converge Beta
  DATASET(Part) iter0(DATASET(Part) Beta, UNSIGNED c):=FUNCTION
    list := [Beta, ind_mat, dep_mat, R_mat];
    Part stp0(DATASET(Part) d) := TRANSFORM
      obs := d[2].part_rows;
      dims := d[2].part_cols;
      B := d[1].mat[1..dims];
      X := d[2].mat;
      Y := d[3].mat;
      R := d[4].mat;
      XB := gemm(FALSE, FALSE, obs, 1, dims, 1.0, X, B);
      V := Apply2Cells(obs, 1, XB, w_i);
      U := Apply2Cells(obs, 1, XB, u_i);
      W := V; //make_diag(obs, 1.0, V);
      XtW := dimm(TRUE, FALSE, FALSE, TRUE, dims, obs, obs, 1.0, X, W);
      XtWX_R := gemm(FALSE, FALSE, dims, dims, obs, 1.0, XtW, X, 1.0, R);
      Y_U := axpy(obs, -1.0, U, 1, Y, 1);
      WXB_Y_U := dimm(FALSE, FALSE, TRUE, FALSE, obs, 1, obs, 1.0, W, XB, 1.0, Y_U);
      XtWXB_Y_U := gemm(TRUE, FALSE, dims, 1, obs, 1.0, X, WXB_Y_U);
      LU_XtWX_R := getf2(dims, dims, XtWX_R);
      identity := make_diag(dims);
      inner := trsm(Side.Ax, Triangle.Lower, FALSE, Diagonal.UnitTri,
                    dims, dims, dims, 1.0, LU_XtWX_R, identity);
      inv_m := trsm(Side.Ax, Triangle.Upper, FALSE, Diagonal.NotUnitTri,
                    dims, dims, dims, 1.0, LU_XtWX_R, inner);
      new_B := gemm(FALSE, FALSE, dims, 1, dims, 1.0, inv_m, XtWXB_Y_U);
      SE := extract_diag(dims, dims, inv_M);
      XB1:= gemm(FALSE, FALSE, obs, 1, dims, 1.0, X, new_B);
      U1 := Apply2Cells(obs, 1, XB1, u_i);
      Y1 := Apply2Cells(obs, 1, U1, sig_v);
      avp := axpy(obs, -1, Y1, 1, Y, 1);
      incorrect := BLAS.dasum(obs, avp, 1);
      B_new_B := axpy(dims, -1.0, new_B, 1, B, 1);
      delta_B := Apply2Cells(dims, 1, b_new_B, abs_v);
      SELF.mat := new_B + SE;
      SELF.iterations := c;
      SELF.max_delta := MAX(delta_B);
      SELF.part_cols := 2;
      SELF.incorrect := incorrect;
      SELF.correct := obs - incorrect;
      SELF := d[1];
    END;
    rslt := JOIN(list, LEFT.wi=RIGHT.wi, stp0(ROWS(LEFT)),
                 SORTED(wi), LOCAL);
    RETURN rslt;
  END;
  calc_B := LOOP(init_B, epsilon<LEFT.max_delta AND max_iter>LEFT.iterations,
                 iter0(ROWS(LEFT), COUNTER));
  // Capture model statistics, multiple responses are still multiple wi
  // Betas and SE for betas, Iterations, last delta, ciorrect, incorrect
  NumericField extBetas(Part p, UNSIGNED subscript) := TRANSFORM
    beta := p.mat[subscript];
    se := SQRT(p.mat[subscript]);
    SELF.wi := p.wi;
    SELF.id := Constants.id_betas + subscript - 1;
    SELF.number := 1;
    SELF.value := IF(subscript>p.part_rows, se, beta);
  END;
  b_se := NORMALIZE(calc_B, LEFT.part_rows*2, extBetas(LEFT,COUNTER));
  NumericField extStats(Part p, UNSIGNED fld) := TRANSFORM
    SELF.wi := p.wi;
    SELF.id := CHOOSE(fld, Constants.id_iters, Constants.id_delta,
                      Constants.id_correct, Constants.id_incorrect);
    SELF.number := 1;
    SELF.value := CHOOSE(fld, p.iterations, p.max_delta, p.correct,
                          p.incorrect);
  END;
  stats := NORMALIZE(calc_B, 4, extStats(LEFT, COUNTER));
  // return multiple wi to single wi and multiple columns
  Layout_Model wi_reset(NumericField nf, wi_info w) := TRANSFORM
    SELF.wi := w.orig_wi;
    SELF.number := w.orig_col;
    SELF := nf;
  END;
  var_data := JOIN(b_se+stats, dist_wi, LEFT.wi=RIGHT.wi,
                   wi_reset(LEFT, RIGHT), LOOKUP, FEW) : ONWARNING(1005,ignore);
  // get base data for the training session
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
  // debug
  has_wi := PROJECT(dist_wi, TRANSFORM(Layout_Model,
                              SELF.wi:=LEFT.wi,
                              SELF.id:=0;
                              SELF.number:=10;
                              SELF.value:=Std.System.thorlib.node()));
  has_calc := PROJECT(calc_B, TRANSFORM(Layout_Model,
                                SELF.wi:=LEFT.wi,
                                SELF.id:=0,
                                SELF.number:=11,
                                SELF.value:=Std.System.thorlib.node()));
  has_init := PROJECT(init_B, TRANSFORM(Layout_Model,
                                SELF.wi:=LEFT.wi,
                                SELF.id:=0,
                                SELF.number:=12,
                                SELF.value:=Std.System.thorlib.node()));
  has_ind  := PROJECT(ind_mat, TRANSFORM(Layout_Model,
                                SELF.wi:=LEFT.wi,
                                SELF.id:=0,
                                SELF.number:=13,
                                SELF.value:=Std.System.thorlib.node()));
  has_dep  := PROJECT(dep_mat, TRANSFORM(Layout_Model,
                                SELF.wi:=LEFT.wi,
                                SELF.id:=0,
                                SELF.number:=14,
                                SELF.value:=Std.System.thorlib.node()));
  has_R     := PROJECT(R_mat, TRANSFORM(Layout_Model,
                                SELF.wi:=LEFT.wi,
                                SELF.id:=0,
                                SELF.number:=15,
                                SELF.value:=Std.System.thorlib.node()));
  db := has_wi + has_calc + has_init + has_ind + has_dep + has_R;
  RETURN base_data + var_data + db;
END;
