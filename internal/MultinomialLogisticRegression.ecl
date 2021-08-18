/*##################################################################################
## HPCC SYSTEMS software Copyright (C) 2017,2021 HPCC Systems.  All rights reserved.
################################################################################# */

IMPORT $ AS LR;
IMPORT PBBLAS.converted AS converted;
IMPORT PBBLAS AS PB;
IMPORT ML_Core;
IMPORT ML_Core.Types AS Core_Types;
IMPORT LR.Types;
IMPORT Types.Ind1 as idx;
IMPORT STD.System.Log as Syslog;


// Define common data types for convenience
Layout_Model2 := Core_Types.Layout_Model2;
NumericField := Core_Types.NumericField;
DiscreteField := Core_Types.DiscreteField;
t_value      := PB.Types.value_t;
Layout_Cell := PB.Types.Layout_Cell;
t_dimension := Types.t_dimension;
Confusion_Detail := Core_Types.Confusion_Detail;
Classification_Accuracy := Core_Types.Classification_Accuracy;
Class_Accuracy := Core_Types.Class_Accuracy;
empty_numeric := DATASET([], NumericField);
empty_discrete := DATASET([], DiscreteField);
l_loop := Types.Loop;
l_wi := Types.wis;

/**
  * Multinomial Logistic Regression
  *
  * Multinomial Logistic Regression model for multi-class classification tasks.
  * The training process will stop if the model hits the maximum iterations
  * or meets the converge threshold.
  *
  *
  * @param max_iter The maxinum number of iterations. It's an integer scalar value.
  *                  The default value is 30.
  * @param alpha    learning rate with a default value 0.00005.
  * @param tol      converge threshold based on the average loss. The default value is 0.00001.
  *
  */
EXPORT MultinomialLogisticRegression( UNSIGNED4 max_iter = 50,
                                      REAL alpha = 0.00005,
                                      REAL tol = 0.00001) := MODULE(ML_Core.Interfaces.IClassify2)
  // oneHotencode independents.
  SHARED encode(DATASET(DiscreteField) dp) := FUNCTION
    dependents := dp(number = 1);
    categories := TABLE(dependents, {wi, number, value}, wi, number, value);
    newNumbers := PROJECT(GROUP(SORT(categories, wi, number, value), wi),
                          TRANSFORM({RECORDOF(LEFT), UNSIGNED4 newNum},
                                    SELF.newNum := COUNTER,
                                    SELF := LEFT));
    encode := JOIN( dependents, newNumbers, LEFT.wi = RIGHT.wi AND LEFT.number = RIGHT.number,
                    TRANSFORM(NumericField,
                              SELF.number := RIGHT.newnum,
                              SELF.value := IF(LEFT.value = RIGHT.value, 1, 0),
                              SELF := LEFT), MANY, LOOKUP);
    RETURN encode;
  END;

  // Initialize the weight
  SHARED   initialWeights(DATASET(numericField) indep=empty_numeric,
                                      DATASET(discreteField) dep=empty_discrete) := FUNCTION
  wis0:= TABLE(indep, {wi, dim := MAX(GROUP, number), obs := MAX(GROUP, id)}, wi, MERGE);
  wis1:= TABLE(dep, {wi, value}, wi, value,MERGE);
  wis2:= TABLE(wis1, {wi, cls := COUNT(GROUP)}, wi, MERGE);
  wis := JOIN(wis0, wis2,
             LEFT.wi = RIGHT.wi,
             TRANSFORM(l_wi,
                      d := LEFT.dim;
                      c := RIGHT.cls;
                      w := DATASET(d * c,
                                        TRANSFORM(Types.NumericField,
                                                 SELF.wi := LEFT.wi,
                                                 SELF.id := IF(COUNTER % c = 0, c, COUNTER % c),
                                                 SELF.number := (COUNTER-1) / c+ 1,
                                                 SELF.value := 1));
                      SELF := LEFT,
                      SELF := RIGHT,
                      SELF.w := w));
  RETURN wis;
  END;

  // Helper functions for matrix manipulation
  SHARED t_value e(t_value v, t_dimension r, t_dimension c) := EXP(v);
  SHARED t_value le(t_value v, t_dimension r, t_dimension c) := LN(v);

  // Transform the dependents using softmax
  SHARED DATASET(Layout_cell) softmax( DATASET(Layout_cell) x,
                                         DATASET(Layout_cell) w) := FUNCTION
    // Calculate Z Z = XWT
    z := PB.gemm(FALSE, TRUE, 1, w, x);
    maxZ := MAX(z, v);
    minZ := MIN(z, v);
    diff := maxz - minz;
    scaledZ := PROJECT(z,
                      TRANSFORM(RECORDOf(LEFT),
                                SELF.v := ((LEFT.v - minz)*(100))/diff,
                                SELF := LEFT ));
    e2z := PB.Apply2Elements(scaledZ, e);
    sum_e2z := TABLE(e2z, {wi_id, y, s:= SUM(GROUP, v)}, wi_id, y);
    A:= JOIN(e2z, sum_e2z,
            LEFT.wi_id = RIGHT.wi_id AND LEFT.y = RIGHT.y,
            TRANSFORM(Layout_Cell, SELF.v :=LEFT.v/RIGHT.s, SELF := LEFT),
            LEFT OUTER);
    RETURN A;
  END;

  // Learning process with gradient decent
  SHARED fit(DATASET(numericField) indep=empty_numeric,
                                      DATASET(DiscreteField) dep=empty_discrete) := FUNCTION
    disIndep := DISTRIBUTE(indep, HASH(wi, id));
    disDep :=  DISTRIBUTE(dep, HASH(wi, id));
    wis00 :=  TABLE(indep(value = 0), {wi,number, m := COUNT(GROUP)}, wi, number, MERGE);
    wis0:= TABLE(indep, {wi, dim := MAX(GROUP, number), obs := MAX(GROUP, id)}, wi, MERGE);
    mID := MAX(dep, id);
    sparseCol :=JOIN(wis00, wis0,
                    LEFT.wi = RIGHT.wi AND LEFT.m = RIGHT.obs,
                    TRANSFORM(RECORDOF(LEFT),
                    SELF := LEFT));
    denseIndep := JOIN(disIndep, sparseCol,
                        LEFT.wi = RIGHT.wi AND LEFT.number = RIGHT.number,
                           TRANSFORM(Types.NumericField,
                           SELF.value :=  IF(LEFT.number <> RIGHT.number ,
                           LEFT.value, IF(LEFT.id = 1, 0.0000000001, LEFT.value)),
                           SELF := LEFT ),
                           LEFT OUTER, LOOKUP );

    // Transform input datasets for matrix manupulation
    dense_x := Converted.NFToMatrix(denseIndep);
    encoded := encode(disDep);
    dense_y := Converted.NFToMatrix(encoded);

    // Iterative learning process
    lp(DATASET(l_loop) input, INTEGER C0) := FUNCTION
        t0 := TABLE(dense_x, {wi_id, x}, wi_id, x, MERGE);
        t1 := PROJECT(t0,
                     TRANSFORM({RECORDOF(t0), REAL r},
                     SELF.r := RANDOM()/1000000,
                     SELF := LEFT));
        t2 := PROJECT(SORT(t1, r),
                      TRANSFORM({RECORDOF(t0), UNSIGNED4 newID},
                                SELF.newID := COUNTER,
                                SELF := LEFT));
        x := JOIN(dense_x, t2,
                  LEFT.wi_id = RIGHT.wi_id AND LEFT.X = RIGHT.x,
                  TRANSFORM(Layout_cell,
                  SELF.x := RIGHT.newID,
                  SELF := LEFT), SMART);
        y := JOIN(dense_y, t2,
                  LEFT.wi_id = RIGHT.wi_id AND LEFT.X = RIGHT.x,
                  TRANSFORM(Layout_cell,
                  SELF.x := RIGHT.newID,
                  SELF := LEFT), SMART);
        last_iter := input(iter = c0 -1);
        w := IF(c0 = 1, input(iter = 0).t, input(iter = c0 -1).t);
        A1 := SOFTMAX(x, w);
        tA := PB.tran(1, A1);
        gd01 := PB.Axpy(-1, y, tA);
        gd1 := PB.gemm(TRUE, FALSE, 1,  gd01, x);
        w1 := SORT(PB.Axpy( -alpha, gd1, w), wi_id, x, y);
        t0_in := softmax(dense_x, w1);
        t1_in := PB.TRAN(1,t0_in);
        log_A1 := PB.Apply2Elements(t1_in, le);
        cel01 := PB.HadamardProduct( dense_y, Log_A1);
        cel11 := SORT(TABLE(cel01, {wi_id, s := (-1) * SUM(GROUP, v)}, wi_id), wi_id);
        logs := Syslog.addWorkunitInformation('Training Status: wi = ' +
                      cel11[1].wi_id + ', Iteration = ' + c0 + ', loss = ' + ROUND(cel11[1].s/mID, 6));
        rst :=  JOIN(last_iter, cel11,
                     LEFT.wi = RIGHT.wi_id,
                     TRANSFORM(RECORDOF(LEFT),
                               SELF.wi := LEFT.wi,
                               SELF.iter := c0,
                               SELF.tol := RIGHT.s,
                               SELF.delta := IF(c0=1, RIGHT.s,  ABS(RIGHT.s - LEFT.tol)),
                               SELF.t := w1(wi_id = LEFT.wi)));
        RETURN WHEN(input + rst, logs);
      END;
    // Get initial weights
    wis := initialWeights(indep, dep);
    // Project initial weights to the Loop format.
    init := PROJECT(wis,
                   TRANSFORM(l_loop,
                             SELF.wi := LEFT.wi,
                             SELF.iter := 0,
                             SELF.t := Converted.NFToMatrix(LEFT.w),
                             SELF.tol := tol + 0.0000000001,
                             SELF.delta := 0));
    // Start the looping process. The looping process will stop if it hits
    // the maximum iterations or meets the converge threshold.
    result := LOOP(init,
                   MAX(ROWS(LEFT),iter) < max_iter AND MIN(ROWS(LEFT), tol) > tol,
                   lp(ROWS(LEFT), counter));
    RETURN SORT(result, wi, iter);
 END;

  /**
    * Return the learned Multinomial Logistic Regression model.
    *
    * NOTE:
    * 1. The number field that store the dependent values is requried to be 1.
    * 2. The classes should be sequential and start with 1.
    *
    * The returned model includes below information:
    * The final weights
    * The iteration number when the training process stops.
    *
    * @param independents The independent varaibls to train the model.
    * @param dependents The dependent variables to train the model.
    * @return  Model in DATASET(Layout_Model2) format.
    * @see ML_core/Types.Layout_Model2
    */
  EXPORT DATASET(Layout_Model2) GetModel(DATASET(NumericField) independents,
                                        DATASET(DiscreteField) dependents) := FUNCTION
    // Get the learned model
    m := fit(independents, dependents);
    // Store the model information in Layout_Model2 format.
    n_iter := TABLE(m, {wi, iteration := MAX(GROUP, iter)}, wi);
    last_iter := DEDUP(GROUP(SORT(m, wi, -iter)), wi );
    weights := PROJECT(last_iter,
                       TRANSFORM({UNSIGNED4 wi, DATASET(Layout_Cell) weight},
                       SELF.weight := LEFT.t,
                       SELF := LEFT));
    iters := PROJECT(n_iter,
                       TRANSFORM(Layout_model2,
                       SELF.wi := LEFT.wi,
                       SELF.value := LEFT.iteration,
                       SELF.indexes := [idx.iterations]));
    coef0 := PROJECT(weights.weight,
                       TRANSFORM(NumericField,
                       SELF.wi := LEFT.wi_id,
                       SELF.value := LEFT.v,
                       SELF.id := LEFT.x,
                       SELF.number := LEFT.y));
    coef := ML_Core.ModelOps2.FromNumericField(coef0, [idx.weights]);
    modelType := DATASET([{0, 0, [idx.ifBinary]}], Layout_Model2);
    // Return the model information
    RETURN modelType + iters + coef;
  END;

  /**
    * Classify the observations using a model.
    * @param model The model, which is produced by a getModel function.
    * @param observations New observations (independent data) to be classified.
    * @return Predicted class values.
    *
    */
  EXPORT DATASET(DiscreteField) Classify( DATASET(Layout_Model2) model,
                                          DATASET(NumericField) observations) := FUNCTION
    ws :=  ML_Core.ModelOps2.ToNumericField(model, [idx.weights]);
    pred0 := softmax(Converted.NFToMatrix(observations), Converted.NFToMatrix(ws));
    pred := PB.TRAN(1, pred0);
    PREDICT := PROJECT(DEDUP(GROUP(SORT(pred, wi_id, x, -v), wi_id, x), wi_id, x),
                      TRANSFORM(ML_Core.Types.DiscreteField,
                                SELF.wi := LEFT.wi_id,
                                SELF.id := LEFT.x,
                                SELF.number := 1,
                                SELF.value := LEFT.y - 1));
    RETURN GROUP(predict);
  END;

END;