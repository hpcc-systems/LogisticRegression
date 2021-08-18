/*##################################################################################
## HPCC SYSTEMS software Copyright (C) 2017,2021 HPCC Systems.  All rights reserved.
################################################################################# */

IMPORT $ AS V;
IMPORT $.^ AS LR;
IMPORT ML_Core;
IMPORT ML_Core.Types AS CTypes;
IMPORT $.^.internal.Types as Types;

IrisData := RECORD
  Types.t_recordID id;
  RECORDOF(V.IrisDS);
END;
IrisData enum_recs(V.IrisDS rec, UNSIGNED c) := TRANSFORM
  SELF.id := c;
  SELF.class := rec.class;
  SELF := rec;
END;
iris := PROJECT(V.IrisDS, enum_recs(LEFT,COUNTER));
iris_ind0 := PROJECT(iris, TRANSFORM(IRISdata, SELF.id := COUNTER, SELF := LEFT));
ML_Core.ToField(iris_ind0, raw_indep, id, , 1, 'sepal_length, sepal_width');
ML_Core.ToField(iris_ind0, raw_dep, id, , 1, 'class');
dep := PROJECT(raw_dep, TRANSFORM(CTypes.discreteField, SELF := LEFT));

L_MLR := LR.LogisticRegression(30,,,,,);
mod4 := L_MLR.GetModel(raw_indep, dep);
pred4 :=L_MLR.classify(mod4, raw_indep);
OUTPUT(pred4,NAMED('pred4'));
cf4 := L_MLR.ConfusionMatrix(mod4, dep, raw_indep);
OUTPUT(cf4,NAMED('cf4'));
acc4:= L_MLR.accuracy(mod4,  dep, raw_indep);
OUTPUT(acc4,NAMED('acc4'));