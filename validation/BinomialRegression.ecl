/*##################################################################################
## HPCC SYSTEMS software Copyright (C) 2017,2021 HPCC Systems.  All rights reserved.
################################################################################# */

IMPORT $ AS V;
IMPORT $.^ AS LR;
IMPORT ML_Core.Types AS Types;  // From Field macro wants Types.t_...
IMPORT ML_Core AS Core;

IrisData := RECORD
  Types.t_recordID id;
  RECORDOF(V.IrisDS);
END;
IrisData enum_recs(V.IrisDS rec, UNSIGNED c) := TRANSFORM
  SELF.id := c;
  SELF.class := IF(rec.class=1, 1, 0);
  SELF := rec;
END;
iris := PROJECT(V.IrisDS, enum_recs(LEFT,COUNTER));
Core.ToField(iris, ind_data, id, , 1,
             'sepal_length,sepal_width');
Core.ToField(iris, iris_dep, id, , 1, 'class');
resp_data := PROJECT(iris_dep, Types.DiscreteField);

L_BLR := LR.LogisticRegression(10);
mod3 := L_BLR.GetModel(ind_data, resp_data);
OUTPUT(mod3,NAMED('mod3'), ALL);
pred3 :=L_BLR.classify(mod3, ind_data);
OUTPUT(pred3,NAMED('pred3'), ALL);
cf3 := L_BLR.ConfusionMatrix(mod3, resp_data, ind_data);
OUTPUT(cf3,NAMED('cf3'));
acc3:= L_BLR.accuracy(mod3, resp_data, ind_data);
OUTPUT(acc3,NAMED('acc3'));
