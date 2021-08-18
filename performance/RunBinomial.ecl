/*##################################################################################
## HPCC SYSTEMS software Copyright (C) 2017,2021 HPCC Systems.  All rights reserved.
################################################################################# */

//*******************************************************
num_work_items := 1;
avg_size := 1000000;
columns := 10;
max_iterations := 20;
//*******************************************************
IMPORT $.^ AS LR;
IMPORT $ AS Perf;
IMPORT ML_Core.Types AS Core_Types;
NumericField := Core_Types.NumericField;
DiscreteField := Core_Types.DiscreteField;
lcl := LR.internal.Constants.builder_irls_local;
gbl := LR.internal.Constants.builder_irls_global;

obs := Perf.GenData(num_work_items, columns, avg_size, 2, TRUE) : INDEPENDENT;
resp_data := PROJECT(obs, TRANSFORM(DiscreteField, SELF:=LEFT.resp_var));
ind_data := NORMALIZE(obs, LEFT.explan_vars, TRANSFORM(NumericField, SELF:=RIGHT));

L_BLR := LR.LogisticRegression(10);
mod3 := L_BLR.GetModel(ind_data, resp_data);
OUTPUT(mod3,NAMED('mod3'));
pred3 :=L_BLR.classify(mod3, ind_data);
OUTPUT(pred3,NAMED('pred3'));
cf3 := L_BLR.ConfusionMatrix(mod3, resp_data, ind_data);
OUTPUT(cf3,NAMED('cf3'));
acc3:= L_BLR.accuracy(mod3, resp_data, ind_data);
OUTPUT(acc3,NAMED('acc3'));
