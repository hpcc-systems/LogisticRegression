/*##################################################################################
## HPCC SYSTEMS software Copyright (C) 2017,2021 HPCC Systems.  All rights reserved.
################################################################################# */

//*******************************************************
num_work_items := 1;
avg_size := 1000000;
columns := 10;
max_iterations := 20;
num_classes := 3;
//*******************************************************
IMPORT $.^ AS LR;
IMPORT $ AS Perf;
IMPORT ML_Core.Types AS Core_Types;
NumericField := Core_Types.NumericField;
DiscreteField := Core_Types.DiscreteField;

// Generate data
obs := Perf.GenData(num_work_items, columns, avg_size, num_classes, TRUE) : INDEPENDENT;
resp_data := PROJECT(obs, TRANSFORM(DiscreteField, SELF.value := LEFT.resp_var.value + 1, SELF:=LEFT.resp_var));
ind_data := NORMALIZE(obs, LEFT.explan_vars, TRANSFORM(NumericField, SELF:=RIGHT));

// Run it
LR_module := LR.LogisticRegression(max_iterations);
mod := LR_module.GetModel(ind_data, resp_data);
pred :=LR_module.classify(mod, ind_data);
OUTPUT(pred,NAMED('pred'));
acc:= LR_module.accuracy(mod, resp_data, ind_data);
OUTPUT(acc,NAMED('acc'));
cf := LR_module.ConfusionMatrix(mod, resp_data, ind_data);
OUTPUT(cf,NAMED('cf'));
