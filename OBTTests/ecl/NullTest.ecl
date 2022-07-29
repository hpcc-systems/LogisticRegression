/*##############################################################################
    
    HPCC SYSTEMS software Copyright (C) 2022 HPCC Systems®.
    
    Licensed under the Apache License, Version 2.0 (the "License");
    you may not use this file except in compliance with the License.
    You may obtain a copy of the License at
       
       http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software
    distributed under the License is distributed on an "AS IS" BASIS,
    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
    See the License for the specific language governing permissions and
    limitations under the License.
############################################################################## */

#ONWARNING(4531, ignore);

// Test that confirms Null_Deviance returns
// the right values;

IMPORT LogisticRegression AS LR;
IMPORT LogisticRegression.Types;
IMPORT LogisticRegression.Validation;
IMPORT ML_Core;

// Helper function

STRING GetResult(INTEGER Expected, INTEGER Actual) := FUNCTION
  RETURN IF(Expected = Actual, 'Pass', 'Fail ' + 'Expected: ' + Expected + ' Result: ' + Actual);
END;

IrisDS := Validation.IrisDs;

IrisData := RECORD
  Types.t_recordID id;
  RECORDOF(IrisDS);
END;

IrisData enum_recs(IrisDS rec, UNSIGNED c) := TRANSFORM
  SELF.id := c;
  SELF.class := IF(rec.class=1, 1, 0);
  SELF := rec;
END;

Iris := PROJECT(IrisDS, enum_recs(LEFT,COUNTER));

ML_Core.ToField(Iris, Independ, id, , 1, 'sepal_length,sepal_width,petal_length,petal_width');
ML_Core.ToField(iris, Depend, id, , 1, 'class');

iris_classes := PROJECT(Depend, Types.DiscreteField);

Model := LR.BinomialLogisticRegression(max_iter:=4, ridge:=0.0).GetModel(Independ, iris_classes);
BetaVals := LR.ExtractBeta_pval(Model);
DevianceDet := LR.Deviance_Detail(iris_classes, LR.LogitScore(BetaVals, Independ));

NullDev := LR.Null_Deviance(DevianceDet);
NDRow := NullDev[1];

// Should be the amount of rows -1 
ExpectedDF := 149;
OUTPUT(GetResult(ExpectedDF, NDRow.df), NAMED('Test1'));

NumRoundDec := 2;

ExpectedDev := 190.95;
OUTPUT(GetResult(ExpectedDev, ROUND(NDRow.deviance, NumRoundDec)), NAMED('Test2'));

// AIC should be deviance + 2
ExpectedAIC := ExpectedDev + 2;
OUTPUT(GetResult(ExpectedAIC, ROUND(NDRow.AIC, NumRoundDec)), NAMED('Test3'));
