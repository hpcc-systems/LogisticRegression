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

// Test the results of binomial confusion to confirm
// it gets the intended values from the report

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

IrisClasses := PROJECT(Depend, Types.DiscreteField);

BLR := LR.BinomialLogisticRegression();

Model := BLR.GetModel(Independ, IrisClasses);
Report := BLR.Report(Model, Independ, IrisClasses);

BinomialConf := LR.BinomialConfusion(Report);
BCRow := BinomialConf[1];

// Tests the true/false positive/negative values

ExpectedTP := Report[4].occurs;
ExpectedTN := Report[1].occurs;
ExpectedFP := Report[2].occurs;
ExpectedFN := Report[3].occurs;

OUTPUT(GetResult(ExpectedTP, BCRow.true_positive), NAMED('Test1A'));
OUTPUT(GetResult(ExpectedTN, BCRow.true_negative), NAMED('Test1B'));
OUTPUT(GetResult(ExpectedFP, BCRow.false_positive), NAMED('Test1C'));
OUTPUT(GetResult(ExpectedFN, BCRow.false_negative), NAMED('Test1D'));

// 3 and 4 are the rows where actual_class = 1,
// so cond_pos should be the sum of these occurs
ExpectedCondPos := Report[3].occurs + Report[4].occurs;
OUTPUT(GetResult(ExpectedCondPos, BCRow.cond_pos), NAMED('Test2A'));

// 1 and 2 are the rows where actual_class = 0,
// so cond_neg should be the sum of these occurs
ExpectedCondNeg := Report[1].occurs + Report[2].occurs;
OUTPUT(GetResult(ExpectedCondNeg, BCRow.cond_neg), NAMED('Test2B'));

// 2 and 4 are the rows where predict_class = 1,
// so pred_pos should be the sum of these occurs
ExpectedPredPos := Report[2].occurs + Report[4].occurs;
OUTPUT(GetResult(ExpectedPredPos, BCRow.pred_pos), NAMED('Test3A'));

// 1 and 3 are the rows where predict_class = 0,
// so pred_neg should be the sum of these occurs
ExpectedPredNeg := Report[1].occurs + Report[3].occurs;
OUTPUT(GetResult(ExpectedPredNeg, BCRow.pred_neg), NAMED('Test3B'));

// Accuracy check
ExpectedAccuracy := 0.74;
OUTPUT(GetResult(ExpectedAccuracy, BCRow.accuracy), NAMED('Test4'));
