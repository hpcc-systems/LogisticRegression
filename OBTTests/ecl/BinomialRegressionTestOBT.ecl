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

// This is the BinomialRegression file from the validation folder
// with modifications so it can be incorporated into the OBT and
// tested regulary

IMPORT LogisticRegression AS LR;
IMPORT LogisticRegression.Types;
IMPORT LogisticRegression.Validation;
IMPORT ML_Core;

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

// Result record
TestValues := RECORD
  STRING8 src;
  REAL8 icept_coef;
  REAL8 sl_coef;
  REAL8 sw_coef;
  REAL8 pl_coef;
  REAL8 pw_coef;
  REAL8 icept_err;
  REAL8 sl_err;
  REAL8 sw_err;
  REAL8 pl_err;
  REAL8 pw_err;
  REAL8 icept_pval;
  REAL8 sl_pval;
  REAL8 sw_pval;
  REAL8 pl_pval;
  REAL8 pw_pval;
  REAL8 aic;
  UNSIGNED4 wi;
END;

ExpectedResult := DATASET([{'sm.logit',
      7.32292705, -0.25274345, -2.77938918,  1.29930595, -2.70427087,
      2.49799533,  0.64945102,  0.78587716,  0.68228159,  1.16265393,
      0.00337306,  0.69715427,  0.00040520,  0.05686405,  0.02002140,
      155.76486509, 1}], TestValues);

// Obtain models
Model := LR.BinomialLogisticRegression(max_iter:=4, ridge:=0.0).GetModel(Independ, iris_classes);
BetaVals := LR.ExtractBeta_pval(Model);
DevianceDet := LR.Deviance_Detail(iris_classes, LR.LogitScore(BetaVals, Independ));
ModelDeviance := LR.Model_Deviance(DevianceDet, BetaVals);

// Maximum allowed difference
REAL8 MaxDiff := 0.007;

// Evaluation record
CompareRec := RECORD
  STRING8 src;
  STRING test;
  REAL8 std_value;
  REAL8 tst_value;
  BOOLEAN equal;
END;

CompareRec check(Types.pval_Model_Coef p, TestValues t, UNSIGNED s):=TRANSFORM
  SELF.src := t.src;
  SELF.tst_value := CHOOSE(s, p.w, p.se, p.p_value);
  SELF.std_value := CHOOSE(p.ind_col+1,
                           CHOOSE(s, t.icept_coef, t.icept_err, t.icept_pval),
                           CHOOSE(s, t.sl_coef, t.sl_err, t.sl_pval),
                           CHOOSE(s, t.sw_coef, t.sw_err, t.sw_pval),
                           CHOOSE(s, t.pl_coef, t.pl_err, t.pl_pval),
                           CHOOSE(s, t.pw_coef, t.pw_err, t.pw_pval));
  SELF.test := CHOOSE(s, 'coef ', 'se ', 'p-val ')
             + CHOOSE(p.ind_col+1,
                       'Intercept', 'sepal length', 'sepal width',
                                    'petal length', 'petal width');
  SELF.equal := ABS(SELF.tst_value-SELF.std_value) <= MaxDiff;
END;

// These Records compare the values of its stated field
// among ExpectedResult and BetaVals
CheckCoef := JOIN(BetaVals, ExpectedResult, LEFT.wi=RIGHT.wi,
                   check(LEFT,RIGHT, 1), LOOKUP);
CheckSe := JOIN(BetaVals, ExpectedResult, LEFT.wi=RIGHT.wi,
                 check(LEFT,RIGHT, 2), LOOKUP);
CheckPval := JOIN(BetaVals, ExpectedResult, LEFT.wi=RIGHT.wi,
                   check(LEFT, RIGHT, 3), LOOKUP);

CompareRec check_aic(Types.Deviance_Record d, TestValues t) := TRANSFORM
  SELF.test := 'AIC';
  SELF.src := t.src;
  SELF.std_value := t.aic;
  SELF.tst_value := d.aic;
  SELF.equal := ABS(SELF.tst_value-SELF.std_value) <= MaxDiff;
END;

CheckAic := JOIN(ModelDeviance, ExpectedResult, LEFT.wi=RIGHT.wi,
                  check_aic(LEFT, RIGHT), LOOKUP);

CheckAll := CheckCoef + CheckSe + CheckPval + CheckAic;
Errors := CheckAll(NOT equal);

OUTPUT(IF(NOT EXISTS(errors), 'Pass', 'Fail'), NAMED('Result'));
OUTPUT(Errors, NAMED('Errors'));
