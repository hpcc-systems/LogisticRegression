IMPORT $ AS V;
IMPORT $.^ AS LR;
IMPORT ML_Core.Types AS Types;  // From Field macro wants Types.t_...
IMPORT ML_Core AS Core;
IMPORT LR.Types AS LR_Types;

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

Core.ToField(iris, iris_indep, id, , 1,
             'sepal_length,sepal_width,petal_length,petal_width');
Core.ToField(iris, iris_dep, id, , 1, 'class');
iris_classes := PROJECT(iris_dep, Types.DiscreteField);

// results
Test_Values := RECORD
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
v_result := DATASET([{'sm.logit',
      7.32292705, -0.25274345, -2.77938918,  1.29930595, -2.70427087,
      2.49799533,  0.64945102,  0.78587716,  0.68228159,  1.16265393,
      0.00337306,  0.69715427,  0.00040520,  0.05686405,  0.02002140,
      155.76486509, 1}], Test_Values);
// ECL version
mdl := LR.BinomialLogisticRegression(max_iter:=4, ridge:=0.0).GetModel(iris_indep, iris_classes);
rpt := LR.ExtractReport(mdl);
coef_pval := LR.ExtractBeta_pval(mdl);
devdet := LR.Deviance_Detail(iris_classes, LR.LogitScore(coef_pval, iris_indep));
modl_dev := LR.Model_Deviance(devdet, coef_pval);
// Compare
REAL8 max_diff := 0.007;
Compare_Rec := RECORD
  STRING8 src;
  STRING test;
  REAL8 std_value;
  REAL8 tst_value;
  BOOLEAN equal;
END;
Compare_Rec check(LR_Types.pval_Model_Coef p, Test_Values t, UNSIGNED s):=TRANSFORM
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
  SELF.equal := ABS(SELF.tst_value-SELF.std_value) <= max_diff;
END;
coef_check := JOIN(coef_pval, v_result, LEFT.wi=RIGHT.wi,
                   check(LEFT,RIGHT, 1), LOOKUP);
se_check := JOIN(coef_pval, v_result, LEFT.wi=RIGHT.wi,
                 check(LEFT,RIGHT, 2), LOOKUP);
pval_check := JOIN(coef_pval, v_result, LEFT.wi=RIGHT.wi,
                   check(LEFT, RIGHT, 3), LOOKUP);
Compare_Rec check_aic(LR_Types.Deviance_Record d, Test_Values t):=TRANSFORM
  SELF.test := 'AIC';
  SELF.src := t.src;
  SELF.std_value := t.aic;
  SELF.tst_value := d.aic;
  SELF.equal := ABS(SELF.tst_value-SELF.std_value) <= max_diff;
END;
aic_check := JOIN(modl_dev, v_result, LEFT.wi=RIGHT.wi,
                  check_aic(LEFT, RIGHT), LOOKUP);
all_checks := coef_check + se_check + pval_check + aic_check;
errors := all_checks(NOT equal);
err_out := OUTPUT(errors, NAMED('Error_report'));
ok_out := OUTPUT('Passed', NAMED('Status'));
EXPORT BinomialRegression := IF(EXISTS(errors), err_out, ok_out);
