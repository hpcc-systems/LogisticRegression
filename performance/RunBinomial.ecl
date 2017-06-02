//*******************************************************
num_work_items := 1;
avg_size := 2000000;
columns := 1000;
max_iterations := 50;
//*******************************************************
IMPORT $.^ AS LR;
IMPORT $ AS Perf;
IMPORT ML_Core.Types AS Core_Types;
NumericField := Core_Types.NumericField;
DiscreteField := Core_Types.DiscreteField;
lcl := LR.Constants.builder_irls_local;
gbl := LR.Constants.builder_irls_global;

obs := Perf.GenData(num_work_items, columns, avg_size, 2, TRUE) : INDEPENDENT;
resp_data := PROJECT(obs, TRANSFORM(DiscreteField, SELF:=LEFT.resp_var));
ind_data := NORMALIZE(obs, LEFT.explan_vars, TRANSFORM(NumericField, SELF:=RIGHT));
// Data stats
ds_tab := TABLE(obs,
      {wi, cls:=resp_var.value,
       REAL8 min_explan:=MIN(GROUP, SUM(explan_vars, value)),
       REAL8 max_explan:=MAX(GROUP, SUM(explan_vars, value)),
       REAL8 ave_explan:=AVE(GROUP, SUM(explan_vars, value)),
       REAL8 sd_explan:=SQRT(VARIANCE(GROUP,SUM(explan_vars, value)))},
      wi, resp_var.value, FEW, UNSORTED);
test_data := OUTPUT(SORT(ds_tab, wi, cls), NAMED('Test_Stats'));
// Run it
LR_module := LR.BinomialLogisticRegression(max_iterations);
mod := LR_module.GetModel(ind_data, resp_data);
reports := LR.ExtractReport(mod);
conf_det := LR_module.Report(mod, ind_data, resp_data);
conf_rpt := LR.BinomialConfusion(conf_det);
confusion_report := OUTPUT(ENTH(conf_rpt, 100), NAMED('Sample_Confusion_Report'));
rpt_smpls := OUTPUT(ENTH(reports,100), NAMED('Sample_Reports'));
bad_reports := reports(NOT EXISTS(stats));
bad := OUTPUT(CHOOSEN(bad_reports, 50), NAMED('No_Stats'));
f_tab := TABLE(reports,
              {ave_records:=AVE(GROUP,obs), min_records:=MIN(GROUP,obs),
               max_records:=MAX(GROUP,obs), ave_ind:=AVE(GROUP, ind_vars),
               min_ind:=MIN(GROUP,ind_vars), max_ind:=MAX(GROUP,ind_vars),
               ave_iter:=AVE(GROUP, AVE(stats, iterations)),
               min_iter:=MIN(GROUP, MIN(stats, iterations)),
               max_iter:=MAX(GROUP, MAX(stats, iterations)),
               converged:=SUM(GROUP, COUNT(stats(max_delta<=0.00000001))),
               perfect:=SUM(GROUP, COUNT(stats(incorrect=0))),
               local_IRLS:=SUM(GROUP,IF(builder=lcl, 1, 0)),
               global_IRLS:=SUM(GROUP,IF(builder=gbl, 1, 0))},
              FEW, UNSORTED);
f_stat := OUTPUT(f_tab, NAMED('Run_stats'));
wrec := {RECORDOF(ind_data), UNSIGNED cls};
lbl_data := NORMALIZE(obs, LEFT.explan_vars,
                      TRANSFORM(wrec, SELF.cls:=LEFT.resp_var.value, SELF:=RIGHT));
lbl_tab := TABLE(lbl_data, {wi, number, cls,
                            var:=VARIANCE(GROUP,value),
                            av:=AVE(GROUP, value),
                            mn:=MIN(GROUP, value),
                            mx:=MAX(GROUP, value)},
                 wi, number, cls, FEW, UNSORTED);
 rng_tab := TABLE(lbl_tab, {wi, number,
                            c0_mn:=MAX(GROUP, IF(cls=0, mn, 0)),
                            c0_mx:=MAX(GROUP, IF(cls=0, mx, 0)),
                            c1_mn:=MAX(GROUP, IF(cls=1, mn, 0)),
                            c1_mx:=MAX(GROUP, IF(cls=1, mx, 0))},
                   wi, number, FEW, UNSORTED);
 overlap(REAL8 r0a, REAL8 r0b, REAL8 r1a, REAL8 r1b) :=
                  r0a BETWEEN r1a AND r1b OR r0b BETWEEN r1a AND r1b
               OR r1a BETWEEN r0a AND r0b OR r1b BETWEEN r0a AND r0b;
 rng_rpt := TABLE(rng_tab,
                 {wi, BOOLEAN converged:=FALSE,
                  sep:=SUM(GROUP, IF(c0_mx<c1_mn OR c0_mn>c1_mx, 1, 0)),
                  mrg:=SUM(GROUP, IF(overlap(c0_mn, c0_mx, c1_mn, c1_mx),1,0))},
                 wi, FEW, UNSORTED);
 selected := JOIN(rng_rpt, reports, LEFT.wi=RIGHT.wi,
                  TRANSFORM(RECORDOF(rng_rpt),
                            SELF.converged:=EXISTS(RIGHT.stats(max_delta<=0.00000001)),
                            SELF:=LEFT),
                  LOOKUP);
 sel_rpt := OUTPUT(TOPN(selected, 200, converged, wi), ALL, NAMED('Select_Report'));
EXPORT RunBinomial := PARALLEL(f_stat, rpt_smpls, bad);
