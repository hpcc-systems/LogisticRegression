//*******************************************************
num_work_items := 40;
avg_size := 40000;
//*******************************************************
IMPORT $.^ AS LR;
IMPORT ML_Core.Types AS Core_Types;
NumericField := Core_Types.NumericField;
DiscreteField := Core_Types.DiscreteField;
lcl := LR.Constants.builder_irls_local;
gbl := LR.Constants.builder_irls_global;

Model_Seed := RECORD
  REAL8 f1_yes;
  REAL8 f1_no;
  REAL8 f1_rng;
  REAL8 f2_yes;
  REAL8 f2_no;
  REAL8 f2_rng;
  REAL8 f3_yes;
  REAL8 f3_no;
  REAL8 f3_rng;
  REAL8 f4_yes;
  REAL8 f4_no;
  REAL8 f4_rng;
END;
Observation := RECORD
  UNSIGNED4 wi;
  UNSIGNED4 id;
  INTEGER resp;
  REAL8 f1;
  REAL8 f2;
  REAL8 f3;
  REAL8 f4;
END;
seeds := DATASET([{ 20,   5,   5, 300,   5,  50,  10, 100,  10,  10,   1,   2},
                  {800, 900,  10,  10,   8,   5,  90, 900,  20, 100,   1,   1}
                 ], Model_Seed);
Enum_Seed := RECORD(Model_Seed)
  UNSIGNED id;
END;
e_seeds := PROJECT(seeds, TRANSFORM(Enum_Seed, SELF.id:=COUNTER, SELF:=LEFT));
Base := RECORD
  UNSIGNED4 wi;
  UNSIGNED4 size;
  UNSIGNED4 seed;
  Model_Seed;
END;
Base make_base(UNSIGNED c) := TRANSFORM
  SELF.wi := c;
  SELF.size := MAX(avg_size - 999 + RANDOM()%2000, 10);
  SELF.seed := 1 + RANDOM() % COUNT(seeds);    // draw a seed
  SELF := [];
END;
bases := DISTRIBUTE(DATASET(num_work_items, make_base(COUNTER)), wi);
Base extend_base(Base br, Enum_Seed es) := TRANSFORM
  SELF := es;
  SELF := br;
END;
ex_bases := JOIN(bases, e_seeds, LEFT.seed=RIGHT.id,
                 extend_base(LEFT, RIGHT), LOOKUP) : ONWARNING(1005,ignore);

REAL8 delta(REAL8 y) := (((RANDOM()%100)/100) - 0.5)*y;
Observation make_obs(Base br, UNSIGNED c) := TRANSFORM
  SELF.wi := br.wi;
  SELF.id := c;
  SELF.resp := c%2;
  SELF.f1 := IF(c%2=0, br.f1_yes, br.f1_no) + delta(br.f1_rng);
  SELF.f2 := IF(c%2=0, br.f2_yes, br.f2_no) + delta(br.f2_rng);
  SELF.f3 := IF(c%2=0, br.f3_yes, br.f3_no) + delta(br.f3_rng);
  SELF.f4 := IF(c%2=0, br.f4_yes, br.f4_no) + delta(br.f4_rng);
END;
obs := NORMALIZE(ex_bases, LEFT.size, make_obs(LEFT, COUNTER));

t0 := TABLE(obs, {wi, resp, num:=COUNT(GROUP),
                  f1_avg:=AVE(GROUP,f1), f1_var:=SQRT(VARIANCE(GROUP,f1)),
                  f2_ave:=AVE(GROUP,f2), f2_var:=SQRT(VARIANCE(GROUP,f2)),
                  f3_ave:=AVE(GROUP,f3), f3_var:=SQRT(VARIANCE(GROUP,f3)),
                  f4_ave:=AVE(GROUP,f4), f4_var:=SQRT(VARIANCE(GROUP,f4))},
                  wi, resp, MERGE);
DiscreteField get_resp(Observation ob) := TRANSFORM
  SELF.value := ob.resp;
  SELF.number := 1;
  SELF := ob;
END;
resp_data := PROJECT(obs, get_resp(LEFT));

NumericField get_ind(Observation ob, UNSIGNED c) := TRANSFORM
  SELF.number := c;
  SELF.value := CHOOSE(c, ob.f1, ob.f2, ob.f3, ob.f4);
  SELF := ob;
END;
ind_data := NORMALIZE(obs, 4, get_ind(LEFT, COUNTER));
// Run it
LR_module := LR.BinomialLogisticRegression();
mod := LR_module.GetModel(ind_data, resp_data);
reports := LR.ExtractReport(mod);
rpt_samples := OUTPUT(ENTH(reports,100), NAMED('Sample_Reports'));
bad_reports := reports(NOT EXISTS(stats));
bad := OUTPUT(CHOOSEN(bad_reports, 50), NAMED('No_Stats'));
f_tab := TABLE(reports,
              {ave_records:=AVE(GROUP,obs), min_records:=MIN(GROUP,obs),
               max_records:=MAX(GROUP,obs), ave_ind:=AVE(GROUP, ind_vars),
               min_ind:=MIN(GROUP,ind_vars), max_ind:=MAX(GROUP,ind_vars),
               ave_iter:=AVE(GROUP, AVE(stats, iterations)),
               min_iter:=MIN(GROUP, MIN(stats, iterations)),
               max_iter:=MAX(GROUP, MAX(stats, iterations)),
               local_IRLS:=SUM(GROUP,IF(builder=lcl, 1, 0)),
               global_IRLS:=SUM(GROUP,IF(builder=gbl, 1, 0))},
              FEW, UNSORTED);
f_stat := OUTPUT(f_tab, NAMED('Run_stats'));
EXPORT RunBinomial := PARALLEL(f_stat, rpt_samples, bad);
