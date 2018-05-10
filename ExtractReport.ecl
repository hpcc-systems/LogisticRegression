IMPORT $ AS LR;
IMPORT LR.Types AS Types;
IMPORT LR.Constants AS Constants;
IMPORT ML_Core.Types AS Core_Types;

// convenience aliases
NumericField := Core_Types.NumericField;
Layout_Model := Core_Types.Layout_Model;
id_base := LR.Constants.id_base;
id_iters := LR.Constants.id_iters;
id_delta := LR.Constants.id_delta;
id_betas := LR.Constants.id_betas;
base_max_iter := LR.Constants.base_max_iter;
base_epsilon := LR.Constants.base_epsilon;
base_ind_vars := LR.Constants.base_ind_vars;
base_dep_vars := LR.Constants.base_dep_vars;
base_obs := LR.Constants.base_obs;
base_builder := LR.Constants.base_builder;
id_correct := LR.Constants.id_correct;
id_incorrect := LR.Constants.id_incorrect;
Model_Report := Types.Model_Report;
Stats := Types.Classifier_Stats;
Ex_Stats := RECORD(Stats)
  Core_Types.t_work_item wi;
END;
/**
 * Create a model report from a model.
 *
 * @param mod_ds the model as returned from GetModel.
 * @return the model report in Model_Report format.
 * @see Types.Model_Report
 */
EXPORT DATASET(Types.Model_Report)
       ExtractReport(DATASET(Core_Types.Layout_Model) mod_ds):=FUNCTION
  grp_mod_ds := GROUP(mod_ds(id<id_betas), wi, ALL);
  base_mod := grp_mod_ds(id=id_base);
  stat_mod := grp_mod_ds(id IN Constants.id_stat_set);
  Types.Model_Report pick1(DATASET(Layout_Model) m_ds) := TRANSFORM
    SELF.wi := m_ds[1].wi;
    SELF.max_iterations := m_ds(number=base_max_iter)[1].value;
    SELF.epsilon := m_ds(number=base_epsilon)[1].value;
    SELF.dep_vars := m_ds(number=base_dep_vars)[1].value;
    SELF.ind_vars := m_ds(number=base_ind_vars)[1].value;
    SELF.obs := m_ds(number=base_obs)[1].value;
    SELF.builder := m_ds(number=base_builder)[1].value;
    SELF.stats := [];
  END;
  base_rpt := ROLLUP(base_mod, GROUP, pick1(ROWS(LEFT)));
  Ex_Stats roll_stats(DATASET(Layout_Model) s_ds) := TRANSFORM
    SELF.wi := s_ds[1].wi;
    SELF.column := s_ds[1].number;
    SELF.max_delta := s_ds(id=id_delta)[1].value;
    SELF.iterations := s_ds(id=id_iters)[1].value;
    SELF.correct := s_ds(id=id_correct)[1].value;
    SELF.incorrect := s_ds(id=id_incorrect)[1].value;
  END;
  grp_stats := GROUP(SORT(stat_mod, number), wi, number);
  stats_block := ROLLUP(grp_stats, GROUP, roll_stats(ROWS(LEFT)));
  rpt := DENORMALIZE(base_rpt, stats_block, LEFT.wi=RIGHT.wi, GROUP,
                     TRANSFORM(Types.Model_Report,
                               SELF.stats:=PROJECT(ROWS(RIGHT), Stats),
                               SELF := LEFT), LOCAL);
  RETURN rpt;
END;