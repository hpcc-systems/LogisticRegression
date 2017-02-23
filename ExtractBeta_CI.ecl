IMPORT $ AS LR;
IMPORT LR.Types AS Types;
IMPORT ML_Core.Types AS Core_Types;

// aliases
Model_Coef := Types.Model_Coef;
id_base := LR.Constants.id_base;
t_work_item := Core_Types.t_work_item;
Model := Core_Types.Layout_Model;
CI_Rec := Types.Confidence_Model_Coef;
// helpers
df_rec := RECORD
  t_work_item wi;
  REAL8 df;
END;
df_rec make_df(Model obs, model vars) := TRANSFORM
  SELF.wi := obs.wi;
  SELF.df := obs.value - vars.value - 1;
END;

/**
 * Extract the beta values form the model dataset.
 * @param mod_ds the model dataset
 * @param level the significance value for the intervals
 * @return the beta values with confidence intervals
 * term.
 */
EXPORT DATASET(Types.Confidence_Model_Coef)
    ExtractBeta_CI(DATASET(Core_Types.Layout_Model) mod_ds, REAL8 level)
    :=FUNCTION
  // get the betas with the standard error values
  ds := LR.ExtractBeta(mod_ds);
  // find the degrees of freedom
  nf := mod_ds(id=id_base AND number=LR.Constants.base_ind_vars);
  no := mod_ds(id=id_base AND number=LR.Constants.base_obs);
  df := JOIN(no, nf, LEFT.wi=RIGHT.wi, make_df(LEFT,RIGHT), LOOKUP);
  // determine confidence interval
  CI_Rec calc_ci(Model_Coef coef, df_rec d):=TRANSFORM
    margin := LR.Distributions.T_PPF((1.0-level)/2, d.df);
    SELF.upper := coef.w + margin*coef.SE;
    SELF.lower := coef.w - margin*coef.SE;
    SELF := coef;
  END;
  RETURN JOIN(ds, df, LEFT.wi=RIGHT.wi, calc_ci(LEFT, RIGHT), LOOKUP);
END;