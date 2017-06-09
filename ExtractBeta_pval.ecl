IMPORT $ AS LR;
IMPORT LR.Types AS Types;
IMPORT ML_Core.Types AS Core_Types;


// aliases
Model_Coef := Types.Model_Coef;
id_base := LR.Constants.id_base;
t_work_item := Core_Types.t_work_item;
Model := Core_Types.Layout_Model;
pval_Rec := Types.pval_Model_Coef;
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
 * @return the beta values with p-values as Model Coefficient records,
 * zero as the constant
 * term.
 */
EXPORT DATASET(Types.pval_Model_Coef)
    ExtractBeta_pval(DATASET(Core_Types.Layout_Model) mod_ds):=FUNCTION
  // get the betas with the standard error values
  ds := LR.ExtractBeta(mod_ds);
  //Calculate p-values
  pval_Rec calc(Model_Coef mod) := TRANSFORM
    Z := mod.w / mod.SE;
    SELF.z := Z;
    SELF.p_value := 2*(1.0 - LR.Distributions.Normal_CDF(ABS(Z)));
    SELF := mod;
  END;
  rslt := PROJECT(ds, calc(LEFT));
  RETURN rslt;
END;