IMPORT $ AS LR;
IMPORT LR.Types AS Types;
IMPORT ML_Core.Types AS Core_Types;

/**
 * Extract the beta values form the model dataset.
 * @param mod_ds the model dataset
 * @return the beta values with p-values as Model Coefficient records,
 * zero as the constant
 * term.
 */
EXPORT DATASET(Types.pval_Model_Coef)
    ExtractBeta_pval(DATASET(Core_Types.Layout_Model) mod_ds):=FUNCTION
  ds := LR.ExtractBeta_full(mod_ds);
  rslt := PROJECT(ds, Types.pval_Model_Coef);
  RETURN rslt;
END;