IMPORT $ AS LR;
IMPORT LR.Types AS Types;
IMPORT ML_Core.Types AS Core_Types;

/**
  * Extract the beta values including z and p value from the model.
  *
  * @param mod_ds the model as returned from GetModel.
  * @return the beta values with p-values in pval_Model_Coef format,
  * with zero as the constant term.
  * @see Types.pval_Model_Coef
  */
EXPORT DATASET(Types.pval_Model_Coef)
    ExtractBeta_pval(DATASET(Core_Types.Layout_Model) mod_ds):=FUNCTION
  ds := LR.ExtractBeta_full(mod_ds);
  rslt := PROJECT(ds, Types.pval_Model_Coef);
  RETURN rslt;
END;