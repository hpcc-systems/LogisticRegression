IMPORT $ AS LR;
IMPORT LR.Types AS Types;
IMPORT ML_Core.Types AS Core_Types;


/**
  * Extract the beta values and confidence intervals from the model dataset.
  * @param mod_ds the model as returned from GetModel.
  * @param level the significance value for the intervals.
  * @return the beta values with confidence intervals in Confidence_Model_Coef
  *         format, with zero as the constant term.
  * @see Types.Confidence_Model_Coef
  */
EXPORT DATASET(Types.Confidence_Model_Coef)
    ExtractBeta_CI(DATASET(Core_Types.Layout_Model) mod_ds, REAL8 level)
    :=FUNCTION
  ds := LR.ExtractBeta_full(mod_ds, level);
  RETURN PROJECT(ds, Types.Confidence_Model_Coef);
END;