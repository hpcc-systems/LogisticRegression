/*##################################################################################
## HPCC SYSTEMS software Copyright (C) 2017,2021 HPCC Systems.  All rights reserved.
################################################################################# */

IMPORT $ AS LR;
IMPORT LR.Types AS Types;
IMPORT ML_Core.Types AS Core_Types;


/**
 * Extract the beta values form the model dataset.
 * @param mod_ds the model dataset
 * @param level the significance value for the intervals
 * @return the beta values with confidence intervals
 * term.
 */
EXPORT DATASET(Types.Confidence_Model_Coef)
    ExtractBeta_CI(DATASET(Core_Types.Layout_Model2) mod_ds, REAL8 level)
    :=FUNCTION
  ds := LR.ExtractBeta_full(mod_ds, level);
  RETURN PROJECT(ds, Types.Confidence_Model_Coef);
END;