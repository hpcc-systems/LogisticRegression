/*##################################################################################
## HPCC SYSTEMS software Copyright (C) 2017,2021 HPCC Systems.  All rights reserved.
################################################################################# */

IMPORT $ AS LR;
IMPORT LR.Types AS Types;
IMPORT ML_Core.Types AS Core_Types;

// convenience aliases
id_betas := LR.Constants.id_betas;
id_betas_coef := LR.Constants.id_betas_coef;
id_betas_SE := LR.Constants.id_betas_SE;
id_base := LR.Constants.id_base;
base_ind_vars := LR.Constants.base_ind_vars;
Model_Coef := Types.Model_Coef;
/**
 * Extract the beta values form the model dataset.
 * @param mod_ds the model dataset
 * @return a beta values as Model Coefficient records, zero as the constant
 * term.
 */
EXPORT ExtractBeta(DATASET(Core_Types.Layout_Model2) mod_ds):=FUNCTION
  full := LR.ExtractBeta_full(mod_ds);
  rslt := PROJECT(full, Model_Coef);
  RETURN rslt;
END;