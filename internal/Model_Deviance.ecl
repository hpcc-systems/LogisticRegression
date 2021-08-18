/*##################################################################################
## HPCC SYSTEMS software Copyright (C) 2017,2021 HPCC Systems.  All rights reserved.
################################################################################# */

IMPORT $ AS LR;
IMPORT LR.Types;

// aliases
Dev_Rec := Types.Deviance_Record;
Obs_Dev := Types.Observation_Deviance;

/**
 * Model Deviance.
 * @param od observation deviance record
 * @param mod model co-efficients
 * @return model deviance
 */
EXPORT DATASET(Types.Deviance_Record)
       Model_Deviance(DATASET(Types.Observation_Deviance) od,
                      DATASET(Types.Model_Coef) mod) := FUNCTION
  // get model parameter counts
  p := TABLE(mod, {wi, dep_nom, parameters:=COUNT(GROUP)},
             wi, dep_nom, FEW, UNSORTED);
  grp_od := GROUP(od, wi, classifier, ALL);
  Dev_Rec roll_d(Obs_Dev frst, DATASET(Obs_Dev) rws) := TRANSFORM
    SELF.df := COUNT(rws);
    SELF.deviance := -2 * SUM(rws, mod_ll);
    SELF.AIC := 0.0;
    SELF := frst;
  END;
  md1 := ROLLUP(grp_od, GROUP, roll_d(LEFT, ROWS(LEFT)));
  Dev_Rec adj_df(Dev_Rec dr, RECORDOF(p) mod) := TRANSFORM
    SELF.df := dr.df - mod.parameters;
    SELF.AIC := dr.deviance + 2 * mod.parameters;
    SELF := dr;
  END;
  rslt := JOIN(md1, p, LEFT.wi=RIGHT.wi AND LEFT.classifier=RIGHT.dep_nom,
               adj_df(LEFT, RIGHT), LOOKUP);
  RETURN rslt;
END;