IMPORT $ AS LR;
IMPORT LR.Types;

// aliases
Dev_Rec := Types.Deviance_Record;
Obs_Dev := Types.Observation_Deviance;

/**
 * Deviance for the null model, that is, a model with only an intercept.
 * @param od Observation Deviance record set.
 * @return a data set of the null model deviances for each work item and
 * classifier.
 */
EXPORT DATASET(Types.Deviance_Record)
       Null_Deviance(DATASET(Types.Observation_Deviance) od):=FUNCTION
  grp_od := GROUP(od, wi, classifier, ALL);
  Dev_Rec roll_d(Obs_Dev frst, DATASET(Obs_Dev) rws) := TRANSFORM
    SELF.df := COUNT(rws) - 1;
    SELF.deviance := -2 * SUM(rws, nil_ll);
    SELF.AIC := SELF.deviance + 2;
    SELF := frst;
  END;
  rslt := ROLLUP(grp_od, GROUP, roll_d(LEFT, ROWS(LEFT)));
  RETURN rslt;
END;