IMPORT $ AS LR;
IMPORT LR.Types;

// aliases
Dev_Rec := Types.Deviance_Record;
Obs_Dev := Types.Observation_Deviance;

/**
  * Return Deviance information for the null model, that is,
  * a model with only an intercept.
  *
  * <p>Analysis of Deviance is analogous to the Analysis of Variance (ANOVA) used in
  * least-squares modeling, but adapted to the general linear model (GLM).  In this case
  * it is adapted specifically to the logistic model.
  *
  * @param od Observation Deviance record set as returned from Deviance_Detail.
  * @return a data set of the null model deviances for each work item and
  * classifier in Deviance_Record format.
  * @see Types.Observation_Deviance
  * @see Types.Deviance_Record
  * @see Deviance_Detail
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