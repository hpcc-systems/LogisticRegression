IMPORT $ AS LR;
IMPORT LR.Types;
IMPORT ML_Core.Math AS Core_Math;

// aliases
AOD := Types.AOD_Record;
Dev := Types.Deviance_Record;

/**
  * Analysis of Deviance Report.
  * <p>Compare deviance information between two models, a base and and proposed
  * model.
  *
  * <p>Analysis of Deviance is analogous to the Analysis of Variance (ANOVA) used in
  * least-squares modeling, but adapted to the general linear model (GLM).  In this case
  * it is adapted specifically to the logistic model.
  *
  * <p>The inputs are the deviance records for each model as obtained from a call
  * to Model_Deviance.
  *
  * @param proposed deviance records of the proposed model.
  * @param base deviance records of the base model for comparison.
  * @return the comparison of the deviance between the models in AOD_Record format.
  * @see Model_Deviance
  * @see Types.Deviance_Record
  * @see Types.AOD_Record
  */
EXPORT DATASET(Types.AOD_Record)
       Deviance_Analysis(DATASET(Types.Deviance_Record) proposed,
                         DATASET(Types.Deviance_Record) base) := FUNCTION
  AOD cvt(Dev dr) := TRANSFORM
    SELF.residual_DF := dr.df;
    SELF.residual_dev := dr.deviance;
    SELF.wi := dr.wi;
    SELF.classifier := dr.classifier;
    SELF := [];
  END;
  c1 := PROJECT(base, cvt(LEFT));
  AOD cmpr(Dev p, Dev b) := TRANSFORM
    dev := b.deviance - p.deviance;
    df := b.df - p.df;
    SELF.residual_DF := p.df;
    SELF.residual_dev := p.deviance;
    SELF.df := df;
    SELF.deviance := dev;
    SELF.p_value := 1.0 - Core_Math.Distributions.Chi2_CDF(ABS(dev), df);
    SELF := p;
  END;
  c2 := JOIN(proposed, base,
            LEFT.wi=RIGHT.wi AND LEFT.classifier=RIGHT.classifier,
            cmpr(LEFT,RIGHT), SMART);
  RETURN c1 + c2;
END;