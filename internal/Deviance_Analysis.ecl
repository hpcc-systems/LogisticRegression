/*##################################################################################
## HPCC SYSTEMS software Copyright (C) 2017,2021 HPCC Systems.  All rights reserved.
################################################################################# */

IMPORT $ AS LR;
IMPORT LR.Types;
IMPORT ML_Core.Math AS Core_Math;

// aliases
AOD := Types.AOD_Record;
Dev := Types.Deviance_Record;

/**
 * Compare deviance information for an analysis of deviance.
 * @param proposed the proposed model
 * @param base the base model for comparison
 * @return the comparison of the deviance between the models
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