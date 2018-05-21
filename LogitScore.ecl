IMPORT $ AS LR;
IMPORT LR.Types;
IMPORT ML_Core.Types AS Core_Types;

//aliases
NumericField := Core_Types.NumericField;
Raw_Prediction := Types.Raw_Prediction;
Model_Coef := Types.Model_Coef;

/**
 * Calculate the score using the logit function and the
 * the supplied beta coefficients.
 *
 * @param coef the model beta coefficients as returned from ExtractBetas.
 * @param independents the observations.
 * @return the raw prediction value in Raw_Prediction format.
 * @see ExtractBetas
 * @see Types.Raw_Prediction
 */
EXPORT DATASET(Raw_Prediction)
      LogitScore(DATASET(Model_Coef) coef,
                 DATASET(NumericField) independents) := FUNCTION
  // Make a list of records and start the Y value with the constant
  rids := TABLE(independents, {wi, id}, wi, id, MERGE);
  Raw_Prediction base(RECORDOF(rids) y, Model_Coef b0) := TRANSFORM
    SELF.raw := b0.w;
    SELF.number := b0.dep_nom;
    SELF := y;
  END;
  // constant term
  term0 := JOIN(rids, coef(ind_col=0), LEFT.wi=RIGHT.wi,
                base(LEFT,RIGHT), LOOKUP, MANY);
  // terms 1 through k
  Raw_Prediction mult(NumericField x, Model_Coef b) := TRANSFORM
    SELF.wi := x.wi;
    SELF.id := x.id;
    SELF.number := b.dep_nom;
    SELF.raw := x.value*b.w;
  END;
  termk := JOIN(independents, coef(ind_col > 0),
                LEFT.wi=RIGHT.wi AND LEFT.number=RIGHT.ind_col,
                mult(LEFT,RIGHT), LOOKUP, MANY);
  //check performance, may have a significant distribution cost
  grp_terms := GROUP(SORT(term0+termk, wi, id, number), wi, id, number);
  Raw_Prediction roll_t(Raw_Prediction nf, DATASET(Raw_Prediction) nfs):=TRANSFORM
    SELF.raw := 1.0 / (1 + EXP(-SUM(nfs, raw)));
    SELF := nf
  END;
  sigmoid := ROLLUP(grp_terms, GROUP, roll_t(LEFT, ROWS(LEFT)));
  RETURN UNGROUP(sigmoid);
END;
