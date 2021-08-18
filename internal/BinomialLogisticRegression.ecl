/*##################################################################################
## HPCC SYSTEMS software Copyright (C) 2017,2021 HPCC Systems.  All rights reserved.
################################################################################# */

IMPORT $ AS LR;
IMPORT LR.Constants;
IMPORT ML_Core.Interfaces;
IMPORT ML_Core.Types AS Types;

/**
 * Binomial logistic regression using iteratively re-weighted least
 * squares.
 * @param max_iter maximum number of iterations to try
 * @param epsilon the minimum change in the Beta value estimate to continue
 * @param ridge a value to populate a diagonal matrix that is added to
 * a matrix help assure that the matrix is invertible.
 */
EXPORT BinomialLogisticRegression(UNSIGNED max_iter=200,
               REAL8 epsilon=Constants.default_epsilon,
               REAL8 ridge=Constants.default_ridge)
  := MODULE(Interfaces.IClassify2)
  /**
   * Calculate the model to fit the observation data to the observed
   * classes.
   * @param independents the observed explanatory values
   * @param dependents the observed classification used to build
   * the model
   * @return the encoded model
   */
  EXPORT DATASET(Types.Layout_Model2)
        GetModel(DATASET(Types.NumericField) independents,
                 DATASET(Types.DiscreteField) dependents)
    := LR.IRLS.GetModel(independents, dependents, max_iter, epsilon, ridge);
  /**
   * Classify the independents using a model.
   * @param model The model, which must be produced by a corresponding
   * getModel function.
   * @param observations independents to be classified
   * @return Classification with a confidence value
   */
  EXPORT DATASET(Types.DiscreteField)
        Classify(DATASET(Types.Layout_Model2) model,
                 DATASET(Types.NumericField) observations)
    := LR.LogitPredict(LR.ExtractBeta(model), observations);

END;
