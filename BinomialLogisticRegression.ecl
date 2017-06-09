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
  := MODULE(Interfaces.IClassify)
  /**
   * Calculate the model to fit the observation data to the observed
   * classes.
   * @param observations the observed explanatory values
   * @param classifications the observed classification used to build
   * the model
   * @return the encoded model
   */
  EXPORT DATASET(Types.Layout_Model)
        GetModel(DATASET(Types.NumericField) observations,
                 DATASET(Types.DiscreteField) classifications)
    := LR.IRLS.GetModel(observations, classifications, max_iter, epsilon, ridge);
  /**
   * Classify the observations using a model.
   * @param model The model, which must be produced by a corresponding
   * getModel function.
   * @param new_observations observations to be classified
   * @return Classification with a confidence value
   */
  EXPORT DATASET(Types.Classify_Result)
        Classify(DATASET(Types.Layout_Model) model,
                 DATASET(Types.NumericField) new_observations)
    := LR.LogitPredict(LR.ExtractBeta(model), new_observations);
  /**
   * Report the confusion matrix for the classifier and training data.
   * @param model the encoded model
   * @param observations the explanatory values.
   * @param classifications the classifications associated with the
   * observations
   * @return the confusion matrix showing correct and incorrect
   * results
   */
  EXPORT DATASET(Types.Confusion_Detail)
        Report(DATASET(Types.Layout_Model) model,
               DATASET(Types.NumericField) observations,
               DATASET(Types.DiscreteField) classifications)
    := LR.Confusion(classifications,
                    LR.LogitPredict(LR.ExtractBeta(model),observations));

END;