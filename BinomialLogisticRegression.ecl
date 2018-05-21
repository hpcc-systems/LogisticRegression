IMPORT $ AS LR;
IMPORT LR.Constants;
IMPORT ML_Core.Interfaces;
IMPORT ML_Core.Types AS Types;

/**
  * Binomial logistic regression using iteratively re-weighted least
  * squares.
  *
  * @param max_iter (Optional) The maximum number of iterations to try.  Default = 200.
  * @param epsilon (Optional) The minimum change in the Beta value estimate to continue
  * @param ridge (Optional) A value to populate a diagonal matrix that is added to
  * a matrix help assure that the matrix is invertible.
  */
EXPORT BinomialLogisticRegression(UNSIGNED max_iter=200,
               REAL8 epsilon=Constants.default_epsilon,
               REAL8 ridge=Constants.default_ridge)
  := MODULE(Interfaces.IClassify)
  /**
    * Calculate the model to fit the observation data to the observed
    * classes.
    * @param observations the observed explanatory values in NumericField
    *                     format.
    * @param classifications the observed classification used to build
    * the model in DiscreteField format.
    * @return the encoded model in Layout_Model format.
    * @see ML_Core.Types.NumericField
    * @see ML_Core.Types.DiscreteField
    * @see ML_Core.Types.Layout_Model
    */
  EXPORT DATASET(Types.Layout_Model)
        GetModel(DATASET(Types.NumericField) observations,
                 DATASET(Types.DiscreteField) classifications)
    := LR.IRLS.GetModel(observations, classifications, max_iter, epsilon, ridge);
  /**
   * Classify the observations using a model as previously returned from GetModel.
   * @param model The model in Layout_Model format.
   * @param new_observations observations to be classified in NumericField format.
   * @return Classification with a confidence value in Classify_Result format.
   * @see ML_Core.Types.Layout_Model
   * @see ML_Core.Types.NumericField
   * @see ML_Core.Types.Classify_Result
   */
  EXPORT DATASET(Types.Classify_Result)
        Classify(DATASET(Types.Layout_Model) model,
                 DATASET(Types.NumericField) new_observations)
    := LR.LogitPredict(LR.ExtractBeta(model), new_observations);
  /**
   * Report the confusion matrix for the classifier and training data.
   * @param model the encoded model as returned from GetModel.
   * @param observations the explanatory values in NumericField format.
   * @param classifications the actual classifications associated with the
   * observations (i.e. ground truth) in DiscreteField format.
   * @return the confusion matrix showing correct and incorrect
   * results in Confusion_Detail format.
   * @see ML_Core.Types.NumericField
   * @see ML_Core.Types.DiscreteField
   * @see ML_Core.Types.ConfusionDetail
   */
  EXPORT DATASET(Types.Confusion_Detail)
        Report(DATASET(Types.Layout_Model) model,
               DATASET(Types.NumericField) observations,
               DATASET(Types.DiscreteField) classifications)
    := LR.Confusion(classifications,
                    LR.LogitPredict(LR.ExtractBeta(model),observations));

END;
