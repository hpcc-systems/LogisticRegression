/*##################################################################################
## HPCC SYSTEMS software Copyright (C) 2017,2021 HPCC Systems.  All rights reserved.
################################################################################# */

IMPORT $.internal AS LR;
IMPORT LR.Constants;
IMPORT ML_Core;
IMPORT ML_Core.Interfaces;
IMPORT ML_Core.Types AS Types;

Layout_Model2 := Types.Layout_Model2;
Classify_Result := Types.Classify_Result;
NumericField := Types.NumericField;
DiscreteField := Types.DiscreteField;
Confusion_Detail := Types.Confusion_Detail;
Classification_Accuracy := Types.Classification_Accuracy;
Class_Accuracy := Types.Class_Accuracy;

/**
 * Logistic Regression is a statisic model designed for classification tasks.
 * This Logistic Regression bundle is suited for both binary and multi-classes classification
 * tasks. It can automatically choose between binomial and multinomial model based on
 * training dataset. The implementation of Binomial logistic regression uses iteratively
 * re-weighted least squares and Multinomial logistic regression uses Softmax. Myriad interface
 * is suppported in the current version.
 *
 * @param max_iter the maximum iterations that is allowed before converge.
 *                 default value is 100.
 * @param epsilon  the minimum change in the Beta value estimate to continue for binary prediction.
 *                 Default value is 0.00000001.
 * @param ridge    a value to populate a diagonal matrix that is added to a matrix help assure
 *                 that the matrix is invertible for binary prediction. Default value is 0.00001.
 * @param alpha    learning rate for multi-classes prediction. Defualt value is 0.00005.
 * @param tol      converge threshold of loss change for multi-classes prediction.
 *                 Defalt value is 0.0001.
 */
EXPORT LogisticRegression(UNSIGNED max_iter=100,
                                  REAL8 epsilon=Constants.default_epsilon,
                                  REAL8 ridge=Constants.default_ridge,
                                  REAL alpha = 0.00005,
                                  REAL tol = 0.0001):= MODULE(ML_Core.Interfaces.IClassify2)
  /**
    * Calculate the model to fit independent data to the observed classes (i.e. dependent data).
    *
    * @param indepenedents The observed independent (explanatory) values with sequential
    *                      record id starting from 1.
    * @param dependents    The observed dependent(class label) values with sequential id
    *                      starting from 1. Class label is 0 or 1 for binary classification
    *                      task. Class label starts from 1 and locates at number field '1'
    *                      for multi-classes classification task.
    *
    * @return The encoded model.
    * @see Types.Layout_Model2
    * @see Types.NumericField
    * @see Types.DiscreteField
    */
  EXPORT DATASET(Types.Layout_Model2)
        GetModel(DATASET(Types.NumericField) independents,
                 DATASET(Types.DiscreteField) dependents):= FUNCTION
    // Check the number of classes.
    dep_cat0 := TABLE(dependents, {wi, number, value}, wi, number, value);
    dep_cat := TABLE(dep_cat0, {wi, number, cnt := COUNT(GROUP)}, wi, number);
    dep_ifBinary := IF(EXISTS(dep_cat(cnt > 2)), FALSE, TRUE);
    RETURN IF(dep_ifBinary,
         LR.IRLS.GetModel(independents, dependents, max_iter, epsilon, ridge),
         LR.multinomialLogisticRegression(max_iter,alpha,tol).getModel(independents,dependents));

  END;
  /**
    * Classify the observations using a model.
    *
    * @param model The model, which must be produced by a corresponding
    *              getModel function.
    * @param observations New observations (independent data) to be classified.
    * @return Predicted class values.
    *
    */
    EXPORT DATASET(Types.DiscreteField)
        Classify(DATASET(Types.Layout_Model2) model,
                 DATASET(Types.NumericField) observations) :=FUNCTION
       ifBinary := IF(model(wi=0)[1].value=1, TRUE, FALSE);
       RETURN IF(ifBinary,
          LR.LogitPredict(LR.ExtractBeta(model), observations),
          LR.multinomialLogisticRegression(max_iter,alpha,tol).classify(model,observations));
    END;
END;