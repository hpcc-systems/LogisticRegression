# LogisticRegression
Bundle for binomial and multinomial Logistic Regression.

This bundle can automatically choose between binomial and multinomial model based on the training dataset.
The implementation of Binomial logistic regression uses iteratively re-weighted least squares and Multinomial logistic regression uses Softmax.

## Installation

Make sure intall bundle dependencies: `ML_Core` and `PBblas`. To install Logistic Regresson bundle, run:

> ecl bundle install https://github.com/hpcc-systems/LogisticRegression.git



## Examples

Under `/validation` folder, `BinomialRegression.ecl` and `MultinomialLogisticRegression.ecl` show how to use Logistic Regression to predict binary and multi-classess classification tasks respectively. The definition of training dataset is in `IrisDS.ecl` file.

## More

For more details and other machine learning bundles, please visit [HPCC Machine Learning Library](http://hpccsystems.com/download/free-modules/machine-learning-library)
