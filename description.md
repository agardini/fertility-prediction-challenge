# Description of submission

## Pre processing
The preprocessing steps primarily involve recoding certain categorical variables, removing variables with a high proportion of missing values in the training set, and applying various transformations to the numeric variables.

## Developping a model
We rely on the Sparse Wrapper AlGorithm (SWAG) presented in (https://doi.org/10.48550/arXiv.2006.12837), employing random forest as base models. The SWAG explores the low-dimensional attribute space to identify a set of learners that use a minimal number of attributes yet possess high predictive power. The algorithm follows a forward-step approach, beginning with a few attributes and progressively adding more in each step. At each fixed number of attributes, it tests various randomly selected learners and selects those with the best training performance. It leverages the information from the top-performing learners at each step to inform the construction and testing of learners in subsequent steps. Ultimately, it produces a set of random forest that considers different combination of variables.

## Prediction
To perform prediction on the test set which contain missing data we consider, for each row to predict, we check am

