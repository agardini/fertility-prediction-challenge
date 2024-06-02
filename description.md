# Description of submission

## Pre processing
The preprocessing steps primarily involve recoding some categorical variables, removing variables with a high proportion of missing values in the training set, and adding some transformed numeric variables.

## Developping a model
We rely on the  [Sparse Wrapper AlGorithm (SWAG)](https://doi.org/10.48550/arXiv.2006.12837), employing [random
forest](https://link.springer.com/article/10.1023/a:1010933404324) as base model and considering the implementation proposed by [Wright et a.](https://www.jstatsoft.org/article/view/v077i01). The SWAG explores the low-dimensional attribute space to identify a set of learners that use a minimal number of attributes yet possess high predictive power. The algorithm follows a forward-step approach, beginning with a few attributes and progressively adding more in each step. At each fixed number of attributes, it tests various randomly selected learners and selects those with the best out-of-sample performance. The algorithm uses the information coming from the best learners at the previous step to build and test learners in the following step. We then construct a set of best performing models based on the estimated out-of-sample performance estimated by a 10 fold cross validation and considering the F1 score.

## Prediction
To predict on the test set with missing data, we use the following strategy. First, we create a copy of the test set where missing data is imputed using the method proposed by [Stekhoven et al.](https://academic.oup.com/bioinformatics/article/28/1/112/219101?login=false
). For each row in the test set, we identify all applicable models from the set of best-performing models. A model is applicable if it uses only the variables observed in that row. We then predict the outcome for each row using all applicable models and select the most frequent class among these predictions as the final prediction for that row. If no top-performing models can be applied to a row due to missing variables, we use the imputed copy of the row. Predictions are then made using all best-performing models, and the most frequent class among these predictions is taken as the final prediction.
