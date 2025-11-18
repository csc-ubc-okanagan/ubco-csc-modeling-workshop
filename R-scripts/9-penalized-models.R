library(ISLR)
library(corrplot)

data(diabetes)
Diabetes <- na.omit(diabetes) # remove missing
dim(Diabetes)
# Check correlations
numVars <- Diabetes[, sapply(Diabetes, is.numeric)]
numVars <- numVars[, !colnames(numVars) %in% "Outcome"]
corMat <- cor(numVars) # get correlations
corrplot(corMat, method = 'color',
         type = 'upper') # plot correlations

X <- model.matrix(Outcome ~ ., data = Diabetes)[, -1] # remove intercept
y <- Diabetes$Outcome
set.seed(2025)
trainIDX <- sample(1:nrow(X), size = floor(0.7 * nrow(X)))
testIDX <- setdiff(1:nrow(X), trainIDX)
Xtrain <- X[trainIDX, ]
ytrain <- y[trainIDX]
Xtest <- X[testIDX, ]
ytest <- y[testIDX]

ridgeModel <- glmnet(X, y, alpha = 0, family = "binomial")
plot(ridgeModel, xvar = 'lambda', label = TRUE)
title('Ridge Regression Paths', line = 3)
# coefficients at specific lambdas
coef(ridgeModel, s = 50)
coef(ridgeModel, s = 500)
coef(ridgeModel, s = 5000)
# Take a look at these :)


# Fit lasso regression
# alpha = 1 for lasso
lassoModel <- glmnet(X, y, alpha = 1, family = "binomial")
plot(lassoModel, xvar = 'lambda', label = TRUE)
title('Lasso Regression Paths', line = 3)
# coefficients at specific lambdas (play around with these!)
coef(lassoModel, s = 50)
coef(lassoModel, s = 500)
coef(lassoModel, s = 5000)
# Notice that many coefficients are
# EXACTLY zero (which no longer contribute to the model)


set.seed(2025)
cvRidge <- cv.glmnet(Xtrain,
                     ytrain,
                     alpha = 0,
                     nfolds = 10, 
                     family = "binomial")
plot(cvRidge)
title('Ridge: 10-Fold CV', line = 3)
# best lambda values
cvRidge$lambda.min # min CV error
cvRidge$lambda.1se # 1 SE rule
# optimal coefficients
coef(cvRidge, s = 'lambda.min')

# 10-fold cross-validation for Lasso
set.seed(2025)
cvLasso <- cv.glmnet(Xtrain,
                     ytrain,
                     alpha = 1,
                     nfolds = 10, family = "binomial")
plot(cvLasso)
title('Lasso: 10-Fold CV', line = 3)
# best lambda values
cvLasso$lambda.min
cvLasso$lambda.1se
# optimal coefficients
coef(cvLasso, s = 'lambda.min')


ridgePredict <- predict(cvRidge, s = 'lambda.min', newx = Xtest, type = "response")
lassoPredict <- predict(cvLasso, s = 'lambda.min', newx = Xtest, type = "response")
# we will compare to multiple linear regression as well.
lmModel <- glm(Outcome ~ ., data = Diabetes[trainIDX, ], family = "binomial")
lmPredict <- predict(lmModel, newdata = Diabetes[testIDX, ], type = "response")
accuracy <- function(actual, predicted) {
  predicted <- ifelse(predicted >= 0.5, 1, 0)
  matches <- sum(diag(Thresher::matchLabels(table(actual, predicted))))
  return(matches/length(predicted))
}
accuracyLM <- accuracy(as.numeric(ytest), lmPredict)
accuracyRIDGE <- accuracy(as.numeric(ytest), ridgePredict)
accuracyLASSO <- accuracy(as.numeric(ytest), lassoPredict)
data.frame(
  Model = c('LM', 'Ridge', 'Lasso'),
  accuracy = c(accuracyLM, accuracyRIDGE, accuracyLASSO)
)


lassoCoefficients <- coef(cvLasso,
                          s = 'lambda.min')
lassoCoefficients
# which variables selected?
varsSelected <-
  lassoCoefficients[lassoCoefficients[, 1] != 0, ]
varsSelected


set.seed(2025)
cvENET <- cv.glmnet(Xtrain, ytrain, alpha = 0.5, nfolds = 10, family = "binomial")
plot(cvENET)
title('Elastic Net ( = 0.5): 10-Fold CV', line = 3)
enetPredict <- predict(cvENET, s = 'lambda.min', newx = Xtest, type = "response")
accuracyENET <- accuracy(as.numeric(ytest), enetPredict)
data.frame(
  Model = c('LM', 'Ridge', 'Lasso', 'Elastic Net'),
  accuracy = c(accuracyLM, accuracyRIDGE, accuracyLASSO, accuracyENET)
)
# how many variables selected?
enetCoefficients <- coef(cvENET, s = 'lambda.min')
sum(enetCoefficients[-1] != 0)



alphaVals <- seq(0, 1, by = 0.05)
cvResults <- list()
set.seed(2025)
for (i in seq_along(alphaVals)) {
  cvResults[[i]] <-
    cv.glmnet(Xtrain, ytrain,
              alpha = alphaVals[i],
              nfolds = 10,
              family = "binomial")
}
# minimum CV error
cvErrors <- sapply(cvResults,
                   function(x) min(x$cvm))
alphaOptimal <-
  alphaVals[which.min(cvErrors)]
alphaOptimal



predictionCompare <- tibble(
  Actual = as.numeric(ytest),
  LM = as.vector(lmPredict),
  Ridge = as.vector(ridgePredict),
  Lasso = as.vector(lassoPredict),
  `Elastic Net` = as.vector(enetPredict)
) %>%
  pivot_longer(cols = -Actual,
               names_to = 'Model',
               values_to = 'Predicted')
ggplot(predictionCompare,
       aes(Actual, Predicted,
           color = Model)) +
  geom_point(alpha = 0.6) +
  geom_abline(slope = 1,
              intercept = 0,
              linetype = 'dashed') +
  facet_wrap(~ Model) 

coefficientComparison <- tibble(
  Variable = rownames(lassoCoefficients)[-1],
  Ridge = as.vector(coef(cvRidge, s = 'lambda.min')[-1]),
  Lasso = as.vector(lassoCoefficients[-1]),
  `Elastic Net` = as.vector(enetCoefficients[-1])
) %>%
  pivot_longer(cols = -Variable,
               names_to = 'Model',
               values_to = 'Coefficient')
ggplot(coefficientComparison,
       aes(Variable, Coefficient,
           fill = Model)) +
  geom_col(position = 'dodge') +
  coord_flip() +
  labs(title = 'Coefficient Comparison')


cvENET2 <- cv.glmnet(Xtrain, ytrain, alpha = 0.7, nfolds = 10, family = "binomial")
plot(cvENET2)
title('Elastic Net ( = 0.5): 10-Fold CV', line = 3)
enetPredict2 <- predict(cvENET2, s = 'lambda.min', newx = Xtest, type = "response")
accuracyENET2 <- accuracy(as.numeric(ytest), enetPredict2)
data.frame(
  Model = c('LM', 'Ridge', 'Lasso', 'Elastic Net', "new"),
  accuracy = c(accuracyLM, accuracyRIDGE, accuracyLASSO, accuracyENET, accuracyENET2)
)

# simulate high-dimensional data where p > n
set.seed(2025)
n <- 100 # observations
p <- 200 # predictors
# only first 10 predictors are truly important
Xhd <- matrix(rnorm(n * p), n, p)
trueCoefficients <- c(rep(2, 5), rep(-2, 5), rep(0, p - 10))
yhd <- Xhd %*% trueCoefficients
# try the linear model, it should fail!
# lmHD <- lm(yhd ~ Xhd)
set.seed(2025)
cvHD <- cv.glmnet(Xhd, yhd, alpha = 1, nfolds = 10)
# how many variables selected?
coef_hd <- coef(cvHD, s = 'lambda.1se')
sum(coef_hd[-1] != 0)
# which variables were selected? (hopefully the first 10)
which(coef_hd[-1] != 0)
