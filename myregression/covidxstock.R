library(DAAG)
#import data
data <- read.csv("dataset.csv", header = TRUE, sep = ",")
data

linearMod <- lm(s.p500 ~ TC, data = data)
#calculates the correlation between s&p500 closing price and total covid19 cases
cor(data$s.p500, data$TC)
#assign the model summary to object modelSummary
modelSummary <- summary(linearMod)
modelSummary
#model coefficients
modelCoeffs <- modelSummary$coefficients
#retrieve the beta estimate for Total Covid19 cases
beta.estimate <- modelCoeffs["TC", "Estimate"] 
#retrieve the standard error for Total Covid19 cases
std.error <- modelCoeffs["TC", "Std. Error"]
#calculate the t-statistic
t_value <- beta.estimate/std.error
#calculate the p-value
p_value <- 2*pt(-abs(t_value), df=nrow(data)-ncol(data))
#calculate the f-statistic
f_statistic <- linearMod$fstatistic[1]
#creates the parameters for the model p-value calculation
f <- summary(linearMod)$fstatistic 
# model p_value calc
model_p <- pf(f[1], f[2], f[3], lower=FALSE)
#The following code creates a training and test data
#sets seed to reproduce results of random sampling
set.seed(100)
#row indices for training data
trainingRowIndex <- sample(1:nrow(data), 0.8*nrow(data))
#model training data
trainingData <- data[trainingRowIndex, ]
#test data
testData <- data[-trainingRowIndex, ]
#builds model
lmMod <- lm(s.p500 ~ TC, data=trainingData)
#predicts distance
distPred <- predict(lmMod, testData)
#model summary
summary(lmMod)
#calculates akaike information criterion 
AIC(lmMod)
#creates the actuals_preds dataframe
actuals_preds <- data.frame(cbind(actuals=testData$s.p500,predicteds=distPred ))
correlation_accuracy <- cor(actuals_preds)
head(actuals_preds)
min_max_accuracy <- mean(apply(actuals_preds, 1, min) / apply(actuals_preds, 1, max))
mape <- mean(abs((actuals_preds$predicteds - actuals_preds$actuals))/actuals_preds$actuals)
cvResults <- suppressWarnings(CVlm(data = data, form.lm= formula(s.p500 ~ TC), m=5, dots=FALSE, seed=29, legend.pos="topleft",  printit=FALSE, main="Small symbols are cross-validation predicted values while bigger ones are actuals.")) 
attr(cvResults, 'ms')

