library(tidyverse)
library(ggplot2)
library(gridExtra)
library(caret)
library(MASS)
library(glmnet)
library(leaps)
library(ROCR)

################################ Exploratory Data Analysis ####################################
#Read in the dataset.
data <- read.csv('student_2509208data.csv')
#The first column will be removed as it only represents the row number.
data <- data[, -1] 
#Check the structure of the data.
str(data)
#Check the summary of the data.
summary(data)

#I devide the dataset into train, validation and test set.
set.seed(713)
n<-nrow(data)
ind1 <- sample(c(1:n), round(2*n/3))
ind2 <- sample((c(1:n)[-ind1]), round(n/6))
ind3 <- setdiff(c(1:n), c(ind1, ind2))
train_set <- data[ind1,]
valid_set <- data[ind2,]
test_set <- data[ind3,]

#Selecting variables for the correlation matrix.
data_cor <- train_set[, c("Year_drafted", "GP", "MIN", "PTS", "FG_made", "FGA", "FG_percent",
                     "TP_made", "TPA", "TP_percent", "FT_made", "FTA", "FT_percent",
                     "OREB", "DREB", "REB", "AST", "STL", "BLK", "TOV", "Yrs", "Target")]
#Correlation matrix and plot.
correl <- cor(data_cor)
corrplot::corrplot(correl, method = "number", type = "upper", tl.cex = 0.65)

#The response variable will be transformed into a factor for having the right plots.
train_set$Target <- as.factor(train_set$Target)
#To avoid repetition of code I wrote a function to plot boxplots for all the variables
#vs the response.
p <- function(a){
  ggplot(data = train_set, aes(x = Target, y = {{a}}, group = Target)) +
    geom_boxplot(aes(colour = Target)) +
    theme(legend.position = 'none')
}

p1 <- p(Year_drafted) 
p2 <- p(GP) 
p3 <- p(MIN) 
p4 <- p(PTS) 
p5 <- p(FG_made) 
p6 <- p(FGA) 
p7 <- p(FG_percent)
p8 <- p(TP_made) 
p9 <- p(TPA) 
p10 <- p(TP_percent) 
p11 <- p(FT_made) 
p12 <- p(FTA) 
p13 <- p(FT_percent) 
p14 <- p(OREB)
p15 <- p(DREB)
p16 <- p(REB) 
p17 <- p(AST) 
p18 <- p(STL) 
p19 <- p(BLK) 
p20 <- p(TOV) 
p21 <- p(Yrs)
#I create this plot only for getting the legend to add to the final plot.
bp <- ggplot(train_set, aes(Target, AST, group = Target)) +
  geom_boxplot(aes(colour = Target)) +
  scale_colour_discrete(name = 'Target', label = c("Years < 5", "Years > 5")) +
  theme(legend.background = element_rect(colour = "black"))
legend <- cowplot::get_legend(bp)
#Creating a grid with all plots above created plus the legend
grid.arrange(p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14,
             p15, p16, p17, p18, p19, p20, p21,
             legend)

#To avoid repetition of code I wrote a function to plot histograms for all the variables.
#I overlap the distribution of the variables for the two values of the response with two 
#different colours.
second_p <- function(a){
  ggplot(train_set, aes(x = {{a}}, group = Target)) +
    geom_histogram(aes(fill = Target, y = ..density..), alpha = 0.5) +
    theme(legend.position = 'none')
}

sp1 <- second_p(Year_drafted)
sp2 <- second_p(GP) 
sp3 <- second_p(MIN) 
sp4 <- second_p(PTS) 
sp5 <- second_p(FG_made) 
sp6 <- second_p(FGA) 
sp7 <- second_p(FG_percent)
sp8 <- second_p(TP_made) 
sp9 <- second_p(TPA) 
sp10 <- second_p(TP_percent) 
sp11 <- second_p(FT_made) 
sp12 <- second_p(FTA) 
sp13 <- second_p(FT_percent) 
sp14 <- second_p(OREB)
sp15 <- second_p(DREB)
sp16 <- second_p(REB) 
sp17 <- second_p(AST) 
sp18 <- second_p(STL) 
sp19 <- second_p(BLK) 
sp20 <- second_p(TOV) 
sp21 <- second_p(Yrs)
#I created this plot only for getting the legend to add to the final plot.
bp2 <- ggplot(train_set, aes(AST, group = Target)) +
  geom_histogram(aes(fill = Target), alpha = 0.5) +
  scale_fill_discrete(name = 'Target', label = c("Years < 5", "Years > 5")) +
  theme(legend.background = element_rect(colour = "black"))
legend2 <- cowplot::get_legend(bp2)
#Creating a grid with all the plots above created plus the legend.
grid.arrange(sp1, sp2, sp3, sp4, sp5 ,sp6,sp7,sp8,sp9,sp10,sp11,sp12,sp13,sp14,
             sp15, sp16, sp17, sp18, sp19, sp20, sp21, legend2)


#To avoid repetition of code I wrote a function to plot scatterplots of all the variables.
#I distinguish the points of the variables for the two values of the response with two 
#different colours.
third_p <- function (a){ggplot(train_set, aes({{a}}, Target)) +
    geom_point(aes(colour = Target))+
    theme(legend.position = 'none')
}

tp1 <- third_p(Year_drafted)
tp2 <- third_p(GP) 
tp3 <- third_p(MIN) 
tp4 <- third_p(PTS) 
tp5 <- third_p(FG_made) 
tp6 <- third_p(FGA) 
tp7 <- third_p(FG_percent)
tp8 <- third_p(TP_made) 
tp9 <- third_p(TPA) 
tp10 <- third_p(TP_percent) 
tp11 <- third_p(FT_made) 
tp12 <- third_p(FTA) 
tp13 <- third_p(FT_percent) 
tp14 <- third_p(OREB)
tp15 <- third_p(DREB)
tp16 <- third_p(REB) 
tp17 <- third_p(AST) 
tp18 <- third_p(STL) 
tp19 <- third_p(BLK) 
tp20 <- third_p(TOV) 
tp21 <- third_p(Yrs)
#Creating a grid with all the plots above created plus the legend.
grid.arrange(tp1, tp2, tp3, tp4, tp5 ,tp6,tp7, tp8, tp9, tp10, tp11, tp12,
             tp13, tp14, tp15, tp16, tp17, tp18, tp19, tp20, tp21, legend2)

train_set$Target <- as.numeric(as.character(train_set$Target))
second_p(Target)




########################################## ANALYSIS ###########################################
#In order to avoid problems with the data previously modified, and since the dataset is small
#enough, I will upload the data again.
data <- read.csv('student_2509208data.csv')
#The first column will be removed as it only represents the row number.
data <- data[, -1]

#I devide the dataset into train, validation and test set.
set.seed(713)
n<-nrow(data)
ind1 <- sample(c(1:n), round(2*n/3))
ind2 <- sample((c(1:n)[-ind1]), round(n/6))
ind3 <- setdiff(c(1:n), c(ind1, ind2))
train_set <- data[ind1,]
valid_set <- data[ind2,]
test_set <- data[ind3,]


#logistic regression.
#I do not include the variable Name in the following models.
bin_model <- glm(Target ~ log(Year_drafted) + log(GP) + log(MIN) + log(PTS) + 
                   log(FG_made) + log(FGA) + log(FG_percent) + log(TP_made+0.1) + 
                   log(TPA+0.1) + log(TP_percent+0.1) + log(FT_made+0.1) + log(FTA+0.1) +
                   log(FT_percent+0.1) + log(OREB+0.1) + log(DREB) + log(REB) + log(AST+0.1) +
                   log(STL+0.1) + log(BLK+0.1) + log(TOV) + log(Yrs), data = train_set,
                 family = 'binomial', maxit = 100)

summary(bin_model)

#I repeat logistic regression without Names and Yrs variables.
bin_model2 <- glm(Target ~ log(Year_drafted) + log(GP) + log(MIN) + log(PTS) + 
                    log(FG_made) + log(FGA) + log(FG_percent) + log(TP_made+0.1) + 
                    log(TPA+0.1) + log(TP_percent+0.1) + log(FT_made+0.1) + log(FTA+0.1) +
                    log(FT_percent+0.1) + log(OREB+0.1) + log(DREB) + log(REB) + log(AST+0.1) +
                    log(STL+0.1) + log(BLK+0.1) + log(TOV), data = train_set,
                  family = 'binomial', maxit = 100)
summary(bin_model2)

#Make predictions.
predictions_bin_model2 <- predict(bin_model2, valid_set)
#Calculating RMSE.
rmse_bin <- sum((as.numeric(round(predictions_bin_model2)) - as.numeric(valid_set$Target))^2) / nrow(valid_set)
#Calculating the pseudo R-squared.
rsq_bin <- rsq::rsq(bin_model2)

#Calculating the Correct Classification Rate.
table_bin <- table(as.numeric(round(predictions_bin_model2)), as.numeric(valid_set$Target))
CCR_bin <- sum(diag(table_bin))/sum(table_bin)

#DataFrame with RMSE and R-squared.
DF_logReg <- data.frame(RMSE_bin = rmse_bin, Rsquare_bin = rsq_bin)



#Creating a model using Step_AIC.
model_stepAIC <- stepAIC(bin_model2)
summary(model_stepAIC)
#Making predictions.
predictions_model_stepAIC <- predict(model_stepAIC, valid_set)
#Calculating RMSE.
rmse_model_stepAIC <- sum((round(predictions_model_stepAIC) - valid_set$Target)^2) / nrow(valid_set)
#Calculating the pseudo R-squared.
rsq_model_stepAIC <- rsq::rsq(model_stepAIC)

#calculating the Correct Classification Rate.
table_stepAIC <- table(round(predictions_model_stepAIC), valid_set$Target)
CCR_stepAIC <- sum(diag(table_stepAIC))/sum(table_stepAIC)

#DataFrame with RMSE and R-squared.
DF_stepAIC <- data.frame(RMSE_stepAIC = rmse_model_stepAIC, Rsquared_stepAIC = rsq_model_stepAIC)



#Creating a model using the leaps package.
model_leaps <- leaps(train_set[, 2:21], train_set$Target, method = 'adjr2')
summary(model_leaps)
plot(model_leaps$size, model_leaps$adjr2, xlab = 'Number of Parameters',
     ylab = expression('Adjusted ' * R^2))

leaps_best <- model_leaps$which[which.max(model_leaps$adjr2), ]
model_leaps <- glm(Target ~ .-Yrs, family = binomial,
                   data = train_set[, c(c(2:21, 23)[leaps_best], 22)],
                   control = list(maxit = 50))
summary(model_leaps)
#Making predictions.
predictions_leaps <- predict(model_leaps, valid_set)
#Calculating the RMSE.
rmse_leaps <- sum((predictions_leaps - valid_set$Target)^2) / nrow(valid_set)
#Calculating the pseudo R-squared.
rsq_leaps <- rsq::rsq(model_leaps)

#Calculating the Correct Classification Rate.
table_leaps <- table(round(predictions_leaps), valid_set$Target)
CCR_leaps <- sum(diag(table_leaps))/sum(table_bin)

#DataFrame with RMSE and R-squared.
DF_leaps <- data.frame(RMSE_leaps = rmse_leaps, Rsquared_leaps = rsq_leaps)



#Scaling the data before applying Reguralised regression techniques.
scaling_data <- preProcess(train_set[, 1:22], method = c('center', 'scale'))
train_set[, 1:22] <- predict(scaling_data, train_set[, 1:22])
valid_set[, 1:22] <- predict(scaling_data, valid_set[, 1:22])
test_set[, 1:22] <- predict(scaling_data, test_set[, 1:22])

#I do not include the variable Name in the model.
x <- model.matrix(Target ~ Year_drafted + GP + MIN + PTS + FG_made + FGA +
                    FG_percent + TP_made + TPA + TP_percent + FT_made + FTA +
                    FT_percent + OREB + DREB + REB + AST + STL + BLK +
                    TOV + Yrs, data = train_set)
y <- train_set$Target

#Ridge regression
ridge_model <- glmnet(x, y, alpha = 0)
plot(ridge_model, xvar = 'lambda', label = TRUE)

#Cross Validation to tune the parameter lambda and plot.
cv_ridge_model <- cv.glmnet(x, y, alpha = 0)
plot(cv_ridge_model)
text(log(cv_ridge_model$lambda.min), 0.25, 'Lambda_min')
text(log(cv_ridge_model$lambda.1se), 0.25, 'Lambda_1se')
log(cv_ridge_model$lambda.min)
log(cv_ridge_model$lambda.1se)

#Fitting the model with the tuned lambda value.
fit_1seLambda_ridge_model <- glmnet(x, y, alpha = 0,
                                    lambda = cv_ridge_model$lambda.1se)
coef(fit_1seLambda_ridge_model)

#Validation of the model and making predictions.
x_valid_ridge <- model.matrix(Target ~ Year_drafted + GP + MIN + PTS + FG_made + FGA +
                         FG_percent + TP_made + TPA + TP_percent + FT_made + FTA +
                         FT_percent + OREB + DREB + REB + AST + STL + BLK +
                         TOV + Yrs, data = valid_set)
predictions_ridge <- fit_1seLambda_ridge_model %>%
  predict(x_valid_ridge) %>% as.vector()

#Calculating the Correct Classification Rate.
table_ridge <- table(round(predictions_ridge), valid_set$Target)
CCR_ridge <- sum(diag(table_ridge))/sum(table_ridge)

#DataFrame with RMSE and R-squared.
DF_ridge <- data.frame(RMSE_ridge = RMSE(round(predictions_ridge), valid_set$Target),
           Rsquare_ridge = R2(round(predictions_ridge), valid_set$Target))

#LASSO regression
lasso_model <- glmnet(x, y)
plot(lasso_model, xvar = 'lambda', label = TRUE)
plot(lasso_model, xvar = 'dev', label = TRUE)

#Cross Validatiaon to tune the parameter lambda and plot.
cv_lasso_model <- cv.glmnet(x, y)
plot(cv_lasso_model)
text(log(cv_lasso_model$lambda.min), 0.25, 'Lambda_min')#-4.7
text(log(cv_lasso_model$lambda.1se), 0.25, 'Lambda_1se')#-2.55
log(cv_lasso_model$lambda.min)
log(cv_lasso_model$lambda.1se)

#fitting the model with the tuned lambda value.
fit_1seLambda_lasso_model <- glmnet(x, y, lambda = cv_lasso_model$lambda.1se)
coef(fit_1seLambda_lasso_model)

#Validating the model and making predictions.
x_valid_lasso <- model.matrix(Target ~ Year_drafted + GP + MIN + PTS + FG_made + FGA +
                               FG_percent + TP_made + TPA + TP_percent + FT_made + FTA +
                               FT_percent + OREB + DREB + REB + AST + STL + BLK +
                               TOV + Yrs, data = valid_set)
predictions_lasso <- fit_1seLambda_lasso_model %>%
  predict(x_valid_lasso) %>% as.vector()

#Calculating the Correct Classification Rate.
table_lasso <- table(round(predictions_lasso), valid_set$Target)
CCR_lasso <- sum(diag(table_lasso))/sum(table_lasso)

#DataFrame with RMSE and R-squared.
DF_lasso <- data.frame(RMSE_lasso = RMSE(round(predictions_lasso), valid_set$Target), 
           Rsquare_lasso = R2(round(predictions_lasso), valid_set$Target))


#Elastic Net regression using the caret package.
set.seed(713)
Net_model <- train(as.factor(Target) ~ .-1, data = train_set[, -1], method = 'glmnet',
                   trControl = trainControl('cv', number = 10))
Net_model$bestTune
coef(Net_model$finalModel, Net_model$bestTune$lambda)

#Validating the model.
x_valid_Net <- model.matrix(as.factor(Target) ~. -1, valid_set[, -1])
predictions_Net <- Net_model %>% predict(x_valid_Net)

#Calculating the Correct classification Rate.
table_Net <- table(predictions_Net, valid_set$Target)
CCR_Net <- sum(diag(table_Net))/sum(table_Net)

#DataFrame with RMSE and R-squared.
DF_Net <- data.frame(RMSE_Net = RMSE(as.numeric(as.character(predictions_Net)), valid_set$Target), 
                     Rsquare_Net = R2(as.numeric(as.character(predictions_Net)), valid_set$Target))


#Comparing RMSE, R-squared and Correct Classification Rate of all models created.
DF_logReg
DF_stepAIC
DF_leaps
DF_ridge
DF_lasso
DF_Net

CCR_bin
CCR_stepAIC
CCR_leaps
CCR_ridge
CCR_lasso
CCR_Net




#Repeat the reguralised regressions without Yrs.
x <- model.matrix(Target ~ Year_drafted + GP + MIN + PTS + FG_made + FGA +
                    FG_percent + TP_made + TPA + TP_percent + FT_made + FTA +
                    FT_percent + OREB + DREB + REB + AST + STL + BLK +
                    TOV, data = train_set)
y <- train_set$Target

#Ridge regression.
ridge_model2 <- glmnet(x, y, alpha = 0)
plot(ridge_model2, xvar = 'lambda', label = TRUE)

#Cross Validation to tune the parameter lambda and plot.
cv_ridge_model2 <- cv.glmnet(x, y, alpha = 0)
plot(cv_ridge_model2)
text(log(cv_ridge_model2$lambda.min), 0.25, 'Lambda_min')#-2.8
text(log(cv_ridge_model2$lambda.1se), 0.25, 'Lambda_1se')#-1.85
log(cv_ridge_model2$lambda.min)
log(cv_ridge_model2$lambda.1se)

#fitting the model with the tuned lambda value.
fit_1seLambda_ridge_model2 <- glmnet(x, y, alpha = 0,
                                    lambda = cv_ridge_model2$lambda.1se)
coef(fit_1seLambda_ridge_model2)

#Validation of the model and making predictions.
x_valid_ridge2 <- model.matrix(Target ~ Year_drafted + GP + MIN + PTS + FG_made + FGA +
                               FG_percent + TP_made + TPA + TP_percent + FT_made + FTA +
                               FT_percent + OREB + DREB + REB + AST + STL + BLK +
                               TOV, data = valid_set)
predictions_ridge2 <- fit_1seLambda_ridge_model2 %>%
  predict(x_valid_ridge2) %>% as.vector()

#Clalculating the Correct Classification Rate.
table_ridge2 <- table(round(predictions_ridge2), valid_set$Target)
CCR_ridge2 <- sum(diag(table_ridge2))/sum(table_ridge2)

#DataFrame with RMSE and R-squared.
DF_ridge2 <- data.frame(RMSE_ridje2 = RMSE(round(predictions_ridge2), valid_set$Target),
                       Rsquare_ridje2 = R2(round(predictions_ridge2), valid_set$Target))

#LASSO regression
lasso_model2 <- glmnet(x, y)
plot(lasso_model2, xvar = 'lambda', label = TRUE)
plot(lasso_model2, xvar = 'dev', label = TRUE)

#Cross Validation to tune the parameter lambda and plot.
cv_lasso_model2 <- cv.glmnet(x, y)
plot(cv_lasso_model2)
text(log(cv_lasso_model2$lambda.min), 0.25, 'Lambda_min')#-4.7
text(log(cv_lasso_model2$lambda.1se), 0.25, 'Lambda_1se')#-2.55
log(cv_lasso_model2$lambda.min)
log(cv_lasso_model2$lambda.1se)

#fitting the model with the tuned lambda value.
fit_1seLambda_lasso_model2 <- glmnet(x, y, lambda = cv_lasso_model2$lambda.1se)
coef(fit_1seLambda_lasso_model2)

x_valid_lasso2 <- model.matrix(Target ~ Year_drafted + GP + MIN + PTS + FG_made + FGA +
                               FG_percent + TP_made + TPA + TP_percent + FT_made + FTA +
                               FT_percent + OREB + DREB + REB + AST + STL + BLK +
                               TOV , data = valid_set)
predictions_lasso2 <- fit_1seLambda_lasso_model2 %>%
  predict(x_valid_lasso2) %>% as.vector()

#Clalculating the Correct Classification Rate.
table_lasso2 <- table(round(predictions_lasso2), valid_set$Target)
CCR_lasso2 <- sum(diag(table_lasso2))/sum(table_lasso2)

#DataFrame with RMSE and R-squared.
DF_lasso2 <- data.frame(RMSE_lasso2 = RMSE(round(predictions_lasso2), valid_set$Target), 
                       Rsquare_lasso2 = R2(round(predictions_lasso2), valid_set$Target))


#Elastic Net regression using the caret package.
set.seed(713)
Net_model2 <- train(as.factor(Target) ~ .-1, data = train_set[, c(-1, -22)], method = 'glmnet',
                    trControl = trainControl('cv', number = 10))
Net_model2$bestTune
coef(Net_model2$finalModel, Net_model2$bestTune$lambda)

#Validating the model.
x_valid_Net2 <- model.matrix(as.factor(Target) ~.-1, valid_set[, c(-1, -22)])
predictions_Net2 <- Net_model2 %>% predict(x_valid_Net2)

#Clalculating the Correct Classification Rate.
table_Net2 <- table(predictions_Net2, valid_set$Target)
CCR_Net2 <- sum(diag(table_Net2))/sum(table_Net2)

#DataFrame with RMSE and R-squared.
DF_Net2 <- data.frame(RMSE_Net2 = RMSE(as.numeric(predictions_Net2), valid_set$Target), 
                      Rsquare_Net2 = R2(as.numeric(predictions_Net2), valid_set$Target))




#Comparing RMSE, R-squared and Correct Classification Rate of all models created.
DF_logReg
DF_stepAIC
DF_leaps
DF_ridge
DF_lasso
DF_Net
DF_ridge2
DF_lasso2
DF_Net2

CCR_bin
CCR_stepAIC
CCR_leaps
CCR_ridge
CCR_lasso
CCR_Net
CCR_ridge2
CCR_lasso2
CCR_Net2


#Creating the ROC curve plots using the ROCR package for the reguralised regression models.
score_ridge <- prediction(predictions_ridge, valid_set$Target)
perf_ridge <- performance(score_ridge, "tpr", "fpr")
auc_ridge <- performance(score_ridge, "auc")
perfd_ridge <- data.frame(x = perf_ridge@x.values[1][[1]], 
                          y = perf_ridge@y.values[1][[1]])
auc_ridge <- ggplot(perfd_ridge, aes(x, y)) + geom_line() +
  xlab('False Positive Rate') + ylab('True Positive Rate') +
  ggtitle(paste('Area Under the Curve (Ridge Regression):', 
                round(auc_ridge@y.values[[1]], 3)))


score_lasso <- prediction(predictions_lasso, valid_set$Target)
perf_lasso <- performance(score_lasso, "tpr", "fpr")
auc_lasso <- performance(score_lasso, "auc")
perfd_lasso <- data.frame(x = perf_lasso@x.values[1][[1]], 
                          y = perf_lasso@y.values[1][[1]])
auc_lasso <- ggplot(perfd_lasso, aes(x, y)) + geom_line() +
  xlab('False Positive Rate') + ylab('True Positive Rate') +
  ggtitle(paste('Area Under the Curve (LASSO Regression):', 
                round(auc_lasso@y.values[[1]], 3)))


score_Net <- prediction(list(as.numeric(predictions_Net)), valid_set$Target)
perf_Net <- performance(score_Net, "tpr", "fpr")
auc_Net <- performance(score_Net, "auc")
perfd_Net <- data.frame(x = perf_Net@x.values[1][[1]], 
                        y = perf_Net@y.values[1][[1]])
auc_Net <- ggplot(perfd_Net, aes(x, y)) + geom_line() +
  xlab('False Positive Rate') + ylab('True Positive Rate') +
  ggtitle(paste('Area Under the Curve (Elastic Net Regression):', 
                round(auc_Net@y.values[[1]], 3)))

#Creating the ROC curve plots using the ROCR package for the reguralised 
#regression models without the Yrs variable.
score_ridge2 <- prediction(predictions_ridge2, valid_set$Target)
perf_ridge2 <- performance(score_ridge2, "tpr", "fpr")
auc_ridge2 <- performance(score_ridge2, "auc")
perfd_ridge2 <- data.frame(x = perf_ridge2@x.values[1][[1]], 
                          y = perf_ridge2@y.values[1][[1]])
auc_ridge2 <- ggplot(perfd_ridge2, aes(x, y)) + geom_line() +
  xlab('False Positive Rate') + ylab('True Positive Rate') +
  ggtitle(paste('Area Under the Curve (Ridge Regression without Yrs):', 
                round(auc_ridge2@y.values[[1]], 3)))


score_lasso2 <- prediction(predictions_lasso2, valid_set$Target)
perf_lasso2 <- performance(score_lasso2, "tpr", "fpr")
auc_lasso2 <- performance(score_lasso2, "auc")
perfd_lasso2 <- data.frame(x = perf_lasso2@x.values[1][[1]], 
                          y = perf_lasso2@y.values[1][[1]])
auc_lasso2 <- ggplot(perfd_lasso2, aes(x, y)) + geom_line() +
  xlab('False Positive Rate') + ylab('True Positive Rate') +
  ggtitle(paste('Area Under the Curve (LASSO Regression without Yrs):', 
                round(auc_lasso2@y.values[[1]], 3)))


score_Net2 <- prediction(list(as.numeric(predictions_Net2)), valid_set$Target)
perf_Net2 <- performance(score_Net2, "tpr", "fpr")
auc_Net2 <- performance(score_Net2, "auc")
perfd_Net2 <- data.frame(x = perf_Net2@x.values[1][[1]], 
                         y = perf_Net2@y.values[1][[1]])
auc_Net2 <- ggplot(perfd_Net2, aes(x, y)) + geom_line() +
  xlab('False Positive Rate') + ylab('True Positive Rate') +
  ggtitle(paste('Area Under the Curve (Elastic Net Regression wthout Yrs):', 
                round(auc_Net2@y.values[[1]], 3)))

grid.arrange(auc_ridge, auc_lasso, auc_Net, auc_ridge2, auc_lasso2, auc_Net2)


#Selected Elastic Net model, making predictions using the test set.
x_test_Net_sel <- model.matrix(as.factor(Target) ~., test_set[, c(-1, -22)])
predictions_Net_sel <- Net_model2 %>% predict(x_test_Net_sel)

#Calculating the Correct classification Rate, Specificity and Sensitivity.
table_Net_sel <- table(predictions_Net_sel, test_set$Target)
CCR_Net_sel <- sum(diag(table_Net_sel))/sum(table_Net_sel)
SS <- sweep(table_Net_sel, 1, apply(table_Net_sel, 1, sum), "/")
sensitivity <- SS[2,2]
specificity <- SS[1,1]

#DataFrame with RMSE and R-squared.
DF_Net_sel <- data.frame(RMSE_Net_sel = RMSE(as.numeric(predictions_Net_sel), test_set$Target), 
                      Rsquare_Net_sel = R2(as.numeric(predictions_Net_sel), test_set$Target))


#Plot of the ROC curve for the final model using the test set.
score_Net_sel <- prediction(list(as.numeric(predictions_Net_sel)), test_set$Target)
perf_Net_sel <- performance(score_Net_sel, "tpr", "fpr")
auc_Net_sel <- performance(score_Net_sel, "auc")
perfd_Net_sel <- data.frame(x = perf_Net_sel@x.values[1][[1]], 
                         y = perf_Net_sel@y.values[1][[1]])
auc_Net_sel <- ggplot(perfd_Net_sel, aes(x, y)) + geom_line() +
  xlab('False Positive Rate') + ylab('True Positive Rate') +
  ggtitle(paste('Area Under the Curve (Elastic Net Regression final model):', 
                round(auc_Net_sel@y.values[[1]], 3)))
auc_Net_sel



