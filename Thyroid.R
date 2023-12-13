## ----setup, include=FALSE-------------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = FALSE, 
                      warning = FALSE, 
                      message = FALSE, 
                      fig.align = "center",
                      out.width = "80%")


## ----installing required packages-----------------------------------------------------------------------------
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org")
if(!require(knitr)) install.packages("knitr", repos = "http://cran.us.r-project.org")
if(!require(kableExtra)) install.packages("kableExtra", repos = "http://cran.us.r-project.org")
if(!require(Hmisc)) install.packages("Hmisc", repos = "http://cran.us.r-project.org")
if(!require(ggcorrplot)) install.packages("ggcorrplot", repos = "http://cran.us.r-project.org")
if(!require(dplyr)) install.packages("dplyr")
if(!require(reshape2)) install.packages("reshape2")
if(!require(devtools)) install.packages("devtools")
if(!require(lares)) install.packages("lares")
if(!require(randomForest)) install.packages("randomForest")
if(!require(kernlab)) install.packages("kernlab")
if(!require(gbm)) install.packages("gbm")
if(!require(xgboost)) install.packages("xgboost")
if(!require(ada)) install.packages("ada")
if(!require(rpart)) install.packages("rpart")
if(!require(caretEnsemble)) install.packages("caretEnsemble")
if(!require(naivebayes)) install.packages("naivebayes")
if(!require(MLmetrics)) install.packages("MLmetrics")
if(!require(ROCR)) install.packages("ROCR")
if(!require(pROC)) install.packages("pROC")
if(!require(LogicReg)) install.packages("LogicReg")

library(tidyverse)
library(caret)
library(dplyr)
library(lubridate)
library(ggplot2)
library(kableExtra)
library(Hmisc)
library(ggcorrplot)
library(dplyr)
library(reshape2)
library(lares)
library(naivebayes)
library(randomForest)
library(kernlab)
library(gbm)
library(xgboost)
library(ada)
library(rpart)
library(caretEnsemble)
library(MLmetrics)
library(ROCR)
library(pROC)
library(LogicReg)


## ----Read thyroid dataset in csv format-----------------------------------------------------------------------
dataset_filename <- "Thyroid_Diff.csv"
if (!file.exists(dataset_filename))
  download.file("https://raw.githubusercontent.com/creetheeka/thyroid_cancer_recurrence/main/Thyroid_Diff.csv", "Thyroid_Diff.csv")
suppressWarnings(thyroid_dataset <- read.csv("Thyroid_Diff.csv"))


## ----display top 6 rows---------------------------------------------------------------------------------------
head(thyroid_dataset) %>% 
  kable(caption = "Top 6 rows of thyroid cancer recurrence dataset", align = 'ccclll', format = "latex", linesep = "", booktabs = TRUE, row.names = FALSE) %>%
  row_spec(0, bold=TRUE) %>%
  kable_styling(full_width = FALSE, position = "center", latex_options = c("scale_down", "HOLD_position", "striped"))


## ----dataset dimensions---------------------------------------------------------------------------------------
dim <- dim(thyroid_dataset)
dim


## ----dataset structure, echo = TRUE---------------------------------------------------------------------------
str(thyroid_dataset)


## ----factorize the categorical features, include=FALSE--------------------------------------------------------
pre_factor <- is.factor(thyroid_dataset$Recurred)
thyroid_dataset[sapply(thyroid_dataset, is.character)] <- lapply(thyroid_dataset[sapply(thyroid_dataset, is.character)], as.factor)
post_factor <- is.factor(thyroid_dataset$Recurred)


## ----dataset structure after factorize, echo = TRUE-----------------------------------------------------------
str(thyroid_dataset)


## ----dataset summary, echo = TRUE-----------------------------------------------------------------------------
summary(thyroid_dataset)


## ----check for NA values in the dataset, include=FALSE--------------------------------------------------------
na <- anyNA(thyroid_dataset)


## ----table to display count of NAs if any---------------------------------------------------------------------
sapply(thyroid_dataset, function(x) sum(is.na(x))) %>% 
  kable(caption = "Table of variables and count of NA", col.names = c("Variable" ,"NA count")) %>%
  row_spec(0, bold=TRUE) %>%
  kable_styling(font_size=4, full_width = FALSE, position = "center", latex_options = c("scale_down", "HOLD_position", "striped"))


## ----percentage of target, include = TRUE, echo = FALSE-------------------------------------------------------
percent <- prop.table(table(thyroid_dataset$Recurred)) * 100


## ----proportion of target-------------------------------------------------------------------------------------
thyroid_dataset %>%
  ggplot(aes(Recurred, fill=Recurred)) +
  geom_bar() +
  scale_x_discrete() +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Proportion of Recurred vs Non-Recurred")


## ----count of Recurred or not, echo = FALSE-------------------------------------------------------------------
thyroid_dataset %>%
  group_by(Recurred) %>%
  summarise(Count = n()) %>% 
  kable(caption = "Count of Yes and No for the Recurred target variable", col.names = c("Recurred" ,"NA count")) %>%
  row_spec(0, bold=TRUE) %>%
  kable_styling(font_size=4, full_width = FALSE, position = "center", latex_options = c("scale_down", "HOLD_position", "striped"))


## ----histogram of features, plot-wider, fig.width=10, fig.height=10-------------------------------------------
hist_data <- thyroid_dataset[, c("Gender","Smoking","Hx.Smoking","Hx.Radiothreapy","Thyroid.Function", "Physical.Examination", "Adenopathy", "Pathology","Focality" , "Risk","T", "N","M","Stage","Response", "Recurred" )]
  ggplot(data = melt(hist_data, id.var = "Recurred"), mapping = aes(x = value)) + 
  geom_bar(aes(fill=Recurred)) +
  labs(title = "Plot of categorical features and target",
       x = "Categorical Feature",
       y = "Count") +
  facet_wrap(~variable, scales='free_x') + 
  theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.5))


## ----histogram of Age feature---------------------------------------------------------------------------------
thyroid_dataset %>% 
  ggplot(aes(x = Age)) + 
  labs(title = "Distribution of Age and Target",
       x = "Age",
       y = "Count") +
  geom_histogram(bins=10, aes(fill=Recurred)) 


## ----factorize categorical features for correlation matrix----------------------------------------------------
cor_dataset <- thyroid_dataset
cor_dataset[sapply(cor_dataset, is.factor)] <- data.matrix(cor_dataset[sapply(cor_dataset, is.factor)])
head(cor_dataset) %>%
  kable(caption = "Top 6 rows of factorized categorical features",align = 'ccclll', format = "latex", linesep = "", booktabs = TRUE, row.names = FALSE) %>%
  row_spec(0, bold=TRUE) %>%
  kable_styling(full_width = FALSE, position = "center", latex_options = c("scale_down", "HOLD_position", "striped"))


## ----correlation matrix, plot-wider, fig.width= 10, fig.height= 10--------------------------------------------
dataset_cormat <- cor(cor_dataset %>% dplyr::select(-Recurred) %>% keep(is.numeric))
ggcorrplot(dataset_cormat, hc.order = TRUE, outline.color = "white", insig = "blank", lab = TRUE) +
  labs(title = "Correlation Matrix of features")

## ----higly correlated if any, include = TRUE------------------------------------------------------------------
high_cor <- colnames(cor_dataset)[findCorrelation(dataset_cormat, verbose = TRUE)]


## ----ranked cross correlation---------------------------------------------------------------------------------
corr_cross(cor_dataset, max_pvalue = 0.5)

## ----correlation of Recurred and Features---------------------------------------------------------------------
corr_var(cor_dataset, Recurred)


## ----create train and validation set--------------------------------------------------------------------------
set.seed(2023, sample.kind="Rounding")
train_idx <- createDataPartition(y = thyroid_dataset$Recurred, times = 1, p = 0.8, list = FALSE)
train_set <- thyroid_dataset[train_idx,]
validation_set <- thyroid_dataset[-train_idx,]
rm(train_idx)


## ----control parameters for training--------------------------------------------------------------------------
model_fit_ctrl <- trainControl(method = "repeatedcv", 
                               repeats = 10,
                               number = 10, 
                               summaryFunction = twoClassSummary,
                               classProbs = TRUE)


## ----confusion matrix table-----------------------------------------------------------------------------------
df <- matrix(c('TP', 'FP', 'FN', 'TN'), ncol=2, byrow=TRUE)
colnames(df) <- c('Positive', 'Negative')
rownames(df) <- c('Positive', 'Negative')
df %>% 
  kable(caption = "Confusion Matrix") %>%
  kable_styling(full_width = FALSE, position = "center", latex_options = c("scale_down", "HOLD_position", "striped"))


## ----train Naive Bayes Model----------------------------------------------------------------------------------
nb_grid <- expand.grid(laplace=c(0),
                           usekernel =c(FALSE),
                           adjust = 0.5)
nb_model <- train(Recurred ~ .,
                           method = "naive_bayes",
                           tuneGrid = nb_grid,
                           data = train_set,
                           metric = "ROC",
                           trControl = model_fit_ctrl,
                           na.action = na.exclude)

predict_nb <- predict(nb_model, validation_set, type = "raw")
cf_matrix_nb <- confusionMatrix(predict_nb, validation_set$Recurred, positive = "Yes")
cf_matrix_nb
roc_obj_nb <- roc(validation_set$Recurred, as.numeric(predict_nb))
auc_val_nb <- auc(roc_obj_nb)
plot(roc_obj_nb, main = paste("Naive Bayes Model AUC:", round(auc_val_nb, digits = 4)))


## ----train Generalized linear model---------------------------------------------------------------------------
glm_model <- train(Recurred ~ .,
                           method = "glm",
                           data = train_set,
                           metric = "ROC",
                           trControl = model_fit_ctrl,
                           na.action = na.exclude)

predict_glm <- predict(glm_model, validation_set, type = "raw")
cf_matrix_glm <- confusionMatrix(predict_glm, validation_set$Recurred, positive = "Yes")
cf_matrix_glm
roc_obj_glm <- roc(validation_set$Recurred, as.numeric(predict_glm))
auc_val_glm <- auc(roc_obj_glm)
plot(roc_obj_glm, main = paste("Generalized Linear Model AUC:", round(auc_val_glm, digits = 4)))


## ----train Random Forest Model--------------------------------------------------------------------------------
rf_grid <- data.frame(mtry = 3)
rf_model <- train(Recurred ~ .,
                           method = "rf",
                           tuneGrid = rf_grid,
                           data = train_set,
                           metric = "ROC",
                           trControl = model_fit_ctrl,
                           na.action = na.exclude)

predict_rf <- predict(rf_model, validation_set)
cf_matrix_rf <- confusionMatrix(predict_rf, validation_set$Recurred, positive = "Yes")
cf_matrix_rf
roc_obj_rf <- roc(validation_set$Recurred, as.numeric(predict_rf))
auc_val_rf <- auc(roc_obj_rf)
plot(roc_obj_rf, main = paste("Random Forest Model AUC:", round(auc_val_rf, digits = 4)))


## ----train Support Vector Machine model with Linear Kernel----------------------------------------------------

svm_model <- train(Recurred ~ .,
                           method = "svmLinear",
            
                           data = train_set,
                           metric = "ROC",
                           trControl = model_fit_ctrl,
                           na.action = na.exclude)

predict_svm <- predict(svm_model, validation_set)
cf_matrix_svm <- confusionMatrix(predict_svm, validation_set$Recurred, positive = "Yes")
cf_matrix_svm
roc_obj_svm <- roc(validation_set$Recurred, as.numeric(predict_svm))
auc_val_svm <- auc(roc_obj_svm)
plot(roc_obj_svm, main = paste("Support Vector Machine Model AUC:", round(auc_val_svm, digits = 4)))


## ----train K-Nearest Neighbors model--------------------------------------------------------------------------
knn_grid <- expand.grid(k= c(1:21))
knn_model <- train(Recurred ~ .,
                           method = "knn",
                           tuneGrid = knn_grid,
                           data = train_set,
                           metric = "ROC",
                           trControl = model_fit_ctrl,
                           na.action = na.exclude)

predict_knn <- predict(knn_model, validation_set)
cf_matrix_knn <- confusionMatrix(predict_knn, validation_set$Recurred, positive = "Yes")
cf_matrix_knn
roc_obj_knn <- roc(validation_set$Recurred, as.numeric(predict_knn))
auc_val_knn <- auc(roc_obj_knn)
plot(roc_obj_knn, main = paste("KNN Model AUC:", round(auc_val_knn, digits = 4)))


## ----train Stochastic Gradient Boosting-----------------------------------------------------------------------
set.seed(1, sample.kind = "Rounding")
gbm_model <- train(Recurred ~ .,
                           method = "gbm",
                           data = train_set,
                           metric = "ROC",
                           trControl = model_fit_ctrl,
                           na.action = na.exclude,
                           verbose = FALSE)

predict_gbm <- predict(gbm_model, validation_set)
cf_matrix_gbm <- confusionMatrix(predict_gbm, validation_set$Recurred, positive = "Yes")
cf_matrix_gbm
roc_obj_gbm <- roc(validation_set$Recurred, as.numeric(predict_gbm))
auc_val_gbm <- auc(roc_obj_gbm)
plot(roc_obj_gbm, main = paste("GBM AUC:", round(auc_val_gbm, digits = 4)))


## ----confusion matrix results for each model------------------------------------------------------------------
cm_list <- list(
    "NB"  = cf_matrix_nb,
    "GLM" = cf_matrix_glm,
    "RF"  = cf_matrix_rf,
    "SVM" = cf_matrix_svm,
    "KNN" = cf_matrix_knn,
    "GBM" = cf_matrix_gbm
    )
cm_results <- sapply(cm_list, function(x) x$byClass)
cm_results <- as.data.frame(cm_results)
cm_results %>% 
  mutate("Best Model" = names(.)[max.col(.)]) %>%
  kable(caption = "Confusion Matrix Results across ML models", align = 'ccclll', format = "latex", linesep = "", booktabs = TRUE) %>%
  row_spec(0,bold=TRUE) %>%
  kable_styling(full_width = FALSE, position = "center", latex_options = c("scale_down", "HOLD_position", "striped"))

