library(tidyverse) # Load required libraries
library(readr)
library(corrplot)
library(MuMIn)
library(writexl)
library(caret)
library(DMwR)
library(ROCR)
library(pdp)
library(vip)
library(tidyverse)

repurchase_training <- read_csv("repurchase_training.csv") # Read dataset

str(repurchase_training) # check structure of dataset

rep <- repurchase_training


# Convert as factor
rep$age_band <- as.factor(rep$age_band)
rep$gender <- as.factor(rep$gender)
rep$car_model  <- as.factor(rep$car_model)
rep$car_segment  <- as.factor(rep$car_segment)
rep$Target  <- as.factor(rep$Target)

rep$ID <- NULL  # drop ID column


str(rep) # check structure of dataframe

# Split data into train and test

trainset_size <- floor(0.75 * nrow(rep))




set.seed(42) 


trainset_indices <- sample(seq_len(nrow(rep)), size = trainset_size)




trainset <- rep[trainset_indices, ]
testset <- rep[-trainset_indices, ]



nrow(trainset)
nrow(testset)
nrow(rep)



ggplot(trainset) +
  aes(x = Target, fill = Target) +
  geom_bar(position = "dodge") +
  scale_fill_hue() +
  labs(y = "Number of observations", title = "Number of observations in each Target class of Training data") +
  theme_gray()

table(trainset$Target)

prop.table(table(trainset$Target))


## SMOTE the trainset to rectify class imbalane
trainset_log_smote <- SMOTE(form = Target ~ ., data = as.data.frame(trainset), perc.over = 100, perc.under=200 , k=10)

ggplot(trainset_log_smote) +
  aes(x = Target, fill = Target) +
  geom_bar(position = "dodge") +
  scale_fill_hue() +
  labs(y = "Number of observations", title = "Number of observations in each Target class of Training data after SMOTE") +
  theme_gray()

table(trainset_log_smote$Target)

prop.table(table(trainset_log_smote$Target))



#Train logistic Regression model

mod_1 = glm(formula = Target ~ .,
            data = trainset_log_smote,
            family = "binomial")


summary(mod_1)



#Prediction on testset
testset$probability = predict(mod_1, newdata = testset, type = "response")



testset$prediction = "0"

#  probability threshold 
testset[testset$probability >= 0.5, "prediction"] = "1" 


testset$prediction  <- as.factor(testset$prediction)

str(testset$prediction)


table(predicted=testset$prediction,true=testset$Target)



#confusion matrix
cm <- confusionMatrix(testset$prediction, reference = testset$Target , positive="1")



# Precision, Recall and F1
metrics <- cm$byClass

metrics[5:7]





trainset_log_smote$probability = predict(mod_1, trainset_log_smote,  type = "response")
trainset_log_smote$prediction = 0
trainset_log_smote[trainset_log_smote$probability >= 0.5, "prediction"] = 1





# Make a prediction object: pred
pred_test <- prediction(testset$probability, testset$Target)
pred_train <- prediction(trainset_log_smote$probability, trainset_log_smote$Target)


# Make a performance object: perf
perf_test <- performance(pred_test, "tpr", "fpr")
perf_train <- performance(pred_train, "tpr", "fpr")





test_auc = performance(pred_test, "auc")
train_auc = performance(pred_train, "auc")



#ROC curve
plot(perf_test, main="Testing and Training ROC Curve for Logistic Regression model", col = "blue")
plot(perf_train, add = T, col = "red")
legend("bottomright", legend = c("Training","Testing"), col = c("red","blue"), lty = 1, lwd = 2)
abline(0,1, col = "darkgray")
grid()



# Area under the ROC curve
train_auc = unlist(slot(train_auc, "y.values"))
train_auc


test_auc = unlist(slot(test_auc, "y.values"))
test_auc








# Here we use the performance function again to extract the sensitivity and specificity graphs from our testing prediction object.

sens = performance(pred_test, "sens")
spec = performance(pred_test, "spec")

# Here we plot a nice graph of sensitivity and specificity
plot(sens, 
     main = "Sensitivity Specificity Chart", type = "l", col = "red", lwd = 2, 
     xlim = c(0,1), ylim = c(0,1), 
     ylab = "Values")
axis(side = 1, at = seq(0, 1, 0.1))
axis(side = 2, at = seq(0, 1, 0.1))
plot(spec, add = T, col = "blue", lwd = 2, 
     xlim = c(0,1), ylim = c(0,1)
)

legend("bottomright", legend = c("Sensitivity","Specificity"), col = c("red", "blue"), lty = 1, lwd = 2)
abline(h = seq(0, 1, 0.1), v = seq(0, 1, 0.1), col="gray", lty=3)








# Variable importance plot 
vip(mod_1, bar = TRUE, horizontal = TRUE, size = 1.5)  # Figure 1


p1 <- partial(mod_1, pred.var = "total_paid_services", plot = TRUE, rug = TRUE)
















