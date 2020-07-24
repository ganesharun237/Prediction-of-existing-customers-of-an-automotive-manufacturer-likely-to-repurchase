library(gbm)  # Load required libraries
library(parallel)
library(readr)
library(DMwR)
library(caret)
library(ROCR)
library(pdp)
library(vip)
library(tidyverse)
library(writexl)

repurchase_training <- read_csv("repurchase_training.csv") # Read dataset



rep <- repurchase_training

# Convert as factor
rep$age_band <- as.factor(rep$age_band)
rep$gender <- as.factor(rep$gender)
rep$car_model  <- as.factor(rep$car_model)
rep$car_segment  <- as.factor(rep$car_segment)
rep$Target  <- as.factor(rep$Target)

rep$ID <- NULL # drop ID column


# Split data into train and test


trainset_size <- floor(0.75 * nrow(rep))







set.seed(42)


trainset_indices <- sample(seq_len(nrow(rep)), size = trainset_size)



trainset <- rep[trainset_indices, ]
testset <- rep[-trainset_indices, ]



nrow(trainset)
nrow(testset)
nrow(rep)


## SMOTE the trainset to rectify class imbalane
trainset_1 <- SMOTE(form = Target ~ ., data = as.data.frame(trainset), perc.over = 100, perc.under=200 , k=10)



prop.table(table(trainset_1$Target))


trainset_1$Target  <- as.numeric(levels(trainset_1$Target))[trainset_1$Target]





# Hyper-parameters for GBM
gbm_depth = 20 #maximum nodes per tree
gbm_n_min = 15 #minimum number of observations in the trees terminal, important effect on overfitting
gbm_shrinkage=0.01 #learning rate
gbm_cv_folds= 70 #number of cross-validation folds to perform
num_trees = 700  # Number of iterations




start <- proc.time()

# fit initial model
gbm_clf = gbm(Target~.,
              data=trainset_1,
              distribution='bernoulli', #binary response
              n.trees=num_trees,
              interaction.depth= gbm_depth,
              n.minobsinnode = gbm_n_min, 
              shrinkage=gbm_shrinkage, 
              cv.folds=gbm_cv_folds,
              verbose = TRUE, #print the preliminary output
)

end <- proc.time() - start
end_time <- as.numeric((paste(end[3])))
end_time





# Estimate the optimal number of iterations (when will the model stop improving)
# The black is the training deviance dropping whilst the green is the test.
best_iter = gbm.perf(gbm_clf, method = "cv")
print(best_iter)

# Gives the variable importance in a graph
summary(gbm_clf,n.trees=best_iter, ylab = "Variable", main = "Variable Relative Importance")

# OR just as a table
summary(gbm_clf)

# Prediction on test set
testset$probability = predict(gbm_clf, testset, n.trees = best_iter, type = "response")
testset$prediction = 0

#  probability threshold 
testset[testset$probability >= 0.5, "prediction"] = 1



testset$prediction  <- as.factor(testset$prediction)

#confusion matrix
cm <- confusionMatrix(data = testset$prediction, reference = testset$Target , positive="1")


# Precision, Recall and F1
metrics <- cm$byClass

metrics[5:7]





trainset_1$probability = predict(gbm_clf, trainset_1,  type = "response")
trainset_1$prediction = 0
trainset_1[trainset_1$probability >= 0.5, "prediction"] = 1






# Make a prediction object: pred
pred_test <- prediction(testset$probability, testset$Target)
pred_train <- prediction(trainset_1$probability, trainset_1$Target)


# Make a performance object: perf
perf_test <- performance(pred_test, "tpr", "fpr")
perf_train <- performance(pred_train, "tpr", "fpr")





test_auc = performance(pred_test, "auc")
train_auc = performance(pred_train, "auc")



#ROC curve
plot(perf_test, main="Testing and Training ROC Curves for GBM", col = "blue")
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

# Here we plot a nice graph between sensitivity and specificity
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
vip(gbm_clf, bar = TRUE, horizontal = TRUE, size = 1.5)  # Figure 1




#Partial dependency plot

plot(gbm_clf, i.var = 6, lwd = 2, col = "blue", main = "")


plot(gbm_clf, i.var = 12, lwd = 2, col = "blue", main = "")

plot(gbm_clf, i.var = 13, lwd = 2, col = "blue", main = "")

plot(gbm_clf, i.var = 8, lwd = 2, col = "blue", main = "")


plot(gbm_clf, i.var = 11, lwd = 2, col = "blue", main = "")



# Prediction on repurchase_validation dataset

rep_valid <- read_csv("repurchase_validation.csv") 



# Let us get our estimates
rep_valid$probability = predict(gbm_clf, rep_valid, type = "response")
rep_valid$prediction = 0

# Modify the probability threshold to see if you can get a better accuracy
rep_valid[rep_valid$probability >= 0.5, "prediction"] = 1



prop.table(table(rep_valid$prediction))

table(rep_valid$prediction)


repurchase_validation_13662388 <- rep_valid %>% select(ID, probability,prediction)


write_xlsx(repurchase_validation_13662388,"C:\\Users\\Ganesh\\Documents\\Dam ass 2\\repurchase_validation_13662388.xlsx")


