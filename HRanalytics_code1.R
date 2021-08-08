#### HR analytics
#### Goal: 1.Classify the employee into staying or leaving categories
####       2.Identify important factors affecting the decision making of staying or leaving.
#### Step 1. Load libraries and datasets
#### Step 2. Data Preprocessing (blank cells to NA,characters to factors)
#### Step 3. Explore the data with simple plots, to see relationship of outcome variable with predictors
#### Step 4. Split data into Testing and training
#### Step 5. Fit the classification models (CART, Random Forest, NB,KNN)
#### Step 6. Us Regularized Regression Methods for variable selection
#### Step 7. Summarize
library(dplyr)
library(tree)
library(rpart)
library(rpart.plot)
library(randomForest)
library(ggplot2)
library(glmnet)
library(gt)
library(e1071)
library(ROCR)

# Step 1 load datasets (kaggle had Train and test datasets for HR analytics)
# https://www.kaggle.com/arashnic/hr-analytics-job-change-of-data-scientists?select=sample_submission.csv
aug_train <- read.csv("~/Downloads/archive/aug_train.csv",header=T, na.strings=c(""," ","NA")) #na.strings=c(""," ","NA")
aug_test <- read.csv("~/Downloads/archive/aug_test.csv",header=T, na.strings=c(""," ","NA"))

# Variable names
names(aug_train)
# First 4 rows of the dataset
aug_train[1:4,]

## Data Preprocessing ##
# # Transform experience onto 6 levels 
aug_train$experience <- as.character(aug_train$experience)
aug_train$experience <- replace(aug_train$experience, aug_train$experience%in%c("<1"), "less_than_one")
aug_train$experience <- replace(aug_train$experience, aug_train$experience%in%c(">20"), "20plus")
aug_train$experience <- replace(aug_train$experience, aug_train$experience%in%c("1","2","3","4","5"), "1_5")
aug_train$experience <- replace(aug_train$experience, aug_train$experience%in%c("6","7","8","9","10"), "6_10")
aug_train$experience <- replace(aug_train$experience, aug_train$experience%in%c("11","12","13","14","15"), "11_15")
aug_train$experience <- replace(aug_train$experience, aug_train$experience%in%c("16","17","18","19","20"), "16_20")
table(aug_train$experience)

aug_test$experience <- as.character(aug_test$experience)
aug_test$experience <- replace(aug_test$experience, aug_test$experience%in%c("<1"), "less_than_one")
aug_test$experience <- replace(aug_test$experience, aug_test$experience%in%c(">20"), "20plus")
aug_test$experience <- replace(aug_test$experience, aug_test$experience%in%c("1","2","3","4","5"), "1_5")
aug_test$experience <- replace(aug_test$experience, aug_test$experience%in%c("6","7","8","9","10"), "6_10")
aug_test$experience <- replace(aug_test$experience, aug_test$experience%in%c("11","12","13","14","15"), "11_15")
aug_test$experience <- replace(aug_test$experience, aug_test$experience%in%c("16","17","18","19","20"), "16_20")
table(aug_test$experience)

# Transform the company size into 4 levels - micro, small, medium, large

aug_train$company_size <- replace(aug_train$company_size, aug_train$company_size%in%c("<10","10/49"), "micro")
aug_train$company_size <- replace(aug_train$company_size, aug_train$company_size%in%c("50-99","100-500"), "small")
aug_train$company_size <- replace(aug_train$company_size, aug_train$company_size%in%c("500-999","1000-4999"), "medium")
aug_train$company_size <- replace(aug_train$company_size, aug_train$company_size%in%c("5000-9999","10000+"), "large")
table(aug_train$company_size)

aug_test$company_size <- replace(aug_test$company_size, aug_test$company_size%in%c("<10","10/49"), "micro")
aug_test$company_size <- replace(aug_test$company_size, aug_test$company_size%in%c("50-99","100-500"), "small")
aug_test$company_size <- replace(aug_test$company_size, aug_test$company_size%in%c("500-999","1000-4999"), "medium")
aug_test$company_size <- replace(aug_test$company_size, aug_test$company_size%in%c("5000-9999","10000+"), "large")
table(aug_test$company_size)

# Transform last_new_job
aug_train$last_new_job <- replace(aug_train$last_new_job, aug_train$last_new_job%in%c( ">4"), "plus4")
table(aug_train$last_new_job)
aug_test$last_new_job <- replace(aug_test$last_new_job, aug_test$last_new_job%in%c( ">4"), "plus4")
table(aug_test$last_new_job)

# Data Preprocessing (as.factor())
aug_train[,c(2,4:12,14)] <- lapply(aug_train[,c(2,4:12,14)], as.factor)
aug_test[,c(2,4:12)] <- lapply(aug_test[,c(2,4:12)], as.factor)
aug_train$target <-as.factor(aug_train$target)

# Preprocessing-Convert target feature from 0,1 to Stay, Leave
levels(aug_train$target) <- c('Stay', 'Leave')

# Count NA columnwise in test and train - 
sapply(aug_train, function(x) sum(is.na(x))) 
sapply(aug_test, function(x) sum(is.na(x)))
# Remove NA
aug_train <- aug_train[complete.cases(aug_train),]
aug_test  <- aug_test[complete.cases(aug_test), ]
# Count NA columnwise in test and train
sapply(aug_train, function(x) sum(is.na(x))) 
sapply(aug_test, function(x) sum(is.na(x)))

# EDA
summary(aug_train)
summary(aug_test)
# Acc. train dataset(aug_train) 25% employees leave, 75% stay
str(aug_train)
str(aug_test)
nrow(aug_train)
sum(is.na(aug_train))# 4996
table(aug_train$target)
levels(aug_train$target)
table(aug_train$gender) # looks wierd, may be due to NA or blank values
table(aug_train$gender)
table(aug_train$relevent_experience)
levels(aug_train$enrolled_university)
table(aug_train$enrolled_university) # 3 classes
table(aug_train$education_level)
table(aug_train$company_size)
table(aug_train$company_type)
levels(aug_train$company_type)#6
table(aug_train$major_discipline)
table(aug_train$last_new_job)
levels(aug_train$last_new_job)
levels(aug_train$company_size)
table(aug_train$experience)


# Plots
# Relation of relevant experience and target

ggplot(aug_train) + aes(x = target, fill=relevent_experience) +
  geom_bar() 
# enrolled univ
ggplot(aug_train) + aes(x = target, fill=enrolled_university) +
  geom_bar() 
# Gender
ggplot(aug_train) + aes(x = target, fill=gender) +
  geom_bar() 
# boxplot training hrs  # NOT STRONG RELATIONSHIP
ggplot(aug_train) + aes(x = target, y = training_hours, fill = target)+
  geom_boxplot() + scale_color_brewer(palette="Dark2") #+stat_summary(fun.y=mean, geom="point", shape=23, size=4)

plot(aug_train$experience) # 1 has more obs as I added NA to 1, I think not a good idea?
plot(aug_train$gender)
plot(aug_train$relevent_experience)
plot(aug_train$enrolled_university)
plot(aug_train$education_level)
plot(aug_train$major_discipline)
plot(aug_train$company_size)
plot(aug_train$company_type)
plot(aug_train$last_new_job)
hist(aug_train$training_hours)
plot(aug_train$target)

# mosaic plot
discrete <- names(aug_train)[c(2,4:12)]
for(v in discrete){
  t <- table(aug_train[,which(names(aug_train)==v)], aug_train$target)
  colnames(t) <- c("Stay", "leave")
  mosaicplot(t, main=v)
}
# EDA: years of experience, company_size, seem strong relationship with target

# Fit Decision Tree model on complete dataset # had lot of errors(tree too large) or only root node, had to inc. minsize to 2000
tree1 <- tree(target ~  gender + relevent_experience + enrolled_university +
                education_level + major_discipline + experience+
                company_type + company_size+last_new_job + training_hours,
              data = aug_train,
              split = "gini",
              mindev = 0.001,mincut = 5,
              minsize = 2000)
par(mfrow = c(1,1))
plot(tree1)
text(tree1, cex = 0.75) # not great as levels are not represented correctly at each node
summary(tree1) # output variables used for tree construction
# PREDICTION from single tree -(All Stay)
table(predict(tree1, aug_train, type = "class")) # 100% 'Stay' prediction
 # output variables used for tree construction

# use rpart() function for decision tree 
tree_rpart <- rpart(target ~ gender + relevent_experience + enrolled_university +
                      education_level + major_discipline + experience+
                      company_type + company_size+last_new_job + training_hours,
                    data = aug_train,
                    method="class",parms=list(split="gini"),
                    control =rpart.control(minsplit=10,minbucket=50, cp=.001))

rpart.plot(tree_rpart)
text(tree_rpart, cex = 0.75)
summary(tree_rpart)
tree_rpart$variable.importance
tree_rpart$parms


# GIVES error: Error in text.rpart(rpart_2, cex = 0.75) : fit is not a tree, just a root
pred <- predict(tree_rpart, aug_train, type = "class")
# table(predict(tree_rpart, aug_test, type = "class"))
# (table(predict(tree_rpart, aug_train, type = "class"))/ nrow(aug_train))*100
# (table(aug_train$target == pred)/ nrow(aug_train))*100
mat <- table(aug_train$target, pred)
accuracy <- sum(diag(mat))/sum(mat)

#USE RANDOM FOREST classification model
# randomForest(formula = theFormula, data = trainset,mtry = 3,ntree = 500,importance = TRUE, do.trace = 100) 

set.seed(123)
rf <- randomForest(as.factor(target) ~ gender + relevent_experience + enrolled_university+
                    education_level + major_discipline+experience + company_type +company_size+last_new_job + training_hours, 
                  data = aug_train[complete.cases(aug_train),], replace=TRUE, importance=TRUE, 
                  ntree=100, mtry=5,) #nodesize = 5
rf
#getTree(rf, labelVar=TRUE)
plot(rf)
varImpPlot(rf, main = "RandomForest Accuracy and Gini Impurity", cex = 0.75)
print(rf) #gives OOB, confusion matrix, prediction error rate per class per tree
print(rf$err.rate[1:5,])
importance(rf)
rf$err.rate
data.matrix(rf$confusion, rownames= TRUE)
rf$predicted
rf$votes
rf$importance
# Plot sensitivity and specificity from randomForest predictions

predrf <- predict(rf,aug_train[complete.cases(aug_train),],type = "prob" )
rf.pred <- predrf[,2]
pred.obj <- prediction(rf.pred, aug_train$target)
rf.perf <- performance(pred.obj, "rec","prec")
plot(rf.perf, col = rainbow(10), ylab = "Sensitivity/Recall", xlab = "Specificity/Precision",
     main = "ROC curve")

ROC.perf <- performance(pred.obj, "fpr","tpr")
plot(ROC.perf, col = rainbow(10), main = "ROC curve")

# plot  (rf.perf@alpha.values[[1]],rf.perf@x.values[[1]]);
# lines (rf.perf@alpha.values[[1]],rf.perf@y.values[[1]]);
# lines (ROC.perf@alpha.values[[1]],ROC.perf@x.values[[1]]);


# PREDICT FROM Random Forest
table(predict(rf, aug_train))
rf_pred <- predict(rf, aug_train, type = "class")
matrixrf <- table(aug_train$target, rf_pred)
accuracy <- sum(diag(matrixrf))/sum(matrixrf)
# ((7457+562)/8955)*100 verifying the accuracy

# Prediction with naiveBayes model
library(e1071)

# Split data into Training and testing for cross validation
set.seed(081)
n <- nrow(aug_train)
trainrows <- sample(1:n, replace = FALSE, size= trunc(0.7*n))
testrows <- (1:n)[-trainrows]
Xtrain <- (aug_train[trainrows, c(1,4:13)])
Xtest <- (aug_train[testrows, c(1,4:13)])
ytrain <- aug_train$target[trainrows]
ytest <- aug_train$target[testrows]
trainset <- as.data.frame(cbind(ytrain, Xtrain))

nb <- naiveBayes(ytrain ~ gender + relevent_experience + enrolled_university +
                   education_level + major_discipline + experience+
                   company_type + company_size+last_new_job + training_hours,
                 data = trainset)

nb$apriori/nrow(trainset)
nb$apriori
# nb$tables$company_size*100
# nb$tables$company_type*100
# nb$tables$relevent_experience*100
# nb$tables$education_level*100
# nb$tables$major_discipline*100
predict_out <- as.character(predict(nb, newdata = Xtest))
comparison <- cbind(predict_out, ytest)
comparison[1:10,]
mean(apply(comparison, 1, function(x){x[1]==x[2]}))

# Accuracy of NaiveBayes Model
table(predict(nb, Xtest))
nb_pred <- predict(nb, Xtest, type = "class")
matrixnb <- table(ytest, nb_pred)
accuracy <- sum(diag(matrixnb))/sum(matrixnb)

# VARIABLE SELECTION with Regularized Regression MOdels
# LASSO 
# Preprocess data into matrix for LASSO/RIDGE function
x <- data.frame(aug_train[, c(4:13)])
x <- data.matrix(x)
y <- factor(aug_train$target)[complete.cases(aug_train)]

lasso_out <- cv.glmnet(x = x, y = y, family = 'binomial', alpha = 1) # LASSO
lasso_out$cvm
which.min(lasso_out$cvm) #39
mean(lasso_out$cvm)
plot(lasso_out$cvm, main = "39th lambda with min CV error rate")
lambda_lasso <- lasso_out$lambda[which.min(lasso_out$cvm)] #0.0008366626
out <- glmnet(x=x, y=y, family="binomial", alpha=1, lambda=lasso_out$lambda)
dim(out$beta)
out$beta[,1]; out$lambda[1]
out$beta[,2]; out$lambda[2]
out$beta[,3]; out$lambda[3]
out$beta[,which.min(lasso_out$cvm)] # coefficients of predictor var
# gender and training_hours were pushed out
table(lasso_pred)

#  RIDGE
# Preprocess data into matrix for LASSO/RIDGE function
x <- data.frame(aug_train[, c(4:13)])
x <- data.matrix(x)
y <- factor(aug_train$target)[complete.cases(aug_train)]

ridge_out <- cv.glmnet(x = x, y = y, family = 'binomial', alpha = 0) # Ridge
which.min(ridge_out$cvm) #81
mean(ridge_out$cvm)
plot(ridge_out$cvm)
lambda_ridge <- ridge_out$lambda[which.min(ridge_out$cvm)] #0.01395637
out <- glmnet(x=x, y=y, family="binomial", alpha=0, lambda=ridge_out$lambda)
dim(out$beta)
out$beta[,1]; out$lambda[1]
out$beta[,2]; out$lambda[2]
out$beta[,3]; out$lambda[3]
out$beta[,which.min(ridge_out$cvm)]
order(out$beta[,which.min(ridge_out$cvm)], decreasing = TRUE)

# After considering all the model and the variable importance, coefficients and MeanDecreaseGini from
# CART, LASSO/Ridge, RandomForest respectively, I think these 4 variables are of great importance 
# for leave/stay decision. 
# Training hours
# Last New Job
# Major Discipline
# Experience
str(aug_train)

mosaicplot(table(aug_train$last_new_job, aug_train$target), 
           main="Years in current role with job-change",
           xlab = "Years in the current role",
           cex = 0.75)

mosaicplot(table(aug_train$experience, aug_train$target), 
           main="Years of experience with job-change",
           xlab = "Work experience (years)",
           cex = 0.75)

mosaicplot(table(aug_train$major_discipline, aug_train$target), 
           main="Major Discline vs job-change",
           xlab = "Subject Major",
           cex = 0.75)
# Plot the MISSCLASIFICATION RATE OF MODELS ####
x <- c('CART' ,'RF','LASSO','RIDGE')
error_rate <- c(0.1646, 0.1759, 0.8879, 0.89)
plot(error_rate, type = 'o',col = "red", xlab = "Model", ylab = "Misclassification Error",
     main = "Misclassification error rate by model", ylim=c(0, 1))
axis(1, at=seq(1,8,2),labels = x)

set.seed(123)
rf <- randomForest(as.factor(target) ~ relevent_experience + major_discipline+experience + company_type +company_size+last_new_job + training_hours, 
                   data = aug_train[complete.cases(aug_train),], replace=TRUE, importance=TRUE, 
                   ntree=100, mtry=5, nodesize = 5)

print(rf)
rf$importance
varImpPlot(rf, main = "RandomForest (Accuracy and Gini Impurity)")

### PREDICTION ON TEST SET #####

# PREDICTION ON TEST SET PROVIDED ON KAGGLE (their target class was not provided hence hard to predict the accuracy on testing data from kaggle)
data.frame(predict(rf, aug_test, type = 'class'))


