# Christina Ling
# IST 565 Data Mining
# final project | decision tree

# ----- preprocess ----- 

# check response variables str for factor
str(fc.discrete$healthy_feeling)
fc.discrete$healthy_feeling = hf.discrete
str(fc.discrete$healthy_feeling)

## randomly selection 10% for test data
n <- round(nrow(fc.discrete)/10)
s <- sample(1:nrow(fc.discrete), n)

#create test and training datasets
test <- fc.discrete[s,]
train <- fc.discrete[-s,]


# ----- decision tree ------
## ONCE: install.packages("rpart")
library("rpart") 
## ONCE: install.packages("rattle")
library("rattle")

# use all data
tree.fit_all <- rpart(train$healthy_feeling ~.,
                      data=train,
                      method="class")
summary(tree.fit_all)

# predict test data
# remove healthy_feeling from test
test_label <- test[,which(colnames(test) == "healthy_feeling")]
test <- test[,-(which(colnames(test) == "healthy_feeling"))]
predict_all <- predict(tree.fit_all, test, type="class")
table(predict_all, test_label)

# visualize the tree
fancyRpartPlot(tree.fit_all)

# ----- sub datasets -----

### exercise dataset

# create test and training dataset
test <- exercise[s,]
train <- exercise[-s,]
# check structure of response variable
str(train$healthy_feeling)
# setup tree
tree.fit_exercise <- rpart(train$healthy_feeling ~.,
                           data=train,
                           method="class")
summary(tree.fit_exercise)
#prediction
test_label <- test[,which(colnames(test) == "healthy_feeling")]
test <- test[,-(which(colnames(test) == "healthy_feeling"))]
predict_exercise <- predict(tree.fit_exercise, test, type="class")
table(predict_exercise, test_label)
#visual
fancyRpartPlot(tree.fit_exercise)


### Upbringing dataset
# create test and training dataset
test <- upbringing[s,]
train <- upbringing[-s,]
# check structure of response variable
str(train$healthy_feeling)
str(train)
# setup tree
tree.fit_upbringing <- rpart(train$healthy_feeling ~.,
                           data=train,
                           method="class")
summary(tree.fit_upbringing)
#prediction
test_label <- test[,which(colnames(test) == "healthy_feeling")]
test <- test[,-(which(colnames(test) == "healthy_feeling"))]
predict_upbringing <- predict(tree.fit_upbringing, test, type="class")
table(predict_upbringing, test_label)
#visual
fancyRpartPlot(tree.fit_upbringing)


### Food intake dataset
# create test and training dataset
test <- food.intake[s,]
train <- food.intake[-s,]
# check structure of response variable
str(train$healthy_feeling)
str(train)
# setup tree
tree.fit_intake <- rpart(train$healthy_feeling ~.,
                           data=train,
                           method="class")
summary(tree.fit_intake)
#prediction
test_label <- test[,which(colnames(test) == "healthy_feeling")]
test <- test[,-(which(colnames(test) == "healthy_feeling"))]
predict_intake <- predict(tree.fit_intake, test, type="class")
table(predict_intake, test_label)
#visual
fancyRpartPlot(tree.fit_intake)


### Food association dataset
# create test and training dataset
test <- food.asso[s,]
train <- food.asso[-s,]
# check structure of response variable
str(train$healthy_feeling)
str(train)
# setup tree
tree.fit_asso <- rpart(train$healthy_feeling ~.,
                           data=train,
                           method="class")
summary(tree.fit_asso)
#prediction
test_label <- test[,which(colnames(test) == "healthy_feeling")]
test <- test[,-(which(colnames(test) == "healthy_feeling"))]
predict_asso <- predict(tree.fit_asso, test, type="class")
table(predict_asso, test_label)
#visual
fancyRpartPlot(tree.fit_asso)


### health
# create test and training dataset
test <- health[s,]
train <- health[-s,]
# check structure of response variable
str(train$healthy_feeling)
str(train)
# setup tree
tree.fit_health <- rpart(train$healthy_feeling ~.,
                           data=train,
                           method="class")
summary(tree.fit_health)
#prediction
test_label <- test[,which(colnames(test) == "healthy_feeling")]
test <- test[,-(which(colnames(test) == "healthy_feeling"))]
predict_health <- predict(tree.fit_health, test, type="class")
table(predict_health, test_label)
#visual
fancyRpartPlot(tree.fit_health)


### status
# create test and training dataset
test <- status[s,]
train <- status[-s,]
# check structure of response variable
str(train$healthy_feeling)
str(train)
#discretization on GPA
train$GPA <- cut(train$GPA,
                       breaks=c(-Inf, 3.2, 3.5, 3.7, Inf),
                       labels=c("very bad", "bad", "good", "very good"))
train$GPA <- as.factor(train$GPA)
test$GPA <- cut(test$GPA,
                 breaks=c(-Inf, 3.2, 3.5, 3.7, Inf),
                 labels=c("very bad", "bad", "good", "very good"))
test$GPA <- as.factor(test$GPA)
# setup tree
tree.fit_status <- rpart(train$healthy_feeling ~.,
                           data=train,
                           method="class")
summary(tree.fit_status)
#prediction
test_label <- test[,which(colnames(test) == "healthy_feeling")]
test <- test[,-(which(colnames(test) == "healthy_feeling"))]
predict_status <- predict(tree.fit_status, test, type="class")
table(predict_status, test_label)
#visual
fancyRpartPlot(tree.fit_status)
               
