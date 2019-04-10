# IST 565 Data Mining
# Final Project | Random Forest
# Christina Ling

# ----- preprocessing -----
# check structure of predicted variable
str(food.choices.numbonly$healthy_feeling)
# change to ordered factor
food.choices.numbonly$healthy_feeling <- as.factor(food.choices.numbonly$healthy_feeling)
food.choices.numbonly$healthy_feeling <- ordered(food.choices.numbonly$healthy_feeling)

# randomly selection 10% for test data
n <- round(nrow(food.choices.numbonly)/10)
s <- sample(1:nrow(food.choices.numbonly), n)

# create test and training dataset
test <- food.choices.numbonly[s,]
train <- food.choices.numbonly[-s,]

# ----- Random Forest -----
## ONCE: install.packages("randomForest")
library("randomForest")

#set up RF
fit.all <- randomForest(healthy_feeling~.,
                       data=train)
print(fit.all)
#predict
test_label <- test[,(which(colnames(test) == "healthy_feeling"))]
test <- test[,-(which(colnames(test) == "healthy_feeling"))]
pred.all <- predict(fit.all, test)
table(pred.all, test_label)
#visual treesize
hist(treesize(fit.all))
varImpPlot(fit.all)

# ----- Sub dataset -----

### exercise ###
#structure of predicted variable
str(exercise.num$healthy_feeling)
# create test and train dataset
test <- exercise.num[s,]
train <- exercise.num[-s,]
str(train)
#set up RF
fit.exercise <- randomForest(healthy_feeling~.,
                        data=train)
print(fit.exercise)
#predict
test_label <- test[,(which(colnames(test) == "healthy_feeling"))]
test <- test[,-(which(colnames(test) == "healthy_feeling"))]
pred.exercise <- predict(fit.exercise, test)
table(pred.exercise, test_label)
#visual treesize
hist(treesize(fit.exercise))
varImpPlot(fit.exercise)


### Upbringing dataset ###
#structure of predicted variable
str(upbringing.num$healthy_feeling)
# create test and train dataset
test <- upbringing.num[s,]
train <- upbringing.num[-s,]
str(train)
#set up RF
fit.upbringing <- randomForest(healthy_feeling~.,
                             data=train)
print(fit.upbringing)
#predict
test_label <- test[,(which(colnames(test) == "healthy_feeling"))]
test <- test[,-(which(colnames(test) == "healthy_feeling"))]
pred.upbringing <- predict(fit.upbringing, test)
table(pred.upbringing, test_label)
#visual treesize
hist(treesize(fit.upbringing))
varImpPlot(fit.upbringing)


### Food intake dataset ###

#structure of predicted variable
str(food.intake.num$healthy_feeling)
# create test and train dataset
test <- food.intake.num[s,]
train <- food.intake.num[-s,]
str(train)
#set up RF
fit.intake <- randomForest(healthy_feeling~.,
                             data=train)
print(fit.intake)
#predict
test_label <- test[,(which(colnames(test) == "healthy_feeling"))]
test <- test[,-(which(colnames(test) == "healthy_feeling"))]
pred.intake <- predict(fit.intake, test)
table(pred.intake, test_label)
#visual treesize
hist(treesize(fit.intake))
varImpPlot(fit.intake)


### Food association ###
#structure of predicted variable
str(food.asso.num$healthy_feeling)
# create test and train dataset
test <- food.asso.num[s,]
train <- food.asso.num[-s,]
str(train)
#set up RF
fit.asso <- randomForest(healthy_feeling~.,
                             data=train)
print(fit.asso)
#predict
test_label <- test[,(which(colnames(test) == "healthy_feeling"))]
test <- test[,-(which(colnames(test) == "healthy_feeling"))]
pred.asso <- predict(fit.asso, test)
table(pred.asso, test_label)
#visual treesize
hist(treesize(fit.asso))
varImpPlot(fit.asso)


### Health dataset ###
#structure of predicted variable
str(health.num$healthy_feeling)
# create test and train dataset
test <- health.num[s,]
train <- health.num[-s,]
str(train)
#set up RF
fit.health <- randomForest(healthy_feeling~.,
                             data=train)
print(fit.health)
#predict
test_label <- test[,(which(colnames(test) == "healthy_feeling"))]
test <- test[,-(which(colnames(test) == "healthy_feeling"))]
pred.health <- predict(fit.health, test)
table(pred.health, test_label)
#visual treesize
hist(treesize(fit.health))
varImpPlot(fit.health)


### Status ###
#structure of predicted variable
str(status.num$healthy_feeling)
# create test and train dataset
test <- status.num[s,]
train <- status.num[-s,]
str(train)
#set up RF
fit.status <- randomForest(healthy_feeling~.,
                             data=train)
print(fit.status)
#predict
test_label <- test[,(which(colnames(test) == "healthy_feeling"))]
test <- test[,-(which(colnames(test) == "healthy_feeling"))]
pred.status <- predict(fit.status, test)
table(pred.status, test_label)
#visual treesize
hist(treesize(fit.status))
varImpPlot(fit.status)
