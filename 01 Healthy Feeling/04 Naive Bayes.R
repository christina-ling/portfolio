# IST 565 Data Mining
# Final Project | Naive Bayes
# Christina Ling

# ----- Preprocessing -----

## randomly selection 10% for test data
n <- round(nrow(food.choices.numbonly)/10)
s <- sample(1:nrow(food.choices.numbonly), n)

#create test and training datasets
test <- food.choices.numbonly[s,]
train <- food.choices.numbonly[-s,]

# check response variables str for factor
str(food.choices.numbonly$healthy_feeling)

# ----- Naive Bayes ------ 
## ONCE: install.packages("e1071")
library("e1071")
## ONCE: install.packages("naivebayes")
library("naivebayes")

#set up model
nb_all <- naiveBayes(healthy_feeling ~.,
                     data=train,
                     na.action= na.pass)

#predictions
#remove label from test
test_label <- test[,(which(colnames(test) == "healthy_feeling"))]
test <- test[,-(which(colnames(test) == "healthy_feeling"))]
nb_predict_all <- predict(nb_all, test)

#look at results
print(nb_predict_all)
table(nb_predict_all, test_label)

#visual
plot(nb_predict_all)

# ----- Sub datasets ------

### exercise
# format predicted variables
exercise.num$healthy_feeling <- as.factor(exercise.num$healthy_feeling)
exercise.num$healthy_feeling <- ordered(exercise.num$healthy_feeling)
#create test and training dataset
test <- exercise.num[s,]
train <- exercise.num[-s,]
#check structure of predicted variables
str(train$healthy_feeling)
# set up model
nb_exercise <- naiveBayes(healthy_feeling~.,
                          data=train,
                          na.action=na.pass,
                          usekernal=T)
#prediction
test_label <- test[,(which(colnames(test) == "healthy_feeling"))]
test <- test[,-(which(colnames(test) == "healthy_feeling"))]
nb_predict_exercise <- predict(nb_exercise, test)
#look at results
print(nb_predict_exercise)
table(nb_predict_exercise, test_label)
#visual
plot(nb_predict_exercise)


### Uprbringing dataset
#format prediction variables
upbringing.num$healthy_feeling <- as.factor(upbringing.num$healthy_feeling)
upbringing.num$healthy_feeling <- ordered(upbringing.num$healthy_feeling)
#create test and training dataset
test <- upbringing.num[s,]
train <- upbringing.num[-s,]
#check structure of predicted variables
str(train$healthy_feeling)
# set up model
nb_upbringing <- naiveBayes(healthy_feeling~.,
                          data=train,
                          na.action=na.pass,
                          usekernal=T)
#prediction
test_label <- test[,(which(colnames(test) == "healthy_feeling"))]
test <- test[,-(which(colnames(test) == "healthy_feeling"))]
nb_predict_upbringing <- predict(nb_upbringing, test)
#look at results
print(nb_predict_upbringing)
table(nb_predict_upbringing, test_label)
#visual
plot(nb_predict_upbringing)


### Food Intake Dataset
# format predicted variables
food.intake.num$healthy_feeling <- as.factor(food.intake.num$healthy_feeling)
food.intake.num$healthy_feeling <- ordered(food.intake.num$healthy_feeling)
#create test and training dataset
test <- food.intake.num[s,]
train <- food.intake.num[-s,]
#check structure of predicted variables
str(train$healthy_feeling)
# set up model
nb_intake <- naiveBayes(healthy_feeling~.,
                          data=train,
                          na.action=na.pass,
                          usekernal=T)
#prediction
test_label <- test[,(which(colnames(test) == "healthy_feeling"))]
test <- test[,-(which(colnames(test) == "healthy_feeling"))]
nb_predict_intake <- predict(nb_intake, test)
#look at results
print(nb_predict_intake)
table(nb_predict_intake, test_label)
#visual
plot(nb_predict_intake)


### Food association
# format predicted variables
food.asso.num$healthy_feeling <- as.factor(food.asso.num$healthy_feeling)
food.asso.num$healthy_feeling <- ordered(food.asso.num$healthy_feeling)
#create test and training dataset
test <- food.asso.num[s,]
train <- food.asso.num[-s,]
#check structure of predicted variables
str(train$healthy_feeling)
# set up model
nb_asso <- naiveBayes(healthy_feeling~.,
                          data=train,
                          na.action=na.pass,
                          usekernal=T)
#prediction
test_label <- test[,(which(colnames(test) == "healthy_feeling"))]
test <- test[,-(which(colnames(test) == "healthy_feeling"))]
nb_predict_asso <- predict(nb_asso, test)
#look at results
print(nb_predict_asso)
table(nb_predict_asso, test_label)
#visual
plot(nb_predict_asso)


### Health
# format predicted variables
health.num$healthy_feeling <- as.factor(health.num$healthy_feeling)
health.num$healthy_feeling <- ordered(health.num$healthy_feeling)
#create test and training dataset
test <- health.num[s,]
train <- health.num[-s,]
#check structure of predicted variables
str(train$healthy_feeling)
# set up model
nb_health <- naiveBayes(healthy_feeling~.,
                          data=train,
                          na.action=na.pass,
                          usekernal=T)
#prediction
test_label <- test[,(which(colnames(test) == "healthy_feeling"))]
test <- test[,-(which(colnames(test) == "healthy_feeling"))]
nb_predict_health <- predict(nb_health, test)
#look at results
print(nb_predict_health)
table(nb_predict_health, test_label)
#visual
plot(nb_predict_health)


### Status dataset
# format predicted variables
status.num$healthy_feeling <- as.factor(status.num$healthy_feeling)
status.num$healthy_feeling <- ordered(status.num$healthy_feeling)
#create test and training dataset
test <-status.num[s,]
train <- status.num[-s,]
#check structure of predicted variables
str(train$healthy_feeling)
# set up model
nb_status <- naiveBayes(healthy_feeling~.,
                          data=train,
                          na.action=na.pass,
                          usekernal=T)
#prediction
test_label <- test[,(which(colnames(test) == "healthy_feeling"))]
test <- test[,-(which(colnames(test) == "healthy_feeling"))]
nb_predict_status <- predict(nb_status, test)
#look at results
print(nb_predict_status)
table(nb_predict_status, test_label)
#visual
plot(nb_predict_status)

