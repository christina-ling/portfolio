# IST 565 Data Mining
# Final Project | SVM
# Christina Ling

# ----- preprocess -----

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

# ----- SVM -----
## ONCE: install.packages("e1071")
library("e1071")

## polynomial
svm_fit_all_p <- svm(healthy_feeling~.,
                     data=train,
                     kernel="polynomial",
                     cost=0.1,
                     scale=F)
print(svm_fit_all_p)
#predictions
#remove label from test
test_label <- test[,(which(colnames(test) == "healthy_feeling"))]
test <- test[,-(which(colnames(test) == "healthy_feeling"))]
pred_all_p <- predict(svm_fit_all_p, test, type="class")
table(pred_all_p, test_label)

## Linear
svm_fit_all_l <- svm(healthy_feeling~.,
                     data=train,
                     kernel="linear",
                     cost=0.1,
                     scale=F)
print(svm_fit_all_l)
#prediction
pred_all_l <- predict(svm_fit_all_l, test, type="class")
table(pred_all_l, test_label)

## radial
svm_fit_all_r <- svm(healthy_feeling~.,
                     data=train,
                     kernel="radial",
                     cost=0.1,
                     scale=F)
print(svm_fit_all_r)
#prediction
pred_all_r <- predict(svm_fit_all_r, test, type="class")
table(pred_all_r, test_label)

##  visualize results
results <- c(pred_all_p, pred_all_l, pred_all_r)

results <- gsub(1, "unhealthy", results)
results <- gsub(2, "average", results)
results <- gsub(3, "healthy", results)
model <- c(replicate(12, "polynomial", simplify=T),
           replicate(12, "linear", simplify = T),
           replicate(12, "radial", simplify = T))

results <- data.frame(prediction=results,
                      model)
plot(results)


# ----- Sub datasets -----

### exercise dataset ###

#structure of predicted variable
str(exercise.num$healthy_feeling)
# create test and train dataset
test <- exercise.num[s,]
train <- exercise.num[-s,]
str(train)
##polynomial SVM
svm_fit_exercise_p <- svm(healthy_feeling~.,
                          data=train,
                          kernel="polynomial",
                          cost=0.1,
                          scale=F)
print(svm_fit_exercise_p)
#prediction
#remove label from test
test_label <- test[,(which(colnames(test) == "healthy_feeling"))]
test <- test[,-(which(colnames(test) == "healthy_feeling"))]
pred_exercise_p <- predict(svm_fit_exercise_p, test, type="class")
table(pred_exercise_p, test_label)
#visualize
plot(svm_fit_exercise_p,
     data=train,
     exercise~weight)
##linear SVM
svm_fit_exercise_l <- svm(healthy_feeling~.,
                          data=train,
                          kernel="linear",
                          cost=0.1,
                          scale=F)
print(svm_fit_exercise_l)
#prediction
pred_exercise_l <- predict(svm_fit_exercise_l, test, type="class")
table(pred_exercise_l, test_label)
#visualize
plot(svm_fit_exercise_l,
     data=train,
     exercise~weight)
##radial SVM
svm_fit_exercise_r <- svm(healthy_feeling~.,
                          data=train,
                          kernel="radial",
                          cost=0.1,
                          scale=F)
print(svm_fit_exercise_r)
#prediction
pred_exercise_r <- predict(svm_fit_exercise_r, test, type="class")
table(pred_exercise_r, test_label)
#visualize
plot(svm_fit_exercise_r,
     data=train,
     exercise~weight)



### Upbringing dataset ###
#structure of predicted variable
str(upbringing.num$healthy_feeling)
# create test and train dataset
test <- upbringing.num[s,]
train <- upbringing.num[-s,]
str(train)
##polynomial SVM
svm_fit_upbringing_p <- svm(healthy_feeling~.,
                          data=train,
                          kernel="polynomial",
                          cost=0.1,
                          scale=F)
print(svm_fit_upbringing_p)
#prediction
#remove label from test
test_label <- test[,(which(colnames(test) == "healthy_feeling"))]
test <- test[,-(which(colnames(test) == "healthy_feeling"))]
pred_upbringing_p <- predict(svm_fit_upbringing_p, test, type="class")
table(pred_upbringing_p, test_label)
#visualize
plot(svm_fit_upbringing_p,
     data=train,
     income~parent_cook)
##linear SVM
svm_fit_upbringing_l <- svm(healthy_feeling~.,
                          data=train,
                          kernel="linear",
                          cost=0.1,
                          scale=F)
print(svm_fit_upbringing_l)
#prediction
pred_upbringing_l <- predict(svm_fit_upbringing_l, test, type="class")
table(pred_upbringing_l, test_label)
#visualize
plot(svm_fit_upbringing_l,
     data=train,
     income~parent_cook)
##radial SVM
svm_fit_upbringing_r <- svm(healthy_feeling~.,
                          data=train,
                          kernel="radial",
                          cost=0.1,
                          scale=F)
print(svm_fit_upbringing_r)
#prediction
pred_upbringing_r <- predict(svm_fit_upbringing_r, test, type="class")
table(pred_upbringing_r, test_label)
#visualize
plot(svm_fit_upbringing_r,
     data=train,
     income~parent_cook)


### Food intake dataset ###
#structure of predicted variable
str(food.intake.num$healthy_feeling)
# create test and train dataset
test <- food.intake.num[s,]
train <- food.intake.num[-s,]
str(train)
##polynomial SVM
svm_fit_intake_p <- svm(healthy_feeling~.,
                          data=train,
                          kernel="polynomial",
                          cost=0.1,
                          scale=F)
print(svm_fit_intake_p)
#prediction
#remove label from test
test_label <- test[,(which(colnames(test) == "healthy_feeling"))]
test <- test[,-(which(colnames(test) == "healthy_feeling"))]
pred_intake_p <- predict(svm_fit_intake_p, test, type="class")
table(pred_intake_p, test_label)
#visualize
plot(svm_fit_intake_p,
     data=train,
     comfort_food_reasons~greek_food)
##linear SVM
svm_fit_intake_l <- svm(healthy_feeling~.,
                          data=train,
                          kernel="linear",
                          cost=0.1,
                          scale=F)
print(svm_intake_exercise_l)
#prediction
pred_intake_l <- predict(svm_fit_intake_l, test, type="class")
table(pred_intake_l, test_label)
#visualize
plot(svm_fit_intake_l,
     data=train,
     comfort_food_reasons~greek_food)
##radial SVM
svm_fit_intake_r <- svm(healthy_feeling~.,
                          data=train,
                          kernel="radial",
                          cost=0.1,
                          scale=F)
print(svm_fit_intake_r)
#prediction
pred_intake_r <- predict(svm_fit_intake_r, test, type="class")
table(pred_intake_r, test_label)
#visualize
plot(svm_fit_intake_r,
     data=train,
     comfort_food_reasons~greek_food)


### Food Association ###
#structure of predicted variable
str(food.asso.num$healthy_feeling)
# create test and train dataset
test <- food.asso.num[s,]
train <- food.asso.num[-s,]
str(train)
##polynomial SVM
svm_fit_asso_p <- svm(healthy_feeling~.,
                          data=train,
                          kernel="polynomial",
                          cost=0.1,
                          scale=F)
print(svm_fit_asso_p)
#prediction
#remove label from test
test_label <- test[,(which(colnames(test) == "healthy_feeling"))]
test <- test[,-(which(colnames(test) == "healthy_feeling"))]
pred_asso_p <- predict(svm_fit_asso_p, test, type="class")
table(pred_asso_p, test_label)
#visualize
plot(svm_fit_asso_p,
     data=train,
     coffee~drink)
##linear SVM
svm_fit_asso_l <- svm(healthy_feeling~.,
                          data=train,
                          kernel="linear",
                          cost=0.1,
                          scale=F)
print(svm_fit_asso_l)
#prediction
pred_asso_l <- predict(svm_fit_asso_l, test, type="class")
table(pred_asso_l, test_label)
#visualize
plot(svm_fit_asso_l,
     data=train,
     coffee~drink)
##radial SVM
svm_fit_asso_r <- svm(healthy_feeling~.,
                          data=train,
                          kernel="radial",
                          cost=0.1,
                          scale=F)
print(svm_fit_asso_r)
#prediction
pred_asso_r <- predict(svm_fit_asso_r, test, type="class")
table(pred_asso_r, test_label)
#visualize
plot(svm_fit_asso_r,
     data=train,
     coffee~drink)


### Health dataset ###
#structure of predicted variable
str(health.num$healthy_feeling)
# create test and train dataset
test <- health.num[s,]
train <- health.num[-s,]
str(train)
##polynomial SVM
svm_fit_health_p <- svm(healthy_feeling~.,
                          data=train,
                          kernel="polynomial",
                          cost=0.1,
                          scale=F)
print(svm_fit_health_p)
#prediction
#remove label from test
test_label <- test[,(which(colnames(test) == "healthy_feeling"))]
test <- test[,-(which(colnames(test) == "healthy_feeling"))]
pred_health_p <- predict(svm_fit_health_p, test, type="class")
table(pred_health_p, test_label)
#visualize
plot(svm_fit_health_p,
     data=train,
     eating_changes1~ideal_diet)
##linear SVM
svm_fit_health_l <- svm(healthy_feeling~.,
                          data=train,
                          kernel="linear",
                          cost=0.1,
                          scale=F)
print(svm_fit_health_l)
#prediction
pred_health_l <- predict(svm_fit_health_l, test, type="class")
table(pred_health_l, test_label)
#visualize
plot(svm_fit_health_l,
     data=train,
     eating_changes1~ideal_diet)
##radial SVM
svm_fit_health_r <- svm(healthy_feeling~.,
                          data=train,
                          kernel="radial",
                          cost=0.1,
                          scale=F)
print(svm_fit_health_r)
#prediction
pred_health_r <- predict(svm_fit_health_r, test, type="class")
table(pred_health_r, test_label)
#visualize
plot(svm_fit_health_r,
     data=train,
     eating_changes~ideal_diet)


### Status dataset ###
#structure of predicted variable
str(status.num$healthy_feeling)
# create test and train dataset
test <- status.num[s,]
train <- status.num[-s,]
str(train)
##polynomial SVM
svm_fit_status_p <- svm(healthy_feeling~.,
                          data=train,
                          kernel="polynomial",
                          cost=0.1,
                          scale=F)
print(svm_fit_status_p)
#prediction
#remove label from test
test_label <- test[,(which(colnames(test) == "healthy_feeling"))]
test <- test[,-(which(colnames(test) == "healthy_feeling"))]
pred_status_p <- predict(svm_fit_status_p, test, type="class")
table(pred_status_p, test_label)
#visualize
plot(svm_fit_status_p,
     data=train,
     grade_level~life_rewarding)
##linear SVM
svm_fit_status_l <- svm(healthy_feeling~.,
                          data=train,
                          kernel="linear",
                          cost=0.1,
                          scale=F)
print(svm_fit_status_l)
#prediction
pred_status_l <- predict(svm_fit_status_l, test, type="class")
table(pred_status_l, test_label)
#visualize
plot(svm_fit_status_l,
     data=train,
     grade_level~life_rewarding)
##radial SVM
svm_fit_status_r <- svm(healthy_feeling~.,
                          data=train,
                          kernel="radial",
                          cost=0.1,
                          scale=F)
print(svm_fit_status_r)
#prediction
pred_status_r <- predict(svm_fit_status_r, test, type="class")
table(pred_status_r, test_label)
#visualize
plot(svm_fit_status_r,
     data=train,
     grade_level~life_rewarding)

