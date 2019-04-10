# IST 565 Data Mining
# Final Project | kNN
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

k <- round(sqrt(nrow(food.choices.numbonly)))

#remove healthy feeling from test and train
train_label <- train[,which(colnames(train)=="healthy_feeling")]
train <- train[,-(which(colnames(train) == "healthy_feeling"))]
test_label <- test[,(which(colnames(test) == "healthy_feeling"))]
test <- test[,-(which(colnames(test) == "healthy_feeling"))]

# ----- kNN -----
## ONCE: install.packages("class")
library("class")
## ONCE: install.packages("descr")
library("descr")

# setup kNN
kNN_all <- class::knn(train=train,
                      test=test,
                      cl=train_label,
                      k=k,
                      prob=T)
print(kNN_all)
#results
table(kNN_all, test_label)
CrossTable(x=test_label,y=kNN_all,prop.chisq=F)
#visualize
plotdf <- data.frame(test,
                     predicted=kNN_all)
plotdf <- data.frame(x=plotdf$self_perception_weight,
                     y=plotdf$life_rewarding,
                     predicted=plotdf$predicted)
find_hull <- function(df) df[chull(df$x, df$y),]
boundary <- ddply(plotdf, .variables="predicted", .fun=find_hull)
ggplot(plotdf, aes(x, y, color=predicted, fill=predicted)) +
  geom_point(size=5) +
  geom_polygon(data=boundary, aes(x,y), alpha=0.5) +
  scale_x_continuous("self_perception_weight") +
  scale_y_continuous("life_reward")

# ----- Sub datasets -----
## ONCE: install.packages("ggplot2")
library("ggplot2")
## ONCE: install.packages("plyr")
library("plyr")

### exercise ###
#structure of predicted variable
str(exercise.num$healthy_feeling)
# create test and train dataset
test <- exercise.num[s,]
train <- exercise.num[-s,]
str(train)
# remove healthy_feeling from test and train
train_label <- train[,(which(colnames(train) == "healthy_feeling"))]
train <- train[,-(which(colnames(train) == "healthy_feeling"))]
test_label <- test[,which(colnames(test) == "healthy_feeling")]
test <- test[,-(which(colnames(test) == "healthy_feeling"))]
# setup model
kNN_exercise <- class::knn(train=train,
                           test=test,
                           cl=train_label,
                           k=k,
                           prob=T)
print(kNN_exercise)
#results
table(kNN_exercise, test_label)
CrossTable(x=test_label,y=kNN_exercise, prop.chisq=F)
#visualize
plotdf <- data.frame(test,
                     predicted=kNN_exercise)
plotdf <- data.frame(x=plotdf$weight,
                     y=plotdf$exercise,
                     predicted=plotdf$predicted)
find_hull <- function(df) df[chull(df$x, df$y),]
boundary <- ddply(plotdf, .variables="predicted", .fun=find_hull)
ggplot(plotdf, aes(x, y, color=predicted, fill=predicted)) +
  geom_point(size=5) +
  geom_polygon(data=boundary, aes(x,y), alpha=0.5) +
  scale_x_continuous("weight") +
  scale_y_continuous("exercise")


### Upbringing dataset ###
#structure of predicted variable
str(upbringing.num$healthy_feeling)
# create test and train dataset
test <- upbringing.num[s,]
train <- upbringing.num[-s,]
str(train)
# remove healthy_feeling from test and train
train_label <- train[,(which(colnames(train) == "healthy_feeling"))]
train <- train[,-(which(colnames(train) == "healthy_feeling"))]
test_label <- test[,which(colnames(test) == "healthy_feeling")]
test <- test[,-(which(colnames(test) == "healthy_feeling"))]
# setup model
kNN_upbringing <- class::knn(train=train,
                           test=test,
                           cl=train_label,
                           k=k,
                           prob=T)
print(kNN_upbringing)
#results
table(kNN_upbringing, test_label)
CrossTable(x=test_label,y=kNN_upbringing, prop.chisq=F)
#visualize
plotdf <- data.frame(test,
                     predicted=kNN_upbringing)
plotdf <- data.frame(x=plotdf$income,
                     y=plotdf$parent_cook,
                     predicted=plotdf$predicted)
find_hull <- function(df) df[chull(df$x, df$y),]
boundary <- ddply(plotdf, .variables="predicted", .fun=find_hull)
ggplot(plotdf, aes(x, y, color=predicted, fill=predicted)) +
  geom_point(size=5) +
  geom_polygon(data=boundary, aes(x,y), alpha=0.5) +
  scale_x_continuous("income") +
  scale_y_continuous("parent_cook")


### Food intake ###
#structure of predicted variable
str(food.intake.num$healthy_feeling)
# create test and train dataset
test <- food.intake.num[s,]
train <- food.intake.num[-s,]
str(train)
# remove healthy_feeling from test and train
train_label <- train[,(which(colnames(train) == "healthy_feeling"))]
train <- train[,-(which(colnames(train) == "healthy_feeling"))]
test_label <- test[,which(colnames(test) == "healthy_feeling")]
test <- test[,-(which(colnames(test) == "healthy_feeling"))]
# setup model
kNN_intake <- class::knn(train=train,
                           test=test,
                           cl=train_label,
                           k=k,
                           prob=T)
print(kNN_intake)
#results
table(kNN_intake, test_label)
CrossTable(x=test_label,y=kNN_intake, prop.chisq=F)
#visualize
plotdf <- data.frame(test,
                     predicted=kNN_intake)
plotdf <- data.frame(x=plotdf$comfort_food_reasons,
                     y=plotdf$fav_cuisine,
                     predicted=plotdf$predicted)
find_hull <- function(df) df[chull(df$x, df$y),]
boundary <- ddply(plotdf, .variables="predicted", .fun=find_hull)
ggplot(plotdf, aes(x, y, color=predicted, fill=predicted)) +
  geom_point(size=5) +
  geom_polygon(data=boundary, aes(x,y), alpha=0.5) +
  scale_x_continuous("comfort_food_reasons") +
  scale_y_continuous("fav_cuisine")


### Food Assocation ###
#structure of predicted variable
str(food.asso.num$healthy_feeling)
# create test and train dataset
test <- food.asso.num[s,]
train <- food.asso.num[-s,]
str(train)
# remove healthy_feeling from test and train
train_label <- train[,(which(colnames(train) == "healthy_feeling"))]
train <- train[,-(which(colnames(train) == "healthy_feeling"))]
test_label <- test[,which(colnames(test) == "healthy_feeling")]
test <- test[,-(which(colnames(test) == "healthy_feeling"))]
# setup model
kNN_asso <- class::knn(train=train,
                           test=test,
                           cl=train_label,
                           k=k,
                           prob=T)
print(kNN_asso)
#results
table(kNN_asso, test_label)
CrossTable(x=test_label,y=kNN_asso, prop.chisq=F)
#visualize
plotdf <- data.frame(test,
                     predicted=kNN_asso)
plotdf <- data.frame(x=plotdf$coffee,
                     y=plotdf$drink,
                     predicted=plotdf$predicted)
find_hull <- function(df) df[chull(df$x, df$y),]
boundary <- ddply(plotdf, .variables="predicted", .fun=find_hull)
ggplot(plotdf, aes(x, y, color=predicted, fill=predicted)) +
  geom_point(size=5) +
  geom_polygon(data=boundary, aes(x,y), alpha=0.5) +
  scale_x_continuous("coffee") +
  scale_y_continuous("drink")


### Health ###
#structure of predicted variable
str(health.num$healthy_feeling)
# create test and train dataset
test <- health.num[s,]
train <- health.num[-s,]
str(train)
# remove healthy_feeling from test and train
train_label <- train[,(which(colnames(train) == "healthy_feeling"))]
train <- train[,-(which(colnames(train) == "healthy_feeling"))]
test_label <- test[,which(colnames(test) == "healthy_feeling")]
test <- test[,-(which(colnames(test) == "healthy_feeling"))]
# setup model
kNN_health <- class::knn(train=train,
                           test=test,
                           cl=train_label,
                           k=k,
                           prob=T)
print(kNN_health)
#results
table(kNN_health, test_label)
CrossTable(x=test_label,y=kNN_health, prop.chisq=F)
#visualize
plotdf <- data.frame(test,
                     predicted=kNN_health)
plotdf <- data.frame(x=plotdf$ideal_diet,
                     y=plotdf$eating_changes1,
                     predicted=plotdf$predicted)
find_hull <- function(df) df[chull(df$x, df$y),]
boundary <- ddply(plotdf, .variables="predicted", .fun=find_hull)
ggplot(plotdf, aes(x, y, color=predicted, fill=predicted)) +
  geom_point(size=5) +
  geom_polygon(data=boundary, aes(x,y), alpha=0.5) +
  scale_x_continuous("ideal_diet") +
  scale_y_continuous("eating_changes")


### status ###
#structure of predicted variable
str(status.num$healthy_feeling)
# create test and train dataset
test <- status.num[s,]
train <- status.num[-s,]
str(train)
# remove healthy_feeling from test and train
train_label <- train[,(which(colnames(train) == "healthy_feeling"))]
train <- train[,-(which(colnames(train) == "healthy_feeling"))]
test_label <- test[,which(colnames(test) == "healthy_feeling")]
test <- test[,-(which(colnames(test) == "healthy_feeling"))]
# setup model
kNN_status <- class::knn(train=train,
                           test=test,
                           cl=train_label,
                           k=k,
                           prob=T)
print(kNN_status)
#results
table(kNN_status, test_label)
CrossTable(x=test_label,y=kNN_status, prop.chisq=F)
#visualize
plotdf <- data.frame(test,
                     predicted=kNN_status)
plotdf <- data.frame(x=plotdf$GPA,
                     y=plotdf$life_rewarding,
                     predicted=plotdf$predicted)
find_hull <- function(df) df[chull(df$x, df$y),]
boundary <- ddply(plotdf, .variables="predicted", .fun=find_hull)
ggplot(plotdf, aes(x, y, color=predicted, fill=predicted)) +
  geom_point(size=5) +
  geom_polygon(data=boundary, aes(x,y), alpha=0.5) +
  scale_x_continuous("GPA") +
  scale_y_continuous("life_rewarding")

