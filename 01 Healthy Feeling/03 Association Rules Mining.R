# Christina Ling
# IST 565 Data Mining
# final project | association rules mining



# ----- Libraries ------
## ONCE: install.packages("arules")
library("arules")
## ONCE: install.packages("dplyr")
library("dplyr")
## ONCE: install.packages("mclust")
library("mclust")
## ONCE:: install.packages("arulesViz")
library("arulesViz")


# ----- data preprocessing -----

#ensure everything is factor
str(fc.discrete)
unique(fc.discrete$healthy_feeling)



# ----- assoicationg rule mining method ----- 
rule1 <- apriori(fc.discrete,
                 parameter = list(supp=0.25, conf=0.97))
rule1 <- sort(rule1, by="confidence", decreasing=TRUE)
inspect(rule1[1:20])
plot(rule1, measure=c("confidence", "lift"), shading="support") #431 rules


### RHS ###

#healthy
healthy.rule <- apriori(fc.discrete,
                         parameter = list(supp=0.07, conf=0.7),
                         appearance = list(default="lhs", rhs="healthy_feeling=healthy"),
                         control=list(verbose=FALSE))
healthy.rule <- sort(healthy.rule, by="confidence", decreasing=TRUE)
inspect(healthy.rule[1:20])
plot(healthy.rule, measure=c("confidence", "lift"), shading="support") #81 rules

#average
average.rule <- apriori(fc.discrete,
                        parameter = list(supp=0.1, conf=0.7),
                        appearance = list(default="lhs", rhs="healthy_feeling=average"),
                        control=list(verbose=FALSE))
average.rule <- sort(average.rule, by="confidence", decreasing=TRUE)
inspect(average.rule[1:20])
plot(average.rule, measure=c("confidence", "lift"), shading="support") #86 rules

#unhealthy
unhealthy.rule <- apriori(fc.discrete,
                        parameter = list(supp=0.1, conf=0.7),
                        appearance = list(default="lhs", rhs="healthy_feeling=unhealthy"),
                        control=list(verbose=FALSE))
unhealthy.rule <- sort(unhealthy.rule, by="confidence", decreasing=TRUE)
inspect(unhealthy.rule[1:20])
plot(unhealthy.rule, measure=c("confidence", "lift"), shading="support") #23 rules


# ----- sub datasets -----

#RHS

### exercise ###
#preprocess
str(exercise)
exercise$weight <- cut(exercise$weight,
                          breaks=c(-Inf,135,155,180,Inf),
                          labels=c("light", "average","heavy", "very heavy"))
str(exercise)
#setup
exercise.rule <- apriori(exercise,
                         parameter= list(supp=0.2, conf=0.7),
                         appearance = list(default="lhs", rhs="healthy_feeling=healthy"),
                         control=list(verbose=F))
exercise.rule <- sort(exercise.rule, by="confidence", decreasing= T)
inspect(exercise.rule)


### upbringing ###
str(upbringing)
#setup
upbringing.rule <- apriori(upbringing,
                         parameter= list(supp=0.2, conf=0.7),
                         appearance = list(default="lhs", rhs="healthy_feeling=healthy"),
                         control=list(verbose=F))
upbringing.rule <- sort(upbringing.rule, by="confidence", decreasing= T)
inspect(upbringing.rule)


### food intake ###
str(food.intake)
#setup
intake.rule <- apriori(food.intake,
                         parameter= list(supp=0.2, conf=0.7),
                         appearance = list(default="lhs", rhs="healthy_feeling=healthy"),
                         control=list(verbose=F))
intake.rule <- sort(intake.rule, by="confidence", decreasing= T)
inspect(intake.rule)


### food association ###
str(food.asso)
#set up
asso.rule <- apriori(food.asso,
                         parameter= list(supp=0.2, conf=0.7),
                         appearance = list(default="lhs", rhs="healthy_feeling=healthy"),
                         control=list(verbose=F))
asso.rule <- sort(asso.rule, by="confidence", decreasing= T)
inspect(asso.rule)


### health ###
str(health)
#setup
health.rule <- apriori(health,
                         parameter= list(supp=0.2, conf=0.7),
                         appearance = list(default="lhs", rhs="healthy_feeling=healthy"),
                         control=list(verbose=F))
health.rule <- sort(health.rule, by="confidence", decreasing= T)
inspect(health.rule)

### status ###
str(status)
status$GPA <- cut(status$GPA,
                       breaks=c(-Inf, 3.2, 3.5, 3.7, Inf),
                       labels=c("very bad", "bad", "good", "very good"))
str(status)
#setup
status.rule <- apriori(status,
                         parameter= list(supp=0.2, conf=0.7),
                         appearance = list(default="lhs", rhs="healthy_feeling=healthy"),
                         control=list(verbose=F))
status.rule <- sort(status.rule, by="confidence", decreasing= T)
inspect(status.rule)


# ---- Exploring results -----
summary(food.choices$on_off_campus)
summary(food.choices$mother_education)
summary(food.choices$breakfast)
summary(food.choices$cuisine)
summary(food.choices$fries)
