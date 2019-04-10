# christina ling
# final project | data exploration
# IST 565 Data Mining

# libraries
## ONCE: install.packages("ggplot2")
library("ggplot2")
## ONCE: install.packages("ggpubr")
library("ggpubr")
## ONCE: install.packages("corrplot")
library("corrplot")
## ONCE: install.package("ggcorrplot")
library("ggcorrplot")
## ONCE: install.packages("psych")
library("psych")


# ----- summary ----

summary(food.choices)
summary(exercise.num)
summary(upbringing.num)
summary(food.intake.num)
summary(food.asso.num)
summary(health.num)
summary(status.num)

# ----- tables -----
  #gender and healthy feeling
table(food.choices$healthy_feeling, food.choices$Gender)

  #calories day and healthy feeling
table(food.choices$healthy_feeling, food.choices$calories_day)

  #cooking and healthy feeling
table(food.choices$healthy_feeling, food.choices$cook)

  #cuisine and healthy feeling
table(food.choices$healthy_feeling, food.choices$cuisine)

  #eating changes and healthy feeling
table(food.choices$healthy_feeling, food.choices$eating_changes_coded)

  #perception of weight and exercise
table(food.choices$self_perception_weight, food.choices$exercise)

unique(food.choices$exercise)
# ----- pair panels ------

pairs.panels(exercise.num)
pairs.panels(upbringing.num)
pairs.panels(food.intake.num)
pairs.panels(food.asso.num)
pairs.panels(health.num)
pairs.panels(status.num)

# ------ correlations ------

#exercise
str(exercise.num)
exercise.num <- exercise.num[,-(which(colnames(exercise.num) == "healthy_feeling"))]
exercise.corr <- round(cor(exercise.num, use="complete.obs"), 2)
ggcorrplot(exercise.corr,
           type="lower",
           hc.order=T,
           lab=T,
           lab_size=3,
           method="circle",
           colors=c("tomato2", "white", "springgreen3"),
           title="Correlation in Exercise",
           ggtheme=theme_bw)

#upbringing
str(upbringing.num)
upbringing.num <- upbringing.num[,-(which(colnames(upbringing.num) == "healthy_feeling"))]
upbringing.corr <- round(cor(upbringing.num, use="complete.obs"), 2)
ggcorrplot(upbringing.corr,
           type="lower",
           hc.order=T,
           lab=T,
           lab_size=3,
           method="circle",
           colors=c("tomato2", "white", "springgreen3"),
           title="Correlation in Upbringing",
           ggtheme=theme_bw)

#food intake
str(food.intake.num)
food.intake.num <- food.intake.num[,-(which(colnames(food.intake.num) == "healthy_feeling"))]
intake.corr <- round(cor(food.intake.num, use="complete.obs"), 2)
ggcorrplot(intake.corr,
           type="lower",
           hc.order=T,
           lab=T,
           lab_size=3,
           method="circle",
           colors=c("tomato2", "white", "springgreen3"),
           title="Correlation in Food Intake",
           ggtheme=theme_bw)

#food association
str(food.asso.num)
food.asso.num <- food.asso.num[,-(which(colnames(food.asso.num) == "healthy_feeling"))]
association.corr <- round(cor(food.asso.num, use="complete.obs"), 2)
ggcorrplot(association.corr,
           type="lower",
           hc.order=T,
           lab=T,
           lab_size=3,
           method="circle",
           colors=c("tomato2", "white", "springgreen3"),
           title="Correlation in Food Association",
           ggtheme=theme_bw)

# health
str(health.num)
health.num <- health.num[,-(which(colnames(health.num) == "healthy_feeling"))]
health.corr <- round(cor(health.num, use="complete.obs"), 2)
ggcorrplot(health.corr,
           type="lower",
           hc.order=T,
           lab=T,
           lab_size=3,
           method="circle",
           colors=c("tomato2", "white", "springgreen3"),
           title="Correlation in Health Habits",
           ggtheme=theme_bw)

#status
str(status.num)
status.num <- status.num[,-(which(colnames(status.num) == "healthy_feeling"))]
status.corr <- round(cor(status.num, use="complete.obs"), 2)
ggcorrplot(status.corr,
           type="lower",
           hc.order=T,
           lab=T,
           lab_size=3,
           method="circle",
           colors=c("tomato2", "white", "springgreen3"),
           title="Correlation in Status",
           ggtheme=theme_bw)

# ----- visuals -----

#GPA and healthy feeling
ggballoonplot(status, 
              x="healthy_feeling", 
              y="GPA", 
              size="GPA",
              fill="GPA",
              facet.by="gender",
              ggtheme = theme_bw())

  #weight and healthy feeling
ggplot(food.choices, aes(x=healthy_feeling, y=weight)) +
  geom_point(aes(color=self_perception_weight, size=self_perception_weight))

  #activities and healthy feeling
food.choices$healthy_feeling <- as.numeric(food.choices$healthy_feeling)

ggplot(food.choices, aes(exercise, healthy_feeling)) +
  geom_boxplot(varwidth = T, fill ="plum") +
  labs(x="Weekly Exercise",
       y="Healthy Feeling")


df <- data.frame(healthy_feeling=hf.discrete,
                 weight=food.choices.coded$weight,
                 self_perception_weight=food.choices$self_perception_weight)

ggballoonplot(df,
              x="healthy_feeling",
              y="weight",
              size="weight",
              fill="weight",
              facet.by="self_perception_weight",
              ggtheme= theme_bw())
