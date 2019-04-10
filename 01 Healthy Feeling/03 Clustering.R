# Christina Ling
# IST 565 Data Mining
# final project | clustering



# ----- Libraries ------
#libraries
## ONCE: install.packages("cluster")
library("cluster")
## ONCE:install.packages("factoextra")
library("factoextra")
# ONCE: install.packages("mclust")
library("mclust")

# ----- preprocess -----

#remove healthy label
food.choices.nolab <- food.choices.numbonly[,-which(colnames(food.choices.numbonly) == "healthy_feeling")]
exercise.nolab <- exercise.num[,-(which(colnames(exercise.num) == "healthy_feeling"))]
upbringing.nolab <- upbringing.num[,-(which(colnames(upbringing.num) == "healthy_feeling"))]
food.intake.nolab <- food.intake.num[,-(which(colnames(food.intake.num) == "healthy_feeling"))]
food.asso.nolab <- food.asso.num[,-(which(colnames(food.asso.num) == "healthy_feeling"))]
health.nolab <- health.num[,-(which(colnames(health.num) == "healthy_feeling"))]
status.nolab <- status.num[,-(which(colnames(status.num) == "healthy_feeling"))]

### ----- clustering ----- 
# ----- k Mean -----

### using all data
#k means, cluster 3, (low, average, high)
kmeans <- kmeans(food.choices.nolab, 3)
print(kmeans)

#results
table(food.choices.numbonly$healthy_feeling, kmeans$cluster) 
#bad results

#visual
fviz_cluster(kmeans, data=food.choices.nolab,
             palette= c("#2E9FDF", "#E7B800", "#92DE6A"),
             ellipse.type= "euclid",
             star.plot=TRUE,
             repel= TRUE,
             ggtheme = theme_minimal())



### Exercise data
# 3 clusters
kmean.exercise <- kmeans(exercise.nolab, 3)
print(kmean.exercise)
#table results
table(exercise.num$healthy_feeling, kmean.exercise$cluster)
#bad results
#visual
fviz_cluster(kmean.exercise, data=exercise.nolab,
             palette= c("#2E9FDF", "#E7B800", "#92DE6A"),
             ellipse.type= "euclid",
             star.plot=TRUE,
             repel= TRUE,
             ggtheme = theme_minimal())


### upbringing dataset
# 3 clusters
kmean.upbringing <- kmeans(upbringing.nolab, 3)
print(kmean.upbringing)
#table results
table(upbringing.num$healthy_feeling, kmean.upbringing$cluster)
#descent results
#visual
fviz_cluster(kmean.upbringing, data=upbringing.nolab,
             palette= c("#2E9FDF", "#E7B800", "#92DE6A"),
             ellipse.type= "euclid",
             star.plot=TRUE,
             repel= TRUE,
             ggtheme = theme_minimal())

### food intake dataset
# 3 clusters
kmean.intake <- kmeans(food.intake.nolab, 3)
print(kmean.intake)
#table results
table(food.intake.num$healthy_feeling, kmean.intake$cluster)
#descent results
#visual
fviz_cluster(kmean.intake, data=food.intake.nolab,
             palette= c("#2E9FDF", "#E7B800", "#92DE6A"),
             ellipse.type= "euclid",
             star.plot=TRUE,
             repel= TRUE,
             ggtheme = theme_minimal())

### food association dataset
# 3 clusters
kmean.asso <- kmeans(food.asso.nolab, 3)
print(kmean.asso)
#table results
table(food.asso.num$healthy_feeling, kmean.asso$cluster)
#bad results
#visual
fviz_cluster(kmean.asso, data=food.asso.nolab,
             palette= c("#2E9FDF", "#E7B800", "#92DE6A"),
             ellipse.type= "euclid",
             star.plot=TRUE,
             repel= TRUE,
             ggtheme = theme_minimal())

### health dataset
# 3 clusters
kmean.health <- kmeans(health.nolab, 3)
print(kmean.health)
#table results
table(health.num$healthy_feeling, kmean.health$cluster)
#descent result
#visual
fviz_cluster(kmean.health, data=health.nolab,
             palette= c("#2E9FDF", "#E7B800", "#92DE6A"),
             ellipse.type= "euclid",
             star.plot=TRUE,
             repel= TRUE,
             ggtheme = theme_minimal())

### status
# 3 clusters
kmean.status <- kmeans(status.nolab, 3)
print(kmean.status)
#table results
table(status.num$healthy_feeling, kmean.status$cluster)
#descent results
#visual
fviz_cluster(kmean.status, data=status.nolab,
             palette= c("#2E9FDF", "#E7B800", "#92DE6A"),
             ellipse.type= "euclid",
             star.plot=TRUE,
             repel= TRUE,
             ggtheme = theme_minimal())




# ----- EM Cluster ------

### all data
head(food.choices.nolab)

### number of cluster as 3
clust_EM <- Mclust(food.choices.nolab,G=3)
# look at results
(clust_EM)
summary(clust_EM)
#table results
table(food.choices.numbonly$healthy_feeling,clust_EM$classification)
# Bad results

#not setting G
clust_EM2 <- Mclust(food.choices.nolab)
(clust_EM2)
summary(clust_EM2)
#table results
table(food.choices$healthy_feeling,clust_EM2$classification)
#made 1 cluster
#horrible results

### exercise data
clust_EM_exercise <- Mclust(exercise.nolab, G=3)
#look at results
(clust_EM_exercise)
summary(clust_EM_exercise)
table(exercise.num$healthy_feeling, clust_EM_exercise$classification)
#bad results
#visualize
plot(clust_EM_exercise, what="classification")

### upbringing data
clust_EM_upbringing <- Mclust(upbringing.nolab, G=3)
#look at results
(clust_EM_upbringing)
summary(clust_EM_upbringing)
table(upbringing.num$healthy_feeling, clust_EM_upbringing$classification)
#visual
plot(clust_EM_upbringing, what="classification")

### food intake
clust_EM_intake <- Mclust(food.intake.nolab, G=3)
#look at results
(clust_EM_intake)
summary(clust_EM_intake)
table(food.intake.num$healthy_feeling, clust_EM_intake$classification)
#visual
plot(clust_EM_intake, what="classification")

### food association
clust_EM_asso <- Mclust(food.asso.nolab, G=3)
#look at results
(clust_EM_asso)
summary(clust_EM_asso)
table(food.asso.num$healthy_feeling, clust_EM_asso$classification)
#visual
plot(clust_EM_asso, what="classification")

### health dataset
clust_EM_health <- Mclust(health.nolab, G=3)
#look at results
(clust_EM_health)
summary(clust_EM_health)
table(health.num$healthy_feeling, clust_EM_health$classification)
#visual
plot(clust_EM_health, what="classification")

### status dataset
clust_EM_status <- Mclust(status.nolab, G=3)
#look at results
(clust_EM_status)
summary(clust_EM_status)
table(status.num$healthy_feeling, clust_EM_status$classification)
#visual
plot(clust_EM_status, what="classification")




# ----- HAC Cluster ------

# all data
#Euclidean
all.e <- dist(food.choices.nolab, method="euclidean")
fit.all.e <- hclust(all.e, method="ward.D2")
plot(fit.all.e,
     labels=food.choices.numbonly$healthy_feeling)
rect.hclust(fit.all.e, k=3)

#Manhattan
all.m <- dist(food.choices.nolab, method="manhattan")
fit.all.m <- hclust(all.m, method="ward.D2")
plot(fit.all.m,
     labels=food.choices.numbonly$healthy_feeling)
rect.hclust(fit.all.m, k=3)

### exercise
#Euclidean
exercise.e <- dist(exercise.nolab, method="euclidean")
fit.exercise.e <- hclust(exercise.e, method="ward.D2")
plot(fit.exercise.e,
     labels=exercise.num$healthy_feeling)
rect.hclust(fit.exercise.e, k=3)

#Manhattan
exercise.m <- dist(exercise.nolab, method="manhattan")
fit.exercise.m <- hclust(exercise.m, method="ward.D2")
plot(fit.exercise.m,
     labels=exercise.num$healthy_feeling)
rect.hclust(fit.exercise.m, k=3)

### Upbringing dataset
#Euclidean
upbringing.e <- dist(upbringing.nolab, method="euclidean")
fit.upbringing.e <- hclust(upbringing.e, method="ward.D2")
plot(fit.upbringing.e,
     labels=upbringing.num$healthy_feeling)
rect.hclust(fit.upbringing.e, k=3)

#Manhattan
upbringing.m <- dist(upbringing.nolab, method="manhattan")
fit.upbringing.m <- hclust(upbringing.m, method="ward.D2")
plot(fit.upbringing.m,
     labels=upbringing.num$healthy_feeling)
rect.hclust(fit.upbringing.m, k=3)

### food intake dataset
#Euclidean
intake.e <- dist(food.intake.nolab, method="euclidean")
fit.intake.e <- hclust(intake.e, method="ward.D2")
plot(fit.intake.e,
     labels=food.intake.num$healthy_feeling)
rect.hclust(fit.intake.e, k=3)

#Manhattan
intake.m <- dist(food.intake.nolab, method="manhattan")
fit.intake.m <- hclust(intake.m, method="ward.D2")
plot(fit.intake.m,
     labels=food.intake.num$healthy_feeling)
rect.hclust(fit.intake.m, k=3)

### Food Association Dataset
#Euclidean
asso.e <- dist(food.asso.nolab, method="euclidean")
fit.asso.e <- hclust(asso.e, method="ward.D2")
plot(fit.asso.e,
     labels=food.asso.num$healthy_feeling)
rect.hclust(fit.asso.e, k=3)

#Manhattan
asso.m <- dist(food.asso.nolab, method="manhattan")
fit.asso.m <- hclust(asso.m, method="ward.D2")
plot(fit.asso.m,
     labels=food.asso.num$healthy_feeling)
rect.hclust(fit.asso.m, k=3)

### Health Dataset
#Euclidean
health.e <- dist(health.nolab, method="euclidean")
fit.health.e <- hclust(health.e, method="ward.D2")
plot(fit.health.e,
     labels=health.num$healthy_feeling)
rect.hclust(fit.health.e, k=3)

#Manhattan
health.m <- dist(health.nolab, method="manhattan")
fit.health.m <- hclust(health.m, method="ward.D2")
plot(fit.health.m,
     labels=health.num$healthy_feeling)
rect.hclust(fit.health.m, k=3)

### Status Dataset
#Euclidean
status.e <- dist(status.nolab, method="euclidean")
fit.status.e <- hclust(status.e, method="ward.D2")
plot(fit.status.e,
     labels=status.num$healthy_feeling)
rect.hclust(fit.status.e, k=3)

#Manhattan
status.m <- dist(status.nolab, method="manhattan")
fit.status.m <- hclust(status.m, method="ward.D2")
plot(fit.status.m,
     labels=status.num$healthy_feeling)
rect.hclust(fit.status.m, k=3)


# ----- Cosine Similarity -----
## ONCE: install.packages("coop")
library("coop")
## ONCE: install.packages("igraph")
library("igraph")

# all data
all.matrix <- as.matrix(food.choices.nolab)
cos_sim.all.matrix <- cosine(all.matrix)
diag(cos_sim.all.matrix) <- 0
head(cos_sim.all.matrix)
#prune edges of the tree
edgeLimit <- 0.75
cos_sim.all.matrix[(cos_sim.all.matrix < edgeLimit)] <- 0
#make network
cos_sim.networkall <- graph_from_adjacency_matrix(cos_sim.all.matrix,
                                                 mode="undirected",
                                                 weight=T)
plot(cos_sim.networkall)

### Exercise dataset
exercise.matrix <- as.matrix(exercise.nolab)
cos_sim.exercise.matrix <- cosine(exercise.matrix)
diag(cos_sim.exercise.matrix) <- 0
head(cos_sim.exercise.matrix)
#prune edges of the tree
cos_sim.exercise.matrix[(cos_sim.exercise.matrix < edgeLimit)] <- 0
#make network
cos_sim.networkexercise <- graph_from_adjacency_matrix(cos_sim.exercise.matrix,
                                                  mode="undirected",
                                                  weight=T)
plot(cos_sim.networkexercise)

### Upbringing dataset
upbringing.matrix <- as.matrix(upbringing.nolab)
cos_sim.upbringing.matrix <- cosine(upbringing.matrix)
diag(cos_sim.upbringing.matrix) <- 0
head(cos_sim.upbringing.matrix)
#prune edges of the tree
cos_sim.upbringing.matrix[(cos_sim.upbringing.matrix < edgeLimit)] <- 0
#make network
cos_sim.networkupbringing <- graph_from_adjacency_matrix(cos_sim.upbringing.matrix,
                                                       mode="undirected",
                                                       weight=T)
plot(cos_sim.networkupbringing)

### food intake dataset
intake.matrix <- as.matrix(food.intake.nolab)
cos_sim.intake.matrix <- cosine(intake.matrix)
diag(cos_sim.intake.matrix) <- 0
head(cos_sim.intake.matrix)
#prune edges of the tree
cos_sim.intake.matrix[(cos_sim.intake.matrix < edgeLimit)] <- 0
#make network
cos_sim.networkintake <- graph_from_adjacency_matrix(cos_sim.intake.matrix,
                                                       mode="undirected",
                                                       weight=T)
plot(cos_sim.networkintake)


### food assocation dataset
asso.matrix <- as.matrix(food.asso.nolab)
cos_sim.asso.matrix <- cosine(asso.matrix)
diag(cos_sim.asso.matrix) <- 0
head(cos_sim.asso.matrix)
#prune edges of the tree
cos_sim.asso.matrix[(cos_sim.exercise.matrix < edgeLimit)] <- 0
#make network
cos_sim.networkasso <- graph_from_adjacency_matrix(cos_sim.asso.matrix,
                                                       mode="undirected",
                                                       weight=T)
plot(cos_sim.networkasso)

### health dataset
health.matrix <- as.matrix(health.nolab)
cos_sim.health.matrix <- cosine(health.matrix)
diag(cos_sim.health.matrix) <- 0
head(cos_sim.health.matrix)
#prune edges of the tree
cos_sim.health.matrix[(cos_sim.health.matrix < edgeLimit)] <- 0
#make network
cos_sim.networkhealth <- graph_from_adjacency_matrix(cos_sim.health.matrix,
                                                       mode="undirected",
                                                       weight=T)
plot(cos_sim.networkhealth)

### status dataset
status.matrix <- as.matrix(status.nolab)
cos_sim.status.matrix <- cosine(status.matrix)
diag(cos_sim.status.matrix) <- 0
head(cos_sim.status.matrix)
#prune edges of the tree
cos_sim.status.matrix[(cos_sim.status.matrix < edgeLimit)] <- 0
#make network
cos_sim.networkstatus <- graph_from_adjacency_matrix(cos_sim.status.matrix,
                                                       mode="undirected",
                                                       weight=T)
plot(cos_sim.networkstatus)
