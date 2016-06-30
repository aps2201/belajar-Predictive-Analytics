rownames(life.scaled) = life$country

par(mfrow=c(1,2))

a=hclust(dist(life.scaled))
plot(a, hang=-1, xlab="Case number", main = "Euclidean")
a = hclust(dist(life.scaled, method= "manhattan"))
plot(a, hang=-1, xlab="Case number", main = "Manhattan")

# how it works ----
A1 = c(2,3,5,7,8,10,20,21,23)
A2 = A1
A3 = A1
library(scatterplot3d)
scatterplot3d(A1,A2,A3, angle = 45, type = "h")
demo = hclust(dist(cbind(A1,A2,A3)))
plot(demo)

#swiss data ----
swiss = read.table("swiss_votes.dat",header = T)
dist_matrix = as.matrix((swiss[-1]))
row.names(dist_matrix) = swiss$Canton
clust_comp = hclust(dist(dist_matrix))
clust_single = hclust(dist(dist_matrix),method = "single")
clust_average = hclust(dist(dist_matrix),method = "average")
par(mfrow = c(1,3))
plot(clust_comp, hang = -1,
     main = "Complete linkage", xlab = "Canton",
     ylab = "Distance")

plot(clust_single, hang = -1,
     main = "Single linkage", xlab = "Canton",
     ylab = "Distance")

plot(clust_average, hang = -1,
     main = "Average linkage", xlab = "Canton",
     ylab = "Distance")

par(mfrow = c(1,1))

plot(swiss$Protection,swiss$Taxes2,
     pch=15, col=gray.colors(6),
     xlab="Protection and support services for peace",
     ylab = "Tax on non-renewable energy")

# Trucks ----
library(vcd)
data(Trucks)
head(Trucks)
Trucks.wd<- Trucks[rep(1:nrow(Trucks),Trucks$Freq),]
Trucks.rm = Trucks.wd[, -(c(1,5))]
set.seed(456)
Trucks.sample = Trucks.rm[sample(nrow(Trucks.rm), 100), ]
Trucks.onoff = data.frame(matrix(nrow =
                                   nrow(Trucks.sample), ncol = ncol (Trucks.sample)))
for (i in 1:nrow(Trucks.sample)) {
  for (j in 1:ncol(Trucks.sample)) {
    if (Trucks.sample[i,j] != Trucks.sample[1,j])
      Trucks.onoff[i,j] = 0
    else Trucks.onoff[i,j] = 1
  }
}
names(Trucks.onoff)=names(Trucks.sample)
b = hclust(dist(Trucks.onoff, method= "binary"))
plot(b)

# exercise ----
library(dplyr)
avg.iris = group_by(iris,Species)
avg.iris = as.data.frame(summarize(avg.iris,Sepal.Length=mean(Sepal.Length),Sepal.Width=mean(Sepal.Width),Petal.Length=mean(Petal.Length),Petal.Width=mean(Petal.Width)))
iris.dist = as.matrix(avg.iris[2:5])
rownames(iris.dist) = avg.iris$Species
plot(hclust(dist(iris.dist),method = "average"))




