myPCA = function (df) {
  eig = eigen(cov(df))
  means = unlist(lapply(df,mean))
  scores = scale(df, center = means) %*% eig$vectors
  list(values = eig$values,vectors = eig$vectors, scores = scores)
}

my_pca = myPCA(iris[1:4])
my_pca

my_pca[[1]] / sum(my_pca[[1]])

pca = princomp(iris[1:4], scores = T)
cbind(unlist(lapply(my_pca[1], sqrt)), pca$sdev)

summary(pca)

my_pca[[1]] / sum(my_pca[[1]])

scores = cbind(matrix(unlist(my_pca[3]),ncol = 4), pca$scores)
round(cor(scores)[1:4,5:8],3)

library(psych)
data(msq)
motiv = msq[,1:72]
