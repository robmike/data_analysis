library(datasets)
data(iris)

x <- iris[, 1:4];
d <- dist(x)
hc <- hclust(d)

load('quiz3question4.rda')
k <- kmeans(dataSet, centers=2)
plot(dataSet$x, dataSet$y, col=k$cluster, pch=19, cex=2)
points(k$centers,col=1:3,pch=3,cex=3,lwd=3)


library(ElemStatLearn)
data(zip.train)

im = zip2image(zip.train,3)
image(im)

im8 = zip2image(zip.train,8)
im18 = zip2image(zip.train,18)

## percent variance accounted by first singular vector
pervar1 <- function(x) {
  s <- (svd(x)$d)^2
  s[1]/sum(s)
}

pervar1(im8)
pervar1(im18)

image(im18)
