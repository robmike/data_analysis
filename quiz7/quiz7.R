
set.seed(53535)
xValues = seq(0,2*pi,length=100)
yValues = rnorm(100) + sin(xValues)

library(splines)
library(hydroGOF)


err <- vector('numeric', 10)
for (i in 1:10) {
  ns1 <- ns(xValues, df=i);
  z <- lm(yValues ~ ns1)
  err[i] <- rmse(z$fitted, yValues)
}
plot(1:10, err)

## Must restart R here to prevent variable masking
library(simpleboot) 
data(airquality)
attach(airquality)    # pull all airquality db variables into environment

quantile(Wind, 0.75)

set.seed(883833)
q75func <- function (x,i) {quantile(x[i], 0.75)}
zz <- boot(Wind, q75func, 1000)
zz

## q4
library(tree)
data(Cars93,package='MASS')

treepredict <- function(x, i) {
  vote <- vector("integer", 3)
  for (i in 1:3) {
  tr <- tree(DriveTrain ~ Price + Type, data=x[i,])
  newdata = data.frame(Type = "Large",Price = 20)
  p <- predict(tr, newdata)
  p
}
}

set.seed(7363)
newdata = data.frame(Type = "Large",Price = 20)
for(j in 1:3) {
  s <- sample(1:nrow(Cars93),replace=TRUE)
  tr <- tree(DriveTrain ~ Price + Type, data=Cars93[s,])
  p <- predict(tr, newdata)
  print(p)
}


library(ElemStatLearn)
library(e1071)
data(vowel.train)
data(vowel.test)
vowel.test$y = as.factor(vowel.test$y)
vowel.train$y = as.factor(vowel.train$y)

library(randomForest)

set.seed(33833, kind = "L'Ecuyer-CMRG", normal.kind = NULL) 
#set.seed(33833)
rf <- randomForest(y ~ ., data = vowel.train)
prf <- predict(rf, vowel.test, type='class')

s <- svm(y ~ ., data = vowel.train)
ps <- predict(s, vowel.test, type='class')

erate <- function(nominal, pred) {
  sum(nominal != pred)/length(pred);
}

erate(prf, vowel.test$y)
erate(ps, vowel.test$y)

idx = prf == ps
## error rate when they both agree
erate(prf[idx], vowel.test$y[idx])
