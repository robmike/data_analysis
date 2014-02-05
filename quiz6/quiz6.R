library(ElemStatLearn)
data(SAheart)
set.seed(8484)
train = sample(1:dim(SAheart)[1],size=dim(SAheart)[1]/2,replace=F)
trainSA = SAheart[train,]
testSA = SAheart[-train,]

missClass = function(values,prediction){sum(((prediction > 0.5)*1) != values)/length(values)}

x <- trainSA
## logistic regression
glm1 <- glm(chd ~ age + alcohol + typea + ldl + obesity + tobacco, data=trainSA, family='binomial')

ptrain <- predict(glm1, newdata = trainSA, type = 'response')
ptest <- predict(glm1, newdata = testSA, type = 'response')

missClass(trainSA$chd, ptrain)
missClass(testSA$chd, ptest)

## example of how to suppress unwanted covariates
# model3 <- glm(chd ~ .-sbp-adiposity-famhist, data = trainSA, family = 'binomial')
## model4 <- glm(chd ~ ., data = trainSA[,-c(1,4,5)], family = 'binomial')


library(pgmm)
data(olive)
olive = olive[,-1]

library(tree)
tree1 <- tree(Area~., data=olive)
newdata = data.frame(Palmitic = 1200, Palmitoleic = 120, Stearic=200,Oleic=7000,Linoleic = 900, Linolenic = 32, Arachidic=60,Eicosenoic=7)
p <- predict(tree1, newdata)

olive[,1] <- as.factor(olive[,1])
t2 <- tree(Area~., data=olive)
tree2 <- prune.tree(t2, best=6)
plot(tree2)
text(tree2)
newdata = data.frame(Palmitic = 1200, Palmitoleic = 120, Stearic=200,Oleic=7000,Linoleic = 900, Linolenic = 32, Arachidic=60,Eicosenoic=6)
p <- predict(tree2, newdata, type='class')

