#set.seed(3343)
set.seed(3343, kind = "Mersenne-Twister", normal.kind = "default")
RNGkind()
for(i in 1:100){
  z = rnorm(20)
  x = rnorm(20)
  y = rnorm(20,mean=0.5*x)
  pValues[i] = summary(lm(y ~ x))$coef[2,4]
}

a = 0.1
sum(pValues < a/length(pValues))
sum(pValues < a*(1:length(pValues))/length(pValues))

sum(p.adjust(pValues, method='bonferroni') < a)
sum(p.adjust(pValues, method='BH') < a)

n = 100
m = 1000
b1err2 = rep(NA, m)
b1err = rep(NA, m)
for(i in 1:m) {
  x = rnorm(n)
  z = rexp(n)
  xdamaged = x
  xdamaged[x > z] = NA
  e = rnorm(n)
  b0 = 1
  b1 = 2
  y = b0 + b1*x + e
  f = lm(y ~ x)
  g = lm(y ~ xdamaged)
  b1err2[i] = (g$coeff[2] - b1)
  b1err[i] = (f$coeff[2] - b1)
}
summary(b1err)
summary(b1err2)


n = 100
m = 1000
b1err2 = rep(NA, m)
b1err = rep(NA, m)
for(i in 1:m) {
  x = rnorm(n)
  z = rexp(n)
  e = rnorm(n)
  b0 = 1
  b1 = 2
  y = b0 + b1*x + e
  ydamaged = y
  ydamaged[y > z + b0] = NA
  f = lm(y ~ x)
  g = lm(ydamaged ~ x)
  b1err2[i] = g$coeff[2] - b1
  b1err[i] = f$coeff[2] - b1
}
summary(b1err)
summary(b1err2)

library(MASS)
n = 100
m = 1000
b1err2 = rep(NA, m)
b1err = rep(NA, m)
for(i in 1:m) {
  x = rnorm(n)
  z = rexp(n)
  xdamaged = x
  xdamaged[x > z] = NA
  e = rnorm(n)
  b0 = 1
  b1 = 2
  y = b0 + b1*x + e
  f = rlm(y ~ x)
  g = rlm(y ~ xdamaged)
  b1err2[i] = (g$coeff[2] - b1)
  b1err[i] = (f$coeff[2] - b1)
}
summary(b1err)
summary(b1err2)


n = 100
m = 1000
b1err2 = rep(NA, m)
b1err = rep(NA, m)
for(i in 1:m) {
  x = rnorm(n)
  z = rexp(n)
  e = rnorm(n)
  b0 = 1
  b1 = 2
  y = b0 + b1*x + e
  ydamaged = y
  ydamaged[y > z + b0] = NA
  f = rlm(y ~ x)
  g = rlm(ydamaged ~ x)
  b1err2[i] = g$coeff[2] - b1
  b1err[i] = f$coeff[2] - b1
}
summary(b1err)
summary(b1err2)
