data(warpbreaks);
x <- warpbreaks;
y <- aov(x$breaks ~ x$tension + x$wool); #Order of independent vars matters for F-statistic calc
summary(y)
y$coeff

library(glm2);
data(crabs);
x <- crabs;
y <- glm(x$Satellites ~ x$Width, family='poisson');
summary(y)

exp(0.16405)                            #multiplicative factor increase

exp(-3.30476 + 22*0.16405)

## auto model selection
library(MASS);
data(quine);
x <- quine[,-5];
lm1 <- lm(I(log(quine$Days+2.5)) ~ ., data=x);
aic = step(lm1);

