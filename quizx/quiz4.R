
x <- read.csv('movies.txt', sep='\t')
colnames(x)
y <- lm(x$score ~ x$box.office)
confint(y, level=0.9)

z <- lm(x$score ~ x$box.office + x$running.time)
summary(z)

w <- x[x$running.time < 175,]
zw <- lm(w$score ~ w$box.office + w$running.time)
summary(zw)

## Including an interaction term between rating and running.time (can also be written as a*b)
# q <- lm(x$score ~ as.factor(x$rating) + x$running.time + x$box.office:as.factor(x$rating))
q <- lm(x$score ~ as.factor(x$rating)*x$running.time);
## Affect of running time for PG movies is -0.6901 + 1.1852

data(warpbreaks)
wb <- warpbreaks
# Reorder factors so avg of coeff[H] - coeff[M] = coeff[3]
## FIXME: should use lm(wb$breaks ~ relevel(wb$tension, ref='H'))
wb$tension <- factor(wb$tension, levels = c('H', 'L', 'M'))
ywb <- lm(wb$breaks ~ as.factor(wb$tension))
confint(ywb, level=0.95)

