library(ggpubr)
library(dplyr)

#https://stackoverflow.com/questions/36699272/why-is-message-a-better-choice-than-print-in-r-for-writing-a-package

#basic example, randomly generating a sample of values from the
#normal distribution

#https://datascienceplus.com/t-tests/
set.seed(6733)
treeVolume <- c(rnorm(75, mean = 36500, sd = 2000))
t.test(treeVolume, mu = 39000) # Ho: mu = 39000

#http://www.sthda.com/english/wiki/one-sample-t-test-in-r
#http://www.sthda.com/english/wiki/normality-test-in-r
#https://www.rdocumentation.org/packages/EnvStats/versions/2.3.1/topics/htest.object
set.seed(1234)
my_data <- data.frame(
  name = paste0(rep("M_", 10), 1:10),
  weight = round(rnorm(10, 20, 2), 1)
)
head(my_data, 10)

summary(my_data$weight)

#Box Plot
ggboxplot(my_data$weight, 
          ylab = "Weight (g)", xlab = FALSE,
          ggtheme = theme_minimal())

#QQ Plot
ggqqplot(my_data$weight)

#Density Plot
ggdensity(my_data$weight, 
          main = "Density Plot of Mice Weight in  Grams",
          xlab = "Weight (g)")


#Significance test for Normality
#K-S normality test or Shapiro-Wilk's method:
#significance test 
#comparing the sample distribution to a normal 
#one in order to ascertain whether data show 
#or not a serious deviation from normality
###
#The null hypothesis of these tests is that 
#“sample distribution is normal”. 
#SO YOU WANT P >= 0.05!
#If the test is significant, the distribution is 
#non-normal.


#Shapiro-Wilk’s method is widely recommended for 
#normality test and it provides better power 
#than K-S. It is based on the correlation 
#between the data and the corresponding normal scores.
#Note that, normality test is sensitive to sample size. 
#Small samples most often pass normality tests. 
#Therefore, it’s important to combine visual 
#inspection and significance test in order to take the 
#right decision.

# RETURNS AN H-TEST OBJECT
# https://www.dummies.com/programming/r/how-to-extract-data-test-results-with-r/
shapiro <- shapiro.test(my_data$weight)
nameOfStatistic <- names(shapiro$statistic)[1]
result <- shapiro$statistic
pValue <- shapiro$p.value


#One-sample t-test
# RETURNS AN H-TEST OBJECT

sample.Mean <- mean(my_data$weight)
sample.stdev <- sd(my_data$weight)
sample.stdError <- sample.stdev/sqrt(length(my_data$weight))
t_statistic_step_by_step <- (sample.Mean - 25)/sample.stdError
p_value_by_hand <- 2*pt(-abs(t_statistic_step_by_step), 
                        length(my_data$weight)-1, lower.tail=TRUE)

tTest <- t.test(my_data$weight, mu=25, conf.level = 0.95)

#dissecting the t test result

#t-statistic
nameOfStatistic <- names(tTest$statistic)[1]
tResult <-tTest$statistic

#method (one-sided, two-sided, one-sample)
hypothesisSides <- tTest$method

#degrees of freedom
parameter.Name <- names(tTest$parameter)[1]
degreesOfFreedom <- tTest$parameter[1]

pValue <- tTest$p.value

cat("t-statistic:", tResult, "\n")
cat("sides: ", degreesOfFreedom, "\n")
cat("Degrees of Freedom:", degreesOfFreedom, "\n")
cat("p-value:", pValue, "\n")


#https://www.youtube.com/watch?v=qCowRPmRmno

mileage = c(11601, 8987, 12166, 9657, 10143, 8230, 3111, 13009, 7891, 10392)

t.test(mileage, 
       mu=12000,  #null hypothesis, alternative hypothesis is 2 sided.
       alternative = "two.sided", 
       conf.level = 0.95)

t.statistic.manually <- (mean(mileage)-12000)/(sd(mileage)/sqrt(length(mileage)))
2*pt(t.statistic.manually, length(mileage)-1)

#https://www.dummies.com/education/math/statistics/plotting-t-base-r-graphics/

plot(x=seq(-4,4,.1), y=dt(seq(-4,4,.1), 3), type="l", lty="dotted",
     ylim = c(0,.4), xlab="t", ylab = "f(t)")

