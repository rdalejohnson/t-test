library(ggpubr)
library(dplyr)


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

tTest <- t.test(my_data$weight, mu=25)

#dissecting the t test result

#t-statistic
nameOfStatistic <- names(tTest$statistic)[1]
tResult <-tTest$statistic

#method (one-sided, two-sided, one-sample)
hypothesisSides <- tTest$method

#degrees of freedom
parameter.Name <- names(tTest$parameter)[1]
degreesOfFreedom <- tTest$parameter[1]


