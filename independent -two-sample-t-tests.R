library(car)

# https://lindeloev.github.io/tests-as-linear/


# http://www.sthda.com/english/wiki/wiki.php?id_contents=7600

# Women's weights
x<- c(38.9, 61.2, 73.3, 21.8, 63.4, 64.6, 48.4, 48.8, 48.5, 43.6)
# Men's weights
y <- c(67.8, 60, 63.4, 76, 89.4, 73.3, 67.3, 61.3, 62.4, 111.2) 


#### Default t-test uses the WELCH T-TEST by default, which is a t-test used when
####  two samples have possibly unequal variances

# can perform an f-test to check if the variances of x and y are equal
var.test(x,y)

#The p-value of the F-test is = 0.8414. It’s greater than the 
#significance level alpha = 0.05. In conclusion, there is no significant 
#difference between the variances of the two sets of data. 
#Therefore, we can use the classic t-test witch assume equality of the two variances.


###############  data in two separate numeric vectors

#WELCH'S t-test
#htest object is returned:

two.sample.ttest <- t.test(x,y)

two.sample.ttest


two.sample.not.welch.ttest <- t.test(x,y, var.equal = TRUE)
two.sample.not.welch.ttest


################# if data were in a dataframe:

d<-as.data.frame(list(
  group=c(rep("Woman", 10), rep("Man", 10)),
  weight=c(x, y)
))

two.sample.dataframe.ttest <-t.test(weight ~ group, data=d)

two.sample.dataframe.ttest


#https://www.statology.org/levenes-test-r/
#are the variances equal?
#null hypothesis: variances among the two groups are equal
#alternative hypothesis: variances among the two groups are NOT equal
#Levene's test produces a test statistic and a corresponding p-value.
#If the p-value is below your pre-selected significance level, you can
#reject the NULL hypothesis

levene <- leveneTest(weight ~ group, data=d)

boxplot(
        weight ~ group,
        data = d,
        main = "Weight By Gender",
        xlab = "gender",
        ylab = "weight",
        col = "steelblue",
        border = "black")


####################

two.sample.not.welch.ttest$p.value
two.sample.not.welch.ttest$conf.int

two.sample.ttest$p.value
two.sample.ttest$conf.int



#Linear model version
#https://scientificallysound.org/2017/06/08/t-test-as-linear-models-r/

t.test.lm = lm(weight ~ group, data=d) 
summary(t.test.lm)