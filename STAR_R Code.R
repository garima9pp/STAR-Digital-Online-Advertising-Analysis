
setwd("E:/BANA277/STAR")
library(tidyverse)
library(readr)
star_digital <- read_csv("star_digital.csv")
#library(readxl)
#star_digital_excel <- dataset <- read_excel("star digital.xls")
summary(star_digital)

#1. Is online advertising effective for Star Digital? In other words,
#is there a difference in conversion rate between the treatment and control groups?
ad_effect <- glm(purchase~test,data=star_digital, family=binomial())
summary(ad_effect)
#the p-value of "test" is 0.0614, which is greater than 0.05. It
#means that the test variable is not significant in predicting the purchase.
exp(0.07676)
#1.079783
exp(coef(ad_effect))
#0.9443631   1.0797852
test <- star_digital %>% filter(test==1)
control <- star_digital %>% filter(test==0)

t.test(test$purchase, control$purchase, alternative = "two.sided", var.equal = FALSE)
#p-value = 0.06139 indicates that there is a no statistically significant difference between the
#two purchases in test and control samples

t.test((test$imp_1+test$imp_2+test$imp_3+test$imp_4+test$imp_5+test$imp_6), 
       (control$imp_1+control$imp_2+control$imp_3+control$imp_4+control$imp_5+control$imp_6),
       alternative = "two.sided", var.equal = FALSE)
##p-value = 0.8987 indicates that there is a no statistically significant difference between the
#two impressions in test and control samples


#2. Is there a frequency effect of advertising on purchase? In particular, 
#the question is whether increasing the frequency of advertising (number of impressions) 
#increases the probability of purchase? 
star_digital_Q2 <- star_digital
head(star_digital_Q2)

star_digital_Q2 <- star_digital_Q2 %>% mutate(imp_11 = imp_1*test,imp_21=imp_2*test,imp_31=imp_3*test,
                           imp_41=imp_4*test,imp_51=imp_5*test,imp_61=imp_6*test)

##model for total impression
star_digital_Q2$tot_imp_test <- star_digital_Q2$imp_11+star_digital_Q2$imp_21+star_digital_Q2$imp_31+
                            star_digital_Q2$imp_41+star_digital_Q2$imp_51+star_digital_Q2$imp_61

star_digital_Q2$tot_imp <- star_digital_Q2$imp_1+star_digital_Q2$imp_2+star_digital_Q2$imp_3+
  star_digital_Q2$imp_4+star_digital_Q2$imp_5+star_digital_Q2$imp_6

fit_tot_imp <- glm(purchase ~ test+tot_imp+tot_imp_test, data= star_digital_Q2,family=binomial())
summary(fit_tot_imp)

#logit(p) = log(p/(1-p))= B0 + B1*test + B2*tot_imp + B3*test*tot_imp or tot_imp_test

#For control group (test=0), the equation is simply

#logit(p) = log(p/(1-p))= B0 + B2*tot_imp

#For test group, the equation is

#logit(p) = log(p/(1-p))= (B0 + B1) + (B2 + B3 )*tot_imp
#for the test group, a one-unit increase in impression yields a change in
#log odds of (0.015889 + 0.015466) = 0.031355 and odds of purchase increases by 3.2%
exp(0.015889 + 0.015466) 
#1.031852
exp( 0.015889 )
#1.016016
#for the control group the odds for purchase with 1 unit increase in impressions is 1.6%
#the p value for tot_imp and tot_imp_test variables is <0.05 implies that this variables are significant in
#predicting the odds of purchase
exp(coef(fit_tot_imp))
#(Intercept)         test      tot_imp tot_imp_test 
#0.8440214    0.9861932    1.0160156    1.0155865

#3.How does the effectiveness of Sites 1-5 compare with that of Site 6?
Star_Digital <- star_digital
head(Star_Digital)
Star_Digital$imp1_5 <- rowSums(Star_Digital[,4:8])	
#Model for site 6
fit_imp_6 <- glm(purchase ~ imp_6, data = Star_Digital, family=binomial())
summary(fit_imp_6)
exp(coef(fit_imp_6))
#Model for sits 1-5
fit_imp1_5 <- glm(purchase ~ imp1_5, data = Star_Digital, family=binomial())
summary(fit_imp1_5)
exp(coef(fit_imp1_5))
#1 unit increase in impressions on sites 1-5 increases the odds of purchase by 3.3%
#1 unit increase in impressions on site 6 increases the odds of purchase by 2%

#4. Optional Challenge Question -- Which sites should Star Digital advertise on? In particular, 
#should it put its advertising dollars in Site 6 or in Sites 1 through 5?

##model for  impression 1 to 5
star_digital_Q2$imp1_5_test <- star_digital_Q2$imp_11+star_digital_Q2$imp_21+star_digital_Q2$imp_31+
  star_digital_Q2$imp_41+star_digital_Q2$imp_51

star_digital_Q2$imp1_5 <- star_digital_Q2$imp_1+star_digital_Q2$imp_2+star_digital_Q2$imp_3+
  star_digital_Q2$imp_4+star_digital_Q2$imp_5

fit_imp1_5 <- glm(purchase ~ test+imp1_5+imp1_5_test, data= star_digital_Q2,family=binomial())
summary(fit_imp1_5)

#logit(p) = log(p/(1-p))= B0 + B1*test + B2*imp1_5 + B3*test*imp1_5 or (imp1_5_test)

#For control group (test=0), the equation is simply

#logit(p) = log(p/(1-p))= B0 + B2*imp1_5

#For test group, the equation is

#logit(p) = log(p/(1-p))= (B0 + B1) + (B2 + B3 )*imp1_5
#for the test group, a one-unit increase in impression yields a change in
#log odds of ( 0.019539  +0.014830) = 0.034369 and odds of purchase increases by 3.5%
exp( 0.019539  +0.014830) 
#1.034966
exp(0.019539)
#1.019731
#for the control group the odds for purchase with 1 unit increase in impressions is 1.9%
#the p value for imp1_5 and imp1_5_test variables is <0.05 implies that this variables are significant in
#predicting the odds of purchase

##model for  impression 6

fit_imp_6 <- glm(purchase ~ test+imp_6+imp_61, data= star_digital_Q2,family=binomial())
summary(fit_imp_6)

#logit(p) = log(p/(1-p))= B0 + B1*test + B2*imp_6 + B3*test*imp_6 or (imp_61)

#For control group (test=0), the equation is simply

#logit(p) = log(p/(1-p))= B0 + B2*imp_6

#For test group, the equation is

#logit(p) = log(p/(1-p))= (B0 + B1) + (B2 + B3 )*imp_6
#for the test group, a one-unit increase in impression yields a change in
#log odds of ( 0.005684  +0.017085) and odds of purchase increases by 2.3%
exp( 0.005684  +0.017085)
#1.02303
exp(0.005684)
#1.0057
#for the control group the odds for purchase with 1 unit increase in impressions is 0.5%
#the p value for imp_61 variables is <0.05 implies that this variables is significant in
#predicting the odds of purchase





