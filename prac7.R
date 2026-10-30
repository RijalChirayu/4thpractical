#Pract 1 

model_1_loglog <- lm(log(pract1$sales_000) ~ log(pract1$ad_exp_000))
summary(model_1_loglog)

model_1_loglin <- lm(log(pract1$sales_000) ~ pract1$ad_exp_000)
summary(model_1_loglin)

model_1_linlog <- lm(pract1$sales_000 ~ log(pract1$ad_exp_000))
summary(model_1_linlog)


#Pract2





#Pract 3 
wind_vel <- c(5 , 6, 3.4,2.7,10,9.7,9.55,3.05,8.15,6.2,2.9,6.35,4.6,5.8,7.4)
dc_output <- c(1.582,1.822,1.057,0.5,2.236,2.386,2.294,0.558,2.166,1.866,0.653,1.93,1.562,1.737,2.088)

model <- lm(dc_output ~ I(1/wind_vel))

summary(model)






#Pract 7
library(ggplot2)
ggplot(pract7 , aes(months , weight_loss_kg)) + geom_line()
#y = b0 + b1x + b2x^2 
pract7$months2 <- pract7$months * pract7$months
model <- lm(pract7$weight_loss_kg ~ pract7$months + pract7$months2)
summary(model)

#weight_loss = 1.633 - 1.232182month + 1.494545months2

#pract8
#y = b0 + b1x + b2x^2 + b3x^3

pract7$months3 <- pract7$months ^ 3
model_3 <- lm(pract7$weight_loss_kg ~ pract7$months + pract7$months2 + pract7$months3)
summary(model_3)


#pract9 
#y = b0 + b1x1 + b2x2 + b11x1^2 + b22x2^2 + b12x1x2 
pract9$income2 <- pract9$income_000^2
pract9$age2 <- pract9$age_yrs ^ 2
pract9$income_age <- pract9$income_000 * pract9$age_yrs

model_poly <- lm(pract9$expenditure_000 ~ pract9$income_000 + pract9$age_yrs + pract9$income2 + pract9$age2 + pract9$income_age)
summary(model_poly)


#pract10 
#y = b0 + b1x1 + b2x2 + b3x3 + b11x1^2 + b22x2^2 + b33x3^2 + b12x1x2 + b13x1x3 + b23x2x3
attach(pract10)
model <- lm(weight_kg ~ age_yrs + calories + height_ft + I(age_yrs ^2) +  I(calories ^2) + I(height_ft ^2)  + age_yrs:calories + age_yrs:height_ft + calories:height_ft)
summary(model)


library(ggplot2)
#fitted vs observed 
ggplot(pract10 , aes(x = weight_kg , y = fitted(model))) + 
  geom_point() + 
  geom_abline(slope = 1 , intercept = 0 , color = "red") +
  labs(x = "Observed" , y = "fitted")

#pract11 
attach(pract11)
#y = bo x1^b1 x2^b2 
#logy = logbo + b1 log(x1) + b2 log(x2)
model_doublas <- lm(log(output) ~ log(labour) + log(capital))
summary(model_doublas)

#pract12 


model <- lm(pract12$income_000 ~ pract12$gender + pract12$occupation)
summary(model)

#pract13
model_13 <- lm(pract13$salary_000 ~ pract13$gender + pract13$months_employed)
summary(model_13)


#pract14 
region <- factor(pract14$region , levels = c("Terai" , "Hill", "Mountain"))
model_14 <- lm(pract14$income_000 ~ pract14$age_yrs + region)
summary(model_14)

#pract15 same as 14 tehi extra independent variable 

#pract16 
library(tidyverse)
long <- pivot_longer(pract16 , cols = q1:q4 , names_to = "season" , values_to = "profit")
model_16 <- lm(long$profit ~ 0 + long$season)
summary(model_16)

#pract5
result_dummy <- factor(pract5$result , levels = c("Pass" , "Fail") , labels = c(1 , 0))
model_5 <- glm(formula = result_dummy ~ pract5$study_hrs + pract5$gender , family = binomial)
summary(model_5)

