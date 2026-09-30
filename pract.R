wind_vel <- c(5 , 6, 3.4,2.7,10,9.7,9.55,3.05,8.15,6.2,2.9,6.35,4.6,5.8,7.4)
dc_output <- c(1.582,1.822,1.057,0.5,2.236,2.386,2.294,0.558,2.166,1.866,0.653,1.93,1.562,1.737,2.088)

model <- lm(dc_output ~ I(1/wind_vel))

summary(model)
new_obs <- data.frame(wind_vel = c(9.5,10))
predict(model , newdata = new_obs)

data <- data.frame(windVelocity = 1/wind_vel , 
                   DcOutput = dc_output)

#pract5
head(pract5)
attach(pract5)
result <- factor(result, levels = c("Pass" ,"Fail") , labels = c(1 , 0))

model <- glm(result ~ study_hrs + gender , family = binomial)
summary(model)



