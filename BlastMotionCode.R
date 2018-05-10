# 1. Initial linear regression model correlating all variables to blast factor
#Manage The Data
Blast.All.Swings$Player = NULL
Blast.All.Swings$Swing.. = NULL

#Build The Model
model = lm(Blast.All.Swings$Blast.Factor ~ ., data = Blast.All.Swings)

summary(model)

# 2. Linear Regression model taking out one variable at a time correlated to blast factor
#Manage The Data
Blast.All.Swings$Player = NULL
Blast.All.Swings$Swing.. = NULL
Blast.All.Swings$Bat.Speed..mph. = NULL

#Build The Model
model = lm(Blast.All.Swings$Blast.Factor ~ ., data = Blast.All.Swings)

summary(model)

#3.  Plot showing the relationship between a variable and Blast Factor
#Build The Plot
ggplot(Blast.All.Swings, aes(Bat.Speed..mph., Blast.Factor)) +
  geom_point(aes(colour=Player)) + 
  labs(title = "Bat Speed v Blast Factor") + 
  labs(colour = "Player Name") + 
  labs(x = "Bat Speed (MPH)") + 
  labs(y = "Blast Factor (1-100)")

# 4.Linear Regression model for spring using all variables
#Load The Data
df <- read.csv("C:/Users/mdemarte1/Documents/SpringwOBATestData.csv")

#build the model 
model = lm(Spring.wOBA ~ ., data = df)

summary(model)

# 5. Linear regression model for spring using backwards selection
#Load The Data
df <- read.csv("C:/Users/mdemarte1/Documents/SpringwOBATestData.csv")

#Select The Variables
model1 = lm(df$Spring.wOBA ~ 1, data=df)
model3 = lm(df$Spring.wOBA ~ ., data=df)
step(model3, scope = list("lower" = model1),
     direction ="backward")

df$On.Plane.... = NULL
df$Body.Rotation.... = NULL
df$Peak.Hand.Speed..mph. = NULL


#build the model 
model = lm(Spring.wOBA ~ ., data = df)

summary(model)

# 6. Linear Regression model for the fall using all variables
#Load The Data
df <- read.csv("C:/Users/mdemarte1/Documents/FallwOBATestData.csv")

#build the model 
model = lm(Fallwoba ~ ., data = df)

summary(model)

# 7. Linear regression for fall using backwards variable selection
#Load The Data
df <- read.csv("C:/Users/mdemarte1/Documents/FallwOBATestData.csv")

#Select The Variables
model1 = lm(df$Fallwoba ~ 1, data=df)
model3 = lm(df$Fallwoba ~ ., data=df)
step(model3, scope = list("lower" = model1),
     direction ="backward")

df$Attack.Angle..deg. = NULL
df$Vertical.Bat.Angle..deg. = NULL

#build the model 
model = lm(Fallwoba ~ ., data = df)

summary(model)

# 8. MAPE & RMSE for spring regression model 
#Load The Data
df <- read.csv("C:/Users/mdemarte1/Documents/SpringwOBATestData.csv")

#Manage The Variables
df$On.Plane.... = NULL
df$Body.Rotation.... = NULL
df$Peak.Hand.Speed..mph. = NULL

#build the model
model = lm(Spring.wOBA ~ ., data = df)

#Make Predictions
Predictions = predict(model, df)

#Evaluate Perfromance
observations = df$Spring.wOBA
errors = observations- predictions

mape = mean(abs(errors)/observations)
rmse = sqrt(mean((errors)^2))

rmse_benchmark = sqrt(mean((observations - mean(observations))^2))
mape_benchmark = mean(abs((observations - mean(observations))/observations))

# 9. MAPE & RMSE for the fall regression model
#Load The Data
df <- read.csv("C:/Users/mdemarte1/Documents/FallwOBATestData.csv")

#Manage The Variables
df$Attack.Angle..deg. = NULL
df$Vertical.Bat.Angle..deg. = NULL

#build the model
model = lm(Fallwoba ~ ., data = df)

#Make Predictions
predictions = predict(model, df)

#Evaluate Perfromance
observations = df$Fallwoba
errors = observations- predictions

mape = mean(abs(errors)/observations)
rmse = sqrt(mean((errors)^2))

rmse_benchmark = sqrt(mean((observations - mean(observations))^2))
mape_benchmark = mean(abs((observations - mean(observations))/observations))

# 10. Regression tree with MAPE & RMSE for spring 
#Load The Data
df <- read.csv("C:/Users/mdemarte1/Documents/SpringwOBATestData.csv")

#Manage The Variables
df$Peak.Hand.Speed..mph. = NULL
df$Body.Rotation.... = NULL
df$On.Plane.... = NULL

#build the model
model = rpart(Spring.wOBA ~ ., data=df)
fancyRpartPlot(model, main="Full Tree")

#Make Predictions
predictions = predict(model, df)

#Evaluate Perfromance
observations = df$Spring.wOBA
errors = observations- predictions

mape = mean(abs(errors)/observations)
rmse = sqrt(mean((errors)^2))

rmse_benchmark = sqrt(mean((observations - mean(observations))^2))
mape_benchmark = mean(abs((observations - mean(observations))/observations))

# 11. Regression tree with MAPE & RMSE for fall
#Load The Data
df <- read.csv("C:/Users/mdemarte1/Documents/SpringwOBATestData.csv")

#Manage The Variables
df$Attack.Angle..deg. = NULL
df$Vertical.Bat.Angle..deg. = NULL

#build the model
model = rpart(Fallwoba ~ ., data=df)
fancyRpartPlot(model, main="Full Tree")

#Make Predictions
predictions = predict(model, df)

#Evaluate Perfromance
observations = df$Fallwoba
errors = observations- predictions

mape = mean(abs(errors)/observations)
rmse = sqrt(mean((errors)^2))

rmse_benchmark = sqrt(mean((observations - mean(observations))^2))
mape_benchmark = mean(abs((observations - mean(observations))/observations))

# 12. Cluster comparing bat speed to blast factor
#Load The Data
df <- read.csv("C:/Users/mdemarte1/Documents/BSvsBF.csv")

model = kmeans(df, 4)
plot(df,col=model$cluster)

#13. Cluster comparing power to time to contact
#Load The Data
df <- read.csv("C:/Users/mdemarte1/Documents/TTCvsP.csv")

model = kmeans(df, 4)
plot(df, main="Clusters of Time to Contact vs. Power", xlab = "Time to Contact", ylab = "Power", col=model$cluster)

# 14. Plot comparing bat speed to blast factor for spring catchers
#Build Plot
plot(SpringCatchers$Bat.Speed..mph.,
     SpringCatchers$Blast.Factor,
     col = SpringCatchers$Player,
     xlab = "Bat Speed (MPH)",
     ylab = "Blast Factor (1-100)",
     main = "Bat Speed v Blast Factor for Spring Catchers")

#Add A Legend
legend("bottomright",
       legend = levels(SpringCatchers$Player),
       col = c(1:2), pch=16)

#Add A Legend
legend("topright",
       legend =c(.161, .411),
       col= c(1:2), pch =16)

# 15. Plot comparing power and time to contact for spring catchers
#Build Plot
plot(SpringCatchers$Power..kW.,
     SpringCatchers$Time.to.Contact..sec.,
     col = SpringCatchers$Player,
     xlab = "Bat Speed (MPH)",
     ylab = "Blast Factor (1-100)",
     main = "Power v Time to Contact for Spring Catchers")

#Add A Legend
legend("bottomright",
       legend = levels(SpringCatchers$Player),
       col = c(1:2), pch=16)

#Add A Legend
legend("topright",
       legend =c(.161, .411),
       col= c(1:2), pch =16)

#16. Plot comparing bat speed and blast factor for spring infielders
#Build Plot
plot(SpringInfielders$Bat.Speed..mph,
     SpringInfielders$Blast.Factor,
     col = SpringInfielders$Player,
     xlab = "Bat Speed (MPH)",
     ylab = "Blast Factor (1-100)",
     main = "Bat Speed v  Blast Facor for Spring Infielders",
     pch =16)

#Add A Legend
legend("topright",
       legend = levels(SpringInfielders$Player),
       col = c(1:3), pch=16)

#Add A Legend
legend("bottomleft",
       legend =c(.398, .349, .323),
       col= c(1:3), pch =16)

# 17. Plot comparing power to time to contact for spring infielders
#Build Plot
plot(SpringInfielders$Power..kW.,
     SpringInfielders$Time.to.Contact..sec.,
     col = SpringInfielders$Player,
     xlab = "Power (KW)",
     ylab = "Time to Contact (sec.)",
     main = "Power v Time to Contact for Spring Infielders",
     pch =16)

#Add A Legend
legend("topright",
       legend = levels(SpringInfielders$Player),
       col = c(1:3), pch=16)

#Add A Legend
legend("bottomleft",
       legend =c(.398, .349, .323),
       col= c(1:3), pch =16)

# 18. Plot comparing bat speed to blast factor for spring outfielders
#Build Plot
plot(SpringOutfielders$Bat.Speed..mph.,
     SpringOutfielders$Blast.Factor,
     col = SpringOutfielders$Player,
     xlab = "Bat Speed (MPH)",
     ylab = "Blast Factor (1-100)",
     main = "Bat Speed v Blast Factor for Spring Outfielders",
     pch =16)

#Add A Legend
legend("bottomright",
       legend = levels(SpringOutfielders$Player),
       col = c(1:4), pch=16)

#Add A Legend
legend("bottomleft",
       legend =c(.295, .285, .377, .470),
       col= c(1:4), pch =16)

#19. Plot comparing power to time to contact for spring outfielders
#Build Plot
plot(SpringOutfielders$Power..kW.,
     SpringOutfielders$Time.to.Contact..sec.,
     col = SpringOutfielders$Player,
     xlab = "Power (kW)",
     ylab = "Time to Contact (sec)",
     main = "Bat Speed v Blast Factor for Spring Outfielders",
     pch =16)

#Add A Legend
legend("topright",
       legend = levels(SpringOutfielders$Player),
       col = c(1:4), pch=16)

#Add A Legend
legend("bottomright",
       legend =c(.295, .285, .377, .470),
       col= c(1:4), pch =16)

# 20. Plot comparing bat speed and blast factor for fall catchers
#Build Plot
plot(Fall.Catchers$Bat.Speed..mph.,
     Fall.Catchers$Blast.Factor.,
     col = Fall.Catchers$Player,
     xlab = "Bat Speed (mph)",
     ylab = "Blast Factor (1-100)",
     main = "Bat Speed v Blast Factor for Fall Catchers",
     pch =16)

#Add A Legend
legend("bottomright",
       legend = levels(Fall.Catchers$Player),
       col = c(1:3), pch=16)

#Add A Legend
legend(85, 60,
       legend =c(.314, .330, .675),
       col= c(1:3), pch =16)

# 21. Plot comparing power to time to contact for fall catchers
#Build Plot
plot(Fall.Catchers$Power..kW.,
     Fall.Catchers$Time.to.Contact..sec.,
     col = Fall.Catchers$Player,
     xlab = "Power (kW)",
     ylab = "Time to Contact (sec)",
     main = "Power v Time to Contact for Fall Catchers",
     pch =16)


#Add A Legend
legend("bottomleft",
       legend = levels(Fall.Catchers$Player),
       col = c(1:3), pch=16)

#Add A Legend
legend("topright",
       legend =c(.314, .330, .675),
       col= c(1:3), pch =16)

# 22. Plot comparing bat speed and blast factor for fall infielders
#Build Plot
plot(Fall.Infielders$Bat.Speed..mph.,
     Fall.Infielders$Blast.Factor,
     col = Fall.Infielders$Player,
     xlab = "Bat Speed (mph)",
     ylab = "Blast Factor (1-100)",
     main = "Bat Speed v Blast Factor for Fall Infielders",
     pch =16)

#Add A Legend
legend("topright",
       legend = levels(Fall.Infielders$Player),
       col = c(1:5), pch=16)

#Add A Legend
legend("bottomleft",
       legend =c(.340, .580, .273, .211, .414),
       col= c(1:5), pch =16)

# 23. Plot comparing power and time to contact for fall infielders
#Build Plot
plot(Fall.Infielders$Power..kW.,
     Fall.Infielders$Time.to.Contact..sec.,
     col = Fall.Infielders$Player,
     xlab = "Power (kW)",
     ylab = "Time to Contact (sec)",
     main = "Power v Time to Contact for Fall Infielders",
     pch =16)

#Add A Legend
legend("topright",
       legend = levels(Fall.Infielders$Player),
       col = c(1:5), pch=16)

#Add A Legend
legend("bottomleft",
       legend =c(.340, .580, .273, .211, .414),
       col= c(1:5), pch =16)

#24. Plot comparing bat speed to blast factor for fall outfielders
#Build Plot
plot(Fall.Outfielders$Bat.Speed..mph.,
     Fall.Outfielders$Blast.Factor,
     col = Fall.Outfielders$Player,
     xlab = "Bat Speed (mph)",
     ylab = "Blast Factor (1-100)",
     main = "Bat Speed v Blast Factor for Fall Outfielders",
     pch =16)

#Add A Legend
legend("bottomright",
       legend = levels(Fall.Outfielders$Player),
       col = c(1:5), pch=16)

#Add A Legend
legend("topleft",
       legend =c(.309, .303, .312, .278, .431),
       col= c(1:5), pch =16)

# 25. Plot comparing power and time to contact for fall outfielders
#Build Plot
plot(Fall.Outfielders$Power..kW.,
     Fall.Outfielders$Time.to.Contact..sec.,
     col = Fall.Outfielders$Player,
     xlab = "Power (kW)",
     ylab = "Time to Contact (sec)",
     main = "Power v Time to Contact for Fall Outfielders",
     pch =16)

#Add A Legend
legend("bottomright",
       legend = levels(Fall.Outfielders$Player),
       col = c(1:5), pch=16)

#Add A Legend
legend("topright",
       legend =c(.309, .303, .312, .278, .431),
       col= c(1:5), pch =16)

# 26. Plot comparing bat speed to blast factor for fall two way players (pitchers and hitters)
#Build Plot
plot(Fall.Two.Way$Bat.Speed..mph.,
     Fall.Two.Way$Blast.Factor,
     col = Fall.Two.Way$Player,
     xlab = "Bat Speed (mph)",
     ylab = "Blast Factor (1-100)",
     main = "Bat Speed v Blast Factor for Fall Two-Way Players",
     pch =16)

#Add A Legend
legend("bottomright",
       legend = levels(Fall.Two.Way$Player),
       col = c(1:6), pch=16)

#Add A Legend
legend("topright",
       legend =c(.099, .368, .370, .384, .238, .170),
       col= c(1:6), pch =16)

# 27. Plot comparing power and time to contact for fall two way players
#Build Plot
plot(Fall.Two.Way$Power..kW.,
     Fall.Two.Way$Time.to.Contact..sec.,
     col = Fall.Two.Way$Player,
     xlab = "Power (kW)",
     ylab = "Time to Contact (sec)",
     main = "Power v Time to Contact for Fall Two-Way Players",
     pch =16)

#Add A Legend
legend("topright",
       legend = levels(Fall.Two.Way$Player),
       col = c(1:6), pch=16)

#Add A Legend
legend(4, .134,
       legend =c(.099, .368, .370, .384, .238, .170),
       col= c(1:6), pch =16)

# 28. Regression tree for swing variables correlating to blast factor with error rates
#ManageTheData
Blast.All.Swings$Player = NULL
Blast.All.Swings$Swing.. = NULL

#build the model
model = rpart(Blast.Factor ~ ., data= Blast.All.Swings)
fancyRpartPlot(model, main="Full Tree")


#Make Predictions
predictions = predict(model, Blast.All.Swings)

#Evaluate Perfromance
observations = Blast.All.Swings$Blast.Factor
errors = observations- predictions

mape = mean(abs(errors)/observations)
rmse = sqrt(mean((errors)^2))

rmse_benchmark = sqrt(mean((observations - mean(observations))^2))
mape_benchmark = mean(abs((observations - mean(observations))/observations))


















