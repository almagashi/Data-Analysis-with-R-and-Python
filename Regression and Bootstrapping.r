### First question ###
# produce 998 random values for numbers between 0 and 100
X <- sample.int (100, 998, replace=TRUE)
#X

# produce noise
Y <- (-3*X) + 20 + rnorm(998)
#Y

# dataset without outliers + regression
dataset1 <- data.frame (X,Y)
lm1 <- lm (Y~X, data=dataset1)
summary(lm1)

# produce outliers
X1 <- c(X, 500, 2000)
#X1

Y1 <- c(Y, 1000, 2000)
#Y1

# dataset with outliers + regression
dataset2 <- data.frame(X1,Y1)
lm2 <- lm(Y1~X1, data=dataset2)
summary (lm2)

# figure that represents the power of outliers
plot(dataset2, xlab="X", ylab="Y", xlim=c(0,2000), ylim=c(-1000,2000), main="Outliers in the Regression")


abline(a = coef(lm1)[1], 
       b = coef (lm1)[2],
       col = "blue")
abline(a = coef(lm2)[1],
       b = coef(lm2)[2],
       col = "red")
legend(500, 2000, pch = c(15,15), legend = c('Original Dataset', 'Dataset with Outlier'), text.col = c('blue', 'red'), col = c('blue', 'red'))




### Second question ###

### A ###

library(Matching)
library (arm)
data (lalonde)

# treats
lalonde.trt <- lalonde[which(lalonde$treat == 1), ]

# find means for the variables and medians
mean_edu <- mean(lalonde.trt$educ)
mean_re74 <- mean (lalonde.trt$re74)
mean_re75 <- mean (lalonde.trt$re75)

median_edu <-  median (lalonde.trt$educ)
median_re74 <-  median (lalonde.trt$re74)
median_re75 <-  median (lalonde.trt$re75)

# regression model
lm.model <- lm (re78 ~ age+I(age**2)+educ+treat+treat*age+re74+re75, data=lalonde)
summary (lm.model)

# simulate
sim_lalonde <- sim (lm.model, n.sims=10000)
sim_lalonde@coef

# predict re78 if coeffiecients are at median
sim.ys_mean <- matrix(NA, nrow = 10000, ncol = length(
  min(lalonde$age):max(lalonde$age)))

for (age in min(lalonde$age):max(lalonde$age)){
    x_es <- c(1, age, age^2, mean_edu, 1, mean_re74, mean_re75, 1*age)
    for (i in 1:10000){
        sim.ys_mean[i, age + 1 - min(lalonde$age)] <- sum(x_es*sim_lalonde@coef[i,])
    }
}

conf.intervals <- apply(sim.ys_mean, 2, quantile, probs = c(0.025, 0.975))
table1 <- t(data.frame(conf.intervals))
colnames(table1) <- c("CI Lower Bound", "CI Upper Bound")
table1 <- data.frame(table1, mean_edu, mean_re74, mean_re75)
rownames(table1) <- min(lalonde$age):max(lalonde$age)
table1

plot(x = c(1:100), y = c(1:100), type = "n", 
     xlim = c(min(lalonde$age),max(lalonde$age)), 
     ylim = c(-10000,20000), 
     main = "Re78 (while predictors held at the means for treated units", xlab = "Age", 
     ylab = "Re78")

for (age in min(lalonde$age):max(lalonde$age)) {
  segments(
    x0 = age,
    y0 = conf.intervals[1, age - min(lalonde$age) + 1],
    x1 = age,
    y1 = conf.intervals[2, age - min(lalonde$age) + 1],
    lwd = 2)
}

### B ###
lalonde_control <- lalonde[which(lalonde$treat == 0), ]
sim.ys_control <- matrix (NA, nrow=10000, ncol=length(min(lalonde$age):max(lalonde$age)))

# find means for control 
mean_edu_control <- mean(lalonde[which(lalonde$treat == 0),]$educ)
mean_re74_control <- mean(lalonde[which(lalonde$treat == 0),]$re74)
mean_re75_control <- mean(lalonde[which(lalonde$treat == 0),]$re75)

# run through all ages

for (age in min(lalonde$age):max(lalonde$age)){
    treat <- 0
    xs <- c(1, age, age**2, mean_edu_control, treat, mean_re74_control, mean_re75_control, treat*age)
  for (i in 1:10000) {
    sim.ys_control[i, age + 1 - min(lalonde_control$age)] <- sum(xs*sim_lalonde@coef[i,])
  }
}

conf.intervals_control <- apply(sim.ys_control, 2, quantile, probs = c(0.025, 0.975))
table2 <- t(data.frame(conf.intervals_control))
colnames(table2) <- c("CI Lower Bound", "CI Upper Bound")
table2 <- data.frame(table2, mean_edu_control, mean_re74_control, mean_re75_control)
rownames(table2) <- min(lalonde$age):max(lalonde$age)
table2

plot(x = c(1:100), y = c(1:100), type = "n", 
     xlim = c(min(lalonde$age),max(lalonde$age)), 
     ylim = c(-10000,20000), 
     main =  "Re78 (while predictors held at the means for control units)", xlab = "Age", 
     ylab = "Re78")

for (age in min(lalonde$age):max(lalonde$age)) {
  segments(
    x0 = age,
    y0 = conf.intervals_control[1, age - min(lalonde$age) + 1],
    x1 = age,
    y1 = conf.intervals_control[2, age - min(lalonde$age) + 1],
    lwd = 2)
}

### C ######
sim.ys_mean <- matrix(NA, nrow = 10000, ncol = length(min(lalonde$age):max(lalonde$age)))
sim.ys_control <- matrix(NA, nrow = 10000, ncol = length(min(lalonde$age):max(lalonde$age)))

mean_edu_effect <- mean(lalonde$educ)
mean_re74_effect <- mean(lalonde$re74)
mean_re75_effect <- mean(lalonde$re75)

for (age in min(lalonde$age):max(lalonde$age)) {
  treat <- 1
  control <- 0
  xs1 <- c(1, age, age**2, mean_edu_effect, treat, mean_re74_effect, mean_re75_effect, treat*age)
  xs2 <- c(1, age, age**2, mean_edu_effect, control, mean_re74_effect, mean_re75_effect, control*age)
  for (i in 1:10000) {
    sim.ys_mean[i, age + 1 - min(lalonde$age)] <- sum(xs1*sim_lalonde@coef[i,])
    sim.ys_control[i, age + 1 - min(lalonde$age)] <- sum(xs2*sim_lalonde@coef[i,])
  }
}

simulated.ys_effect <- sim.ys_mean-sim.ys_control

simulated.ys_effect

conf.intervals_eff <- apply(simulated.ys_effect, 2, quantile, probs = c(0.025, 0.975))
table_eff <- t(data.frame(conf.intervals_eff))
colnames(table_eff) <- c("CI Lower Bound", "CI Upper Bound")
table_eff <- data.frame(table_eff, mean_edu_effect, mean_re74_effect, mean_re75_effect)
rownames(table_eff) <- min(lalonde$age):max(lalonde$age)
table_eff

plot(x = c(1:100), y = c(1:100), type = "n", 
     xlim = c(min(lalonde$age),max(lalonde$age)), 
     ylim = c(-10000,20000), 
     main = "Re78 treatment effect (while predictors held at the means)", xlab = "Age", 
     ylab = "Treatment Effect for Re78")

for (age in min(lalonde$age):max(lalonde$age)) {
  segments(
    x0 = age,
    y0 = conf.intervals_eff[1, age - min(lalonde$age) + 1],
    x1 = age,
    y1 = conf.intervals_eff[2, age - min(lalonde$age) + 1],
    lwd = 2)
}


### D ###

sim.ys_mean_sigma <- matrix (NA, nrow = 10000, ncol = length(min(lalonde$age): max(lalonde$age)))
sim.ys_control_sigma <- matrix (NA, nrow = 10000, ncol = length(min(lalonde$age): max(lalonde$age)))

median_edu <-  median (lalonde.trt$educ)
median_re74 <-  median (lalonde.trt$re74)
median_re75 <-  median (lalonde.trt$re75)

for (age in min(lalonde$age):max(lalonde$age)) {
  treat <- 1
  control <- 0
  xs1 <- c(1, age, age**2, median_edu, treat, median_re74, median_re75, treat*age)
  xs2 <- c(1, age, age**2, median_edu, control, median_re74, median_re75, control*age)
  for (i in 1:10000) {
    sim.ys_mean_sigma[i, age + 1 - min(lalonde$age)] <- sum(xs1*sim_lalonde@coef[i,]) + rnorm (1, 0, sim_lalonde@sigma[i])
    sim.ys_control_sigma[i, age + 1 - min(lalonde$age)] <- sum(xs2*sim_lalonde@coef[i,]) + rnorm (1, 0, sim_lalonde@sigma[i])
  }
}

sim.predict <- sim.ys_mean_sigma-sim.ys_control_sigma

confint_predictions <- apply (sim.predict, 2, quantile, probs = c (0.025, 0.975))
table_pred <- t (data.frame(confint_predictions))
colnames(table_pred) <- c("PI Lower Bound", "PI Upper Bound")
table_pred <- data.frame (table_pred, median_edu, median_re74, median_re75)
rownames(table_pred) <- min(lalonde$age):max(lalonde$age)
table_pred

plot(x = c(1:100), y = c(1:100), type = "n", 
     xlim = c(min(lalonde$age),max(lalonde$age)), 
     ylim = c(-30000,30000), 
     main = "Re78 prediction (while predictors held at the medians)", xlab = "Age", 
     ylab = "Treatment Effect for Re78")

for (age in min(lalonde$age):max(lalonde$age)) {
  segments(
    x0 = age,
    y0 = confint_predictions[1, age - min(lalonde$age) + 1],
    x1 = age,
    y1 = confint_predictions[2, age - min(lalonde$age) + 1],
    lwd = 2)
}


### Third Question ###

after_school <- read.csv(url("https://tinyurl.com/y2prc9xq"))
after_school <- after_school[-which(is.na(after_school)),]

# regression

lm.after_school <- lm (MATH_SCORE ~ TREATMENT, data=after_school)
summary(lm.after_school)
summary(lm.after_school)$coef[2]

# bootstrapping function 
storage <- rep (NA, 10000)
for (i in (1:10000)){
    temporary_lm= lm(MATH_SCORE~ TREATMENT, data=after_school[sample(1:nrow(after_school), nrow(after_school), replace=T),])
    storage[i] <- temporary_lm$coef[2]
}
head(storage)


# plot histogram
hist(storage, 20,
    xlab = "Coefficient of treatment",
    ylab = "Frequency",
    main = "Histogram of coefficient of treatment")

# simulated confint vs regression std error

#mean(storage)
quantile(storage, probs=c(0.025, 0.975))
#summary(lm_afterschool)$coef[2]
confint(lm.after_school, level = 0.95)

summary(storage)


# plot histogram
hist(storage, 20,
    xlab = "Coefficient of treatment",
    ylab = "Frequency",
    main = "Histogram of coefficient of treatment")
    
 
### Fourth Question ###
afterSchool <- after_school[complete.cases(after_school),]
set.seed(123)
library(boot)
bootstrap <- function(y, pred_y){
    storage = NA
    for (i in 1:10000){
        index = sample(1:length(y), length(y), replace = TRUE)
        storage[i] = 1-((sum((y[index]-pred_y[index])**2))/sum((y[index]- mean(y[index])**2)))    
    }
    
    return(mean(storage))
}

bootstrap(afterSchool$MATH_SCORE, predict(lm.after_school))


### Fifth Question ###
library (boot)
newdata <- read.csv(url("https://tinyurl.com/yx8tqf3k"))
set.seed(12345)
newdata_test <- sample(1:length(newdata$age),2000, replace=FALSE)
newdata_te <- newdata[newdata_test,]
newdata_tr <- newdata[-newdata_test,]

# generalized linear model, binomial
glm_1 <- glm (treat~ age+education+nodegree+black+hispanic+married, data=newdata_tr)
glm_2 <- glm (treat~ age+education+nodegree+black+married, data=newdata_tr)
glm_3 <- glm (treat~ age+education, data=newdata_tr)
glm_4 <- glm (treat~ age+education+married, data=newdata_tr)
glm_5 <- glm (treat~ age+education+hispanic+married, data=newdata_tr)

# cross -validation
cv_err_1 <- cv.glm(newdata_tr, glm_1)
cv_err_2 <- cv.glm(newdata_tr, glm_2)
cv_err_3 <- cv.glm(newdata_tr, glm_3)
cv_err_4 <- cv.glm(newdata_tr, glm_4)
cv_err_5 <- cv.glm(newdata_tr, glm_5)

# test set error
mse1 <- mean((newdata_te$treat-predict(glm_1, newdata_te))**2)
mse2 <- mean((newdata_te$treat-predict(glm_2, newdata_te))**2)
mse3 <- mean((newdata_te$treat-predict(glm_3, newdata_te))**2)
mse4 <- mean((newdata_te$treat-predict(glm_4, newdata_te))**2)
mse5 <- mean((newdata_te$treat-predict(glm_5, newdata_te))**2)

# table

table_final <- t(data.frame ("MSE for Testing Data"=c(mse1,mse2, mse3, mse4, mse5), "CrossValidation Error"=c(c(cv_err_1$delta[1],cv_err_2$delta[1],cv_err_3$delta[1],cv_err_4$delta[1],cv_err_5$delta[1]))))
colnames(table_final) <- c("GLM1 - age + education + nodegree + black + hispanic + married", "GLM2 - age + education + nodegree + black + married", "GLM3 - age + education", "GLM4 - age + education+ married ", "GLM5 - age + education + hispanic + married ")
table_final


