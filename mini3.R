library(glmnet)
library(ggplot2)
library(gridExtra)
library(MASS)
library(anytime)
library(leaps)
#
# Preparing data as in lab 2 to fit model
# Using the elastic net to estimate.
#
#
houseDat = read.csv("http://www.math.chalmers.se/Stat/Grundutb/GU/MSG500/A18/kc_house_data.csv")
houseDat = houseDat[,-c(9, 10, 11, 12, 14)]

houseDat$sqft_lot = log(houseDat$sqft_lot)
houseDat$sqft_lot15 = log(houseDat$sqft_lot15)

houseDat$yr_renovated[(houseDat$yr_renovated > 0 & houseDat$yr_renovated < 1980)] = 1
houseDat$yr_renovated[(houseDat$yr_renovated >= 1980 & houseDat$yr_renovated < 1990)] = 2
houseDat$yr_renovated[(houseDat$yr_renovated >= 1990 & houseDat$yr_renovated < 2000)] = 3
houseDat$yr_renovated[(houseDat$yr_renovated >= 2000 & houseDat$yr_renovated < 2010)] = 4
houseDat$yr_renovated[(houseDat$yr_renovated >= 2010)] = 5

houseDat$date = anytime(houseDat$date)

set.seed(1337)

samp = sample(seq(1, dim(houseDat)[1]), 500)
houseDat.train = houseDat[samp,]

houseDat.remain = houseDat[-samp,]

samp = sample(seq(1,dim(houseDat.remain)[1]), 500)
houseDat.test = houseDat.remain[samp,]

row.names(houseDat.train) = 1:500
row.names(houseDat.test) = 501:1000

load("elasticModel.RData") # Model from lab2 that I run with

houseDat.train.x = data.matrix(houseDat.train[,-3])
houseDat.train.y = log(houseDat.train[,3])
houseDat.test.x = data.matrix(houseDat.test[,-3])
houseDat.test.y = log(houseDat.test[,3])

elastic.prediction = predict(elastic.model, newx = houseDat.test.x, type = "link")
#
# GGPLOT OF PREDICTION VS ACTUAL VALUES HERE
#

dat.tmp = data.frame(cbind(elastic.prediction, houseDat.test.y))
p1 = ggplot(data = dat.tmp, aes(s0, houseDat.test.y, color = houseDat.test.y)) +
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  theme(legend.position = "none") +
  xlab("Predicted") +
  ylab("Actual") +
  ggtitle("Actual vs Predicted for lambda.1se")

p2 = ggplot(data = dat.tmp, aes(s3, houseDat.test.y, color = houseDat.test.y)) +
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  theme(legend.position = "none") +
  xlab("Predicted") +
  ylab("Actual") +
  ggtitle("Actual vs Predicted for lambda.min")

elastic.rMSE.1se = sum((houseDat.test.y - elastic.prediction[,1])^2)/length(houseDat.test.y)
elastic.rMSE.min = sum((houseDat.test.y - elastic.prediction[,4])^2)/length(houseDat.test.y)
#
# 
# 
# 
#
# How about some model fit with backward and forward selections?
#
#

load("naiveModels.RData") # The naive models from lab 2 that I run with

forward.prediction = predict(forward.model, newdata = houseDat.test[,-3])
backward.prediction = predict(backward.model, newdata = houseDat.test[,-3])
both.prediction = predict(both.model, newdata = houseDat.test[,-3])

dat.tmp = data.frame(cbind(forward.prediction, houseDat.test.y))
p3 = ggplot(data = dat.tmp, aes(forward.prediction, houseDat.test.y, color = houseDat.test.y)) +
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  theme(legend.position = "none") +
  xlab("Predicted") +
  ylab("Actual") +
  ggtitle("Actual vs Predicted for stepwise models")

#
# All three predicted vs actual plots
#

grid.arrange(p1,p2, p3, ncol = 1)

forward.rMSE = sum((houseDat.test.y - forward.prediction)^2)/length(houseDat.test.y)
backward.rMSE = sqrt(sum((houseDat.test.y - backward.prediction)^2)/length(houseDat.test.y))
both.rMSE = sqrt(sum((houseDat.test.y - both.prediction)^2)/length(houseDat.test.y))
#
# All three the same. It's clear that they have worse rMSE than the elastic regularization
# But better than the .1se with more LASSO behaviour. Although the difference is small (relatively)
# and the elastic "LASSO" removes more variables (8 vars vs 11 in forward/backward/both).
#
#
# Let's now try to do exhaustive modelling. Testing all possible models for this data.
# We'll fit all of them and use to predict to calculate pMSE on test set.
# Afterwards we'll take the best models and performa a cross validation 
# On the whole data.
#
regsub.model = regsubsets(log(price)~., data = houseDat.train, nvmax = 16, intercept = TRUE)

#
#
# pMSE on test data and train data (RSS/n)
#
#
regsub.model.sum = summary(regsub.model)$which
rMSE.vec = rep(NA, dim(regsub.model.sum)[1])
train.rMSE.vec = rMSE.vec

for(i in 1:dim(regsub.model.sum)[1]){
  mm = lm(houseDat.train.y~houseDat.train.x[,regsub.model.sum[i,-1]])
  mm.pred = sum((houseDat.test.y-
                   cbind(rep(1,500),houseDat.test.x[,regsub.model.sum[i,-1]])%*%
                   mm$coef)^2)/length(houseDat.test.y)
  
  mm.pred.train = sum((houseDat.train.y-
                   cbind(rep(1,500),houseDat.train.x[,regsub.model.sum[i,-1]])%*%
                   mm$coef)^2)/length(houseDat.train.y)
  
  rMSE.vec[i] = mm.pred
  train.rMSE.vec[i] = mm.pred.train
}

ggplot(data = data.frame(rMSE.vec), aes(1:15, rMSE.vec)) + 
  geom_point() +
  geom_line()
#
#
# Larger models have better pMSE. Not strange, we don't correct for model size. 
# How about using all data and run 10 fold cross validation wiht these best models?
#
#
n.folds <- 10
folds.i <- sample(rep(1:n.folds, length.out = dim(houseDat)[1]))
rMSE.mat = matrix(data = NA, nrow = n.folds, ncol = dim(regsub.model.sum)[1])

for(i in 1:n.folds){
  train.dat.x = data.matrix(houseDat[which(folds.i != i),-3])
  train.dat.y = log(houseDat[which(folds.i != i), 3])
  test.dat.x = data.matrix(houseDat[which(folds.i == i),-3])
  test.dat.y = log(houseDat[which(folds.i == i), 3])
  
  for(j in 1:dim(regsub.model.sum)[1]){
    mm = lm(train.dat.y~train.dat.x[,regsub.model.sum[j,-1]])
    mm.pred = sum((test.dat.y-
                     cbind(rep(1,dim(test.dat.x)[1]), test.dat.x[,regsub.model.sum[j,-1]])%*%
                     mm$coef)^2)/length(test.dat.y)
    rMSE.mat[i,j] = mm.pred
  }
}
#
#
# We'll take the average of the prediction MSE's over the folds
#
#
avg.pMSE = apply(rMSE.mat, 2, mean)
sd.pMSE = apply(rMSE.mat, 2, sd)/sqrt(n.folds)
regsub.model.sum[which.min(avg.pMSE),]   # with 13 degrees of freedom

dat.tmp = data.frame(cbind(avg.pMSE, sd.pMSE, rMSE.vec, train.rMSE.vec))
ggplot(data = dat.tmp, aes(1:15, avg.pMSE, color = "Cross validation pMSE")) +
  geom_point(size = 3) +
  geom_line(size = 1) +
  geom_point(aes(1:15, rMSE.vec, color = "Original test pMSE")) +
  geom_line(aes(1:15, rMSE.vec, color = "Original test pMSE"), linetype = "dashed", size = 1) +
  geom_line(aes(1:15, train.rMSE.vec, color = "Training pMSE"), size = 1) +
  geom_vline(xintercept = which.min(avg.pMSE), color = "#CC6666", size = 1, linetype = "dotted") +
  geom_vline(xintercept = which.min(rMSE.vec), color = "#3366FF", size = 1, linetype = "dotted") +
  scale_color_manual(values=c("#CC6666", "#3366FF", "#66CC33")) +
  xlab("Nr of variables") +
  ylab("Prediction errro") +
  ggtitle("Prediction error, different model size") +
  guides(color = guide_legend(title="")) +
  scale_x_continuous(breaks = 1:15) +
  geom_text(x = which.min(avg.pMSE), y= 0.12, 
                        label="CV min pMSE", 
                        colour="#CC6666", hjust = 1.25, size = 3.5, angle = 45) +
  geom_text(x = which.min(rMSE.vec), y= 0.14, 
            label="test min pMSE", 
            colour="#3366FF", hjust = 1.20, size = 3.5, angle = 45)
 
#
#
# Mix in some categorical variables too.
# We wil choose model with cross validation on full data
#
rm(list = ls())
houseDat = read.csv("http://www.math.chalmers.se/Stat/Grundutb/GU/MSG500/A18/kc_house_data.csv")
houseDat = houseDat[,-14]
houseDat$sqft_lot = log(houseDat$sqft_lot)
houseDat$sqft_lot15 = log(houseDat$sqft_lot15)

houseDat$yr_renovated[(houseDat$yr_renovated > 0 & houseDat$yr_renovated < 1980)] = 1
houseDat$yr_renovated[(houseDat$yr_renovated >= 1980 & houseDat$yr_renovated < 1990)] = 2
houseDat$yr_renovated[(houseDat$yr_renovated >= 1990 & houseDat$yr_renovated < 2000)] = 3
houseDat$yr_renovated[(houseDat$yr_renovated >= 2000 & houseDat$yr_renovated < 2010)] = 4
houseDat$yr_renovated[(houseDat$yr_renovated >= 2010)] = 5

houseDat$date = anytime(houseDat$date)

#
# Look at variables before removed:
# waterfront view condition and grade
# We will treat them as factors
#

houseDat$waterfront = factor(houseDat$waterfront)
houseDat$view = factor(houseDat$view)
houseDat$condition = factor(houseDat$condition)

#
# We'll divide grade into three factors as description gives them in lab 1
#

houseDat$grade[(houseDat$grade <= 3)] = 0
houseDat$grade[(houseDat$grade <= 7 & houseDat$grade > 3)] = 1
houseDat$grade[(houseDat$grade <= 13 & houseDat$grade > 7)] = 2

houseDat$grade = factor(houseDat$grade)

#
# Now let's look at some plots
# Also checking out sqft_living as it seems it could do with a log transform
#
#
#
#
# Histograms:
#

p1 = ggplot(data = houseDat, aes(fill = waterfront)) +
  geom_bar(alpha = 0.6) + aes(waterfront) +
  scale_x_discrete(labels = c("No", "Yes"), name = "") +
  scale_fill_discrete(labels = c("No", "Yes")) +
  stat_count(aes(label=..count..), vjust=-0.5, geom="text")
  

p2 = ggplot(data = houseDat, aes(fill = view)) +
  geom_bar(alpha = 0.6) + aes(view) +
  stat_count(aes(label=..count..), vjust=-0.5, geom="text") +
  scale_x_discrete(name = "")

p3 = ggplot(data = houseDat, aes(fill = condition)) +
  geom_bar(alpha = 0.6) + aes(condition) +
  stat_count(aes(label=..count..), vjust=-0.5, geom="text") +
  scale_x_discrete(name = "")

p4 = ggplot(data = houseDat, aes(fill = grade)) +
  geom_bar(alpha = 0.6) + aes(grade) +
  stat_count(aes(label=..count..), vjust=-0.5, geom="text") +
  scale_x_discrete(labels = c("Bad", "Medium", "Good"), name = "") +
  scale_fill_discrete(labels = c("Bad", "Medium", "Good"))


grid.arrange(p1,p2,p3,p4, ncol = 2)
#
# Box plots:
#

p1 = ggplot(data = houseDat, aes(fill = waterfront)) +
  geom_boxplot(aes(waterfront, log(price)), alpha = 0.6) +
  scale_x_discrete(labels = c("No", "Yes"), name = "") +
  scale_fill_discrete(labels = c("No", "Yes")) 
 

p2 = ggplot(data = houseDat, aes(fill = view)) +
  geom_boxplot(aes(view, log(price)), alpha = 0.6) +
  scale_x_discrete(name = "")

p3 = ggplot(data = houseDat, aes(fill = condition)) +
  geom_boxplot(aes(condition, log(price)), alpha = 0.6) +
  scale_x_discrete(name = "")

p4 = ggplot(data = houseDat, aes(fill = grade)) +
  geom_boxplot(aes(grade, log(price)), alpha = 0.6) +
  scale_x_discrete(labels = c("Bad", "Medium", "Good"), name = "") +
  scale_fill_discrete(labels = c("Bad", "Medium", "Good"))

grid.arrange(p1,p2,p3,p4, ncol = 2)

#
# Uneven counts in most cases. Not much to do about that however
# Regression with categorical predictors are not much different 
# from with continuous ones, we proceed as before.
#
# Question is if there are any interactions present in the data?
# We can perhaps use randomForrest to detect!
#
#
library(randomForest)
library(forestFloor)

houseDat$date = as.numeric(houseDat$date)
rf.obj = randomForest(x = houseDat[,-3], y = log(houseDat[,3]), 
                      keep.inbag = TRUE, 
                      importance = TRUE,
                      sampsize = 1500)  #Had to decrease size or computer froze

ff.fit = forestFloor(rf.fit = rf.obj, X = houseDat[,-3])

plot(ff.fit, plot_seq = 1:6, orderByImportance = TRUE)
plot(ff.fit, plot_seq = 7:12, orderByImportance = FALSE)
plot(ff.fit, plot_seq = 13:18, orderByImportance = FALSE)
plot(ff.fit, plot_seq = 19:19, orderByImportance = FALSE)


Col=fcol(ff.fit,17,orderByImportance=FALSE) #Color by longitude since it looks a bit fishy
plot(ff.fit, plot_seq = 1:6, col = Col, plot_GOF = TRUE)
plot(ff.fit, plot_seq = 7:12, col = Col, plot_GOF = TRUE)
plot(ff.fit, plot_seq = 13:18, col = Col, plot_GOF = TRUE)

#
# There don't seem to be any major interactions going on judging 
# from these plots. Neither does knowledge about the data warrant
# any great suspicions of interactions existing.
#
# We could however notice that sqft_living and sqft_living15 did
# look to have a more polynomial relation to price, and some uneven variance
# Let's see if we can't fix with a log transform
#

p1 = ggplot(data = houseDat, aes(sqft_living, log(price), color = log(price))) +
  geom_point() + geom_smooth() +
  theme(legend.position = "none")

p2 = ggplot(data = houseDat, aes(sqft_living15, log(price), color = log(price))) +
  geom_point() + geom_smooth() +
  theme(legend.position = "none")

p3 = ggplot(data = houseDat, aes(log(sqft_living), log(price), color = log(price))) +
  geom_point() + geom_smooth() +
  theme(legend.position = "none")

p4 = ggplot(data = houseDat, aes(log(sqft_living15), log(price), color = log(price))) +
  geom_point() + geom_smooth() +
  theme(legend.position = "none")

grid.arrange(p1,p2,p3,p4, ncol = 2)
#
# While it's debatable if sqft_living15 actually fare better after transformation
# it does seem like we should keep the transformation of sqft_living!
#
houseDat$sqft_living = log(houseDat$sqft_living)
houseDat$sqft_above = log(houseDat$sqft_above)
#
# We'll continue with the cross validaiton of the linear models
# just as we did before when not including the categorical predictors.
#
# Just as before we use a training set of 500 observations drawn randomly
# to fit all models with regsubset, then we'll use the best models for the 
# cross validation on full data.
#

set.seed(123)

samp = sample(seq(1, dim(houseDat)[1]), 500)
houseDat.train = houseDat[samp,]

houseDat.remain = houseDat[-samp,]

samp = sample(seq(1,dim(houseDat.remain)[1]), 500)
houseDat.test = houseDat.remain[samp,]

row.names(houseDat.train) = 1:500
row.names(houseDat.test) = 501:1000

#
# Find best models of all sizes on random training set
#
regsub.model = regsubsets(log(price)~., data = houseDat.train, nvmax = 2000, intercept = TRUE)
regsub.model.sum = summary(regsub.model)$which
#
# Start cross validation
#
library(dummies)
#
# Need to dummy things up to work with how regsubset works with factors
#
train.dat.x = dummy.data.frame(houseDat[,-3])
train.dat.x = subset(train.dat.x, select = -c(waterfront0,view0,condition1,grade0))
train.dat.y = log(houseDat[,3])

test.dat.x = train.dat.x
test.dat.y = train.dat.y

n.folds <- 10
folds.i <- sample(rep(1:n.folds, length.out = dim(houseDat)[1]))
rMSE.mat = matrix(data = NA, nrow = n.folds, ncol = dim(regsub.model.sum)[1])

for(i in 1:n.folds){
  train.dat.xx = train.dat.x[which(folds.i != i),]
  train.dat.yy = train.dat.y[which(folds.i != i)]
  test.dat.xx = test.dat.x[which(folds.i == i),]
  test.dat.yy = test.dat.y[which(folds.i == i)]
  
  for(j in 1:dim(regsub.model.sum)[1]){
    train.dat.tmp = data.matrix(subset(train.dat.xx, select = as.integer(which(regsub.model.sum[j,-1]==T))))
    test.dat.tmp = data.matrix(subset(test.dat.xx, select = as.integer(which(regsub.model.sum[j,-1]==T))))
    
    mm = lm(train.dat.yy~train.dat.tmp)
    mm.pred = sum((test.dat.yy-(cbind(rep(1,dim(test.dat.tmp)[1]), test.dat.tmp)%*%
                     mm$coef))^2)/length(test.dat.yy)
    rMSE.mat[i,j] = mm.pred
  }
}

avg.pMSE = apply(rMSE.mat, 2, mean)
sd.pMSE = apply(rMSE.mat, 2, sd)/sqrt(n.folds)
min.pMSE = regsub.model.sum[which.min(avg.pMSE),]   # with 13 degrees of freedom

dat.tmp = data.frame(cbind(avg.pMSE, sd.pMSE))
ggplot(data = dat.tmp, aes(1:length(avg.pMSE), avg.pMSE, color = "Cross validation pMSE")) +
  geom_point(size = 3) +
  geom_line(size = 1) +
  geom_vline(xintercept = which.min(avg.pMSE), color = "#CC6666", size = 1, linetype = "dotted") +
  xlab("Nr of variables") +
  ylab("Prediction errro") +
  ggtitle("Prediction error, different model size, categorical included") +
  guides(color = guide_legend(title="")) +
  scale_x_continuous(breaks = 1:length(avg.pMSE)) +
  geom_text(x = which.min(avg.pMSE), y= 0.12, 
            label="CV min pMSE = 0.0698", 
            colour="#CC6666", hjust = 1.25, size = 3.5, angle = 45)
#
#
# Just need to display winning model here
#
#