###Machine Learning Final Project###
###Spotify Song Attributes###
library(ISLR)
library(MASS)
library(class)
require(boot)
rm(list = ls())
data <- read.csv(file = "data.csv", header = T)
n <- nrow(data)
#Logistic Regression
glm.fit <- glm(target ~ acousticness+danceability+duration_ms+energy+instrumentalness+key+liveness+loudness+mode+speechiness+tempo+time_signature+valence,
               family = binomial, data = data)
glm.probs <- predict(glm.fit, type = "response")
glm.pred <- rep(0,n)
glm.pred[glm.probs > 0.5] = 1
mytable <- table(data$target, glm.pred)
mytable
#Overall fraction of correct prediction
mean(glm.pred == data$target)
#Overall error rate
mean(glm.pred != data$target)
#Type I error
type1ErrorRate <- mytable[1, 2] / sum(mytable[1, ])
type1ErrorRate
#Type II error
type2ErrorRate <- mytable[2, 1] / sum(mytable[2, ])
type2ErrorRate
#Power of the model
Power <- mytable[2, 2] / sum(mytable[2, ])
Power
#Precision of the model
precision <- mytable["1", "1"] / sum(mytable[, "1"])
precision

#Predictors that are significant
coef(glm.fit)
summary(glm.fit)

summary(glm.fit)$coefficients[,4]

which(summary(glm.fit)$coefficients[,4] < 0.05)

#Logistic Regression Using Statistically Significant Predictors
glm.fit2 <- glm(target ~ acousticness+danceability+duration_ms+instrumentalness+loudness+speechiness+tempo+valence,
                family = binomial, data = data)
glm.probs2 <- predict(glm.fit2, type = "response")
glm.pred2 <- rep(0,n)
glm.pred2[glm.probs2 > 0.5] = 1
mytable2 <- table(data$target, glm.pred2)
mytable2
#Overall fraction of correct prediction
mean(glm.pred2 == data$target)
#Overall error rate
mean(glm.pred2 != data$target)
#Type I error
type1ErrorRate2 <- mytable2[1, 2] / sum(mytable2[1, ])
type1ErrorRate2
#Type II error
type2ErrorRate2 <- mytable2[2, 1] / sum(mytable2[2, ])
type2ErrorRate2
#Power of the model
Power2 <- mytable2[2, 2] / sum(mytable2[2, ])
Power2
#Precision of the model
precision2 <- mytable2["1", "1"] / sum(mytable2[, "1"])
precision2


###LDA###
lda.fit <- lda(target ~ acousticness+danceability+duration_ms+instrumentalness+loudness+speechiness+tempo+valence,
               data = data)
lda.pred <- predict(lda.fit, data)
lda.class <- lda.pred$class
mytable3 <- table(data$target,lda.class)
mytable3
#Overall fraction of correct prediction
mean(lda.class == data$target)
#Overall error rate
mean(lda.class != data$target)
#Type I error
type1FalsePositiveErrorRate <- mytable[1, 2] / sum(mytable[1, ])
type1FalsePositiveErrorRate
#Type II error
type2FalseNegativeErrorRate <- mytable[2, 1] / sum(mytable[2, ])
type2FalseNegativeErrorRate
#Power of the model
sensitivityPowerRecall <- mytable[2, 2] / sum(mytable[2, ])
sensitivityPowerRecall
#Precision of the model
precision <- mytable["Up", "Up"] / sum(mytable[, "Up"])
precision

###QDA###
qda.fit <- qda(target ~ acousticness+danceability+duration_ms+instrumentalness+loudness+speechiness+tempo+valence,
               data = data)
qda.pred <- predict(qda.fit, data)
qda.class <- qda.pred$class
mytable4 <- table(data$target,qda.class)
mytable4
#Overall fraction of correct prediction
mean(qda.class == data$target)
#Overall error rate
mean(qda.class != data$target)
