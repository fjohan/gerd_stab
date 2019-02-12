library(stargazer)
library(nnet)
library(effects)
briana<-read.table('~/RStudioProjects/gerd_stab/jlwdist_for_r_removed2.txt', header=T)
briana$gjl <- relevel(briana$gjl, ref = "n")
briana$human <- relevel(briana$human, ref = "not")
briana$mass <- relevel(briana$mass, ref = "not")
briana$animals <- relevel(briana$animals, ref = "not")
briana$time <- relevel(briana$time, ref = "not")
briana$deriv <- relevel(briana$deriv, ref = "not")
briana$disyllabic <- relevel(briana$disyllabic, ref = "not")

with(briana, table(disyllabic, gjl))
test.deriv <- multinom(gjl ~ deriv, data = briana) 
test.rrr = pmin(exp(coef(test)),99)
stargazer(test, type="text", coef=list(test.rrr), p.auto=FALSE)
plot(effect("disyllabic",test))

test <- multinom(gjl ~ human + mass + animals + time + deriv + disyllabic, data = briana) 

library(rpart)
library(rpart.plot)
#set.seed(100)
trainingRows <- sample(1:nrow(briana), 0.5*nrow(briana))
training <- briana[trainingRows, ]
test <- briana[-trainingRows, ]
form <- as.formula(gjl ~ human + mass + animals + time + deriv + disyllabic)
tree.2 <- rpart(form,training,control=rpart.control(minsplit = 10))
predicted_class <- predict(tree.2, test, type="class")
table(predicted_class, test$gjl)
mean(as.character(predicted_class) == as.character(test$gjl))
rpart.plot(tree.2)
#briana[as.character(predicted_class) == as.character(test$gjl),] # correct
#briana[as.character(predicted_class) != as.character(test$gjl),] # incorrect

