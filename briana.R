library(ggplot2)
library(ggthemes)

#conflicts2<-read.table('~/Dropbox/scratch/conflicts2.csv', sep=';', header=T)
conflicts2<-read.table('~/RStudioProjects/gerd_stab/conflicts2.csv', sep=';', header=T)
df<-data.frame(table(conflicts2))

df$CFreq<-df$Freq
maxFreq<-max(df$CFreq)
df$Freq<-df$CFreq/maxFreq


ggplot(df, aes(x=Loser,y=Winner))+
    geom_tile(aes(fill=Freq))+
    geom_text(aes(label=CFreq)) +
    #scale_fill_gradient(low="red",high="white")+
    scale_fill_gradient(low = "beige", high = "green",
                        labels = round(seq(0,maxFreq, length.out=5)),
                        guide = guide_legend(
                            label.position = "right"
                        )) +
    geom_text(aes(label=CFreq), data=cbind(aggregate(CFreq~Loser, df, sum), Winner="total")) + 
    geom_text(aes(label=CFreq), data=cbind(aggregate(CFreq~Winner, df, sum), Loser="total")) + 
    #theme(axis.text.x  = element_text(angle=45))+
    scale_x_discrete(limits=c(levels(df$Loser), "total"))+
    scale_y_discrete(limits=c("total", rev(levels(df$Winner)))) + 
    theme_solarized(base_family = "Helvetica")

###

conflicts<-read.table('~/Dropbox/scratch/Conflicts.csv', sep=';', header=T)

conflicts<-data.frame(Rule = conflicts$Rule, Loses.to = conflicts$Loses.to, Losses = conflicts$Losses)

conflicts<-conflicts[complete.cases(conflicts),]
conflicts<-conflicts[conflicts$Losses > 0,]


#conflicts$Losses<-as.integer(conflicts$Losses)


    
geom_text(aes(label=CFreq)) +
    xlim(0.5,5.5) +
    ylim(0.5,5.5) +
    theme_fivethirtyeight() +
    theme(
        axis.title = element_text(),
        legend.position = "right",
        legend.direction = "vertical"
    )


require(foreign)
require(nnet)
require(ggplot2)
require(reshape2)
briana<-read.table('~/RStudioProjects/gerd_stab/jlwdist_for_r_removed2.txt', header=T)
with(briana, table(human, gjl))
with(briana, table(mass, gjl))
with(briana, table(animals, gjl))
with(briana, table(time, gjl))
with(briana, table(deriv, gjl))
with(briana, table(disyllabic, gjl))

#with(ml, do.call(rbind, tapply(write, prog, function(x) c(M = mean(x), SD = sd(x)))))

# null model
test.null <- multinom(gjl ~ 1, data = briana)
ll.null = logLik(test.null)

# full model
test.full <- multinom(gjl ~ human + mass + animals + time + deriv + disyllabic, data = briana)
ll.full = logLik(test.full)

anova(test.null,test.full,test='Chisq')

briana$gjl2 <- relevel(briana$gjl, ref = "n")
test <- multinom(gjl2 ~ human + mass + animals + time + deriv + disyllabic, data = briana) 
#test <- multinom(gjl2 ~ disyllabic, data = briana) 
summary(test)
z <- summary(test)$coefficients/summary(test)$standard.errors
z
p <- (1 - pnorm(abs(z), 0, 1)) * 2
p

exp(coef(test))
head(pp <- fitted(test))


set.seed(300)
trainingRows <- sample(1:nrow(briana), 0.7*nrow(briana))
training <- briana[trainingRows, ]
test <- briana[-trainingRows, ]
training <- briana
test <- briana
multinomModel <- multinom(gjl ~ human + mass + animals + time + deriv + disyllabic, data=training)
#predicted_scores <- predict(multinomModel, test, "probs")
predicted_class <- predict(multinomModel, test)
table(predicted_class, test$gjl)
mean(as.character(predicted_class) != as.character(test$gjl))
mean(as.character(predicted_class) == as.character(test$gjl))

require(rpart)
require(rpart.plot)
set.seed(100)
trainingRows <- sample(1:nrow(briana), 0.7*nrow(briana))
training <- briana[trainingRows, ]
test <- briana[-trainingRows, ]
form <- as.formula(gjl ~ human + mass + animals + time + deriv + disyllabic)
tree.2 <- rpart(form,training)
predicted_class <- predict(tree.2, test, type="class")
table(predicted_class, test$gjl)
mean(as.character(predicted_class) == as.character(test$gjl))
rpart.plot(tree.2)
briana[as.character(predicted_class) == as.character(test$gjl),] # correct
briana[as.character(predicted_class) != as.character(test$gjl),] # incorrect

require(glmnet)

x<-cbind(briana$human,briana$mass,briana$animals,briana$time,briana$deriv,briana$disyllabic)
y<-briana$gjl
fit = glmnet(x, y, family = "multinomial", type.multinomial = "grouped")
plot(fit, xvar = "lambda", label = TRUE, type.coef = "2norm")
plot(fit, xvar = "dev", label = TRUE)
cvfit=cv.glmnet(x, y, family="multinomial", type.multinomial = "grouped", parallel = TRUE)
plot(cvfit)
