#install.packages('FNN')
#install.packages('chemometrics')
#install.packages('class')
library(rpart)
library(rpart.plot)
#data()

library(FNN)
library(class)
library(chemometrics)
set.seed(1)
x <- sort(rnorm(100))
y<- 3+x^2 + rnorm(100)
plot(x, y, pch = 20)
fit <- lm(y~x)
abline(a = fit$coefficients[1], b = fit$coefficients[2], col = 'blue'  )
ytrue <- 3+ x^2
lines(x, ytrue , lty = 2, col = 'black')




tmp <- data.frame(y = y, x = x)
m1 <-rpart(y~., data = tmp)
rpart.plot(m1, type = 2, fallen.leaves = TRUE)


x<-seq(-3,3 ,length = 100) 
tx <- data.frame(x = x)
fit.y<-predict(m1, newdata = tx)
plot(x, y, pch = 20)
lines(x, fit.y, col = 'blue')

# impurity measure
plot(x, y, pch = 20)
abline(v = 0 , lty = 2)
h1 = mean(y[x<0])
h2 = mean(y[x>=0])
lines( rbind(c(-4,h1), c(0,h1)), lty = 2, col = 'red' )
lines( rbind(c(0,h2), c(4,h2)), lty = 2, col = 'red' )


plot(x, y, pch = 20)
abline(v = 1.4 , lty = 2)
h1 = mean(y[x<1.4])
h2 = mean(y[x>=1.4])
lines( rbind(c(-4,h1), c(1.4,h1)), lty = 2, col = 'red' )
lines( rbind(c(1.4,h2), c(4,h2)), lty = 2, col = 'red' )


svec = seq(-2,2, length = 100)
mse = c()
i = 1
for ( i in 1:100)
{
  ty1<- y[x<svec[i]]
  ty2<- y[x>=svec[i]]
  if (length(ty1) == 0  | length(ty2) == 0 ) next
  if (length(ty2) == 0 ) ty2 = 0
  mse[i] <-var(ty1)*length(ty1) + var(ty2)*length(ty2)
}

plot(x, mse)

##################################
# classification

library(MASS)
set.seed(1)
x1<-mvrnorm(n = 50, mu = c(1,1), Sigma = matrix(c( 1,0.5,0.5,1), 2,2))

x2.1<-mvrnorm(n = 25, mu = c(0,0), Sigma = matrix(c( 1,-0.5,-0.5,1), 2,2))
x2.2<-mvrnorm(n = 25, mu = c(3,3), Sigma = matrix(c( 1,-0.5,-0.5,1), 2,2))
x2 = rbind(x2.1, x2.2)
x<- rbind(x1, x2)
y<- c(rep(1,50), rep(0,50))
plot(x1, col = 'red', pch = 20, xlim = c(-2,5), ylim = c(-2,5), xlab= 'x1', ylab = 'x2')
points(x2, col= 'blue', pch = 20)
legend('bottomright', legend = c('Y=1', 'Y=0'), col = c('red', 'blue'),
       pch = c(20,20))

jdata <- data.frame(y = y, x = x)

m2 <- rpart(y~., data = jdata, method = 'class')
rpart.plot(m2)
tx <- NULL
i = 1
for (i in 1:70)
{
  tmp <-cbind( rep(-2+0.1*(i-1),70), seq(-2,5, length  = 70))
  tx <- rbind(tx, tmp)
}
tx<- as.data.frame(tx)
names(tx)<- names(jdata)[-1]
yhat <- predict(m2, newdata = tx)
yhat <- yhat[,2]
z<- t(matrix(yhat, 70,70))
contour(x = seq(-2,5, length = 70) , y = seq(-2,5, length = 70), z)

##

setwd("~/GitHub/classroom/bio")
biodata = read.csv("./data/data.csv")
biodata$X <- NULL
id = biodata[,1]
biodata = biodata[,-1]
fit = rpart(diagnosis~., data = biodata, method = 'anova')
fit.prune = prune.rpart(fit,  cp = 0.001)
predict(fit.prune)
rpart.plot(fit)
rpart.plot(fit.prune)

control=rpart.control(minsplit=2, minbucket=1, cp=0)
fit = rpart(diagnosis~., data = biodata, method = 'anova',
            control = control)
rpart.plot(fit)
predict(fit)
fit.prune = prune.rpart(fit,  cp = 0.005)
rpart.plot(fit.prune)

library(randomForest)
fit = randomForest(diagnosis~., data = biodata, importance = T)
plot(fit)
varImpPlot(fit)

