rm(list = ls())
gc()
setwd("~/GitHub/classroom/bio")
biodata = read.csv("./data/data.csv")
str(biodata)
table(biodata$id)
biodata$X <- NULL
str(biodata)
table(biodata$diagnosis)

id = biodata[,1]
biodata = biodata[,-1]

summary(biodata[,-1])[3,]
m1 = apply(biodata[,-1], 2, FUN = mean)
m2 = apply(biodata[,-1], 2, FUN = sd)


# fig
par(mai = c(2.5,1,1,1))
barplot(m1, main = 'mean', las = 2)
barplot(m2, main = 'sd', las = 2)


library(dplyr)
btable = biodata %>% group_by(diagnosis) %>%
  summarise_all(.funs = list(mean,sd))
View(t(btable))
idx = biodata$diagnosis=="M"
m = biodata[,-1]
j = 1
r  = c()
for (j in 1:ncol(m))
{
  fit = t.test(m[idx,j], m[!idx,j])
  r[j] = fit$p.value
}
names(r) = names(m)
barplot(r, las = 2)



library(corrplot)
corrmat_full= cor(biodata[,-(1:2)])
corrplot(corrmat_full, method="circle")

corrmat_B= cor(biodata[biodata$diagnosis == "B",-(1:2)])
corrplot(corrmat_B, method="circle")

corrmat_M= cor(biodata[biodata$diagnosis == "M",-(1:2)])
corrplot(corrmat_M, method="circle")
### logistic regression
x = biodata[,-1]
y = as.integer(biodata[,1])-1
cdata = as.data.frame(cbind(y,x))
names(cdata) = names(biodata)
plot(y~radius_mean, data = cdata,
     ylim = c(-0.5, 1.5),
     col = c("lightblue",
             "orange")[biodata[,1]],
     ylab = 'prob')
# linear regression 
fit = lm(y~radius_mean, data= cdata)
abline(a = fit$coefficients[1], 
       b = fit$coefficients[2], 
       col ='blue', lwd = 1.5)
# logistic regression with a single predictor
library(MASS)
fit = glm(y~radius_mean, data= cdata,
          family = binomial)
rb = sort(cdata$radius_mean)
yhat = sort(predict(fit, type = 'response'))
plot(y~radius_mean, data = cdata,
     ylim = c(-0.5, 1.5),
     col = c("lightblue",
             "orange")[biodata[,1]],
     ylab = 'prob')
lines(rb, yhat, col = 'blue', lwd=1.5)

## coefficients
fit$coefficients

# x%*%beta
predict(fit, type = 'link')
## yhat
predict(fit, type = 'response')

# measure the performance
# accuracy
predy = as.integer( predict(fit, type = 'response') > 0.5 )
mean(predy == y)

# TP and FP
rtable = data.frame(pred = predy, ground_truth = y)
xtabs( ~ pred+ ground_truth, data = rtable)

a = xtabs( ~ pred+ ground_truth, data = rtable)
prop.table(a, margin = 1)
prop.table(a, margin = 2)

# AUC
library(pROC)
score = predict(fit, type = 'response')
y = cdata$diagnosis
roc.fit =roc(y, score, ci = T)
plot( x = 1-roc.fit$specificities, 
      y = roc.fit$sensitivities,
      type = 'l', xlab = 'FP rate', ylab = 'TP rate')
roc.fit



# logistic regression with multiple predictors
fit = glm(y~radius_mean + radius_se, data= cdata,
          family = binomial)
fit
predict(fit, type = 'response')


# standardization
xm = apply(x,2,mean)
xv = apply(x,2,sd)
x = sweep(x, 2, xm, "-")
x = sweep(x, 2, xv, "/")

cdata = as.data.frame(cbind(y,x))
names(cdata) = names(biodata)

# model selection

# full model
fit_full=glm(diagnosis~.,  data = cdata, family = binomial())
barplot(fit_full$coefficients, las = 2)
# AIC
AIC(fit_full)
# BIC
BIC(fit_full)

# the simplest model
fit_init =glm(diagnosis~1,  
        data = cdata, family = binomial())
AIC(fit_init)

# forward selection
model_s = step(fit_init, 
               scope=list(lower=formula(fit_init),
                          upper=formula(fit_full)),
               direction = 'forward')
summary(model_s)
AIC(model_s)

# glmnet
library(glmnet)
x = as.matrix(cdata[,-1])
class(x)
y = as.integer(cdata$diagnosis)
fit <- glmnet(x, y, family = "binomial", standardize = T) 
plot(fit)
# model selection by cv
fit_cv = cv.glmnet(x,as.integer(y), nfold = 5)
plot(fit_cv)
fit_cv$lambda
i = which.min(fit_cv$cvm)
fit_re <- glmnet(x, y, family = "binomial",
              lambda = fit_cv$lambda[i])
barplot(as.numeric(fit_re$beta), 
        names = names(cdata)[-1], 
        las = 2)

# model selection by BIC
pred = predict(fit, x)
n = nrow(x)
v = c()
for ( i in 1:fit$dim[2])
{
  xb = pred[,i]
  loglik = sum(y*xb  - log( 1 + exp(xb)))
  v[i] = -2*loglik + 2*fit$df[i]
}
fit$df[which.min(v)]
barplot(fit$beta[,which.min(v)],las = 2 )
min(v)

# compare the Model performances
# prediction error
v1 = v2 = c()
for (iter in 1:50)
{
  cat("iter::", iter, '\n')
  idx = sample(1:n, trunc(n*0.7))
  cdata.tr = cdata[idx,]
  cdata.te = cdata[-idx,]
  # forward selection
  fit_init =glm(diagnosis~1,  
          data = cdata.tr, family = binomial())
  fit_full =glm(diagnosis~.,  
          data = cdata.tr, family = binomial())
  model_s = step(fit_init, 
                 scope=list(lower=formula(fit_init),
                            upper=formula(fit_full)),
                 direction = 'forward',
                 trace = 0)
  pred = predict(model_s, cdata.te, type = 'response')
  v1[iter] = mean(as.integer(pred>0.5)  == cdata.te$diagnosis)
  
  # lasso
  x = as.matrix(cdata.tr[,-1])
  y = as.integer(cdata.tr$diagnosis)
  # model selection by cv
  fit_cv = cv.glmnet(x,as.integer(y), nfold = 5)
  i = which.min(fit_cv$cvm)
  fit <- glmnet(x, y, family = "binomial", standardize = T,
                lambda = fit_cv$lambda[i]) 
  predx = as.matrix(cdata.te[,-1])
  predy = as.matrix(cdata.te[,1])
  pred = predict(fit, predx, type = 'response')
  v2[iter] = mean(as.integer(pred>0.5)  == cdata.te$diagnosis)
}
boxplot(v1, v2, main = 'accuracy plot',
        names = c('FS','Lasso'), 
        col = c("orange","lightblue"))
  

