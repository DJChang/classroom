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
barplot(m1)
barplot(m2)


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
par(mai = c(2.5,1,1,1))
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
y = biodata[,1]
xm = apply(x,2,mean)
xv = apply(x,2,sd)
x = sweep(x, 2, xm, "-")
x = sweep(x, 2, xv, "/")

cdata = as.data.frame(cbind(y,x))
names(cdata) = names(biodata)
library(MASS)
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
BIC(fit_init)

# forward selection
model_s = step(fit_init, 
               scope=list(lower=formula(fit_init),
                          upper=formula(fit_full)),
               direction = 'forward')
summary(model_s)
BIC(model_s)

# glmnet
x = as.matrix(cdata[,-1])
class(x)
y = as.integer(cdata$diagnosis)-1
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
  v[i] = -2*loglik + log(n)*fit$df[i]
}
fit$df[which.min(v)]
barplot(fit$beta[,which.min(v)],las = 2 )
min(v)



