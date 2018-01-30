library(hdi)
library(glmnet)

setwd("C:/Users/Farees Patel/Downloads")

data<- read.csv("blogData_train.csv", header = FALSE)

#try<- lasso.cv(as.matrix(data[,1:280]), data[,281],nfolds = 10)
test<- read.csv("blogData_test-2012.03.31.01_00.csv", header=FALSE)

glmnet1<-cv.glmnet(as.matrix(data[,1:280]), data[,281],type.measure='mse',nfolds=10,alpha=0)

fit<- glmnet(as.matrix(data[,1:280]), data[,281], alpha = 1, lambda = 100 )

pred<- predict.cv.glmnet(glmnet1,as.matrix(test[,1:280]))

pred<- predict(fit,as.matrix(test[,1:280]))

sum((pred-test[,281])^2)^0.5


cv.ridge=cv.glmnet(as.matrix(data[,1:280]), data[,281],alpha=0)
plot(cv.ridge)


fit_lm<- lm(V281~., data=data)
pred_lm<- predict(fit_lm,test[,1:280])
rmse(pred_lm, test[,281])

fit_ridge<- glmnet(as.matrix(data[,1:280]), data[,281],alpha=0)
pred_ridge<- predict(fit_ridge,as.matrix(test[,1:280]))
rmse(pred_ridge, test[,281])

fit_lasso<- glmnet(as.matrix(data[,1:280]), data[,281],alpha=1)
pred_lasso<- predct(fit_lasso,as.matrix(test[,1:280]))
rmse(pred_lasso, test[,281])

sum((pred-test[,281])^2)^0.5



rmse= sqrt(apply((pred_ridge-test[,281])^2,2,mean))
plot(log(fit_ridge$lambda),rmse,type="b",xlab="Log(lambda)")
lam.best=fit_ridge$lambda[order(rmse)[1]]
lam.best
coef(fit_ridge,s=lam.best)

