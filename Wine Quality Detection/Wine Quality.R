white = read.csv("D:/VIT/Fourth Sem/Information Visualization/Dataset/wineQualityWhites.csv")
View(white)
head(white)
str(white)
sum(is.na(white))
#attach(white)

#Logistic Regression
white$quality=ifelse(test=white$quality >= 6,yes = "Good",no = "Bad")
white$quality=as.factor(white$quality)
xtabs(~quality,data=white)


glm.fit=glm( quality ~ ., white,family="binomial")
summary(glm.fit)
glm.probs=predict(glm.fit,type="response")
glm.pred=rep("Bad",4898)
glm.pred[glm.probs>.5]="Good"
table(glm.pred,white$quality)
(814+2860)/4898
mean(glm.pred==white$quality)

#important factors deemed to be volatile.acidity, residual.sugar, free.sulfur.dioxide, density, pH, sulphates, alcohol



train=white[1:4500,]
test=white[4501:4898,]
dim(train)
dim(test)

glm.fit2=glm(quality~volatile.acidity+residual.sugar+free.sulfur.dioxide+density+pH+sulphates+alcohol,data=train,family = "binomial")
summary(glm.fit2)
dim(test)
glm.probs=predict(glm.fit,test,type="response")
glm.pred=rep("Bad",398)
glm.pred[glm.probs>.5]="Good"
table(glm.pred,test$quality)
(274+33)/398
mean(glm.pred==test$quality)


#LDA
library(MASS)
lda.fit=lda(quality~volatile.acidity+residual.sugar+free.sulfur.dioxide+density+pH+sulphates+alcohol,data=train)
lda.fit
##plot(lda.fit)
lda.pred=predict(lda.fit, test)
names(lda.pred)
length(lda.pred)
lda.class=lda.pred$class
length(lda.class)
table(lda.pred$class,test$quality)
mean(lda.pred$class==test$quality)
sum(lda.pred$posterior[,1]>=.5)
sum(lda.pred$posterior[,1]<.5)
lda.pred$posterior[1:20,1]
lda.class[1:20]
sum(lda.pred$posterior[,1]>.9)
lda.pred


#QDA  
qda.fit=qda(quality~volatile.acidity+residual.sugar+free.sulfur.dioxide+density+pH+sulphates+alcohol,data=train)
qda.fit
qda.class=predict(qda.fit,test)$class
table(qda.class,test$quality)
mean(qda.class==test$quality)

#THIRDDDD

#KNN
library(class)
train.X=cbind(train$volatile.acidity,train$residual.sugar,train$free.sulfur.dioxide,train$density,train$pH,train$sulphates,train$alcohol)
test.X=cbind(test$volatile.acidity,test$residual.sugar,test$free.sulfur.dioxide,test$density,test$pH,test$sulphates,test$alcohol)
train.quality=train$quality
set.seed(1)
knn.pred=knn(train.X,test.X,train.quality,k=1)
table(knn.pred,test$quality)
282/(282+116)
mean(knn.pred==test$quality)
knn.pred=knn(train.X,test.X,train.quality,k=3)
table(knn.pred,test$quality)
288/(288+110)
mean(knn.pred==test$quality)
knn.pred=knn(train.X,test.X,train.quality,k=5)
table(knn.pred,test$quality)
297/(297+101)
mean(knn.pred==test$quality)
knn.pred=knn(train.X,test.X,train.quality,k=10)
table(knn.pred,test$quality)
284/(284+114)
mean(knn.pred==test$quality)


# Bagging and Random Forests

library(randomForest)
set.seed(1)
bag.white=randomForest(quality~.,data=train,family=binomial,mtry=11,importance=TRUE)
bag.white
yhat.bag = predict(bag.white,newdata=white[-train,])
plot(yhat.bag, white.test)
abline(0,1)
mean((yhat.bag-white.test)^2)
bag.white=randomForest(quality~.,data=train,family=binomial,mtry=11,ntree=25)
yhat.bag = predict(bag.white,newdata=white[-train,])
mean((yhat.bag-white.test)^2)
set.seed(1)
rf.white=randomForest(quality~.,data=white,subset=train,mtry=3,importance=TRUE)
yhat.rf = predict(rf.white,newdata=white[-train,])
mean((yhat.rf-white.test)^2)
importance(rf.white)
varImpPlot(rf.white)

# Boosting

library(gbm)
set.seed(1)
boost.white=gbm(quality~.,data=train,distribution="gaussian",n.trees=5000,interaction.depth=4)
summary(boost.white)
par(mfrow=c(1,2))
plot(boost.white,i="alcohol")
plot(boost.white,i="density")
yhat.boost=predict(boost.white,newdata=white[-train,],n.trees=5000)
mean((yhat.boost-white.test)^2)
boost.white=gbm(quality~.,data=white[train,],distribution="gaussian",n.trees=5000,interaction.depth=4,shrinkage=0.2,verbose=F)
yhat.boost=predict(boost.white,newdata=white[-train,],n.trees=5000)
mean((yhat.boost-white.test)^2)

#SVM

white = read.csv(file = 'winequality-white.csv', sep=";")
fix(white)
names(white)
summary(white)
white$quality=ifelse(test=white$quality>=6,yes = "Good",no = "Bad")
white$quality=as.factor(white$quality)
xtabs(~quality,data=white)
set.seed(1)
dat = white[x<4500,]
library(e1071)
svmfit=svm(quality~., data=dat, kernel="binomial", cost=10,scale=FALSE)
plot(svmfit,dat,alcohol~volatile.acidity)
svmfit$index
summary(svmfit)
set.seed(2)
tune.out=tune(svm,quality~.,data=dat,kernel="linear",ranges=list(cost=c(0.001, 0.01, 0.1, 1,5,10,100)))
summary(tune.out)
bestmod=tune.out$best.model
summary(bestmod)

set.seed(15)
test2=white[4501:4898,]
ypred=predict(bestmod,test2)
table(predict=ypred, truth=test2$quality)
acc = (41+268)/(41+36+53+268)
acc
mean(ypred==test2$quality)


#using imp factors
set.seed(20)
dat2 = white[1:4500,]
library(e1071)
svmfit2=svm(quality~volatile.acidity+residual.sugar+free.sulfur.dioxide+density+pH+sulphates+alcohol, data=dat2, kernel="linear", cost=10,scale=FALSE)

svmfit2$index
summary(svmfit2)
set.seed(25)
tune.out2=tune(svm,quality~volatile.acidity+residual.sugar+free.sulfur.dioxide+density+pH+sulphates+alcohol,data=dat2,kernel="linear",ranges=list(cost=c(0.001, 0.01, 0.1, 1,5,10,100)))
summary(tune.out2)
bestmod1=tune.out2$best.model
summary(bestmod1)

set.seed(35)
test3=white[4501:4898,]
ypred1=predict(bestmod1,test3)
table(predict=ypred1, truth=test3$quality)
acc2 = (40+277)/(40+27+54+277)
acc2
mean(ypred==test3$quality)

# Support Vector Machine

set.seed(1)
dat = white[1:4500,]


svmfit20=svm(quality~., data=dat, kernel="radial",  gamma=1, cost=1)

summary(svmfit20)
svmfit21=svm(quality~., data=dat, kernel="radial",gamma=1,cost=1e5)


set.seed(1)
tune.out20=tune(svm, quality~., data=dat, kernel="radial", ranges=list(cost=c(0.1,1,10,100,1000),gamma=c(0.5,1,2,3,4)))
summary(tune.out20)
bestmod20=tune.out20$best.model
summary(bestmod20)

set.seed(75)
test20=white[4501:4898,]
ypred20=predict(bestmod20,test20)
table(predict=ypred20, truth=test20$quality)
acc2 = (41+269)/(41+35+53+269)
acc2

set.seed(1)
dat = white[1:4500,]


svmfit25=svm(quality~volatile.acidity+residual.sugar+free.sulfur.dioxide+density+pH+sulphates+alcohol, data=dat, kernel="radial",  gamma=1, cost=1)
set.seed(80)
test21=white[4501:4898,]
ypred21=predict(svmfit25,test21)
table(predict=ypred21, truth=test21$quality)
acc2 = (47+271)/(47+33+47+271)
acc2