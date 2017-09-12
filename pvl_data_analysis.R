
library('rpart')
library('nnet')
library('gbm')
library('mlbench')
library('FSelector')
library('ada')

setwd("C:/Users/ajalal01/SkyDrive/rproject")


# reading the data

w_data<-read.csv('W_Features.csv',header=TRUE,sep = ",")
summary(w_data)

bg_data<-read.csv('BG_Features.csv',header=TRUE)
summary(bg_data)

dg_data<-read.csv('Demog.csv',header=TRUE)
summary(dg_data)

full_data<-cbind(w_data,bg_data,dg_data)
summary(full_data)


pvl_w<-full_data$PVL
age_w<-full_data$Age

# grouping by age

age3<- age_w <=3
age4<- age_w>3 & age_w<=5
age6<- age_w>5 

young<-w_data[age3,]
middle<-w_data[age4,]
old<-w_data[age6,]

# Training & Testing

train_inx<-c(1:40)
test_inx<-c(41:65)
train_data<-full_data[train_inx,]
test_data<-full_data[test_inx,]
pvl_train<-pvl_w[train_inx]
pvl_test<-pvl_w[test_inx]

# Adaboost

adap_train_test<-ada(train_data[,-340],pvl_train,loss="logistic",type="discrete",iter=50, nu=0.1,model.coef=TRUE, bag.shift=FALSE, max.iter=20, delta=10^(-10),na.action=na.rpart)
adap_train_test<-addtest(adap_train_test,test_data[,-340],pvl_test)
summary(adap_train_test)
varplot(adap_train_test)

# Decision Tree

wfit <- rpart(pvl_train~., method="class", data=train_data[,-340],control=rpart.control(minsplit=1))
wfit
plot(wfit)



information.gain(pvl_w~.,data=w_data[,-340])
wfit <- rpart(pvl_w~., method="class", data=w_data[,-340])
wfit
wfit1 <- tree(pvl_w~,w_data[,-340])

plot(wfit)

gbm1<-gbm(pvl_w~.,data=w_data[,-252],distribution="gaussian",n.trees=100,shrinkage=0.05,interaction.depth=3)
best.iter<-gbm.perf(gbm1,method="OOB")
summary(gbm1,n.trees=best.iter)
print(pretty.gbm.tree(gbm1,1))
plot(gbm1,3,best.iter)
gbm.perf(gbm1,plot.it = TRUE)


adap_data<-ada(w_data[,-252],pvl_w,loss="exponential", type="discrete",iter=50, nu=0.1,model.coef=TRUE, bag.shift=FALSE, max.iter=20, delta=10^(-10),na.action=na.rpart)
summary(adap_data)
varplot(adap_data)

yy<-unique(age_w)