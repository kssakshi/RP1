vehicle<-read.csv(file.choose())
str(vehicle)
vehicle$lh[vehicle$lh==0]<-mean(vehicle$lh)
vehicle$lc[vehicle$lc==0]<-mean(vehicle$lc)
head(vehicle)
install.packages("psych")
library(psych)
pairs.panels(vehicle[3:5])
## Data partition -
## 1. training and testing model 2. to prevent overfitting 
## 3. Estimate real-world performance
set.seed(1234)
ind<-sample(2,nrow(vehicle),
            replace=T,
            prob=c(0.7,0.3))
training<-vehicle[ind==1,]
testing<-vehicle[ind==2,]
head(training)
##develpoing regression model
model<-lm(lc~Mileage+lh,data=vehicle)
summary(model)
#our model is showing that, milage is not significant variable. so 
# we are removing it and continuing with lh
model<-lm(lc~lh,data=vehicle)
summary(model)

## evalution
plot(lc~lh,testing,main="Labor Cost Vs Labor Hours")
abline(model,col="red")
par(mfrow=c(2,2))
plot(model)
##In the fourth plot, data points 688 and 1620 are outside Cook’s distance. Let’s take a look at
##these
vehicle[c(688,1620),]
## we will predict the labour cost for above.
## lh=32.4 & 33.9 with confidence interval 95%
predict(model,data.frame(lh=c(32.4,33.9)),interval='confidence')
predict(model,data.frame(lh=10),interval='confidence')



