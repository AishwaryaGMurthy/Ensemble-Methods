# To generate a decision tree
library(tree)
Data <- read.table("SkillCraft1_Dataset.csv", header=T,sep=",", na.strings="?")
# The '?' are omitted
Data<-na.omit(Data)

# Training data
train<-Data[1:(nrow(Data)/2),1:ncol(Data)]
# Test Data
test<-Data[(nrow(Data)/2):nrow(Data),1:ncol(Data)]
B <- 1000
fhat<-matrix(nrow=nrow(test),ncol = B)
fbag<- vector(length = nrow(Data)/2)
y <- test$TotalHours

for(b in 1:B){
 # Training data
 train <- sample(1:nrow(Data), (nrow(Data)/2))
 tree.data <- tree(TotalHours~., Data, subset=train)
 fhat[,b] <- predict(tree.data, newdata = test)
}


for(i in 1:(nrow(Data)/2)){
 fbag[i] <- mean(fhat[i,])
}

sum <- 0

for(i in 1:length(fbag))
{
 sum=sum+(fbag[i]-y[i])^2
}
 

bagged.mse<-sum/nrow(test)
cat("The test MSE obtained in bagging is ",bagged.mse,"\n")

#For Statistical Comparison

bagged.correct <- 0
bagged.wrong <- 0

diff<-abs(y-fbag)

m<-sample(mean(diff):max(diff),1)

for(i in 1:nrow(test)){
 if(diff[i]<m)	
 bagged.correct<-bagged.correct+1
 else
 bagged.wrong<-bagged.wrong+1
 }

cat("The number of times algorithm gives correct values", bagged.correct,"\n")
cat("The number of times algorithm gives wrong values", bagged.wrong,"\n")
