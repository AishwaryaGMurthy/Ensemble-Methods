# To generate a decision tree
library(tree)
Data <- read.table("SkillCraft1_Dataset.csv", header=T,sep=",", na.strings="?")
# The '?' are omitted
Data<-na.omit(Data)	

# Training data
train.data <- sample(1:nrow(Data), (nrow(Data)/2))
# Test Data		
test <- Data[-train.data,"TotalHours"]

# Generate a tree using tree()
tree.data <- tree(TotalHours~., Data, subset=train.data)
# Predict the output using predict()
yhat <- predict(tree.data, newdata = Data[-train.data,])

# Test MSE is calculated
test.mse <- (1 / length(test)) * sum((yhat-test)^2)
cat("The test MSE is",test.mse,"\n")

#For Statistical Comparison
for(i in 1:length(test)){
diff[i]<-abs(test[i]-yhat[i])
 }
m<- sample(mean(diff):max(diff),1)
 correct <- 0
 wrong <- 0
 for(i in 1:length(test)){
 if(diff[i]<m)	
 correct<-correct+1
 else
 wrong<-wrong+1
 }
cat("The number of times algorithm gives correct values", correct,"\n")
cat("The number of times algorithm gives wrong values", wrong,"\n")
