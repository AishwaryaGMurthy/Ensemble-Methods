#Prelimnary steps
Data <- read.table("SkillCraft1_Dataset.csv", header=T,sep=",", na.strings="?")
# The '?' are omitted
Data<-na.omit(Data)
		
# learning rate
eta <- 0.01    
# the number of decision trees
B <- 1000

# Training data
train<-Data[1:(nrow(Data)/2),1:ncol(Data)]
# Test Data
test<-Data[(nrow(Data)/2):nrow(Data),1:ncol(Data)]
# The actual output
y.train<-train$TotalHours
# The test label
y.test<-test$TotalHours

# the attributes in training set
x.train <- matrix(nrow=nrow(train),ncol=4)
j <- 1
for(i in 6:7){
x.train[,j] <- train[,i]	
j<-j+1
}	
# the attributes in test set
x.test <- matrix(nrow= nrow(test),ncol=4)
j <- 1
for(i in 6:7){
x.test[,j] <- test[,i]
j<-j+1
}

# the range of the attributes 
range <- matrix(nrow=4,ncol=2)
for(i in 1:2){
range[i,1] <- floor(max(x.train[,i]))
range[i,2] <- ceiling(min(x.train[,i]))
}
# the first column of decision_stump matrix contains the input attribute number
# the second column of decision_stump matrix contains the threshold value which 
# produces the minimum training rss
#the third column of decision_stump matrix contains best yless
#the fourth column of decision_stump matrix contains best ymore
decision_stump <- matrix(nrow=B,ncol=4)   
# Initialising residuals
r <- y.train   
for (b in 1:B) {
  attribute <- 0    
  threshold <- 0 
  left.yhat <- 0
  right.yhat <- 0
  minimum.rss <- Inf
  for (j in 1:2) {
   ##diff <- abs(range[j,1] - range[j,2])/nrow(train)
   s <- seq(range[j,2],range[j,1])
   for(t in 1 : length(s)) {
   # yless is the left side prediction
   # ymore is the right side prediction
   # l is the counter for values less than the threshold
   # m is the counter for values greater than the threshold 
      yless <- 0
      l <- 0
      ymore <- 0
      m <- 0
      for (i in 1:nrow(train)) {
        if (x.train[i,j]<s[t]) {
          yless <- yless + r[i]
          l <- l + 1
        }
        else {
          ymore <- ymore + r[i]
          m <- m + 1
        }
      }
      yless <- yless / l
      ymore <- ymore / m
      
      rss <- 0   
      for (i in 1:nrow(train)) {
        if (x.train[i,j]<s[t]) {
          rss <- rss + (yless - r[i])^2
        }
        else {
          rss <- rss + (ymore - r[i])^2
        }
      }
if(rss < minimum.rss){
left.yhat <- yless
right.yhat <- ymore
threshold <- s[t]
attribute <- j
minimum.rss <- rss
}
}   
}  
  decision_stump[b,1] <- attribute
  decision_stump[b,2] <- threshold
  decision_stump[b,3] <- left.yhat
  decision_stump[b,4] <- right.yhat
  # update residuals
  for (i in 1:nrow(train)) {
    if (x.train[i,attribute]<threshold) {
      r[i] <- r[i] - eta * left.yhat
    }
    else {
      r[i] <- r[i] - eta * right.yhat
    }
  }
}     
train.rss <- vector(length=nrow(train)) 
test.rss <- vector(length=nrow(train))   
for (i in 1:nrow(train)) {
  yhat <- 0   
  for (b in 1:B) {
    if (x.train[i,decision_stump[b,1]]<decision_stump[b,2]) {
      yhat <- yhat + eta*decision_stump[b,3]
    }
    else {
      yhat <- yhat + eta*decision_stump[b,4]
    }
  }
  train.rss[i] <- (yhat - y.train[i])^2
}	
train.MSE <- mean(train.rss)

for (i in 1:nrow(test)) {
  yhat <- 0
  for (b in 1:B) {
    if (x.test[i,decision_stump[b,1]] < decision_stump[b,2]) {
      yhat <- yhat + eta * decision_stump[b,3]
    }
    else {
      yhat <- yhat + eta*decision_stump[b,4]
    }
  }
  test.rss[i] <- (yhat - y.test[i])^2
}
test.MSE <- mean(test.rss)
cat("The training MSE is",train.MSE,"\n")
cat("The test MSE is",test.MSE,"\n")
