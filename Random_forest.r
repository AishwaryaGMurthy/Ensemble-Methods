#Functions used for random forest
# For selecting the root at each split in the tree
root.selection <- function(data, p, m,y){
attributes <- matrix(,nrow(data),m)
#attribute numbers are chosen
a<- sample(p,m)
for(i in 1:m){
attributes[,i] <- data[,a[i]]
}	
attributes <- na.omit(attributes) 
# the range of the attributes 
# each column of the range matrix indicate an attribute among the ‘m’ attributes
# The first row of range matrix contains the maximum value of m attributes
# The second row of range matrix contains the minimum value of m attributes
range <- matrix(nrow=m,ncol=2)
#diff <- vector(length=m)
for(i in 1:m){	
range[i,1] <-  floor(max(attributes[,i]))
range[i,2] <-  ceiling(min(attributes[,i]))
}
range <- na.omit(range)
best.attribute <- 0
threshold <- 0
left.yhat <- 0
right.yhat <- 0
minimum.rss <- Inf
leaf.indicator <- 0
  for (j in 1:m) {
  # diff <- abs(range[j,1] - range[j,2])/nrow(train)
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
      for (i in 1:nrow(attributes)) {
        if (attributes[i,j]<s[t]) {
          yless <- yless + y[i]
          l <- l + 1
        }
        else {
          ymore <- ymore + y[i]
          m <- m + 1
        }
      }
      yless <- yless / l
      ymore <- ymore / m
      
      rss <- 0   
      for (i in 1:nrow(attributes)) {
        if (attributes[i,j]<s[t]) {
          rss <- rss + (yless - y.train[i])^2
        }
        else {
          rss <- rss + (ymore - y.train[i])^2
        }
      }
if(rss < minimum.rss){
left.yhat <- yless
right.yhat <- ymore
threshold <- s[t]
best.attribute <- a[j]
minimum.rss <- rss
}
} # ends loop for s   
} # ends loop for m or attributes
return(c(best.attribute, threshold, left.yhat, right.yhat))

} 

# For updating the dataset
updating.data <- function(data,b1,b2){
for(i in 1:length(b1)){
d1[i,]<-data[b1[i],]
}
for(i in 1:length(b2)){
 d2[i,]<-data[b2[i],]
}	
l <- list(d1,d2)
return(l)
}

# To generate the stopping condition
condition <- function(cond,b1,b2){
for(i in 1:length(b1)){
c1[i] <- cond[b1[i]]
}
for(i in 1:length(b2)){
c2[i] <- cond[b2[i]]
}
c <- list(c1,c2)
return (c)
} 

# To check the purity of the condition 
check <- function(cond){
if(all(cond==cond[1])){
return (TRUE)
} else{
return (FALSE)
}
}

# The main program
#Prelimnary steps
Data <- read.table("SkillCraft1_Dataset.csv", header=T,sep=",", na.strings="?")
# The '?' are omitted
Data<-na.omit(Data)

B <- 10
for(b in 1:B){
training <- sample(nrow(Data), size= (nrow(Data)/2))
train<-Data[training,]
# Test Data	
test<-Data[-training,]
# The actual output	
y<-train$TotalHours
test<-Data[-training,]
y.test <- test$TotalHours
n<-mean(y)	
cond <- ifelse(y < n,TRUE,FALSE)
inputs <- train		
inputs$TotalHours <- NULL
data <- inputs	
data <- na.omit(data)
p <- ncol(data)
m <- floor(sqrt(p))
##m <- p
# dataset_list contains the list of datasets
dataset_list <- list() 
checking.condition <- list()
attribute <- 1
x <- root.selection(inputs,p,m,y)
while(attribute != 0){
root <- data[,x[1]]
root <- na.omit(root)
threshold <- x[2]
if(length(dataset_list)!=0)
data <- dataset_list[[1]]
b1 <- vector()
b2 <- vector()	
  j1<-1		
  j2<-1
 for(i in 1:length(root)){
  if(root[i]<threshold){
  b1[j1] <- i
  j1<-j1+1
  }
  else{
  b2[j2] <- i
  j2<-j2+1
  }
  }
if(length(dataset_list)!=0)
dataset_list[[1]] <- NULL
dataset_list <- updating.data(data,b1,b2)
checking.condition <- condition(cond,b1,b2)
checking.condition[[1]]<- na.omit(checking.condition[[1]])
checking.condition[[2]]<- na.omit(checking.condition[[2]])
if(check(checking.condition[[1]])==TRUE && check(checking.condition[[2]])==TRUE){
attribute <- 0
} else{
attribute <- 1
}
for(i in 1: length(dataset_list)){
cond <- checking.condition[[i]]
cond <- na.omit(cond)	
correctness <- check(cond)
if (correctness == FALSE){
data <- dataset_list[[i]]
data <- na.omit(data) 
if(nrow(data)!=0)	
x <- root.selection(dataset_list[[i]],p,m,y)
dataset_list[length(dataset_list)+1] <- data
} 
data <- na.omit(data)
if(nrow(data)==0)
attribute <- 0
}
}
yless[b] <- x[3]
ymore[b] <- x[4]
}
yless <- na.omit(yless)
yless <- mean(yless)
ymore <- na.omit(ymore)
ymore <- mean(ymore)
n <- mean(y.test)	
rss <- 0	
for(i in 1:nrow(test)){
if(y.test[i] < n){
rss[i] <- ((y.test - yless)^2)
} else{	
rss[i] <- ((y.test - ymore)^2)
}	
}
test.mse<- mean(rss)
cat("The test MSE obtained in random forest is",test.mse,"\n")

# For Statistical Comparison
yhat <- (yless+ymore)/2 
diff<-abs(y-yhat)
 forest.correct <- 0
 forest.wrong <- 0
 m<-sample(mean(diff):max(diff),1)
 for(i in 1:nrow(test)){
  if(diff[i]<m)
 forest.correct <- forest.correct + 1
 else
 forest.wrong <- forest.wrong + 1
}
cat ("The number of times random forest gives correct values",forest.correct,"\n")
cat("The number of times random forest gives wrong values",forest.wrong,"\n")
