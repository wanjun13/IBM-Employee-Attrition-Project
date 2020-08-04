#Decision Tree in R
### Decision Tree using Tree Package
library(ISLR)
library(tree)
library(readr)
summary(ibm)
attach(ibm)
dim(ibm)
mydata = ibm
#### Drop variables that don't make any impact
mydata$EmployeeCount = NULL
mydata$EmployeeNumber = NULL
mydata$StandardHours = NULL
#### Convert variables into categorical
mydata$Department = factor(mydata$Department)
mydata$BusinessTravel = factor(mydata$BusinessTravel)
mydata$EducationField = factor(mydata$EducationField)
mydata$Gender = factor(mydata$Gender)
mydata$JobRole = factor(mydata$JobRole)
mydata$MaritalStatus = factor(mydata$MaritalStatus)
mydata$OverTime = factor(mydata$OverTime)
mydata$Education = factor(mydata$Education)
mydata$EnvironmentSatisfaction = factor(mydata$EnvironmentSatisfaction)
mydata$JobInvolvement = factor(mydata$JobInvolvement)
mydata$JobLevel = factor(mydata$JobLevel)
mydata$JobSatisfaction = factor(mydata$JobSatisfaction)
mydata$PerformanceRating = factor(mydata$PerformanceRating)
mydata$RelationshipSatisfaction = factor(mydata$RelationshipSatisfaction)
mydata$StockOptionLevel = factor(mydata$StockOptionLevel)
mydata$WorkLifeBalance = factor(mydata$WorkLifeBalance)
mydata = data.frame(mydata,Attrition)
#### Split dataset into Train and Test
set.seed(88)
train <- sample(1:nrow(mydata),735)
modeldata <- mydata[train,]
testdata = mydata[-train,]
##### Build the Tree on Training data
tree1 = tree(Attrition.1~.,data = modeldata)
summary(tree1)
tree1
plot(tree1)
text(tree1,pretty=0,cex=0.4)
tree.pred=predict(tree1,testdata,type="class")
plot(tree.pred)
table(tree.pred, testdata$Attrition)
(560+33)/(560+33+68+74)   
######## Prune the Tree
set.seed(89)
cv.data=cv.tree(tree1,FUN=prune.misclass)####We want to use misclassification rate to guide the cv and pruning process
names(cv.data)
cv.data
par(mfrow=c(1,2))
plot(cv.data$size,cv.data$dev,type="b")
plot(cv.data$k,cv.data$dev,type="b")###"k" is the cost/tuning parameter "alpha" in CC(T). 
dev.off()
prune.tree=prune.misclass(tree1,best=4) # best is 4 nodes
plot(prune.tree)
text(prune.tree,pretty=0,cex=0.8)
tree.pred2=predict(prune.tree,testdata,type="class")
table(tree.pred2,testdata$Attrition)
(614+13)/(614+13+8+100)  ## 85.3% after pruning
