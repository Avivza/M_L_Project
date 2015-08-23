library(caret)
#Loading the Data
url<-"https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
download.file(url,"trainData.csv")
library(caret)
unclean_trndt<-read.csv("trainData.csv")

#Cleaning the data of irrelevant variabels
#first I will clean variabels that consists summary statistics on thw window (max/min/mean)
unnescry_stat<-grep("max|min|avg|stddev|var|amplitude|total|kurtosis|skewness",names(unclean_trndt))
#now I will erase irrelevent meta data
meta_data<-c(1:7)
remove<-c(unnescry_stat,meta_data)
trndt<-unclean_trndt[,-remove]

#removing corrlated variabels
rm_cor<-findCorrelation(cor(trndt[-49]))
trndt<-trndt[,-rm_cor]

traininx<-createDataPartition(y = trndt$classe,p = 0.7,list = FALSE)
train<-trndt[traininx,]
test <-trndt[-traininx,]

#find variabels with creelation to classe:
colums<-vector()
for ( i in 1:42){
        p<-summary(aov(train[,i]~train[,43]))[[1]][["Pr(>F)"]][1]
        if (p<0.05) {
                colums<-c(colums,i)
                }
         }

new_train<-train[,c(colums,43)]

preproc<-preProcess(new_train[,-39],method="pca")
trainpc<-predict(preproc,new_train[,-39])

t_c<- trainControl(method="repeatedcv", number=10,repeats = 3)
mdl<- train(new_train$classe~., data=trainpc, trControl=t_c, method="rpart")


rownames(varImp(mdl,scale=FALSE)$importance)[which(varImp(mdl,scale=FALSE)$importance==0)]

mdl2<- train(classe~pitch_forearm+magnet_belt_y+yaw_belt+magnet_dumbbell+pitch_belt+, data=trndt, trControl=t_c, method="rpart")
rownames(varImp(mdl2)$importance)[which(varImp(mdl2)$importance>40)]

mdl3<- train(classe~ magnet_arm_x+magnet_belt_y+magnet_dumbbell_y+pitch_forearm+yaw_belt, data=trndt, trControl=t_c, method="rpart")


