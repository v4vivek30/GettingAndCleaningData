#Set Working Directory to the folder with Project Data
setwd("~/Downloads")

# Read Test Data Tables
X_test <- read.table("UCI HAR Dataset/test/X_test.txt", quote="\"", comment.char="")
Y_test <- read.table("UCI HAR Dataset/test/y_test.txt", quote="\"", comment.char="")
Subject_test <- read.table("UCI HAR Dataset/test/subject_test.txt", quote="\"", comment.char="")

# Making Subject and Activity data as factor
Y_test[,1]<-as.factor(Y_test[,1])
Subject_test[,1]<-as.factor(Subject_test[,1])

# Read Train Data Tables
X_train <- read.table("UCI HAR Dataset/train/X_train.txt", quote="\"", comment.char="")
Y_train <- read.table("UCI HAR Dataset/train/y_train.txt", quote="\"", comment.char="")
Subject_train <- read.table("UCI HAR Dataset/train/subject_train.txt", quote="\"", comment.char="")

# Making Subject and Activity data af factor
Y_train[,1]<-as.factor(Y_train[,1])
Subject_train[,1]<-as.factor(Subject_train[,1])

#---------------------------- 1-----------------------------------------
# Merging Training and Test Data Tables to Get one Data Set.
X_data <- rbind(X_test,X_train)
Y_data <- rbind(Y_test,Y_train)
Subject_data <- rbind(Subject_test,Subject_train)
colnames(Subject_data)<-"Subject"
colnames(Y_data)<-"Activity"
Dataset<-cbind(Subject_data,Y_data,X_data) 
# this data set will have the first column with Subject infromation, 2nd column with Activity Information and the remaining 561 vectors for feature vector data

#----------------------------2--------------------------------------------
# Extracting mean and Std Deviation for each measurment
Mean_data<-apply(Dataset[,c(3:563)],2,mean,na.rm=TRUE)
SD_data<-apply(Dataset[,c(3:563)],2,sd,na.rm=TRUE)

# Read Activity Labels Tables
Activity_labels <- read.table("UCI HAR Dataset/activity_labels.txt", quote="\"", comment.char="")
Activity_labels$V1<-as.factor(Activity_labels$V1)
colnames(Activity_labels)<-c("number","Activity Description")

#----------------------------3--------------------------------------------
# Merging Activity Label data with Data Set
MergedData<-merge(Dataset,Activity_labels,by.x = "Activity", by.y = "number")


# Read Features Labels Tables
Features_labels <- read.table("UCI HAR Dataset/features.txt", quote="\"", comment.char="")
Features<-Features_labels[,2]

#----------------------------4--------------------------------------------
# Assigning Column Names to Merged Data 
colnames(MergedData)<-c("Activity","Subject",paste(Features),"Activity Description")

library(dplyr)

#----------------------------5--------------------------------------------
# Average by Activity and by Subject
Average_Activ_Sub<-(aggregate(MergedData[,c(3:563)],by=list(MergedData$Activity,MergedData$Subject),FUN=mean))
colnames(Average_Activ_Sub)<-c("By.Activity","By.Subject",paste(Features))

# Average by Subject
Average_Sub<-(aggregate(MergedData[,c(3:563)],by=list(MergedData$Subject),FUN=mean))
colnames(Average_Sub)<-c("By.Subject",paste(Features))

# Average by Activity
Average_Activ<-(aggregate(MergedData[,c(3:563)],by=list(MergedData$Activity),FUN=mean))
colnames(Average_Activ)<-c("By.Activity",paste(Features))

By.Activity="NULL"
By.Subject="NULL"
Average_Sub_v1<-cbind(By.Activity,Average_Sub)
Average_Activ_v1<-cbind(Average_Activ[,1],By.Subject,Average_Activ[,-1])
colnames(Average_Activ_v1)<-c("By.Activity","By.Subject",paste(Features))

Average_data<-rbind(Average_Activ_v1,Average_Sub_v1,Average_Activ_Sub)
write.table(Average_data,file="Average_Data.txt", row.names = FALSE)
