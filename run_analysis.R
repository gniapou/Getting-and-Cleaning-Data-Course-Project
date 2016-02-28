## You should create one R script called run_analysis.R that does the following.
## 1. Merges the training and the test sets to create one data set.
## 2. Extracts only the measurements on the mean and standard deviation for each measurement.
## 3. Uses descriptive activity names to name the activities in the data set
## 4. Appropriately labels the data set with descriptive variable names.
## 5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

##Load required packages
library(dplyr)
library(data.table)
library(tidyr)

## Download the Dataset
filesPath <- "C:/Users/Alexandre/Documents/R/Docs"
setwd(filesPath)
if(!file.exists("courseraData")){dir.create("courseraData")}
fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(fileUrl,destfile="./courseraData/Dataset.zip",method="curl")

##Unzip DataSet to /courseraData directory
unzip(zipfile="./courseraData/Dataset.zip",exdir="./courseraData")

##Set working directory
filesPath <- file.path(getwd(), "./courseraData/UCI HAR Dataset")

##Read subject files
subjectTrain <- tbl_df(read.table(file.path(filesPath, "train", "subject_train.txt")))
subjectTest  <- tbl_df(read.table(file.path(filesPath, "test" , "subject_test.txt" )))

##Read activity files
activityTrain <- tbl_df(read.table(file.path(filesPath, "train", "Y_train.txt")))
activityTest  <- tbl_df(read.table(file.path(filesPath, "test" , "Y_test.txt" )))

##Read data files
dataTrain <- tbl_df(read.table(file.path(filesPath, "train", "X_train.txt" )))
dataTest  <- tbl_df(read.table(file.path(filesPath, "test" , "X_test.txt" )))

## 1. Merges the training and the test sets to create one data set.

## combine activity and subject test and training files and 
## rename variables "subject" and "activityNum"
combinedSubject <- rbind(subjectTrain, subjectTest)
setnames(combinedSubject, "V1", "subject")
combinedActivity <- rbind(activityTrain, activityTest)
setnames(combinedActivity, "V1", "activityId")

##combine the data training and test files
combinedData <- rbind(dataTrain, dataTest)

##name combinedData variables according to features file 
dataFeatures <- tbl_df(read.table(file.path(filesPath, "features.txt")))
setnames(dataFeatures, names(dataFeatures), c("featureId", "featureName"))
colnames(combinedData) <- dataFeatures$featureName

##column names for activity labels
activityLabels <- tbl_df(read.table(file.path(filesPath, "activity_labels.txt")))
setnames(activityLabels, names(activityLabels), c("activityId","activityName"))

##combine combinedData with 
combinedSubjAct<- cbind(combinedSubject, combinedActivity)
combinedData <- cbind(combinedSubjAct, combinedData)


## 2. Extracts only the measurements on the mean and standard deviation for each measurement.

##Extracting only the mean and standard deviation
dataFeaturesMeanStd <- grep("mean\\(\\)|std\\(\\)",dataFeatures$featureName,value=TRUE)

##Taking only measurements for the mean and standard deviation and add "subject","activityId"
dataFeaturesMeanStd <- union(c("subject","activityId"), dataFeaturesMeanStd)
combinedData <- subset(combinedData,select=dataFeaturesMeanStd) 


## 3. Uses descriptive activity names to name the activities in the data set

##enter name of activity into combinedData
combinedData <- merge(activityLabels, combinedData , by="activityId", all.x=TRUE)
combinedData$activityName <- as.character(combinedData$activityName)

##create combinedData with variable means sorted by subject and activity
dataAggr <- aggregate(. ~ subject - activityName, data = combinedData, mean) 
combinedData<- tbl_df(arrange(dataAggr,subject,activityName))


## 4. Appropriately labels the data set with descriptive variable names.

names(combinedData)<-gsub("std()", "SD", names(combinedData))
names(combinedData)<-gsub("mean()", "MEAN", names(combinedData))
names(combinedData)<-gsub("^t", "time", names(combinedData))
names(combinedData)<-gsub("^f", "frequency", names(combinedData))
names(combinedData)<-gsub("Acc", "Accelerometer", names(combinedData))
names(combinedData)<-gsub("Gyro", "Gyroscope", names(combinedData))
names(combinedData)<-gsub("Mag", "Magnitude", names(combinedData))
names(combinedData)<-gsub("BodyBody", "Body", names(combinedData))


## 5. From the data set in step 4, creates a second, independent tidy data set
##with the average of each variable for each activity and each subject.

write.table(combinedData, "TidyData.txt",row.name=FALSE)