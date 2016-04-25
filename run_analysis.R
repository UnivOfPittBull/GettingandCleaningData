# Getting and Cleaning Data Week 4 Project Assignment
# Uses data collected from the accelerometers from the Samsung Galaxy S smartphone. 
# A full description is available at the site where the data was obtained:

# http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones

##Here are the data for the project:
  
# https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip

# Load Libraries
library(data.table)
library(dplyr)

# 1. Merges the training and the test sets to create one data set.

featName <- read.table("UCI HAR Dataset/features.txt")
actLabels <- read.table("UCI HAR Dataset/activity_labels.txt", header = FALSE)
subTrain <- read.table("UCI HAR Dataset/train/subject_train.txt", header = FALSE)
actTrain <- read.table("UCI HAR Dataset/train/y_train.txt", header = FALSE)
featTrain <- read.table("UCI HAR Dataset/train/X_train.txt", header = FALSE)
subTest <- read.table("UCI HAR Dataset/test/subject_test.txt", header = FALSE)
actTest <- read.table("UCI HAR Dataset/test/y_test.txt", header = FALSE)
featTest <- read.table("UCI HAR Dataset/test/X_test.txt", header = FALSE)

subj <- rbind(subTrain, subTest)
act <- rbind(actTrain, actTest)
feats <- rbind(featTrain, featTest)

colnames(feats) <- t(featName[2])

colnames(act) <- "Activity"
colnames(subj) <- "Subject"
completeData <- cbind(feats,act,subj)

# 2. Extracts only the measurements on the mean and standard deviation for each measurement.
columnsWithMeanSTD <- grep(".*Mean.*|.*Std.*", names(completeData), ignore.case=TRUE)

requiredColumns <- c(columnsWithMeanSTD, 562, 563)
dim(completeData)

extractedData <- completeData[,requiredColumns]
dim(extractedData)

# 3. Uses descriptive activity names to name the activities in the data set
extractedData$Activity <- as.character(extractedData$Activity)
for (i in 1:6){
  extractedData$Activity[extractedData$Activity == i] <- as.character(actLabels[i,2])
}

extractedData$Activity <- as.factor(extractedData$Activity)

# 4. Appropriately labels the data set with descriptive variable names.

names(extractedData)<-gsub("Acc", "Acceleration", names(extractedData))
names(extractedData)<-gsub("Gyro", "Gyroscope", names(extractedData))
names(extractedData)<-gsub("BodyBody", "Body", names(extractedData))
names(extractedData)<-gsub("Mag", "Magnitude", names(extractedData))
names(extractedData)<-gsub("^t", "Time", names(extractedData))
names(extractedData)<-gsub("^f", "Frequency", names(extractedData))
names(extractedData)<-gsub("tBody", "Time-Body", names(extractedData))
names(extractedData)<-gsub("gravity", "Gravity", names(extractedData))
names(extractedData)<-gsub("-mean()", "Mean", names(extractedData), ignore.case = TRUE)
names(extractedData)<-gsub("-std()", "Standard", names(extractedData), ignore.case = TRUE)
names(extractedData)<-gsub("-freq()", "Frequency", names(extractedData), ignore.case = TRUE)
names(extractedData)<-gsub("angle", "Angle", names(extractedData))


# names(extractedData)

# 5. From the data set in step 4, creates a second, independent tidy data set with the average of each 
# variable for each activity and each subject.

extractedData$Subject <- as.factor(extractedData$Subject)
extractedData <- data.table(extractedData)

tidyData <- aggregate(. ~Subject + Activity, extractedData, mean)
tidyData <- tidyData[order(tidyData$Subject,tidyData$Activity),]

# Write to File

write.table(tidyData, file = "Tidy_Data.txt", row.names = FALSE)
