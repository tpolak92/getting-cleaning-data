##########################################################################################################

## Coursera Getting and Cleaning Data Course Project
## 2017-10-22

# runAnalysis.r File Description:

# This script will perform the following steps on the UCI HAR Dataset downloaded from 
# https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip 
# 1. Merge the training and the test sets to create one data set.
# 2. Extract only the measurements on the mean and standard deviation for each measurement. 
# 3. Use descriptive activity names to name the activities in the data set
# 4. Appropriately label the data set with descriptive activity names. 
# 5. Creates a second, independent tidy data set with the average of each variable for each activity and each subject. 

##########################################################################################################

#####################################################
# STEP 0: Library packages
#####################################################

# Load packages - While this could be done all in baseR, the below packages will make this process much cleaner
library(tidyverse)
library(stringr)


#####################################################
# STEP 1: Download the data to the local directory
#####################################################

# URL with our dataset
dataURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"

# Determine which directory you're currently in. If you want to store somewhere different, use setwd()
getwd()

# Utilized an if statement to only download the dataset if it doesn't already exist. No unnecessary time spent downloading a file if it is already on the computer
if(!file.exists("URI Raw Data")) {
  dir.create("URI Raw Data")
  
  fp <- "./URI Raw Data/URI Data.zip" #File Path of where the data will be stored
  
  download.file(dataURL, fp)
  unzip(fp,exdir = "./URI Raw Data") #use this function to extract the contents of the zip file
  file.remove(fp) #Removes the zip file, since we have already extracted the important data
}


#####################################################
# STEP 2: Pull the data into R
#####################################################

# Manuever the directory into the folder of data, saves time and shortens strings later on.
setwd("./URI Raw Data")

# Pull in the features and activityType tables, will use these to attach necessary descriptions. No headers in table.
features = read.table('./UCI HAR Dataset/features.txt',header=FALSE)
activityType = read.table('./UCI HAR Dataset/activity_labels.txt',header=FALSE)

# Pull in the training dataset. No headers in table. 
subjectTrain = read.table('./UCI HAR Dataset/train/subject_train.txt',header=FALSE)
xTrain = read.table('./UCI HAR Dataset/train/x_train.txt',header=FALSE)
yTrain = read.table('./UCI HAR Dataset/train/y_train.txt',header=FALSE)

# Pull in the test dataset. No headers in table. 
subjectTest = read.table('./UCI HAR Dataset/test/subject_test.txt',header=FALSE)
xTest= read.table('./UCI HAR Dataset/test/x_test.txt',header=FALSE)
yTest = read.table('./UCI HAR Dataset/test/y_test.txt',header=FALSE)


#####################################################
# STEP 3: Transform the raw datasets
#####################################################

# Since the raw datasets, did not have column headers, name them here.
colnames(activityType) = c("activity_type_id","activity_type")
# Training dataset
colnames(subjectTrain) = "subject_id"
colnames(yTrain) = "activity_type_id"
colnames(xTrain) = features[,2]
# Test dataset
colnames(subjectTest) = "subject_id"
colnames(yTest) = "activity_type_id"
colnames(xTest) = features[,2]


# Combine the three datasets for training and testing into one table
trainingData <- cbind(xTrain,yTrain,subjectTrain)
testingData <- cbind(xTest,yTest,subjectTest)

# Combine the two datasets, training and testing, into one dataset
combinedData <- rbind(trainingData,testingData)

#####################################################
# STEP 4: Tidy the combined dataset
#####################################################

# Create a vector of column names to determine if the column name should stay or not
combinedDataColNames = names(combinedData)
# Utilizing grepl, find all the column names that meet the following criteria, will return TRUE or FALSE
keepColumns = (
              grepl("activity..",combinedDataColNames) | 
              grepl("subject..",combinedDataColNames) | 
                  grepl("-mean..",combinedDataColNames) & 
                  !grepl("-meanFreq..",combinedDataColNames) & 
                  !grepl("mean..-",combinedDataColNames) | 
              grepl("-std..",combinedDataColNames) & 
                !grepl("-std()..-",combinedDataColNames)
              )
# Use the logical vector to keep only the columns we want
combinedData = combinedData[keepColumns == TRUE]

# Join in the activity type
combinedData = left_join(combinedData, activityType, by = "activity_type_id")
combinedDataColNames = names(combinedData)

combinedDataColNames

# Cleaning up the variable names
for (i in 1:length(combinedDataColNames)) {
  combinedDataColNames[i] = gsub("\\()","",combinedDataColNames[i])
  combinedDataColNames[i] = gsub("-std$","_stdDev",combinedDataColNames[i])
  combinedDataColNames[i] = gsub("-mean$","_mean",combinedDataColNames[i])
  combinedDataColNames[i] = gsub("^t","time_",combinedDataColNames[i])
  combinedDataColNames[i] = gsub("^f","freq_",combinedDataColNames[i])
  combinedDataColNames[i] = gsub("[Gg]ravity","gravity_",combinedDataColNames[i])
  combinedDataColNames[i] = gsub("([Bb]ody[Bb]ody|[Bb]ody)","body_",combinedDataColNames[i])
}

# Apply the cleaned up column names to the dataset
colnames(combinedData) = combinedDataColNames

# Gather the table so now it is tidy
combinedDataClean <- gather(combinedData, key = "features",value = "measurements",-subject_id, -activity_type_id,-activity_type)

# combinedDataClean is the final clean dataset output
combinedDataClean

#####################################################
# STEP 5: Summarise dataset
#####################################################

# Summarise the data with an average of each feature, subject, and activity type. 
summaryData <- combinedDataClean %>% 
  group_by(subject_id,activity_type,features) %>% 
  summarise(
    avgMeasurements = mean(measurements),
    observations = n()
  )

# summaryData is the final summarized dataset with the average of measurements and a count of observations
