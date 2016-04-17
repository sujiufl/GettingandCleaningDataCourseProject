#========================================================================================================
## Getting and Cleaning Data Course Project
#========================================================================================================
## file URL
datafileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"

## Create Data File
if(!file.exists("./data")) {dir.create("./data")}

## download data zip file
download.file(datafileUrl, destfile = "./data/accelerometerdata.zip", method = "curl")

## unzip data file
unzip("./data/accelerometerdata.zip")

## load the libraries
library(dplyr)
library(data.table)
#========================================================================================================
## ------ Read all the Data files---------

## Read Activity_lables.txt file
activityLables <- read.table("./UCI HAR Dataset/activity_labels.txt", sep = "", col.names = c("ActivityId", "ActivityName"))
## convert ActivityLables ActivityName to all lower cases
activityLables[, 2] <- tolower(activityLables[, 2])
## remove all '_' in the ActivityName
activityLables[, 2] <- sub("_", " ", activityLables[, 2])

## Read features.txt file
features <- read.table("./UCI HAR Dataset/features.txt", sep = "", colClasses = c("character"))

## Read test data files
x_test <- read.table("./UCI HAR Dataset/test/X_test.txt", sep = "")
y_test <- read.table("./UCI HAR Dataset/test/y_test.txt", sep = "")
subject_test <- read.table("./UCI HAR Dataset/test/subject_test.txt", sep = "")

## Read train data files
x_train <- read.table("./UCI HAR Dataset/train/X_train.txt", sep = "")
y_train <- read.table("./UCI HAR Dataset/train/y_train.txt", sep = "")
subject_train <- read.table("./UCI HAR Dataset/train/subject_train.txt", sep = "")

#========================================================================================================
## Part 1.  Merge the training and the test sets to create one data set

# rbind x_test and x_train to create x_data
x_data <- rbind(x_train, x_test)

# rbind y_test abd y_train to create y_data
y_data <- rbind(y_train, y_test)

# rbind subject_test and subject_train to create subject_data
subject_data <- rbind(subject_train, subject_test)

# cbind x_data, y_data and subject_data to create combined_data
combined_data <- cbind(x_data, y_data, subject_data)

# name the columns in combined_data
combined_data_lable <- rbind(features, c(562, "ActivityId"), c(563, "Subject")) [, 2]
names(combined_data) <- combined_data_lable

#========================================================================================================
## Part 2. Extracts only the measurements on the mean and standard deviation for each measurement

# get only columns with 'mean' or 'std' in features
Only_MeanStdDev_MeasMent <- grep("-(mean|std)\\(\\)", features[, 2])

# get only the columns in combined_data with 'mean' and 'std' 
combined_data_MeanStd <- combined_data[, c(Only_MeanStdDev_MeasMent, 562, 563)]

#========================================================================================================
## Part 3. Uses descriptive activity names to name the activities in the data set

# merge combined_data_MeanStd with activityLables by "ActivityId" 
combined_data_MeanStd <- merge(combined_data_MeanStd, activityLables, by = "ActivityId", match = "first")

# disregard the redundant Column 1: ActivityID 
combined_data_MeanStd <- combined_data_MeanStd[ , -1]

#========================================================================================================
## Part 4. Appropriately labels the data set with descriptive variable names
names(combined_data_MeanStd)

names(combined_data_MeanStd) <- gsub("^t","time", names(combined_data_MeanStd))

names(combined_data_MeanStd) <- gsub("^f","frequency", names(combined_data_MeanStd))

names(combined_data_MeanStd) <- gsub('\\(|\\)', "", names(combined_data_MeanStd))

names(combined_data_MeanStd) <- gsub('Acc', "Accelerometer", names(combined_data_MeanStd))

names(combined_data_MeanStd) <- gsub('Gyro', "Gyroscope", names(combined_data_MeanStd))

names(combined_data_MeanStd) <- gsub('Mag', "Magnitude", names(combined_data_MeanStd))

names(combined_data_MeanStd) <- gsub('BodyBody', 'Body', names(combined_data_MeanStd))

names(combined_data_MeanStd) <- gsub('mean', 'Mean', names(combined_data_MeanStd))

names(combined_data_MeanStd) <- gsub('std', 'StandardDeviation', names(combined_data_MeanStd))

#========================================================================================================
## Part 5. From the data set in step 4, create a second, independent tidy data set with the average of each variable for each activity and each subject ---
library(plyr)
combined_data_MeanStd_avg <- ddply(combined_data_MeanStd, c("Subject", "ActivityName"), numcolwise(mean))
write.table(combined_data_MeanStd_avg, "average_tidydata.txt", row.names = FALSE)
#========================================================================================================



