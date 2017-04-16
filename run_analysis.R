## Coursera Getting and Cleaning Data project
## 16 April, 2017
## run_analysis.R


## Problem statement:
## 1. You should create one R script called run_analysis.R that does the following.
## 
## 2. Merges the training and the test sets to create one data set.
## 
## 3. Extracts only the measurements on the mean and standard
##    deviation for each measurement.
## 
## 4. Uses descriptive activity names to name the activities in
##    the data set
## 
## 5. Appropriately labels the data set with descriptive variable names.
##    From the data set in step 4, creates a second, independent
##    tidy data set with the average of each variable for each
## activity and each subject.

library(utils)
library(dplyr)


## Additional description of data
## descURL <- "http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones"




## Data: download, unzip
dataURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"

if (!file.exists("dataset.zip")){
    download.file(dataURL, destfile="dataset.zip", method="curl")    
}

if (!file.exists("UCI HAR Dataset")){
    unzip("dataset.zip")
}

fdir <- "UCI HAR Dataset"
flist <- list.files(fdir, recursive=TRUE)



####
## Save feature list
####
features <- tbl_df(read.table(paste(fdir,"/", flist[3], sep='')))

####
## 2. Merge the training and the test sets to create one data set
####

## training data features
tr_X <- tbl_df(read.table(paste(fdir,"/", flist[27], sep='')))
colnames(tr_X) <- features$V2

## training data candidate index
tr_sub <- tbl_df(read.table(paste(fdir,"/", flist[26], sep='')))
colnames(tr_sub) <- c("candidateID")

## training data activity index
tr_y <- tbl_df(read.table(paste(fdir,"/", flist[28], sep='')))
colnames(tr_y) <- c("activityID")
train <- bind_cols(list(tr_X, tr_sub, tr_y))


## test data features
te_X <- tbl_df(read.table(paste(fdir,"/", flist[15], sep='')))
colnames(te_X) <- features$V2

## test data candidate index
te_sub <- tbl_df(read.table(paste(fdir,"/", flist[14], sep='')))
colnames(te_sub) <- c("candidateID")

## test data activity index
te_y <- tbl_df(read.table(paste(fdir,"/", flist[16], sep='')))
colnames(te_y) <- c("activityID")
test <- bind_cols(list(te_X, te_sub, te_y))

## Combinting the test & train data to make a full dataset
fullData <- rbind(train, test)

####
## 3. Extract only the measurements on the mean and standard 
##    deviation for each measurement.
####

## Getting feature names with mean and standard (logical vector)
mean_std_vec <- grepl("mean()|std()", features$V2)
## the above line also gets the meanFreq() columns
meanfreq_vec <- grepl("meanFreq()", features$V2)  ## probably not necessary
## making those columns also FALSE
mean_std_vec[which(meanfreq_vec==TRUE)] <- FALSE
## Since fullData contains 2 extra columns, we want to 
## extract those columns also 
mean_std_vec <- c(mean_std_vec, TRUE, TRUE)

## saving the data with mean and std deviation
subsetData <- fullData[, mean_std_vec]


####
## 4. Use descriptive activity names to name the activities in 
##    the data set
####

## Activity labels are in activity_labels.txt -> flist[1]
act_labels <- tbl_df(read.table(paste(fdir,"/", flist[1], sep='')))
colnames(act_labels) <- c("activityID", "activity")

newsetData <- merge(subsetData, act_labels,
                    by="activityID",
                    all.x=TRUE)

####
## 5. Appropriately label the data set with descriptive variable names.
##    From the data set in step 4, create a second, independent 
##    tidy data set with the average of each variable for each 
##    activity and each subject.
####

## Cleaning up the data labels 
names(newsetData) <- gsub("\\()", "", names(newsetData))
names(newsetData) <- gsub("^t", "Time.", names(newsetData))
names(newsetData) <- gsub("^f", "FFT.", names(newsetData))
names(newsetData) <- gsub("Acc", ".Accelerometer", names(newsetData))
names(newsetData) <- gsub("Gyro", ".Gyroscope", names(newsetData))
names(newsetData) <- gsub("Mag", ".Magnitude", names(newsetData))
names(newsetData) <- gsub("-std", ".StdDev", names(newsetData))
names(newsetData) <- gsub("-mean", ".Mean", names(newsetData))
names(newsetData) <- gsub("Jerk", ".Jerk", names(newsetData))
names(newsetData) <- gsub("-", ".", names(newsetData))


##
## ActivityID is now redundant also
tidyData <- newsetData %>%
    group_by_(.dots=c("activity", "candidateID")) %>% 
    summarise_each(funs(mean), -candidateID, -activityID, -activity)


## Save the tidy data set
write.table(tidyData, "tidyData.txt", row.name=FALSE)

