library(data.table)
library(dplyr)

# set working directory to folder with UCI HAR Dataset 
# (optional) if dataset is already in working directory

#setwd("~/Documents/Data Science/Course 3 (Getting and Cleaning Data)")

# Read in data into data tables

## features
features <- fread("./UCI HAR Dataset/features.txt") 
        
## test/train data
testData <- fread("./UCI HAR Dataset/test/X_test.txt")
trainData <- fread("./UCI HAR Dataset/train/X_train.txt")

## subject data
### there were 30 subjects (1-30)
subjectTestData <- fread("./UCI HAR Dataset/test/subject_test.txt")
subjectTrainData <- fread("./UCI HAR Dataset/train/subject_train.txt")

# activity data
activityTestData <- fread("./UCI HAR Dataset/test/y_test.txt")
activityTrainData <- fread("./UCI HAR Dataset/train/y_train.txt")
activityLabels <- fread("./UCI HAR Dataset/activity_labels.txt") 
## activity key: (1-6)
# 1 = walking
# 2 = walking upstairs
# 3 = walking downstairs
# 4 = sitting
# 5 = standing
# 6 = laying

# Merge test and train data
mergedData <- rbind(testData, trainData)

# Extract only mean and std 
indices <- as.vector(grep("mean|std", features$V2)) # only want the rows that have mean or std dev
features <- features[indices]
mergedData <- mergedData[,..indices]

# Add subject and activity data to mergedData
subjectData <- rbind(subjectTestData, subjectTrainData)
activityData <- rbind(activityTestData, activityTrainData)
mergedData <- cbind(subjectData, activityData, mergedData)

# Labels the data set with descriptive variables

names(mergedData)[3:81] <- features$V2 
names(mergedData)[1] <- "Subject"
names(mergedData)[2] <- "Activity Type"

## make the names of the merged set more descriptive and less confusing
names(mergedData) <- gsub("BodyBody", "Body", names(mergedData)) # word shouldn't be repeated
names(mergedData) <- gsub("^t", "Time", names(mergedData))
names(mergedData) <- gsub("^f", "Frequency", names(mergedData))


# Use descriptive labels for activity names

## create function
activityTypeReplacer <- function(x){
        if (x==1){
                gsub(1, "walking", x)
        }  
        else if (x==2){
                gsub(2, "walking upstairs", x)
        }
        else if (x==3){
                gsub(3, "walking downstairs", x)
        }
        else if (x==4){
                gsub(4, "sitting", x)
        }
        else if (x==5){
                gsub(5, "standing", x)
        }
        else {# x==6 
                gsub(6, "laying", x)        
        }
}

mergedData$`Activity Type` <- sapply(mergedData$`Activity Type`, activityTypeReplacer)

# Create second independent tidy dataset with average for each subject/activity pair combination
byComboData <- mergedData %>% group_by(Subject, `Activity Type`) %>% summarize_all(funs(mean))
write.table(byComboData, "finalTidyData.txt", row.names = FALSE)
