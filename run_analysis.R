
## check readme.md

# study subjects consists 1-6 activities
# 1 WALKING 2 WALKING_UPSTAIRS 3 WALKING_DOWNSTAIRS 4 SITTING 5 STANDING
# 6 LAYING
# read activity labels
labels<-read.table("UCI HAR Dataset/activity_labels.txt")

# combine activity list from train and test sets. 
activity_test <- read.table("UCI HAR Dataset/test/y_test.txt")
activity_train <- read.table("UCI HAR Dataset/train/y_train.txt")
activity_combine <- rbind(activity_test, activity_train) 
activity_combine$V1<-as.numeric(activity_combine$V1)

#combine the subject list consists of 30 individual subjects 1-30
subject_test<-read.table("UCI HAR Dataset/test/subject_test.txt")
subject_train<-read.table("UCI HAR Dataset/train/subject_train.txt")
subject_combine <- rbind(subject_test, subject_train)

#read the complete feature list 561 rows 
# the list include 17 variables(mean, std, etc.)
# apply to various vectors(tBodyAcc-XYZ, tGravityAcc-XYZ, etc.)
# more information features_info.txt

features<-read.table("UCI HAR Dataset/features.txt")

# read in data for test and training sets
data_test <- read.table("UCI HAR Dataset/test/X_test.txt")
data_train <- read.table("UCI HAR Dataset/train/X_train.txt")
data_combine <- rbind(data_test, data_train)

# combine column from subject, activity, data
# 1. Merges the training and the test sets to create one data set.
tidyData1 <- cbind(subject_combine,activity_combine,data_combine)

# 4. Appropriately labels the data set with descriptive variable names.
names(tidyData1)<- c("Subject","Activity",as.character(features$V2))

# find mean
findMean <-grep("[Mm]ean",features$V2)
# find std
findStd<-grep("std",features$V2)
# combine mean, std col index
mean_std <- c(findMean, findStd)

# 2. Extracts only the measurements on the mean and standard deviation 
# for each measurement.
# this is the combined data with subject, activity, and mean, std variable
tidyData2 <- tidyData1[,c(1,2,mean_std+2)]
  
# 3. Uses descriptive activity names to name the activities in the data set
activityLabel<-as.character(labels$V2)
tidyData2$Activity<-sapply(tidyData2$Activity,function(n) activityLabel[n])

# 5. From the data set in step 4, creates a second, independent tidy data 
# set with the average of each variable for each activity and each subject.

library(dplyr)
tidyData3 <- tidyData2 %>%
     group_by(Subject,Activity) %>%
     summarize_each(funs(mean))

write.csv(tidyData2,file="tidyDataHAR.csv")
write.csv(tidyData3,file="tidyDataHARavg.csv")
###
### output tidyData1 is a temp holding varible, combined 
###                  training and test data, with descriptive col names
###        tidyData2 is tidyData1 extracted mean and std measurements,
###                  and transformed activity to meaningful labels
###        tidyData3 is requirement 5, a second tidy data set with
###                  the average of each variable for each activity and 
###                  each subject.different data
###                  and activity labels