#load packages
library(dplyr)
library(tidyr)
library(reshape2)

#assign the activity labels and features to R
activitylabels <- read.table("./UCI HAR Dataset/activity_labels.txt")
features <- read.table("./UCI HAR Dataset/features.txt")

#assign the train data sets to R
trains <- read.table("./UCI HAR Dataset/train/subject_train.txt")
trainx <- read.table("./UCI HAR Dataset/train/X_train.txt")
trainy <- read.table("./UCI HAR Dataset/train/y_train.txt")

#assign the test data sets to R
tests <- read.table("./UCI HAR Dataset/test/subject_test.txt")
testx <- read.table("./UCI HAR Dataset/test/x_test.txt")
testy <- read.table("./UCI HAR Dataset/test/y_test.txt")

#change column names
names(activitylabels) <- c("activity_code", "activity_label")
names(features) <- c("feature_code", "feature_label")
names(trains) <- "subject"
names(trainy) <- "activity"
names(tests) <- "subject"
names(testy) <- "activity"

#combine all 3 data sets for train & test
trainall <- cbind(trains, trainy, trainx)
testall <- cbind(tests, testy, testx)

#merge train and test data sets (PART 1)
data1 <- rbind(trainall, testall)

#order the data set by subject
data2 <- arrange(data1, subject)

#get rid of the "angle" measurements
data3 <- select(data2, subject:V554)

#extract only the mean and sd for each measurement (PART 2)
extract1 <- grep("mean", features$feature_label)          #determine "mean" col
extract2 <- grep("std", features$feature_label)           #determine "std" col
extract3 <- grep("meanFreq", features$feature_label)
extract4 <- c(extract1, extract2)                         #combine into vector
extract5 <- extract4[! extract4 %in% extract3]            #removes "meanFreq"
extract6 <- extract5 + 2         #since first 2 col's of data3 are not features
extract7 <- extract6[order(extract6)]            #put the columns back in order
data4 <- data3[c(1, 2, extract7)]            #extract only mean and std columns

#add the activity name to each activity (PART 3)
activitylabels[2] <- as.character(activitylabels[[2]])
for(i in 1:10299) {
        x <- activitylabels[data4$activity[i], 2]
        data4$activity[i] <- x
}

#label each variable (PART 4)
features[2] <- as.character(features[[2]])          #convert to chr
data5 <- select(data4, V1:V543, subject, activity)  #reorganize cols
variablelocations <- extract_numeric(names(data5))  #get col number
z <- NULL
for(i in 1:66) {                                    #make a vector of col names
        x <- variablelocations[i]
        y <- features[x, 2]
        z[i] <- y
}
colnames(data5) <- c(z, "subject", "activity")        #insert col names

#separate into sections
data6 <- data5[c(67, 68, 1:66)]                #put back in order
data6$activity <- factor(data6$activity)       #convert to a factor vector
data6$subject <- as.factor(data6$subject)

#make data frame narrower by moving feature names into row data
data7 <- gather(data6, feature, value, -c(subject, activity))

#find mean of each permutation of subject, activity, & feature (PART 5)
#wide version of the data
tidy_data <- dcast(data7, subject + activity ~ feature, mean)