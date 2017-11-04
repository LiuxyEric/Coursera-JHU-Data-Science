#0. Download the file and set the enviroment.
work_dir <- getwd()
data_dir <- "UCI HAR Dataset"
file_path <- function(...){ paste(data_dir,..., sep="/")}
if (!"Dataset.zip" %in% dir()){
download.file('https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip', destfile=paste(work_dir,'Dataset.zip', sep='/'))
unzip('Dataset.zip')}
library(data.table)


# 1. Merges the training and the test sets to create one data set.

training_set <- fread(file_path("train/X_train.txt"))
test_set <- fread(file_path("test/X_test.txt"))
data_set <- rbind(training_set, test_set)

#set the correct column names.
features_name <- fread(file_path("features.txt"))[,2]
colnames(data_set) <- features_name$V2
data_set[1:5,1:5]
dim(data_set)

#remove the useless dataset
rm(training_set, test_set)


#2. Extracts only the measurments on the mean and standard deviation for each measurement.

select_col_names <- grepl('-(mean|std)\\(', features_name$V2)
data_set_select <- subset(data_set, select = select_col_names)
dim(data_set_select)

#4. Appropriately labels the data set with descriptive variable names

colnames(data_set_select) <- gsub("^t", "Time.", colnames(data_set_select))
colnames(data_set_select) <- gsub("mean", "Mean.", colnames(data_set_select))
colnames(data_set_select) <- gsub("std", "Std.", colnames(data_set_select))
colnames(data_set_select) <- gsub("^f", "Frequency.", colnames(data_set_select))
colnames(data_set_select) <- gsub("\\(\\)", "", colnames(data_set_select))
colnames(data_set_select) <- gsub("-", "", colnames(data_set_select))
colnames(data_set_select) <- gsub("Body|BodyBody", "Body.", colnames(data_set_select))
colnames(data_set_select) <- gsub("Acc", "Acceleration.", colnames(data_set_select))
colnames(data_set_select) <- gsub("Gravity", "Gravity.", colnames(data_set_select))

#3. Uses descriptive activity names to name the activities in the data set

subjects_train <- fread(file_path("train/subject_train.txt"))
subjects_test <- fread(file_path("test/subject_test.txt"))
subjects <- rbind(subjects_train, subjects_test)[,1]
activities_train <- fread(file_path("train/y_train.txt"))
activities_test <- fread(file_path("test/y_test.txt"))
activities <- rbind(activities_train, activities_test)[,1]
labels <- c("Walking", "Walking upstairs", "Walking downstairs", "sitting", "Standing", "Laying")
activities <- labels[activities$V1]
data_set_select <- cbind(Activity = activities, data_set_select)
data_set_select <- cbind(Subject = subjects$V1, data_set_select)
dim(data_set_select)

#5. From the data set in step4, creates a second, independent tidy data set 
#with the average of each variable for each activity and each subject

library(dplyr)

average_data_set <- data_set_select %>% group_by(Subject, Activity) %>% summarise_all(funs(mean))

write.table(average_data_set, row.name=FALSE, file = "tidy_data_set.txt")
