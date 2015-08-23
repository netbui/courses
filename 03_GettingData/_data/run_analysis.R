# Step 1: Merge the training and the test sets to create one data set.

#Train data
train_Data<-read.table("UCI HAR Dataset/train/X_train.txt")
train_Label<-read.table("UCI HAR Dataset/train/y_train.txt")
train_Subject <- read.table("UCI HAR Dataset/train/subject_train.txt")

#Test data
test_Data <- read.table("UCI HAR Dataset/test/X_test.txt")
test_Label <- read.table("UCI HAR Dataset/test/y_test.txt") 
test_Subject <- read.table("UCI HAR Dataset/test/subject_test.txt")

#Make tables
join_Data <- rbind(train_Data, test_Data)
join_Label <- rbind(train_Label, test_Label)
join_Subject <- rbind(train_Subject, test_Subject)

# Step 2: Extract the mean and standard deviation
features <- read.table("UCI HAR Dataset/features.txt")
grepstats <- grep("mean\\(\\)|std\\(\\)", features[, 2])

join_Data <- join_Data[, grepstats]

#Make columns readable
names(join_Data) <- gsub("\\(\\)", "", features[grepstats, 2]) # remove parens
names(join_Data) <- gsub("mean", "Mean", names(join_Data)) # capitalize M
names(join_Data) <- gsub("std", "Std", names(join_Data)) # capitalize S
names(join_Data) <- gsub("-", "", names(join_Data)) # remove hypen in column names 

# Step 3: Create descriptive activity names
activity <- read.table("UCI HAR Dataset/activity_labels.txt")
activity[, 2] <- tolower(gsub("_", "", activity[, 2]))
substr(activity[2, 2], 8, 8) <- toupper(substr(activity[2, 2], 8, 8))
substr(activity[3, 2], 8, 8) <- toupper(substr(activity[3, 2], 8, 8))
activity_Label <- activity[join_Label[, 1], 2]
join_Label[, 1] <- activity_Label


# Step 4: Appropriately labels the data set with descriptive activity 
names(join_Label) <- "activity"
names(join_Subject) <- "subject"
output <- cbind(join_Subject, join_Label, join_Data)

write.table(output, "merged_data.txt") # write out merged table

# Step 5: Creates a second, independent tidy data set with the average of each variable for each activity and each subject. 
subject_Len <- length(table(join_Subject)) # 30
activity_Len <- dim(activity)[1] # 6
column_Len <- dim(output)[2]
result <- matrix(NA, nrow=subject_Len*activity_Len, ncol=column_Len) 
result <- as.data.frame(result)
colnames(result) <- colnames(output)
row <- 1
for(i in 1:subject_Len) {
  for(ii in 1:activity_Len) {
    result[row, 1] <- sort(unique(join_Subject)[, 1])[i]
    result[row, 2] <- activity[ii, 2]
    bool1 <- i == output$subject
    bool2 <- activity[ii, 2] == output$activity
    result[row, 3:column_Len] <- colMeans(output[bool1&bool2, 3:column_Len])
    row <- row + 1
  }
}

write.table(result, "final_data.txt",row.name=FALSE) # write out final table

