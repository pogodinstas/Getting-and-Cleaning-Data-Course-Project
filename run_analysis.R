filename <- 'getdata_dataset.zip'

# download and unzip
if (!file.exists(filename)){
  fileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
  download.file(fileURL, filename, method='curl')
}
if (!file.exists('UCI HAR Dataset')) {
  unzip(filename)
}

# reading labels and features
activity_labes <- read.table('UCI HAR Dataset/activity_labels.txt')
features <- read.table('UCI HAR Dataset/features.txt', as.is = TRUE)
#features_info <- read.csv('UCI HAR Dataset/features_info.txt', header=FALSE)

# Extracts only the measurements on the mean and standard deviation for each measurement
means_and_sd <- grep('.*mean().*|.*std().*', features[,2])
means_and_sd_names <- features[means_and_sd,2]
means_and_sd_names <- gsub('-mean', 'Mean',means_and_sd_names)
means_and_sd_names <- gsub('-std', 'STD', means_and_sd_names)
means_and_sd_names <- gsub('[()-]','',means_and_sd_names)
means_and_sd_names <- gsub('BodyBody','Body', means_and_sd_names)
means_and_sd_names <- gsub('^f', 'frequencyDomain', means_and_sd_names)
means_and_sd_names <- gsub('^t', 'timeDomain', means_and_sd_names)
means_and_sd_names <- gsub('Acc', 'Accelerometer', means_and_sd_names)
means_and_sd_names <- gsub('Gyro', 'Gyroscope', means_and_sd_names)
means_and_sd_names <- gsub('Mag', 'Magnitude', means_and_sd_names)
means_and_sd_names <- gsub('Freq', 'Frequency', means_and_sd_names)

# reading train and test datasets
y_test <- read.table('UCI HAR Dataset/test/y_test.txt')
testSubject <- read.table('UCI HAR Dataset/test/subject_test.txt')
x_test <- read.table('UCI HAR Dataset/test/X_test.txt')[,means_and_sd]
allTest <- cbind(testSubject, y_test, x_test)

x_train <- read.table('UCI HAR Dataset/train/X_train.txt')[,means_and_sd]
y_train <- read.table('UCI HAR Dataset/train/y_train.txt')
trainSubject <- read.table('UCI HAR Dataset/train/subject_train.txt')
allTrain <- cbind(trainSubject, y_train, x_train)

# Merges the training and the test sets to create one data set.
allData <- rbind(allTrain, allTest) 
rm(y_test,y_train,x_test,x_train,allTrain,allTest, trainSubject, testSubject)
colnames(allData) <- c('Subject', 'Activity', means_and_sd_names)

# Uses descriptive activity names to name the activities in the data set
allData$Activity <- factor(allData$Activity, 
                           levels = activity_labes[,1],
                           labels = activity_labes[,2])


# From the data set in step 4, creates a second, independent tidy data set with 
# the average of each variable for each activity and each subject.

library(dplyr)
allDataPivotMean <- allData %>% group_by(Subject,Activity) %>% summarise_all(mean, na.rm=TRUE)

# write result
write.table(allDataPivotMean, 'tidy_data.txt', quote = FALSE, 
            row.names = FALSE, col.names = TRUE)



