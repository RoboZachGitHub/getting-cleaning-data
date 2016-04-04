
##### PRELIMINARY LOADING AND LABELING OF DATAFILES #####

# clear the workspace, load the reshape package
rm(list = ls())
library(reshape2)

# download dataset
url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
filename1 = "Dataset.zip"
download.file(url, destfile = filename1)

# unzip (extract) the files
unzip(filename1)


# modify the working path so it points to the extracted directory and the files contained therein
path <- file.path(getwd(), "UCI HAR Dataset")


# read in all files to be used
ds_features <- read.table(file.path(path, "features.txt"))
ds_activity_labels <- read.table(file.path(path, "activity_labels.txt"))
ds_subject_train <- read.table(file.path(path, "train", "subject_train.txt"))
ds_x_train <- read.table(file.path(path, "train", "X_train.txt"))
ds_y_train <- read.table(file.path(path, "train", "Y_train.txt"))
ds_subject_test <- read.table(file.path(path, "test", "subject_test.txt"))
ds_x_test <- read.table(file.path(path, "test", "X_test.txt"))
ds_y_test <- read.table(file.path(path, "test", "Y_test.txt"))



## label columns of the data sets
# label the datasets describing the subjects accordingly
colnames(ds_subject_train) = c("subjectID")
colnames(ds_subject_test) = c("subjectID")

# label the datasets describing the different activities accordingly
# ds_activity_labels gives an ID number to each different activity
colnames(ds_activity_labels) = c("activityID", "activity")

# label the y datasets accordingly, they are lists of the activities by an ID number
colnames(ds_y_train) = c("activityID")
colnames(ds_y_test) = c("activityID")

# the x datasets are the actual measurements corresponding to the labels in the ds_features
# label the x datasets based on the names provided in ds_features
feature_names <- ds_features[,2]
colnames(ds_x_train) = feature_names
colnames(ds_x_test) = feature_names

# create the training and the test data sets
ds_train = cbind(ds_y_train, ds_subject_train, ds_x_train)
ds_test = cbind(ds_y_test, ds_subject_test , ds_x_test)

# at this point, the train and testing data have been merged and all columns are labeled

##### PRELIMINARY LOADING AND LABELING OF DATAFILES: COMPLETED #####


#### ASSIGNMENT OBJECTIVES #####

### Objective 1: merge the training and test sets into one master set
# the master dataset is simply the merging of the ds_train and ds_test by row
ds_master = rbind(ds_train, ds_test)


### Objective 2: subset the dataset to only include mean and standard deviation data
# subset only standard deviations, means, but also keep activityID, subjectNumber
subset_vector <- grepl("activity..|subject..|mean\\(\\)|std\\(\\)", colnames(ds_master))
ds_master = ds_master[subset_vector] 


### Objective 3: Give descriptive activity names to the dataset
ds_master = merge(ds_activity_labels, ds_master, by='activityID', all.x=TRUE)


### Objective 4: Label the dataset with descriptive activity names
# must rename all columns with ugly names
column_names = colnames(ds_master)
for (i in 1:length(column_names)){
  tmp = column_names[i]
  tmp = gsub("\\()","", tmp)
  tmp = gsub("^(t)","Time", tmp) 
  tmp = gsub("^(f)","Freq", tmp)
  tmp = gsub("mean","Mean", tmp)
  tmp = gsub("std","StandardDev", tmp)
  tmp = gsub("Mag", "Magnitude",tmp)
  tmp = gsub("Freqreqreqreq", "Freq",tmp)
  tmp = gsub("BodyBody", "Body",tmp)
  column_names[i] = tmp
}
colnames(ds_master) = column_names


#### 5. Create a new data set with the averaged values of each activity for each subject
ds_tmp <- ds_master[,names(ds_master) != "activityID"]
ds_melt <- melt(ds_tmp ,id=c("subjectID", "activity"), measure.vars=names(ds_tmp[,3:ncol(ds_tmp)]))
ds_melt = ds_melt[order(ds_melt[1], ds_melt[2]),]
ds_tidy <- aggregate(ds_melt$value, list(SubjectID=ds_melt$subjectID, activity=ds_melt$activity, variableSet=ds_melt$variable), FUN=mean)
colnames(ds_tidy)[4] = "average"  


# Export the tidyData set 
filename <- paste(getwd(), "/tidy_data.txt", sep="")
write.table(ds_tidy, filename, row.names=TRUE, sep='\t')

