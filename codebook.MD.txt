---
title: "Getting and Cleaning Data Course Project"
author: "Zachary Piazza"
date: "04/04/2016"
output:
   The code "run_analysis.R" creates the output "tidy_data.txt" which is created in the working directory
---

## Project Description
The purpose of this project was to demonstrate the ability to collect, work with, and clean a data set. The goal was to prepare tidy data that can be used for later analysis. 

## Source Data
The data set was gathered from the following website:
http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones

The source data can be acquired here:
http://archive.ics.uci.edu/ml/machine-learning-databases/00240/

The data described movement tracked by the smartphones of 30 individuals. Each entry relates a specific movement activity to various measurements taken by the smartphone's accelerometer system.

##Section 1. Merge the training and the test sets to create one data set.

load relevant libraries for R, download the data and load it into R, read in the tables from the following source files:

features.txt
activity_labels.txt
subject_train.txt
x_train.txt
y_train.txt
subject_test.txt
x_test.txt
y_test.txt

Following this, create a dataset which merges the data of both the training and test sets.

##Section 2. Extract only the measurements on the mean and standard deviation for each measurement.

Create a logical vector by searching for only data relevant to standard deviations and means
Use this logical vector to subset the merged dataset created in Section 1.

##Section 3. Use descriptive activity names to name the activities in the data set.

Use the data in the activity_labels.txt file to label the dataset with meaningful names for each activity

##Section 4. Appropriately labels the data set with descriptive variable names.

Use a series of gsub commands to clean up the names of the data labels (columns in the dataset) 

##Section 5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

Construct a new dataset by "melting" and aggregating the master dataset. This new dataset should only have an average value for each measurement variable.


##Description of the resultant tidy_data.txt file
General description of the file including:
 The data set is 4 columns and 11880 rows
 The data provides the average values for a number of variables related to movement tracked by smart phones
 There were 30 different people whose movement data were gathered from


