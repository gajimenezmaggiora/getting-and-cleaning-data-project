# Getting and Cleaning Data Course Project

This repository contains the following files:

* CodeBook.md
* README.md
* run_analysis.R

## CodeBook.md

This file contains a complete list of variable names, definitions and units that describe the
output data file **output.csv** generated by executing the **run_analisys.R** script.

## README.md

This file describes the contents of the current repository and provides a description of the
steps applied by the **run_analysis.R** script to clean and transform the raw UCI HAR Dataset.

## run_analysis.R

### Description

This R script is designed to generate a tidy, analysis-ready data set **output.csv** from the raw UCI HAR Dataset.
The script applies the following steps:

1. Merges the raw training and test data sets
2. Attaches meaningful variable names based on the vector features lables described in
the **features.txt** file.
3. Labels the activity codes using the descriptive activity names listed in the **activity_labels.txt** file.
4. Subsets the merged data set to include only the variables associated with mean and standard deviation
  * This is done by selecting only variable names that contain 'mean' or 'std' key words.
5. Creates a final data set by averaging each variable by subject and activity.  The resulting data set
is written out to the **output.csv** file.

### How to execute run_analysis.R

1. Download and unzip the UCI HAR Dataset from the following URL:
  * https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip
  * NOTE: Make sure the **UCI HAR Data directory** and **run_analysis.R** are both in your working directory.
2. Execute **run_analysis.R**
  * The resulting analysis data set **output.csv** will be written in the current working directory.  A complete
  description of this data set can be found in the **CodeBook.md** file.
