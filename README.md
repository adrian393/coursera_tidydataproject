# README

The file "run_analysis.R" contains the script I used to download, prepare and tidy a data set as part of the Coursera project "Getting and Cleaning Data". The data set comes from accelerometers from the Samsung Galaxy S smartphone (futher information: http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones).

The script, through the function tempfile(), should enable the user to download all the necessary data sets WITHOUT the need to manually download the data (stored as a zip file) into a local drive.

Required packages are dplyr, tidyr, reshape, and data.table.

Two of the main outputs from the script are: (1) tt (the tidy data set merging both the test and training data sets); and (2) ttmean (averages for each activity, variable and subject, classified by data set type (i.e. training and test)).
