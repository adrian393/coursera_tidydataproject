# load packages
library(dplyr)
library(tidyr)
library(reshape)
library(data.table)

# read zip folder in temporory file
fileurl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
temp <- tempfile()
download.file(fileurl,temp)

# load variable data frame (feature)
features <- read.table(unzip(temp,"UCI HAR Dataset/features.txt"))
features <- features%>%
  mutate(V1=paste0("V",V1),variable=gsub("\\(\\)","",V2))

# load activity data frame
act <- read.table(unzip(temp,"UCI HAR Dataset/activity_labels.txt"))
act <- mutate(act,V2=tolower(V2))

# obtain variable code that represents mean, std
varcode <- features[grep("mean|std",features$variable),"V1"]

# function for loading and tidying data
tidyfunction <- function(subjecturl, labelurl, dataurl){
  ## subject data
  sub <- read.table(unzip(temp,subjecturl))
  ### give a more meaningful variable name
  sub <- sub%>%
    mutate(subject=V1)%>%
    select(subject)
  
  ## label data
  lab <- read.table(unzip(temp,labelurl))
  ### join with activity labels
  lab <- left_join(lab,act)
  lab <- lab%>%
    mutate(activity=V2)%>%
    select(activity)
  
  ## main data set with values
  dat <- read.table(unzip(temp,dataurl))
  ### merge subject, label and data sets
  dat <- do.call(cbind, list(sub,lab,dat))
  ### reshape data
  dat2 <- melt(setDT(dat), id=c("subject","activity"),variable.name="var")
  ### extract mean, std; and add meaningful variable names from 'features'
  dat2 <- dat2%>%
    filter(var%in%varcode)%>%
    left_join(features, by=c("var"="V1"))%>%
    select(subject,activity,variable,value)
}

# training set
train <- tidyfunction(
  subjecturl="UCI HAR Dataset/train/subject_train.txt",
  labelurl="UCI HAR Dataset/train/y_train.txt",
  dataurl="UCI HAR Dataset/train/X_train.txt"
)
## add column 'set' to identify training set
train <- mutate(train, set="training")

# test set
test <- tidyfunction(
  subjecturl="UCI HAR Dataset/test/subject_test.txt",
  labelurl="UCI HAR Dataset/test/y_test.txt",
  dataurl="UCI HAR Dataset/test/X_test.txt"
)
## add column 'set' to identify test set
test <- mutate(test, set="test")

# merge training and test sets
tt <- rbind(train,test)

# calculate mean of each variable, activity, subject and set
tt <- tbl_df(tt) #convert to tibble
## factorise all columns except value
cols <- names(tt)[-which(names(tt)=="value")]
tt[cols] <- lapply(tt[cols],factor)
# calculate mean per group
tt <- tt%>%
  group_by(subject,activity,variable,set)
ttmean <- summarise(tt,mean=mean(value))

# ttmean should give the average of each variable for each activity and each subject, according to set type (training vs test)

write.table(ttmean, file="step5.txt", row.names = FALSE)
