library(dplyr)
# merge file function reads given two files
# and merge into single one 
# and returns the merged data set
mergeFile<-function(xfileName,yfileName){
  xdata<- read.table(xfileName) #read file one as table
  ydata<- read.table(yfileName) # read file two as table
  wholedata<- rbind(xdata,ydata)# merge rows of two sets into one
  wholedata
}

getSelectedColumns<-function(variablesFileName,columnRegex){
  vnames<- read.table(variablesFileName) #read variables
  selectedVnames<-vnames[grep(columnRegex,vnames[,2]),] #apply regex on vnames
  selectedVnames
}


# reads variable names from file 
# filter it using regex
# retun filtered variables from data
getColumnsData<-function(data,variablesFileName,columnRegex){
  selectedVnames<- getSelectedColumns(variablesFileName,columnRegex)
  data[,selectedVnames[,1]] #select interested data
}





#merge data
  #merge X data
  xdata<- mergeFile("./UCI HAR Dataset/train/X_train.txt","./UCI HAR Dataset/test/X_test.txt")
  
  #merge y data
  ydata<- mergeFile("./UCI HAR Dataset/train/Y_train.txt","./UCI HAR Dataset/test/Y_test.txt")
  
  #merge subject data
  subjectData<- mergeFile("./UCI HAR Dataset/train/subject_train.txt","./UCI HAR Dataset/test/subject_test.txt")

  
  validRegexForVariableFetching<-"mean\\(\\)|std"
  
  #filter selected mean and std data
  selectedXdata<- getColumnsData(xdata,"./UCI HAR Dataset/features.txt",validRegexForVariableFetching)
  
  #Read activity labels
  actiCollabelsForydata <- read.table("./UCI HAR Dataset/activity_labels.txt")
  
  #Name the activities
  ydata$activityName <- factor(ydata$V1, labels = as.character(actiCollabelsForydata[,2]))
  activityName <- ydata[,-1]
  
  #change column names of x data
  colnames(xdata)<-unique(read.table("./UCI HAR Dataset/features.txt")[,2])
  
  xdata<- xdata[,!is.na(colnames(xdata))]
  colnames(subjectData) <- "subject"
  wholeData <- cbind(xdata, activityName, subjectData)
  wholeDataMean <- wholeData %>% group_by(activityName, subject) %>%  summarize_each(funs(mean))
  write.table(wholeDataMean, file = "./UCI HAR Dataset/tidydata.txt", row.names = FALSE, col.names = TRUE)
  
  
  
  
  