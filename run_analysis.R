# "You should create one R script called run_analysis.R that does the following. 
# 1. Merges the training and the test sets to create one data set.
# 2. Extracts only the measurements on the mean and standard deviation for each 
# measurement. 
# 3. Uses descriptive activity names to name the activities in the data set
# 4. Appropriately labels the data set with descriptive variable names. 
# 5. From the data set in step 4, creates a second, independent tidy data set 
# with the average of each variable for each activity and each subject."
# ------------------------------------------------------------------------------
# 
# libraries needed

# directory info
dataDir <- "UCI HAR Dataset" #this directory should be in working directory!

# 1. Merges the training and the test sets to create one data set.
# ------------------------------------------------------------------------------
#activity labels data table (6)
activityLabels <- read.table(paste0(dataDir,"/","activity_labels.txt"))
#features data table (561)
features <- read.table(paste0(dataDir,"/","features.txt"))

#charging data set function (suitable for train and test data sets)
chargeInertialSignals <- function(prefix="total_acc_", 
                                  varName="x",
                                  dataSetName="train") {
        inertialDataSet <- read.table(paste0(dataDir,"/",dataSetName,
                                       "/Inertial Signals/",prefix, varName,
                                       "_", dataSetName,".txt"))
        for(i in 1:length(colnames(inertialDataSet))) 
                colnames(inertialDataSet)[i]<-paste0(prefix,varName,i)
        inertialDataSet
}
chargeDataSet <- function(dataSetName="train") {
        dataSetName<-ifelse(dataSetName!="train" & dataSetName!="test",
                            "train",
                            dataSetName)
        subjects <- read.table(paste0(dataDir,"/",dataSetName,"/","subject_",
                                      dataSetName,".txt"))
        colnames(subjects)<-c("subject")
        activities <- read.table(paste0(dataDir,"/",dataSetName,"/","y_",
                                        dataSetName,".txt"))
        colnames(activities)<-c("activity")
        features561 <- read.table(paste0(dataDir,"/",dataSetName,"/","X_",
                                         dataSetName,".txt"))
        colnames(features561)<-sapply(features["V2"],as.character)
        totalAccX <- chargeInertialSignals("total_acc_","x", dataSetName)
        totalAccY <- chargeInertialSignals("total_acc_","y", dataSetName)
        totalAccZ <- chargeInertialSignals("total_acc_","z", dataSetName)
        bodyAccX <- chargeInertialSignals("body_acc_","x", dataSetName)
        bodyAccY <- chargeInertialSignals("body_acc_","y", dataSetName)
        bodyAccZ <- chargeInertialSignals("body_acc_","z", dataSetName)
        gyroAccX <- chargeInertialSignals("body_gyro_","x", dataSetName)
        gyroAccY <- chargeInertialSignals("body_gyro_","y", dataSetName)
        gyroAccZ <- chargeInertialSignals("body_gyro_","z", dataSetName)
        
        dataSet <- cbind(subjects, activities, features561,
                         totalAccX, totalAccY, totalAccZ,
                         bodyAccX, bodyAccY, bodyAccZ,
                         gyroAccX, gyroAccY, gyroAccZ
                         )
        dataSet
}

dataSetTest <- chargeDataSet("test") #reading data set test
dataSetTrain <- chargeDataSet("train") #reading data set train
dataSetFull <- rbind(dataSetTest,dataSetTrain) #adding rows from both tables
        

# 2. Extracts only the measurements on the mean and standard deviation for each 
# measurement. 
# ------------------------------------------------------------------------------
meanOrStdCols <- grep("mean()|std()",colnames(dataSetFull))
dataSetMeanStd <- cbind(dataSetFull[1:2],
                        dataSetFull[meanOrStdCols],
                        dataSetFull[564:1715]
                        )

# 3. Uses descriptive activity names to name the activities in the data set
# ------------------------------------------------------------------------------
colnames(activityLabels)<-c("activity","activityName")
dataSetMSActivities <- merge(dataSetMeanStd,activityLabels,
                             by.x="activity",
                             by.y="activity")
# 4. Appropriately labels the data set with descriptive variable names. 
# ------------------------------------------------------------------------------
# already done in charging datasets previously!!!!
tidyDataSet <- dataSetMSActivities

# 5. From the data set in step 4, creates a second, independent tidy data set with 
# the average of each variable for each activity and each subject.
# ------------------------------------------------------------------------------
#frequencies per subject/activity
with(tidyDataSet,table(activityName,subject))
#average of variables
xtabs(total_acc_x1 ~ activityName+subject,data=tidyDataSet) #example sum of 
                                                            #total_acc_x1
AvgDataSet <- tidyDataSet[c("subject","activity","activityName")]
colNum<-4 #we are going to calculate de average of vars per activity/subject
for(var in colnames(tidyDataSet)[4:length(colnames(tidyDataSet))]) {
        AvgDataSet$newCol <- ave(tidyDataSet[colNum-1][[1]],
                                 tidyDataSet$subject,
                                 tidyDataSet$activity,
                                 FUN=function(x) mean(x,na.rm=TRUE)
                              )
        colnames(AvgDataSet)[colNum]<-var
        colNum <- colNum+1
}
AvgDataSet <- AvgDataSet[!duplicated(AvgDataSet),] #eliminating duplicates        
        

# writting average dataset in a text file
write.table(AvgDataSet,"AvgDataSet.txt",row.names=FALSE)
