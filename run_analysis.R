## Getting and Clean Data Course - Project
## The aim is to combine the training data and testing datasets to produce a 
## tidy dataset with only the mean and standard deviation for each measurement.
## A second data set with the average of each variable for each activity and 
## each subject will be created and written to a file called 
## "averageMeasurementForEachActivityAndSubject.txt".
## See "README.md" on how to set up running the scripts and some of the 
## assumptions/considerations.

library(plyr)

## Get a list of feature names from "features.txt"
featureList <- read.table("features.txt")

cleanFeatureNamesDf <- NULL

## Grab all the features that have the mean and standard deviation
for (row in 1:nrow(featureList))
{
    featureName <- featureList[row, 2]
    
    ## Check if the rawFeatureName has mean() or sd()
    hasMean <- grep("mean()", featureName, fixed = TRUE)
    hasSd <- grep("std()", featureName, fixed = TRUE)
    
    if (length(hasMean) > 0 || length(hasSd) > 0)
    {
        # Clean up domain descriptions
        featureName <- gsub("^t", "timeDom", featureName)
        featureName <- gsub("^f", "freqDom", featureName)
        
        # Give -mean() and -std() descriptive names
        featureName <- gsub("-mean()", "Mean", featureName, fixed = TRUE)
        featureName <- gsub("-std()", "StdDev", featureName, fixed = TRUE)
        
        # Give -X, -Y and -Z descriptive names
        featureName <- gsub("-X$", "Xaxis", featureName)
        featureName <- gsub("-Y$", "Yaxis", featureName)
        featureName <- gsub("-Z$", "Zaxis", featureName)
        
        # Clean up the "BodyBody" typo in the orginal featureList
        featureName <- gsub("BodyBody", "Body", featureName, fixed = TRUE)
                
        cleanFeatureNamesDf <- rbind(
            cleanFeatureNamesDf,
            data.frame(featureNum = featureList[[row, 1]],
                       oldFeatureName = featureList[[row, 2]],
                       cleanFeatureName = featureName))
    }
}

# Merge the training set and testing set in one table
trainAllFeatures <- read.table("X_train.txt")
trainOnlyMeanAndStdFeatures <- 
    trainAllFeatures[, cleanFeatureNamesDf$featureNum]

testAllFeatures <- read.table("X_test.txt")
testOnlyMeanAndStdFeatures <- testAllFeatures[, cleanFeatureNamesDf$featureNum]

onlyMeanAndStdFeatures <- rbind(trainOnlyMeanAndStdFeatures, 
                                testOnlyMeanAndStdFeatures)

# Give the features descriptive names
colnames(onlyMeanAndStdFeatures) <- cleanFeatureNamesDf$cleanFeatureName

# Activity data frames
activityLabels <- read.table("activity_labels.txt", col.names = c("activityID", "activity"))
trainActivity <- read.table("y_train.txt", col.names = c("activityID"))
trainActivityWithLabels <- join(trainActivity, activityLabels)

testActivity <- read.table("y_test.txt", col.names = c("activityID"))
testActivityWithLabels <- join(testActivity, activityLabels)

activityWithLabels <- rbind(
    data.frame(activity = trainActivityWithLabels$activity),
    data.frame(activity = testActivityWithLabels$activity))

# Merge features with activities (this is step 4 answer)
featuresAndActivities <- cbind(activityWithLabels, onlyMeanAndStdFeatures)

# Get a list of training and testing subjects
subjectTrainList <- read.table("subject_train.txt", col.names = "subject")
subjectTestList <- read.table("subject_test.txt", col.names = "subject")
subjectList <- rbind(subjectTrainList, subjectTestList)

subjectAndActivitiesAndFeatures <- cbind(subjectList, featuresAndActivities)

## For each subject and each activity, calculate the average of all features
avgFeaturesBySubjectAndActivity <- ddply(subjectAndActivitiesAndFeatures,
                                         .(subject, activity),
                                         colwise(mean))

# Change the variable name to relect the term "AverageBySubjectAndActivity"
colNamesBySubAndAct <- lapply(
    colnames(avgFeaturesBySubjectAndActivity), 
    function(colName)  {
        if (colName != "subject" && colName != "activity")
        {
            colName <- paste(colName, "AvgBySubjAndAct", sep = "")        
        }
        
        colName
    })

colnames(avgFeaturesBySubjectAndActivity) <- colNamesBySubAndAct

# Write to file
write.table(avgFeaturesBySubjectAndActivity,
            file = "avgFeatBySubjAndAct.txt",
            row.names = FALSE)
