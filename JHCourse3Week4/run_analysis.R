## Step 1. Read the tab-delimited data into R.

library(plyr)
library(dplyr)
library(reshape)
library(stringr)
library(tidyr)

## Load metadata
features <- read.table("features.txt", header = FALSE)
activity_labels <- read.table("activity_labels.txt", header = FALSE)

## Load test data
testY <- read.table("./test/y_test.txt", sep = "\t", header = FALSE) 
testX <- read.table("./test/X_test.txt", header = FALSE) 
test_subj <- read.table("./test/subject_test.txt", sep = "\t", header = FALSE)

## Load train data
trainY <- read.table("./train/y_train.txt", sep = "\t", header = FALSE) 
trainX <- read.table("./train/X_train.txt", header = FALSE) 
train_subj <- read.table("./train/subject_train.txt", sep = "\t", header = FALSE)

## Rename columns for tidy data
names(testX) <- features$V2     ;        names(trainX) <- features$V2
names(test_subj) <- "subject"   ;        names(train_subj) <- "subject"
names(testY) <- "activity_num"  ;        names(trainY) <- "activity_num"
names(activity_labels) <- c("activity_num", "activity")


## Next steps: Build a combined data frame with...
## 2. only measurements that feature a mean or standard deviation
## 3. both train and test data
## 4. renamed activity names and variable names (to tidy data standards).

## 2.
mean_or_std <- grep("mean|std", features$V2)    ## Looks for 'mean' or 'std' in col names
trainX_subset <- trainX[,c(mean_or_std)]        ## Subsets dataframe to only include col
testX_subset <- testX[,c(mean_or_std)]          ##      names with 'mean' and 'std'

## 3.
train_data <- cbind(train_subj, trainY, trainX_subset)
test_data <- cbind(test_subj, testY, testX_subset)
all_data <- rbind(test_data, train_data)
all_data_join <- join(activity_labels, all_data) ## joining by column 'activity_num'

## 4. 
all_data_mut <- mutate(all_data_join, activity = tolower(sub("_", " ", activity))) ## convert
                                                ## activity names to lowercase and remove all
                                                ## underscores

names(all_data_mut) <- gsub("-|\\(|\\)", "", names(all_data_mut)) ## remove hyphen and 
                                                ## parentheses from col names for readability

tidy_data <- all_data_mut[, c(3, 2, 4:82)]      ## eliminate the "activity_num" column,
                                                ## swap the 'subject' and 'activity' cols,
                                                ## and keep the 'feature' subsetted variables





