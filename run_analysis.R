# opening and reading the files

test_url <- "~/Downloads/UCI HAR Dataset/test/X_test.txt"
train_url <-"~/Downloads/UCI HAR Dataset/train/X_train.txt"

open_test <- file(test_url)
open_train <- file(train_url)

rtest <- read.table(open_test)
rtrain <- read.table(open_train)

# renaming columns
features_url <- "~/Downloads/UCI HAR Dataset/features.txt"
features <- read.table(features_url, stringsAsFactors = FALSE)
nms <- features[, 2]

colnames(rtest) <- nms
colnames(rtrain) <- nms

# adding subject column

x <- "~/Downloads/UCI HAR Dataset/test/subject_test.txt"
x <- read.table(x)
rtest <- cbind(rtest,x)

y <- "~/Downloads/UCI HAR Dataset/train/subject_train.txt"
y <- read.table(y)
rtrain <- cbind(rtrain,y)

# step1. merging the two data frames

df <- rbind(rtest,rtrain)
names(df)[names(df)=="V1"] <- "Subject"

# step2. extracting the measurements of mean and standard deviation of each measurement

## gathering both measurements in only one data frame
measures <- df[,grep("mean\\(\\)|std\\(\\)|Subject",names(df),value = FALSE)]

## extracting only means
means <- df[,grep("mean\\(\\)",names(df),value = FALSE)]

## extracting only standard deviation
standardev <- df[,grep("std\\(\\)",names(df),value = FALSE)]

# step3. using descriptive activity names to name the activities in the data set

## since i merged the rtrain to the rtrain, the order of the names must be: first the
## names of activities carryed in the train and after that in the test
ac_ID_test <- "~/Downloads/UCI HAR Dataset/test/y_test.txt"
ac_ID_test <- read.table(ac_ID_test)

ac_ID_train <- "~/Downloads/UCI HAR Dataset/train/y_train.txt"
ac_ID_train <- read.table(ac_ID_train)

## gathering together the names of test and train
ac_ID <- rbind(ac_ID_test,ac_ID_train)

## addressing the IDs to the df data set, producing the new_df data set

new_df <- cbind(measures,ac_ID)
names(new_df)[names(new_df)=="V1"] <- "Activity.ID"

## taking the activity labels

ac_labels <- "~/Downloads/UCI HAR Dataset/activity_labels.txt"
ac_labels <- read.table(ac_labels)

## addressing the labels to IDs

new_df$Activity.Name = factor(new_df[,"Activity.ID"],
                              levels = ac_labels[,1],
                              labels = ac_labels[,2])

# step4. already had been made, but, just for better organization,
# I'll put the "Suject", "Activity.ID" and "Activity.Name" from
# the bottom to the top

## just to assure the positions of these three collumns

new_df <- new_df[, c(
        which(names(new_df) == "Subject"),
        which(names(new_df) == "Activity.ID"),
        which(names(new_df) == "Activity.Name"),
        which(!names(new_df) %in% c("Subject", "Activity.ID", "Activity.Name"))
)]

# step5. creating the new data frame, tidy and with the average of each variable
# grouped by Subject, Activity.ID and Activity.Name

df_tidy <- new_df %>%
        group_by(Subject, Activity.ID, Activity.Name) %>%
        summarise(
                across(everything(),
                        mean
                )
        )