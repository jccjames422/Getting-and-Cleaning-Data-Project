library(dplyr)

# Read the data
################################################################################################

X_test <- read.table("./data/test/X_test.txt")
Y_test <- read.table("./data/test/Y_test.txt")
subject_test <- read.table("./data/test/subject_test.txt")

X_train <- read.table("./data/train/X_train.txt")
Y_train <- read.table("./data/train/Y_train.txt")
subject_train <- read.table("./data/train/subject_train.txt")

activity_labels <- read.table("./data/activity_labels.txt")

# Merge the training and test sets to create one data set
################################################################################################

test_data <- cbind(subject_test, Y_test, X_test)
train_data <- cbind(subject_train, Y_train, X_train)
complete_data <- rbind(test_data, train_data)

# Appropriately label the data set with descriptive variable names
################################################################################################

column_names <- readLines("./data/features.txt")    # Grabs a character vector with the names of the columns
new_names <- c("subjectid", "activityname")
total_column_names <- c(new_names, column_names)    # Adds the two additional column names to the front of the vector
names(complete_data) <- total_column_names          # Updates the names of the columns in the complete_data set

# Extract only the meaurements on the mean and standard deviation for each measurement
################################################################################################

columns_to_extract <- grep("(subjectid)|(activityname)|(.+-mean\\(\\)-.+)|(.+-std\\(\\)-.+)", total_column_names)
selected_data <- complete_data[ ,columns_to_extract]

# Change the data.frame(s) to tbl_df for use with dplyr package
################################################################################################
selected_data <- tbl_df(selected_data)
activity_labels <- tbl_df(activity_labels)

# Get the descriptive activity names to name the activities in the data set.
# These activities are labeled 1 through 6 in the data set.  I am joining the activity_labes
# data frame with the selected data frame to make a new column with the actual names
# of the activities.  Then the original column contianing the 1 through 6 values is then deleted.
################################################################################################

activity_labels <- rename(activity_labels, activityname = V1, activitylabel = V2)
labeled_data <- inner_join(activity_labels, selected_data)
names <- names(labeled_data)
names <- sub("-", "", names)
names(labeled_data) <- names

labeled_data <- labeled_data %>%
        select(-activityname) %>% 
        group_by(subjectid, activitylabel) %>%
        summarize_each(funs(mean))

write.table(labeled_data, file = "result.txt", row.names = FALSE)