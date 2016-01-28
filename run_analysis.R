library(dplyr)

# Read the data
################################################################################################

X_test <- read.table("./data/test/X_test.txt", strip.white = TRUE, stringsAsFactors = FALSE)
Y_test <- read.table("./data/test/Y_test.txt", strip.white = TRUE, stringsAsFactors = FALSE)
subject_test <- read.table("./data/test/subject_test.txt", strip.white = TRUE, stringsAsFactors = FALSE)

X_train <- read.table("./data/train/X_train.txt", strip.white = TRUE, stringsAsFactors = FALSE)
Y_train <- read.table("./data/train/Y_train.txt", strip.white = TRUE, stringsAsFactors = FALSE)
subject_train <- read.table("./data/train/subject_train.txt", strip.white = TRUE, stringsAsFactors = FALSE)

activity_labels <- read.table("./data/activity_labels.txt", strip.white = TRUE, stringsAsFactors = FALSE)

# Step 1. Merge the training and test sets to create one data set
################################################################################################

test_data <- cbind(subject_test, Y_test, X_test)
train_data <- cbind(subject_train, Y_train, X_train)
complete_data <- rbind(test_data, train_data)

# Step 3. Use descriptive activity names to name the activities in the data set
################################################################################################

column_names <- readLines("./data/features.txt")    # Grabs a character vector with the names of the columns
new_names <- c("subjectid", "activityname")
total_column_names <- c(new_names, column_names)    # Adds the two additional column names to the front of the vector
names(complete_data) <- total_column_names          # Updates the names of the columns in the complete_data set


selected_data <- complete_data[,grep(  "(subjectid)|(activityname)|(.+-mean\\(\\)-.+)|(.+-std\\(\\)-.+)", total_column_names)]
selected_data <- tbl_df(selected_data)
activity_labels <- tbl_df(activity_labels)
activity_labels <- rename(activity_labels, activityname = V1, activitylabel = V2)
labeled_data <- inner_join(activity_labels, selected_data)
names <- names(labeled_data)
names <- sub("-", "", names)
names(labeled_data) <- names
grouped_data <- group_by(labeled_data, subjectid, activitylabel)
grouped_data <- select(grouped_data, -activityname)
summarize_each(grouped_data, funs(mean))