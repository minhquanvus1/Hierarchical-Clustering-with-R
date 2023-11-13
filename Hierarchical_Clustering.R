# read data from .csv, and load to a dataframe
library(readr)
data <- read_csv("C:/Users/ASUS/Downloads/Elective Subject Quiz (Responses) - Form Responses 1.csv")
head(data)

# remove "timestamp" column
data <- data %>%
  select(-Timestamp)
head(data)

# Rename Columns in the dataframe
column_names <- colnames(data)
print(column_names)
column_mapping <- c(
  "What is your age?\nTuổi của bạn là bao nhiêu?" = "Age",
  "Which year are you in?\nBạn là sinh viên năm mấy?" = "Intake",
  "What is your gender?\nGiới tính của bạn là gì?" = "Gender",
  
  "What is your major or field of study?\nChuyên ngành học của bạn là gì?" = "Major",
  "Are you involved in any extracurricular activities? (Select all that apply)\nBạn có tham gia vào các hoạt động ngoại khóa nào không? (Hãy chọn tất cả những hoạt động bạn tham gia)" = "Activities",
  "How many hours per week, on average, do you spend on extracurricular activities? (Example: Sports, Club, Project,...)\nTrung bình mỗi tuần, bạn dành bao nhiêu giờ cho các hoạt động ngoại khóa?\nPlease insert number of hours" = "Frequency",
  "What is your current GPA? - German Scale 1.0-4.0\n(All information is secured and non-disclosed and only used for educational and research purposes)\nĐiểm trung bình hiện tại hoặc tình hình học tập của bạn là gì? (Tất cả thông tin được bảo mật và không được tiết lộ, chỉ được sử dụng cho mục đích giáo dục và nghiên cứu)" = "GPA",
  "How satisfied are you with your academic performance? \nBạn cảm thấy hài lòng như thế nào với hiệu suất học tập của mình?" = "Academic_Satisfaction",
  
  "How satisfied are you with your overall university experience? (Example: Teacher dedication, friends, life quality,...)\nBạn cảm thấy hài lòng như thế nào với trải nghiệm tổng thể tại trường đại học? (Ví dụ: Sự tận tâm của giảng viên, bạn bè, chất lượng cuộc sống,...)" = "Student_Life_Satisfaction"
)
colnames(data) <- column_mapping
column_names <- colnames(data)

# Create Dummy Variables for "Gender" column
data_new_gender <- cbind(
  data,
  Male = Male,
  Female = Female,
  Prefer_Not_To_Say = Prefer_Not_To_Say
)

# View the new data frame
data_new_gender

# create Dummy Variables for "Activities" column
Clubs <- ifelse(data$Activities == 'Clubs or Organizations (Câu lạc bộ hoặc tổ chức)', 1, 0)
Side_Project <- ifelse(data$Activities == 'Side project', 1, 0)
Sports_Not_Clubs_Based <- ifelse(data$Activities == 'Sports (not club-based) (Thể thao không thuộc câu lạc bộ )', 1, 0)
Volunteers <- ifelse(data$Activities == 'Volunteer Work (Hoạt động tình nguyện)', 1, 0)
No_Activities <- ifelse(data$Activities == 'No', 1, 0)

data_new_gender_activities <- cbind(
  data_new_gender,
  Clubs = Clubs,
  Side_Project = Side_Project,
  Sports_Not_Clubs_Based = Sports_Not_Clubs_Based,
  Volunteers = Volunteers,
  No_Activities = No_Activities
)

# View the new data frame
data_new_gender_activities

# create Dummy Variables for "Major" column
CSE <- ifelse(data$Major == 'CSE', 1, 0)
MEN <- ifelse(data$Major == 'MEN', 1, 0)
BFA <- ifelse(data$Major == 'BFA', 1, 0)
ARC <- ifelse(data$Major == 'ARC', 1, 0)
BCE <- ifelse(data$Major == 'BCE', 1, 0)
BBA <- ifelse(data$Major == 'BBA', 1, 0)
ECE <- ifelse(data$Major == 'ECE', 1, 0)

data_gender_activities_major <- cbind(data_activities_test,
                                      CSE = CSE,
                                      MEN = MEN,
                                      ECE = ECE,
                                      BFA = BFA,
                                      BBA = BBA,
                                      ARC = ARC,
                                      BCE = BCE
)


# create Dummy (Category) Variables for Academic Satisfaction, and Student Life Satisfaction column
data_gender_activities_major$Academic_Satisfaction_Low <- ifelse(data_gender_activities_major$Academic_Satisfaction >= 1 & data_gender_activities_major$Academic_Satisfaction <= 3, 1, 0)
data_gender_activities_major$Academic_Satisfaction_Medium <- ifelse(data_gender_activities_major$Academic_Satisfaction >= 4 & data_gender_activities_major$Academic_Satisfaction <= 7, 1, 0)
data_gender_activities_major$Academic_Satisfaction_High <- ifelse(data_gender_activities_major$Academic_Satisfaction >= 8 & data_gender_activities_major$Academic_Satisfaction <= 10, 1, 0)
data_gender_activities_major

data_gender_activities_major$Student_Life_Satisfaction_Low <- ifelse(data_gender_activities_major$Student_Life_Satisfaction >= 1 & data_gender_activities_major$Student_Life_Satisfaction <= 3, 1, 0)
data_gender_activities_major$Student_Life_Satisfaction_Medium <- ifelse(data_gender_activities_major$Student_Life_Satisfaction >= 4 & data_gender_activities_major$Student_Life_Satisfaction <= 7, 1, 0)
data_gender_activities_major$Student_Life_Satisfaction_High <- ifelse(data_gender_activities_major$Student_Life_Satisfaction >= 8 & data_gender_activities_major$Student_Life_Satisfaction <= 10, 1, 0)


new_data_test <- new_data %>%
  select(-Academic_Satisfaction_Low)

new_data_test <- new_data %>%
  select(-Student_Life_Satisfaction_Low)

# Change intake values to numeric
intake_mapping <- c(
  "First year of University (Năm nhất)" = 1,
  "Second year of University (Năm hai)" = 2,
  "Third year of University (Năm ba)" = 3,
  "Fourth Year of University (Năm bốn)" = 4,
  "Fifth" = 5  # Assign the numeric value for "Fifth"
)

# Use the mapping to update the "Intake" column
data_gender_activities_major$Intake <- intake_mapping[data_gender_activities_major$Intake]

column_names <- colnames(data_gender_activities_major)

# select necessary variables to a new dataframe for analysis
relevant_variables <- c(
  "Frequency",
  "GPA",
  "Academic_Satisfaction_Low",
  "Academic_Satisfaction_Medium",
  "Academic_Satisfaction_High",
  "Student_Life_Satisfaction_Low",
  "Student_Life_Satisfaction_Medium",
  "Student_Life_Satisfaction_High"
)

library(dplyr)

# Create a new data frame with the selected variables
new_data <- data_gender_activities_major[relevant_variables]

new_data_test <- new_data_test %>%
  select(-Academic_Satisfaction_Low) 
new_data_test <- new_data_test %>%
  select(-Student_Life_Satisfaction_Low) 

# Draw the Dendrogram (using "Complete Linkage", "Euclidean" Distance)
hc_new_data <- new_data_test %>% #get cars data
  dist %>% #compute distance/dissimilarity matrix
  hclust #compute hierarchical cluster
plot(hc_new_data) #plot dendogram
# cut the Dendrogram to create 6 clusters
rect.hclust(hc_new_data, k = 6, border = "darkred")

# Remove datapoint 33th (due to Outlier), and re-draw the Dendrogram
new_data_after <- new_data_test[-33,]
hc_new_data_after <- new_data_after %>%
  dist %>%
  hclust
plot(hc_new_data_after)
# cut the Dendrogram to create 5 clusters
rect.hclust(hc_new_data_after, k = 5, border = 2:5)

# extract observations from Cluster 1 to a dataframe for analysis
# cluster 1
cluster_1 <- new_data_after[c(8,23,12,31,15),]
summary(cluster_1)
plot(cluster_1)

# Draw the Histogram for Frequency, and GPA Distribution of students in Cluster 1
hist(cluster_1$GPA, 
     main = "Distribution of GPA of Cluster 1",
     xlab = "GPA",
     ylab = "Frequency",
     col = "skyblue",
     border = "black")

hist(cluster_1$Frequency, 
     main = "Distribution of Hours per Week of participating in Extracurricular Activities of Cluster 1",
     xlab = "Number of Hours per Week",
     ylab = "Frequency",
     col = "skyblue",
     border = "black")


# extract observations of Cluster 1, from the original dataframe
# to draw the Pie chart showing percentage of students basing on Academic Satisfaction Level

selected_records <- c(23, 8, 12, 15,31)

# Logical indexing to select rows
subset_df <- data_gender_activities_major[selected_records, ]

categorize_academic_satisfaction <- function(level) {
  if (level >= 1 & level <= 3) {
    return("Low")
  } else if (level >= 4 & level <= 7) {
    return("Medium")
  } else if (level >= 8 & level <= 10) {
    return("High")
  } else {
    return("Invalid")
  }
}

subset_df$Academic_Satisfaction_Category <- sapply(subset_df$Academic_Satisfaction, categorize_academic_satisfaction)

library(ggplot2)
ggplot(subset_df, aes(x = "", fill = Academic_Satisfaction_Category)) +
  geom_bar(width = 1) +
  geom_text(stat = "count", aes(label = paste(round((..count..)/sum(..count..)*100), "%")), 
            position = position_stack(vjust = 0.5)) +
  coord_polar(theta = "y") +
  labs(title = "Academic Satisfaction Levels of Cluster 1", fill = "Category") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5)  # Center the title
  )


# extract observations of Cluster 1 from the original dataframe
# to draw the Pie chart to see the percentage of students basing on Student Life Satisfaction Level
selected_records_sl <- c(23, 8, 12, 15,31)

subset_df_sl <- data_gender_activities_major[selected_records_sl, ]
subset_df_sl

categorize_student_life_satisfaction <- function(level) {
  if (level >= 1 & level <= 3) {
    return("Low")
  } else if (level >= 4 & level <= 7) {
    return("Medium")
  } else if (level >= 8 & level <= 10) {
    return("High")
  } else {
    return("Invalid")
  }
}

subset_df_sl$Student_Life_Satisfaction_Category <- sapply(subset_df_sl$Student_Life_Satisfaction, categorize_student_life_satisfaction)

library(ggplot2)
ggplot(subset_df_sl, aes(x = "", fill = Student_Life_Satisfaction_Category)) +
  geom_bar(width = 1) +
  geom_text(stat = "count", aes(label = paste(round((..count..)/sum(..count..)*100), "%")), 
            position = position_stack(vjust = 0.5)) +
  coord_polar(theta = "y") +
  labs(title = "Student Life Satisfaction Levels of Cluster 1", fill = "Category") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5)  # Center the title
  )


# extract observations of Cluster 2 for analysis
# cluster 2
cluster_2 <- new_data_after[c(39,40,24,28,11,16,30,37),]
summary(cluster_2)
plot(cluster_2)

# Draw the Histogram to see the Distribution of GPA, and Frequency of students in Cluster 2
hist(cluster_2$GPA, 
     main = "Distribution of GPA of Cluster 2",
     xlab = "GPA",
     ylab = "Frequency",
     col = "skyblue",
     border = "black")

hist(cluster_2$Frequency, 
     main = "Distribution of Hours per Week of participating in Extracurricular Activities of Cluster 2",
     xlab = "Number of Hours per Week",
     ylab = "Frequency",
     col = "skyblue",
     border = "black")


# extract observations of Cluster 2 from the original dataframe
# to draw the Pie chart to see percentage of students basing on Academic Satisfaction Levels
selected_records_2 <- c(39,40,24,28,11,16,30,37)


subset_df_2 <- data_gender_activities_major[selected_records_2, ]
subset_df_2

categorize_academic_satisfaction <- function(level) {
  if (level >= 1 & level <= 3) {
    return("Low")
  } else if (level >= 4 & level <= 7) {
    return("Medium")
  } else if (level >= 8 & level <= 10) {
    return("High")
  } else {
    return("Invalid")
  }
}

subset_df_2$Academic_Satisfaction_Category <- sapply(subset_df_2$Academic_Satisfaction, categorize_academic_satisfaction)

library(ggplot2)
ggplot(subset_df_2, aes(x = "", fill = Academic_Satisfaction_Category)) +
  geom_bar(width = 1) +
  geom_text(stat = "count", aes(label = paste(round((..count..)/sum(..count..)*100), "%")), 
            position = position_stack(vjust = 0.5)) +
  coord_polar(theta = "y") +
  labs(title = "Academic Satisfaction Levels of Cluster 2", fill = "Category") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5)  # Center the title
  )

# extract observations of Cluster 2 from the original dataframe
# to draw the Pie chart to see percentage of Students basing on Student Life Satisfaction Levels
selected_records_sl_2 <- c(39,40,24,28,11,16,30,37)

subset_df_sl_2 <- data_gender_activities_major[selected_records_sl_2, ]
subset_df_sl_2


categorize_student_life_satisfaction <- function(level) {
  if (level >= 1 & level <= 3) {
    return("Low")
  } else if (level >= 4 & level <= 7) {
    return("Medium")
  } else if (level >= 8 & level <= 10) {
    return("High")
  } else {
    return("Invalid")
  }
}

subset_df_sl_2$Student_Life_Satisfaction_Category <- sapply(subset_df_sl_2$Student_Life_Satisfaction, categorize_student_life_satisfaction)

library(ggplot2)
ggplot(subset_df_sl_2, aes(x = "", fill = Student_Life_Satisfaction_Category)) +
  geom_bar(width = 1) +
  geom_text(stat = "count", aes(label = paste(round((..count..)/sum(..count..)*100), "%")), 
            position = position_stack(vjust = 0.5)) +
  coord_polar(theta = "y") +
  labs(title = "Student Life Satisfaction Levels of Cluster 2", fill = "Category") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5)  # Center the title
  )

# extract Cluster 3 
# cluster 3
cluster_3 <- new_data_after[c(27,29,34,36,10,44,22,17,46,9,1,4),]
summary(cluster_3)
plot(cluster_3)

# Draw Histogram to see the Distribution of GPA, and Frequency of students in Cluster 3
hist(cluster_3$GPA, 
     main = "Distribution of GPA of Cluster 3",
     xlab = "GPA",
     ylab = "Frequency",
     col = "skyblue",
     border = "black")

hist(cluster_3$Frequency, 
     main = "Distribution of Hours per Week of participating in Extracurricular Activities of Cluster 3",
     xlab = "Number of Hours per Week",
     ylab = "Frequency",
     col = "skyblue",
     border = "black")

# extract observations of Cluster 3 from the original dataframe
# to draw the Pie chart to see percentage of students basing on Academic Satisfaction Levels
selected_records_3 <- c(27,29,34,36,10,44,22,17,46,9,1,4)


subset_df_3 <- data_gender_activities_major[selected_records_3, ]
subset_df_3


categorize_academic_satisfaction <- function(level) {
  if (level >= 1 & level <= 3) {
    return("Low")
  } else if (level >= 4 & level <= 7) {
    return("Medium")
  } else if (level >= 8 & level <= 10) {
    return("High")
  } else {
    return("Invalid")
  }
}

subset_df_3$Academic_Satisfaction_Category <- sapply(subset_df_3$Academic_Satisfaction, categorize_academic_satisfaction)

library(ggplot2)
ggplot(subset_df_3, aes(x = "", fill = Academic_Satisfaction_Category)) +
  geom_bar(width = 1) +
  geom_text(stat = "count", aes(label = paste(round((..count..)/sum(..count..)*100), "%")), 
            position = position_stack(vjust = 0.5)) +
  coord_polar(theta = "y") +
  labs(title = "Academic Satisfaction Levels of Cluster 3", fill = "Category") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5)  # Center the title
  )

# extract observations of Cluster 3 from original dataframe
# to draw Pie chart to see percentage of Students basing on Student Life Satisfaction Levels
selected_records_sl_3 <- c(27,29,34,36,10,44,22,17,46,9,1,4)

subset_df_sl_3 <- data_gender_activities_major[selected_records_sl_3, ]
subset_df_sl_3

categorize_student_life_satisfaction <- function(level) {
  if (level >= 1 & level <= 3) {
    return("Low")
  } else if (level >= 4 & level <= 7) {
    return("Medium")
  } else if (level >= 8 & level <= 10) {
    return("High")
  } else {
    return("Invalid")
  }
}

subset_df_sl_3$Student_Life_Satisfaction_Category <- sapply(subset_df_sl_3$Student_Life_Satisfaction, categorize_student_life_satisfaction)

library(ggplot2)
ggplot(subset_df_sl_3, aes(x = "", fill = Student_Life_Satisfaction_Category)) +
  geom_bar(width = 1) +
  geom_text(stat = "count", aes(label = paste(round((..count..)/sum(..count..)*100), "%")), 
            position = position_stack(vjust = 0.5)) +
  coord_polar(theta = "y") +
  labs(title = "Student Life Satisfaction Levels of Cluster 3", fill = "Category") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5)  # Center the title
  )


# extract Cluster 4
# cluster 4
cluster_4 <- new_data_after[c(25,43,18,41,5,45,3,20,19,2,7,26),]
summary(cluster_4)
plot(cluster_4)

# Draw Histogram to see the Distribution of GPA, and Frequency of students in Cluster 4
hist(cluster_4$GPA, 
     main = "Distribution of GPA of Cluster 4",
     xlab = "GPA",
     ylab = "Frequency",
     col = "skyblue",
     border = "black")

hist(cluster_4$Frequency, 
     main = "Distribution of Hours per Week of participating in Extracurricular Activities of Cluster 4",
     xlab = "Number of Hours per Week",
     ylab = "Frequency",
     col = "skyblue",
     border = "black")

# extract observations of Cluster 4 from original dataframe
# to draw Pie chart to see percentage of students basing on Academic Satisfaction Levels
selected_records_4 <- c(25,43,41,18,5,45,20,3,19,2,7,26)

# Logical indexing to select rows
subset_df_4 <- data_gender_activities_major[selected_records_4, ]
subset_df_4

categorize_academic_satisfaction <- function(level) {
  if (level >= 1 & level <= 3) {
    return("Low")
  } else if (level >= 4 & level <= 7) {
    return("Medium")
  } else if (level >= 8 & level <= 10) {
    return("High")
  } else {
    return("Invalid")
  }
}

subset_df_4$Academic_Satisfaction_Category <- sapply(subset_df_4$Academic_Satisfaction, categorize_academic_satisfaction)

library(ggplot2)
ggplot(subset_df_4, aes(x = "", fill = Academic_Satisfaction_Category)) +
  geom_bar(width = 1) +
  geom_text(stat = "count", aes(label = paste(round((..count..)/sum(..count..)*100), "%")), 
            position = position_stack(vjust = 0.5)) +
  coord_polar(theta = "y") +
  labs(title = "Academic Satisfaction Levels of Cluster 4", fill = "Category") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5)  # Center the title
  )

# extract observations of Cluster 4 from original dataframe
# to draw Pie chart to see percentage of students basing on Student Life Satisfaction Levels
selected_records_sl_4 <- c(25,43,18,41,5,45,3,20,19,2,7,26)

# Logical indexing to select rows
subset_df_sl_4 <- data_gender_activities_major[selected_records_sl_4, ]
subset_df_sl_4


categorize_student_life_satisfaction <- function(level) {
  if (level >= 1 & level <= 3) {
    return("Low")
  } else if (level >= 4 & level <= 7) {
    return("Medium")
  } else if (level >= 8 & level <= 10) {
    return("High")
  } else {
    return("Invalid")
  }
}

subset_df_sl_4$Student_Life_Satisfaction_Category <- sapply(subset_df_sl_4$Student_Life_Satisfaction, categorize_student_life_satisfaction)

library(ggplot2)
ggplot(subset_df_sl_4, aes(x = "", fill = Student_Life_Satisfaction_Category)) +
  geom_bar(width = 1) +
  geom_text(stat = "count", aes(label = paste(round((..count..)/sum(..count..)*100), "%")), 
            position = position_stack(vjust = 0.5)) +
  coord_polar(theta = "y") +
  labs(title = "Student Life Satisfaction Levels of Cluster 4", fill = "Category") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5)  # Center the title
  )


# extract Cluster 5
# cluster 5
cluster_5 <- new_data_after[c(14,21,35,42,38,13,32,6),]
summary(cluster_5)
plot(cluster_5)


# Draw Histogram to see the Distribution of GPA, and Frequency of students in Cluster 5
hist(cluster_5$GPA, 
     main = "Distribution of GPA of Cluster 5",
     xlab = "GPA",
     ylab = "Frequency",
     col = "skyblue",
     border = "black")

hist(cluster_5$Frequency, 
     main = "Distribution of Hours per Week of participating in Extracurricular Activities of Cluster 5",
     xlab = "Number of Hours per Week",
     ylab = "Frequency",
     col = "skyblue",
     border = "black")

# extract observations of Cluster 5 from original dataframe
# to draw Pie chart to see percentage of students basing on Academic Satisfaction Levels
selected_records_5 <- c(14,21,42,35,38,32,13,6)


subset_df_5 <- data_gender_activities_major[selected_records_5, ]
subset_df_5


categorize_academic_satisfaction <- function(level) {
  if (level >= 1 & level <= 3) {
    return("Low")
  } else if (level >= 4 & level <= 7) {
    return("Medium")
  } else if (level >= 8 & level <= 10) {
    return("High")
  } else {
    return("Invalid")
  }
}

subset_df_5$Academic_Satisfaction_Category <- sapply(subset_df_5$Academic_Satisfaction, categorize_academic_satisfaction)

library(ggplot2)
ggplot(subset_df_5, aes(x = "", fill = Academic_Satisfaction_Category)) +
  geom_bar(width = 1) +
  geom_text(stat = "count", aes(label = paste(round((..count..)/sum(..count..)*100), "%")), 
            position = position_stack(vjust = 0.5)) +
  coord_polar(theta = "y") +
  labs(title = "Academic Satisfaction Levels of Cluster 5", fill = "Category") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5)  # Center the title
  )

# extract observations of Cluster 5 from original dataframe
# to draw Pie chart to see percentage of Students basing on Student Life Satisfaction
selected_records_sl_5 <- c(14,21,35,42,38,13,32,6)

subset_df_sl_5 <- data_gender_activities_major[selected_records_sl_5, ]
subset_df_sl_5


categorize_student_life_satisfaction <- function(level) {
  if (level >= 1 & level <= 3) {
    return("Low")
  } else if (level >= 4 & level <= 7) {
    return("Medium")
  } else if (level >= 8 & level <= 10) {
    return("High")
  } else {
    return("Invalid")
  }
}

subset_df_sl_5$Student_Life_Satisfaction_Category <- sapply(subset_df_sl_5$Student_Life_Satisfaction, categorize_student_life_satisfaction)

library(ggplot2)
ggplot(subset_df_sl_5, aes(x = "", fill = Student_Life_Satisfaction_Category)) +
  geom_bar(width = 1) +
  geom_text(stat = "count", aes(label = paste(round((..count..)/sum(..count..)*100), "%")), 
            position = position_stack(vjust = 0.5)) +
  coord_polar(theta = "y") +
  labs(title = "Student Life Satisfaction Levels of Cluster 5", fill = "Category") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5)  # Center the title
  )


# Whole dataset

# Draw Histogram to see the Distribution of GPA, and Frequency of students of the whole dataset
hist(data_new_gender_activities$GPA, 
     main = "Distribution of GPA of whole dataset",
     xlab = "GPA",
     ylab = "Frequency",
     col = "skyblue",
     border = "black")

hist(data_new_gender_activities$Frequency, 
     main = "Distribution of Frequency (hours/week) of whole dataset",
     xlab = "Number of Hours per Week",
     ylab = "Frequency",
     col = "skyblue",
     border = "black")

# Draw Pie chart to see percentage of students basing on Academic Satisfaction Levels
data_new_gender_activities$Academic_Satisfaction_Category <- sapply(data_new_gender_activities$Academic_Satisfaction, categorize_academic_satisfaction)

library(ggplot2)
ggplot(data_new_gender_activities, aes(x = "", fill = Academic_Satisfaction_Category)) +
  geom_bar(width = 1) +
  geom_text(stat = "count", aes(label = paste(round((..count..)/sum(..count..)*100), "%")), 
            position = position_stack(vjust = 0.5)) +
  coord_polar(theta = "y") +
  labs(title = "Academic Satisfaction Levels of whole dataset", fill = "Category") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5)  # Center the title
  )

# Draw Pie chart to see percentage of students basing on Student Life Satisfaction Levels
data_new_gender_activities$Student_Life_Satisfaction_Category <- sapply(data_new_gender_activities$Student_Life_Satisfaction, categorize_student_life_satisfaction)

library(ggplot2)
ggplot(subset_df_sl_5, aes(x = "", fill = Student_Life_Satisfaction_Category)) +
  geom_bar(width = 1) +
  geom_text(stat = "count", aes(label = paste(round((..count..)/sum(..count..)*100), "%")), 
            position = position_stack(vjust = 0.5)) +
  coord_polar(theta = "y") +
  labs(title = "Student Life Satisfaction Levels of whole dataset", fill = "Category") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5)  # Center the title
  )