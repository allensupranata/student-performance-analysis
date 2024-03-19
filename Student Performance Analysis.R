# Load the necessary packages
library(ggplot2)
library(psych)
library(polycor)
library(scales)
library(dplyr)

# Read the file
PATH = "C:\\Users\\User\\OneDrive - Asia Pacific University\\Documents\\Asia Pacific University\\Semester 3\\PFDA\\PFDA Assignment-2309\\student_prediction.csv"
dfstu <- read.csv(PATH, sep = ",")
data <- dfstu
View(data)

# Factoring the data
factor(data$WORK)
data$WORK <- factor(data$WORK, levels = c(1,2), labels = c("Additional work", "Not having additional work"))
data$GRADE <- factor(data$GRADE,levels = c(0,1,2,3,4,5,6,7), labels =c("Fail","DD", "DC","CC","CB", "BB","BA", "AA"), ordered = TRUE)

data$NOTES <- factor(data$NOTES,levels = c(1,2,3), labels = c("Never","Sometimes", "Always"), ordered = TRUE)

# Analysis 1-1: Univariate analysis on students having additional work and their grades 
# Univariate bar chart count
ggplot(unvplot, aes(x = WORK, y = n)) + 
  geom_bar(stat="identity", fill='forestgreen', col='black') +
  geom_text(aes(label = n), vjust=-0.5) +
  labs(x = "Category", 
       y = "Frequency", 
       title  = "Student with Additional Work Distribution")

# Analysis 1-2: Univariate analysis on students having additional work and their grades 
# Univariate bar chart percentage
unv_plot_percentage <- data %>%
  count(WORK) %>%
  mutate(pct = n / sum(n),
         pctlabel = paste0(round(pct*100), "%"))

ggplot(unv_plot_percentage, 
       aes(x = reorder(WORK, pct), y = pct)) + 
  geom_bar(stat="identity", fill="forestgreen", color="black") +
  geom_text(aes(label = pctlabel), vjust=-0.25) +
  scale_y_continuous(labels = percent) +
  labs(x = "Work", 
       y = "Percent", 
       title  = "Student with Additional Work")

# Analysis 2-1: Bivariate analysis on students having additional work and their grades.
# Bivariate plot
biv_plot <- xtabs(~WORK+GRADE, data=data)
plot(biv_plot, main="Student with Additional Work and Their Grade", col="forestgreen")

# Analysis 2-2: Bivariate analysis on students having additional work and their grades.
# Bivariate bar chart
ggplot(data, aes(x = WORK, fill = GRADE)) + 
  geom_bar(position = "dodge")

# Analysis 2-2: Bivariate analysis on students having additional work and their grades.
# Bivariate bar chart count
brv_bar_count <- data %>%
  group_by(WORK, GRADE) %>%
  summarise(Count = n())

ggplot(brv_bar_count, aes(x = WORK, y = Count, fill = GRADE)) + 
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = Count),
            position = position_dodge(width = 0.9), vjust = -0.5, size = 3) +
  ylab("Count") +
  labs(title = "Count of Students by Additional Work and Grade") +
  theme_minimal()

# Analysis 2-2: Bivariate analysis on students having additional work and their grades.
# Bivariate bar chart percentage
brv_bar_pct <- data %>%
  group_by(WORK, GRADE) %>%
  summarise(Count = n()) %>%
  group_by(WORK) %>%
  mutate(Percentage = Count / sum(Count) * 100)

ggplot(brv_bar_pct, aes(x = WORK, y = Percentage, fill = GRADE)) + 
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = sprintf("%0.1f%%", Percentage)),
            position = position_dodge(width = 0.9), vjust = -0.5, size = 3) +
  labs(y = "Percentage",
       x = "Category",
       title = "Percentage of Students by Additional Work and Grade") +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  theme_minimal()

#Analysis 3-1: Multivariate analysis on student’s grade and their frequency of taking notes grouped by their additional work.
#Multivariate bar chart count
ggplot(data, aes(x = NOTES, fill = GRADE)) + 
  geom_bar(position = "dodge") +
  geom_text(stat = "count", aes(label = ..count..),
            position = position_dodge(width = 0.9), vjust = -0.5) +
  facet_wrap(~WORK) +
  xlab("NOTES") +
  ylab("Count") +
  labs(title = "Count of Student's Grade and their Frequency of Taking Notes Grouped by their Additional Work") +
  theme_bw()

#Analysis 3-1: Multivariate analysis on student’s grade and their frequency of taking notes grouped by their additional work.
#multivariate bar chart percentage
multivariate_bar <- data %>%
  group_by(WORK, NOTES, GRADE) %>%
  summarise(Count = n()) %>%
  group_by(WORK, NOTES) %>%
  mutate(Percentage = Count / sum(Count) * 100)

ggplot(multivariate_bar, aes(x = NOTES, y = Percentage, fill = GRADE)) + 
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = sprintf("%0.1f%%", Percentage)),
            position = position_dodge(width = 0.9), vjust = -0.5) +
  facet_wrap(~WORK) +
  xlab("NOTES") +
  ylab("Percentage") +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  theme_bw()

#Chi square test  
contingency_table<-table(data$GRADE,data$WORK)
chi_squared_result<-chisq.test(contingency_table)
print(chi_squared_result)

#Polychor test
polychor(data$GRADE, data$WORK)