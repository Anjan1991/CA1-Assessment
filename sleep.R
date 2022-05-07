# Stress and sleep analysis
# Analysing different features associated with sleeping habits
# to determine if the sleeping habits effect stress level in a human being

# Loading sleep data to a dataframe
sleep_df <- read.csv("sleep.csv")

# Displaying dataframe and structure of dataframe
sleep_df
str(sleep_df)

# Renaming the columns as per medical terms
names(sleep_df) <- c("Snoring_Rate",
                     "Respiration_Rate",
                     "Body_Temp",
                     "Limb_Movement",
                     "Blood_Oxy_Level",
                     "Rapid_Eye_Movement",
                     "Sleeping_Hours",
                     "Heart_Rate",
                     "Stress_Level")

# Displaying dataframe after renaming columns
sleep_df

# Finding the no of rows in the dataframe
nrow(sleep_df)

# ----------------------------------------------------------
# From the initial looks of the dataframe we can see
# --that there are 630 records.
# --variable 1 to 8 is continuous in nature. 
# --variable 9 is categorical.
# --Also there are no missing values.
# --Sleeping_hours variable does have 0's however, those are
#   outliers which will be shown in the normality test.
# ----------------------------------------------------------

# Finding summary of the sleep dataframe
summary(sleep_df)

# I'm examining body sleeping_hours and stress_levels
# so I will be preparing both variables first
# changing stress_level to a factor variable
# as it seems to be a categorical variable
# The sleeping_hours variable is a continuous variable, and it is 
# in numeric format already so does not need to be converted

# labels starts with what is assigned to lower value first
# eg 0 = normal, 1 = low, 2 = medium, 3 = medium high, 4 = high
# The above levels have been taken from the instructions provided
# in the assignment sheet

sleep_df$Stress_Level <- factor(sleep_df$Stress_Level,
                                labels = c("low",
                                           "normal",
                                           "intermediate",
                                           "medium",
                                           "high"),
                                ordered = TRUE
                                )
# displaying dataframe and structure of dataframe after converting
# the categorical variable to factor variable
sleep_df                                                                  
str(sleep_df)

# ----------
# EDA
# ----------

# checking for missing values in the sleep dataframe
sum(is.na(sleep_df))

# Choosing the optimum variable required to
# answer the research questions
include_list <- names(sleep_df) %in% c("Snoring_Rate",
                                       "Body_Temp",
                                       "Blood_Oxy_Level",
                                       "Sleeping_Hours",
                                       "Heart_Rate",
                                       "Stress_Level")
include_list

modified_sleep_df <- sleep_df[(include_list)]
modified_sleep_df

# Displaying the summary of snoring_range, 
# body_temp, blood_oxy_level, sleeping hours,
# and heart rate as these are my target variables
# which i will be using to answer my hypothesis
summary(modified_sleep_df)

# Sorting modified_sleep_df dataframe by stress_level variable
# and storing it into new dataframe called sorted_sleep_df
sorted_sleep_df <- modified_sleep_df[order(modified_sleep_df$Stress_Level),]
sorted_sleep_df

# Finding the categorical and numerical variables
# -----------numerical variables----------------------
unique(sorted_sleep_df$Snoring_Rate)
unique(sorted_sleep_df$Body_Temp)
unique(sorted_sleep_df$Blood_Oxy_Level)
unique(sorted_sleep_df$Sleeping_Hours)
unique(sorted_sleep_df$Heart_Rate)

#-------------categorical variables--------------------
unique(sorted_sleep_df$Stress_Level)


# We try to check the histogram of each variables in our dataframe

install.packages("ggplot2")
library(ggplot2)
library(viridis)
max_Snoring_Range <- max(sorted_sleep_df$Snoring_Rate)
max_Snoring_Range
min_Snoring_Range <- min(sorted_sleep_df$Snoring_Rate)
min_Snoring_Range
hist(sorted_sleep_df$Snoring_Rate,
     main = "Histogram for snoring rate",
     xlab = "Snoring Rate")

max_body_temp <- max(sorted_sleep_df$Body_Temp)
max_body_temp
min_body_temp <- min(sorted_sleep_df$Body_Temp)
min_body_temp
hist(sorted_sleep_df$Body_Temp,
     main = "Histogram for body tempearture",
     xlab = "Body Temperature", col = "light green")


max_blood_oxy_level <- max(sorted_sleep_df$Blood_Oxy_Level)
max_blood_oxy_level
min_blood_oxy_level <- min(sorted_sleep_df$Blood_Oxy_Level)
min_blood_oxy_level
hist(sorted_sleep_df$Blood_Oxy_Level,
     main = "Histogram for blood oxy level",
     xlab = "blood oxy level", col = "blue")


max_sleeping_hours <- max(sorted_sleep_df$Sleeping_Hours)
max_sleeping_hours
min_sleeping_hours <- min(sorted_sleep_df$Sleeping_Hours)
min_sleeping_hours
hist(sorted_sleep_df$Sleeping_Hours,
     main = "Histogram for sleeping hours",
     xlab = "sleeping hours", col = "yellow")


max_heart_rate <- max(sorted_sleep_df$Heart_Rate)
max_heart_rate
min_heart_rate <- min(sorted_sleep_df$Heart_Rate)
min_heart_rate
hist(sorted_sleep_df$Heart_Rate,
     main = "Histogram for heart rate",
     xlab = "heart rate", col = "red")

# Through initial looks data is not normally
# distributed for all of the continuous variables

# Counting no of records for each levels of stress
count_stress_level <- table(sleep_df$Stress_Level)
count_stress_level
# Showing the distribution of stress levels across the dataset
barplot(count_stress_level, 
        main = "distribution of stress levels", 
        col = "Light Blue", 
        xlab = "Stress Level", 
        ylab = "Count of Stress Level"
        )
# So from this bar plot we can see that the count of each
# levels of stress is equally distributed across the dataset

# Now here i will try to find the correlation between variables
# which will give a better understanding of the distribution of each variable

# Using the default pairs() option 
# to examine correlations between variables
pairs(sorted_sleep_df,
      labels = colnames(sorted_sleep_df),
      main = "Sleep dataset correlation plot"
      )

# This plot does not give a clear idea about the correlation between variables

# Now we will try to check for correlation for the data with pairs.panel
install.packages("psych")
library(psych)

pairs.panels(sorted_sleep_df,
             smooth = TRUE,      # If TRUE, draws loess smooths
             scale = FALSE,      # If TRUE, scales the correlation text font
             density = TRUE,     # If TRUE, adds density plots and histograms
             ellipses = TRUE,    # If TRUE, draws ellipses
             method = "spearman",# Correlation method (also "pearson" or "kendall")
             pch = 21,           # pch symbol
             lm = FALSE,         # If TRUE, plots linear fit rather than the LOESS (smoothed) fit
             cor = TRUE,         # If TRUE, reports correlations
             jiggle = FALSE,     # If TRUE, data points are jittered
             factor = 2,         # Jittering factor
             hist.col = 4,       # Histograms color
             stars = TRUE,       # If TRUE, adds significance level with stars
             ci = TRUE)          # If TRUE, adds confidence intervals
# Through this correlation plot we can find correlation between our variables
# with some clarity to an extent
# For example Sleeping hours has strong negative correlation with stress level
# which means that more the stress level, less will be the sleeping hours.

# -------------Research Questions---------------
# Q1. We want to examine if the stress level affects the sleeping hours?
#  So, H0:Sleeping hours is not affected by stress level
#      H1:Stress level affects sleeping hours
# Q2. Does stress level cause changes to body temperature?
#      H0:Stress level has no effect on body temp
#      H1:Body temperature varies with the level of stress in a human
# Q3. Does the snoring range in a human gets affected by the stress level?
#      H0:Stress level has no impact on snoring range
#      H1:Stress level has an impact on snoring range
# Q4. Does variation in body temperature causes variation in heart rate?
#      H0:Body temperature has no effect on heart rate
#      H1:Body temperature has effect on heart rate
# Q5. Does stress level affect blood oxygen level?
#      H0:Stress level has no effect on blood oxygen level
#      H1:Stress level has effect on blood oxygen level
# --------------------------------------------------------------------------



# ----------------------------------------------------------------

#               Checking for normality in the data

# ----------------------------------------------------------------

# Here i will check the normality of the data more formally
# for each of the variables that i will be using 
# to answer my hypothesis

# For that i need to first identify dependent 
# and independent variables for each of the
# hypothesis question

# Question 1: 
# Here i am dealing with sleeping_hours
# which is my dependent variable and
# continuous in nature and stress_level
# which is my independent variable which is
# categorical in nature

normality_test <- shapiro.test(sorted_sleep_df$Sleeping_Hours)
normality_test$p.value

# So we can see that p-value is less than 0.05(p = ), 
# therefore our dependent variable is not normally distributed

# Now its time to choose our statistical test to perform our first
# hypothesis testing=

# We can see that we have more than 2 levels of the independent variable
# and a continuous dependent variable. So consulting the statistical test chart
# i will be using the Kruskal Wallis test as the dependent variable is not
# normally distributed

# Format = kruskal.test(dependent Var, Independent Var)
kruskal.test(sorted_sleep_df$Sleeping_Hours, sorted_sleep_df$Stress_Level)

# p-value is < 0.05 so we reject H0 and conclude that
# sleeping hours is affected by stress level(p = 2.2e-16)


# ------------------------------------------------------------------------


# Question 2
