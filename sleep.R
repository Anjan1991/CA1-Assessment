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

sleep_df$Stress_Level_factored <- factor(sleep_df$Stress_Level,
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
                                       "Stress_Level_factored")
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

mean(sorted_sleep_df$Snoring_Rate)
max_Snoring_Range <- max(sorted_sleep_df$Snoring_Rate)
max_Snoring_Range
min_Snoring_Range <- min(sorted_sleep_df$Snoring_Rate)
min_Snoring_Range
hist(sorted_sleep_df$Snoring_Rate,
     main = "Histogram for snoring rate",
     xlab = "Snoring Rate")
boxplot(sorted_sleep_df$Snoring_Rate,
        main = "box plot for snoring rate",
        xlab = "Snoring Rate",
        ylab = "Snoring rate value"
        )
plot(density(sorted_sleep_df$Snoring_Rate),
             main = "density plot for snoring rate",
             xlab = "Snoring Rate",
    )
polygon(density(sorted_sleep_df$Snoring_Rate), col = "grey")

mean(sorted_sleep_df$Body_Temp)
max_body_temp <- max(sorted_sleep_df$Body_Temp)
max_body_temp
min_body_temp <- min(sorted_sleep_df$Body_Temp)
min_body_temp
hist(sorted_sleep_df$Body_Temp,
     main = "Histogram for body tempearture",
     xlab = "Body Temperature", col = "light green")
boxplot(sorted_sleep_df$Body_Temp,
        main = "box plot for Body Temperature",
        xlab = "Body Temperature",
        ylab = "Body Temperature Value",
        col = "light green"
        )
plot(density(sorted_sleep_df$Body_Temp),
             main = "Density plot for Body Temperature",
             xlab = "Body Temperature",
             ylab = "Body Temperature Value",
             col = "light green"
    )
polygon(density(sorted_sleep_df$Body_Temp), col = "light green")

mean(sorted_sleep_df$Blood_Oxy_Level)
max_blood_oxy_level <- max(sorted_sleep_df$Blood_Oxy_Level)
max_blood_oxy_level
min_blood_oxy_level <- min(sorted_sleep_df$Blood_Oxy_Level)
min_blood_oxy_level
hist(sorted_sleep_df$Blood_Oxy_Level,
     main = "Histogram for blood oxy level",
     xlab = "blood oxy level", col = "blue")
boxplot(sorted_sleep_df$Blood_Oxy_Level,
        main = "Box plot for Blood Oxygen Level",
        xlab = "Blood Oxygen Level",
        ylab = "Blood Oxygen Level Value",
        col = "blue")
plot(density(sorted_sleep_df$Blood_Oxy_Level),
             main = "Density plot for blood oxygen Level",
             xlab = "Blood Oxygen Level",
             ylab = "Blood Oxygen Level Value",
             col = "blue"
     )
polygon(density(sorted_sleep_df$Blood_Oxy_Level), col = "blue")

mean(sorted_sleep_df$Sleeping_Hours)
max_sleeping_hours <- max(sorted_sleep_df$Sleeping_Hours)
max_sleeping_hours
min_sleeping_hours <- min(sorted_sleep_df$Sleeping_Hours)
min_sleeping_hours
hist(sorted_sleep_df$Sleeping_Hours,
     main = "Histogram for sleeping hours",
     xlab = "sleeping hours", col = "yellow")
boxplot(sorted_sleep_df$Sleeping_Hours,
        main = "Box plot for Sleeping Hours",
        xlab = "Sleeping Hours",
        ylab = "Sleeping Hours Value",
        col = "yellow")
plot(density(sorted_sleep_df$Sleeping_Hours),
             main = "Density plot for Sleeping Hours",
             xlab = "Sleeping Hours",
             ylab = "Sleeping Hours",
             col = "yellow"
     )
polygon(density(sorted_sleep_df$Sleeping_Hours), col = "yellow")

mean(sorted_sleep_df$Heart_Rate)
max_heart_rate <- max(sorted_sleep_df$Heart_Rate)
max_heart_rate
min_heart_rate <- min(sorted_sleep_df$Heart_Rate)
min_heart_rate
hist(sorted_sleep_df$Heart_Rate,
     main = "Histogram for heart rate",
     xlab = "heart rate", col = "red")
boxplot(sorted_sleep_df$Heart_Rate,
        main = "Box plot for heart rate",
        xlab = "Hear Rate",
        ylab = "Hear Rate",
        col = "red")
plot(density(sorted_sleep_df$Heart_Rate),
             main = "Density plot for heart rate",
             xlab = "heart rate",
             ylab = "heart rate",
             col = "red"
     )
polygon(density(sorted_sleep_df$Heart_Rate), col = "red")

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

# Extracting numeric variables for correlation with corrplot
include_list_corr <- names(sleep_df) %in% c("Snoring_Rate",
                                            "Body_Temp",
                                            "Blood_Oxy_Level",
                                            "Sleeping_Hours",
                                            "Heart_Rate",
                                            "Stress_Level"
                                          )
include_list_corr

modified_sleep_df1 <- sleep_df[(include_list_corr)]
modified_sleep_df1
# Correlation between variables using corrplot library
library(corrplot)
M <- cor(modified_sleep_df1)
corrplot(M, method = 'number')

# This method gives a very clear picture about the correlation
# between variables


# -------------Research Questions---------------
# Q1. We want to examine if the stress level affects the sleeping hours?
#      H0:Sleeping hours is not affected by stress level
#      H1:Stress level affects sleeping hours
# Q2. Does stress level cause changes to body temperature?
#      H0:Stress level has no effect on body temp
#      H1:Body temperature varies with the level of stress in a human
# Q3. Does the blood oxygen level in a human gets affected by the body temperature?
#      H0:body temperature has no impact on body temperature
#      H1:body blood oxygen has an impact on snoring range
# Q4. Does variation in body temperature causes variation in heart rate?
#      H0:Body temperature has no effect on heart rate
#      H1:Body temperature has effect on heart rate
# Q5. Does blood oxygen level affect heart rate?
#      H0:blood oxygen level has no effect on heart rate
#      H1:blood oxygen level has effect on heart rate
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
# We want to examine if the stress level affects the sleeping hours?
#      H0:Sleeping hours is not affected by stress level
#      H1:Stress level affects sleeping hours
# Here i am dealing with sleeping_hours
# which is my dependent variable and
# continuous in nature and stress_level
# which is my independent variable which is
# categorical in nature

normality_test_Q1<- shapiro.test(sorted_sleep_df$Sleeping_Hours)
normality_test_Q1$p.value

# So we can see that p-value is less than 0.05(p = 2.176741e-20), 
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


# Question 2:
# Does stress level cause changes to body temperature?
#      H0:Stress level has no effect on body temp
#      H1:Body temperature varies with the level of stress in a human

# Here my independent variable is stress level which is ordinal in nature
# and dependent variable is body temperature which is continuous in nature

# checking normality of dependent variable
normality_test_Q2 <- shapiro.test(sorted_sleep_df$Body_Temp)
normality_test_Q2$p.value

# So we can see that p-value is less than 0.05(p = 6.96876e-09), 
# therefore our dependent variable is not normally distributed

# Choosing the statistical test
# Since our dependent variable is continuous and not normally distributed
# and independent variable is categorical with more than 2 levels
# we will again perform a kruskal-wallis test to test our second hypothesis

# Format = kruskal.test(dependent Var, Independent Var)
kruskal.test(sorted_sleep_df$Body_Temp, sorted_sleep_df$Stress_Level)

# p-value is < 0.05 so we reject H0 and conclude that
# body temperature is affected by stress level(p = 2.2e-16)


# ------------------------------------------------------------------------

# Question3:
# Does the blood oxygen level in a human gets affected by the body temperature?
#      H0:blood oxygen level has no impact on body temperature 
#      H1:body blood oxygen has an impact on body temperature

# Here my independent variable is blood oxygen level which is continuous in nature
# and dependent variable is body temperature which is also continuous in nature

# checking normality of dependent variable
normality_test_Q3 <- shapiro.test(sorted_sleep_df$Body_Temp)
normality_test_Q3$p.value

# So we can see that p-value is less than 0.05(p = 6.96876e-09), 
# therefore our dependent variable is not normally distributed

# Choosing the statistical test
# Since our dependent variable is continuous and not normally distributed
# and independent variable is also continuous
# we will perform a Spearman’s Correlation Coefficient test to test our third hypothesis

# First let us find the correlation between blood oxy level and body temperature
library(corrplot)

# Extracting only blood oxy level and body temp variables and
# storing into new dataframe
include_list_cor1 <- names(sleep_df) %in% c("Blood_Oxy_Level", "Body_Temp")
include_list_cor1

new_sleep_df1 <- sleep_df[(include_list_cor1)]
new_sleep_df1

# Showing correlation plot for blood oxy level and body temperature
M <- cor(new_sleep_df)
corrplot(M, method = 'number')

# From the plot we can see that both the variables have perfect correlation

# No We will perform the Spearman’s Correlation Coefficient test
cor.test(sorted_sleep_df$Body_Temp, sorted_sleep_df$Blood_Oxy_Level, method = "spearman")

# p-value is < 0.05 so we reject H0 and conclude that
# body temperature is affected by blood oxygen level(p = 2.2e-16)


# ------------------------------------------------------------------

# Question4: Does variation in body temperature causes variation in heart rate?
#      H0:Body temperature has no effect on heart rate
#      H1:Body temperature has effect on heart rate

# Here my independent variable is body temperature which is continuous in nature
# and dependent variable is heart rate which is also continuous in nature

# checking normality of dependent variable
normality_test_Q4 <- shapiro.test(sorted_sleep_df$Heart_Rate)
normality_test_Q4$p.value

# So we can see that p-value is less than 0.05(p = 1.571483e-15), 
# therefore our dependent variable is not normally distributed

# Choosing the statistical test
# Since our dependent variable is continuous and not normally distributed
# and independent variable is also continuous
# we will perform a Spearman’s Correlation Coefficient test to test our fourth hypothesis

# First let us find the correlation between blood oxy level and body temperature
library(corrplot)

# Extracting only blood oxy level and body temp variables and
# storing into new dataframe
include_list_cor2 <- names(sleep_df) %in% c("Heart_Rate", "Body_Temp")
include_list_cor2

new_sleep_df2 <- sleep_df[(include_list_cor2)]
new_sleep_df2

# Showing correlation plot for blood oxy level and body temperature
M <- cor(new_sleep_df2)
corrplot(M, method = 'number')

# From the plot we can see that both the variables have a strong negative correlation

# No We will perform the Spearman’s Correlation Coefficient test
cor.test(sorted_sleep_df$Heart_Rate, sorted_sleep_df$Body_Temp, method = "spearman")

# p-value is < 0.05 so we reject H0 and conclude that
# Heart Rate is affected by body temperature(p = 2.2e-16)


# ------------------------------------------------------------------------------


# Question5: Does blood oxygen level affect heart rate?
#      H0:blood oxygen level has no effect on heart rate
#      H1:blood oxygen level has effect on heart rate

# Here my independent variable is blood oxygen level which is continuous in nature
# and dependent variable is heart rate which is also continuous in nature

# checking normality of dependent variable
normality_test_Q5 <- shapiro.test(sorted_sleep_df$Heart_Rate)
normality_test_Q5$p.value

# So we can see that p-value is less than 0.05(p = 1.571483e-15), 
# therefore our dependent variable is not normally distributed

# Choosing the statistical test
# Since our dependent variable is continuous and not normally distributed
# and independent variable is also continuous
# we will perform a Spearman’s Correlation Coefficient test to test our fourth hypothesis

# First let us find the correlation between blood oxy level and body temperature
library(corrplot)

# Extracting only blood oxy level and body temp variables and
# storing into new dataframe
include_list_cor3 <- names(sleep_df) %in% c("Blood_Oxy_Level", "Heart_Rate")
include_list_cor3

new_sleep_df3 <- sleep_df[(include_list_cor3)]
new_sleep_df3

# Showing correlation plot for blood oxy level and body temperature
M <- cor(new_sleep_df3)
corrplot(M, method = 'number')

# From the plot we can see that both the variables have a negative correlation

# No We will perform the Spearman’s Correlation Coefficient test
cor.test(sorted_sleep_df$Heart_Rate, sorted_sleep_df$Blood_Oxy_Level, method = "spearman")

# p-value is < 0.05 so we reject H0 and conclude that
# Heart Rate is affected by body temperature(p = 2.2e-16)

# Writing the extracted data frame that was used for final analysis to csv file for future use
write.csv(sorted_sleep_df, file = "Sorted sleep.csv")
