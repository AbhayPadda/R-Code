
# Survey Link: https://goo.gl/forms/ElUp30eEAgCIj6fh1


# Load packages that will be used for the analysis
library(ggplot2)
library(plyr)
library(dplyr)
library(reshape2)
library(plotly)

# Library to install statsr library
library(devtools)

# Install Statsr from github repository
install_github("StatsWithR/statsr") 
library(statsr)

# Change the working directory
setwd('C:\\Users\\Abhay\\Desktop\\Business Statistics Project Files\\')

# Load data set
load("193_project_dataset.RData")

#covert the data to tbl_df as they are easier to analyze
shopping_tbl <- tbl_df(shopping) 
head(shopping_tbl)

# Check the summary of dataset for different variables
age_summary <- tbl_df(summary(shopping_tbl$Age))

# Plot a pie chart to represent the various age groups
labels = c('', '20 to 25 Years', '26 to 30 Years', 'Less than 20 Years', 'More than 30 Years')
values = age_summary$value
ds <- data.frame(labels = labels,
                 values = values)
plot_ly(ds, labels = labels, values = values, type = "pie") %>%
  layout(title = "Age Groups of the consumers participating in the survey")

##                        20 to 25 Years     26 to 30 Years 
##                  0                 71                 74 
## Less than 20 Years More than 30 Years 
##                  6                 11
summary(shopping_tbl$Felt_Furious)

#                               Agree          Disagree           Neutral    Strongly Agree Strongly Disagree 
#                 0                32                47                64                 8                11 

# Check for the missing values in the dataset
sum(is.na(shopping_tbl))
# [1] 0 
# No missing values in the data set, so we can safely proceed

# This will give us the structure of the categorical variables 
str(shopping_tbl$Felt_Unhappy)
# Factor w/ 6 levels "","Agree","Disagree",..: 3 4 2 3 4 3 4 2 3 2 ...
# This gives 6 levels, whereas we have only 5 levels, so we need to remove the extra "" level

for(i in names(shopping_tbl[,3:60])){
  shopping_tbl[[i]] <- droplevels(shopping_tbl[[i]])
  shopping_tbl[[i]] <- factor(shopping_tbl[[i]], ordered=TRUE)      
}


str(shopping_tbl$Felt_Unhappy)

# Assign the correct value to each of the ordinal level
new_levels <- c("1"="Strongly Disagree", "2"="Disagree", "3"="Neutral", "4"="Agree", "5"="Stinstall_github("StatsWithR/statsr")rongly Agree")
for(i in names(shopping_tbl[,11:60])){
  shopping_tbl[[i]] <- revalue(shopping_tbl[[i]], new_levels)
  shopping_tbl[[i]] <- ordered(shopping_tbl[[i]], levels=c("Strongly Disagree", "Disagree","Neutral", "Agree","Strongly Agree"))      
}

# Check for the structure after removing the "" level
str(shopping_tbl$Felt_Unhappy)
##  Ord.factor w/ 5 levels "Agree"<"Disagree"<..: 2 3 1 2 3 2 3 1 2 1 ...

# Assign the following codes to the ordinal values:
# "1"="Strongly Disagree", "2"="Disagree", "3"="Neutral", "4"="Agree", "5"="Strongly Agree"
for(i in names(shopping_tbl[,11:60])) {
  shopping_tbl[[i]] = as.numeric(shopping_tbl[[i]])
}


# Calculate the Online dissonance score
shopping_tbl <- shopping_tbl %>% mutate(Online_Dissonance_Score = Felt_Unhappy + Felt_Made_A_Mistake + 
                                          Felt_Done_A_Wastage + Thought_Of_Corrective_Action + Expectation_Not_Met + 
                                          Genuine_Discounts_Or_Not + Return_And_Exchange_Policy + 
                                          Painful_Product_Replacemnt + Keeping_Promise + No_Trust)

# Calculate the Human interface score
shopping_tbl <- shopping_tbl %>% mutate(Human_Interface_Score = Cash_Payment_Than_Online_Payment + 
                                          Negotiation_For_Better_Deal + Missing_Personal_Touch + 
                                          Need_Of_Sales_Person + Smile_Of_Sales_Person + Trail_Of_Product )

# Calculate the privacy score
shopping_tbl <- shopping_tbl %>% mutate(Privacy_Score = Increase_In_Spam_Mails + Selling_Personal_Data + Selling_Financial_Data + 
                                          Decoding_Shopping_Pattern)

# After adding the three scores, check the table with the new values
head(shopping_tbl[,62:64])

# Check the variation in diffent scores by gender
shopping_tbl_gender <- melt(shopping_tbl, id.vars = "Gender", measure.vars = c("Online_Dissonance_Score","Human_Interface_Score","Privacy_Score"))

# Plot the variation using ggplot
ggplot(data = shopping_tbl_gender, aes(x=variable, y=value)) + geom_boxplot(aes(fill = Gender))

# Take the sample of 80 random observations, setting seed as the last three digits of the person in the group
set.seed(193)
n <- 80
samp <- sample_n(shopping_tbl, n)

## Hypothesis Testing Based on GENDER ##
# Testing hypotheis for Online Dissonance Score based on Gender
inference(y = Online_Dissonance_Score, x = Gender, data = samp, statistic = "mean", type = "ht", null = 0, alternative = "twosided", method = "theoretical")
## Response variable: numerical
## Explanatory variable: categorical (2 levels) 
## n_Female = 19, y_bar_Female = 31.7895, s_Female = 7.5098
## n_Male = 61, y_bar_Male = 31.9836, s_Male = 7.2284
## H0: mu_Female =  mu_Male
## HA: mu_Female != mu_Male
## t = -0.0993, df = 18
## p_value = 0.922

# Testing hypotheis for Human Interface Score based on Gender
inference(y = Human_Interface_Score, x = Gender, data = samp, statistic = "mean", type = "ht", null = 0, alternative = "twosided", method = "theoretical")
## Response variable: numerical
## Explanatory variable: categorical (2 levels) 
## n_Female = 19, y_bar_Female = 18.4737, s_Female = 4.8117
## n_Male = 61, y_bar_Male = 19.8525, s_Male = 4.3468
## H0: mu_Female =  mu_Male
## HA: mu_Female != mu_Male
## t = -1.1153, df = 18
## p_value = 0.2794

# Testing hypotheis for Privacy Score based on Gender
inference(y = Privacy_Score, x = Gender, data = samp, statistic = "mean", type = "ht", null = 0, alternative = "twosided", method = "theoretical")
## Response variable: numerical
## Explanatory variable: categorical (2 levels) 
## n_Female = 19, y_bar_Female = 12.6842, s_Female = 2.4732
## n_Male = 61, y_bar_Male = 12.9344, s_Male = 2.886
## H0: mu_Female =  mu_Male
## HA: mu_Female != mu_Male
## t = -0.3695, df = 18
## p_value = 0.716

## Hypotheis Testing based on MARITAL STATUS ##
removing_single_again <- filter(shopping_tbl, Marital_Status != "Single Again")
shopping_tbl_marital_status <- melt(removing_single_again, id.vars = "Marital_Status", measure.vars = c("Online_Dissonance_Score","Human_Interface_Score","Privacy_Score"))

ggplot(data = shopping_tbl_marital_status, aes(x=variable, y=value)) + geom_boxplot(aes(fill = Marital_Status))

# Removing 'Single Again' level from the dataset
removing_single_again[["Marital_Status"]] <- droplevels(removing_single_again[["Marital_Status"]])
removing_single_again[["Marital_Status"]] <- factor(removing_single_again[["Marital_Status"]], ordered = TRUE)

# Taking a different seed here than our previous sample because that might contain data for `single again` level of Marital_Status variable, 
# which we are to exclude from our analysis.
set.seed(194)
n <- 80
samp_marital_status <- sample_n(removing_single_again, n)

# Testing for Online Disonance based on Marital Status
inference(y = Online_Dissonance_Score, x = Marital_Status, data = samp_marital_status, statistic = "mean", type = "ht", null = 0, alternative = "twosided", method = "theoretical")
## Response variable: numerical
## Explanatory variable: categorical (2 levels) 
## n_Married = 14, y_bar_Married = 33, s_Married = 6.4807
## n_Single = 66, y_bar_Single = 33.1667, s_Single = 6.9362
## H0: mu_Married =  mu_Single
## HA: mu_Married != mu_Single
## t = -0.0863, df = 13
## p_value = 0.9325

# Testing for Human Interface based on Marital Status
inference(y = Human_Interface_Score, x = Marital_Status, data = samp_marital_status, statistic = "mean", type = "ht", null = 0, alternative = "twosided", method = "theoretical")
## Response variable: numerical
## Explanatory variable: categorical (2 levels) 
## n_Married = 14, y_bar_Married = 19.4286, s_Married = 2.7932
## n_Single = 66, y_bar_Single = 20.1212, s_Single = 4.3695
## H0: mu_Married =  mu_Single
## HA: mu_Married != mu_Single
## t = -0.7528, df = 13
## p_value = 0.465

# Testing for Privacy based on Marital Status
inference(y = Privacy_Score, x = Marital_Status, data = samp_marital_status, statistic = "mean", type = "ht", null = 0, alternative = "twosided", method = "theoretical")
## Response variable: numerical
## Explanatory variable: categorical (2 levels) 
## n_Married = 14, y_bar_Married = 12.7857, s_Married = 2.1187
## n_Single = 66, y_bar_Single = 13.0758, s_Single = 2.5499
## H0: mu_Married =  mu_Single
## HA: mu_Married != mu_Single
## t = -0.448, df = 13
## p_value = 0.6615

## Hypothesis testing based on EDUCATION ##
shopping_tbl_education <- melt(shopping_tbl, id.vars = "Education", measure.vars = c("Online_Dissonance_Score","Human_Interface_Score","Privacy_Score"))

ggplot(data = shopping_tbl_education, aes(x=variable, y=value)) + geom_boxplot(aes(fill = Education))

# Testing for online Dissonance based on Education
inference(y = Online_Dissonance_Score, x = Education, data = samp, statistic = "mean", type = "ht", null = 0, alternative = "greater", method = "theoretical")
## Response variable: numerical
## Explanatory variable: categorical (5 levels) 
## n_Graduate = 25, y_bar_Graduate = 31.52, s_Graduate = 8.8182
## n_Others = NA, y_bar_Others = NA, s_Others = NA
## n_Post Graduate = 23, y_bar_Post Graduate = 33.6957, s_Post Graduate = 6.4062
## n_Professional = 19, y_bar_Professional = 31.9474, s_Professional = 6.0962
## n_Under Graduate = 13, y_bar_Under Graduate = 29.6154, s_Under Graduate = 6.8743
## 
## ANOVA:
##           df    Sum_Sq Mean_Sq      F p_value
## Education  3  145.5536 48.5179 0.9207   0.435
## Residuals 76 4005.1339 52.6991               
## Total     79 4150.6875

# Testing for Human Interface based on Education
inference(y = Human_Interface_Score, x = Education, data = samp, statistic = "mean", type = "ht", null = 0, alternative = "greater", method = "theoretical")
## Response variable: numerical
## Explanatory variable: categorical (5 levels) 
## n_Graduate = 25, y_bar_Graduate = 19.72, s_Graduate = 5.6163
## n_Others = NA, y_bar_Others = NA, s_Others = NA
## n_Post Graduate = 23, y_bar_Post Graduate = 20.3913, s_Post Graduate = 3.5128
## n_Professional = 19, y_bar_Professional = 19.7895, s_Professional = 3.7354
## n_Under Graduate = 13, y_bar_Under Graduate = 17.2308, s_Under Graduate = 4.1864
## 
## ANOVA:
##           df    Sum_Sq Mean_Sq      F p_value
## Education  3   87.9662 29.3221 1.4956  0.2225
## Residuals 76 1489.9838 19.6051               
## Total     79   1577.95

# Testing for Privacy based on Education
inference(y = Privacy_Score, x = Education, data = samp, statistic = "mean", type = "ht", null = 0, alternative = "greater", method = "theoretical")
## Response variable: numerical
## Explanatory variable: categorical (5 levels) 
## n_Graduate = 25, y_bar_Graduate = 12.96, s_Graduate = 3.4215
## n_Others = NA, y_bar_Others = NA, s_Others = NA
## n_Post Graduate = 23, y_bar_Post Graduate = 12.913, s_Post Graduate = 2.8109
## n_Professional = 19, y_bar_Professional = 13.1579, s_Professional = 2.3157
## n_Under Graduate = 13, y_bar_Under Graduate = 12.2308, s_Under Graduate = 2.0878
## 
## ANOVA:
##           df   Sum_Sq Mean_Sq      F p_value
## Education  3   7.1299  2.3766 0.2992  0.8258
## Residuals 76 603.6201  7.9424               
## Total     79   610.75