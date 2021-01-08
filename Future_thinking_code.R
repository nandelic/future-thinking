###################################################################################################
#                       Future self-continuity predicts financial decisions                       #
#                         of people with problem debts (December, 2020)                           #
###################################################################################################

# The following script outlines all analysis steps required to replicate the findings in our paper

################## Load packages and set up
# data wrangling 
library(Hmisc)
library(dplyr)
library(purrr)
library(tidyr)
library(MASS)
library(mice)
library(data.table)
library(car)
# graphics
library(ggplot2)
library(cowplot)
library(ggpubr)
# statistics
library(rcompanion)
library(psych)
library(ppcor)
library(e1071)
library(lavaan)
library(psychometric)
# options
options(scipen=999)
# functions

setwd("H:/Papers/Future Thinking/Data")

#####################################################################################################
#                                          Study 1                                                  #
#####################################################################################################

# Load data
df <- read.csv("study1_paper_data.csv") # variable data
df_auc <- read.csv("study1_discount_modelselector.csv") # AUC data from Model Discount Selector (available for 247 participants, 20 had NAs)
df2 <- read.csv("study1_TD_switch_point.csv") # discounting switching point to calculate J&B rules

################## Data wrangling

# select variables
df_auc <- df_auc %>%
  dplyr::select(probmodel, probmodel.AUC, ResponseId) # Add ResponseId

# Applying Johnson & Bickel rules 
# RULE 1
df2$rule1 <- ifelse((df2$T2_1month-df2$T1_1week) >20, "FAIL",
                    ifelse((df2$T3_6months-df2$T2_1month) >20, "FAIL",
                           ifelse((df2$T4_1year-df2$T3_6months) >20, "FAIL",
                                  ifelse((df2$T5_3years-df2$T4_1year) >20, "FAIL",
                                         ifelse((df2$T6_5years-df2$T5_3years) >20, "FAIL",
                                                ifelse((df2$T7_10years-df2$T6_5years) >20, "FAIL", "PASS"))))))
# RULE 2
df2$rule2 <- ifelse((df2$T1_1week-df2$T7_10years) >=10, "PASS", "FAIL")
df2$criteria <- ifelse(df2$rule1 == "FAIL" | df2$rule2 == "FAIL", "FAIL", "PASS")
df2$criteria <- as.factor(df2$criteria)
df2 <- df2 %>%
  dplyr::select(criteria, ResponseId)

df <- merge(df, df_auc) # Combine numeric data with prob.AUC data
df <- merge(df, df2) # Combine data with the JB rules data
df <- df %>%
  dplyr::filter(criteria != "FAIL") # Remove those who failed J&B rules reduces from 247 to 214 participants
df.1 <- df %>% # simplifying the dataset
  dplyr::select(ResponseId, meanstress, FSC, probmodel.AUC, condition, age)
df.2 <- df %>%
  dplyr::select(ResponseId, meanstress, FSC, probmodel.AUC, condition)
df <- na.omit(df.2) # 7 NAs in FSC, after removal we have 207 participants. (109 control, 98 IVA)
df <- merge(df, df.1)

# Checking skewness of AUC
skewness(df$probmodel.AUC) #current skew: 1.29
df$AUC <- sqrt(df$probmodel.AUC) # transforming AUC with square root
skewness(df$AUC) #new skew: 0.54

################# Statistics

# Correlations between stress, FSC, AUC and age
corrdf <- df %>%
  dplyr::select(meanstress, FSC, AUC, age)
mat <- rcorr(as.matrix(corrdf))
mat

# Confidence Intervals for each correlation
CIr(r=-0.29, n = 207, level = .95) # AUC and PFS
CIr(r=0.28, n = 207, level = .95) # AUC and FSC
CIr(r=-0.38, n = 207, level = .95) # FSC and PFS

# means & t-tests for AUC, FSC, Stress
df %>%
  dplyr::select(AUC, FSC, meanstress, condition) %>%
  group_by(condition) %>%
  dplyr::summarise(m_AUC=mean(AUC, na.rm=TRUE), sd_AUC=sd(AUC, na.rm=TRUE), 
                   m_FSC=mean(FSC, na.rm=TRUE), sd_FSC=sd(FSC, na.rm=TRUE), 
                   m_stress=mean(meanstress), sd_stress=sd(meanstress)) # table with means and SDs

# Variance testing
bartlett.test(meanstress ~ condition, data=df)
bartlett.test(AUC ~ condition, data=df)
bartlett.test(FSC ~ condition, data=df)

t.test(df$meanstress ~ df$condition) # welch test
t.test(AUC ~ condition, data=df, # two-sided t-test
       var.equal=TRUE,
       conf.level=0.95)
t.test(FSC ~ condition, data=df, # two-sided t-test
       var.equal=TRUE,
       conf.level=0.95)

# Regressions with CIs
model1 <- lm(AUC ~ FSC + condition, data=df) # FSC and condition
summary(model1)
confint(model1)

model2 <- lm(AUC ~ meanstress + condition, data=df) # stress and condition
summary(model2)
confint(model2)

model3 <- lm(AUC ~ condition + FSC + meanstress, data=df) # condition and both covariates
summary(model3)
confint(model3)



#####################################################################################################
#                                          Study 2                                                  #
#####################################################################################################

# Load data
df <- read.csv("study2_paper_data.csv") # survey data
df2 <- read.csv("study2_switch_point.csv") # temporal discounting value at switching point to calculate J&B rules
df3 <- read.csv("study2_discount_modelselector.csv") # AUC data from Model Discount Selector

# Johnson & Bickel rules
# RULE 1
df2$rule1 <- ifelse((df2$T2_1month-df2$T1_1week) >20, "FAIL",
                    ifelse((df2$T3_6months-df2$T2_1month) >20, "FAIL",
                           ifelse((df2$T4_1year-df2$T3_6months) >20, "FAIL",
                                  ifelse((df2$T5_3years-df2$T4_1year) >20, "FAIL",
                                         ifelse((df2$T6_5years-df2$T5_3years) >20, "FAIL",
                                                ifelse((df2$T7_10years-df2$T6_5years) >20, "FAIL", "PASS"))))))

# RULE 2
df2$rule2 <- ifelse((df2$T1_1week-df2$T7_10years) >=10, "PASS", "FAIL")
df2$criteria <- ifelse(df2$rule1 == "FAIL" | df2$rule2 == "FAIL", "FAIL", "PASS")
df2$criteria <- as.factor(df2$criteria)
df2 <- df2 %>%
  dplyr::select(criteria, ResponseId)


############# Data wrangling

df3 <- df3 %>%
  dplyr::select(probmodel.AUC, ResponseId, probmodel)
df <- merge(df, df3)
df <- merge(df, df2)

#df <- df %>%
#  dplyr::filter(criteria != "FAIL") # Removing those who fail J&B criteria - only done for supplementary mat, leaves 186 ppts
# Transforming AUC to reduce skewness
df$AUC <- sqrt(df$probmodel.AUC)

############ Statistics

# means
groupwiseMean(FSC ~ 1, data   = df, conf   = 0.95, digits = 3)
groupwiseMean(financialstress ~ 1, data   = df, conf   = 0.95, digits = 3)
groupwiseMean(mhi ~ 1, data   = df, conf   = 0.95, digits = 3)
groupwiseMean(IVA_missed ~ 1, data   = df, conf   = 0.95, digits = 3)
groupwiseMean(IVA_months ~ 1, data   = df, conf   = 0.95, digits = 3)

# variance tests
bartlett.test(FSC ~ missed_factor, data=df)
bartlett.test(financialstress ~ missed_factor, data=df)
bartlett.test(mhi ~ missed_factor, data=df) # none of them have significantly different variance

t.test(FSC ~ missed_factor, data=df, # two-sided t-test
       var.equal=TRUE,
       conf.level=0.95)
t.test(financialstress ~ missed_factor, data=df, # two-sided t-test
       var.equal=TRUE,
       conf.level=0.95)
t.test(mhi ~ missed_factor, data=df, # two-sided t-test
       var.equal=TRUE,
       conf.level=0.95)


# Spearman's correlations
my_data <- df %>%
  dplyr::select(AUC, FSC, mhi, financialstress, age, IVA_missed, IVA_months)
res2 <- corr.test(as.matrix(my_data), method="spearman", ci=TRUE)
print(res2, short=FALSE) # printing with normal CIs

#### CIs for the correlations


# Filtering out those who did not have missed payments
df2 <- df %>%
  dplyr::filter(missed_factor == "missed")

# Correlations
my_data <- df2 %>%
  dplyr::select(AUC, FSC, mhi, financialstress, age, IVA_missed, IVA_months)
res2 <- rcorr(as.matrix(my_data), type="spearman")
res2
res2 <- corr.test(as.matrix(my_data), method="spearman", ci=TRUE) # allows for CIs as well
print(res2, short=FALSE) # printing with normal CIs

res2 <- rcorr(as.matrix(my_data), type="spearman")
res2

# Regression on full sample
model1 <- lm(IVA_missed ~ FSC + mhi + financialstress + IVA_months + age, data = df) # Regression with full sample
summary(model1)
confint(model1)

# Regression on only those with missed payments
model2 <- lm(IVA_missed ~ FSC + mhi + financialstress + IVA_months + age, data = df2) # Regression with only those w/ missed payments
summary(model2)
confint(model2)

# Checking Outliers for Model 2

influencePlot(model2, id.method="noteworthy", main="Influence Plot", sub="Circle size is proportial to Cook's Distance")
# 5 influential outliers identified

#       StudRes        Hat       CookD
#3  -0.2117072 0.20431572 0.001940475 # R_10vZBkPAWUED8CL
#5   4.1574667 0.07588625 0.197760921 # R_12u0tQNLBkzJZdB
#20  3.2367252 0.06296323 0.105302334 # R_1PZSzFC73W9BVGG
#39  3.1801454 0.20667915 0.395681855 # R_2ttnmu9EVmwNBNY
#83  2.9634324 0.14672162 0.230102242 # R_UfQHzkXmWMkmArT




# Removing the influential observations and re-running model
df3 <- df2 %>%
  dplyr::filter(!ResponseId %in% c("R_12u0tQNLBkzJZdB", "R_2ttnmu9EVmwNBNY", "R_UfQHzkXmWMkmArT", "R_10vZBkPAWUED8CL", "R_1PZSzFC73W9BVGG"))
model3 <- lm(IVA_missed ~ FSC + mhi + financialstress + IVA_months + age, data = df3) # Regression with only those w/ missed payments
summary(model3)
confint(model3)

