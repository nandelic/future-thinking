###################################################################################################
#                       Future self-continuity predicts financial decisions                       #
#                           of people with problem debts (May, 2020)                              #
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
library(psych)
library(ppcor)
library(e1071)
library(lavaan)
library(psychometric)
# options
options(scipen=999)
# functions



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
model1 <- lm(AUC ~ FSC + condition, data=df) # M1 (FSC) and condition
summary(model1)
confint(model1)

model2 <- lm(AUC ~ meanstress + condition, data=df) # M2 (stress) and condition
summary(model2)
confint(model2)

model3 <- lm(AUC ~ condition + FSC + meanstress, data=df) # condition and both mediators
summary(model3)
confint(model3)

# Mediation analysis
multipleMediation <- '
    AUC ~ b1 * FSC + b2 * meanstress + c * condition
    # mediator
    FSC ~ a1 * condition
    meanstress ~ a2 * condition
    # indirect effects
    indirect1 := a1 * b1
    indirect2 := a2 * b2
    # total effect
    total    := c + (a1 * b1) + (a2 * b2)
    FSC ~~ meanstress
'
fit <- sem(model = multipleMediation, data = df)
summary(fit) 

### Bootstrapping for mediation effect
fit <- sem(
  model = multipleMediation,
  data  = df,
  se = "bootstrap",
  bootstrap = 1000 # 1000 is the default
)
summary(fit, fit.measures=TRUE, standardize=TRUE, rsquare=TRUE, 
        estimates = TRUE, ci = TRUE)


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
#  dplyr::filter(criteria != "FAIL") # Removing those who fail J&B criteria
# Transforming AUC to reduce skewness
df$AUC <- sqrt(df$probmodel.AUC)

############ Statistics

# Correlations
my_data <- df %>%
  dplyr::select(AUC, FSC, mhi, financialstress, age, IVA_missed, IVA_months)
res2 <- rcorr(as.matrix(my_data))
res2
#### CIs for the correlations

# sample of 243, ordered as mentioned in-text
CIr(r=-0.11, n = 243, level = .95) # FSC and missed
CIr(r=-0.00, n = 243, level = .95) # PFS and missed
CIr(r=-0.15, n = 243, level = .95) # FSC and PFS

# Filtering out those who did not have missed payments
df2 <- df %>%
  dplyr::filter(missed_factor == "missed")

# Correlations
my_data <- df2 %>%
  dplyr::select(AUC, FSC, mhi, financialstress, age, IVA_missed, IVA_months)
res2 <- rcorr(as.matrix(my_data))
res2
# CIs for sample of 89, ordered as mentioned in-text
CIr(r=-0.32, n = 89, level = .95) # FSC and missed
CIr(r=-0.12, n = 89, level = .95) # PFS and missed

# CIs for supplementary material
CIr(r=-0.24, n = 186, level = .95) # AUC and PFS
CIr(r=0.11, n = 186, level = .95) # AUC and FSC
CIr(r=0.01, n = 186, level = .95) # AUC and missed
CIr(r=0.04, n = 186, level = .95) # AUC and months in IVA


# Regression on full sample
model1 <- lm(IVA_missed ~ FSC + mhi + financialstress + IVA_months, data = df) # Regression with full sample
summary(model1)
confint(model1)

# Regression on only those with missed payments
model2 <- lm(IVA_missed ~ FSC + mhi + financialstress + IVA_months, data = df2) # Regression with only those w/ missed payments
summary(model2)
confint(model2)

# Checking Outliers for Model 2

influencePlot(model2, id.method="noteworthy", main="Influence Plot", sub="Circle size is proportial to Cook's Distance")
# 5 influential outliers identified, R_12u0tQNLBkzJZdB & R_1PZSzFC73W9BVGG & R_2ttnmu9EVmwNBNY & R_1LWBkjXZ4BZLMid & R_UfQHzkXmWMkmArT 

#       StudRes        Hat       CookD
#5   4.1671474 0.07479009 0.234967605 over cut-off of 0.049
#17 -0.3909503 0.15703627 0.005752629 not over cut-off but has high Hat values
#20  3.2537649 0.06294216 0.127656051 over cut-off of 0.049
#39  3.2007074 0.18908772 0.430395123 over cut-off of 0.049
#83  2.9905526 0.13589639 0.257000341 over cut-off of 0.049




# Removing the influential observations and re-running model
df3 <- df2 %>%
  dplyr::filter(!ResponseId %in% c("R_12u0tQNLBkzJZdB", "R_2ttnmu9EVmwNBNY", "R_UfQHzkXmWMkmArT", "R_1LWBkjXZ4BZLMid", "R_1PZSzFC73W9BVGG"))
model3 <- lm(IVA_missed ~ FSC + mhi + financialstress + IVA_months, data = df3) # Regression with only those w/ missed payments
summary(model3)
confint(model3)

