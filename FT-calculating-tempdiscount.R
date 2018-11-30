###############################################################################################
#                           Calculating temporal discounting curve                            #
#                                   2019 Qualtrics Survey                                     #
###############################################################################################


# Packages
library(dplyr)

# Read data
mydata <- read.csv("C:/Users/3053836/Documents/Studies/pilot-study-1/analysis/testdata.csv")


# recoding so 1 is instant, 2 is delayed

mydata <- mydata %>% 
  mutate_at(c("Q22_1","Q22_5", "Q22_7",
              "Q22_8","Q22_11", "Q22_12",
              "Q22_14", "Q22_16", "Q22_17",
              "Q22_19", "Q22_20", "Q22_21",
              "Q24_3", "Q24_4", "Q24_7",
              "Q24_10", "Q24_11", "Q24_13",
              "Q24_14", "Q24_17", "Q24_18", 
              "Q24_19", "Q25_1", "Q25_3",
              "Q25_4", "Q25_5", "Q25_9",
              "Q25_11", "Q25_12", "Q25_16",
              "Q25_18", "Q25_19", "Q25_20",
              "Q26_3", "Q26_4", "Q26_6",
              "Q26_9", "Q26_10", "Q26_11",
              "Q26_13", "Q26_15", "Q26_18",
              "Q26_20", "Q26_21"), funs(recode(., `1`=2, `2`=1, .default = NaN)))

df <- mydata %>%
  dplyr::select("Q22_1", "Q22_2", "Q22_3", "Q22_4", "Q22_5", "Q22_6", "Q22_7",
                "Q22_8", "Q22_9", "Q22_10","Q22_11", "Q22_12", "Q22_13",
                "Q22_14", "Q22_15", "Q22_16", "Q22_17", "Q22_18",
                "Q22_19", "Q22_20", "Q22_21",
                "Q24_1", "Q24_2", "Q24_3", "Q24_4", "Q24_5", "Q24_6", "Q24_7",
                "Q24_8", "Q24_9", "Q24_10", "Q24_11", "Q24_12", "Q24_13",
                "Q24_14", "Q24_15", "Q24_16", "Q24_17", "Q24_18", 
                "Q24_19", "Q24_20", "Q24_21", "Q25_1", "Q25_2", "Q25_3",
                "Q25_4", "Q25_5", "Q25_6", "Q25_7", "Q25_8", "Q25_9",
                "Q25_10", "Q25_11", "Q25_12", "Q25_13", "Q25_14", "Q25_15", "Q25_16",
                "Q25_17", "Q25_18", "Q25_19", "Q25_20", "Q25_21", "Q26_1", "Q26_2",
                "Q26_3", "Q26_4", "Q26_5", "Q26_6", "Q26_7", "Q26_8",
                "Q26_9", "Q26_10", "Q26_11", "Q26_12",
                "Q26_13", "Q26_14", "Q26_15", "Q26_16", "Q26_17", "Q26_18",
                "Q26_19", "Q26_20", "Q26_21", "ResponseId")

# all character columns to factor:
df2 <- mutate_if(df, is.character, as.factor)

#Recode to Instant and Delayed
df3 <- df2 %>% 
  mutate_all(funs(recode(., '1' = 'I', '2'='D')))


#Calculate the subjective values for each time point

df3$T1_1week <- ifelse(df3$Q22_6 == "I", 2.5, 
            ifelse(df3$Q22_10 == "I", 7.5, 
                   ifelse(df3$Q22_19 == "I", 15, 
                          ifelse(df3$Q24_3 == "I", 25, 
                                 ifelse(df3$Q24_9 == "I", 35, 
                                        ifelse(df3$Q24_16 == "I", 45, 
                                               ifelse(df3$Q25_2 == "I", 55, 
                                                      ifelse(df3$Q25_12 == "I", 65, 
                                                             ifelse(df3$Q25_16 == "I", 75, 
                                                                    ifelse(df3$Q26_2 == "I", 85, 
                                                                           ifelse(df3$Q26_9 == "I", 91, 
                                                                                  ifelse(df3$Q26_17 == "I", 94, 98))))))))))))

df3$T2_1month <- ifelse(df3$Q22_4 == "I", 2.5, 
                        ifelse(df3$Q22_8 == "I", 7.5, 
                               ifelse(df3$Q22_17 == "I", 15, 
                                      ifelse(df3$Q24_1 == "I", 25, 
                                             ifelse(df3$Q24_14 == "I", 35, 
                                                    ifelse(df3$Q24_21 == "I", 45, 
                                                           ifelse(df3$Q25_7 == "I", 55, 
                                                                  ifelse(df3$Q25_10 == "I", 65, 
                                                                         ifelse(df3$Q25_21 == "I", 75, 
                                                                                ifelse(df3$Q26_7 == "I", 85, 
                                                                                       ifelse(df3$Q26_14 == "I", 91, 
                                                                                              ifelse(df3$Q26_15 == "I", 94, 98))))))))))))

df3$T3_6months <- ifelse(df3$Q22_5 == "I", 2.5, 
                        ifelse(df3$Q22_9 == "I", 7.5, 
                               ifelse(df3$Q22_18 == "I", 15, 
                                      ifelse(df3$Q24_2 == "I", 25, 
                                             ifelse(df3$Q24_12 == "I", 35, 
                                                    ifelse(df3$Q24_19 == "I", 45, 
                                                           ifelse(df3$Q25_5 == "I", 55, 
                                                                  ifelse(df3$Q25_11 == "I", 65, 
                                                                         ifelse(df3$Q25_19 == "I", 75, 
                                                                                ifelse(df3$Q26_5 == "I", 85, 
                                                                                       ifelse(df3$Q26_12 == "I", 91, 
                                                                                              ifelse(df3$Q26_16 == "I", 94, 98))))))))))))

df3$T4_1year <- ifelse(df3$Q22_7 == "I", 2.5, 
                         ifelse(df3$Q22_13 == "I", 7.5, 
                                ifelse(df3$Q22_21 == "I", 15, 
                                       ifelse(df3$Q24_6 == "I", 25, 
                                              ifelse(df3$Q24_10 == "I", 35, 
                                                     ifelse(df3$Q24_17 == "I", 45, 
                                                            ifelse(df3$Q25_3 == "I", 55, 
                                                                   ifelse(df3$Q25_14 == "I", 65, 
                                                                          ifelse(df3$Q25_17 == "I", 75, 
                                                                                 ifelse(df3$Q26_3 == "I", 85, 
                                                                                        ifelse(df3$Q26_10 == "I", 91, 
                                                                                               ifelse(df3$Q26_20 == "I", 94, 98))))))))))))


df3$T5_3years <- ifelse(df3$Q22_2 == "I", 2.5, 
                       ifelse(df3$Q22_11 == "I", 7.5, 
                              ifelse(df3$Q22_20 == "I", 15, 
                                     ifelse(df3$Q24_4 == "I", 25, 
                                            ifelse(df3$Q24_13 == "I", 35, 
                                                   ifelse(df3$Q24_20 == "I", 45, 
                                                          ifelse(df3$Q25_6 == "I", 55, 
                                                                 ifelse(df3$Q25_13 == "I", 65, 
                                                                        ifelse(df3$Q25_20 == "I", 75, 
                                                                               ifelse(df3$Q26_6 == "I", 85, 
                                                                                      ifelse(df3$Q26_13 == "I", 91, 
                                                                                             ifelse(df3$Q26_18 == "I", 94, 98))))))))))))

df3$T6_5years <- ifelse(df3$Q22_1 == "I", 2.5, 
                        ifelse(df3$Q22_14 == "I", 7.5, 
                               ifelse(df3$Q22_15 == "I", 15, 
                                      ifelse(df3$Q24_7 == "I", 25, 
                                             ifelse(df3$Q24_11 == "I", 35, 
                                                    ifelse(df3$Q24_18 == "I", 45, 
                                                           ifelse(df3$Q25_4 == "I", 55, 
                                                                  ifelse(df3$Q25_8 == "I", 65, 
                                                                         ifelse(df3$Q25_18 == "I", 75, 
                                                                                ifelse(df3$Q26_4 == "I", 85, 
                                                                                       ifelse(df3$Q26_11 == "I", 91, 
                                                                                              ifelse(df3$Q26_21 == "I", 94, 98))))))))))))

df3$T7_10years <- ifelse(df3$Q22_3 == "I", 2.5, 
                        ifelse(df3$Q22_12 == "I", 7.5, 
                               ifelse(df3$Q22_16 == "I", 15, 
                                      ifelse(df3$Q24_5 == "I", 25, 
                                             ifelse(df3$Q24_8 == "I", 35, 
                                                    ifelse(df3$Q24_15 == "I", 45, 
                                                           ifelse(df3$Q25_1 == "I", 55, 
                                                                  ifelse(df3$Q25_9 == "I", 65, 
                                                                         ifelse(df3$Q25_15 == "I", 75, 
                                                                                ifelse(df3$Q26_1 == "I", 85, 
                                                                                       ifelse(df3$Q26_8 == "I", 91, 
                                                                                              ifelse(df3$Q26_19 == "I", 94, 98))))))))))))
# Normalise the subjective values for each participant
y1 <- 1
y2 <- df3$T1_1week/100
y3 <- df3$T2_1month/100
y4 <- df3$T3_6months/100
y5 <- df3$T4_1year/100
y6 <- df3$T5_3years/100
y7 <- df3$T6_5years/100
y8 <- df3$T7_10years/100

# Assigning the delay values (based on your future timepoints)
x1 <- 0
x2 <- 0.0019
x3 <- 0.0077
x4 <- 0.05
x5 <- 0.1
x6 <- 0.3
x7 <- 0.5
x8 <- 1

# Calculate area under the curve
AUC <- ((x2-x1)*((y1+y2)/2)) +
  ((x3-x2)*((y2+y3)/2)) + 
  ((x4-x3)*((y3+y4)/2)) + 
  ((x5-x4)*((y4+y5)/2)) + 
  ((x6-x5)*((y5+y6)/2)) + 
  ((x7-x6)*((y6+y7)/2)) + 
  ((x8-x7)*((y7+y8)/2))

