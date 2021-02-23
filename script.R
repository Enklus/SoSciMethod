
#set working directory
##This needs to be changed to your working directory!

setwd("C:/Users/Well Met/Desktop/folder methodology")

#empty workspace

rm(list = ls())

#install and load packages

install.packages("dplyr")
install.packages("readxl")
install.packages("stargazer")
install.packages("car")
install.packages("carData")
install.packages("ggpubr")
library(carData)
library(car)
library(stargazer)
require("readxl")
library("ggpubr")
library(dplyr)


# load Excel-dataset

data_survey <- read.csv(file = 'results-survey715685.csv')


## Correlation
cor.test(data_survey$E4, data_survey$B1,
         method = "spearman")

cor.test(data_survey$E3, data_survey$B2,
         method = "pearson")
##create second dataframe without unusable education data

data_survey_education <- data_survey %>%
  filter(E4 <= 5) 

##recode to factor variable
data_survey$occupation <- as.character(data_survey$E5)
data_survey$education <- as.character(data_survey$E4)

##Add labels

data_survey$occupation <- factor(data_survey$E5, 
                                levels = c(1, 2, 3, 4, 5, 6), 
                                labels = c("-Schueler", "-Auszubildender", "-Studierender",
                                           "-Berufstätiger", "-Arbeitssuchender", "-Arbeitslos"))


data_survey$education <- factor(data_survey$E4, 
                                levels = c(1, 2, 3, 4, 5, 6, 7), 
                                labels = c("-Hauptschulabschluss/Mittlere Reife", "-Matura/Abitur", "-Bachelor or EQUIV",
                                           "-Master or EQUIV", "-Doktor or EQUIV", "-other post-secondary", "-other"))


data_survey$income <- factor(data_survey$E3,  order = TRUE,
                            levels = c(1,2,3,4,5,6,7,8),
                            labels = c("0-500???", "501 - 750???", "751 - 1,000???", "1,001 - 1,250 ???", "1,251 - 1,500???", "1,501 - 1,750???", "1,751 - 2,000???", "mehr als 2,000???"))


# Piechart

#plotting pie chart of education
pie(table(data_survey$education), main = "Distribution of levels of  education")

#plotting pie chart of occupatiion

pie(table(data_survey$occupation))



## Regression Analysis ##


ra_B1 <- lm(B1.SQ001. ~ education  + E2 + E3 + occupation , data = data_survey)

ra_B2 <- lm(B2 ~ income + education  + E2 + E3 + occupation , data = data_survey , na.action = na.exclude )



# Save regression output:
stargazer(ra_B1,  type = "html", out = "regression_B1.html") 

stargazer(ra_B2,  type = "html", out = "regression_B2.html") 




