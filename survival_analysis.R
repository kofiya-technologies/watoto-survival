################################################################################
#
# Survival analysis of under-five (u5) child mortality
#
# By Kofiya Technologies (https://kofiyatech.com/)
#
################################################################################


# Set working directory
setwd("D:\\RStudioProjects\\watoto-survival")

################################################################################
# Load data
################################################################################
library(readr)

surv_data <- readr::read_csv("./dhs-table-preprocessed-ayele2017-survival-paper.csv")

# Load schema definition about variables
schema_definition <- readr::read_csv("./schema-definition-variable-ayele2017-survival-paper.csv")


################################################################################
# Survival analysis for under five (U5) mortality rate
################################################################################
# Import library  ##############################################################
library(dplyr)        # for data wrangling and processing 
library(magrittr)     # for pipe operator
library(ggplot2)      # for visualization
library(skimr)        # to summarise data
library(survival)     # for survival analysis


# Get columns of interest for analysis   #######################################
col_of_interest <- schema_definition %>% 
  dplyr::filter(Type %in% c("feature_numeric", "feature_categorical", "response")) %>% 
  dplyr::select(Name, Recoded)

col_of_interest <- col_of_interest %>% 
  dplyr::mutate(NameModified = ifelse(Recoded == TRUE, 
                                      paste(Name, "recoded", sep='_'),
                                      Name)) %>% 
  dplyr::pull(NameModified)
  
# Keep analysis variables in the data
surv_data <- surv_data %>% 
  dplyr::select(any_of(col_of_interest))
  

# Derive response variable for survival analysis  ##############################
# ------------------------------------------------------------------------------
# Variable definition
# - B6:  Age at death
# - B7:  Age at death (months, imputed)
# - B13: Flag for age at death
# - HW1: Child's age in months
# ------------------------------------------------------------------------------
# Obtained from SQLite DB
# > SELECT * from ".\ET_2011_DHS_07082021_1847_58107.ETKR61FL.FlatValuesSpec"
# > WHERE Name = 'B13'
# ------------------------------------------------------------------------------
# Variable values --> B13 (Flag for age at death)
# - 0 = No flag
# - 1 = > interview
# - 2 = < breastfeeding
# - 3 = < age supplemented
# - 4 = < first breastfed
# - 5 = < last vaccination
# - 6 = Outside range
# - 7 = Imputed, units given
# - 8 = Imputed, no units
# ------------------------------------------------------------------------------
# Variable values --> B6 (Age at death)
# - 100 = Died on day of birth
# - 101 = Days: 1
# - 199 = Days: number missing
# - 201 = Months: 1
# - 299 = Months: number missing
# - 301 = Years: 1
# - 399 = Years: number missing
# ------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
# Check B13
# ------------------------------------------------------------------------------
# Output
# B13     n
# <dbl> <int>
#   1     0   814
# 2     1    10
# 3     4     3
# 4     6    17
# 5     8     2
# 6    NA 10808
# ------------------------------------------------------------------------------
surv_data %>% 
  dplyr::count(B13) %>% 
  dplyr::arrange(B13)

# Let's exclude other flag types for B13 variable
surv_data <- surv_data %>% 
  dplyr::filter(B13 %in% c(0, NA))


# ------------------------------------------------------------------------------
# Do we need B6 (Age at death) as we have B7 (Age at death (months, imputed))?
# ------------------------------------------------------------------------------
# - B7 takes values from 0 (died on day of birth) to 48 months (4 years)
# - N=10,808 are children who are alive.
# - N=814 are children who died.
table_b7 <- surv_data %>% 
  dplyr::count(B7) %>% 
  dplyr::arrange(B7)

table_b6 <- surv_data %>% 
  dplyr::count(B6) %>% 
  dplyr::arrange(B6)

# Drop B6
surv_data <- surv_data %>% 
  dplyr::select(-c(B6))

# ------------------------------------------------------------------------------
# How are B7, B13 and HW1 variables related to each other?
# ------------------------------------------------------------------------------
# B7 contains age for died kids, and HW1 for alive kids!
# ------------------------------------------------------------------------------
# B7 	B13 HW1
# NA  NA  43
# 0   0  	NA
# NA  NA  4
# 2   0  	NA
# NA  NA  30
# 7   0  	NA
# 10  0	  NA
# NA  NA  9
# NA  NA  57
# NA  NA  23
# ------------------------------------------------------------------------------
# Find out if there are children whose age is not known!
# - There are 328 children with age unknown
unknown_age_children <- surv_data %>% dplyr::select(B7, HW1) %>% 
  dplyr::filter(is.na(B7) & is.na(HW1))

# Drop children whose age is not known
surv_data <- surv_data %>% 
  dplyr::filter(!(is.na(B7) & is.na(HW1)))

# Sanity checks
# - This gives a total of 10,480 alive kids and 814 died kids!
# B7              HW1       
# Min.   : 0.000   Min.   : 0.00  
# 1st Qu.: 0.000   1st Qu.:14.00  
# Median : 0.000   Median :29.00  
# Mean   : 5.784   Mean   :29.13  
# 3rd Qu.: 7.000   3rd Qu.:44.00  
# Max.   :48.000   Max.   :59.00  
# NA's   :10480    NA's   :814  
surv_data %>% dplyr::select(B7, HW1) %>% summary()


# Create target variable
surv_data <- surv_data %>% 
  dplyr::mutate(is_died = ifelse(is.na(B13), 'No', 'Yes'))

# Distribution of number of deaths
# is_died     n
# <dbl> <int>
#   1       1   814
#   2      NA 10480
surv_data %>% 
  dplyr::count(is_died)

# Drop B13
surv_data <- surv_data %>% 
  dplyr::select(-c(B13))

# Derive a new age variable from B13 and HW1 variables.
surv_data <- surv_data %>% 
  dplyr::mutate(age_child_month = ifelse(!is.na(HW1), 
                                         HW1,
                                         ifelse(!is.na(B7), B7,
                                                NA)))

# Sanity checks
# age_child_month
# Min.   : 0.00  
# 1st Qu.:11.00  
# Median :27.00  
# Mean   :27.44  
# 3rd Qu.:43.00  
# Max.   :59.00 
surv_data %>% 
  dplyr::select(age_child_month) %>% 
  summary()

# Distribution of age by death
ggplot2::ggplot(surv_data, aes(x=age_child_month, color=is_died)) +
  ggplot2::geom_histogram()

# Drop original variables that are used to derive target variable(s)
surv_data <- surv_data %>% 
  dplyr::select(-c(B7, HW1))

# Check distribution of child age
# - Most of the children have died before the age of 7 months (see Q3_AGE)!
# - All of the children who died couldn't make 48 months (see MAX_AGE)!
# - The average age of died children is about 6 months!
surv_data %>% 
  dplyr::select(is_died, age_child_month) %>% 
  dplyr::group_by(is_died) %>% 
  skimr::skim()


# Create censoring variable (right censoring)   ################################
# - Our study endpoint is U5 (under five years). Any death after 5 years should 
#   be censored.
surv_data <- surv_data %>% 
  dplyr::mutate(censored_u5 = ifelse((is_died == 'Yes') & (age_child_month < 60), 
                                     1,
                                     0))

# Distribution of censoring variable
#   censored_u5     n
#   0            10480
#   1            814
surv_data %>% 
  dplyr::count(censored_u5)


# Kaplan-Meier (KM) Analysis - Life Table ######################################
km_baseline <- survival::survfit(Surv(age_child_month, censored_u5) ~ 1,
                                 data=surv_data,
                                 type='kaplan-meier')

print(km_baseline)

# Get life table from KM result
summary(km_baseline)

plot(km_baseline)

# A better survival plot
library(survminer)

survminer::ggsurvplot(km_baseline, 
                      data = surv_data,
                      risk.table = TRUE,
                      conf.int = TRUE,
                      ggtheme = theme_minimal())

# KM Analysis by area of residence
km_por <- survival::survfit(Surv(age_child_month, censored_u5) ~ V025_recoded,
                            data=surv_data,
                            type='kaplan-meier')

print(km_por)

summary(km_por)

survminer::ggsurvplot(km_por, 
                      data = surv_data,
                      risk.table = TRUE,
                      conf.int = TRUE,
                      pval = TRUE,
                      pval.method = TRUE,
                      ggtheme = theme_minimal())


# Fit Cox Proportional Hazard (PH) model - Cox Regression model  ###############
cox_por <- survival::coxph(Surv(age_child_month, censored_u5) ~ V025_recoded,
                           data = surv_data)

summary(cox_por)

# KM Analysis by Husband/partner's education level
km_edu <- survival::survfit(Surv(age_child_month, censored_u5) ~ V701_recoded,
                            data=surv_data,
                            type='kaplan-meier')

print(km_edu)

summary(km_edu)

survminer::ggsurvplot(km_edu, 
                      data = surv_data,
                      risk.table = TRUE,
                      conf.int = TRUE,
                      pval = TRUE,
                      pval.method = TRUE,
                      ggtheme = theme_minimal())


# Fit Cox Proportional Hazard (PH) model - Cox Regression model  ###############
cox_por <- survival::coxph(Surv(age_child_month, censored_u5) ~ V025_recoded,
                           data = surv_data)

summary(cox_por)

# KM Analysis by Source of drinking water
km_sdw <- survival::survfit(Surv(age_child_month, censored_u5) ~ V113_recoded,
                            data=surv_data,
                            type='kaplan-meier')

print(km_sdw)

summary(km_sdw)

survminer::ggsurvplot(km_sdw, 
                      data = surv_data,
                      risk.table = TRUE,
                      conf.int = TRUE,
                      pval = TRUE,
                      pval.method = TRUE,
                      ggtheme = theme_minimal())


# Fit Cox Proportional Hazard (PH) model - Cox Regression model  ###############
# Place of residence
cox_por <- survival::coxph(Surv(age_child_month, censored_u5) ~ V025_recoded,
                           data = surv_data)

summary(cox_por)

# Source of drinking water 
cox_sdw <- survival::coxph(Surv(age_child_month, censored_u5) ~ V113_recoded,
                           data = surv_data)

summary(cox_sdw)





# ------------------------------------------------------------------------------
# Fit Cox PH model for all features
# ------------------------------------------------------------------------------
# Get features
feature_vars <- schema_definition %>% 
  dplyr::filter(Type %in% c("feature_numeric", "feature_categorical")) %>% 
  dplyr::select(Name, Recoded) %>% 
  dplyr::mutate(NameModified = ifelse(Recoded == TRUE, 
                                      paste(Name, "recoded", sep='_'),
                                      Name)) %>% 
  dplyr::pull(NameModified)

# Create data set with feature_vars and target vars
all_vars <- c(feature_vars, 'age_child_month', 'censored_u5')

surv_data_all <- surv_data %>% 
  dplyr::select(any_of(all_vars))

# Fit model with all features
cox_all <- survival::coxph(Surv(age_child_month, censored_u5) ~ .,
                           data = surv_data_all)

summary(cox_all)


################################################################################
# Survival analysis for under one (U1) mortality rate
################################################################################
# Create censoring variable (right censoring)   ################################
# - Our study endpoint is U1 (under one year). Any death after a year should 
#   be censored.
surv_data <- surv_data %>% 
  dplyr::mutate(censored_u1 = ifelse((is_died == 'Yes') & (age_child_month < 12), 
                                     1,
                                     0))

# Distribution of censoring variable
#   censored_u1     n
#   0            10480
#   1            814
surv_data %>% 
  dplyr::count(censored_u1)


# Kaplan-Meier (KM) Analysis - Life Table ######################################
km_baseline <- survival::survfit(Surv(age_child_month, censored_u1) ~ 1,
                                 data=surv_data,
                                 type='kaplan-meier')

print(km_baseline)

# Get life table from KM result
summary(km_baseline)

# Survival below 12 months
summary(km_baseline, times=seq(1, 12))

# Survival after 12 months
summary(km_baseline, times=seq(12, 60))

plot(km_baseline)




################################################################################
# RESOURCES
################################################################################
# - https://rviews.rstudio.com/2017/09/25/survival-analysis-with-r/





