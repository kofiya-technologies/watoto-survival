################################################################################
#
# WATOTO SURVIVAL 
#
#
# An open source project that utilizes classical survival analysis methods in R, 
# including Kaplan-Meier and Cox regression techniques. Its primary objective 
# is to enable researchers and graduate students to collaborate and identify 
# risk factors related to child mortality in children under five years old. 
# By leveraging statistical techniques and the rich dataset from DHS surveys, 
# WATOTO SURVIVAL aims to provide valuable insights for policymakers and 
# researchers in their efforts to address this critical issue. 
#
# Copyright © 2023 Kofiya Technologies
################################################################################

# Import dependence  ###########################################################
library(dplyr)        # for data wrangling and processing 
library(magrittr)     # for pipe operator
library(ggplot2)      # for visualization
library(skimr)        # to summarise data
library(survival)     # for survival analysis


# Set working directory  #######################################################
setwd("D:\\RStudioProjects\\watoto-survival\\data")


################################################################################
# Load data
################################################################################
library(readr)

surv_data <- readr::read_csv("./processed_mintilo_ai_community_version.csv")

# Check data
surv_data %>% 
  skimr::skim()


################################################################################
# Survival analysis for under five (U5) mortality rate
################################################################################
# Sanity checks
surv_data %>% 
  dplyr::select(age_child_months) %>% 
  summary()

# Distribution of age by death
ggplot2::ggplot(surv_data, aes(x=age_child_months, color=is_child_alive)) +
  ggplot2::geom_histogram()

# Check distribution of child mortality by age
surv_data %>% 
  dplyr::select(is_child_alive, age_child_months) %>% 
  dplyr::group_by(is_child_alive) %>% 
  skimr::skim()


# Create censoring variable (right censoring)   ################################
# - Our study endpoint is U5. Any death after 5 years should be censored.
surv_data <- surv_data %>% 
  dplyr::mutate(censored_u5 = ifelse((is_child_alive == 'no') 
                                     & (age_child_months < 60), 
                                     1,
                                     0))

# Distribution of censoring variable
#
# OUTPUT
# is_child_alive censored_u5     n
# no             1               537
# yes            0               8218
surv_data %>% 
  dplyr::count(is_child_alive, censored_u5)

# Drop input variable used to derive censored_u5
surv_data <- surv_data %>% 
  dplyr::select(-is_child_alive)



# Kaplan-Meier (KM) Analysis - Life Table ######################################
km_baseline <- survival::survfit(Surv(age_child_months, censored_u5) ~ 1,
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

# KM Analysis by place of residence (POR)   ####################################
km_por <- survival::survfit(Surv(age_child_months, 
                                 censored_u5) ~ place_of_residence,
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
cox_por <- survival::coxph(Surv(age_child_months, censored_u5) ~ place_of_residence,
                           data = surv_data)

summary(cox_por)


# ------------------------------------------------------------------------------
# Fit Cox PH model for all features
# ------------------------------------------------------------------------------
# Get features
feature_vars <- names(surv_data)
feature_vars <- subset(feature_vars, 
                       feature_vars != "age_child_months" 
                       & feature_vars != "censored_u5" 
                       & feature_vars != "survey_country"
                       & feature_vars != "survey_year"
                       & feature_vars != "cluster_number"
                       & feature_vars != "household_number"
                       & feature_vars != "respondent_line_number"
                       )

# Create data set with feature_vars and target vars
variable_of_interest <- c(feature_vars, 'age_child_months', 'censored_u5')

surv_data_ready <- surv_data %>% 
  dplyr::select(any_of(variable_of_interest))

# Fit model with all features
cox_all <- survival::coxph(Surv(age_child_months, censored_u5) ~ .,
                           data = surv_data_ready)

summary(cox_all)


################################################################################
# RESOURCES
################################################################################
# - https://rviews.rstudio.com/2017/09/25/survival-analysis-with-r/





