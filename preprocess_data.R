################################################################################
#
# Survival analysis for under-five (u5) mortality
#
# By Kofiya Technologies (https://kofiyatech.com/)
#
################################################################################



################################################################################
# Import library
################################################################################
library(readr)      # for loading csv data files
library(tidyr)      # for data wrangling
library(dplyr)      # for data transformation
library(stringr)    # for string/character manipulation
library(magrittr)   # for pipe operator
library(rlang)      # to unquote variable name in dplyr::mutate
library(zeallot)    # to enable R return multiple output values from a function call


################################################################################
# Config file paths
################################################################################
DIR_DATA = 'D:\\RStudioProjects\\watoto-survival'  # main path for dhs data storage 

FILENAME_DATA_KR = 'dhs-table-kr-ayele2017-survival-paper.csv'   # raw data table observations (RECORD1) obtained from children (KR) table
FILENAME_DATA_HR = 'dhs-table-hr-ayele2017-survival-paper.csv'   # raw data table observations (RECORD1) obtained from households (HR) table

FILENAME_METADATA_VARIABLE_NAME_KR = 'dhs-metadata-variable-name-kr-ayele2017-survival-paper.csv'   # metadata variable name (FlatRecordSpec) obtained from children (KR) metadata table
FILENAME_METADATA_VARIABLE_NAME_HR = 'dhs-metadata-variable-name-hr-ayele2017-survival-paper.csv'   # metadata variable name (FlatRecordSpec) obtained from children (HR) metadata table

FILENAME_METADATA_VARIABLE_VALUE_KR = 'dhs-metadata-variable-value-kr-ayele2017-survival-paper.csv'   # metadata variable value recode (FlatValuesSpec) obtained from children (KR) metadata table
FILENAME_METADATA_VARIABLE_VALUE_HR = 'dhs-metadata-variable-value-hr-ayele2017-survival-paper.csv'   # metadata variable value recode (FlatValuesSpec) obtained from children (HR) metadata table


################################################################################
# 1. Load data
################################################################################
################################################################################
# 1.1. Load data tables
################################################################################
# ------------------------------------------------------------------------------
# Load data table - KR
# ------------------------------------------------------------------------------
filepath_data <- file.path(DIR_DATA, FILENAME_DATA_KR)
data_kr <- readr::read_csv(file = filepath_data)

# View data
data_kr %>% head()

# Check shape of the data
dim(data_kr)

# Get a glimpse of the data
dplyr::glimpse(data_kr)


# ------------------------------------------------------------------------------
# Load data table - HR
# ------------------------------------------------------------------------------
filepath_data <- file.path(DIR_DATA, FILENAME_DATA_HR)
data_hr <- readr::read_csv(file = filepath_data)


################################################################################
# 1.2. Merge tables
################################################################################
data_merged <- dplyr::left_join(x=data_kr, y=data_hr, 
                                by=c("V001" = "HV001", "V002" = "HV002"))

# Sanity checks
dim(data_kr)
dim(data_hr)
dim(data_merged)

# ------------------------------------------------------------------------------
# Drop ID columns from the HR table after the merge. What we need to keep from 
# the HR table is HA35 (about smoking) for analysis.
# ------------------------------------------------------------------------------
IDENTIFIER_DROP = c('HHID', 'HV000', 'HV003', 'HV004', 'HV005')

data_merged <- data_merged %>% 
  dplyr::select(-one_of(IDENTIFIER_DROP))

dim(data_merged)

# Create analysis data
data_prep1 <- data_merged

# Housekeeping
rm(data_merged)


################################################################################
# 1.3. Load metadata tables
################################################################################
# Load metadata for variable name - KR
filepath_data <- file.path(DIR_DATA, FILENAME_METADATA_VARIABLE_NAME_KR)
metadata_variable_name_kr <- readr::read_csv(file = filepath_data)

metadata_variable_name_kr %>% head()


# Load metadata for variable name - HR
filepath_data <- file.path(DIR_DATA, FILENAME_METADATA_VARIABLE_NAME_HR)
metadata_variable_name_hr <- readr::read_csv(file = filepath_data)

metadata_variable_name_hr %>% head()


# Load metadata for variable value - KR
filepath_data <- file.path(DIR_DATA, FILENAME_METADATA_VARIABLE_VALUE_KR)
metadata_variable_value_kr <- readr::read_csv(file = filepath_data)

metadata_variable_value_kr %>% head()


# Load metadata for variable value - HR
filepath_data <- file.path(DIR_DATA, FILENAME_METADATA_VARIABLE_VALUE_HR)
metadata_variable_value_hr <- readr::read_csv(file = filepath_data)

metadata_variable_value_hr %>% head()


################################################################################
# 1.4. Create schema definition using metadata tables
################################################################################
# Clean metadata tables before merge
col_names_data <- dput(colnames(data_prep1))

metadata_variable_name_kr <- metadata_variable_name_kr %>% 
  dplyr::filter(Name %in% col_names_data)

metadata_variable_name_hr <- metadata_variable_name_hr %>% 
  dplyr::filter(Name %in% col_names_data)

# Create schema definition table for variables
schema_definition_variable <- dplyr::bind_rows(metadata_variable_name_kr, 
                                               metadata_variable_name_hr)

schema_definition_variable <- schema_definition_variable %>% 
  dplyr::select(Name, Label)

schema_definition_variable %>% head()
schema_definition_variable %>% tail()

# Add an identifier for variable to schema definition.
# - This step should be done by the analyst!
variable_identifier <- c(rep("id", 6), 
                         "feature_numeric", 
                         rep("feature_categorical", 3), 
                         "feature_numeric",
                         rep("feature_categorical", 5),
                         "feature_numeric", "feature_numeric",
                         "feature_categorical",
                         "feature_numeric", "feature_numeric",
                         "feature_categorical", "feature_categorical",
                         "feature_numeric", "feature_categorical",
                         "feature_categorical", "feature_categorical",
                         "response", "response", "response", "response",
                         "feature_numeric"
                         )

schema_definition_variable <- schema_definition_variable %>% 
  dplyr::mutate(Type = variable_identifier)

# ------------------------------------------------------------------------------
# Combine metadata table for variable VALUES
# ------------------------------------------------------------------------------
metadata_variable_value_merged <- dplyr::bind_rows(metadata_variable_value_kr, 
                                                   metadata_variable_value_hr)


################################################################################
# 2. Recode missing values as defined by DHS
################################################################################
# Get data
data_recoded_missing <- data_prep1

# Identify variables with too many missing values
# FINDINGS:
# 1. The response label indicator variables (such as B6, B7, HW1 and B13) have missing data by design. 
#    The variables are age_at_death, age_at_death_months_imputed, child_age_in_months and flag_for_age_at_death. 
# 2. The variable V701 has 71 subjects with missing values.
get_n_na <- function(df, vars=NULL) {
  ##############################################################################
  # A custom function to count number of missing values by column
  ##############################################################################
  if(!is.null(vars)) {
    df <- df %>% 
      dplyr::select(vars)
  }
  
  n_na <- df %>% 
    dplyr::summarise(vars = names(df), 
                     n_na = colSums(is.na(.)))
  
  return(n_na)
}

get_n_na(data_recoded_missing)
  
################################################################################
# 2.1. Check and recode missing values in Numeric features
################################################################################
# Get data
data_recoded_missing_numeric_features <- data_recoded_missing

# Get numeric features from schema definition table
numeric_features_name_list <- schema_definition_variable %>% 
  dplyr::filter(Type == 'feature_numeric') %>%
  dplyr::select(Name) %>%
  dplyr::pull()

numeric_features_name_list

# FINDINGS:
# V012 (respondents_current_age): MAX=49 --> GOOD
# V115 (time_to_get_to_water_source): MIN=1 - IS IT MINS OR HRS? MAX=999 LOOKS SUSPICIOUS --> CHECK!
# V133 (education_in_single_years): GOOD
# V136 (number_of_household_members_listed): GOOD
# V201 (total_children_ever_born): GOOD
# V212 (age_of_respondent_at_1st_birth): MIN=10 (LOOKS SUSPICIOUS) and MAX=39. CHECK!
# V525 (age_at_first_sex): MIN=8 (LOOKS SUSPICIOUS) and MAX=99. CHECK!
# V701 (husband_partners_education_level): MIN=0 and MAX=9. Moreover, 71 missing values. CHECK!
# HA35 (smoking): MAX=99 looks suspicious. CHECK!
data_recoded_missing_numeric_features %>% 
  dplyr::select(one_of(numeric_features_name_list)) %>% 
  summary()

# List of suspicious numeric features
suspicious_numeric_features = c('V115', 'V212', 'V525', 'HA35')

# ------------------------------------------------------------------------------
# Check and Recode missing value for V115
# FINDINGS: V115 (time_to_get_to_water_source): MIN=20 - IS IT MINS OR HRS? 
# MAX=999 LOOKS SUSPICIOUS --> CHECK!
# ------------------------------------------------------------------------------
# RECODINGS PROPOSAL
# -- 996 = 'On premises' --> set value to 0!
# -- 997, 998, 999, '' --> set value to NaN
# ------------------------------------------------------------------------------
# Check V115 in metadata variable value table
metadata_variable_value_kr %>% 
  dplyr::filter(Name == 'V115')

# Recode values
data_recoded_missing_numeric_features <- data_recoded_missing_numeric_features %>% 
  dplyr::mutate(V115_recoded = ifelse(V115 == 996,
                                      0,
                                      ifelse(V115 %in% c(800, 960, 995, 997, 998, 999),
                                             NA, V115)))

get_n_na(data_recoded_missing_numeric_features, c('V115', 'V115_recoded'))

# Sanity checks: old vs new variable values distribution
data_recoded_missing_numeric_features %>% 
  dplyr::select(V115, V115_recoded) %>% 
  summary()

# Drop original variable
data_recoded_missing_numeric_features <- data_recoded_missing_numeric_features %>% 
  dplyr::select(-c('V115'))


# ------------------------------------------------------------------------------
# Check and Recode missing value for V212
# FINDINGS: V212 (age_of_respondent_at_1st_birth): MIN=10 (LOOKS SUSPICIOUS) 
# and MAX=39. CHECK!
# ------------------------------------------------------------------------------
# RECODINGS PROPOSAL
# -- Nothing looks suspicious in the metadata table. Therefore, no need to 
#    recode here!
# ------------------------------------------------------------------------------
# Check V212 in metadata variable value table
metadata_variable_value_kr %>% 
  dplyr::filter(Name == 'V212')


# ------------------------------------------------------------------------------
# Check and Recode missing value for V525
# FINDINGS: V525 (age_at_first_sex): MIN=8 (LOOKS SUSPICIOUS) and MAX=99. CHECK!
# ------------------------------------------------------------------------------
# RECODINGS PROPOSAL
# -- 0, 97, 98, 99 --> set value to NaN
# ------------------------------------------------------------------------------
# Check V525 in metadata variable value table
metadata_variable_value_kr %>% 
  dplyr::filter(Name == 'V525')

# Recode values
data_recoded_missing_numeric_features <- data_recoded_missing_numeric_features %>% 
  dplyr::mutate(V525_recoded = ifelse(V525 %in% c(0, 97, 98, 99),
                                      NA,
                                      V525))  # The value=0 ("Not had sex") marked as missing since we're not sure what it would represent!

get_n_na(data_recoded_missing_numeric_features, c('V525', 'V525_recoded'))

data_recoded_missing_numeric_features %>% 
  dplyr::select(V525, V525_recoded) %>% 
  summary()


# Check frequency of 96 ("At first union") values!
data_recoded_missing_numeric_features %>% 
  dplyr::filter(V525 == 96) %>% 
  dim()

# Since many women has this value=96 (first sex at the union), we impute median value!
# Proxy value to impute = 15 years
data_recoded_missing_numeric_features %>% 
  dplyr::filter(V525_recoded != 96) %>% 
  dplyr::select(V525_recoded) %>% 
  summary()

# Recode values
data_recoded_missing_numeric_features <- data_recoded_missing_numeric_features %>% 
  dplyr::mutate(V525_recoded = ifelse(V525 %in% c(0, 97, 98, 99),
                                      NA,
                                      ifelse(V525 == 96,
                                             15,
                                             V525)))

data_recoded_missing_numeric_features %>% 
  dplyr::select(V525, V525_recoded) %>% 
  summary()

# Drop original variable
data_recoded_missing_numeric_features <- data_recoded_missing_numeric_features %>% 
  dplyr::select(-c('V525'))


# ------------------------------------------------------------------------------
# Check and Recode missing value for HA35
# FINDINGS: HA35 (Smoking (cigarettes in last 24 hours)): MAX=99
# ------------------------------------------------------------------------------
# RECODINGS PROPOSAL
# -- Recode 94 and 99 as missing values!
# ------------------------------------------------------------------------------
# Check HA35 in metadata variable value table
metadata_variable_value_hr %>% 
  dplyr::filter(Name == 'HA35')

# Recode values
data_recoded_missing_numeric_features <- data_recoded_missing_numeric_features %>% 
  dplyr::mutate(HA35_recoded = ifelse(HA35 %in% c(94, 99),
                                      NA,
                                      HA35))

get_n_na(data_recoded_missing_numeric_features, c('HA35', 'HA35_recoded'))

data_recoded_missing_numeric_features %>% 
  dplyr::select(HA35, HA35_recoded) %>% 
  summary()

# Drop original variable
data_recoded_missing_numeric_features <- data_recoded_missing_numeric_features %>% 
  dplyr::select(-c('HA35'))



################################################################################
# 2.2. Check and recode missing values in Categorical features
################################################################################
# Get data
data_recoded_missing_categorical_features <- data_recoded_missing_numeric_features

# Get list of categorical features
categorical_features_name_list <- schema_definition_variable %>% 
  dplyr::filter(Type == 'feature_categorical') %>%
  dplyr::select(Name) %>%
  dplyr::pull()

categorical_features_name_list

# Check suspicious values
# FINDINGS:
# V113, V116, V127, V128, V129, V130, V161, V701, V714: MAX=99/97/9 --> CHECK!
data_recoded_missing_categorical_features %>% 
  dplyr::select(one_of(categorical_features_name_list)) %>% 
  summary()


# ------------------------------------------------------------------------------
# Check and Recode missing value for V113
# FINDINGS: V113 (Source of drinking water): MAX=99
# ------------------------------------------------------------------------------
# RECODINGS PROPOSAL
# -- Recode 97 and 99 as missing values!
# ------------------------------------------------------------------------------
# Check V113 in metadata variable value table
metadata_variable_value_kr %>% 
  dplyr::filter(Name == 'V113')

# Recode values
data_recoded_missing_categorical_features <- data_recoded_missing_categorical_features %>% 
  dplyr::mutate(V113_recoded = ifelse(V113 %in% c(97, 99),
                                      NA,
                                      V113))

get_n_na(data_recoded_missing_categorical_features, c('V113', 'V113_recoded'))

data_recoded_missing_categorical_features %>% 
  dplyr::select(V113, V113_recoded) %>% 
  summary()

# Drop original variable
data_recoded_missing_categorical_features <- data_recoded_missing_categorical_features %>% 
  dplyr::select(-c('V113'))


# ------------------------------------------------------------------------------
# Check and Recode missing value for V116
# FINDINGS: V116 (Type of toilet facility): MAX=99
# ------------------------------------------------------------------------------
# RECODINGS PROPOSAL
# -- Recode 97 and 99 as missing values!
# ------------------------------------------------------------------------------
# Check V116 in metadata variable value table
metadata_variable_value_kr %>% 
  dplyr::filter(Name == 'V116')

# Recode values
data_recoded_missing_categorical_features <- data_recoded_missing_categorical_features %>% 
  dplyr::mutate(V116_recoded = ifelse(V116 %in% c(97, 99),
                                      NA,
                                      V116))

get_n_na(data_recoded_missing_categorical_features, c('V116', 'V116_recoded'))

data_recoded_missing_categorical_features %>% 
  dplyr::select(V116, V116_recoded) %>% 
  summary()

# Drop original variable
data_recoded_missing_categorical_features <- data_recoded_missing_categorical_features %>% 
  dplyr::select(-c('V116'))


# ------------------------------------------------------------------------------
# Check and Recode missing value for V127
# FINDINGS: V127 (Main floor material): MAX=99
# ------------------------------------------------------------------------------
# RECODINGS PROPOSAL
# -- Recode 97 and 99 as missing values!
# ------------------------------------------------------------------------------
# Check V127 in metadata variable value table
metadata_variable_value_kr %>% 
  dplyr::filter(Name == 'V127')

# Recode values
data_recoded_missing_categorical_features <- data_recoded_missing_categorical_features %>% 
  dplyr::mutate(V127_recoded = ifelse(V127 %in% c(97, 99),
                                      NA,
                                      V127))

get_n_na(data_recoded_missing_categorical_features, c('V127', 'V127_recoded'))

data_recoded_missing_categorical_features %>% 
  dplyr::select(V127, V127_recoded) %>% 
  summary()

# Drop original variable
data_recoded_missing_categorical_features <- data_recoded_missing_categorical_features %>% 
  dplyr::select(-c('V127'))


# ------------------------------------------------------------------------------
# Check and Recode missing value for V128
# FINDINGS: V128 (Main wall material): MAX=99
# ------------------------------------------------------------------------------
# RECODINGS PROPOSAL
# -- Recode 97 and 99 as missing values!
# ------------------------------------------------------------------------------
# Check V128 in metadata variable value table
metadata_variable_value_kr %>% 
  dplyr::filter(Name == 'V128') %>% 
  rmarkdown::paged_table()   # This is added to print out all rows!

# Recode values
data_recoded_missing_categorical_features <- data_recoded_missing_categorical_features %>% 
  dplyr::mutate(V128_recoded = ifelse(V128 %in% c(97, 99),
                                      NA,
                                      V128))

get_n_na(data_recoded_missing_categorical_features, c('V128', 'V128_recoded'))

data_recoded_missing_categorical_features %>% 
  dplyr::select(V128, V128_recoded) %>% 
  summary()

# Drop original variable
data_recoded_missing_categorical_features <- data_recoded_missing_categorical_features %>% 
  dplyr::select(-c('V128'))


# ------------------------------------------------------------------------------
# Check and Recode missing value for V129
# FINDINGS: V129 (Main roof material): MAX=99
# ------------------------------------------------------------------------------
# RECODINGS PROPOSAL
# -- Recode 97 and 99 as missing values!
# ------------------------------------------------------------------------------
# Check V129 in metadata variable value table
metadata_variable_value_kr %>% 
  dplyr::filter(Name == 'V129') %>% 
  rmarkdown::paged_table()

# Recode values
data_recoded_missing_categorical_features <- data_recoded_missing_categorical_features %>% 
  dplyr::mutate(V129_recoded = ifelse(V129 %in% c(97, 99),
                                      NA,
                                      V129))

get_n_na(data_recoded_missing_categorical_features, c('V129', 'V129_recoded'))

data_recoded_missing_categorical_features %>% 
  dplyr::select(V129, V129_recoded) %>% 
  summary()

# Drop original variable
data_recoded_missing_categorical_features <- data_recoded_missing_categorical_features %>% 
  dplyr::select(-c('V129'))


# ------------------------------------------------------------------------------
# Check and Recode missing value for V130
# FINDINGS: V130 (Religion): MAX=999
# ------------------------------------------------------------------------------
# RECODINGS PROPOSAL
# -- Recode 99 as missing values!
# ------------------------------------------------------------------------------
# Check V130 in metadata variable value table
metadata_variable_value_kr %>% 
  dplyr::filter(Name == 'V130') %>% 
  rmarkdown::paged_table()

# Recode values
data_recoded_missing_categorical_features <- data_recoded_missing_categorical_features %>% 
  dplyr::mutate(V130_recoded = ifelse(V130 %in% c(99),
                                      NA,
                                      V130))

get_n_na(data_recoded_missing_categorical_features, c('V130', 'V130_recoded'))

data_recoded_missing_categorical_features %>% 
  dplyr::select(V130, V130_recoded) %>% 
  summary()

# Drop original variable
data_recoded_missing_categorical_features <- data_recoded_missing_categorical_features %>% 
  dplyr::select(-c('V130'))


# ------------------------------------------------------------------------------
# Check and Recode missing value for V161
# FINDINGS: V161 (Type of cooking fuel): MAX=99
# ------------------------------------------------------------------------------
# RECODINGS PROPOSAL
# -- Recode 97 and 99 as missing values!
# ------------------------------------------------------------------------------
# Check V161 in metadata variable value table
metadata_variable_value_kr %>% 
  dplyr::filter(Name == 'V161') %>% 
  rmarkdown::paged_table()

# Recode values
data_recoded_missing_categorical_features <- data_recoded_missing_categorical_features %>% 
  dplyr::mutate(V161_recoded = ifelse(V161 %in% c(97, 99),
                                      NA,
                                      V161))

get_n_na(data_recoded_missing_categorical_features, c('V161', 'V161_recoded'))

data_recoded_missing_categorical_features %>% 
  dplyr::select(V161, V161_recoded) %>% 
  summary()

# Drop original variable
data_recoded_missing_categorical_features <- data_recoded_missing_categorical_features %>% 
  dplyr::select(-c('V161'))



# ------------------------------------------------------------------------------
# Check and Recode missing value for V714
# FINDINGS: V714 (Respondent currently working): MAX=9
# ------------------------------------------------------------------------------
# RECODINGS PROPOSAL
# -- Recode 99 as missing values!
# ------------------------------------------------------------------------------
# Check V714 in metadata variable value table
metadata_variable_value_kr %>% 
  dplyr::filter(Name == 'V714') %>% 
  rmarkdown::paged_table()

# Recode values
data_recoded_missing_categorical_features <- data_recoded_missing_categorical_features %>% 
  dplyr::mutate(V714_recoded = ifelse(V714 %in% c(9),
                                      NA,
                                      V714))

get_n_na(data_recoded_missing_categorical_features, c('V714', 'V714_recoded'))

data_recoded_missing_categorical_features %>% 
  dplyr::select(V714, V714_recoded) %>% 
  summary()

# Drop original variable
data_recoded_missing_categorical_features <- data_recoded_missing_categorical_features %>% 
  dplyr::select(-c('V714'))


# ------------------------------------------------------------------------------
# Check and Recode missing value for V701
# FINDINGS: V701 (husband/partner's education level): MAX=9 (LOOKS SUSPICIOUS). CHECK!
# ------------------------------------------------------------------------------
# RECODINGS PROPOSAL
# -- 8, 9 --> set value to NaN
# ------------------------------------------------------------------------------
# Check V701 in metadata variable value table
metadata_variable_value_kr %>%
  dplyr::filter(Name == 'V701')

# Recode values
data_recoded_missing_categorical_features <- data_recoded_missing_categorical_features %>%
  dplyr::mutate(V701_recoded = ifelse(V701 %in% c(8, 9),
                                      NA,
                                      V701))

get_n_na(data_recoded_missing_categorical_features, c('V701', 'V701_recoded'))

data_recoded_missing_categorical_features %>%
  dplyr::select(V701, V701_recoded) %>%
  summary()

# Drop original variable
data_recoded_missing_categorical_features <- data_recoded_missing_categorical_features %>% 
  dplyr::select(-c('V701'))


# Get final data from section #2
data_prep2 <- data_recoded_missing_categorical_features



################################################################################
# Update schema definition for the recoding task
################################################################################
recoded_numeric_features = c('V115', 'V525', 'HA35')
recoded_categorical_features <- c('V113', 'V116', 'V127', 'V128', 'V129', 'V130', 'V161', 'V714', 'V701')
recoded_all <- append(recoded_numeric_features, recoded_categorical_features)

schema_definition_variable <- schema_definition_variable %>% 
  dplyr::mutate(Recoded = ifelse(Name %in% recoded_all,
                                 TRUE,
                                 FALSE))

################################################################################
# 3. Recode categorical variables using DHS recode metadata
################################################################################
# Get data
data_recoded_categorical_features <- data_prep2


# View metadata for categorical features
schema_definition_variable %>% 
  dplyr::filter(Type == 'feature_categorical')



# Automate the above steps in a single function
recode_categorical_feature <- function(df, metadata_variable_value, 
                                       categirical_features_list, 
                                       schema_definition_variable) {
  for (var in categirical_features_list) {
    print(paste0('Processing var=', var))
    
    # Check variable name
    is_recoded_variable <- schema_definition_variable %>% 
      dplyr::filter(Name == var) %>% 
      dplyr::select(Recoded) %>% 
      dplyr::pull()
    
    if (is_recoded_variable) {
      varname_rhs <- paste0(var, "_recoded")
    } else {
      varname_rhs <- var
    }
      
    
    # Start recoding
    skip_to_next <- FALSE
    
    tryCatch(expr = {
      # Step 1. Get recoding values from metadata table
      metadata_variable_value_selected <- metadata_variable_value %>% 
        dplyr::filter(Name == var)
      
      # Step 2. Create a dictionary
      metadata_variable_value_selected_dict <- metadata_variable_value_selected %>% 
        dplyr::select(Value, ValueDesc) %>% 
        tibble::deframe()
      
      # Step 3. Recode variable value using the dictionary
      # Inspired by: 
      # - https://stackoverflow.com/questions/70588354/create-a-new-column-based-on-a-dictionary-using-r
      # - https://stackoverflow.com/questions/46131829/unquote-the-variable-name-on-the-right-side-of-mutate-function-in-dplyr
      varname_lhs <- paste0(var, "_recoded")
      
      df <- df %>%
        dplyr::mutate(!!varname_lhs := dplyr::recode(!!rlang::sym(varname_rhs), 
                                                     !!!metadata_variable_value_selected_dict, .default=NA_character_))
      
      # Step 4. Drop the original variable after recoding
      if (!is_recoded_variable) {
        # Drop variable
        df <- df %>% 
          dplyr::select(-c(var))
        
        # Update schema definition table
        schema_definition_variable <- schema_definition_variable %>% 
          dplyr::mutate(Recoded = ifelse(Name == var,
                                         TRUE,
                                         Recoded))
      }
    },
    error = function(e){
      print(paste0("WARNING - Something went wrong with var=", var, " during recoding!"))
      
      skip_to_next <<- TRUE
    })
    
    if(skip_to_next) { next }
  }
  
  return(list(df, schema_definition_variable))
}

# Recode all categorical features
c(data_recoded_categorical_features_final, schema_definition_variable) %<-% recode_categorical_feature(data_recoded_categorical_features, 
                                                                                                       metadata_variable_value_merged, 
                                                                                                       categorical_features_name_list,
                                                                                                       schema_definition_variable)

# Do some sanity changes compared to the original variable
data_recoded_categorical_features %>%
  dplyr::group_by(V025) %>%
  dplyr::tally()

data_recoded_categorical_features_final %>%
  dplyr::group_by(V025_recoded) %>%
  dplyr::tally()


# Create data set for this section
data_prep3 <- data_recoded_categorical_features_final



################################################################################
# 4. Create final data set and final schema definition
################################################################################
# Get final data
data_anal_final <- data_prep3

dim(data_anal_final)


# Save the final data set
filepath_output <- file.path(DIR_DATA, 'dhs-table-preprocessed-ayele2017-survival-paper.csv')
readr::write_csv(x=data_anal_final, file=filepath_output)


# Save the schema definition
filepath_output <- file.path(DIR_DATA, 'schema-definition-variable-ayele2017-survival-paper.csv')
readr::write_csv(x=schema_definition_variable, file=filepath_output)



