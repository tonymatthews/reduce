
# R-PROFILE --------------------------------------------------------------------
source("W:/C6_Berglund/amatthews/r-profile.R")

# PACKAGES ---------------------------------------------------------------------
library(here)
library(dplyr)
options(dplyr.summarise.inform = FALSE)
library(purrr)
library(tableone)
library(splitstackshape)
library(tidyr)
library(ggplot2)
library(survival)
library(survminer)
library(survey)
library(tictoc)
library(pammtools)
library(stringr)
library(gridExtra)
library(WeightIt)
library(cobalt)
library(splines)
library(rlang)
library(labelled)
library(berryFunctions)
library(ggsci)
library(tictoc)
library(rsample)
library(lobstr)
library(rms)
library(furrr)
library(future.apply)
library(parallel)

# LOAD MAIN DATA ---------------------------------------------------------------

load("W:/C6_Berglund/data/procdata/reduce-emulation/cr_data_main.Rda")

# FUNCTIONS --------------------------------------------------------------------

# 1. Create analyzable baseline data frames (restricted to confounders of interest)
source(here("scripts", "func_cr_analyze.R"))
# INPUT: data - which dataframe is being changed
#        confounders - which list of confounders should it be restricted to
#        missing - which format should missing data be in: 
#                       "base" - original data, but only restricted to specified confounding variables
#                       "imputemedian" - restricted to specified confounding variable and missing numeric variables imputed with median

# 2. Create analyzeable data frames, with persontime data by some period of time
source(here("scripts", "func_cr_persontime.R"))
# INPUT: input_data - which dataframe is being updated
#        fustart_date - specify variable that indicates start of follow up date
#        outcome_date - specify variable that indicates outcome date
#        censor_date - specify variable that indicates censoring date
#        time_cut - specify the number of days by which you want to split data (e.g., 7=weekly data, 30=monthly data)

# 3. Create table one
source(here("scripts", "func_cr_tableone.R"))
# INPUT: input_data - data to be used to create table one
#         intervention - specify exposure variable in quotation marks ""
#         weighted - T or F are the data weighted?
#         txt_name - name of txt file that will be stored in output

# 4. IP weights
source(here("scripts", "func_ipweights.R"))
# INPUT: input_data - which dataframe you want to build the models on 
#        confounders - which list of confounders you want to use

# 4. Adherence IP weights
source(here("scripts", "func_ipweights_adherence.R"))
# INPUT: input_data - which dataframe you want to build the models on 
#        confounders_baseline - which list of confounders you want to use - all baseline
#        confounders_timeupdated - which list of confounders you want to use - all baseline and time updated

# 5. IPW modelling
source(here("scripts", "func_model_ipw.R"))
# INPUT: input_data - which dataframe is being used (must be one row per person)
#        fustart_date - specify variable that indicates start of follow up date
#        outcome - specify variable that indicates outcome date
#        censor - specify variable that indicates censoring date
#        time_cut - specify the number of days by which you want to split data (e.g., 7=weekly data, 30=monthly data)
#        confounders - which list of confounders you want to use

# 5. Standardization modelling
source(here("scripts", "func_model_standardize.R"))
# INPUT: input_data - which dataframe is being used (must be one row per person)
#        fustart_date - specify variable that indicates start of follow up date
#        outcome - specify variable that indicates outcome date
#        censor - specify variable that indicates censoring date
#        time_cut - specify the number of days by which you want to split data (e.g., 7=weekly data, 30=monthly data)
#        confounders - which list of confounders you want to use


# 6. Run analyses (can update this to include other options than IPW)
source(here("scripts", "func_analyze.R"))
# INPUT: input_data - which dataframe is being used (must be one row per person)
#        intervention - specify the variable coresponding the to the main treatment at start of fup
#        fustart_date - specify variable that indicates start of follow up date
#        outcome - specify variable that indicates outcome date
#        censor - specify variable that indicates censoring date
#        perprotocol - specify if you are running a per protocol analysis, i.e., censoring at deviation from the protocol and futher adjustment 
#                 If TRUE here, the confounder list must also include time updated covariates 
#        time_cut - specify the number of days by which you want to split data (e.g., 7=weekly data, 30=monthly data)
#        confounders - which confounders you want to use - must include a list of the variables, and adjustment set (with interactions etc.)
#        missing - which format should missing data be in: 
#                "base" - original data
#                "imputemedian" - missing numeric variables imputed with median
#        bootstraps - number of bootstraps,
#        time_results - cuts which want to create a table for (based on time_cut above) 

# 7. Deal with missing variables 
source(here("scripts", "func_min_max.R"))


# SPECIFY LISTS OF CONFOUNDERS -------------------------------------------------
source(here("scripts", "01_an_confounders.R"))

# TABLE ONE --------------------------------------------------------------------
source(here("scripts", "02_an_tableone.R"))

# ANALYSES - PRIMARY AND SENSITIVITIES -----------------------------------------
source(here("scripts", "03_an_analysis.R"))





