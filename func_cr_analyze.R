

func_cr_analyze <- function(data, intervention, confounders, missing) {
  
  # FUNCTIONS ------------------------------------------------------------------
  # function to impute median for missing numerical variables  
  func_impute_median <- function(data_input, varlist) {
    data_input[varlist] <- lapply(data_input[varlist], function(x) {
      if(is.numeric(x)) x[is.na(x)] <- median(x, na.rm = TRUE)
      return(x)
    })
    return(data_input[varlist])
  }
  
  # function to find non-missing (or 9 for factors) rows
  func_completecase <- function(data_input, varlist) {
    # get numeric (without id) and factor variable names
    num <- unlist(lapply(data_input[varlist], is.numeric))
    varlist_numeric <- ls(select(data_input[varlist], is.numeric))
    varlist_numeric <- varlist_numeric[!(varlist_numeric %in% c("lopnr"))]
    varlist_factor <- ls(select(data_input[varlist], is.factor))
    # drop missing for numeric and 9 for factor
    data <- data_input %>% 
      filter_at(vars(varlist_numeric), all_vars(!is.na(.))) %>% 
      filter_at(vars(varlist_factor), all_vars(.!=9))
    return(data[varlist])
  }
  
  
  # IF STATEMENT FOR CONFOUNDERS -----------------------------------------------
  if (missing(confounders)) {
    temp_dat <- data %>% 
      rename(intervention = {{intervention}})
    
  } else if (!missing(confounders)) {
    # remove any higher order terms rom confounder list 
    confounders <- confounders[!grepl("\\^2", confounders)]
    confounders <- confounders[!grepl("\\^3", confounders)]
    confounders <- confounders[!grepl("\\^4", confounders)]
    
    # update the confounder list to only include the variable name of the splines
    confounders_spline <- confounders[str_detect(confounders, "ns\\(")]
    confounders_spline <- str_extract(confounders_spline, "(?<=ns\\()\\w+(?=,)")
    confounders <- confounders[!grepl("ns\\(", confounders)]
    confounders <- c(confounders, confounders_spline)
    
    
    temp_dat <- data %>% 
      rename(intervention = {{intervention}}) %>% 
      select("id", "lopnr", "intervention",
             "pp_censor_time",  
             "death_time", "mi_time", "composite_time",
             "endfup_time", "maxfup_time", all_of(confounders))
      
  }
  
  # IF STATEMENT FOR MISSING ---------------------------------------------------
  
  # Create the list of variables to be processed
  var_list <- ls(temp_dat)
  var_list <- var_list[!(var_list %in% c("lopnr", "intervention",
                                         "pp_censor_time",  
                                         "death_time", "mi_time", "composite_time",
                                         "endfup_time", "maxfup_time"))] 
  
  # Go through all he 
  if (missing == "main") {
    return(temp_dat)
  }
  
  # impute median when numerical are missing
  else if (missing == "imputemedian") {
    
    # get imputed covariates
    cr_imputed <- func_impute_median(temp_dat, var_list)
    
    # return imputed dataset
    return(temp_dat[c("id", "lopnr", "intervention",
                      "pp_censor_time", 
                      "death_time", "mi_time", "composite_time",
                      "endfup_time", "maxfup_time")] %>% 
              left_join(cr_imputed, by = "id")
    )
  
  } # end  else if (missing == "imputemedian")
  
  else if (missing == "completecase") {
    
    # get complete data 
    cr_complete <- func_completecase(temp_dat, var_list)
    
    return(temp_dat[c("id", "lopnr", "intervention",
                      "pp_censor_time", 
                      "death_time", "mi_time", "composite_time",
                      "endfup_time", "maxfup_time")] %>% 
             inner_join(cr_complete, by = "id")
    )
    
  } # end else if (missing == "completecase")
  
  else if (missing == "continuoustocat") {
    
    return(
    temp_dat %>% 
      # create the factor levels
      mutate(age = cut(age, breaks = c(0,55,70,85,999)),
             heart_rate = cut(heart_rate, breaks = c(0,59,100,999)),
             systolic_blood_pressure = cut(systolic_blood_pressure, breaks = c(0,119,139,999)),
             diastolic_blood_pressure = cut(diastolic_blood_pressure, breaks = c(0,79,89,999)),
             d_ldl_cholesterol = cut(d_ldl_cholesterol, breaks = c(0, 3.3, 4.8, 999)),
             d_hdl_cholesterol = cut(d_hdl_cholesterol, breaks = c(0, 0.9, 1.5, 999)),
             d_s_creatinin = cut(d_s_creatinin, breaks = c(0,44,59,89,999)),
             d_bmi = cut(d_bmi, breaks = c(0, 18.4, 24.9, 29.9, 999))
      ) %>% 
      # make NA a factor level 
      mutate(across(c(age, heart_rate, systolic_blood_pressure, diastolic_blood_pressure,
                      d_ldl_cholesterol, d_hdl_cholesterol, d_s_creatinin, d_bmi),
                    ~addNA(.)))
    )
    
  } # end else if (missing == "continuoustocat")
  
  else {stop("FUNCTION NOT DEFINED PROPERLY")}
    
}

  
    
    