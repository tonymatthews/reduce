
func_cr_persontime <- function(input_data, 
                               outcome_time, 
                               censor_time,
                               perprotocol = FALSE) {
  
# CREATE VECTOR OF ALL TIME UPDATED VARIABLE IN DATA----------------------------   
tu <- colnames({{input_data}}) %>% str_subset(pattern = "_tu")
tu_baseline <- gsub("_tu", "_baseline", tu)

# PERSON TIME DATA WITHOUT CENSORING AT PP DEVIATION ---------------------------

if(!{{perprotocol}}) {
# Create person time data with all covariates (baseline and time updated)
input_data %>% 
    
    # Create new id (for bootstrapping purpose)
    mutate(id = dplyr::row_number()) %>% 
    
    # Drop lopnr
    select(-lopnr) %>% 
    
    # Create time that each person exits due to event or lossfup
    rowwise() %>% 
    mutate(lossfup_date_min = min({{censor_time}}, na.rm = TRUE)) %>% 
    mutate(exit_date = min(c({{outcome_time}}, lossfup_date_min), na.rm = TRUE)) %>%
    ungroup() %>% 
    
    # create binary for outcome 
    mutate(outcome_fixed = case_when(
      {{outcome_time}} == exit_date ~ 1L,
      is.na({{outcome_time}}) ~ 0L, 
      TRUE ~ 0L
    )) %>% 
    
    # create binary for lossfup 
    mutate(lossfup_fixed = case_when(
      lossfup_date_min == exit_date ~ 1L,
      is.na(lossfup_date_min) ~ 0L, 
      TRUE ~ 0L
    )) %>% 
    
    # Make surv variable 
    mutate(surv = exit_date) %>% 
    
    # Split period until leave
    expandRows("surv", drop=F) %>% 
    
    # Create a variable that indicates the time period
    mutate(time = sequence(rle(id)$lengths)-1) %>% 
    
    # Create binary variable for outcome time updated (from k+1)
    mutate(outcome = ifelse(time == surv - 1 & outcome_fixed == 1, 1, 0)) %>%
    
    # Create binary variable for lossfup time updated (from k+1)
    mutate(lossfup = ifelse(time == surv - 1 & lossfup_fixed == 1, 1, 0)) %>%
    
    # if lossfup, make event missing in that period
    mutate(outcome = ifelse(lossfup==1, NA, outcome)) %>% 
    
    #create powers of time to allow time dependent baseline hazard
    mutate(time_2 = time^2,
           time_3 = time^3) %>%
    relocate(time, .before=time_2) %>% 
    
    # remove unwanted variables
    select(-c(contains("min"), contains("date"), contains("_time"), contains("fixed"))) %>% 
    
    select(id, intervention, outcome, lossfup, surv, time, time_2, time_3, everything())

  } # end if(is.na(ppcensor_date)) 

# PERSON TIME DATA CENSORING AT PP DEVIATION -----------------------------------

else if(perprotocol) {
  
  input_data %>% 
    
    # Create new id (for bootstrapping purpose)
    mutate(id = dplyr::row_number()) %>% 
    
    # Drop lopnr
    select(-lopnr) %>% 
    
    # Create time that each person exits due to event or lossfup
    rowwise() %>% 
    mutate(lossfup_date_min = min({{censor_time}}, na.rm = TRUE)) %>% 
    mutate(exit_date = min(c({{outcome_time}}, lossfup_date_min, pp_censor_time), na.rm = TRUE)) %>%
    ungroup() %>% 
    
    # create binary for outcome 
    mutate(outcome_fixed = case_when(
      {{outcome_time}} == exit_date ~ 1L,
      is.na({{outcome_time}}) ~ 0L, 
      TRUE ~ 0L
    )) %>% 
    
    # create binary for lossfup 
    mutate(lossfup_fixed = case_when(
      lossfup_date_min == exit_date ~ 1L,
      is.na(lossfup_date_min) ~ 0L, 
      TRUE ~ 0L
    )) %>% 
    
    # Create binary for pp censor
    mutate(ppcensor_fixed = case_when(
      pp_censor_time == exit_date ~ 1L,
      is.na(pp_censor_time) ~ 0L, 
      TRUE ~ 0L
    )) %>% 
    
    # Make surv variable 
    mutate(surv = exit_date) %>% 
    
    # Split period until leave
    expandRows("surv", drop=F) %>% 
    
    # Create a variable that indicates the time period
    mutate(time = sequence(rle(id)$lengths)-1) %>% 
    
    # Create binary variable for outcome time updated (from k+1)
    mutate(outcome = ifelse(time == surv - 1 & outcome_fixed == 1, 1, 0)) %>%
    
    # Create binary variable for lossfup time updated (from k+1)
    mutate(lossfup = ifelse(time == surv - 1 & lossfup_fixed == 1, 1, 0)) %>%
    
    # Create binary variable for pp censoring time updated (from k+1)
    mutate(ppcensor = ifelse(time == surv - 1 & ppcensor_fixed == 1, 1, 0)) %>%
    
    # if lossfup or pp censored, make event missing in that period
    mutate(outcome = ifelse(lossfup==1, NA, outcome)) %>% 
    mutate(outcome = ifelse(ppcensor==1, NA, outcome)) %>% 
    
    # Update the time updated covariates to be binary and change during period there is an update
    # Also takes the baseline variable into account
    mutate(across(.cols = tu_baseline,
                  .fns = ~ .,
                  .names = "{.col}_temp"
    )) %>% 
    rename_at(.vars = vars(ends_with("_baseline_temp")),
              .funs = funs(sub("_baseline_temp", "_tu_temp", .))) %>% 
    mutate(across(.cols = tu,
                  .fns = ~case_when(
                    get(glue::glue("{cur_column()}_temp")) == 1 ~ 1, 
                    get(glue::glue("{cur_column()}_temp")) == 0 & . <= time ~  1,
                    get(glue::glue("{cur_column()}_temp")) == 0 & . > time ~  0
                  )
    )) %>% 
    
    select(-contains("tu_temp")) %>% 
    
    # Make timeupdated variables factors
    mutate_at(vars(tu), list(~factor(.))) %>% 
    
    #create powers of time to allow time dependent baseline hazard
    mutate(time_2 = time^2,
           time_3 = time^3) %>%
    relocate(time, .before=time_2) %>% 
    
    # remove unwanted variables
    select(-c(contains("min"), contains("date"), contains("_time"), contains("fixed"))) %>% 
    
    select(id, intervention, outcome, lossfup, ppcensor, surv, time, time_2, time_3, everything())
  
} # end else if(!is.na(ppcensor_date))
  
} # end function

