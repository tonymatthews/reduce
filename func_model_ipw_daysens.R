
func_model_ipw <- function(input_data, 
                           outcome, 
                           censor, 
                           perprotocol = FALSE,
                           confounders,
                           trunc = TRUE,
                           trunc_sens = FALSE) {
  
# Set up data ------------------------------------------------------------------

# If ppcensor_date is missing, just adjust at baseline 

# If specified a pp censor date, then censor at the date of non-adherence and adjust 
# time updated predictors of non-adherence and outcome
if(!{{perprotocol}}) {
  persontime_data <- func_cr_persontime(input_data = {{input_data}},
                                                outcome = {{outcome}},
                                                censor = {{censor}},
                                                perprotocol = FALSE) 
  
  
  # Estimate baseline ipweights
  ip_weights <- func_ipweights(input_data = persontime_data[persontime_data$time==0,], 
                               confounders = {{confounders}},
                               trunc_sens = {{trunc_sens}})
  
  # Bring all together
  model_data <- inner_join(persontime_data, 
                           select(ip_weights, id, ipw, ipw_trunc, s_ipw, s_ipw_trunc), 
                           by = "id")  
  rm(persontime_data, ip_weights)
  gc()
  
} else if({{perprotocol}}) {
  
  persontime_data <- func_cr_persontime(input_data = {{input_data}},
                                                outcome = {{outcome}},
                                                censor = {{censor}},
                                                perprotocol = TRUE)

  
  confounders_timeupdated <- {{confounders}}
  confounders_baseline <- confounders_timeupdated[!grepl("_tu", confounders_timeupdated)]
  
  # Estimate baseline ipweights
  ip_weights <- func_ipweights(input_data = persontime_data[persontime_data$time==0,], 
                               confounders = confounders_baseline)
  
  # Estimate timevarying weights for adherence
  ip_weights_adherence <- func_ipweights_adherence(input_data = persontime_data,
                                                   confounders_baseline = confounders_baseline,
                                                   confounders_timeupdated = confounders_timeupdated) 
  
  
  ip_weights_multiplied <- ip_weights %>% 
    left_join(ip_weights_adherence, by = "id") %>% 
    mutate(ipw = ipw * adh_ipw,
           ipw_trunc = ipw_trunc * adh_ipw_trunc,
           s_ipw = s_ipw * adh_s_ipw, 
           s_ipw_trunc = s_ipw_trunc * adh_s_ipw_trunc) %>% 
    select(-contains("adh"))
  
  
    # Bring all together
    model_data <- inner_join(persontime_data, 
                             select(ip_weights_multiplied, id, time, ipw, ipw_trunc, s_ipw, s_ipw_trunc), 
                             by = c("id", "time"))  
    
    rm(persontime_data, ip_weights, ip_weights_adherence, ip_weights_multiplied)
    gc()
    
}
  

# Number of events -------------------------------------------------------------
  
events <- model_data %>% 
    group_by(time, intervention) %>% 
    summarise(events = sum(outcome, na.rm = T)) %>% 
    ungroup() %>% 
    pivot_wider(names_from = intervention, values_from = events) %>% 
    rename(events_0 = "0",
           events_1 = "1") %>% 
    mutate(events_0 = cumsum(events_0),
           events_1 = cumsum(events_1))

# Model and estimation of risks, RD, and RR -------------------------------------

model_hr <- as.formula(paste("outcome==1 ~ intervention + 
                       rcs(time, c(180,360,720,1440))"))

model_risk <- as.formula(paste("outcome==1 ~ intervention + 
                        intervention*rcs(time, c(180,360,720,1440))"
                       ))

if (trunc == T) {
ipw_model_hr <- glm(model_hr,
                    family=quasibinomial(), 
                    weight=s_ipw_trunc,
                    data=model_data)

ipw_model_risk <- glm(model_risk,
                      family=quasibinomial(), 
                      weight=s_ipw_trunc,
                      data=model_data)

} else if (trunc == F) {

ipw_model_hr <- glm(model_hr,
                    family=quasibinomial(), 
                    weight=s_ipw,
                    data=model_data)

ipw_model_risk <- glm(model_risk,
                      family=quasibinomial(), 
                      weight=s_ipw,
                      data=model_data)
}


#Create datasets with all time points under each treatment level
intervention_0 <- data.frame(0, cbind(seq(0,max(model_data$time)), seq(0,max(model_data$time))^2, (seq(0,max(model_data$time))^3)))
intervention_1 <- data.frame(1, cbind(seq(0,max(model_data$time)), seq(0,max(model_data$time))^2, (seq(0,max(model_data$time))^3)))

colnames(intervention_0) <- c("intervention", "time", "time_2", "time_3")
colnames(intervention_1) <- c("intervention", "time",  "time_2", "time_3")

# assignment of 1-prob of outcome to each person time
intervention_0$p_0 <- 1 - predict(ipw_model_risk, intervention_0, type="response")
intervention_1$p_1 <- 1 - predict(ipw_model_risk, intervention_1, type="response")

# survival
intervention_0$s_0 <- cumprod(intervention_0$p_0)
intervention_1$s_1 <- cumprod(intervention_1$p_1)

#merge both datasets together & calculate cuminc
estimates <- merge(intervention_0, intervention_1, by=c("time", "time_2", "time_3")) %>% 
  merge(events, by = "time") %>% 
  mutate(cuminc_0 = 1-s_0,
         cuminc_1 = 1-s_1,
         rd = cuminc_1-cuminc_0,
         logrr = log(cuminc_1/cuminc_0)) %>% 
  arrange(time) %>% 
  mutate(loghr = ifelse(time == 0, summary(ipw_model_hr)$coefficients[2,1], NA)) %>% 
  select(time, events_0, events_1, cuminc_0, cuminc_1, rd, logrr, loghr) 

rm(model_data, events, model_hr, model_risk, ipw_model_hr, ipw_model_risk, 
   intervention_1, intervention_0)
gc()

return(estimates)

}
