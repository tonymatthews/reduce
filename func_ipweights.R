
# CREATE IP WEIGHTS AT BASELINE 

func_ipweights <- function(input_data, 
                           confounders, 
                           trunc_sens = FALSE) {
  
  model_conditional <- paste("intervention ~ ", paste({{confounders}}, collapse = " + "), sep = "")
  model_unconditional <- paste("intervention ~ 1")
  
  # ipw denominator model
  p_denom <- glm(formula = model_conditional, 
                 family=binomial(), 
                 data=input_data)
  
  # ipw numerator model
  p_num <- glm(formula = model_unconditional,
               family=binomial(), 
               data=input_data)
  
  input_data$pd_intervention <- predict(p_denom, input_data, type="response")
  
  input_data$pn_intervention <- predict(p_num, input_data, type="response")
  
  #Compute estimated IP weights
  input_data$ipw <- ifelse(input_data$intervention==1, 1/input_data$pd_intervention, 
                             1/(1-input_data$pd_intervention))
  
  #Compute estimated standardized IP weights
  input_data$s_ipw <- ifelse(input_data$intervention==1, input_data$pn_intervention/input_data$pd_intervention, 
                          (1-input_data$pn_intervention)/(1-input_data$pd_intervention))
  

  #Truncate IPW and standardised IPW to 99th percentile, and keep only weights and id
    output <- input_data %>%
      mutate(ps = pd_intervention) %>% 
      mutate(ipw_trunc = ifelse(ipw >= quantile(ipw, 0.995), quantile(ipw, 0.995),ipw)) %>%
      mutate(s_ipw_trunc = ifelse(s_ipw >= quantile(s_ipw, 0.995), quantile(s_ipw, 0.995), s_ipw)) %>%
      select(id, intervention, ps, ipw, ipw_trunc, s_ipw, s_ipw_trunc)
  
  
  rm(model_conditional, model_unconditional, p_denom, p_num, input_data)
  gc()
  
  return(output)
  

  
}

