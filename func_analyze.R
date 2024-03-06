
func_analyze <- function(input_data,
                        analysis,
                        intervention,
                        outcome, 
                        censor, 
                        perprotocol = FALSE,
                        confounders,
                        missing,
                        bootstraps,
                        time_results,
                        trunc_sens = FALSE,
                        trunc = TRUE) {

# Preparation ------------------------------------------------------------------
  
# create the baseline data set with chosen way to deal with missing
dat <- func_cr_analyze(data = {{input_data}}, 
                       intervention = {{intervention}},
                       confounders = {{confounders}},
                       missing = {{missing}})

# Estimates of risk, rd and rr using IPW models --------------------------------

# Get point estimates for each time point, and average hr
if(analysis == "ipw") {
point_estimates <- func_model_ipw(input_data = dat,
                                  outcome = {{outcome}},
                                  censor = {{censor}},
                                  perprotocol = {{perprotocol}},
                                  confounders = {{confounders}},
                                  trunc_sens = {{trunc_sens}},
                                  trunc = {{trunc}})
} else if(analysis == "standardize") {
point_estimates <- func_model_standardize(input_data = dat,
                                  outcome = {{outcome}},
                                  censor = {{censor}},
                                  perprotocol = {{perprotocol}},
                                  confounders = {{confounders}})
}


# Bootstrapping - with same function inputs 
set.seed(1)
random.resample <- function(df_1) {df_1[sample(1:nrow(df_1), nrow(df_1), replace = TRUE), ]}
bootstrap <- function(n, df, analysis) {
  if(analysis=="ipw") {
    replicate(n, random.resample(df), FALSE) %>% 
      map(~ func_model_ipw(input_data = .,
                           outcome = {{outcome}},
                           censor = {{censor}},
                           perprotocol = {{perprotocol}},
                           confounders = {{confounders}},
                           trunc_sens = {{trunc_sens}})) %>%
      bind_rows() %>%
      group_by(time) %>% 
      summarise(cuminc_0_lci = quantile(cuminc_0, probs = 0.025, na.rm = T),
                cuminc_0_uci = quantile(cuminc_0, probs = 0.975, na.rm = T),
                cuminc_1_lci = quantile(cuminc_1, probs = 0.025, na.rm = T),
                cuminc_1_uci = quantile(cuminc_1, probs = 0.975, na.rm = T),
                rd_lci = quantile(rd, probs = 0.025, na.rm = T),
                rd_uci = quantile(rd, probs = 0.975, na.rm = T),
                logrr_lci = quantile(logrr, probs = 0.025, na.rm = T),
                logrr_uci = quantile(logrr, probs = 0.975, na.rm = T),
                loghr_lci = quantile(loghr, probs = 0.025, na.rm = T),
                loghr_uci = quantile(loghr, probs = 0.975, na.rm = T))
  } else if(analysis == "standardize"){
    replicate(n, random.resample(df), FALSE) %>% 
      map(~ func_model_standardize(input_data = .,
                           outcome = {{outcome}},
                           censor = {{censor}},
                           perprotocol = {{perprotocol}},
                           confounders = {{confounders}})) %>%
      bind_rows() %>%
      group_by(time) %>% 
      summarise(cuminc_0_lci = quantile(cuminc_0, probs = 0.025, na.rm = T),
                cuminc_0_uci = quantile(cuminc_0, probs = 0.975, na.rm = T),
                cuminc_1_lci = quantile(cuminc_1, probs = 0.025, na.rm = T),
                cuminc_1_uci = quantile(cuminc_1, probs = 0.975, na.rm = T),
                rd_lci = quantile(rd, probs = 0.025, na.rm = T),
                rd_uci = quantile(rd, probs = 0.975, na.rm = T),
                logrr_lci = quantile(logrr, probs = 0.025, na.rm = T),
                logrr_uci = quantile(logrr, probs = 0.975, na.rm = T),
                loghr_lci = quantile(loghr, probs = 0.025, na.rm = T),
                loghr_uci = quantile(loghr, probs = 0.975, na.rm = T))
  } # end if
} # end bootstrap


# Bring together point estimates and bootstrap confidence intervals (all time points) ----
full_results <- bootstrap(n = bootstraps, df = dat, analysis = {{analysis}}) %>% 
  left_join(point_estimates, by = "time") %>% 
  mutate(rr = exp(logrr),
         rr_lci = exp(logrr_lci),
         rr_uci = exp(logrr_uci),
         hr = exp(loghr),
         hr_lci = exp(loghr_lci),
         hr_uci = exp(loghr_uci)
         ) %>% 
  select(time, 
         events_0, events_1,
         cuminc_0, cuminc_0_lci, cuminc_0_uci,
         cuminc_1, cuminc_1_lci, cuminc_1_uci,
         rd, rd_lci, rd_uci,
         rr, rr_lci, rr_uci,
         hr, hr_lci, hr_uci)


# Risks at all times------------------------------------------------------------

risks_all <- full_results %>% 
  select(-c(hr, hr_lci, hr_uci)) %>%
  mutate(cuminc_0 = format(round(cuminc_0*100,1), nsmall=1), 
         cuminc_0_lci = format(round(cuminc_0_lci*100,1), nsmall=1),
         cuminc_0_uci = format(round(cuminc_0_uci*100,1), nsmall=1), 
         cuminc_1 = format(round(cuminc_1*100,1), nsmall=1), 
         cuminc_1_lci = format(round(cuminc_1_lci*100,1), nsmall=1),
         cuminc_1_uci = format(round(cuminc_1_uci*100,1), nsmall=1),
         rd = format(round(rd*100,1), nsmall=1), 
         rd_lci = format(round(rd_lci*100,1), nsmall=1),
         rd_uci = format(round(rd_uci*100,1), nsmall=1), 
         rr = format(round(rr,2), nsmall=2), 
         rr_lci = format(round(rr_lci,2), nsmall=2),
         rr_uci = format(round(rr_uci,2), nsmall=2)) %>% 
  mutate(cuminc_0 = paste0(cuminc_0, " (", cuminc_0_lci,", ", cuminc_0_uci, ")"),
         cuminc_1 = paste0(cuminc_1, " (", cuminc_1_lci,", ", cuminc_1_uci, ")"),
         rd = paste0(rd, " (", rd_lci,", ", rd_uci, ")"),
         rr = paste0(rr, " (", rr_lci,", ", rr_uci, ")")) %>% 
  select(time, events_1, events_0, cuminc_1, cuminc_0, rd, rr)
  

# Cut with only the risks at time points of interest and make table ------------
y <- time_results - 1
risks_table <- lapply(y, function(x) {
  risks_all %>% 
    filter(time == x) %>% 
    mutate(time = time + 1)
}) %>% 
  bind_rows()

# Get table with event, max risk difference, and hazard ratio ------------------

hr_table_temp <- select(full_results, time, hr, hr_lci, hr_uci) %>% 
  filter(time == 0) %>% 
  mutate(hr = format(round(hr,2), nsmall=2), 
         hr_lci = format(round(hr_lci,2), nsmall=2),
         hr_uci = format(round(hr_uci,2), nsmall=2)) %>%
  mutate(hr = paste0(hr, " (", hr_lci,", ", hr_uci, ")")) %>% 
  select(hr) 

hr_table_temp_eventsrisks <- risks_table %>% 
  filter(time == as.numeric(max(time_results))) %>% 
  select(time, events_1, events_0, cuminc_1, cuminc_0)

hr_table <- bind_cols(hr_table_temp_eventsrisks, hr_table_temp)

rm(hr_table_temp, hr_table_temp_eventsrisks)

# Plot ------------------------------------------------------------------------

x_label <- "Months"

plot_0 <- 
  full_results %>% 
  mutate(Intervention = as.factor(0),
         cuminc = cuminc_0*100,
         lci = cuminc_0_lci*100,
         uci = cuminc_0_uci*100) %>% 
  select(time, Intervention, cuminc, lci, uci)

plot_1 <- 
  full_results %>% 
  mutate(Intervention = as.factor(1),
         cuminc = cuminc_1*100,
         lci = cuminc_1_lci*100,
         uci = cuminc_1_uci*100) %>% 
  select(time, Intervention, cuminc, lci, uci)

max_time <- max(plot_0$time)

plot <- rbind(plot_1, plot_0) %>% 
ggplot(aes(x = time, y = cuminc, color = Intervention, fill = Intervention)) +
  geom_smooth(stat = "identity") +
  geom_ribbon(aes(ymin = lci, ymax = uci), alpha = 0.2, linetype = 0) +
  scale_y_continuous(expand = c(0,0), limits = c(0,25)) +
  scale_x_continuous(expand = c(0,0), limits = c(0,max(time_results)), breaks = time_results) +
  theme_classic(base_size = 13) +
  scale_color_nejm(labels = c("Beta blockers", "No beta blockers")) +
  scale_fill_nejm(labels = c("Beta blockers", "No beta blockers")) +
  xlab(x_label) + 
  ylab("Risk (%)") +
  theme(legend.title = element_blank(),
        legend.position = c(0.25, 0.9),
        text = element_text(size = 24),
        plot.margin = margin(15,15,15,15))

# IPW plots ------------------------------------------------------------------

# DATA SET UP

# if standardization, dont do IPW KM plot, just unadjusted

# If ppcensor_date is missing, just adjust at baseline 

# If specified a pp censor date, then censor at the date of non-adherence and adjust 
# time updated predictors of non-adherence and outcome

if(perprotocol == F) {

if(analysis == "standardize" & {{perprotocol}} == F) {
  model_data <- func_cr_persontime(input_data = dat,
                                        outcome = {{outcome}},
                                        censor = {{censor}},
                                        perprotocol = F)
}else if(analysis == "ipw" & {{perprotocol}} == F) {
  persontime_data <- func_cr_persontime(input_data = dat,
                                                outcome = {{outcome}},
                                                censor = {{censor}},
                                                perprotocol = F) 
  
  # Estimate baseline ipweights
  ip_weights <- func_ipweights(input_data = persontime_data[persontime_data$time==0,], 
                               confounders = {{confounders}})
  
  # Bring all together
  model_data <- inner_join(persontime_data, 
                           select(ip_weights, id, ipw, ipw_trunc, s_ipw, s_ipw_trunc), 
                           by = "id")  
}

# Data for KM plots

km_dat <- model_data %>% 
  group_by(id) %>% 
  mutate(outcome_max = func_max(outcome)) %>%
  mutate(intervention = factor(intervention, levels = c(1,0))) %>% 
  ungroup() %>% 
  filter(time==0)

survdiff(Surv(surv, outcome_max) ~ intervention, data=km_dat)

# ADJUSTED - only for IPW

if(analysis == "ipw") {

fit <- survfit(Surv(surv, outcome_max) ~ intervention, 
               weights = s_ipw_trunc,
               data=km_dat)

km_plot_ipw <- 
  ggsurvplot(fit, 
             data = km_dat, 
             fun = function(x) 100*(1-x),
             #conf.int = T,
             xlab = x_label,
             ylab ="Risk (%)",
             xlim = c(0, max(fit$time)),
             ylim = c(0, 25),
             main ="Product-Limit Survival Estimates", 
             risk.table = F,
             censor = F,
             legend.title = element_blank(),
             legend.labs = c("Beta blockers","No beta blockers"),
             legend = c(.2,.9),
             font.legend = c(12, "plain"),
             palette = "nejm") 

}

# UNADJUSTED - for both

fit <- survfit(Surv(surv, outcome_max) ~ intervention, 
               data=km_dat)

km_plot_unadjusted <- 
  ggsurvplot(fit, 
             data = km_dat, 
             fun = function(x) 100*(1-x),
             #conf.int = T,
             xlab = x_label,
             ylab ="Risk (%)",
             xlim = c(0, max(fit$time)),
             ylim = c(0, 25),
             main ="Product-Limit Survival Estimates", 
             risk.table = F,
             censor = F,
             legend.title = element_blank(),
             legend.labs = c("Beta blockers","No beta blockers"),
             legend = c(.2,.9),
             font.legend = c(12, "plain"),
             palette = "nejm") 

}


# Return -----------------------------------------------------------------------

output <- list()
output$hr_table <- hr_table
output$risks_table <- risks_table
output$risks_all <- risks_all
output$plot <- plot
if(perprotocol == F) {
output$km_plot_unadjusted <- km_plot_unadjusted
if(analysis == "ipw"){
output$km_plot_ipw <- km_plot_ipw
}
}
return(output)
}



