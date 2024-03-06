
# 1. Based on all variables in full dataset
# func_cr_tableone(input_data = cr_data_main, 
#                  intervention = "itt",
#                  txt_name = "cr_tableone_base")


# 2. Based on variables that have been specified as main confounders
func_cr_tableone(input_data = func_cr_analyze(data = cr_data_main, 
                                              intervention = "itt",
                                              confounders = an_confounders$full,
                                              missing = "main"), 
                 intervention = "intervention",
                 txt_name = "cr_tableone_main")

# 4. Weighted table one - main confounders, imputed median
weighted_data <- inner_join(func_cr_analyze(data = cr_data_main,
                                            intervention = "itt",
                                            confounders = an_confounders$full,
                                            missing = "imputemedian"), 
                            select(func_ipweights(input_data = func_cr_analyze(data = cr_data_main,
                                                                               intervention = "itt",
                                                                               confounders = an_confounders$full,
                                                                               missing = "imputemedian"),
                                                  confounders = an_confounders$full), id, s_ipw_trunc),
                            by = "id") %>% 
  svydesign(ids = ~1, data = ., weight = ~s_ipw_trunc) 

func_cr_tableone(input_data = weighted_data, 
                 weight = TRUE,
                 intervention = "intervention",
                 txt_name = "cr_tableone_main_weighted")

# Make tableone main have  with additional ipweighted smd, and name variables
ipw <-  select(read.delim(here("output","cr_tableone_main_weighted.txt")), X, SMD) %>% 
  filter(SMD != "") %>% 
  rename("IPW SMD" = SMD)

base <-  read.delim(here("output","cr_tableone_main.txt")) %>% 
  left_join(ipw, by = "X") %>% 
  rename("Beta blockers" = "Beta.blockers") %>% 
  rename("No beta blockers" = "No.beta.blockers") %>% 
  relocate(Missing, .before = "SMD") %>% 
  mutate(X = str_remove(X, " \\(%\\)")) %>% 
  mutate(X = str_remove(X,  " \\(median \\[IQR\\]\\)")) %>% 
  mutate(X = case_when(
    X == "n" ~ "N",
    X == "age" ~ "Age",
    X == "centreid_ic" ~ "Hospital",
    X == "eligible_year" ~ "Year of index",
    X == "d_gender" ~ "Female",
    X == "infarcttype" ~ "NSTEMI",
    X == "smoking_status" ~ "Smoking status",
    X == "hypertension" ~ "Hypertension",
    X == "diabetes" ~ "Diabetes",
    X == "previous_mi" ~ "Myocardial infarction",
    X == "previous_stroke" ~ "Stroke",
    X == "history_of_chf" ~ "Chronic heart failure",
    X == "previous_pci" ~ "Percutaneous coronary intervention",
    X == "prior_cardiac_surgery" ~ "Cardiac surgery",
    X == "other_serious_disease" ~ "Other serious diseases",
    X == "renal_pat_baseline" ~ "Renal disease",
    X == "arb_pdr_baseline" ~ "Angiotensin 2 receptor blockers",
    X == "ace_pdr_baseline" ~ "ACE inhibitors",
    X == "ccb_pdr_baseline" ~ "Calcium channel blockers",
    X == "diuretics_pdr_baseline" ~ "Diuretics",
    X == "nitrates_pdr_baseline" ~ "Nitrates",
    X == "diabetes_pdr_baseline" ~ "Diabetes treatment",
    X == "cpr_before_hospital" ~ "Cardiopulmonary resuscitation",
    X == "thromb_before_hospital" ~ "Thrombolysis",
    X == "cardiac_shock" ~ "Cardiogenic shock",
    X == "ecg_rhythm" ~ "ECG ryhthm",
    X == "ecg_qrs_annotation" ~ "ECG QRS annotation",
    X == "ecg_stt_changes" ~ "ECG ST- & T- wave changes",
    X == "d_repertreatment" ~ "Reperfusion treatment",
    X == "d_pci" ~ "Percutaneous coronary intervention",
    X == "fynd" ~ "Angiography finding",
    X == "stenosklass" ~ "Stenosis class",
    X == "segment" ~ "Proportion stenosis",
    X == "d_beta_blockers_treat" ~ "Intravenous beta blockers",
    X == "d_diuretics_treat" ~ "Intravenous diuretics",
    X == "d_inotropes" ~ "Intravenous intropic drugs",
    X == "d_nitrates_treat" ~ "Intravenous nitrates",
    X == "heart_rate" ~ "Heart rate",
    X == "systolic_blood_pressure" ~ "Systolic blood pressure (mm/Hg)",
    X == "diastolic_blood_pressure" ~ "Diastolic blood pressure (mm/Hg)",
    X == "d_ldl_cholesterol" ~ "LDL cholesterol (mmol/L)",
    X == "d_hdl_cholesterol" ~ "HDL cholesterol (mmol/L)",
    X == "d_s_creatinin" ~ "Creatinine (µmol/L)",
    X == "d_bmi" ~ "Body mass index (kg/m^2)",
    TRUE ~ X
  ))

# Replace all NAs with ""
base[is.na(base)] <- ""

# Insert header rows
char <- c("Characteristics and prior diagnoses","","","","","","")
pres <- c("Presentation","","","","","","" )
during <- c("During hopitalization", "","","","","","" )
med <- c("Other medications", "","","","","","")
measurements <- c("Measurements", "","","","","","" )

base <- base %>% 
  insertRows(51, char) %>% 
  insertRows(70, pres) %>% 
  insertRows(92, during) %>%
  insertRows(119, med) %>% 
  insertRows(126, measurements)  
  

write.table(base, 
            "output/cr_tableone_main.txt", 
            sep="\t", 
            quote=FALSE, 
            row.names=FALSE)

rm(ipw, base, char, med, pres, during, measurements, weighted_data)



