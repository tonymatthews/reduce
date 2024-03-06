
an_confounders <- list()

# Age/sex ----------------------------------------------------------------------

an_confounders$agesex <- c(
  "ns(age,5)",
  "d_gender"
)

# Full -------------------------------------------------------------------------

an_confounders$full <- c(
  "centreid_ic",
  "eligible_year",
# Characteristics and prior diagnoses at baseline
  "ns(age,5)",
  "d_gender",
  "smoking_status",
  "hypertension",
  "diabetes",
  "previous_mi",
  "previous_stroke",
  "previous_pci",
  "prior_cardiac_surgery",
  "renal_pat_baseline",
  "other_serious_disease",
# Presentation
  "infarcttype",
  "cpr_before_hospital",
  "thromb_before_hospital",
  "cardiac_shock",
  "ecg_rhythm",
  "ecg_qrs_annotation",
  "ecg_stt_changes",
# In hospital
  "d_pci",
  "fynd",
  "stenosklass",
  "segment",
  "d_beta_blockers_treat",
  "d_diuretics_treat",
  "d_inotropes",
  "d_nitrates_treat",
# Readings
  "ns(heart_rate,5)",
  "ns(systolic_blood_pressure,5)",
  "ns(diastolic_blood_pressure,5)",
  "ns(d_ldl_cholesterol,5)",
  "ns(d_hdl_cholesterol,5)",
  "ns(d_s_creatinin,5)",
  "ns(d_bmi,5)",
  # Previous medications at admission or in time zero
  "arb_pdr_baseline",
  "ace_pdr_baseline",
  "ccb_pdr_baseline",
  "diuretics_pdr_baseline",
  "nitrates_pdr_baseline",
  "diabetes_pdr_baseline"
)

# Full with time-varying vars --------------------------------------------------

an_confounders$full_tu <- c(
  "centreid_ic",
  "eligible_year",
  # Characteristics and prior diagnoses at admission
  "ns(age,5)",
  "d_gender",
  "smoking_status",
  "hypertension",
  "diabetes",
  "previous_mi",
  "previous_stroke",
  "previous_pci",
  "prior_cardiac_surgery",
  "renal_pat_baseline",
  "other_serious_disease",
  # Presentation
  "infarcttype",
  "cpr_before_hospital",
  "thromb_before_hospital",
  "cardiac_shock",
  "ecg_rhythm",
  "ecg_qrs_annotation",
  "ecg_stt_changes",
  # In hospital
  "d_pci",
  "fynd",
  "stenosklass",
  "segment",
  "d_beta_blockers_treat",
  "d_diuretics_treat",
  "d_inotropes",
  "d_nitrates_treat",
  # Readings
  "ns(heart_rate,5)",
  "ns(systolic_blood_pressure,5)",
  "ns(diastolic_blood_pressure,5)",
  "ns(d_ldl_cholesterol,5)",
  "ns(d_hdl_cholesterol,5)",
  "ns(d_s_creatinin,5)",
  "ns(d_bmi,5)",
  # Previous medications at admission or in time zero
  "arb_pdr_baseline", 
  "ace_pdr_baseline",
  "ccb_pdr_baseline",
  "diuretics_pdr_baseline",
  "nitrates_pdr_baseline",
  "diabetes_pdr_baseline",
  # time updated
  "renal_pat_tu",
  "arb_pdr_tu",
  "ace_pdr_tu",
  "ccb_pdr_tu",
  "diuretics_pdr_tu",
  "nitrates_pdr_tu",
  "diabetes_pdr_tu"
)

# No BB exclusion sensitivity, additionally adjust for bb in pdr and swedheart -----

an_confounders$full_sens_nobbexcl <- c(
  "centreid_ic",
  "eligible_year",
  # Characteristics and prior diagnoses at baseline
  "ns(age,5)",
  "d_gender",
  "smoking_status",
  "hypertension",
  "diabetes",
  "previous_mi",
  "previous_stroke",
  "previous_pci",
  "prior_cardiac_surgery",
  "renal_pat_baseline",
  "other_serious_disease",
  # Presentation
  "infarcttype",
  "cpr_before_hospital",
  "thromb_before_hospital",
  "cardiac_shock",
  "ecg_rhythm",
  "ecg_qrs_annotation",
  "ecg_stt_changes",
  # In hospital
  "d_pci",
  "fynd",
  "stenosklass",
  "segment",
  "d_beta_blockers_treat",
  "d_diuretics_treat",
  "d_inotropes",
  "d_nitrates_treat",
  # Readings
  "ns(heart_rate,5)",
  "ns(systolic_blood_pressure,5)",
  "ns(diastolic_blood_pressure,5)",
  "ns(d_ldl_cholesterol,5)",
  "ns(d_hdl_cholesterol,5)",
  "ns(d_s_creatinin,5)",
  "ns(d_bmi,5)",
  # Previous medications at admission or in time zero
  "arb_pdr_baseline",
  "ace_pdr_baseline",
  "ccb_pdr_baseline",
  "diuretics_pdr_baseline",
  "nitrates_pdr_baseline",
  "diabetes_pdr_baseline",
  # BB before  in PDR and swed
  "bb_pdr_bef",
  "beta_blockers_reg"
)

# BB exclusion sensitivity, only exclude if recorded in swedheart, adjust for pdr -----

an_confounders$full_sens_bbexclswed <- c(
  "centreid_ic",
  "eligible_year",
  # Characteristics and prior diagnoses at baseline
  "ns(age,5)",
  "d_gender",
  "smoking_status",
  "hypertension",
  "diabetes",
  "previous_mi",
  "previous_stroke",
  "previous_pci",
  "prior_cardiac_surgery",
  "renal_pat_baseline",
  "other_serious_disease",
  # Presentation
  "infarcttype",
  "cpr_before_hospital",
  "thromb_before_hospital",
  "cardiac_shock",
  "ecg_rhythm",
  "ecg_qrs_annotation",
  "ecg_stt_changes",
  # In hospital
  "d_pci",
  "fynd",
  "stenosklass",
  "segment",
  "d_beta_blockers_treat",
  "d_diuretics_treat",
  "d_inotropes",
  "d_nitrates_treat",
  # Readings
  "ns(heart_rate,5)",
  "ns(systolic_blood_pressure,5)",
  "ns(diastolic_blood_pressure,5)",
  "ns(d_ldl_cholesterol,5)",
  "ns(d_hdl_cholesterol,5)",
  "ns(d_s_creatinin,5)",
  "ns(d_bmi,5)",
  # Previous medications at admission or in time zero
  "arb_pdr_baseline",
  "ace_pdr_baseline",
  "ccb_pdr_baseline",
  "diuretics_pdr_baseline",
  "nitrates_pdr_baseline",
  "diabetes_pdr_baseline",
  # BB before  in PDR 
  "bb_pdr_bef"
)

# Categorize continuous variables sensitivity ----------------------------------

an_confounders$full_sens_continuoustocat <- c(
  "centreid_ic",
  "eligible_year",
  # Characteristics and prior diagnoses at baseline
  "age",
  "d_gender",
  "smoking_status",
  "hypertension",
  "diabetes",
  "previous_mi",
  "previous_stroke",
  "previous_pci",
  "prior_cardiac_surgery",
  "renal_pat_baseline",
  "other_serious_disease",
  # Presentation
  "infarcttype",
  "cpr_before_hospital",
  "thromb_before_hospital",
  "cardiac_shock",
  "ecg_rhythm",
  "ecg_qrs_annotation",
  "ecg_stt_changes",
  # In hospital
  "d_pci",
  "fynd",
  "stenosklass",
  "segment",
  "d_beta_blockers_treat",
  "d_diuretics_treat",
  "d_inotropes",
  "d_nitrates_treat",
  # Readings
  "heart_rate",
  "systolic_blood_pressure",
  "diastolic_blood_pressure",
  "d_ldl_cholesterol",
  "d_hdl_cholesterol",
  "d_s_creatinin",
  "d_bmi",
  # Previous medications at admission or in time zero
  "arb_pdr_baseline",
  "ace_pdr_baseline",
  "ccb_pdr_baseline",
  "diuretics_pdr_baseline",
  "nitrates_pdr_baseline",
  "diabetes_pdr_baseline"
)


