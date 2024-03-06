
# Main #########################################################################

# ITT, composite ---------------------------------------------------------------
load("W:/C6_Berglund/data/procdata/reduce-emulation/cr_data_main.Rda")
tic()
results_itt_composite <-  func_analyze(input_data = cr_data_main,
                             analysis = "ipw",
                             intervention = itt,
                             outcome = composite_time,
                             censor = c(endfup_time, 60),
                             perprotocol = F,
                             confounders = an_confounders$full,
                             missing = "imputemedian",
                             bootstraps = 500,
                             time_results = c(12,24,36,48,60)
                             )
toc()

write.table(results_itt_composite$hr_table,
            "output/table_itt_composite_hr_5y.txt",
            sep="\t", quote=FALSE, row.names=FALSE)

write.table(results_itt_composite$risks_table,
            "output/table_itt_composite_risks.txt",
            sep="\t", quote=FALSE, row.names=FALSE)

ggsave("output/plot_itt_composite_standardized.png",
       plot = results_itt_composite$plot,
       width = 8, height = 6)

ggsave("output/plot_itt_composite_kmadj.png",
       plot = print(results_itt_composite$km_plot_ipw$plot),
       width = 6, height = 4.7)

rm(results_itt_composite) 
gc()


# ITT, death -------------------------------------------------------------------
load("W:/C6_Berglund/data/procdata/reduce-emulation/cr_data_main.Rda")
results_itt_death <-  func_analyze(input_data = cr_data_main,
                                   analysis = "ipw",
                                   intervention = itt,
                                   outcome = death_time,
                                   censor = c(endfup_time, 60),
                                   perprotocol = F,
                                   confounders = an_confounders$full,
                                   missing = "imputemedian",
                                   bootstraps = 500,
                                   time_results = c(12,24,36,48,60)
                                   )

write.table(results_itt_death$hr_table,
            "output/table_itt_death_hr_5y.txt",
            sep="\t", quote=FALSE, row.names=FALSE)

write.table(results_itt_death$risks_table,
            "output/table_itt_death_risks.txt",
            sep="\t", quote=FALSE, row.names=FALSE)

ggsave("output/plot_itt_death_standardized.png",
       plot = results_itt_death$plot,
       width = 8, height = 6)

ggsave("output/plot_itt_death_kmadj.png",
       plot = print(results_itt_death$km_plot_ipw$plot),
       width = 6, height = 4.7)

rm(results_itt_death) 
gc()


# ITT, MI ----------------------------------------------------------------------
load("W:/C6_Berglund/data/procdata/reduce-emulation/cr_data_main.Rda")
results_itt_mi <-  func_analyze(input_data = cr_data_main,
                                analysis = "ipw",
                                intervention = itt,
                                outcome = mi_time,
                                censor = c(endfup_time, 60),
                                perprotocol = F,
                                confounders = an_confounders$full,
                                missing = "imputemedian",
                                bootstraps = 500,
                                time_results = c(12,24,36,48,60)
                                )

write.table(results_itt_mi$hr_table,
            "output/table_itt_mi_hr_5y.txt",
            sep="\t", quote=FALSE, row.names=FALSE)

write.table(results_itt_mi$risks_table,
            "output/table_itt_mi_risks.txt",
            sep="\t", quote=FALSE, row.names=FALSE)

ggsave("output/plot_itt_mi_standardized.png",
       plot = results_itt_mi$plot,
       width = 8, height = 6)

ggsave("output/plot_itt_mi_kmadj.png",
       plot = print(results_itt_mi$km_plot_ipw$plot),
       width = 6, height = 4.7)

rm(results_itt_mi) 
gc()


# PP, composite ---------------------------------------------------------------
load("W:/C6_Berglund/data/procdata/reduce-emulation/cr_data_main.Rda")
tic()
results_pp_composite <-  func_analyze(input_data = cr_data_main,
                             analysis = "ipw",
                             intervention = itt,
                             outcome = composite_time,
                             censor = c(endfup_time, 60),
                             perprotocol = T,
                             confounders = an_confounders$full_tu,
                             missing = "imputemedian",
                             bootstraps = 500,
                             time_results = c(12,24,36,48,60)
)
toc()

write.table(results_pp_composite$hr_table,
            "output/table_pp_composite_hr_5y.txt",
            sep="\t", quote=FALSE, row.names=FALSE)

write.table(results_pp_composite$risks_table,
            "output/table_pp_composite_risks.txt",
            sep="\t", quote=FALSE, row.names=FALSE)

ggsave("output/plot_pp_composite_standardized.png",
       plot = results_pp_composite$plot,
       width = 8, height = 6)

rm(results_pp_composite) 
gc()

# PP, death -------------------------------------------------------------------
load("W:/C6_Berglund/data/procdata/reduce-emulation/cr_data_main.Rda")
results_pp_death <-  func_analyze(input_data = cr_data_main,
                                   analysis = "ipw",
                                   intervention = itt,
                                   outcome = death_time,
                                   censor = c(endfup_time, 60),
                                   perprotocol = T,
                                   confounders = an_confounders$full_tu,
                                   missing = "imputemedian",
                                   bootstraps = 500,
                                   time_results = c(12,24,36,48,60)
)

write.table(results_pp_death$hr_table,
            "output/table_pp_death_hr_5y.txt",
            sep="\t", quote=FALSE, row.names=FALSE)

write.table(results_pp_death$risks_table,
            "output/table_pp_death_risks.txt",
            sep="\t", quote=FALSE, row.names=FALSE)

ggsave("output/plot_pp_death_standardized.png",
       plot = results_pp_death$plot,
       width = 8, height = 6)

rm(results_pp_death) 
gc()


# PP, MI ----------------------------------------------------------------------
load("W:/C6_Berglund/data/procdata/reduce-emulation/cr_data_main.Rda")
results_pp_mi <-  func_analyze(input_data = cr_data_main,
                                analysis = "ipw",
                                intervention = itt,
                                outcome = mi_time,
                                censor = c(endfup_time, 60),
                                perprotocol = T,
                                confounders = an_confounders$full_tu,
                                missing = "imputemedian",
                                bootstraps = 500,
                                time_results = c(12,24,36,48,60)
)

write.table(results_pp_mi$hr_table,
            "output/table_pp_mi_hr_5y.txt",
            sep="\t", quote=FALSE, row.names=FALSE)

write.table(results_pp_mi$risks_table,
            "output/table_pp_mi_risks.txt",
            sep="\t", quote=FALSE, row.names=FALSE)

ggsave("output/plot_pp_mi_standardized.png",
       plot = results_pp_mi$plot,
       width = 8, height = 6)

rm(results_pp_mi) 
gc()


# HR AT VARIOUS TIME POINTS ####################################################

# ITT, composite, 3y -----------------------------------------------------------
load("W:/C6_Berglund/data/procdata/reduce-emulation/cr_data_main.Rda")

results_itt_composite_3y <-  func_analyze(input_data = cr_data_main,
                                       analysis = "ipw",
                                       intervention = itt,
                                       outcome = composite_time,
                                       censor = c(endfup_time, 36),
                                       perprotocol = F,
                                       confounders = an_confounders$full,
                                       missing = "imputemedian",
                                       bootstraps = 500,
                                       time_results = c(36)
)


write.table(results_itt_composite_3y$hr_table,
            "output/table_itt_composite_hr_3y.txt",
            sep="\t", quote=FALSE, row.names=FALSE)

rm(results_itt_composite_3y)
gc()

# ITT, composite, 4y -----------------------------------------------------------
load("W:/C6_Berglund/data/procdata/reduce-emulation/cr_data_main.Rda")

results_itt_composite_4y <-  func_analyze(input_data = cr_data_main,
                                          analysis = "ipw",
                                          intervention = itt,
                                          outcome = composite_time,
                                          censor = c(endfup_time, 48),
                                          perprotocol = F,
                                          confounders = an_confounders$full,
                                          missing = "imputemedian",
                                          bootstraps = 500,
                                          time_results = c(48)
)


write.table(results_itt_composite_4y$hr_table,
            "output/table_itt_composite_hr_4y.txt",
            sep="\t", quote=FALSE, row.names=FALSE)

rm(results_itt_composite_4y)
gc()


# PP, composite, 3y -------------------------------------------------------------
load("W:/C6_Berglund/data/procdata/reduce-emulation/cr_data_main.Rda")
results_pp_composite_3y <-  func_analyze(input_data = cr_data_main,
                                      analysis = "ipw",
                                      intervention = itt,
                                      outcome = composite_time,
                                      censor = c(endfup_time, 36),
                                      perprotocol = T,
                                      confounders = an_confounders$full_tu,
                                      missing = "imputemedian",
                                      bootstraps = 500,
                                      time_results = c(36)
)

write.table(results_pp_composite_3y$hr_table,
            "output/table_pp_composite_hr_3y.txt",
            sep="\t", quote=FALSE, row.names=FALSE)

rm(results_pp_composite_3y) 
gc()


# PP, composite, 4y -------------------------------------------------------------
load("W:/C6_Berglund/data/procdata/reduce-emulation/cr_data_main.Rda")
results_pp_composite_4y <-  func_analyze(input_data = cr_data_main,
                                         analysis = "ipw",
                                         intervention = itt,
                                         outcome = composite_time,
                                         censor = c(endfup_time, 48),
                                         perprotocol = T,
                                         confounders = an_confounders$full_tu,
                                         missing = "imputemedian",
                                         bootstraps = 500,
                                         time_results = c(48)
)

write.table(results_pp_composite_4y$hr_table,
            "output/table_pp_composite_hr_4y.txt",
            sep="\t", quote=FALSE, row.names=FALSE)

rm(results_pp_composite_4y) 
gc()





SENSITIVITIES ################################################################

# No BB exclusion, ITT, composite ----------------------------------------------
load("W:/C6_Berglund/data/procdata/reduce-emulation/cr_data_nobbexcl.Rda")
sens_nobbexcl_results_itt_composite <-  func_analyze(input_data = cr_data_main,
                                       analysis = "ipw",
                                       intervention = itt,
                                       outcome = composite_time,
                                       censor = c(endfup_time, 60),
                                       perprotocol = F,
                                       confounders = an_confounders$full_sens_nobbexcl,
                                       missing = "imputemedian",
                                       bootstraps = 200,
                                       time_results = c(12,24,36,48,60)
)


write.table(sens_nobbexcl_results_itt_composite$hr_table,
            "output/sens_nobbexcl_table_itt_composite_hr.txt",
            sep="\t", quote=FALSE, row.names=FALSE)

write.table(sens_nobbexcl_results_itt_composite$risks_table,
            "output/sens_nobbexcl_table_itt_composite_risks.txt",
            sep="\t", quote=FALSE, row.names=FALSE)

ggsave("output/sens_nobbexcl_plot_itt_composite_standardized.png",
       plot = sens_nobbexcl_results_itt_composite$plot,
       width = 8, height = 6)

ggsave("output/sens_nobbexcl_plot_itt_composite_kmadj.png",
       plot = print(sens_nobbexcl_results_itt_composite$km_plot_ipw$plot),
       width = 6, height = 4.7)

rm(sens_nobbexcl_results_itt_composite)
gc()


# BB exclusion - SWEDEHEART only, ITT, composite ----------------------------------------------
load("W:/C6_Berglund/data/procdata/reduce-emulation/cr_data_bbexclswed.Rda")
sens_bbexclswed_results_itt_composite <-  func_analyze(input_data = cr_data_main,
                                                     analysis = "ipw",
                                                     intervention = itt,
                                                     outcome = composite_time,
                                                     censor = c(endfup_time, 60),
                                                     perprotocol = F,
                                                     confounders = an_confounders$full_sens_bbexclswed,
                                                     missing = "imputemedian",
                                                     bootstraps = 200,
                                                     time_results = c(12,24,36,48,60)
)


write.table(sens_bbexclswed_results_itt_composite$hr_table,
            "output/sens_bbexclswed_table_itt_composite_hr.txt",
            sep="\t", quote=FALSE, row.names=FALSE)

write.table(sens_bbexclswed_results_itt_composite$risks_table,
            "output/sens_bbexclswed_table_itt_composite_risks.txt",
            sep="\t", quote=FALSE, row.names=FALSE)

ggsave("output/sens_bbexclswed_plot_itt_composite_standardized.png",
       plot = sens_bbexclswed_results_itt_composite$plot,
       width = 8, height = 6)

ggsave("output/sens_bbexclswed_plot_itt_composite_kmadj.png",
       plot = print(sens_bbexclswed_results_itt_composite$km_plot_ipw$plot),
       width = 6, height = 4.7)

rm(sens_bbexclswed_results_itt_composite)
gc()


# Continuous varaibles categoized, with NA as missing level, ITT ---------------
load("W:/C6_Berglund/data/procdata/reduce-emulation/cr_data_main.Rda")

sens_continuoustocat_results_itt_composite <-  func_analyze(input_data = cr_data_main,
                                                         analysis = "ipw",
                                                         intervention = itt,
                                                         outcome = composite_time,
                                                         censor = c(endfup_time, 60),
                                                         perprotocol = F,
                                                         confounders = an_confounders$full_sens_continuoustocat,
                                                         missing = "continuoustocat",
                                                         bootstraps = 200,
                                                         time_results = c(12,24,36,48,60)
)

write.table(sens_continuoustocat_results_itt_composite$hr_table,
            "output/sens_continuoustocat_table_itt_composite_hr.txt",
            sep="\t", quote=FALSE, row.names=FALSE)

write.table(sens_continuoustocat_results_itt_composite$risks_table,
            "output/sens_continuoustocat_table_itt_composite_risks.txt",
            sep="\t", quote=FALSE, row.names=FALSE)

ggsave("output/sens_continuoustocat_plot_itt_composite_standardized.png",
       plot = sens_continuoustocat_results_itt_composite$plot,
       width = 8, height = 6)

ggsave("output/sens_continuoustocat_plot_itt_composite_kmadj.png",
       plot = print(sens_continuoustocat_results_itt_composite$km_plot_ipw$plot),
       width = 6, height = 4.7)

rm(sens_continuoustocat_results_itt_composite)
gc()


# 90 day grace period, PP -----------------------------------------------------
load("W:/C6_Berglund/data/procdata/reduce-emulation/cr_data_grace_90d.Rda")
sens_grace90d_results_pp_composite <-  func_analyze(input_data = cr_data_main,
                                                         analysis = "ipw",
                                                         intervention = itt,
                                                         outcome = composite_time,
                                                         censor = c(endfup_time, 60),
                                                         perprotocol = T,
                                                         confounders = an_confounders$full_tu,
                                                         missing = "imputemedian",
                                                         bootstraps = 200,
                                                         time_results = c(12,24,36,48,60)
)

write.table(sens_grace90d_results_pp_composite$hr_table,
            "output/sens_grace90d_results_table_pp_composite_hr.txt",
            sep="\t", quote=FALSE, row.names=FALSE)

write.table(sens_grace90d_results_pp_composite$risks_table,
            "output/sens_grace90d_results_table_pp_composite_risks.txt",
            sep="\t", quote=FALSE, row.names=FALSE)

ggsave("output/sens_grace90d_plot_pp_composite_standardized.png",
       plot = sens_grace90d_results_pp_composite$plot,
       width = 8, height = 6)

rm(sens_grace90d_results_pp_composite) 
gc()



# Standardize, ITT -------------------------------------------------------------
load("W:/C6_Berglund/data/procdata/reduce-emulation/cr_data_main.Rda")
tic()
sens_standardize_results_itt_composite <-  func_analyze(input_data = cr_data_main,
                                                        analysis = "standardize",
                                                        intervention = itt,
                                                        outcome = composite_time,
                                                        censor = c(endfup_time, 60),
                                                        perprotocol = F,
                                                        confounders = an_confounders$full,
                                                        missing = "imputemedian",
                                                        bootstraps = 200,
                                                        time_results = c(12,24,36,48,60)
)
toc()

write.table(sens_standardize_results_itt_composite$hr_table,
            "output/sens_standardize_table_itt_composite_hr.txt",
            sep="\t", quote=FALSE, row.names=FALSE)

write.table(sens_standardize_results_itt_composite$risks_table,
            "output/sens_standardize_table_itt_composite_risks.txt",
            sep="\t", quote=FALSE, row.names=FALSE)

ggsave("output/sens_standardize_plot_itt_composite_standardized.png",
       plot = sens_standardize_results_itt_composite$plot,
       width = 8, height = 6)


# Complete case, ITT -----------------------------------------------------------
load("W:/C6_Berglund/data/procdata/reduce-emulation/cr_data_main.Rda")
# need to remove d_pci from adjustment set as can't fit bootstraps (only 2 people without when data are complete case)
sens_confounders <- an_confounders$full[!grepl("d_pci", an_confounders$full)]
sens_completecase_results_itt_composite <-  func_analyze(input_data = cr_data_main,
                                                         analysis = "ipw",
                                                         intervention = itt,
                                                         outcome = composite_time,
                                                         censor = c(endfup_time, 60),
                                                         perprotocol = F,
                                                         confounders = sens_confounders,
                                                         missing = "completecase",
                                                         bootstraps = 200,
                                                         time_results = c(12,24,36,48,60)
)

write.table(sens_completecase_results_itt_composite$hr_table,
            "output/sens_completecase_table_itt_composite_hr.txt",
            sep="\t", quote=FALSE, row.names=FALSE)

write.table(sens_completecase_results_itt_composite$risks_table,
            "output/sens_completecase_table_itt_composite_risks.txt",
            sep="\t", quote=FALSE, row.names=FALSE)

ggsave("output/sens_completecase_plot_itt_composite_standardized.png",
       plot = sens_completecase_results_itt_composite$plot,
       width = 8, height = 6)

ggsave("output/sens_completecase_plot_itt_composite_kmadj.png",
       plot = print(sens_completecase_results_itt_composite$km_plot_ipw$plot),
       width = 6, height = 4.7)

rm(sens_completecase_results_itt_composite) 
gc()

# Only in previous BB users, ITT, composite ----------------------------------------------
load("W:/C6_Berglund/data/procdata/reduce-emulation/cr_data_nobbexcl.Rda")
cr_data_main <- cr_data_main %>% filter(beta_blockers_reg == 1 | bb_pdr_bef == 1)
sens_prevbb_results_itt_composite <-  func_analyze(input_data = cr_data_main,
                                                     analysis = "ipw",
                                                     intervention = itt,
                                                     outcome = composite_time,
                                                     censor = c(endfup_time, 60),
                                                     perprotocol = F,
                                                     confounders = an_confounders$full,
                                                     missing = "imputemedian",
                                                     bootstraps = 2,
                                                     time_results = c(12,24,36,48,60)
)



# ITT, composite, cloning, days ---------------------------------------------------------------
load("W:/C6_Berglund/data/procdata/reduce-emulation/cr_data_daysens.Rda")
# Update function - Create analyzable baseline data frames (restricted to confounders of interest)
source(here("scripts", "func_cr_analyze_daysens.R"))
# Update function - IPW modelling
source(here("scripts", "func_model_ipw_daysens.R"))
tic()
results_itt_composite_daysens <-  func_analyze(input_data = cr_data_main,
                                       analysis = "ipw",
                                       intervention = itt,
                                       outcome = composite_time,
                                       censor = c(clone_censor_time, endfup_time, 1800),
                                       perprotocol = F,
                                       confounders = an_confounders$full,
                                       missing = "imputemedian",
                                       bootstraps = 1,
                                       time_results = c(360,720,1080,1440,1800)
)
