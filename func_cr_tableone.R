
func_cr_tableone <- function(input_data, intervention, weighted=FALSE, txt_name) {
  
# Select columns needed
if (weighted == F) {
  if (ncol(input_data)>= 80) {
    vars <- names(input_data)[-1:-12]
  } else {
    vars <- names(input_data)[-1:-9]
  }
} else {
  vars <- names(input_data$variables)[-1:-8]
  # Remove the last variable name in the vector as this will be the weights
  vars <- vars[-length(vars)]
}

# Make sure age is in the third position  
vars <- vars[!grepl("age", vars)]
vars <- c(vars[1:2], "age", vars[3:length(vars)])


if (weighted == F) {
# Make 9's missing for categorical variables
vars_cat <- names(Filter(is.factor, input_data))
  vars[1:(length(vars)-7)]
func_missing <- function(x) {factor(x, exclude = 9)}
input_data <- input_data %>% mutate_at(vars_cat, func_missing)

# Label categorical variables where needed
input_data$intervention <- factor(input_data$intervention, levels=c(1,0),
                              labels=c("Beta blockers", "No beta blockers"))
input_data$centreid_ic<- factor(input_data$centreid_ic,levels=c("SE10011", "SE10013", "SE11001",
                                                                "SE11002", "SE11010", "SE12001", 
                                                                "SE13010", "SE21001", "SE21013",
                                                                "SE21014", "SE22010", "SE22011",
                                                                "SE23011", "SE25010", "SE28010",
                                                                "SE28011", "SE30001", "SE41001",
                                                                "SE41012", "SE42010", "SE42011",
                                                                "SE50001", "SE50010", "SE51011",
                                                                "SE51012", "SE52011", "SE52012",
                                                                "SE53011", "SE53013", "SE54010",
                                                                "SE55010", "SE56010", "SE56012",
                                                                "SE57010", "SE57011", "SE62010",
                                                                "SE63010", "SE64001", "SE65016"),
                                  labels=c("Stockholm St Göran", "Stockholm SÖS", "Stockholm KI Solna",
                                           "Stockholm KI Huddinge", "Stockholm Danderyd", "Uppsala",
                                           "Eskiltuna", "Linköping", "Norrköping Vrinnevi",
                                           "Motala", "Jönköping", "Eksjö",
                                           "Ljungby", "Kalmar", "Kristianstad",
                                           "Ängelholm", "Malmö", "Lund", 
                                           "Helsingborg", "Halmstad", "Varberg",
                                           "Göteberg Sahlgrenska", "Göteborg Östra", "Göteborg Mölndal",
                                           "Kungälv", "Borås", "Alingsås",
                                           "Lidköping", "Skövde", "Karlstad",
                                           "Örebro", "Västerås", "Köping",
                                           "Falun", "Mora", "Sundsvall",
                                           "Östersund", "Umeå", "Sunderbyn"
                                           ))
input_data$smoking_status <- factor(input_data$smoking_status, levels=c(0,1,2),
                                  labels=c("Never smoker", "Ex-smoker (>1 month)", "Smoker"))
input_data$other_serious_disease <- factor(input_data$other_serious_disease, levels=c(0,1,2,8),
                                    labels=c("No", "Cancer", "Dementia", "Other"))
input_data$ecg_rhythm <- factor(input_data$ecg_rhythm, levels=c(1,2,8),
                                           labels=c("Sinus", "Ability flicker/flutter", "Other"))
input_data$ecg_qrs_annotation <- factor(input_data$ecg_qrs_annotation, levels=c(1,2,3,4,5,8),
                                labels=c("Normal", "Pacemaker", "Left block branch", "Pathological Q-wave", "Right block branch", "Other"))
input_data$ecg_stt_changes <- factor(input_data$ecg_stt_changes, levels=c(1,2,3,4,8),
                                        labels=c("Normal", "ST raise", "ST reduction", "Pathological T-wave", "Other"))
input_data$fynd <- factor(input_data$fynd, levels=c(2,3,4,5,6,7,8),
                                      labels=c("1 vessel, not left main", 
                                               "2 vessels, not left main",
                                               "3 vessels not left main ",
                                               "Left main + 1 vessel",
                                               "Left main + 2 vessels",
                                               "Left main + 3 vessels",
                                               "Left main"))
input_data$stenosklass <- factor(input_data$stenosklass, levels=c(1,2,3,4,5,6,7),
                                      labels=c("A", "B1", "B2", "C", "B1 bifurcation", "B2 bifurcation", "C bifurcation"))

input_data$segment<- factor(input_data$segment, levels=c(3,4,5,6),
                                 labels=c("<70%", "70-89%", "90-99%","100%"))

}

# Create the table one  
if (weighted == F) {
  tableone <- CreateTableOne(vars = vars, 
                             strata = c(intervention), 
                             includeNA = FALSE, 
                             data = input_data)
} else {
tableone <- svyCreateTableOne(vars = vars, 
                              strata = c(intervention), 
                              includeNA = FALSE, 
                              data = input_data)
}
  
# Print
tableoneprint <- print(tableone, 
                       nonnormal = TRUE,
                       contDigits = 1, 
                       test = FALSE, 
                       missing = TRUE, 
                       quote = FALSE, 
                       varLabel = TRUE, 
                       dropEqual = TRUE, 
                       noSpace = TRUE, 
                       smd=TRUE)

# Save to output file
write.table(tableoneprint, here("output",paste0(txt_name, ".txt")), sep="\t", quote = FALSE, row.names=TRUE, col.names=NA)

rm(tableone, tableoneprint)

}

