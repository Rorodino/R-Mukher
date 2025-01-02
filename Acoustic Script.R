library(readxl)

MBdata <- read_excel("/Users/rohitsatish48gmail.com/Downloads/02-JHU/Mukherjee Lab/Meningioma/NCDBMeningioma 1.xlsx")

#Preprocessing 
MBdata$`Survival months`[MBdata$`Survival months` == "Unknown"] <- NA
MBdata <- subset(MBdata, `Race recode (W, B, AI, API)` != "Unknown")
MBdata <- subset(MBdata, `Site recode ICD-O-3/WHO 2008` %in% c('Cranial Nerves Other Nervous System', 'Brain'))
MBdata$SurgeryREC <- ifelse(MBdata$`Reason no cancer-directed surgery` %in% c('Surgery performed', 'Recommended but not performed, unknown reason', 'Recommended but not performed, patient refused', 'Recommended, unknown if performed'), 1, 
                            ifelse(MBdata$`Reason no cancer-directed surgery` %in% c('Not recommended'), 0, NA ))
MBdata <- na.omit(MBdata)

#Cohort Classification
MBdata$IncomeIndex <- ifelse(MBdata$`Median household income inflation adj to 2019` == '$75,000+', 1, 
                             ifelse(MBdata$`Median household income inflation adj to 2019` != '$75,000+', 0, NA ))
MBdata$URgroup <- ifelse(MBdata$`Rural-Urban Continuum Code` %in% c('Counties in metropolitan areas ge 1 million pop', 'Counties in metropolitan areas of 250,000 to 1 million pop', 'Counties in metropolitan areas of lt 250 thousand pop'), 1,
                         ifelse(MBdata$`Rural-Urban Continuum Code` %in% c('Unknown/missing/no match/Not 1990-2018', 'Nonmetropolitan counties adjacent to a metropolitan area', 'Nonmetropolitan counties not adjacent to a metropolitan area', 'Unknown/missing/no match (Alaska or Hawaii - Entire State)'), 0, NA))
MBdata$Perf <- ifelse(MBdata$`Reason no cancer-directed surgery` == 'Surgery performed', 1, 
                      ifelse(MBdata$`Reason no cancer-directed surgery` != 'Surgery performed', 0, NA ))
Sur_Rec <- na.omit(MBdata[MBdata$SurgeryREC == 1, ])
Sur_NRec<- na.omit(MBdata[MBdata$SurgeryREC == 0, ])

#Age
Sur_Rec_Mean_Age <- mean(Sur_Rec$`Age Recoded`, na.rm = TRUE)
Sur_NRec_Mean_Age <- mean(Sur_NRec$`Age Recoded`, na.rm = TRUE)
Sur_Rec_Mean_Age_SD <- sd(Sur_Rec$`Age Recoded`, na.rm = TRUE)
Sur_NRec_Mean_Age_SD <- sd(Sur_NRec$`Age Recoded`, na.rm = TRUE)

#Survival Time
library(survival)
MBdata$Last_Contact <- ifelse(MBdata$`Vital status recode (study cutoff used)` == 'Dead', 1, 
                              ifelse(MBdata$`Vital status recode (study cutoff used)` == 'Alive', 0, NA ))
MBdata$`Survival months` <- as.numeric(MBdata$`Survival months`)
surv_object <- Surv(time = MBdata$`Survival months`, event = MBdata$Last_Contact)
fit1 <- survfit(surv_object ~ MBdata$SurgeryREC, data = MBdata)
library(survminer)
ggsurvplot(fit1, data = MBdata, pval = TRUE, legend.labs= c('Surgery Not Recommended', 'SurgeryRecommended'), xlab = 'Survival Time (Month)')
median_survival_time <- surv_median(fit1)

#Sex
fm_Rec <- sum(Sur_Rec$Sex == 'Female', na.rm = TRUE)
ma_Rec <- sum(Sur_Rec$Sex == 'Male', na.rm = TRUE)
fm_NRec <- sum(Sur_NRec$Sex == 'Female', na.rm = TRUE)
ma_NRec <- sum(Sur_NRec$Sex == 'Male', na.rm = TRUE)

#Race
whiteR_Rec <- sum(Sur_Rec$`Race recode (W, B, AI, API)` == 'White', na.rm = TRUE)
blackR_Rec <- sum(Sur_Rec$`Race recode (W, B, AI, API)` == 'Black', na.rm = TRUE)
APIR_Rec <- sum(Sur_Rec$`Race recode (W, B, AI, API)` == 'Asian or Pacific Islander', na.rm = TRUE)
AIR_Rec <- sum(Sur_Rec$`Race recode (W, B, AI, API)` == 'American Indian/Alaska Native', na.rm = TRUE)
whiteR_NRec <- sum(Sur_NRec$`Race recode (W, B, AI, API)` == 'White', na.rm = TRUE)
blackR_NRec <- sum(Sur_NRec$`Race recode (W, B, AI, API)` == 'Black', na.rm = TRUE)
APIR_NRec <- sum(Sur_NRec$`Race recode (W, B, AI, API)` == 'Asian or Pacific Islander', na.rm = TRUE)
AIR_NRec <- sum(Sur_NRec$`Race recode (W, B, AI, API)` == 'American Indian/Alaska Native', na.rm = TRUE)

#Hispanic
His_Rec <- sum(Sur_Rec$`Origin recode NHIA (Hispanic, Non-Hisp)` == 'Spanish-Hispanic-Latino')
His_not_Rec <- sum(Sur_Rec$`Origin recode NHIA (Hispanic, Non-Hisp)` == 'Non-Spanish-Hispanic-Latino')
His_NRec <- sum(Sur_NRec$`Origin recode NHIA (Hispanic, Non-Hisp)` == 'Spanish-Hispanic-Latino')
His_not_NRec <- sum(Sur_NRec$`Origin recode NHIA (Hispanic, Non-Hisp)` == 'Non-Spanish-Hispanic-Latino')

#Median Household Income
MHIup_Rec <- sum(Sur_Rec$`Median household income inflation adj to 2019` == '$75,000+')
MHIdown_Rec <- sum(Sur_Rec$`Median household income inflation adj to 2019` != '$75,000+')
MHIup_NRec <- sum(Sur_NRec$`Median household income inflation adj to 2019` == '$75,000+')
MHIdown_NRec <- sum(Sur_NRec$`Median household income inflation adj to 2019` != '$75,000+')

#RuralUrban Index
UR_Rec <- sum(Sur_Rec$`Rural-Urban Continuum Code` %in% c('Counties in metropolitan areas ge 1 million pop', 'Counties in metropolitan areas of 250,000 to 1 million pop', 'Counties in metropolitan areas of lt 250 thousand pop') )
RU_Rec <- sum(Sur_Rec$`Rural-Urban Continuum Code` %in% c('Unknown/missing/no match/Not 1990-2018', 'Nonmetropolitan counties adjacent to a metropolitan area', 'Nonmetropolitan counties not adjacent to a metropolitan area', 'Unknown/missing/no match (Alaska or Hawaii - Entire State)'))
UR_NRec <- sum(Sur_NRec$`Rural-Urban Continuum Code` %in% c('Counties in metropolitan areas ge 1 million pop', 'Counties in metropolitan areas of 250,000 to 1 million pop', 'Counties in metropolitan areas of lt 250 thousand pop') )
RU_NRec <- sum(Sur_NRec$`Rural-Urban Continuum Code` %in% c('Unknown/missing/no match/Not 1990-2018', 'Nonmetropolitan counties adjacent to a metropolitan area', 'Nonmetropolitan counties not adjacent to a metropolitan area', 'Unknown/missing/no match (Alaska or Hawaii - Entire State)'))

#Number of Tumors
NoTuAve_Rec <- mean(Sur_Rec$`Total number of in situ/malignant tumors for patient`)
NoTuSD_Rec <- sd(Sur_Rec$`Total number of in situ/malignant tumors for patient`)
NoTuAve_NRec <- mean(Sur_NRec$`Total number of in situ/malignant tumors for patient`)
NoTuSD_NRec <- sd(Sur_NRec$`Total number of in situ/malignant tumors for patient`)

#Surgery Performance 
Performed_Rec <- sum(Sur_Rec$Perf == 1)
Performed_not_Rec <-sum(Sur_Rec$Perf == 0)
Performed_NRec <- sum(Sur_NRec$Perf == 1)
Performed_not_NRec <-sum(Sur_NRec$Perf == 0)

#P Values Cohort
#Wilcoxon Age
Age_result <- wilcox.test( `Age Recoded` ~ SurgeryREC, data = MBdata)

#Chisquare Sex
Sex_result <- chisq.test(table(MBdata$Sex, MBdata$SurgeryREC))

#Chisquare RACE
Race_result <- chisq.test(table(MBdata$`Race recode (W, B, AI, API)`, MBdata$SurgeryREC))

#Chisquare His
Spanish_result <- chisq.test(table(MBdata$`Origin recode NHIA (Hispanic, Non-Hisp)`, MBdata$SurgeryREC))

#Chisquare Household Income
Income_result <- chisq.test(table(MBdata$IncomeIndex, MBdata$SurgeryREC))

#Chisquare Urban Rural 
UR_result <- chisq.test(table(MBdata$URgroup, MBdata$SurgeryREC))

#Number of Tumors
NoTu_Result <- wilcox.test( MBdata$`Total number of in situ/malignant tumors for patient` ~ MBdata$SurgeryREC)

#Surgery Performance 
Performance_result <- chisq.test(table(MBdata$Perf, MBdata$SurgeryREC))



#Odd Ratio Calculation
#Univariable
#Age
Age_model <- glm(MBdata$SurgeryREC ~ MBdata$`Age Recoded`, data = MBdata, family = binomial)
summary_Age_model <- summary(Age_model)
Age_coefficients <- summary_Age_model$coefficients
Age_odds_ratios <- exp(summary(Age_model)$coefficients[, "Estimate"])
Age_conf_int <- exp(confint(Age_model))  # 95% confidence intervals for odds ratios
Age_p_values <- summary(Age_model)$coefficients[, "Pr(>|z|)"]
Age_Odd_results <- data.frame(
  Estimate = Age_coefficients[, "Estimate"],
  OR = Age_odds_ratios,
  `2.5 %` = Age_conf_int[, 1],
  `97.5 %` = Age_conf_int[, 2],
  `P-value` = Age_p_values
)
print(Age_Odd_results)

#Sex
Sex_model <- glm(MBdata$SurgeryREC ~ MBdata$Sex, data = MBdata, family = binomial)
summary_Sex_model <- summary(Sex_model)
Sex_coefficients <- summary_Sex_model$coefficients
Sex_odds_ratios <- exp(summary(Sex_model)$coefficients[, "Estimate"])
Sex_conf_int <- exp(confint(Sex_model))  # 95% confidence intervals for odds ratios
Sex_p_values <- summary(Sex_model)$coefficients[, "Pr(>|z|)"]
Sex_Odd_results <- data.frame(
  Estimate = Sex_coefficients[, "Estimate"],
  OR = Sex_odds_ratios,
  `2.5 %` = Sex_conf_int[, 1],
  `97.5 %` = Sex_conf_int[, 2],
  `P-value` = Sex_p_values
)
print(Sex_Odd_results)

#Race
MBdata$`Race recode (W, B, AI, API)` <- factor(MBdata$`Race recode (W, B, AI, API)`)
MBdata$`Race recode (W, B, AI, API)` <- relevel(MBdata$`Race recode (W, B, AI, API)`, ref = "White")
Race_model <- glm(SurgeryREC ~ MBdata$`Race recode (W, B, AI, API)`, data = MBdata, family = binomial)
summary_Race_model <- summary(Race_model)
Race_coefficients <- summary_Race_model$coefficients
Race_odds_ratios <- exp(Race_coefficients[, "Estimate"])
Race_conf_int <- exp(confint(Race_model))  # 95% confidence intervals for odds ratios
Race_p_values <- Race_coefficients[, "Pr(>|z|)"]
Race_Odd_results <- data.frame(
  Estimate = Race_coefficients[, "Estimate"],
  OR = Race_odds_ratios,
  `2.5 %` = Race_conf_int[, 1],
  `97.5 %` = Race_conf_int[, 2],
  `P-value` = Race_p_values
)
print(Race_Odd_results)

#Ethnicity
His_model <- glm(SurgeryREC ~ MBdata$`Origin recode NHIA (Hispanic, Non-Hisp)`, data = MBdata, family = binomial)
summary_His_model <- summary(His_model)
His_coefficients <- summary_His_model$coefficients
His_odds_ratios <- exp(His_coefficients[, "Estimate"])
His_conf_int <- exp(confint(His_model))  # 95% confidence intervals for odds ratios
His_p_values <- His_coefficients[, "Pr(>|z|)"]
His_Odd_results <- data.frame(
  Estimate = His_coefficients[, "Estimate"],
  OR = His_odds_ratios,
  `2.5 %` = His_conf_int[, 1],
  `97.5 %` = His_conf_int[, 2],
  `P-value` = His_p_values
)
print(His_Odd_results)

#MHI
MHI_model <- glm(SurgeryREC ~ MBdata$IncomeIndex, data = MBdata, family = binomial)
summary_MHI_model <- summary(MHI_model)
MHI_coefficients <- summary_MHI_model$coefficients
MHI_odds_ratios <- exp(MHI_coefficients[, "Estimate"])
MHI_conf_int <- exp(confint(MHI_model))  # 95% confidence intervals for odds ratios
MHI_p_values <- MHI_coefficients[, "Pr(>|z|)"]
MHI_Odd_results <- data.frame(
  Estimate = MHI_coefficients[, "Estimate"],
  OR = MHI_odds_ratios,
  `2.5 %` = MHI_conf_int[, 1],
  `97.5 %` = MHI_conf_int[, 2],
  `P-value` = MHI_p_values
)
print(MHI_Odd_results)

#NoTu
NoTu_model <- glm(MBdata$SurgeryREC ~ MBdata$`Total number of in situ/malignant tumors for patient`, family = binomial)
summary_NoTu_model <- summary(NoTu_model)
NoTu_coefficients <- summary_NoTu_model$coefficients
NoTu_odds_ratios <- exp(NoTu_coefficients[, "Estimate"])
NoTu_conf_int <- exp(confint(NoTu_model))  # 95% confidence intervals for odds ratios
NoTu_p_values <- NoTu_coefficients[, "Pr(>|z|)"]
NoTu_Odd_results <- data.frame(
  Estimate = NoTu_coefficients[, "Estimate"],
  OR = NoTu_odds_ratios,
  `2.5 %` = NoTu_conf_int[, 1],
  `97.5 %` = NoTu_conf_int[, 2],
  `P-value` = NoTu_p_values
)
print(NoTu_Odd_results)

#Function for SurgPerf
# Define the function
logistic_regression_analysis <- function(response_column, predictor_column, data) {
  # Create the formula for the logistic regression
  formula <- as.formula(paste(response_column, "~", predictor_column))
  
  # Fit the logistic regression model
  model <- glm(formula, data = data, family = binomial)
  
  # Get the summary of the model
  summary_model <- summary(model)
  
  # Extract the coefficients, odds ratios, confidence intervals, and p-values
  coefficients <- summary_model$coefficients
  odds_ratios <- exp(coefficients[, "Estimate"])
  conf_int <- exp(confint(model))  # 95% confidence intervals for odds ratios
  p_values <- coefficients[, "Pr(>|z|)"]
  
  # Combine results into a single data frame for easy viewing
  odd_results <- data.frame(
    Estimate = coefficients[, "Estimate"],
    OR = odds_ratios,
    `2.5 %` = conf_int[, 1],
    `97.5 %` = conf_int[, 2],
    `P-value` = p_values
  )
  
  # Return the results
  return(odd_results)
}

#SurgeryPerf
#Age
AgePerf_Odd_results <- logistic_regression_analysis(
  response_column = "Perf",
  predictor_column = "`Age Recoded`",
  data = MBdata
 )
print(AgePerf_Odd_results)

#Sex
SexPerf_Odd_results <- logistic_regression_analysis(
  response_column = "Perf",
  predictor_column = "Sex",
  data = MBdata
)
print(SexPerf_Odd_results)

#Race
RacePerf_Odd_results <- logistic_regression_analysis(
  response_column = "Perf",
  predictor_column = "`Race recode (W, B, AI, API)`",
  data = MBdata
)
print(RacePerf_Odd_results)

#Ethnicity
HisPerf_Odd_results <- logistic_regression_analysis(
  response_column = "Perf",
  predictor_column = "`Origin recode NHIA (Hispanic, Non-Hisp)`",
  data = MBdata
)
print(HisPerf_Odd_results)

#MHI
MHIPerf_Odd_results <- logistic_regression_analysis(
  response_column = "Perf",
  predictor_column = "IncomeIndex",
  data = MBdata
)
print(MHIPerf_Odd_results)

#NoTu
NoTuPerf_Odd_results <- logistic_regression_analysis(
  response_column = "Perf",
  predictor_column = "`Total number of in situ/malignant tumors for patient`",
  data = MBdata
)
print(NoTuPerf_Odd_results)

#Multivariable
#Multivariable 
SRMV_model <- glm(MBdata$SurgeryREC ~ MBdata$`Age Recoded` + MBdata$Sex + MBdata$`Race recode (W, B, AI, API)` + MBdata$`Origin recode NHIA (Hispanic, Non-Hisp)` + MBdata$IncomeIndex + MBdata$`Total number of in situ/malignant tumors for patient`, family = binomial)
summary(SRMV_model)
summary_SRMV_model <- summary(SRMV_model)
SRMV_coefficients <- summary_SRMV_model$coefficients
SRMV_odds_ratios <- exp(SRMV_coefficients[, "Estimate"])
SRMV_conf_int <- exp(confint(SRMV_model))  # 95% confidence intervals for odds ratios
SRMV_p_values <- SRMV_coefficients[, "Pr(>|z|)"]

# Combine results into a single data frame for easy viewing
SRMV_results <- data.frame(
  Estimate = SRMV_coefficients[, "Estimate"],
  OR = SRMV_odds_ratios,
  `2.5 %` = SRMV_conf_int[, 1],
  `97.5 %` = SRMV_conf_int[, 2],
  `P-value` = SRMV_p_values
)

# Display the results
print(SRMV_results)

SMV_model <- glm(MBdata$Perf ~ MBdata$`Age Recoded` + MBdata$Sex + MBdata$`Race recode (W, B, AI, API)` + MBdata$`Origin recode NHIA (Hispanic, Non-Hisp)` + MBdata$IncomeIndex + MBdata$`Total number of in situ/malignant tumors for patient`, family = binomial)
summary(SMV_model)
summary_SMV_model <- summary(SMV_model)
SMV_coefficients <- summary_SMV_model$coefficients
SMV_odds_ratios <- exp(SMV_coefficients[, "Estimate"])
SMV_conf_int <- exp(confint(SMV_model))  # 95% confidence intervals for odds ratios
SMV_p_values <- SMV_coefficients[, "Pr(>|z|)"]

# Combine results into a single data frame for easy viewing
SMV_results <- data.frame(
  Estimate = SMV_coefficients[, "Estimate"],
  OR = SMV_odds_ratios,
  `2.5 %` = SMV_conf_int[, 1],
  `97.5 %` = SMV_conf_int[, 2],
  `P-value` = SMV_p_values
)

# Display the results
print(SMV_results)





