########################
#title: "Operationalization"
#author: "Nir Kosti"
#date: "`r Sys.time()`"
########################


# hebrew setup
Sys.setlocale(category = "LC_ALL", locale = "Hebrew_Israel.1255")
#rmarkdown setup
knitr::opts_chunk$set(message = FALSE)
#read relevant libraries 
library(haven)
library(scales)
library(ggplot2)
library(reshape2)
library(ggthemes)
library(dplyr)
library(qualtRics)
library(tidyr)
library(stringr)
library(gridExtra)
library(openxlsx)
library(scales)
library(gtsummary)
library(psy)
library(lavaan)
library(labelled)

#read data with qualtRics 
setwd("C:\\Users\\Nir\\Dropbox\\Civil Servants' Perceptions of Democratic Decline\\data\\raw data")
data <- read_spss("survey_14.3.2023.sav")

#create a new dataset with combined variables for men and women
new_data <- data[, 1:19]

#combine variables for men and women
new_data$Q2 <- coalesce(data$Q2_cs_filter, data$Q2_f)

#Q4 - influence past
new_data$Q4_1 <- coalesce(data$Q4_influence_past_1, data$Q4_f_influence_past_1)
new_data$Q4_2 <- coalesce(data$Q4_influence_past_2, data$Q4_f_influence_past_2)
new_data$Q4_3 <-coalesce(data$Q4_influence_past_3, data$Q4_f_influence_past_3)

#Q5 - influence future
new_data$Q5 <- coalesce(data$Q5_influence_future, data$Q5_f_influence_futur)

#Q6 - influence comments
new_data$Q6 <- str_c(data$Q6_influence_comment, data$Q6_f_influence_comme)

#Q10 - meritocracy - past
new_data$Q10_1 <- coalesce(data$Q10_meritocracy_past_1, data$Q10_f_meritocracy_pa_1)
new_data$Q10_2 <- coalesce(data$Q10_meritocracy_past_2, data$Q10_f_meritocracy_pa_2)
new_data$Q10_3 <- coalesce(data$Q10_meritocracy_past_3, data$Q10_f_meritocracy_pa_3)

#Q11 - meritocracy - future
new_data$Q11 <- coalesce(data$Q11_meritocracy_futu, data$Q11_f_meritocracy_fu)

#Q12 - meritocracy - comments
new_data$Q12 <- str_c(data$Q12_meritocracy_comm, data$Q12_f_meritocracy_co)

#Q13 - functioning - past
new_data$Q13 <- coalesce(data$Q13_functioning_poli,data$Q13_f_functioning_po)

#Q14 - functioning - hr
new_data$Q14 <- coalesce(data$Q14_functioning_hr, data$Q14_f_functioni_hr)

#Q15 - democratic decline - future expectations
new_data$Q15 <- coalesce(data$Q15_democracy_future, data$Q15_f_democracy_futu)

#Q16 - democratic decline - protest
new_data$Q16_1 <- coalesce(data$Q16_democracy_1, data$Q16_f_democracy_1)
new_data$Q16_2 <- coalesce(data$Q16_democracy_2, data$Q16_f_democracy_2)
new_data$Q16_3 <- coalesce(data$Q16_democracy_3, data$Q16_f_democracy_3)
new_data$Q16_4 <- coalesce(data$Q16_democracy_4, data$Q16_f_democracy_4)
new_data$Q16_5 <- coalesce(data$Q16_democracy_5, data$Q16_f_democracy_5)

#Q17 - democratic decline - comments
new_data$Q17 <- str_c(data$Q17_democracy_commen, data$Q17_f_democracy_comm) 

#Q19 - investment at work - past
new_data$Q19 <- coalesce(data$Q19_investment_past, data$Q19_f_investment_pa) 

#Q20 - investment at work - collegaues
new_data$Q20 <- coalesce(data$Q20_investment_compa, data$Q20_f_investment_com) 

#Q21 - investment at work - future
new_data$Q21 <- coalesce(data$Q21_investment_futur, data$Q21_f_investment_fut)

#Q21_a - investment at work - comments
new_data$Q21_a <- str_c(data$Q21_comment, data$Q21_f_comment)

#Q22 - voice - past
new_data$Q22_1 <- coalesce(data$Q22_voice_past_1, data$Q22_f_voice_past_1)
new_data$Q22_2 <- coalesce(data$Q22_voice_past_2, data$Q22_f_voice_past_2)
new_data$Q22_3 <- coalesce(data$Q22_voice_past_3, data$Q22_f_voice_past_3)

#Q22_a - voice - collegaues
new_data$Q22_a <- coalesce(data$Q22a_voice_compare, data$Q22a_f_voice_compare)

#Q23 - voice - future
new_data$Q23 <- coalesce(data$Q23_voice_future, data$Q23_f_voice_future)

#Q24 - voice - comments
new_data$Q24 <- str_c(data$Q24_voice_comment, data$Q24_f_voice_comment)

#Q25 - exit 
new_data$Q25 <- coalesce(data$Q25_CS_resign, data$Q25_f_CS_resign)

#Q26 - exit - private sector's equal salary
new_data$Q26 <- coalesce(data$Q26_CS_resign_money, data$Q26_f_CS_resign_mone)

#Q27-  reutrn to CS
new_data$Q27 <- coalesce(data$Q27_CS_return_money, data$Q27_f_CS_return_mon)

#Q27_a- comments
new_data$Q27_a <- str_c(data$Q27_comment, data$Q27_f_comment)

#Q28 - PSM
new_data$Q28_1 <- coalesce(data$Q28_PSM_1, data$Q28_f_PSM_1)
new_data$Q28_2 <- coalesce(data$Q28_PSM_2, data$Q28_f_PSM_2)
new_data$Q28_3 <- coalesce(data$Q28_PSM_3, data$Q28_f_PSM_3)
new_data$Q28_4 <- coalesce(data$Q28_PSM_4, data$Q28_f_PSM_4)

#Q29 - subotage
new_data$Q29_1 <- coalesce(data$Q29_subotage_1, data$Q29_f_subotage_1)
new_data$Q29_2 <- coalesce(data$Q29_subotage_2, data$Q29_f_subotage_2)
new_data$Q29_3 <- coalesce(data$Q29_subotage_3, data$Q29_f_subotage_3)
new_data$Q29_4 <- coalesce(data$Q29_subotage_4, data$Q29_f_subotage_4)
new_data$Q29_5 <- coalesce(data$Q29_subotage_5, data$Q29_f_subotage_5)
new_data$Q29_6 <- coalesce(data$Q29_subotage_6, data$Q29_f_subotage_6)
new_data$Q29_7 <- coalesce(data$Q29_subotage_7, data$Q29_f_subotage_7)
new_data$Q29_8 <- coalesce(data$Q29_subotage_8, data$Q29_f_subotage_8)
new_data$Q29_9 <- coalesce(data$Q29_subotage_9, data$Q29_f_subotage_9)

#Q29_a - subotage comments
new_data$Q29_a <- str_c(data$Q29a_subotage_commen, data$Q29a_f_subotage_comm)

#Q31 - unit (if still works)
new_data$Q31 <- coalesce(data$Q31_unit, data$Q31_f_unit)

#Q31_a - unit (if still works) - other
new_data$Q31_a <- str_c(data$Q31_unit_26_TEXT, data$Q31_f_unit_26_TEXT)

#Q32 - unit (if left)
new_data$Q32 <- coalesce(data$Q32_unit_left, data$Q32_f_unit_left)

#Q32_a - unit (if left) - other
new_data$Q32_a <- str_c(data$Q32_unit_left_26_TEXT, data$Q32_unit_left_26_TEXT)

#Q33 - seniority (if still works)
new_data$Q33 <- coalesce(data$Q33_seniority, data$Q33_f_seniority)

#Q34 - seniority (if left)
new_data$Q34 <- coalesce(data$Q34_seniority_left, data$Q34_f_seniority_left)

#Q35 - appointing (if still works)
new_data$Q35 <- coalesce(data$Q35_appointing, data$Q35_f_appointing) 

#Q35_a - appointing (if still works) - other
new_data$Q35_a <- coalesce(data$Q35_appointing_4_TEXT, data$Q35_f_appointing_4_TEXT) 

#Q36 - appointing (if left)
new_data$Q36 <- coalesce(data$Q36_appointing_left, data$Q36_f_appointing_lef) 

#Q36_a - appointing (if left) - other
new_data$Q36_a <- coalesce(data$Q36_appointing_left_4_TEXT, data$Q36_f_appointing_lef_4_TEXT) 

#Q37 - percieved seniority (if still works)
new_data$Q37 <- coalesce(data$Q37_senior_per, data$Q37_f_senior_per) 

#Q38 - percieved seniority (if left)
new_data$Q38 <- coalesce(data$Q38_senior_per_left, data$Q38_f_senior_per_le) 

#Q39 - jewish
new_data$Q39 <- coalesce(data$Q39_jewish, data$Q39_f_jewish) 

#Q40 - age
new_data$Q40 <- coalesce(data$Q40_age, data$Q40_f_age) 

#Q41 - religion
new_data$Q41 <- coalesce(data$Q41_religion, data$Q41_f_religion) 

#Q42 - education
new_data$Q42 <- coalesce(data$Q42_education, data$Q42_f_education)

#Q42_a - education - other
new_data$Q42_a <- coalesce(data$Q42_education_999_TEXT, data$Q42_f_education_999_TEXT)

#Q43 - education field
new_data$Q43 <- str_c(data$Q43_education_field, data$Q43_f_education_fiel)

#Q44 - survey feelings
new_data$Q44 <- str_c(data$Q44_feelings, data$Q44_f_feelings)

#Q45 - survey formulation
new_data$Q45 <- coalesce(data$Q45_common_method_va, data$Q45_f_common_meth_va)

#Q47 - emails
new_data$Q47 <- str_c(data$Q47_f_future_resear, data$Q47_future_research)




### Operationalization of explanatory variables ###

# Compose an index for perceptions of past professional influence and rescale it
new_data$PAST_INFLUENCE <- (new_data$Q4_1 + new_data$Q4_2 + new_data$Q4_3)/3
new_data$PAST_INFLUENCE <-  rescale(new_data$PAST_INFLUENCE , from = c(min(new_data$PAST_INFLUENCE, na.rm = TRUE), max(new_data$PAST_INFLUENCE, na.rm = TRUE)), to = c(0, 1))

# Compose an index for past politicization (former labelling- past meritiocratic), reverse, and rescale it
new_data$PAST_POLITICIZATION <- (new_data$Q10_1 +new_data$Q10_2 + new_data$Q10_3)/3
reverse_cols = c("PAST_POLITICIZATION")
new_data[ , reverse_cols] <- 8 - new_data[ , reverse_cols]
new_data$PAST_POLITICIZATION <-  rescale(new_data$PAST_POLITICIZATION , from = c(min(new_data$PAST_POLITICIZATION, na.rm = TRUE), max(new_data$PAST_POLITICIZATION, na.rm = TRUE)), to = c(0, 1))

## Past effort
new_data$PAST_EFFORT <- remove_labels(new_data$Q19)
new_data$PAST_EFFORT <-  rescale(new_data$PAST_EFFORT , from = c(min(new_data$PAST_EFFORT, na.rm = TRUE), max(new_data$PAST_EFFORT, na.rm = TRUE)), to = c(0, 1))


## Compose an index for Past voice behavior and rescale it
new_data$PAST_VOICE <-(new_data$Q22_1 +new_data$Q22_2 + new_data$Q22_3)/3
new_data$PAST_VOICE <-rescale(new_data$PAST_VOICE , from = c(min(new_data$PAST_VOICE, na.rm = TRUE), max(new_data$PAST_VOICE, na.rm = TRUE)), to = c(0, 1))

## Projected change in professional influence - rescale 
new_data$PROJECT_INFLUENCE <- remove_labels(new_data$Q5)
new_data$PROJECT_INFLUENCE <-  rescale(new_data$PROJECT_INFLUENCE , from = c(min(new_data$PROJECT_INFLUENCE, na.rm = TRUE), max(new_data$PROJECT_INFLUENCE, na.rm = TRUE)), to = c(0, 1))

## Projected change in politicization (former labelling- proejct meritiocratic) - reverse and rescale 
new_data$PROJECT_POLITICIZATION <- remove_labels(new_data$Q11)
reverse_cols = c("PROJECT_POLITICIZATION")
new_data[ , reverse_cols] <- 6 - new_data[ , reverse_cols]
new_data$PROJECT_POLITICIZATION <-  rescale(new_data$PROJECT_POLITICIZATION , from = c(min(new_data$PROJECT_POLITICIZATION, na.rm = TRUE), max(new_data$PROJECT_POLITICIZATION, na.rm = TRUE)), to = c(0, 1))

## Compose an index for Perceptions of democratic backsliding, reverse and rescale
reverse_cols = c("Q16_2", "Q16_3", "Q16_4")
new_data[ , reverse_cols] <- 8 - new_data[ , reverse_cols]

#without Q15
new_data$BACKSLIDING_1 <- (new_data$Q16_1 +new_data$Q16_2 + new_data$Q16_3 + new_data$Q16_4 + new_data$Q16_5)/5
new_data$BACKSLIDING_1 <-rescale(new_data$BACKSLIDING_1 , from = c(min(new_data$BACKSLIDING_1, na.rm = TRUE), max(new_data$BACKSLIDING_1, na.rm = TRUE)), to = c(0, 1))

#add Q15
new_data$Q15[new_data$Q15 == 999] <- NA
new_data$Q15_rescale <- remove_labels(new_data$Q15)
new_data$Q15_rescale <- rescale(new_data$Q15_rescale , from = c(min(new_data$Q15_rescale, na.rm = TRUE), max(new_data$Q15_rescale, na.rm = TRUE)), to = c(1, 7))
reverse_cols = c("Q15_rescale")
new_data[ , reverse_cols] <- 8 - new_data[ , reverse_cols]

new_data$BACKSLIDING_2 <- (new_data$Q16_1 +new_data$Q16_2 + new_data$Q16_3 + new_data$Q16_4 + new_data$Q16_5 + new_data$Q15_rescale)/6
new_data$BACKSLIDING_2 <-rescale(new_data$BACKSLIDING_2 , from = c(min(new_data$BACKSLIDING_2, na.rm = TRUE), max(new_data$BACKSLIDING_2, na.rm = TRUE)), to = c(0, 1))

## Compose an index for Public Service Motivation (PSM) and reverse 
new_data$PSM <- (new_data$Q28_1 + new_data$Q28_2 + new_data$Q28_3 + new_data$Q28_4)/4
new_data$PSM <-rescale(new_data$PSM , from = c(min(new_data$PSM, na.rm = TRUE), max(new_data$PSM, na.rm = TRUE)), to = c(0, 1))




### Operationalization of dependant variables ###

## Compose an index for Projected exertion of effort at work
new_data$PROJECT_EFFORT <- remove_labels(new_data$Q21)
new_data$PROJECT_EFFORT <-  rescale(new_data$PROJECT_EFFORT , from = c(min(new_data$PROJECT_EFFORT, na.rm = TRUE), max(new_data$PROJECT_EFFORT, na.rm = TRUE)), to = c(0, 1))

## Projected voice behavior
new_data$PROJECT_VOICE <- remove_labels(new_data$Q23)
new_data$PROJECT_VOICE <-  rescale(new_data$PROJECT_VOICE , from = c(min(new_data$PROJECT_VOICE, na.rm = TRUE), max(new_data$PROJECT_VOICE, na.rm = TRUE)), to = c(0, 1))

## Intentions to exit the civil service
new_data$INTENT_EXIT_1 <- remove_labels(new_data$Q25)
new_data$INTENT_EXIT_1 <-  rescale(new_data$INTENT_EXIT_1 , from = c(min(new_data$INTENT_EXIT_1, na.rm = TRUE), max(new_data$INTENT_EXIT_1, na.rm = TRUE)), to = c(0, 1))

new_data$INTENT_EXIT_2 <- remove_labels(new_data$Q26)
new_data$INTENT_EXIT_2 <-  rescale(new_data$INTENT_EXIT_2 , from = c(min(new_data$INTENT_EXIT_2, na.rm = TRUE), max(new_data$INTENT_EXIT_2, na.rm = TRUE)), to = c(0, 1))

## Compose an index for Scenario - sabotage
new_data$RESPONSE_SABOTAGE <- (new_data$Q29_1 +new_data$Q29_2 + new_data$Q29_3 + new_data$Q29_4)/4
new_data$RESPONSE_SABOTAGE <-rescale(new_data$RESPONSE_SABOTAGE , from = c(min(new_data$RESPONSE_SABOTAGE, na.rm = TRUE), max(new_data$RESPONSE_SABOTAGE, na.rm = TRUE)), to = c(0, 1))

## Compose an index for Scenario - voice
new_data$RESPONSE_VOICE <- (new_data$Q29_5 +new_data$Q29_6 + new_data$Q29_7)/3
new_data$RESPONSE_VOICE <-rescale(new_data$RESPONSE_VOICE , from = c(min(new_data$RESPONSE_VOICE, na.rm = TRUE), max(new_data$RESPONSE_VOICE, na.rm = TRUE)), to = c(0, 1))

## Compose an index for Scenario - exit
new_data$RESPONSE_EXIT <- (new_data$Q29_8 +new_data$Q29_9)/2 
new_data$RESPONSE_EXIT <-rescale(new_data$RESPONSE_EXIT , from = c(min(new_data$RESPONSE_EXIT, na.rm = TRUE), max(new_data$RESPONSE_EXIT, na.rm = TRUE)), to = c(0, 1))




### Operationalization of control variables ###
#ranking
new_data$ranking <- coalesce(new_data$Q37, new_data$Q38)
new_data$ranking <- as.factor(new_data$ranking)
levels(new_data$ranking) <- c("junior", "middle", "senior", "very senior")

#position_type
new_data$position_type <- coalesce(new_data$Q35, new_data$Q36)
new_data$position_type <- as.factor(new_data$position_type)
levels(new_data$position_type) <- c("trust based", "competitive tender", "replacement", "other")

#tenure
new_data$tenure <- coalesce(new_data$Q33, new_data$Q34)
new_data$tenure <- as.factor(new_data$tenure)
levels(new_data$tenure) <- c("1-", "1-5", "6-10", "11-20", "20+")

#ministry
ministry_text_to_fill <- read.xlsx("C:\\Users\\Nir\\Dropbox\\Civil Servants' Perceptions of Democratic Decline\\data\\coding of units\\units_manually_coded.xlsx")
new_data$ministry <- coalesce(new_data$Q31, new_data$Q32)

for (response_id_new_data in 1:length(new_data$ResponseId)){
  for (response_id_ministry_to_fill in 1:length(ministry_text_to_fill$ResponseId)){
    if (ministry_text_to_fill$ResponseId[response_id_ministry_to_fill] == new_data$ResponseId[response_id_new_data] &  is.na(ministry_text_to_fill$code[response_id_ministry_to_fill]) == FALSE){
      new_data$ministry[response_id_new_data] <- ministry_text_to_fill$code[response_id_ministry_to_fill]
    }
  }
}

#education field
education_field_to_fill <- read.xlsx("C:\\Users\\Nir\\Dropbox\\Civil Servants' Perceptions of Democratic Decline\\data\\coding of academic degree\\coding_Orpaz.xlsx")
new_data$education_field <- new_data$Q43

for (response_id_new_data in 1:length(new_data$ResponseId)){
  for (response_id_education_field_to_fill in 1:length(education_field_to_fill$ResponseId)){
    if (education_field_to_fill$ResponseId[response_id_education_field_to_fill] == new_data$ResponseId[response_id_new_data] &  is.na(education_field_to_fill$Q43[response_id_education_field_to_fill]) == FALSE){
      new_data$education_field[response_id_new_data] <- education_field_to_fill$Q43[response_id_education_field_to_fill]
    }
  }
}

#jurist
new_data$jurist <- 0

for (response_id_new_data in 1:length(new_data$ResponseId)){
  for (response_id_education_field_to_fill in 1:length(education_field_to_fill$ResponseId)){
    if (education_field_to_fill$ResponseId[response_id_education_field_to_fill] == new_data$ResponseId[response_id_new_data]){
      new_data$jurist[response_id_new_data] <- education_field_to_fill$jurist[response_id_education_field_to_fill]
    }
  }
}

#ministry affliation
new_data$ministry <- as.factor(new_data$ministry)
levels(new_data$ministry) <- levels(new_data$ministry) <- c("Energy", "Defense", "National Security", "Treasury", "Construction and Housing", "Health", "Environmental Protection", "Foreign Affairs", "Education", "Agriculture", "Economy", "Science and Technology", "Law", "Welfare and Social Security", "Aliyah and Integration", "Interior", "Prime Minister", "Population and Immigration Authority", "Social Equality", "Religious Services", "Transportation", "Tourism ", "Communications", "Culture and Sports", "Other", "Ministry of Labor", "Local Authorities", "Independent Authorities")

#age
new_data$age <- new_data$Q40
new_data$age <- as.factor(new_data$age)
levels(new_data$age) <- c("20-30", "31-40", "41-50", "51-60", "61+")

#gender
new_data$gender <- new_data$Q1_gender
new_data <- new_data %>% mutate(gender=case_when(gender %in% c("1","9") ~ "1",
                                                 gender %in% "2" ~ "2"))
new_data$gender <- as.factor(new_data$gender)
levels(new_data$gender) <- c("male", "female")

#religiosity
new_data$religiosity <- new_data$Q41
new_data$religiosity <- as.factor(new_data$religiosity)
levels(new_data$religiosity) <- c("secular", "traditional-unsecular", "traditional-religious", "religious", "haredi", "other")

#nationality
new_data$nationality <- new_data$Q39
new_data$nationality <- as.factor(new_data$nationality)
levels(new_data$nationality) <- c("jewish", "non-jewish")

#education level
new_data$education <- new_data$Q42
new_data$education <- as.factor(new_data$education)
levels(new_data$education) <- c("high-school", "bachelor", "master", "phd", "other")

#Common Method Bias
new_data$cmv <- new_data$Q45

#future research
new_data$emails_future_research <- new_data$Q45

### Subset and Save ###
#subset relevant data for analysis
data_for_analysis <- subset(new_data, select = c("ResponseId", "PAST_INFLUENCE", "PROJECT_INFLUENCE", "PAST_POLITICIZATION", "PROJECT_POLITICIZATION",  "PAST_EFFORT", "PROJECT_EFFORT",   "PAST_VOICE", "PROJECT_VOICE",  "BACKSLIDING_1",  "BACKSLIDING_2", "PSM",  "INTENT_EXIT_1", "INTENT_EXIT_2", "RESPONSE_EXIT", "RESPONSE_SABOTAGE", "RESPONSE_VOICE", "ranking", "position_type", "tenure", "ministry","age", "gender", "religiosity", "nationality", "education"))

#subset relevant variables for analysis including indices' composed questions
data_for_analysis_extended <- subset(new_data, select = c("ResponseId", "PAST_INFLUENCE", "Q4_1", "Q4_2", "Q4_3", "PAST_POLITICIZATION", "Q10_1", "Q10_2", "Q10_3", "PAST_EFFORT", "PAST_VOICE", "Q22_1", "Q22_2", "Q22_3", "PROJECT_VOICE", "PROJECT_POLITICIZATION", "BACKSLIDING_1", "Q16_1", "Q16_2", "Q16_3", "Q16_4", "Q16_5", "BACKSLIDING_2", "Q15", "Q15_rescale", "PSM", "Q28_1", "Q28_2", "Q28_3", "Q28_4", "PROJECT_EFFORT", "PROJECT_INFLUENCE", "INTENT_EXIT_1", "INTENT_EXIT_2", "RESPONSE_EXIT","Q29_8", "Q29_9", "RESPONSE_SABOTAGE", "Q29_1", "Q29_2", "Q29_3", "Q29_4", "RESPONSE_VOICE", "Q29_5", "Q29_6", "Q29_7", "ranking", "position_type", "tenure", "ministry","age", "gender", "religiosity", "nationality", "education", "education_field", "jurist", "cmv", "emails_future_research"))
#write.csv(data_for_analysis_extended, "data_for_analysis_extended_with_emails.csv", row.names = FALSE)
