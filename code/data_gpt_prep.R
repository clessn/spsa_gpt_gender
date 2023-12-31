library(tidyverse)
library(sondr)


# Load data ---------------------------------------------------------------

df_ses_gpt <- sondr::read_any_csv("/Users/sarah-janevincent/Dropbox/CLESSN/spsa_gpt_gender/_SharedFolder_spsa_gpt_gender/data/questions/df_ses_gpt.csv")


# Clean dataset -----------------------------------------------------------

clean_ses_gpt <- df_ses_gpt[, 1:8]


# Cleaning data GPT ---------------------------------------------------------

#### Variables immigration ####
# 0 = pour l'immigration 1 = contre l'immigration

table(df_ses_gpt$fed_spend)
clean_ses_gpt$fed_spend_gpt <- NA
clean_ses_gpt$fed_spend_gpt[df_ses_gpt$fed_spend == "Spend less"] <- 1
clean_ses_gpt$fed_spend_gpt[df_ses_gpt$fed_spend %in% c("Spend about the same as now", "Spend the same as now", "Spend-about-the-same-as-now")] <- 0.5
clean_ses_gpt$fed_spend_gpt[df_ses_gpt$fed_spend == "Spend more"] <- 0
clean_ses_gpt$fed_spend_gpt[df_ses_gpt$fed_spend == "Don't know/Prefer not to answer"] <- NA
table(clean_ses_gpt$fed_spend_gpt)

table(df_ses_gpt$recent_immi)
clean_ses_gpt$recent_immi_gpt <- NA
clean_ses_gpt$recent_immi_gpt[df_ses_gpt$recent_immi == "Strongly disagree"] <- 0
clean_ses_gpt$recent_immi_gpt[df_ses_gpt$recent_immi == "Somewhat disagree"] <- 0.25
clean_ses_gpt$recent_immi_gpt[df_ses_gpt$recent_immi %in% c("Neither agree nor disagree", "neither agree nor disagree", "Neither-agree-nor-disagree")] <- 0.5
clean_ses_gpt$recent_immi_gpt[df_ses_gpt$recent_immi == "Somewhat agree"] <- 0.75
clean_ses_gpt$recent_immi_gpt[df_ses_gpt$recent_immi == "Strongly agree"] <- 1
clean_ses_gpt$recent_immi_gpt[df_ses_gpt$recent_immi == "Don't know/Prefer not to answer"] <- NA
table(clean_ses_gpt$recent_immi_gpt)

table(df_ses_gpt$immi_take_jobs)
clean_ses_gpt$immi_take_jobs_gpt <- NA
clean_ses_gpt$immi_take_jobs_gpt[df_ses_gpt$immi_take_jobs %in% c("Strongly disagree", "Strongly disagree.")] <- 0
clean_ses_gpt$immi_take_jobs_gpt[df_ses_gpt$immi_take_jobs %in% c("Somewhat disagree", "Somewhat disagree\n", "somewhat disagree", "Somewhat disagree.")] <- 0.25
clean_ses_gpt$immi_take_jobs_gpt[df_ses_gpt$immi_take_jobs %in% c("Neither agree nor disagree", "Neither agree nor disagree\n", "Neither-agree-nor-disagree", "Neither agree nor disagree.")] <- 0.5
clean_ses_gpt$immi_take_jobs_gpt[df_ses_gpt$immi_take_jobs %in% c("Somewhat agree", "Somewhat agree\n", "Somewhat agree.")] <- 0.75
clean_ses_gpt$immi_take_jobs_gpt[df_ses_gpt$immi_take_jobs == "Strongly agree"] <- 1
clean_ses_gpt$immi_take_jobs_gpt[df_ses_gpt$immi_take_jobs == "Don't know/Prefer not to answer"] <- NA
table(clean_ses_gpt$immi_take_jobs_gpt)

#### Variables Environnement ####
# 0 = Attitudes environnementales positives 1 = Attitudes environnementales négatives

table(df_ses_gpt$reduce_emi)
clean_ses_gpt$reduce_emi_gpt <- NA
clean_ses_gpt$reduce_emi_gpt[df_ses_gpt$reduce_emi == "Spend less"] <- 1
clean_ses_gpt$reduce_emi_gpt[df_ses_gpt$reduce_emi == "Spend about the same as now"] <- 0.5
clean_ses_gpt$reduce_emi_gpt[df_ses_gpt$reduce_emi %in% c("Spend more", "spendMore")] <- 0
clean_ses_gpt$reduce_emi_gpt[df_ses_gpt$reduce_emi == "Don't know/Prefer not to answer"] <- NA
table(clean_ses_gpt$reduce_emi_gpt)

table(df_ses_gpt$cont_carb_tax)
clean_ses_gpt$cont_carb_tax_gpt <- NA
clean_ses_gpt$cont_carb_tax_gpt[df_ses_gpt$cont_carb_tax == "Strongly disagree"] <- 1
clean_ses_gpt$cont_carb_tax_gpt[df_ses_gpt$cont_carb_tax == "Somewhat disagree"] <- 0.75
clean_ses_gpt$cont_carb_tax_gpt[df_ses_gpt$cont_carb_tax == "Neither agree nor disagree"] <- 0.5
clean_ses_gpt$cont_carb_tax_gpt[df_ses_gpt$cont_carb_tax == "Somewhat agree"] <- 0.25
clean_ses_gpt$cont_carb_tax_gpt[df_ses_gpt$cont_carb_tax %in% c("Strongly agree", "StronglyAgree")] <- 0
clean_ses_gpt$cont_carb_tax_gpt[df_ses_gpt$cont_carb_tax == "Don't know/Prefer not to answer"] <- NA
table(clean_ses_gpt$cont_carb_tax_gpt)

table(df_ses_gpt$enviro_reg)
clean_ses_gpt$enviro_reg_gpt <- NA
clean_ses_gpt$enviro_reg_gpt[df_ses_gpt$enviro_reg == "Strongly disagree"] <- 1
clean_ses_gpt$enviro_reg_gpt[df_ses_gpt$enviro_reg %in% c("Somewhat disagree", "Somewhat disagree\n")] <- 0.75
clean_ses_gpt$enviro_reg_gpt[df_ses_gpt$enviro_reg %in% c("Neither agree nor disagree", "Neither agree nor disagree.")] <- 0.5
clean_ses_gpt$enviro_reg_gpt[df_ses_gpt$enviro_reg %in% c("Somewhat agree", "Somewhat agree\n", "Somewhat agree.", "SomewhatAgree")] <- 0.25
clean_ses_gpt$enviro_reg_gpt[df_ses_gpt$enviro_reg %in% c("Strongly agree", "Strongly agree\n", "Strongly agree.")] <- 0
clean_ses_gpt$enviro_reg_gpt[df_ses_gpt$enviro_reg == "Don't know/Prefer not to answer"] <- NA
table(clean_ses_gpt$enviro_reg_gpt)
