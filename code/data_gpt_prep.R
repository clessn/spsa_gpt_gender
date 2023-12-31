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

#### Variable intervention de l'État ####
# 0 = gauche 1 = droite

table(df_ses_gpt$rich_gap)
clean_ses_gpt$rich_gap_gpt <- NA
clean_ses_gpt$rich_gap_gpt[df_ses_gpt$rich_gap %in% c("Much more", "Much-more")] <- 0
clean_ses_gpt$rich_gap_gpt[df_ses_gpt$rich_gap %in% c("Somewhat more", "Somewhat-more", "Somewhatmore", "SomewhatMore")] <- 0.25
clean_ses_gpt$rich_gap_gpt[df_ses_gpt$rich_gap %in% c("About the same as now", "About-the-same-as-now", "Aboutthesameasnow")] <- 0.5
clean_ses_gpt$rich_gap_gpt[df_ses_gpt$rich_gap == "Somewhat less"] <- 0.75
clean_ses_gpt$rich_gap_gpt[df_ses_gpt$rich_gap == "Much less"] <- 1
clean_ses_gpt$rich_gap_gpt[df_ses_gpt$rich_gap == "Don't know/Prefer not to answer"] <- NA
table(clean_ses_gpt$rich_gap_gpt)

table(df_ses_gpt$ineq_prob)
clean_ses_gpt$ineq_prob_gpt <- NA
clean_ses_gpt$ineq_prob_gpt[df_ses_gpt$ineq_prob %in% c("Definitely yes", "Definitely-yes", "Definitelyyes", " Definitely yes")] <- 0
clean_ses_gpt$ineq_prob_gpt[df_ses_gpt$ineq_prob %in% c("Probably yes", " Probably yes", "probably yes", "Probably-yes", "Probablyyes", "ProbablyYes")] <- 0.25
clean_ses_gpt$ineq_prob_gpt[df_ses_gpt$ineq_prob %in% c("Not sure", "Not-sure", "Notsure")] <- 0.5
clean_ses_gpt$ineq_prob_gpt[df_ses_gpt$ineq_prob %in% c("Probably not", "Probablynot")] <- 0.75
clean_ses_gpt$ineq_prob_gpt[df_ses_gpt$ineq_prob == "Definitely not"] <- 1
clean_ses_gpt$ineq_prob_gpt[df_ses_gpt$ineq_prob == "Don't know/Prefer not to answer"] <- NA
table(clean_ses_gpt$ineq_prob_gpt)

table(df_ses_gpt$option_priv)
clean_ses_gpt$option_priv_gpt <- NA
clean_ses_gpt$option_priv_gpt[df_ses_gpt$option_priv %in% c("See to it that everyone has a decent standard of living", " See to it that everyone has a decent standard of living", "See to it that everyone has a decent standard of living\n", "See to it that everyone has a decent standard of living.", "See-to-it-that-everyone-has-a-decent-standard-of-living", "See-to-itthateveryonehasadecentstandardofliving", "Seetoiteveryonehasadecentstandardofliving", "Seetoitthateveryonehasadecentstandardofliving", "SeeToItThatEveryoneHasADecentStandardOfLiving")] <- 0
clean_ses_gpt$option_priv_gpt[df_ses_gpt$option_priv %in% c("Leave people to get ahead on their own", "Leave people to get ahead on their own\n", "Leave people to get ahead on their own.", "Leave-people-to-get-ahead-on-their-own", "Leavepeopletogetaheadontheirown", "Leavepoepletogetaheadontheirown", "Leavepoletogaheadontheirown", "Leavepoletogetaheadontheirown", "Leavepropletogetaheadontheirown", "Leavetopeopletogetaheadontheirown.")] <- 1
clean_ses_gpt$option_priv_gpt[df_ses_gpt$option_priv == "Don't know/Prefer not to answer"] <- NA
table(clean_ses_gpt$option_priv_gpt)


# Save data as RDS --------------------------------------------------------

saveRDS(clean_ses_gpt, "_SharedFolder_spsa_gpt_gender/data/clean_ses_gpt.rds")
