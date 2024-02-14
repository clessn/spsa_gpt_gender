#### Vérifier les données bizarres / NA

clessnverse::count_na(clean_ses_gpt$fed_spend_gpt)
## 4 NA

clessnverse::count_na(clean_ses_gpt$recent_immi_gpt)
## 1 NA

clessnverse::count_na(clean_ses_gpt$immi_take_jobs_gpt)
## 1 NA

clessnverse::count_na(clean_ses_gpt$reduce_emi_gpt)
## 1 NA

clessnverse::count_na(clean_ses_gpt$cont_carb_tax_gpt)
## 0 NA

clessnverse::count_na(clean_ses_gpt$enviro_reg_gpt)
## 0 NA

clessnverse::count_na(clean_ses_gpt$rich_gap_gpt)
## 131 NA
table(clean_ses_gpt$rich_gap_gpt, useNA = "always")
table(df_ses_gpt$rich_gap_gpt, useNA = "always")


clessnverse::count_na(clean_ses_gpt$ineq_prob_gpt)
## 128 NA
table(clean_ses_gpt$ineq_prob_gpt, useNA = "always")


clessnverse::count_na(clean_ses_gpt$option_priv_gpt)
## 127 NA