library(tidyverse)

#### Loader ma df ####
df_ces21 <- sondr::read_any_csv("_SharedFolder_spsa_gpt_gender/data/ces2021.csv")

#### Vérifier le n des minorités visibles ####
table(df_ces21$cps21_vismin_1)
table(df_ces21$cps21_genderid)

#### Arab ####
table(df_ces21$cps21_vismin_1,df_ces21$cps21_genderid)

#### Asian ####
table(df_ces21$cps21_vismin_2,df_ces21$cps21_genderid)

#### Black ####
table(df_ces21$cps21_vismin_3,df_ces21$cps21_genderid)

#### Indigenous ####
table(df_ces21$cps21_vismin_4,df_ces21$cps21_genderid)

#### Latino/Latina ####
table(df_ces21$cps21_vismin_5,df_ces21$cps21_genderid)

#### South Asian ####
table(df_ces21$cps21_vismin_6,df_ces21$cps21_genderid)

#### Southeast Asian ####
table(df_ces21$cps21_vismin_7,df_ces21$cps21_genderid)

#### West Asian ####
table(df_ces21$cps21_vismin_8,df_ces21$cps21_genderid)

#### White ####
table(df_ces21$cps21_vismin_9,df_ces21$cps21_genderid)

#### Other (please specify) ####
table(df_ces21$cps21_vismin_10,df_ces21$cps21_genderid)

#### None of the above ####
table(df_ces21$cps21_vismin_11,df_ces21$cps21_genderid)

#### Prefer not to answer ####
table(df_ces21$cps21_vismin_12,df_ces21$cps21_genderid)
