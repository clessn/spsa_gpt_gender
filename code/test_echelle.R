library(tidyverse)

#### Loader ma df ####
df_ces21 <- sondr::read_any_csv("_SharedFolder_spsa_gpt_gender/data/ces2021.csv")


# Gauche-droite économique ------------------------------------------------

#### pes21_gap Que devrait-on faire pour réduire les écarts entre les riches et les pauvres au Canada?

table(df_ces21$pes21_gap,useNA = "always")

table(df_ces21$pes21_gap,df_ces21$cps21_vismin_1,df_ces21$cps21_genderid, useNA = "always")
cps21_vismin_1

table(df_ces21$pes21_gap,df_ces21$cps21_pos_subsid, useNA = "always")
