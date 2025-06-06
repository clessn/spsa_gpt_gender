# Packages ---------------------------------------------------------------
library(rcompanion)
library(DescTools)
library(dplyr)

# Load data --------------------------------------------------------------
df_ces21 <- read.csv("_SharedFolder_spsa_gpt_gender/data/CES21_CleanData_2024-01-07.csv")
rawdata <- sondr::read_any_csv("_SharedFolder_spsa_gpt_gender/data/ces2021.csv")
df_grille <- readRDS("_SharedFolder_spsa_gpt_gender/data/clean_ses_gpt.rds")

# Clean data -------------------------------------------------------------
table(df_ces21$female)
df_ces21$gender <- NA
df_ces21$gender[df_ces21$female == 1] <- "female"
df_ces21$gender[df_ces21$female == 0] <- "male"
df_ces21$gender <- factor(df_ces21$gender)
table(df_ces21$gender)

table(df_ces21$age34m)
table(df_ces21$age35p54)
table(df_ces21$age55p)
df_ces21$age <- NA
df_ces21$age[df_ces21$age34m == 1] <- "18-34 years old"
df_ces21$age[df_ces21$age35p54== 1] <- "35-54 years old"
df_ces21$age[df_ces21$age55p == 1] <- "55 years old or more"
table(df_ces21$age)
df_ces21$age <- factor(df_ces21$age, levels = c("18-34 years old", "35-54 years old", "55 years old or more"))

table(df_ces21$educBHS)
table(df_ces21$educHS)
table(df_ces21$educUniv)
df_ces21$education <- NA
df_ces21$education[df_ces21$educBHS == 1] <- "before highschool"
df_ces21$education[df_ces21$educHS == 1] <- "collegial/technical"
df_ces21$education[df_ces21$educUniv == 1] <- "university"
df_ces21$education <- factor(df_ces21$education, levels = c("before highschool", "collegial/technical", "university"))
table(df_ces21$education)

table(df_ces21$income)
df_ces21$income <- NA
df_ces21$income[as.numeric(rawdata$cps21_income_number) < 40000] <- "less than $40,000"
df_ces21$income[as.numeric(rawdata$cps21_income_number) >= 40000 & 
                as.numeric(rawdata$cps21_income_number) <= 100000] <- "between $40,001 and $100,000"
df_ces21$income[as.numeric(rawdata$cps21_income_number) > 100000] <- "more than $100,000"
df_ces21$income <- factor(df_ces21$income, level = c("less than $40,000", "between $40,001 and $100,000", "more than $100,000"))
table(df_ces21$income)

table(df_ces21$quebec)
df_ces21$province <- NA
df_ces21$province[df_ces21$bc == 1] <- "British-Columbia"
df_ces21$province[df_ces21$alberta == 1 | df_ces21$sask == 1 | df_ces21$manitoba == 1] <- "Prairies (Alberta, Saskatchewan, Manitoba)"
df_ces21$province[df_ces21$nb == 1 | df_ces21$nfld == 1 | df_ces21$ns == 1 | df_ces21$pei == 1] <- "Atlantic Canada"
df_ces21$province[df_ces21$ontario == 1] <- "Ontario"
df_ces21$province[df_ces21$quebec == 1] <- "Québec"
df_ces21$province[df_ces21$nunavut == 1 | df_ces21$yukon19 == 1 | df_ces21$nwt19 == 1] <- "Canadian Territories"
df_ces21$province <- factor(df_ces21$province)
table(df_ces21$province)

df_ces21$rurality <- NA
table(rawdata$pes21_rural_urban)
df_ces21$rurality[rawdata$pes21_rural_urban == "A large town or city (more than 50K people)" | rawdata$pes21_rural_urban == "A suburb of a large town or city"] <- "urban"
df_ces21$rurality[rawdata$pes21_rural_urban == "A middle-sized town (15K-50K people) not attached to a city" | rawdata$pes21_rural_urban == "A rural area or village (less than1000 people)" | rawdata$pes21_rural_urban == "A small town (more than 1000 people but less than 15K)"] <- "rural"
df_ces21$rurality <- factor(df_ces21$rurality)
table(df_ces21$rurality)

visMin <- c("White", "Black", "Indigenous", "Asian", "Arab")

df_ces21$visMinArab <- ifelse(rawdata$cps21_vismin_1 == 1, 1, 0)
table(df_ces21$visMinArab)
df_ces21$visMinAsian <- ifelse(rawdata$cps21_vismin_2 == 1, 1, 0)
df_ces21$visMinBlack <- ifelse(rawdata$cps21_vismin_3 == 1, 1, 0)
df_ces21$visMinIndigenous <- ifelse(rawdata$cps21_vismin_4 == 1, 1, 0)
df_ces21$visMinWhite <- ifelse(rawdata$cps21_vismin_9 == 1, 1, 0)

df_ces21$visMin <- factor(
  dplyr::case_when(
    rawdata$cps21_vismin_1 == 1 ~ "Arab",
    rawdata$cps21_vismin_2 == 1 ~ "Asian",
    rawdata$cps21_vismin_3 == 1 ~ "Black",
    rawdata$cps21_vismin_4 == 1 ~ "Indigenous",
    rawdata$cps21_vismin_9 == 1 ~ "White",
    TRUE ~ "Other"
  ),
  levels = c("Arab", "Asian", "Black", "Indigenous", "White", "Other")
)

table(df_ces21$visMin)

# Scales -----------------------------------------------------------------

## Intervention de l'État --------------------------------------------------

nnas_gd_econo <- (1 - is.na(df_ces21$issProbInegality21)) +
  (1 - is.na(df_ces21$issGovShouldDoStdOfLiving21)) +
  (1 - is.na(df_ces21$issGapRichPoor21))

df_ces21$scale_gd_econo <- (ifelse(is.na(df_ces21$issProbInegality21), 0, df_ces21$issProbInegality21) +
                           ifelse(is.na(df_ces21$issGovShouldDoStdOfLiving21), 0, df_ces21$issGovShouldDoStdOfLiving21) +
                           ifelse(is.na(df_ces21$issGapRichPoor21), 0, df_ces21$issGapRichPoor21)) / nnas_gd_econo

df_ces21$scale_gd_econo[is.nan(df_ces21$scale_gd_econo)] <- NA

hist(df_ces21$scale_gd_econo)

## Environnement -----------------------------------------------------------

nnas_enviro <-
  (1 - is.na(df_ces21$issTaxeCarbone21)) +
  (1 - is.na(df_ces21$issSpendEnviro21)) +
  (1 - is.na(df_ces21$issReglEnviroPrix21))

df_ces21$scale_enviro <- (ifelse(is.na(df_ces21$issTaxeCarbone21), 0, df_ces21$issTaxeCarbone21) +
                         ifelse(is.na(df_ces21$issSpendEnviro21), 0, df_ces21$issSpendEnviro21) +
                         ifelse(is.na(df_ces21$issReglEnviroPrix21), 0, df_ces21$issReglEnviroPrix21)) / nnas_enviro

df_ces21$scale_enviro[is.nan(df_ces21$scale_enviro)] <- NA

hist(df_ces21$scale_enviro)

## Immigration -------------------------------------------------------------

nnas_immigr <- (1 - is.na(df_ces21$issGovSpendImmigr21)) +
  (1 - is.na(df_ces21$issImmigrEnleveJobs21)) +
  (1 - is.na(df_ces21$issIntégrationImmigr21))
table(nnas_immigr)

df_ces21$scale_immigr <- (ifelse(is.na(df_ces21$issGovSpendImmigr21), 0, df_ces21$issGovSpendImmigr21) +
                         ifelse(is.na(df_ces21$issImmigrEnleveJobs21), 0, df_ces21$issImmigrEnleveJobs21) +
                         ifelse(is.na(df_ces21$issIntégrationImmigr21), 0, df_ces21$issIntégrationImmigr21)) / nnas_immigr

df_ces21$scale_immigr[is.nan(df_ces21$scale_immigr)] <- NA

hist(df_ces21$scale_immigr)


# Scales in gpt data -----------------------------------------------------

df_grille$scale_gpt_intervention <- (df_grille$rich_gap_gpt + df_grille$ineq_prob_gpt + df_grille$option_priv_gpt) / 3

df_grille$scale_gpt_enviro <- (df_grille$reduce_emi_gpt + df_grille$cont_carb_tax_gpt + df_grille$enviro_reg_gpt) / 3

df_grille$scale_gpt_immigr <- (df_grille$fed_spend_gpt + df_grille$recent_immi_gpt + df_grille$immi_take_jobs_gpt) / 3

# Join gpt scales on df_ces21 --------------------------------------------

df_join <- df_grille |> 
  select(
    gender, age, education,
    income, province, rurality, visMin,
    scale_gpt_enviro, scale_gpt_immigr, scale_gpt_intervention
  ) %>%
  left_join(
    df_ces21, .,
    by = c(
      "gender", "age", "education",
      "income", "province", "rurality",
      "visMin"
    )
  ) |> 
  tidyr::drop_na(
    scale_gd_econo,
    scale_enviro,
    scale_immigr,
    scale_gpt_enviro, scale_gpt_immigr, scale_gpt_intervention
  )

saveRDS(df_join, "_SharedFolder_spsa_gpt_gender/data/df_cramer.rds")
# Cramer's V -------------------------------------------------------------

rcompanion::cramerV(df_join$scale_gd_econo, df_join$scale_gpt_intervention)
rcompanion::cramerV(df_join$scale_enviro, df_join$scale_gpt_enviro)
rcompanion::cramerV(df_join$scale_immigr, df_join$scale_gpt_immigr)
