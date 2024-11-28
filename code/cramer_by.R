library(dplyr)
library(knitr)
library(tidyr)

df <- readRDS("_SharedFolder_spsa_gpt_gender/data/df_cramer.rds") %>%
  mutate(Gender = ifelse(female == 1, "Female", "Male"))

# Cramer's V -------------------------------------------------------------

rcompanion::cramerV(df_join$scale_gd_econo, df_join$scale_gpt_intervention)
rcompanion::cramerV(df_join$scale_enviro, df_join$scale_gpt_enviro)
rcompanion::cramerV(df_join$scale_immigr, df_join$scale_gpt_immigr)

# Gender ----------------------------------------------------------------
# female == 1
# female == 0

# vismin ----------------------------------------------------------------
# visMinArab == 1
# visMinAsian == 1
# visMinBlack == 1
# visMinIndigenous == 1
# visMinWhite == 1

df_econ <- df %>%
  group_by(Gender, visMin) %>%
  summarise(
    cramer_econo = rcompanion::cramerV(scale_gd_econo, scale_gpt_intervention)
  ) %>%
  pivot_wider(names_from = visMin, values_from = cramer_econo)

df_enviro <- df %>%
  group_by(Gender, visMin) %>%
  summarise(
    cramer_enviro = rcompanion::cramerV(scale_enviro, scale_gpt_enviro)
  ) %>%
  pivot_wider(names_from = visMin, values_from = cramer_enviro)

df_immigr <- df %>%
  group_by(Gender, visMin) %>%
  summarise(
    cramer_immigr = rcompanion::cramerV(scale_immigr, scale_gpt_immigr)
  ) %>%
  pivot_wider(names_from = visMin, values_from = cramer_immigr)

# Combine the dataframes with a new column indicating the source
df_combined <- bind_rows(
  df_econ %>% mutate(Source = "Economic"),
  df_enviro %>% mutate(Source = "Environmental"),
  df_immigr %>% mutate(Source = "Immigration")
)

# Reorder columns to place the Source column first
df_combined <- df_combined %>% select(Source, everything())

# Create a Markdown table
df_combined %>%
  kable(format = "markdown", align = "lcccccc")
