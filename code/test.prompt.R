library(tidyverse)
library(openai)

#### Data laoding - Clean data CES 21 ####
df_ces21 <- read.csv("_SharedFolder_spsa_gpt_gender/data/CES21_CleanData_2023-10-27.csv")

#### OpenAi loop testing ####
#### Test loop avec 1 seul répondant ####