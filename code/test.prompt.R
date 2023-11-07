library(tidyverse)
library(openai)
install.packages("devtools")
devtools::install_github("ellipse-science/sondr")


#### Data laoding - Raw data CES 21 ####
df_ces21 <- sondr::read_any_csv("_SharedFolder_spsa_gpt_gender/data/ces2021.csv")


#### OpenAi loop testing ####
#### Test loop avec 1 seul répondant ####