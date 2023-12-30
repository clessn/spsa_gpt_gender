# Packages -----------------------------------------------------------------
library(tidyverse)

# Data --------------------------------------------------------------------

GptData <- readRDS("_SharedFolder_spsa_gpt_gender/data/questions/df_ses_gpt2023.rds")

# Création de colonnes binaires
visMin_dummies <- model.matrix(~ visMin - 1, data=GptData)

# Ajout des colonnes binaires à votre dataframe
GptData <- cbind(GptData, visMin_dummies)

# ICI: faire les échelles GPT ---------------------------------------------

## il faut remplacer les réponses likert (somewhat agree etc.) par les chiffres correspondant, puis faire la moyenne
#### Comme on faisait dans le sondage


# Real world models ------------------------------------------------------------------

model_intervention <- readRDS("_SharedFolder_spsa_gpt_gender/data/models/intervention.rds")
model_enviro <- readRDS("_SharedFolder_spsa_gpt_gender/data/models/enviro.rds")
model_immigr <- readRDS("_SharedFolder_spsa_gpt_gender/data/models/immigr.rds")

# Predict real world models on GptData -----------------------------------------------

GptData$pred_intervention <- predict(object = model_intervention,
                                     newdata = GptData)
hist(GptData$pred_intervention) ## prédictions du modèle sur la grille
