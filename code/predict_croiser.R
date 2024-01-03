# Packages -----------------------------------------------------------------
library(tidyverse)
library(ggplot2)

# Data --------------------------------------------------------------------

GptData <- readRDS("_SharedFolder_spsa_gpt_gender/data/clean_ses_gpt.rds")

# Création de colonnes binaires
visMin_dummies <- model.matrix(~ visMin - 1, data=GptData)

# Ajout des colonnes binaires à votre dataframe
GptData <- cbind(GptData, visMin_dummies)

# ICI: faire les échelles GPT ---------------------------------------------

GptData$gpt_intervention <- (GptData$rich_gap_gpt + GptData$ineq_prob_gpt + GptData$option_priv_gpt) / 3

GptData$gpt_enviro <- (GptData$reduce_emi_gpt + GptData$cont_carb_tax_gpt + GptData$enviro_reg_gpt) / 3

GptData$gpt_immigr <- (GptData$fed_spend_gpt + GptData$recent_immi_gpt + GptData$immi_take_jobs_gpt) / 3

# Real world models ------------------------------------------------------------------

model_intervention <- readRDS("_SharedFolder_spsa_gpt_gender/data/models/intervention.rds")
model_enviro <- readRDS("_SharedFolder_spsa_gpt_gender/data/models/enviro.rds")
model_immigr <- readRDS("_SharedFolder_spsa_gpt_gender/data/models/immigr.rds")

predict(object = model_enviro, newdata = GptData[1,])
predict(object = model_enviro, newdata = GptData[2,])

# Predict real world models on GptData -----------------------------------------------
# Intervention où 0 = gauche et 1 = droit
GptData$pred_intervention <- predict(object = model_intervention,
                                     newdata = GptData)
hist(GptData$pred_intervention) 

ggplot(GptData, aes(x = pred_intervention)) +
  geom_histogram(binwidth = 0.05, fill = "blue", color = "black") +
  clessnverse::theme_clean_light() +
  labs(title = "Histogramme de pred_intervention")
  

ggsave("_SharedFolder_spsa_gpt_gender/graph/GPTData$pred_intervention.png", height = 10, width = 12)

# Environnement où 0 = Attitudes environnementales positives et 1 = Attitudes environnementales négatives

GptData$pred_enviro <- predict(object = model_enviro,
                               newdata = GptData)
hist(GptData$pred_enviro)

ggplot(GptData, aes(x = pred_enviro)) +
  geom_histogram(binwidth = 0.05, fill = "green", color = "black") +
  clessnverse::theme_clean_light() +
  labs(title = "Histogramme de pred_enviro")

ggsave("_SharedFolder_spsa_gpt_gender/graph/GPTData$pred_enviro.png", height = 10, width = 12)

# Immigration où 0 = Pour l'immigration et 0 = Contre l'immigration

GptData$pred_immigr <- predict(object = model_immigr,
                               newdata = GptData)
hist(GptData$pred_immigr)

ggplot(GptData, aes(x = pred_immigr)) +
  geom_histogram(binwidth = 0.05, fill = "red", color = "black") +
  clessnverse::theme_clean_light() +
  labs(title = "Histogramme de pred_immigr")

ggsave("_SharedFolder_spsa_gpt_gender/graph/GPTData$pred_immigr.png", height = 10, width = 12)

# Croiser les prédictions de gpt et des modèles ---------------------------

ggplot(GptData, aes(x = pred_enviro, y = scale_enviro)) +
  geom_point() +
  geom_smooth(method = "lm")

ggplot(GptData, aes(x = pred_immigr, y = scale_immigr)) +
  geom_point() +
  geom_smooth(method = "lm")

ggplot(GptData, aes(x = pred_intervention, y = scale_intervention)) +
  geom_point() +
  geom_smooth(method = "lm")

ggplot(GptData, aes(x = pred_enviro, y = scale_enviro)) +
  geom_point() +
  geom_smooth(method = "lm") +
  facet_wrap(~gender)

ggplot(GptData, aes(x = pred_enviro, y = scale_enviro)) +
  geom_point() +
  geom_smooth(method = "lm") +
  facet_wrap(~visMin)

ggplot(GptData, aes(x = pred_immigr, y = scale_immigr)) +
  geom_point() +
  geom_smooth(method = "lm") +
  facet_wrap(~visMin)

# Créer une VD par échelle qui est la différence (biais) entre les deux modèles -------

## créer différence

## créer une différence absolue avec abs()
#abs(c(-5, 7, -2))
#GptData$scale_abs <- abs(GptData$scale)


# Faire un modèle de régression avec les VIs et les contrôles -------------


