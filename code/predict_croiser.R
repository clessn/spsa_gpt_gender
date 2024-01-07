# Packages -----------------------------------------------------------------
library(tidyverse)
library(ggplot2)
#install.packages("marginaleffects")
library(marginaleffects)

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

# Immigration où 0 = Pour l'immigration et 1 = Contre l'immigration

GptData$pred_immigr <- predict(object = model_immigr,
                               newdata = GptData)
hist(GptData$pred_immigr)

ggplot(GptData, aes(x = pred_immigr)) +
  geom_histogram(binwidth = 0.05, fill = "red", color = "black") +
  clessnverse::theme_clean_light() +
  labs(title = "Histogramme de pred_immigr")

ggsave("_SharedFolder_spsa_gpt_gender/graph/GPTData$pred_immigr.png", height = 10, width = 12)

# Croiser les prédictions de gpt et des modèles ---------------------------

ggplot(GptData, aes(x = pred_enviro, y = gpt_enviro)) +
  geom_point() +
  geom_smooth(method = "lm")

ggplot(GptData, aes(x = pred_immigr, y = gpt_immigr)) +
  geom_point() +
  geom_smooth(method = "lm")

ggplot(GptData, aes(x = pred_intervention, y = gpt_intervention)) +
  geom_point() +
  geom_smooth(method = "lm")

ggplot(GptData, aes(x = pred_enviro, y = gpt_enviro)) +
  geom_point() +
  geom_smooth(method = "lm") +
  facet_wrap(~gender)

ggplot(GptData, aes(x = pred_enviro, y = gpt_enviro)) +
  geom_point() +
  geom_smooth(method = "lm") +
  facet_wrap(~visMin)

ggplot(GptData, aes(x = pred_immigr, y = gpt_immigr)) +
  geom_point() +
  geom_smooth(method = "lm") +
  facet_wrap(~visMin)

# Créer une VD par échelle qui est la différence (biais) entre les deux modèles -------

## créer différence

GptData$diff_pred_intervention <- (GptData$gpt_intervention - GptData$pred_intervention)
GptData$diff_pred_enviro <- (GptData$gpt_enviro - GptData$pred_enviro)
GptData$diff_pred_immigr <- (GptData$gpt_immigr - GptData$pred_immigr)

## créer une différence absolue avec abs()

GptData$abs_diff_pred_intervention <- abs(GptData$gpt_intervention - GptData$pred_intervention)
GptData$abs_diff_pred_enviro <- abs(GptData$gpt_enviro - GptData$pred_enviro)
GptData$abs_diff_pred_immigr <- abs(GptData$gpt_immigr - GptData$pred_immigr)


# Faire un modèle de régression avec les VIs et les contrôles -------------

model_multi_intervention <- lm(diff_pred_intervention ~ gender * visMin + age + education + income +
                            province + rurality, data = GptData)
saveRDS(model_multi_intervention, "_SharedFolder_spsa_gpt_gender/data/models/predict_difference/intervention.rds")
summary(model_multi_intervention)

model_multi_abs_intervention <- lm(abs_diff_pred_intervention ~ gender * visMin + age + education + income +
                                 province + rurality, data = GptData)
saveRDS(model_multi_abs_intervention, "_SharedFolder_spsa_gpt_gender/data/models/predict_difference/abs_intervention.rds")
summary(model_multi_abs_intervention)

model_multi_enviro <- lm(diff_pred_enviro ~ gender * visMin + age + education + income +
                           province + rurality, data = GptData)
saveRDS(model_multi_enviro, "_SharedFolder_spsa_gpt_gender/data/models/predict_difference/enviro.rds")
summary(model_multi_enviro)

model_multi_abs_enviro <- lm(abs_diff_pred_enviro ~ gender * visMin + age + education + income +
                           province + rurality, data = GptData)
saveRDS(model_multi_abs_enviro, "_SharedFolder_spsa_gpt_gender/data/models/predict_difference/abs_enviro.rds")
summary(model_multi_abs_enviro)

model_multi_immigr <- lm(diff_pred_immigr ~ gender * visMin + age + education + income +
                           province + rurality, data = GptData)
saveRDS(model_multi_immigr, "_SharedFolder_spsa_gpt_gender/data/models/predict_difference/immigr.rds")
summary(model_multi_immigr)

model_multi_abs_immigr <- lm(abs_diff_pred_immigr ~ gender * visMin + age + education + income +
                           province + rurality, data = GptData)
saveRDS(model_multi_abs_immigr, "_SharedFolder_spsa_gpt_gender/data/models/predict_difference/abs_immigr.rds")
summary(model_multi_abs_immigr)


# Prédire les biais par VI ----------------------------------------------

## Prédire les biais absolus selon les visMin

graphdataabsvismin <- rbind(predictions(model_multi_abs_intervention, by = "visMin",
                                        conf_level = 0.99) ,
                   predictions(model_multi_abs_enviro, by = "visMin", conf_level = 0.99),
                   predictions(model_multi_abs_immigr, by = "visMin", conf_level = 0.99)) %>% 
  mutate(id = c(rep('intervention', times = 5),
                rep('enviro', times = 5),
                rep('immigr', times = 5))) 

## Prédire les biais selon les visMin

graphdatavismin <- rbind(predictions(model_multi_intervention, by = "visMin", 
                                     conf_level = 0.99),
                         predictions(model_multi_enviro, by = "visMin", conf_level = 0.99),
                         predictions(model_multi_immigr, by = "visMin", conf_level = 0.99)) %>% 
  mutate(id = c(rep('intervention', times = 5),
                rep('enviro', times = 5),
                rep('immigr', times = 5))) 

## Prédire les biais absolus selon le genre

graphdataabsgender <- rbind(predictions(model_multi_abs_intervention, by = "gender",
                                        conf_level = 0.99),
                   predictions(model_multi_abs_enviro, by = "gender", conf_level = 0.99),
                   predictions(model_multi_abs_immigr, by = "gender", conf_level = 0.99)) %>% 
  mutate(id = c(rep('intervention', times = 2),
                rep('enviro', times = 2),
                rep('immigr', times = 2))) 

## Prédire les biais selon le genre

graphdatagender <- rbind(predictions(model_multi_intervention, by = "gender", conf_level = 0.99),
                         predictions(model_multi_enviro, by = "gender", conf_level = 0.99),
                         predictions(model_multi_immigr, by = "gender", conf_level = 0.99)) %>% 
  mutate(id = c(rep('intervention', times = 2),
                rep('enviro', times = 2),
                rep('immigr', times = 2))) 


# Graphiques globaux selon les 2 VD--------------------------------------------------------------


## Graphiques absolus selon les visMin

ggplot(data = graphdataabsvismin, aes(x = visMin, y = estimate)) +
  geom_point() +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2, color = "black", alpha = 0.7) +
  clessnverse::theme_clean_light() +
  labs(x = "Race", y = "Absolute differences", title = "Absolute differences between GPT-4 prediction and real-world data by race") +
  facet_wrap(~ id)

ggsave("_SharedFolder_spsa_gpt_gender/graph/À présenter/absolute_biases_vismin_predicted.png", width = 12, height = 6)
  
## Graphiques absolus selon le gender

ggplot(data = graphdataabsgender, aes(x = gender, y = estimate)) +
  geom_point() +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2, color = "black", alpha = 0.7) +
  clessnverse::theme_clean_light() +
  labs(x = "Gender", y = "Absolute differences", title = "Absolute differences between GPT-4 prediction and real-world data by gender") +
  facet_wrap(~ id)
  
ggsave("_SharedFolder_spsa_gpt_gender/graph/À présenter/absolute_biases_gender_predicted.png", width = 12, height = 6)

## Graphiques biais selon les vismin

ggplot(data = graphdatavismin, aes(x = estimate, y = visMin)) +
  geom_point() +
  geom_linerange(aes(xmin = conf.low, xmax = conf.high), color = "black", alpha = 0.7) +
  geom_vline(xintercept = 0, color = "black") +
  clessnverse::theme_clean_light() +
  scale_x_continuous(limits = c(-1, 1)) +
  labs(x = "Differences (non-absolute)", y = "Race", title = "Differences between GPT-4 prediction and real-world data by race") +
  facet_wrap(~ id)

ggsave("_SharedFolder_spsa_gpt_gender/graph/À présenter/biases_vismin.png", width = 12, height = 6)

## Graphiques biais selon le genre

ggplot(data = graphdatagender, aes(x = estimate, y = gender)) +
  geom_point() +
  geom_linerange(aes(xmin = conf.low, xmax = conf.high), color = "black", alpha = 0.7) +
  geom_vline(xintercept = 0, color = "black") +
  scale_x_continuous(limits = c(-1, 1)) +
  clessnverse::theme_clean_light() +
  labs(x = "Differences (non-absolute)", y = "Gender", title = "Differences between GPT-4 prediction and real-world data by gender") +
  facet_wrap(~ id)

ggsave("_SharedFolder_spsa_gpt_gender/graph/À présenter/biases_gender.png", width = 12, height = 6)


# Graphiques intervention de l'État ---------------------------------------

filtered_intervention_vismin <- subset(graphdatavismin, id == "intervention")
filtered_intervention_gender <- subset(graphdatagender, id == "intervention")

ggplot(data = filtered_intervention_vismin, aes(x = estimate, y = visMin)) +
  geom_point() +
  geom_linerange(aes(xmin = conf.low, xmax = conf.high), color = "black", alpha = 0.7) +
  geom_vline(xintercept = 0, color = "black") +
  scale_x_continuous(limits = c(-1, 1)) +
  clessnverse::theme_clean_light() +
  labs(x = "Differences (non-absolute)", y = "Race", title = "Differences between GPT-4 prediction and real-world data on State intervention scale by race")

ggsave("_SharedFolder_spsa_gpt_gender/graph/À présenter/biases_intervention_vismin.png", width =  12, height = 6)

ggplot(data = filtered_intervention_gender, aes(x = estimate, y = gender)) +
  geom_point() +
  geom_linerange(aes(xmin = conf.low, xmax = conf.high), color = "black", alpha = 0.7) +
  geom_vline(xintercept = 0, color = "black") +
  scale_x_continuous(limits = c(-1, 1)) +
  clessnverse::theme_clean_light() +
  labs(x = "Differences (non-absolute)", y = "Gender", title = "Differences between GPT-4 prediction and real-world data on State intervention scale by gender")

ggsave("_SharedFolder_spsa_gpt_gender/graph/À présenter/biases_intervention_gender.png", width = 12, height = 6)


# Graphiques enviro -------------------------------------------------------

filtered_enviro_vismin <- subset(graphdatavismin, id == "enviro")
filtered_enviro_gender <- subset(graphdatagender, id == "enviro")

ggplot(data = filtered_enviro_vismin, aes(x = estimate, y = visMin)) +
  geom_point() +
  geom_linerange(aes(xmin = conf.low, xmax = conf.high), color = "black", alpha = 0.7) +
  geom_vline(xintercept = 0, color = "black") +
  scale_x_continuous(limits = c(-1, 1)) +
  clessnverse::theme_clean_light() +
  labs(x = "Differences (non-absolute)", y = "Race", title = "Differences between GPT-4 prediction and real-world data on environnement's scale by race")

ggsave("_SharedFolder_spsa_gpt_gender/graph/À présenter/biases_enviro_vismin.png", width = 12, height = 6)

ggplot(data = filtered_enviro_gender, aes(x = estimate, y = gender)) +
  geom_point() +
  geom_linerange(aes(xmin = conf.low, xmax = conf.high), color = "black", alpha = 0.7) +
  geom_vline(xintercept = 0, color = "black") +
  scale_x_continuous(limits = c(-1, 1)) +
  clessnverse::theme_clean_light() +
  labs(x = "Differences (non-absolute)", y = "Gender", title = "Differences between GPT-4 prediction and real-world data on environnement's scale by gender")

ggsave("_SharedFolder_spsa_gpt_gender/graph/À présenter/biases_enviro_gender.png", width = 12, height = 6)


# Graphiques immigr -------------------------------------------------------

filtered_immigr_vismin <- subset(graphdatavismin, id == "immigr")
filtered_immigr_gender <- subset(graphdatagender, id == "immigr")

ggplot(data = filtered_immigr_vismin, aes(x = estimate, y = visMin)) +
  geom_point() +
  geom_linerange(aes(xmin = conf.low, xmax = conf.high), color = "black", alpha = 0.7) +
  geom_vline(xintercept = 0, color = "black") +
  scale_x_continuous(limits = c(-1, 1)) +
  clessnverse::theme_clean_light() +
  labs(x = "Differences (non-absolute)", y = "Race", title = "Differences between GPT-4 prediction and real-world data on immigration's scale by race")

ggsave("_SharedFolder_spsa_gpt_gender/graph/À présenter/biases_immigr_vismin.png", width = 12, height = 6)

ggplot(data = filtered_immigr_gender, aes(x = estimate, y = gender)) +
  geom_point() +
  geom_linerange(aes(xmin = conf.low, xmax = conf.high), color = "black", alpha = 0.7) +
  geom_vline(xintercept = 0, color = "black") +
  scale_x_continuous(limits = c(-1, 1)) +
  clessnverse::theme_clean_light() +
  labs(x = "Differences (non-absolute)", y = "Gender", title = "Differences between GPT-4 prediction and real-world data on immigration's scale by race") +
  annotate("text", x = -1, y = 0.5, label = "Support immgiration", hjust = 0, vjust = 0.5, color = "black") +
  annotate("text", x = 1, y = 0.5, label = "Doesn't support immigration", hjust = 1, vjust = 0.5, color = "black")

ggsave("_SharedFolder_spsa_gpt_gender/graph/À présenter/biases_immigr_gender.png", width = 12, height = 6)

# Graphiques intersectionnalité -------------------------------------------

# Intervention

dataintersectionality_intervention <- predictions(model_multi_abs_intervention, by = c("gender", "visMin"))

ggplot(data = dataintersectionality_intervention, aes(x = estimate, y = visMin, color = gender)) +
  geom_point() +
  geom_linerange(aes(xmin = conf.low, xmax = conf.high), color = "black", alpha = 0.7) +
  geom_vline(xintercept = 0, color = "black") +
  scale_x_continuous(limits = c(-1, 1)) +
  clessnverse::theme_clean_light() +
  labs(x = "Absolute differences", y = "Race", title = "Absolute differences between GPT-4 prediction and real-world data on State intervention's scale")

ggsave("_SharedFolder_spsa_gpt_gender/graph/À présenter/biases_intervention_intersectional.png", width = 12, height = 6)

# Environnement

dataintersectionality_enviro <- predictions(model_multi_abs_enviro, by = c("gender", "visMin"))

ggplot(data = dataintersectionality_enviro, aes(x = estimate, y = visMin, color = gender)) +
  geom_point() +
  geom_linerange(aes(xmin = conf.low, xmax = conf.high), color = "black", alpha = 0.7) +
  geom_vline(xintercept = 0, color = "black") +
  scale_x_continuous(limits = c(-1, 1)) +
  clessnverse::theme_clean_light() +
  labs(x = "Absolute differences", y = "Race", title = "Absolute differences between GPT-4 prediction and real-world data on Environement's scale")

ggsave("_SharedFolder_spsa_gpt_gender/graph/À présenter/biases_enviro_intersectional.png", width = 12, height = 6)

# Immigration

dataintersectionality_immigr <- predictions(model_multi_abs_immigr, by = c("gender", "visMin"))

ggplot(data = dataintersectionality_immigr, aes(x = estimate, y = visMin, color = gender)) +
  geom_point() +
  geom_linerange(aes(xmin = conf.low, xmax = conf.high), color = "black", alpha = 0.7) +
  geom_vline(xintercept = 0, color = "black") +
  scale_x_continuous(limits = c(-1, 1)) +
  clessnverse::theme_clean_light() +
  labs(x = "Absolute differences", y = "Race", title = "Absolute differences between GPT-4 prediction and real-world data on Immigration's scale")

ggsave("_SharedFolder_spsa_gpt_gender/graph/À présenter/biases_immigr_intersectional.png", width = 12, height = 6)



  