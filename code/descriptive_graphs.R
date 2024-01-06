# Packages ----------------------------------------------------------------
library(dplyr)
library(ggplot2)


# Data --------------------------------------------------------------------

intervention <- readRDS("_SharedFolder_spsa_gpt_gender/data/models/intervention.rds")[["model"]] %>% 
  rename(scale_position = scale_gd_econo) %>% 
  mutate(scale = "intervention")
enviro <- readRDS("_SharedFolder_spsa_gpt_gender/data/models/enviro.rds")[["model"]] %>% 
  rename(scale_position = scale_enviro) %>% 
  mutate(scale = "enviro")
immigr <- readRDS("_SharedFolder_spsa_gpt_gender/data/models/immigr.rds")[["model"]] %>% 
  rename(scale_position = scale_immigr) %>% 
  mutate(scale = "immigr")

Data <- rbind(intervention, enviro, immigr)


# Graph -------------------------------------------------------------------

ggplot(Data, aes(x = scale_position, y = scale)) +
  ggridges::geom_density_ridges(quantile_lines = TRUE,
                                quantiles = c(0.25, 0.5, 0.75),
                                scale = 3) + ## changer ça pour empiler ou non les densités l'une sur l'autre
  labs(caption = "Lines in the densities show the 25th, 50th and 75th centiles of the distributions.")

ggplot(Data, aes(x = scale_position, y = scale)) +
  ggridges::geom_density_ridges(quantile_lines = TRUE,
                                quantiles = c(0.25, 0.5, 0.75),
                                scale = 1) + ## changer ça pour empiler ou non les densités l'une sur l'autre
  labs(caption = "Lines in the densities show the 25th, 50th and 75th centiles of the distributions.")

ggplot(Data, aes(x = scale_position, y = scale)) +
  ggridges::geom_density_ridges(quantile_lines = TRUE,
                                quantiles = c(0.25, 0.5, 0.75),
                                scale = 0.5) + ## changer ça pour empiler ou non les densités l'une sur l'autre
  labs(caption = "Lines in the densities show the 25th, 50th and 75th centiles of the distributions.")
