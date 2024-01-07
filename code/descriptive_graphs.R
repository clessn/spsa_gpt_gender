# Packages ----------------------------------------------------------------
library(dplyr)
library(ggplot2)
library(clessnverse)


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
labelsx <- data.frame(left = c("More intervention",
           "Environmental action",
           "Less immigration"),
  right = c("Less intervention",
            "Environmental inaction",
            "More immigration")
)


ggplot(Data, aes(x = scale_position, y = scale)) +
  ggridges::geom_density_ridges(quantile_lines = TRUE,
                                quantiles = c(0.25, 0.5, 0.75),
                                scale = 3,
                                fill = "#69b3a2",
                                color = "white",
                                alpha = 0.7) + 
  clessnverse::theme_clean_light() + 
  labs(caption = "Lines in the densities show the 25th, 50th and 75th centiles of the distributions.", title = "Distribution of CES Respondents",
       x = "", y = "Scale") +
  theme(axis.title.y = element_text(hjust = 0.5), plot.title = element_text(hjust = 0.5)) + scale_y_discrete(labels = c("Intervention", "Environment", "Immigration")) +
  annotate("text", x = 0, y = 0, label = "Positive view of immigration\n Environmental Action\n More State's intervention", hjust = 0, vjust = -0.5, color = "black") +
  annotate("text", x = 1, y = 0, label = "Negative view of immigration\n Environmental Inction\n Less State's intervention", hjust = 1, vjust = -0.5, color = "black")
  
ggsave("_SharedFolder_spsa_gpt_gender/graph/paper_pres/distribution_real_data_scales.png", height = 8, width = 12)

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
