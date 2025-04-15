# Packages ----------------------------------------------------------------
library(dplyr)
library(ggplot2)
library(marginaleffects)

# Models ------------------------------------------------------------------

m_intervention <- readRDS("_SharedFolder_spsa_gpt_gender/data/models/predict_difference/intervention.rds")
m_enviro <- readRDS("_SharedFolder_spsa_gpt_gender/data/models/predict_difference/enviro.rds")
m_immigr <- readRDS("_SharedFolder_spsa_gpt_gender/data/models/predict_difference/immigr.rds")
m_abs_intervention <- readRDS("_SharedFolder_spsa_gpt_gender/data/models/predict_difference/abs_intervention.rds")
m_abs_enviro <- readRDS("_SharedFolder_spsa_gpt_gender/data/models/predict_difference/abs_enviro.rds")
m_abs_immigr <- readRDS("_SharedFolder_spsa_gpt_gender/data/models/predict_difference/abs_immigr.rds")

# Predire les biais absolus selon les 2 VI: gender et visMin ----------------------

GraphAbs <- rbind(
  predictions(m_abs_intervention, by = c("gender", "visMin"), conf_level = 0.99),
  predictions(m_abs_enviro, by = c("gender", "visMin"), conf_level = 0.99),
  predictions(m_abs_immigr, by = c("gender", "visMin"), conf_level = 0.99)
) %>%
  mutate(
    id = c(
      rep('Intervention\ngouvernementale', times = 10),
      rep('Environnement', times = 10),
      rep('Immigration', times = 10)
    ),
    gender = ifelse(gender == "male", "Hommes", "Femmes"),
    visMin = case_when(
      visMin == "White" ~ "Personne blanche",
      visMin == "Black" ~ "Personne noire",
      visMin == "Arab" ~ "Personne arabe",
      visMin == "Asian" ~ "Personne asiatique",
      visMin == "Indigenous" ~ "Personne autochtone"
    )
  )


ggplot(GraphAbs, aes(x = estimate, y = visMin)) +
  geom_bar(stat = "identity",
           aes(group = gender,
               fill = gender),
           position = position_dodge(width = 0.5),
           width = 0.4,
           alpha = 0.4) +
  geom_point(aes(color = gender),
             size = 3,
             position = position_dodge(width = 0.5)) +
  geom_linerange(aes(xmin = conf.low, xmax = conf.high,
                     color = gender),
                 position = position_dodge(width = 0.5)) +
  scale_color_manual(values = c("#d3d3d3", "#4f4f4f")) +
  scale_fill_manual(values = c("#d3d3d3", "#4f4f4f")) +
  scale_x_continuous(limits = c(0, 0.4)) +
  facet_wrap(~id, ncol = 1) +
  clessnize::theme_clean_light() +
  ylab("") +
  labs(caption = "Les lignes autour des points représentent l'intervalle de confiance à 99 %.") +
  xlab("\nDifférence absolue prédite\nentre la prédiction de GPT et le modèle CES\n") +
  theme(axis.title.x = element_text(hjust = 0.5, size = 15), axis.text.x = element_text(size = 12), 
        axis.text.y = element_text(size = 12), strip.text.x = element_text(size = 12),
        legend.text = element_text(size = 12), plot.caption = element_text(size = 10))

ggsave("_SharedFolder_spsa_gpt_gender/graph/paper_pres/predicted_absolute_diff_fr.png",
       width = 9, height = 8)


# Predire les biais absolus selon chacune des 2 VI à la fois --------------

#Gender 

GraphAbsGender <-
  rbind(
    predictions(m_abs_intervention, by = c("gender"), conf_level = 0.99) ,
    predictions(m_abs_enviro, by = c("gender"), conf_level = 0.99),
    predictions(m_abs_immigr, by = c("gender"), conf_level = 0.99)
  ) %>%
  mutate(id = c(
    rep('Intervention\ngouvernementale', times = 2),
    rep('Environnement', times = 2),
    rep('Immigration', times = 2)
  ),
  gender = ifelse(gender == "male", "Hommes", "Femmes"))

ggplot(GraphAbsGender, aes(x = estimate, y = gender)) +
  geom_bar(stat = "identity",
           aes(group = gender,
               fill = gender),
           position = position_dodge(width = 0.5),
           width = 0.4,
           alpha = 0.4) +
  geom_point(aes(color = gender),
             size = 3,
             position = position_dodge(width = 0.5)) +
  geom_linerange(aes(xmin = conf.low, xmax = conf.high,
                     color = gender),
                 position = position_dodge(width = 0.5)) +
  scale_color_manual(values = c("#d3d3d3", "#4f4f4f")) +
  scale_fill_manual(values = c("#d3d3d3", "#4f4f4f")) +
  scale_x_continuous(limits = c(0, 0.4)) +
  facet_wrap(~id, ncol = 1) +
  clessnize::theme_clean_light() +
  ylab("") +
  labs(caption = "Les lignes autour des points représentent l'intervalle de confiance à 99 %.") +
  xlab("\nDifférence absolue prédite\nentre la prédiction de GPT et le modèle CES\n") +
  theme(axis.title.x = element_text(hjust = 0.5, size = 15), 
        legend.position = "none", axis.text.x = element_text(size = 12), 
        axis.text.y = element_text(size = 12), 
        strip.text.x = element_text(size = 12), 
        plot.caption = element_text(size = 10))

ggsave("_SharedFolder_spsa_gpt_gender/graph/paper_pres/predicted_absolute_diff_gender_fr.png",
       width = 9, height = 8)

# visMin

GraphAbsvisMin <-
  rbind(
    predictions(m_abs_intervention, by = c("visMin"), conf_level = 0.99) ,
    predictions(m_abs_enviro, by = c("visMin"), conf_level = 0.99),
    predictions(m_abs_immigr, by = c("visMin"), conf_level = 0.99)
  ) %>% mutate(
    id = c(
      rep('Intervention\ngouvernementale', times = 5),
      rep('Environnement', times = 5),
      rep('Immigration', times = 5)
    ),
    visMin = case_when(
      visMin == "White" ~ "Personne blanche",
      visMin == "Black" ~ "Personne noire",
      visMin == "Arab" ~ "Personne arabe",
      visMin == "Asian" ~ "Personne asiatique",
      visMin == "Indigenous" ~ "Personne autochtone"
    )
  )

         ggplot(GraphAbsvisMin, aes(x = estimate, y = visMin)) +
  geom_bar(stat = "identity",
           aes(group = visMin, fill = visMin),
           position = position_dodge(width = 0.5),
           width = 0.4,
           alpha = 0.4) +
  geom_point(aes(color = visMin),
             size = 3,
             position = position_dodge(width = 0.5)) +
  geom_linerange(aes(xmin = conf.low, xmax = conf.high,
                     color = visMin),
                 position = position_dodge(width = 0.5)) +
           scale_fill_manual(values = c("#d9d9d9","#bdbdbd","#969696", "#636363","#252525"
           )) +
           scale_color_manual(values = c("#d9d9d9", "#bdbdbd", "#969696", "#636363", "#252525")) +
  scale_x_continuous(limits = c(0, 0.4)) +
  facet_wrap(~id, ncol = 1) +
  clessnize::theme_clean_light() +
  ylab("") +
    labs(caption = "Les lignes autour des points représentent l'intervalle de confiance à 99 %.") +
    xlab("\nDifférence absolue prédite\nentre la prédiction de GPT et le modèle CES\n") +
  theme(axis.title.x = element_text(hjust = 0.5, size = 15), legend.position = "none",
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        strip.text.x = element_text(size = 12),
        plot.caption = element_text(size = 10))
  
ggsave("_SharedFolder_spsa_gpt_gender/graph/paper_pres/predicted_absolute_diff_visMin_fr.png",
       width = 9, height = 8)


# Predire les biais raw ---------------------------------------------------

GraphRaw <-
  rbind(
    predictions(m_intervention, by = c("gender", "visMin"), conf_level = 0.99) ,
    predictions(m_enviro, by = c("gender", "visMin"), conf_level = 0.99),
    predictions(m_immigr, by = c("gender", "visMin"), conf_level = 0.99)
  ) %>%
  mutate(
    id = c(
      rep('Intervention\ngouvernementale', times = 10),
      rep('Environnement', times = 10),
      rep('Immigration', times = 10)
    ),
    gender = ifelse(gender == "male", "Hommes", "Femmes"),
    visMin = case_when(
      visMin == "White" ~ "Personne blanche",
      visMin == "Black" ~ "Personne noire",
      visMin == "Arab" ~ "Personne arabe",
      visMin == "Asian" ~ "Personne asiatique",
      visMin == "Indigenous" ~ "Personne autochtone"
    )
  )

labels <- data.frame(
  id = c('Intervention\ngouvernementale',
         'Environnement',
         'Immigration'),
  left = c("Plus d'intervention étatique",
           "Action environnementale",
           "Perception positive de l’immigration"),
  right = c("Moins d'intervention étatique",
            "Inaction environnementale",
            "Perception négative de l’immigration")
)

ggplot(GraphRaw, aes(x = estimate, y = visMin)) +
  facet_wrap(~id, ncol = 1) +
  geom_vline(xintercept = 0, linetype = "dotted") +
  geom_bar(stat = "identity",
           aes(group = gender,
               fill = gender),
           position = position_dodge(width = 0.5),
           width = 0.4,
           alpha = 0.4) +
  geom_linerange(aes(xmin = conf.low, xmax = conf.high,
                     group = gender, color = gender),
                 position = position_dodge(width = 0.5)) +
  geom_point(aes(color = gender),
             size = 3,
             position = position_dodge(width = 0.5)) +
  scale_color_manual(values = c("#d3d3d3", "#4f4f4f")) +
  scale_fill_manual(values = c("#d3d3d3", "#4f4f4f")) +
  geom_text(data = labels, y = 0, x = -0.35,
            aes(y = 0, label = left),
            size = 4) +
  geom_text(data = labels, y = 0, x = 0.35,
            aes(y = 0, label = right),
            size = 4) +
  scale_x_continuous(limits = c(-0.4, 0.4)) +
  scale_y_discrete(expand = c(0.50, 0.50)) +
  clessnize::theme_clean_light() +
  ylab("") +
  labs(caption = "Les lignes autour des points représentent l'intervalle de confiance à 99 %.\nUn point à gauche de 0 indique que GPT a prédit une position plus à gauche que le modèle CES.") +
  xlab("\nDifférence brute prédite\nentre la prédiction de GPT et le modèle CES\n") +
  theme(axis.title.x = element_text(hjust = 0.5, size = 15), 
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        strip.text.x = element_text(size = 12),
        legend.text = element_text(size = 12),
        plot.caption = element_text(size = 10))

ggsave("_SharedFolder_spsa_gpt_gender/graph/paper_pres/predicted_raw_diff_fr.png",
       width = 12, height = 8)

