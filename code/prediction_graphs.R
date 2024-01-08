# Packages ----------------------------------------------------------------
library(dplyr)
library(ggplot2)


# Models ------------------------------------------------------------------

m_intervention <- readRDS("_SharedFolder_spsa_gpt_gender/data/models/predict_difference/intervention.rds")
m_enviro <- readRDS("_SharedFolder_spsa_gpt_gender/data/models/predict_difference/enviro.rds")
m_immigr <- readRDS("_SharedFolder_spsa_gpt_gender/data/models/predict_difference/immigr.rds")
m_abs_intervention <- readRDS("_SharedFolder_spsa_gpt_gender/data/models/predict_difference/abs_intervention.rds")
m_abs_enviro <- readRDS("_SharedFolder_spsa_gpt_gender/data/models/predict_difference/abs_enviro.rds")
m_abs_immigr <- readRDS("_SharedFolder_spsa_gpt_gender/data/models/predict_difference/abs_immigr.rds")

# Predire les biais absolus selon les 2 VI: gender et visMin ----------------------

GraphAbs <- rbind(
    predictions(m_abs_intervention, by = c("gender", "visMin"), conf_level = 0.99) ,
    predictions(m_abs_enviro, by = c("gender", "visMin"), conf_level = 0.99),
    predictions(m_abs_immigr, by = c("gender", "visMin"), conf_level = 0.99)
  ) %>%
  mutate(id = c(
    rep('Government\nintervention', times = 10),
    rep('Environment', times = 10),
    rep('Immigration', times = 10)
  ),
  gender = ifelse(gender == "male", "Men", "Women"))

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
  scale_color_manual(values = c("#003f5c", "#ff7c43")) +
  scale_fill_manual(values = c("#003f5c", "#ff7c43")) +
  scale_x_continuous(limits = c(0, 0.4)) +
  facet_wrap(~id, ncol = 1) +
  clessnverse::theme_clean_light() +
  ylab("") +
  labs(caption = "Lines around points represent the 99% confidence interval.") +
  xlab("\nPredicted absolute difference\nbetween GPT's prediction and CES model\n") +
  theme(axis.title.x = element_text(hjust = 0.5))

ggsave("_SharedFolder_spsa_gpt_gender/graph/paper_pres/predicted_absolute_diff.png",
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
    rep('Government\nintervention', times = 2),
    rep('Environment', times = 2),
    rep('Immigration', times = 2)
  ),
  gender = ifelse(gender == "male", "Men", "Women"))

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
  scale_color_manual(values = c("#003f5c", "#ff7c43")) +
  scale_fill_manual(values = c("#003f5c", "#ff7c43")) +
  scale_x_continuous(limits = c(0, 0.4)) +
  facet_wrap(~id, ncol = 1) +
  clessnverse::theme_clean_light() +
  ylab("") +
  labs(caption = "Lines around points represent the 99% confidence interval.") +
  xlab("\nPredicted absolute difference\nbetween GPT's prediction and CES model\n") +
  theme(axis.title.x = element_text(hjust = 0.5), legend.position = "none")


ggsave("_SharedFolder_spsa_gpt_gender/graph/paper_pres/predicted_absolute_diff_gender.png",
       width = 9, height = 8)

# visMin

GraphAbsvisMin <-
  rbind(
    predictions(m_abs_intervention, by = c("visMin"), conf_level = 0.99) ,
    predictions(m_abs_enviro, by = c("visMin"), conf_level = 0.99),
    predictions(m_abs_immigr, by = c("visMin"), conf_level = 0.99)
  ) %>% mutate(id = c(rep('Government\nintervention', times = 5),
                rep('Environment', times = 5),
                rep('Immigration', times = 5)))

         

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
  scale_color_manual(values = c("#FF6F8B", "#3A8BFF", "#4CAF50", "#FFA07A", "#8A2BE2")) +
  scale_fill_manual(values = c("#FF6F8B", "#3A8BFF", "#4CAF50", "#FFA07A", "#8A2BE2")) +
  scale_x_continuous(limits = c(0, 0.4)) +
  facet_wrap(~id, ncol = 1) +
  clessnverse::theme_clean_light() +
  ylab("") +
  labs(caption = "Lines around points represent the 99% confidence interval.") +
  xlab("\nPredicted absolute difference\nbetween GPT's prediction and CES model\n") +
  theme(axis.title.x = element_text(hjust = 0.5), legend.position = "none")
  
  
ggsave("_SharedFolder_spsa_gpt_gender/graph/paper_pres/predicted_absolute_diff_visMin.png",
       width = 9, height = 8)


# Predire les biais raw ---------------------------------------------------

GraphRaw <-
  rbind(
    predictions(m_intervention, by = c("gender", "visMin"), conf_level = 0.99) ,
    predictions(m_enviro, by = c("gender", "visMin"), conf_level = 0.99),
    predictions(m_immigr, by = c("gender", "visMin"), conf_level = 0.99)
  ) %>%
  mutate(id = c(
    rep('Government\nintervention', times = 10),
    rep('Environment', times = 10),
    rep('Immigration', times = 10)
  ),
  gender = ifelse(gender == "male", "Men", "Women"))

labels <- data.frame(
  id = c('Government\nintervention',
         'Environment',
         'Immigration'),
  left = c("More State's intervention",
           "Environmental action",
           "Positive view of immigration"),
  right = c("Less State's intervention",
            "Environmental inaction",
            "Negative view of immigration")
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
  scale_color_manual(values = c("#003f5c", "#ff7c43")) +
  scale_fill_manual(values = c("#003f5c", "#ff7c43")) +
  geom_text(data = labels, y = 0, x = -0.35,
            aes(y = 0, label = left),
            size = 3.5) +
  geom_text(data = labels, y = 0, x = 0.35,
            aes(y = 0, label = right),
            size = 3.5) +
  scale_x_continuous(limits = c(-0.4, 0.4)) +
  scale_y_discrete(expand = c(0.50, 0.50)) +
  clessnverse::theme_clean_light() +
  ylab("") +
  labs(caption = "Lines around points represent the 99% confidence interval.\nA point left of 0 suggests GPT predicted a more left-leaning stance than the CES model.") +
  xlab("\nPredicted raw difference\nbetween GPT's prediction and CES model\n") +
  theme(axis.title.x = element_text(hjust = 0.5))

ggsave("_SharedFolder_spsa_gpt_gender/graph/paper_pres/predicted_raw_diff.png",
       width = 12, height = 8)

