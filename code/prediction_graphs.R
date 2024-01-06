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

GraphAbs <-
  rbind(
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
  left = c("More intervention",
           "Environmental action",
           "Less immigration"),
  right = c("Less intervention",
            "Environmental inaction",
            "More immigration")
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
  scale_y_discrete(expand = c(0.25, 0.25)) +
  clessnverse::theme_clean_light() +
  ylab("") +
  labs(caption = "Lines around points represent the 99% confidence interval.\nA point left of 0 suggests GPT predicted a more left-leaning stance than the CES model.") +
  xlab("\nPredicted raw difference\nbetween GPT's prediction and CES model\n") +
  theme(axis.title.x = element_text(hjust = 0.5))

ggsave("_SharedFolder_spsa_gpt_gender/graph/paper_pres/predicted_raw_diff.png",
       width = 9, height = 8)
