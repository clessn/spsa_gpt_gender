#### Je veux créer des tables pertinentes pour mieux comprendre mes graphiques ####


# Installation et loading des packages ------------------------------------

install.packages("modelsummary")
install.packages("kableExtra")
install.packages("gt")
install.packages("webshot2")
library(modelsummary)
library(kableExtra)
library(gt)
library(webshot2)

# Créer la liste pour la table --------------------------------------------


models <- list(
  "State's Intervention model" = lm(scale_gd_econo ~
                             gender * visMinBlack +
                             gender * visMinWhite +
                             gender * visMinIndigenous +
                             gender * visMinAsian +
                             gender * visMinArab +
                             age + education + income +
                             province + rurality,
                           data = df_ces21),
  "Environmental model" = lm(scale_enviro ~
                       gender * visMinBlack +
                       gender * visMinWhite +
                       gender * visMinIndigenous +
                       gender * visMinAsian +
                       gender * visMinArab +
                       age + education + income +
                       province + rurality, data = df_ces21),
  "Immigration model" = lm(scale_immigr ~
                       gender * visMinBlack +
                       gender * visMinWhite +
                       gender * visMinIndigenous +
                       gender * visMinAsian +
                       gender * visMinArab +
                       age + education + income +
                       province + rurality, data = df_ces21)
  )

modelsummary(models, gof_map = c("nobs", "r.squared"), stars = TRUE, coef_rename = c("gendermale" = "Male", "visMinBlack" = "Black", "visMinWhite" = "White", "visMinIndigenous" = "Indigenous", "visMinAsian" = "Asian", "visMinArab" = "Arab", "provinceBritish-Columbia" = "British-Columbia", "provinceCanadian Territories" = "Canadian Territories", "provinceOntario" = "Ontario", "provincePrairies (Alberta, Saskatchewan, Manitoba)" = "Prairies (Alberta, Saskatchewan, Manitoba", "provinceQuébec" = "Quebec"), output = "table.png")
