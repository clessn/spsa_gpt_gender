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

modelsummary(models, estimate  = "{estimate} [{std.error}] {stars}", statistic = NULL, gof_map = c("nobs", "r.squared"), stars = TRUE, coef_rename = c("genderfemale" = "Gender Female", "visMinBlack" = "Race Black", "visMinWhite" = "Race White", "visMinIndigenous" = "Race Indigenous", "visMinAsian" = "Race Asian", "visMinArab" = "Race Arab", "provinceBritish-Columbia" = "British-Columbia", "provinceCanadian Territories" = "Canadian Territories", "provinceOntario" = "Ontario", "provincePrairies (Alberta, Saskatchewan, Manitoba)" = "Prairies (Alberta, Saskatchewan, Manitoba)", "provinceQuébec" = "Quebec", "age35-54 years old" = "35-54 years old", "age55 years old or more" = "55+ years old", "educationcollegial/technical" = "College or Technical Education", "educationuniversity" = "University", "incomebetween $40,001 and $100,000" = "Income between $40,001 and $100,000", "incomemore than $100,000" = "Income more than $100 000", "ruralityurban" = "Urban's area of living"),  output = "text.png")

