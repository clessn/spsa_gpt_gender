# Packages to install -----------------------------------------------------
#install.packages("psych")

# Loading packages --------------------------------------------------------
library(tidyverse)

# Loading data ------------------------------------------------------------
CES21 <- read.csv ("_SharedFolder_spsa_gpt_gender/data/CES21_CleanData_2024-01-07.csv")

# Custom functions --------------------------------------------------------
topdown_fa <- function(df, nfactors = 1) {
  # Cronbach's alpha (Test 1)
  cronbachAlpha <<- round(psych::alpha(df)$total$raw_alpha, 2)
  
  # Analyse factorielle (Test 2)
  
  factAnalysis <- factanal(df, factors=nfactors) # Analyse factorielle
  factorVarNames <- names(df)
  
  factorLoadings <- as.numeric(factAnalysis$loadings[,1]) 
  factor1stEigen <<- round(eigen(cor(df))$values[1], digit=2)
  
  
  FAplot <- ggplot(data.frame(factorVarNames,factorLoadings), 
                   aes(x=factorVarNames, y=factorLoadings)) + 
    coord_flip() +
    geom_bar(stat="identity", colour="black", fill="black", size=1, width=0.5) +
    geom_text(aes(label=as.character(round(factorLoadings, 
                                           digits = 2))), vjust=0.35, hjust=-0.3, size = 5) +
    geom_hline(yintercept=0.3, colour="gray", linetype = "longdash") +
    annotate("text", label=paste("Alpha de Cronbach =", as.character(cronbachAlpha)), 
             x=1.1, y=1.28, size=5) +
    annotate("text", label=paste("Première Valeur Propre =", as.character(factor1stEigen)), 
             x=0.75, y=1.28, size=5) +
    annotate("segment", x = 0.4, xend = 1.45, 
             y = 1, yend = 1, colour = "black") +
    annotate("segment", x = 1.45, xend = 1.45, 
             y = 1, yend = Inf, colour = "black") +
    scale_y_continuous(name="\n Saturation Factorielle \n", 
                       limits=c(0, 1.55), breaks=seq(0, 1, by=0.1),
                       expand = c(0,0)) +
    xlab("\n") + 
    theme_linedraw() +
    theme(axis.text.y = element_text(size=15,
                                     margin = margin(r = 5, l = 3)), 
          axis.title.y = element_text(size = 15), 
          axis.text.x = element_text(size = 15),
          axis.title.x = element_text(hjust=0.3, vjust=-0.17, size=20), 
          panel.grid=element_blank())
  print(FAplot)
  print("What we want:")
  print(paste0("Cronbach Alpha > 0.6 -> ",cronbachAlpha))
  print(paste0("First eigenvalue > 1 -> ",factor1stEigen))
  print(paste0("Factor loadings > 0.3"))
}

# Tester les questions : Gauche-droite économique

df_gd_econo <- CES21 %>% 
  select(issStopSubv21, issGapRichPoor21, issProbInegality21, issGovShouldDoStdOfLiving21) %>%
  drop_na() %>%
  rename(
    "The federal government should \n end all corporate and economic\n development subsidies." = issStopSubv21,
    "How much do you think should \n be done to reduce the gap between\n the rich and the poor in Canada?" = issGapRichPoor21,
    "Is income inequality a \n big problem in Canada?" = issProbInegality21,
    "The government should:\nSee to it that everyone has a \n decent standard of living;\n or leave people to \n get ahead on their own;" = issGovShouldDoStdOfLiving21
  )

factor_analysis_intervention <- topdown_fa(df_gd_econo)

ggsave("_SharedFolder_spsa_gpt_gender/graph/paper_pres/factor_analysis_intervention.png", width = 14, height = 10)

df_gd_econo_fr <- CES21 %>% 
  select(issStopSubv21, issGapRichPoor21, issProbInegality21, issGovShouldDoStdOfLiving21) %>%
  drop_na() %>%
  rename(
    "Le gouvernement fédéral devrait\nmettre fin à toutes les subventions\nau développement des\nentreprises et de l'économie." = issStopSubv21,
    "Que devrait-on faire pour\nréduire les écarts entre les\nriches et les pauvres au Canada?" = issGapRichPoor21,
    "Est-ce que les inégalités\nde revenu sont un problème\nimportant au Canada?" = issProbInegality21,
    "Le gouvernement devrait:\nVoir à ce que tout le monde ait\nun niveau de vie décent ou\nlaisser les gens avancer par eux-mêmes" = issGovShouldDoStdOfLiving21
  )

factor_analysis_intervention_fr <- topdown_fa(df_gd_econo_fr)

ggsave("_SharedFolder_spsa_gpt_gender/graph/paper_pres/factor_analysis_intervention_fr.png", width = 14, height = 10)

# Tester les questions : Environnement

df_enviro <- CES21 %>% 
  select(issSpendEnviro21, issTaxeCarbone21, issConstructionOleoducs21, issReglEnviroPrix21, issEnviroJob21, issChangeClim21) %>% 
  drop_na() %>% 
  rename( "How much should \n the federal government \n spend on the environment?" = issSpendEnviro21, "To help reduce greenhouse gas \n emissions, the federal government \n should continue the carbon tax." = issTaxeCarbone21, "The federal government should \n do more to help Canada’s energy \n sector, including building \n oil pipelines." = issConstructionOleoducs21, "Environmental regulation should \n be stricter, even if it \n leads to consumers having \n to pay higher prices." = issReglEnviroPrix21, "When there is a conflict between \n protecting the environment and \ncreating jobs, jobs should come first." = issEnviroJob21, "Do you think \n that climate change \n is happening?" = issChangeClim21)

factor_analysis_enviro <- topdown_fa(df=df_enviro)

ggsave("_SharedFolder_spsa_gpt_gender/graph/paper_pres/factor_analysis_enviro.png", width = 14, height = 10)

#fr

df_enviro_fr <- CES21 %>% 
  select(issSpendEnviro21, issTaxeCarbone21, issConstructionOleoducs21, issReglEnviroPrix21, issEnviroJob21, issChangeClim21) %>% 
  drop_na() %>% 
  rename( "Combien le gouvernement fédéral devrait-il\ndépenser en environnement?" = issSpendEnviro21, "Afin d'aider à réduire les émissions\nde gaz à effet de serre, le gouvernement\nfédéral devrait maintenir la taxe sur le carbone." = issTaxeCarbone21, "Le gouvernement fédéral devrait\nen faire davantage afin d'aider\nle secteur énergétique canadien,\nnotamment en construisant des oléoducs." = issConstructionOleoducs21, "La réglementation environnementale devrait\nêtre plus stricte, même si elle\noblige les consommateurs à payer\ndes prix plus élevés." = issReglEnviroPrix21, "Lorsqu'il existe un conflit\nentre la protection de l'environnement et\nla création d'emplois, les\nemplois devraient avoir la priorité." = issEnviroJob21, "Pensez-vous que les changements\nclimatiques se produisent réellement?" = issChangeClim21)

factor_analysis_enviro_fr <- topdown_fa(df=df_enviro_fr)

ggsave("_SharedFolder_spsa_gpt_gender/graph/paper_pres/factor_analysis_enviro_fr.png", width = 14, height = 10)

# Tester les questions : Immigration

df_immigr <- CES21 %>% 
  select(issGovSpendImmigr21, issNumberImmigr21, issNumberRefugees21, issIntégrationImmigr21, issImmigrEnleveJobs21) %>% 
  drop_na() %>% 
  rename("How much should \n the federal government \n spend on immigrants \n and minorities?" = issGovSpendImmigr21, "Do you think Canada should admit: \n More immigrants; \n Fewer immigrants; \n About the same number \n of immigrants as now" = issNumberImmigr21, "Do you think Canada should admit: \n More refugees;\n Fewer refugees; \n About the same number \n of refugees as now" = issNumberRefugees21, "Too many recent immigrants \n just don't want to fit \n in to Canadian society." = issIntégrationImmigr21, "Immigrants take jobs \n away from other Canadians." = issImmigrEnleveJobs21)

factor_analysis_immigr <- topdown_fa(df=df_immigr)

ggsave("_SharedFolder_spsa_gpt_gender/graph/paper_pres/factor_analysis_immigr.png", width = 14, height = 10)

#fr

df_immigr_fr <- CES21 %>% 
  select(issGovSpendImmigr21, issNumberImmigr21, issNumberRefugees21, issIntégrationImmigr21, issImmigrEnleveJobs21) %>% 
  drop_na() %>% 
  rename("Combien le gouvernement fédéral\ndevrait-il dépenser pour les\nimmigrants et les minorités?" = issGovSpendImmigr21, "Pensez-vous que le\nCanada devrait admettre:\nPlus d'immigrants;\nMoins d'immigrants;\nÀ peu près le même\nnombre d'immigrants" = issNumberImmigr21, "Pensez-vous que le\nCanada devrait admettre:\nPlus de réfugiés;\nMoins de réfugiés;\nÀ peu près le même\nnombre de réfugiés" = issNumberRefugees21, "Un trop grand nombre d’immigrants\nrécents ne veulent tout simplement\npas s'intégrer." = issIntégrationImmigr21, "Les immigrants enlèvent\ndes emplois aux autres Canadiens." = issImmigrEnleveJobs21)

factor_analysis_immigr_fr <- topdown_fa(df=df_immigr_fr)

ggsave("_SharedFolder_spsa_gpt_gender/graph/paper_pres/factor_analysis_immigr_fr.png", width = 14, height = 10)

# Échelle Gauche-droite économique ####

## create a vector containing the number of questions answered by each respondent in the columns making the scale
nnas_gd_econo <- (1 - is.na(CES21$issProbInegality21)) +
  (1 - is.na(CES21$issGovShouldDoStdOfLiving21)) +
  (1 - is.na(CES21$issGapRichPoor21))

## 1 - is.na() ??
is.na(5)
is.na(NA)
TRUE + 2
FALSE + 1
FALSE + 2
1 - FALSE
1 - TRUE
## Donc:
(1 - is.na(5)) + (1 - is.na(NA)) + (1 - is.na(2)) ## 2 réponses sur 3

## if is.na() == TRUE --> 0. Anyway, après on divise par le nombre de réponses complètes.
CES21$scale_gd_econo <- (ifelse(is.na(CES21$issProbInegality21), 0, CES21$issProbInegality21) +
                           ifelse(is.na(CES21$issGovShouldDoStdOfLiving21), 0, CES21$issGovShouldDoStdOfLiving21) +
                           ifelse(is.na(CES21$issGapRichPoor21), 0, CES21$issGapRichPoor21)) / nnas_gd_econo

## pourquoi?
(NA + 2 + 4) / 2
(0 + 2 + 4) / 2
(2 + 4) / 2 ## même résultat

# Remplacer les valeurs indéfinies (NaN) par NA dans le cas où nnas_gd_econo est égal à 0 (aucune réponse)
CES21$scale_gd_econo[is.nan(CES21$scale_gd_econo)] <- NA

hist(CES21$scale_gd_econo)
clessnverse::count_na(CES21$scale_gd_econo)

# Échelle Environnement ####

nnas_enviro <- (1 - is.na(CES21$issTaxeCarboneII21)) +
               (1 - is.na(CES21$issTaxeCarbone21)) +
               (1 - is.na(CES21$issSpendEnviro21)) +
               (1 - is.na(CES21$issReglEnviroPrix21)) +
               (1 - is.na(CES21$issEnviroJob21)) +
               (1 - is.na(CES21$issConstructionOleoducs21)) +
               (1 - is.na(CES21$issChangeClim21))

CES21$scale_enviro <- (ifelse(is.na(CES21$issTaxeCarboneII21), 0, CES21$issTaxeCarboneII21) +
                           ifelse(is.na(CES21$issTaxeCarbone21), 0, CES21$issTaxeCarbone21) +
                           ifelse(is.na(CES21$issSpendEnviro21), 0, CES21$issSpendEnviro21) +
                           ifelse(is.na(CES21$issReglEnviroPrix21), 0, CES21$issReglEnviroPrix21) +
                           ifelse(is.na(CES21$issEnviroJob21), 0, CES21$issEnviroJob21) +
                           ifelse(is.na(CES21$issConstructionOleoducs21), 0, CES21$issConstructionOleoducs21) +
                           ifelse(is.na(CES21$issChangeClim21), 0, CES21$issChangeClim21)) / nnas_enviro

hist(CES21$scale_enviro)
clessnverse::count_na(CES21$scale_enviro)

# Échelle Immigration ####

# Calculer le nombre de réponses non manquantes pour chaque répondant
nnas_immigr <- (1 - is.na(CES21$issGovSpendImmigr21)) +
  (1 - is.na(CES21$issImmigrEnleveJobs21)) +
  (1 - is.na(CES21$issIntégrationImmigr21))
table(nnas_immigr)

# Calculer l'échelle en traitant les NA comme des zéros
CES21$scale_immigr <- (ifelse(is.na(CES21$issGovSpendImmigr21), 0, CES21$issGovSpendImmigr21) +
                         ifelse(is.na(CES21$issImmigrEnleveJobs21), 0, CES21$issImmigrEnleveJobs21) +
                         ifelse(is.na(CES21$issIntégrationImmigr21), 0, CES21$issIntégrationImmigr21)) / nnas_immigr

# Remplacer les valeurs indéfinies (NaN) par NA
CES21$scale_immigr[is.nan(CES21$scale_immigr)] <- NA

hist(CES21$scale_immigr)
clessnverse::count_na(CES21$scale_immigr)

