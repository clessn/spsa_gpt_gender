# Packages to install -----------------------------------------------------
#install.packages("psych")

# Loading packages --------------------------------------------------------
library(tidyverse)

# Loading data ------------------------------------------------------------
CES21 <- read.csv ("_SharedFolder_spsa_gpt_gender/data/CES21_CleanData_2023-11-20.csv")

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
    annotate("text", label=paste("Première valeur propre =", as.character(factor1stEigen)), 
             x=0.75, y=1.28, size=5) +
    annotate("segment", x = 0.4, xend = 1.45, 
             y = 1, yend = 1, colour = "black") +
    annotate("segment", x = 1.45, xend = 1.45, 
             y = 1, yend = Inf, colour = "black") +
    scale_y_continuous(name="\n Coefficients de saturation \n", 
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
  print(paste0("Alpha de Cronbach > 0.6 -> ",cronbachAlpha))
  print(paste0("Première Valeur Propre > 1 -> ",factor1stEigen))
  print(paste0("Tous les coefficients de saturation > 0.3"))
}

# Tester les questions : Gauche-droite économique

df_gd_econo <- CES21 %>% 
  select(issStopSubv21, issGapRichPoor21, issProbInegality21, issGovShouldDoStdOfLiving21) %>%
  drop_na()

topdown_fa(df_gd_econo)

ggplot(mtcars, aes(x = mpg)) + 
  geom_histogram()

# Tester les questions : Environnement

df_enviro <- CES21 %>% 
  select(issSpendEnviro21, issTaxeCarbone21, issConstructionOleoducs21, issReglEnviroPrix21, issEnviroJob21, issTaxeCarboneII21, issChangeClim21) %>% 
  drop_na()

topdown_fa(df=df_enviro)

# Tester les questions : Immigration

df_immigr <- CES21 %>% 
  select(issGovSpendImmigr21, issNumberImmigr21, issNumberRefugees21, issIntégrationImmigr21, issImmigrEnleveJobs21) %>% 
  drop_na()

topdown_fa(df=df_immigr)

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
