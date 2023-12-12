library(tidyverse)
library(openai)

#### Data laoding - Raw data CES 21 ####
df_ces21 <- sondr::read_any_csv("_SharedFolder_spsa_gpt_gender/data/ces2021.csv")

likert <- "Strongly agree, Somewhat agree, Neither agree nor disagree, Somewhat disagree, Strongly disagree"
spend_likert <- "Spend less, Spend about the same as now, Spend more, Don't know/Prefer not to answer"
yesno_likert <- "Yes, No, Don't know/Prefer not to answer"
more_likert <- "Much more, Somewhat more, About the same as now, Somewhat less, Much less, Don't know/Prefer not to answer"
definitely_likert <- "Definitely yes, Probably yes, Not sure, Probably not, Definitely not, Don't know/Prefer not to answer"
funky_likert <- "See to it that everyone has a decent standard of living, Leave people to get ahead on their own, Don’t know/ Prefer not to answer"

####
immigration_questions <- c("How much should the federal government spend on immigrants and minorities?", "Too many recent immigrants just don't want to fit in to Canadian society.", "Immigrants take jobs away from other Canadians.")
immigration_likerts <- c(spend_likert, likert, likert)
data_immigration_questions <- data.frame(immigration_questions, immigration_likerts) %>% 
    rename(question = immigration_questions, likert = immigration_likerts)

environment_questions <- c("How much should the federal government spend on the environment?", "To help reduce greenhouse gas emissions, the federal government should continue the carbon tax.", "The federal government should do more to help Canada’s energy sector, including building oil pipelines.", "Environmental regulation should be stricter, even if it leads to consumers having to pay higher prices.", "When there is a conflict between protecting the environment and creating jobs, jobs should come first.", "Do you think that climate change is happening?")
environment_likerts <- c(spend_likert, likert, likert, likert, likert, yesno_likert)
data_environment_questions <- data.frame(environment_questions, environment_likerts) %>% 
    rename(question = environment_questions, likert = environment_likerts)

state_intervention_questions <- c("How much do you think should be done to reduce the gap between the rich and the poor in Canada?", "Is income inequality a big problem in Canada?", "Which of these two options should the government privilege?")
state_intervention_likerts <- c(more_likert, definitely_likert, funky_likert)
data_state_intervention_questions <- data.frame(state_intervention_questions, state_intervention_likerts)  %>% 
    rename(question = state_intervention_questions, likert = state_intervention_likerts)

data_questions <- rbind(data_immigration_questions, data_environment_questions, data_state_intervention_questions)

#### OpenAi loop testing ####
#### Test loop avec 1 seul répondant ####

complete_data <- df_ces21[complete.cases(df_ces21$cps21_yob, 
                                         df_ces21$cps21_education, 
                                         df_ces21$cps21_income_cat,
                                         df_ces21$cps21_province, 
                                         df_ces21$cps21_religion, 
                                         df_ces21$cps21_citizenship,
                                         df_ces21$cps21_pos_envreg), ]

# Randomly sample 100 respondents from the subset of complete cases
set.seed(42069)  # Setting a seed to make the sample reproducible
df_ces21$cps21_pos_envreg_gpt <- NA

sampled_data <- complete_data[sample(nrow(complete_data), 100), ]  %>% 
    select(cps21_yob, 
           cps21_education, 
           cps21_income_cat, 
           cps21_province, 
           cps21_religion, 
           cps21_citizenship, 
           cps21_pos_envreg,
           cps21_pos_envreg_gpt,
           cps21_genderid)

# ----------------------- Defining SES -----------------------------------------

df_test <- data.frame(matrix(NA, ncol = 10, nrow = 10))
colnames(df_test) <- paste("run", 1:10, sep = "")

j <- 5
i <- 4

for (i in seq_along(data_questions$question)) {
     for (i in 1:nrow(df_ces21)) {
       
        gpt_answer <- create_chat_completion(
            model = "gpt-4",
            messages = list(
                list(
                    "role" = "system",
                    "content" = "You are tasked with predicting survey respondents' positions on various issues. Your responses should be limited to selections from a given Likert scale based on respondents' socio-economic status and demographic information."
                ),
                list(
                    "role" = "user",
                    "content" = paste0("Given the respondent's details: Year of birth: ", 
                    paste0(df_ces21$cps21_yob[j]),
                    ", Education level: ",
                    paste0(df_ces21$cps21_education[j]),
                    ", Income category: ",
                    paste0(df_ces21$cps21_income_cat[j]),
                    ", Province of residence: ",
                    paste0(df_ces21$cps21_province[j]),
                    ", Religious affiliation: ",
                    paste0(df_ces21$cps21_religion[j]),
                    ", Immigration status: ",
                    paste0(df_ces21$cps21_citizenship[j]),
                    ", choose the most fitting position from the provided likert scale on the following issue: ", 
                    paste0(data_questions$question[i]), 
                    ". Answer with only one of these options: ", 
                    paste0(toString(data_questions$likert[i])), 
                    ". Provide only your selected position as a response.")
                )
            )
        )


print(sampled_data$cps21_pos_envreg[i])
print(gpt_answer$choices$message.content)

df_test[i, column_name] <- gpt_answer$choices$message.content

        Sys.sleep(3)
    }
}

# Immigration :
# cps21_spend_imm_min(Raw)/issGovSpendImmigr21(Clean) How much should the federal government spend on immigrants and minorities? (Spend less, Spend about the same as now, Spend more, Don't know/Prefer not to answer)

# pes21_fitin(Raw)/issintégrationImmigr21(Clean) Un trop grand nombre d’immigrants récents ne veulent tout simplement pas s'intégrer. (Likert)

# pes21_immigjobs(Raw)/issImmigrEnleveJobs21(Clean) Les immigrants enlèvent des emplois aux autres Canadiens. (Likert)

# Environnement :

# cps21_spend_env(Raw)/issSpendEnviro21(Clean) How much should the federal government spend on the environment? (Spend less, Spend about the same as now, Spend more, Don't know/Prefer not to answer)

# cps21_pos_carbon(raw)/issTaxeCarbone21(Clean) Afin d'aider à réduire les émissions de gaz à effet de serre, le gouvernement fédéral devrait maintenir la taxe sur le carbone. (Likert)

# cps21_pos_energy(Raw)/issConstructionOleoducs21(Clean) Le gouvernement fédéral devrait en faire davantage afin d'aider le secteur énergétique canadien, notamment en construisant des oléoducs. (Likert)

# cps21_pos_envreg(Raw)/issReglEnviroPrix21(Clean) La réglementation environnementale devrait être plus stricte, même si elle oblige les consommateurs à payer des prix plus élevés. (Likert)

# cps21_pos_jobs(Raw)/issEnviroJob21(Clean) Lorsqu'il existe un conflit entre la protection de l'environnement et la création d'emplois, les emplois devraient avoir la priorité (Likert)

# pes21_cc1(Raw)/issChangeClim21(Clean) Pensez-vous que les changements climatiques se produisent réellement? (Oui, Non, Je ne sais pas/Préfère ne pas répondre)

# Intervention de l'État :

# pes21_gap(Raw)/issGapRichPoor21(Clean) Que devrait-on faire pour réduire les écarts entre les riches et les pauvres au Canada? (Beaucoup plus, Un peu plus, Ni plus ni moins, Un peu moins, Beaucoup moins, Je ne sais pas / Préfère ne pas répondre)

# pes21_inequal(Raw)/ issProbInegality21(Clean) Est-ce que les inégalités de revenu sont un problème important au Canada? (Définitivement oui, Probablement oui, Par certain(e), Probablement pas, Définitivement pas, Je ne sais / Préfère ne pas répondre)

# pes21_stdofliving(Raw)/issGovShouldDoStdOfLiving21(Clean) Le gouvernement devrait: (Voir à ce que tout le monde ait un niveau de vie décent / Laisser les gens avancer par eux-mêmes / Je ne sais pas / Préfère ne pas répondre)


To help reduce greenhouse gas emissions, the federal government should continue the carbon tax.
The federal government should do more to help Canada’s energy sector, including building oil pipelines.
When there is a conflict between protecting the environment and creating jobs, jobs should come first.

Do you think that climate change is happening?

How much do you think should be done to reduce the gap between the rich and the poor in Canada?
Is income inequality a big problem in Canada?

The government should: See to it that everyone has a decent standard of living (1) Leave people to get ahead on their own (2) Don’t know/ Prefer not to answer (3)