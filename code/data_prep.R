library(tidyverse)
library(openai)

df_ces21 <- sondr::read_any_csv("_SharedFolder_spsa_gpt_gender/data/ces2021.csv")

gender <- c("male", "female")
age <- c("18-34 years old", "35-54 years old", "55 years old or more")
education <- c("before highschool", "collegial/technical", "university")
income <- c("less than $40,000", "between $40,001 and $100,000", "more than $100,000")
province <- c("British-Columbia", "Prairies (Alberta, Saskatchewan, Manitoba)", "Atlantic Canada", "Ontario", "Québec", "Canadian Territories")
rurality <- c("urban", "rural")
visMin <- c("White", "Black", "Indigenous", "Asian", "Arab")

df_ses <- expand.grid(gender = gender, age = age, education = education, income = income, province = province, rurality = rurality, visMin = visMin)
saveRDS(df_ses, "_SharedFolder_spsa_gpt_gender/data/questions/df_ses.rds")

likert <- "Strongly agree, Somewhat agree, Neither agree nor disagree, Somewhat disagree, Strongly disagree"
spend_likert <- "Spend less, Spend about the same as now, Spend more, Don't know/Prefer not to answer"
yesno_likert <- "Yes, No, Don't know/Prefer not to answer"
more_likert <- "Much more, Somewhat more, About the same as now, Somewhat less, Much less, Don't know/Prefer not to answer"
definitely_likert <- "Definitely yes, Probably yes, Not sure, Probably not, Definitely not, Don't know/Prefer not to answer"
funky_likert <- "See to it that everyone has a decent standard of living, Leave people to get ahead on their own, Don’t know/ Prefer not to answer"
.
####
immigration_questions <- c("How much should the federal government spend on immigrants and minorities?", "Too many recent immigrants just don't want to fit in to Canadian society", "Immigrants take jobs away from other Canadians.")
immigration_likerts <- c(spend_likert, likert, likert)
df_immigration_questions <- data.frame(immigration_questions, immigration_likerts) %>% 
  rename(question = immigration_questions, likert = immigration_likerts)

environment_questions <- c("How much should the federal government spend on the environment?", "To help reduce greenhouse gas emissions, the federal government should continue the carbon tax.", "Environmental regulation should be stricter, even if it leads to consumers having to pay higher prices.")
environment_likerts <- c(spend_likert, likert, likert)
df_environment_questions <- data.frame(environment_questions, environment_likerts) %>% 
  rename(question = environment_questions, likert = environment_likerts)

state_intervention_questions <- c("How much do you think should be done to reduce the gap between the rich and the poor in Canada?", "Is income inequality a big problem in Canada?", "Which of these two options should the government privilege?")
state_intervention_likerts <- c(more_likert, definitely_likert, funky_likert)
df_state_intervention_questions <- data.frame(state_intervention_questions, state_intervention_likerts)  %>% 
  rename(question = state_intervention_questions, likert = state_intervention_likerts)

df_questions <- rbind(df_immigration_questions, df_environment_questions, df_state_intervention_questions)
saveRDS(df_questions, "_SharedFolder_spsa_gpt_gender/data/questions/df_question.rds")


