df_ces21 <- sondr::read_any_csv("_SharedFolder_spsa_gpt_gender/data/ces2021.csv")

gender <- c("male", "female")
age <- c("18-34 years old", "35-54 years old", "55 years old or more")
education <- c("before highschool", "collegial/technical", "university")
income <- c("less than $40,000", "between $40,001 and $100,000", "more than $100,000")
province <- c("British-Columbia", "Prairies (Alberta, Saskatchewan, Manitoba)", "Atlantic Canada", "Ontario", "Québec", "Canadian Territories")
rurality <- c("urban", "rural")
visMin <- c("White", "Black", "Indigenous", "Asian", "Arab")

ses_df <- expand.grid(gender = gender, age = age, education = education, income = income, province = province, rurality = rurality, visMin = visMin)

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

environment_questions <- c("How much should the federal government spend on the environment?", "To help reduce greenhouse gas emissions, the federal government should continue the carbon tax.", "Environmental regulation should be stricter, even if it leads to consumers having to pay higher prices.")
environment_likerts <- c(spend_likert, likert, likert)
data_environment_questions <- data.frame(environment_questions, environment_likerts) %>% 
  rename(question = environment_questions, likert = environment_likerts)

state_intervention_questions <- c("How much do you think should be done to reduce the gap between the rich and the poor in Canada?", "Is income inequality a big problem in Canada?", "Which of these two options should the government privilege?")
state_intervention_likerts <- c(more_likert, definitely_likert, funky_likert)
data_state_intervention_questions <- data.frame(state_intervention_questions, state_intervention_likerts)  %>% 
  rename(question = state_intervention_questions, likert = state_intervention_likerts)

data_questions <- rbind(data_immigration_questions, data_environment_questions, data_state_intervention_questions)


# Prompting loop ----------------------------------------------------------

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





gpt_answer <- create_chat_completion(
  model = "gpt-4",
  messages = list(
    list(
      "role" = "system",
      "content" = "You are tasked with predicting survey respondents' positions on three specific issues based on their socio-economic status and demographic information. You should provide answers on a Likert scale for each of the three questions. Your response should be formatted as a list, with each item in the list representing the answer to one of the three questions."
    ),
    list(
      "role" = "user",
      "content" = paste0("Given the respondent's details: Gender: ",
                         paste0(ses_df$gender[1]),
                         ", Ethnicity: ",
                         paste0(ses_df$visMin[1]),
                         ", Age group: ", 
                         paste0(ses_df$age[1]),
                         ", Education level: ",
                         paste0(ses_df$education[1]),
                         ", Income category: ",
                         paste0(ses_df$income[1]),
                         ", Residential environment: ",
                         paste0(ses_df$rurality[1]),
                         ", Region of residence: ",
                         paste0(ses_df$province[1]),
                         ". Please predict their positions on the following three issues: ",
                         paste0(data_questions$question[1], ", ", 
                                data_questions$question[2], ", ", 
                                data_questions$question[3]),
                         ". For each issue, choose the most fitting position from the provided likert scale: ", 
                         paste0(toString(data_questions$likert[1]), ", ", 
                                toString(data_questions$likert[2]), ", ",
                                toString(data_questions$likert[3])),
                         ". Format your response as a list of three items, each representing the selected position for one issue.")
    )
  )
)
