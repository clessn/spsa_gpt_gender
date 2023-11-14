library(tidyverse)
library(openai)


#### Data laoding - Raw data CES 21 ####
df_ces21 <- sondr::read_any_csv("_SharedFolder_spsa_gpt_gender/data/ces2021.csv")


####
questions <- c("Environmental regulation should be stricter, even if it leads to consumers having to pay higher prices.")
likert_scale <- c("Strongly agree", "Somewhat agree", "Neither agree nor disagree", "Somewhat disagree", "Strongly disagree")

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
for (i in 86:nrow(sampled_data)) {
gpt_answer <- create_chat_completion(
    model = "gpt-4",
    messages = list(
        list(
            "role" = "system",
            "content" = "You are a helpful assistant. Your role is help a survey analyst in making predictions about respondants' positions on various issues based on their socio-economic status."
        ),
        list(
            "role" = "user",
            "content" = paste0("Please consider this survey respondant: ",
            "Year of birth = ", 
            paste0(df_ces21$cps21_yob[i]),
            ", Education = ",
            paste0(df_ces21$cps21_education[i]),
            ", Income = ",
            paste0(df_ces21$cps21_income_cat[i]),
            ", Province of residence = ",
            paste0(df_ces21$cps21_province[i]),
            ", Religion = ",
            paste0(df_ces21$cps21_religion[i]),
            ", Immigration status = ",
            paste0(df_ces21$cps21_citizenship[i]),
            ". After reviewing this respondant's information, please make a prediction about their position on the following issue: ", paste0(questions[1]), ". Use these answer choices to answer the question: ", paste0(toString(likert_scale)), ". Please only output the answer and nothing else.")
        )
    )
)

print(sampled_data$cps21_pos_envreg[i])
print(gpt_answer$choices$message.content)

sampled_data$cps21_pos_envreg_gpt[i] <- gpt_answer$choices$message.content 
}
