library(tidyverse)
library(openai)


#### Data laoding - Raw data CES 21 ####
df_ces21 <- sondr::read_any_csv("_SharedFolder_spsa_gpt_gender/data/ces2021.csv")


####
questions <- c("Environmental regulation should be stricter, even if it leads to consumers having to pay higher prices.", "Too many recent immigrants just don't want to fit in to Canadian society.")
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

df_test <- data.frame(matrix(NA, ncol = 10, nrow = 10))
colnames(df_test) <- paste("run", 1:10, sep = "")

for (j in 1:10) {
     column_name <- paste("run", j, sep = "")
    for (i in 1:10) {
       
gpt_answer <- create_chat_completion(
    model = "gpt-4",
    messages = list(
        list(
            "role" = "system",
            "content" = "You are an advanced AI tasked with predicting survey respondents' positions on various issues. Your responses should be limited to selections from a given Likert scale based on respondents' socio-economic status and demographic information."
        ),
        list(
            "role" = "user",
            "content" = paste0("Given the respondent's details: Year of birth: ", 
            paste0(df_ces21$cps21_yob[i]),
            ", Education level: ",
            paste0(df_ces21$cps21_education[i]),
            ", Income category: ",
            paste0(df_ces21$cps21_income_cat[i]),
            ", Province of residence: ",
            paste0(df_ces21$cps21_province[i]),
            ", Religious affiliation: ",
            paste0(df_ces21$cps21_religion[i]),
            ", Immigration status: ",
            paste0(df_ces21$cps21_citizenship[i]),
            ", choose the most fitting position from the Likert scale on the issue: ", 
            paste0(questions[2]), 
            ". Use only these options: ", 
            paste0(toString(likert_scale)), 
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
