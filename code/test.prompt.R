library(tidyverse)
library(openai)
library(stringr)

df_ces21 <- sondr::read_any_csv("_SharedFolder_spsa_gpt_gender/data/ces2021.csv")
df_ses <- readRDS("_SharedFolder_spsa_gpt_gender/data/questions/df_ses.rds")
df_questions <- readRDS("_SharedFolder_spsa_gpt_gender/data/questions/df_question.rds")

questions_id <- c("fed_spend", "recent_immi", "immi_take_jobs", "reduce_emi", 
                  "cont_carb_tax", "enviro_reg", "rich_gap", "ineq_prob", "option_priv")

df_questions$questions_id <- questions_id

for(question_id in df_questions$questions_id) {
  df_ses[[question_id]] <- NA
}
                         
# Loop through df_questions and df_ses
for (i in seq(1, nrow(df_questions), by = 3)) {
    for (j in 1:nrow(df_ses)) {
        attempts <- 0
        repeat {
            attempts <- attempts + 1
            # Create GPT API call
            gpt_answer <- create_chat_completion(
                model = "gpt-4",
                messages = list(
                    list(
                        "role" = "system",
                        "content" = "You are a helpful assistant"
                    ),
                    list(
                        "role" = "user",
                        "content" = paste0("Given the following Canadian survey respondent's details: Gender: ",
                                           paste0(df_ses$gender[j]),
                                           ", Ethnicity: ",
                                           paste0(df_ses$visMin[j]),
                                           ", Age group: ", 
                                           paste0(df_ses$age[j]),
                                           ", Education level: ",
                                           paste0(df_ses$education[j]),
                                           ", Income category: ",
                                           paste0(df_ses$income[j]),
                                           ", Residential environment: ",
                                           paste0(df_ses$rurality[j]),
                                           ", Region of residence: ",
                                           paste0(df_ses$province[j]),
                                           ". Please predict their positions on the following three issues: ",
                                           paste0(df_questions$question[i], ", ", 
                                                  df_questions$question[i + 1], ", ", 
                                                  df_questions$question[i + 2]),
                                           ". For each issue, choose the most fitting position from the provided likert scale: ", 
                                           paste0(toString(df_questions$likert[i]), ", ", 
                                                  toString(df_questions$likert[i + 1]), ", ",
                                                  toString(df_questions$likert[i + 2])),
                                           ". Format your answer as a list containing only the three positions, in the same order as the questions. Nothing else. Don't use brackets, don't use quotation marks, don't use commas, don't use spaces, don't use dashes.")
                    )
                )
            )

            # Process the GPT response
            responses <- gpt_answer$choices$message.content
            parts <- strsplit(responses, ", ")[[1]]

            if (length(parts) == 3) {
                df_ses[j, df_questions$questions_id[i]] <- parts[1]
                df_ses[j, df_questions$questions_id[i + 1]] <- parts[2]
                df_ses[j, df_questions$questions_id[i + 2]] <- parts[3]
                break  # Exit the repeat loop
            } else {
                warning("Unexpected response format: ", responses)
                # The repeat loop will continue since the condition is not met
                if (attempts >= 3) {
                    break  # Exit the repeat loop after 3 attempts
                }
            }
        }

        # Print completion of current iteration
        cat("Completed: i =", i, ", j =", j, "\n")

        # Wait for 2 seconds to avoid API rate limits
        Sys.sleep(2)

        # Save progress
        saveRDS(df_ses, "_SharedFolder_spsa_gpt_gender/data/questions/df_ses_gpt.rds")
    }
}

write.csv(df_ses,"_SharedFolder_spsa_gpt_gender/data/questions/df_ses_gpt.csv")
saveRDS(df_ses, "_SharedFolder_spsa_gpt_gender/data/questions/df_ses_gpt2023.rds")