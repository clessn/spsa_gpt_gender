library(tidyverse)
library(openai)
library(stringr)

df_ces21 <- sondr::read_any_csv("_SharedFolder_spsa_gpt_gender/data/ces2021.csv")
df_ses <- readRDS("_SharedFolder_spsa_gpt_gender/data/questions/df_ses.rds")
df_questions <- readRDS("_SharedFolder_spsa_gpt_gender/data/questions/df_question.rds")

df_results <- data.frame(Answer1 = character(), 
                         Answer2 = character(), 
                         Answer3 = character(), 
                         stringsAsFactors = FALSE)

for(i in 1:nrow(df_questions)) {
  for (j in 1:nrow(df_ses)) {

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

responses <- gpt_answer$choices$message.content

for (response in responses) {
    # Split the response into three parts at the commas
    parts <- strsplit(response, ", ")[[1]]
    
    # Check if the response contains exactly three parts
    if (length(parts) == 3) {
        # Append the parts to the dataframe
        df_results <- rbind(df_results, data.frame(Answer1 = parts[1], 
                                                   Answer2 = parts[2], 
                                                   Answer3 = parts[3]))
    } else {
        # Handle cases where the response does not fit the expected format
        warning("Unexpected response format: ", response)
    }
}

i <- i + 3
print (i)
}

}
