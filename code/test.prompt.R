library(tidyverse)
library(openai)


#### Data laoding - Raw data CES 21 ####
df_ces21 <- sondr::read_any_csv("_SharedFolder_spsa_gpt_gender/data/ces2021.csv")

####

#### OpenAi loop testing ####
#### Test loop avec 1 seul répondant ####

# ----------------------- Defining SES -----------------------------------------
ses_variables <- create_chat_completion(
    model = "gpt-4",
    messages = list(
        list(
            "role" = "system",
            "content" = "You are a helpful assistant. Your role is to help a R programmer analyse survey data and output only vectors containing socio-economic status variables."
        ),
        list(
            "role" = "user",
            "content" = paste0("Please analyse all of these survey variable names carefully: ", 
            paste0(names(data), collapse = ", "), 
            ". I want you to return a single vector containing all of the socio-economic status variables (SES) in the following format: c(variable 1, variable 2, variable 3, variable 4, variable 5, variable 6, variable n). Please don't output anything else than the vector. I repeat, make sure you only return the vector containing every SES. Also, please include every single SES variable in the vector. Don't forget to include any SES.")
        )
    )
)

