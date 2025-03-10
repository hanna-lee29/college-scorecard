library(httr)
library(jsonlite)

api_key <-Sys.getenv("API_KEY")
base_url <-"https://api.data.gov/ed/collegescorecard/v1/schools?"

vars <- paste("id",
              "school.name",
              "latest.student.size",
              "latest.student.retention_rate.four_year.full_time_pooled",
              "latest.student.retention_rate.lt_four_year.full_time_pooled",
              "latest.student.retention_rate.four_year.part_time_pooled",
              "latest.student.retention_rate.lt_four_year.part_time_pooled",
              "latest.completion.completion_rate_4yr_150_white",
              "latest.completion.completion_rate_4yr_150_black",
              "latest.completion.completion_rate_4yr_150_hispanic",
              "latest.completion.completion_rate_4yr_150_asian",
              "latest.completion.completion_rate_4yr_150_aian",
              "latest.completion.completion_rate_4yr_150_nhpi",
              "latest.completion.completion_rate_4yr_150_2ormore",
              "latest.completion.completion_rate_4yr_150_nonresident.alien",
              "latest.completion.completion_rate_4yr_150_race.unknown",
              "latest.completion.completion_rate_l4yr_150_white",
              "latest.completion.completion_rate_l4yr_150_black",
              "latest.completion.completion_rate_l4yr_150_hispanic",
              "latest.completion.completion_rate_l4yr_150_asian",
              "latest.completion.completion_rate_l4yr_150_aian",
              "latest.completion.completion_rate_l4yr_150_nhpi",
              "latest.completion.completion_rate_l4yr_150_2ormore",
              "latest.completion.completion_rate_l4yr_150_nonresident.alien",
              "latest.completion.completion_rate_l4yr_150_race.unknown",
              "latest.completion.completion_rate_4yr_150nt_pooled",
              "latest.completion.completion_rate_less_than_4yr_150nt_pooled",
              sep = ",")

all_data <- list()
page <-1
for (page in 1:100)
{
  url <- paste0(base_url,"api_key=",api_key,"&fields=",vars,"&page=",page,"&per_page=100")
  response <- GET(url)
  data <- fromJSON(content(response,"text",encoding="UTF-8"), flatten = TRUE)$results
  if(any(is.null(data$id))){
    
    break
  }
  all_data <- c(all_data,list(data))
  page <- page +1
}

final_df <- do.call(rbind,lapply(all_data, as.data.frame))

outputs <- list()

completion_columns <- c(
  "latest.completion.completion_rate_4yr_150_white",
  "latest.completion.completion_rate_4yr_150_black",
  "latest.completion.completion_rate_4yr_150_hispanic",
  "latest.completion.completion_rate_4yr_150_asian",
  "latest.completion.completion_rate_4yr_150_aian",
  "latest.completion.completion_rate_4yr_150_nhpi",
  "latest.completion.completion_rate_4yr_150_2ormore",
  "latest.completion.completion_rate_4yr_150_nonresident.alien",
  "latest.completion.completion_rate_4yr_150_race.unknown"
)

for (column in completion_columns) {
  model <- lm(as.formula(paste(column, "~ latest.student.retention_rate.four_year.full_time_pooled")), data = final_df)
  model_summary <- summary(model)
  
  output <- data.frame(
    Term = ifelse(rownames(model_summary$coefficients) == "(Intercept)", "Baseline Completion Rate (if Retention = 0)", "FT Retention Rate at 4yr Institutions"),
    Coefficient = model_summary$coefficients[, "Estimate"],
    p_value = model_summary$coefficients[, "Pr(>|t|)"],
    R_squared = rep(model_summary$r.squared, nrow(model_summary$coefficients)),
    Race_Ethnicity = gsub("latest.completion.completion_rate_4yr_150_","", column)
  )
  
  outputs[[column]] <- output
}

final_output <- do.call(rbind, outputs)
colnames(final_output)[colnames(final_output)=="Term"] <- "Retention Rate (Predictor)"

print(final_output)

outputs_lt4yr <- list()

completion_columns_lt4yr <-c(
  "latest.completion.completion_rate_l4yr_150_white",
  "latest.completion.completion_rate_l4yr_150_black",
  "latest.completion.completion_rate_l4yr_150_hispanic",
  "latest.completion.completion_rate_l4yr_150_asian",
  "latest.completion.completion_rate_l4yr_150_aian",
  "latest.completion.completion_rate_l4yr_150_nhpi",
  "latest.completion.completion_rate_l4yr_150_2ormore",
  "latest.completion.completion_rate_l4yr_150_nonresident.alien",
  "latest.completion.completion_rate_l4yr_150_race.unknown"
)

for (column_lt4yr in completion_columns_lt4yr) {
  model_lt4yr <- lm(as.formula(paste(column_lt4yr, "~ latest.student.retention_rate.lt_four_year.full_time_pooled")), data=final_df)
  model_summary_lt4yr <- summary(model_lt4yr)
  
  output_lt4yr <- data.frame(
    Term= ifelse(rownames(model_summary_lt4yr$coefficients)=="(Intercept)","Baseline Completion Rate (if Retention = 0)", "FT Retention Rate at Less Than 4yr Institutions"),
    Coefficient = model_summary_lt4yr$coefficients[, "Estimate"],
    p_value = model_summary_lt4yr$coefficients[, "Pr(>|t|)"],
    R_squared = rep(model_summary_lt4yr$r.squared, nrow(model_summary_lt4yr$coefficients)),
    Race_Ethnicity = gsub("latest.completion.completion_rate_l4yr_150_","", column_lt4yr)
  )
  
  outputs_lt4yr[[column_lt4yr]] <- output_lt4yr
}

final_output_lt4yr <- do.call(rbind, outputs_lt4yr)
colnames(final_output_lt4yr)[colnames(final_output_lt4yr)== "Term"] <- "Retention Rate (Predictor)"


print(final_output_lt4yr)
print(final_df)
