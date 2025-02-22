library(httr)
library(jsonlite)

# api_key <- Sys.getenv("API_KEY")
api_key <- "qWqq0Sjyjs9sJkJgr3SPt7ic53nx7UdIOw9dZriM"
base_url <- "https://api.data.gov/ed/collegescorecard/v1/schools?"

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
page <- 1
for (page in 1:100)
  {
  url <- paste0(base_url, "api_key=", api_key, "&fields=", vars, "&page=", page, "&per_page=100")
  response <- GET(url)
  data <- fromJSON(content(response,"text",encoding="UTF-8"), flatten = TRUE)$results
  if (any(is.null(data$id))){

    break
  }

  all_data <- append(all_data, list(data))
  page <- page + 1
}


final_df <- do.call(rbind, lapply(all_data, as.data.frame))
print(final_df)