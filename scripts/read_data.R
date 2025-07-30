# Dependencies ----
library(rio)
library(nanoparquet)
library(arrow)
library(dplyr)
library(stringr)

# Read data ----
api_raw <- arrow::read_ipc_stream("Data/api.arrow")
api_responses <- lapply(seq_along(api_raw$history), \(i) {
    api_raw$history[[i]] |> mutate(id = i)
}) |> do.call(what = rbind) |> 
    left_join(mutate(api_raw$metadata, id = 1:n()))
gui_responses <- rbind(
    list.files(file.path("data", "gui", "regular"), pattern = ".txt$", full.names = TRUE) |> 
        lapply(FUN = \(f) {
            tibble(response = readLines(f, encoding = "UTF-8") |> paste(collapse = "\n"), file = f, condition = "Regular")
        }) |> 
        do.call(what = rbind) |> 
        filter(response != ""),
    list.files(file.path("data", "gui", "diverse"), pattern = ".txt$", full.names = TRUE, recursive = TRUE) |> 
        lapply(FUN = \(f) {
            tibble(response = readLines(f, encoding = "UTF-8") |> paste(collapse = "\n"), file = f, condition = "Diverse")
        }) |> 
        do.call(what = rbind) |> 
        filter(response != "")
) |> 
    mutate(date = str_extract(file, "\\d{8}")) |> 
    mutate(date = lubridate::ymd(date)) |> 
    distinct(file, date, condition, response) |> 
    mutate(condition = factor(condition, levels = c("Regular", "Diverse"), labels = c("Regular", "Diverse")))
rm(api_raw)