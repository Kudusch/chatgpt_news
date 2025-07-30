# Dependencies ----
library(rio)
library(nanoparquet)
library(arrow)
library(dplyr)

# Read data ----
api_raw <- arrow::read_ipc_stream("Data/api.arrow")
api_df <- lapply(seq_along(api_raw$history), \(i) {
    api_raw$history[[i]] |> mutate(id = i)
}) |> do.call(what = rbind) |> 
    left_join(mutate(api_raw$metadata, id = 1:n()))
rm(gui_raw, api_raw)
