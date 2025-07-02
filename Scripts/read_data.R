# Dependencies ----
library(rio)
library(nanoparquet)
library(arrow)
library(dplyr)
library(tidyr)
library(widyr)
library(stringr)
library(lubridate)
library(jsonlite)
library(gt)
library(ggplot2)
library(scales)

# Read data ----
gui_raw <- arrow::read_ipc_stream("Data/gui.arrow")
gui_df <- lapply(seq_along(gui_raw$history), \(i) {
    if (!i %in% c(188, 234, 256, 263, 277, 320, 336, 349, 362)) {
        gui_raw$history[[i]] |> 
        mutate(id = i)
    }
}) |> do.call(what = rbind) |> 
    left_join(mutate(gui_raw$metadata, id = 1:n()))

api_raw <- arrow::read_ipc_stream("Data/api.arrow")
api_df <- lapply(seq_along(api_raw$history), \(i) {
    api_raw$history[[i]] |> mutate(id = i)
}) |> do.call(what = rbind) |> 
    left_join(mutate(api_raw$metadata, id = 1:n()))
rm(gui_raw, api_raw)
