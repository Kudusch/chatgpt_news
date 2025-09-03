# Dependencies ----
library(rio)
library(nanoparquet)
library(arrow)
library(dplyr)
library(stringr)
source("scripts/functions_and_vars.R")

# Read data ----
## Read 3rd party data ----
tranco <- import("data/tranco_top-1m.csv") |> 
    rename(tranco_rank = 1, domain = 2) |> 
    mutate(tranco_rank_inv = 1000001-tranco_rank)
gond <- import("data/GONDv3_domains.csv") |> 
    rename(gond_type = type, gond_lang = language) |> 
    mutate(gond_type = factor(gond_type, levels = unique(gond_type), labels = str_to_title(unique(gond_type)))) |> 
    select(domain, gond_type, gond_lang)
# import aggregated data; Reuters raw data not available
reuters <- import("data/reuters.csv", row.names = 1) |> 
    group_by(domain) |> 
    summarise(
        rank_reuters  = mean(rank_reuters, na.rm = TRUE),
        media_leanings = mean(media_leanings, na.rm = TRUE)
    ) |> 
    ungroup()
# import ranking from similarweb.de
similar <- import("data/similarweb_news_list.csv") |> 
    mutate(domain = case_when(
        domain == "news.google.com" ~ "news.google.com",
        TRUE ~ str_extract(domain, "\\.?([a-z0-9-]+\\.[a-z]{2,})$", 1) # remove subdomains
    )) |> 
    mutate(domain = case_when(
        domain %in% c("ard.de", "tagesschau.de") ~ "ard.de/tagesschau.de",
        domain %in% c("sat1.de", "prosieben.de") ~ "sat1.de/prosieben.de",
        TRUE ~ domain
    )) |> 
    group_by(domain) |> 
    summarise(rank_similarweb = min(rank_similarweb)) |> 
    ungroup()

## Read responses ----
api_raw <- arrow::read_ipc_stream("Data/api.arrow")
api_responses <- lapply(seq_along(api_raw$history), \(i) {
    api_raw$history[[i]] |> mutate(id = i)
}) |> do.call(what = rbind) |> 
    left_join(mutate(api_raw$metadata, id = 1:n()), by = "id") |> 
    mutate(condition = factor(experiment, levels = c("A", "B"), labels = c("Regular", "Diverse"))) |> 
    mutate(date = ymd_hms(timestamp)) |> 
    rename(uid = id) |> 
    select(uid, date, condition, response, role)
gui_responses <- rbind(
    list.files(file.path("data", "gui", "fixed", "regular"), pattern = ".txt$", full.names = TRUE) |> 
        lapply(FUN = \(f) {
            tibble(response = readLines(f, encoding = "UTF-8", warn = FALSE) |> paste(collapse = "\n"), file = f, condition = "Regular")
        }) |> 
        do.call(what = rbind) |> 
        filter(response != ""),
    list.files(file.path("data", "gui", "fixed", "diverse"), pattern = ".txt$", full.names = TRUE, recursive = TRUE) |> 
        lapply(FUN = \(f) {
            tibble(response = readLines(f, encoding = "UTF-8", warn = FALSE) |> paste(collapse = "\n"), file = f, condition = "Diverse")
        }) |> 
        do.call(what = rbind) |> 
        filter(response != "")
) |> 
    mutate(date = str_extract(file, "\\d{8}")) |> 
    mutate(date = lubridate::ymd(date)) |> 
    distinct(file, date, condition, response) |> 
    mutate(condition = factor(condition, levels = c("Regular", "Diverse"), labels = c("Regular", "Diverse"))) |> 
    rename(uid = file)
rm(api_raw)

## Extract links ----
gui_links_regular <- list.files(file.path("data", "gui", "fixed", "regular"), pattern = ".txt$", full.names = TRUE) |> 
    lapply(FUN = \(f) {
        tibble(links = readLines(f, warn = FALSE) |> str_extract_all(pattern = "https?:[^) >]*", simplify = TRUE) |> unique() |> as.vector(), file = f)
    }) |> 
    do.call(what = rbind) |> 
    filter(links != "") |> 
    mutate(condition = "Regular")
gui_links_diverse <- list.files(file.path("data", "gui", "fixed", "diverse"), pattern = ".txt$", full.names = TRUE, recursive = TRUE) |> 
    lapply(FUN = \(f) {
        tibble(links = readLines(f, warn = FALSE) |> str_extract_all(pattern = "https?:[^) >]*", simplify = TRUE) |> unique() |> as.vector(), file = f)
    }) |> 
    do.call(what = rbind) |> 
    filter(links != "") |> 
    mutate(condition = "Diverse")
gui_links <- rbind(gui_links_diverse, gui_links_regular) |> 
    mutate(date = str_extract(file, "\\d{8}")) |> 
    mutate(date = lubridate::ymd(date)) |> 
    mutate(domain = urltools::url_parse(links)) |> 
    unnest(domain) |> 
    mutate(domain = str_remove(domain, "^www\\.")) |> 
    mutate(domain = str_remove(domain, "^www1\\.")) |> 
    mutate(domain = str_remove(domain, "\\].*$")) |> 
    mutate(domain = str_extract(domain, "\\.?([a-z0-9-]+\\.[a-z]{2,})$", 1)) |> 
    mutate(domain = str_trim(domain)) |> 
    mutate(links = str_remove(links, fixed("?utm_source=openai"))) |> 
    filter(!str_detect(links, "favicons")) |> 
    distinct(file, date, condition, links, domain) |> 
    mutate(condition = factor(condition, levels = c("Regular", "Diverse"), labels = c("Regular", "Diverse"))) |> 
    rename(uid = file, url = links)
rm(gui_links_diverse, gui_links_regular)
api_links <- jsonlite::fromJSON("Data/links_api.json") |> 
    unnest(metadata) |> 
    unite(uid, -links, remove = FALSE) |> 
    unnest(links) |> 
    select(-Name) |> 
    rename(url = URL) |> 
    unite(date, year:minute, sep = "-") |> 
    mutate(date = strptime(date, format = "%Y-%m-%d-%H-%M")) |> 
    mutate(domain = urltools::url_parse(url)) |> 
    unnest(domain) |> 
    mutate(domain = str_remove(domain, "^www\\.")) |> 
    mutate(domain = str_remove(domain, "^www1\\.")) |> 
    mutate(domain = str_extract(domain, "\\.?([a-z0-9-]+\\.[a-z]{2,})$", 1)) |> 
    mutate(domain = str_trim(domain)) |> 
    mutate(url = str_remove(url, fixed("?utm_source=openai"))) |> 
    mutate(condition = f.get_condition(uid)) |> 
    distinct(uid, date, experiment, condition, url, domain)
## Extract domains ----
gui_domains <- gui_links |> 
    mutate(domain = str_trim(domain)) |> 
    mutate(domain = str_remove(domain, "\\s.*")) |> 
    mutate(domain = str_remove(domain, "\\] .*")) |> 
    group_by(condition) |> 
    count(domain, sort = TRUE) |> 
    mutate(p = n/sum(n)) |> 
    mutate(cp = cumsum(p)) |> 
    ungroup() |> 
    left_join(tranco, by = "domain") |> 
    left_join(gond, by = "domain") |> 
    mutate(across(tranco_rank_inv, \(x) ifelse(is.na(x), 0, x))) |> 
    mutate(is_springer = domain %in% springer_domains) |> 
    distinct() |> 
    filter(!is.na(domain))
api_domains <- api_links |> 
    group_by(condition) |> 
    count(domain, sort = TRUE) |> 
    mutate(p = n/sum(n)) |> 
    mutate(cp = cumsum(p)) |> 
    ungroup() |> 
    left_join(tranco, by = "domain") |> 
    left_join(gond, by = "domain") |> 
    mutate(across(tranco_rank_inv, \(x) ifelse(is.na(x), 0, x))) |> 
    mutate(is_springer = domain %in% springer_domains) |> 
    distinct() |> 
    filter(!is.na(domain))