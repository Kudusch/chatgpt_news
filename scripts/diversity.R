# Dependencies ----
library(rio)
library(dplyr)
library(tidyr)
library(widyr)
library(stringr)
library(lubridate)
library(jsonlite)
library(gt)
library(ggplot2)
library(scales)
library(ggrepel)

# Functions and colors ----
options(ggplot2.discrete.colour = c("#4e79a7", "#f28e2b", "#e15759", "#76b7b2", "#59a14f", "#edc948", "#b07aa1", "#ff9da7", "#9c755f", "#bab0ac"))
options(ggplot2.discrete.fill = c("#4e79a7", "#f28e2b", "#e15759", "#76b7b2", "#59a14f", "#edc948", "#b07aa1", "#ff9da7", "#9c755f", "#bab0ac"))

source("scripts/rao.diversity.R")

## Shannon Diversity Index: Higher values mean higher diversity
sdi <- function(counts) {
    props <- counts / sum(counts)
    -sum(props * log(props))
}
## Herfindahl–Hirschman index: Higher values mean higher concentration
hhi <- function(counts, limit=NA) {
    if (!is.na(limit)) {
        counts <- counts[1:limit]
    }
    props <- (counts/sum(counts, na.rm = TRUE))
    sum(props^2, na.rm = TRUE)
}
skew <- function(x) {
    sum(((x - mean(x))^3))/((length(x)-1)*(sd(x)^3))
}
get_condition <- function(uid) {
    factor(
        str_extract(uid, "[AB]"), 
        levels = c("A", "B"),
        labels = c("Regular", "Diverse")
    )
}

# Load data from json ----
links <- jsonlite::fromJSON("Data/links_api.json") |> 
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
    mutate(url = str_remove(url, fixed("?utm_source=openai"))) |> 
    mutate(condition = get_condition(uid)) |> 
    distinct(uid, date, experiment, condition, url, domain)

links <- links |> 
    mutate(domain = ifelse(
        str_detect(domain, "news-pravda.com"),
        "news-pravda.com",
        domain
    ))

tranco <- import("data/tranco_top-1m.csv") |> 
    rename(tranco_rank = 1, domain = 2) |> 
    mutate(tranco_rank_inv = 1000001-tranco_rank)

gond <- import("data/GONDv3_domains.csv") |> 
    rename(gond_type = type, gond_lang = language) |> 
    select(domain, gond_type, gond_lang)

manual_coding <- list.files("data", pattern = "coder_.*\\.xlsx", full.names = TRUE) |> 
    import_list(rbind = TRUE, setclass = "tibble") |> 
    mutate(coder = factor(Coder, levels = c("Tim", "Example", "Leonie", "Justin"))) |> 
    rename(category = Kategorie, is_springer = `Ist Springer?`) |> 
    mutate(domain = str_remove(Domain, "https://")) |> 
    mutate(is_springer = is_springer == "Ja") |> 
    mutate(is_springer = ifelse(is.na(is_springer), FALSE, is_springer)) |> 
    select(domain, category, is_springer, coder) |> 
    mutate(domain = ifelse(
        str_detect(domain, "news-pravda.com"),
        "news-pravda.com",
        domain
    )) |> 
    mutate(category = case_match(
        category,
        c("private_newsmedia", "public_broadcaster", "local_media", "alternative_media") ~ "journalistic_media",
        .default = category
    )) |> 
    mutate(category = factor(
        category,
        c("journalistic_media", "news_agency", "encyclopedias", "organization", "other"),
        c("Journalistic Media", "News Agencies", "Encyclopedias", "Organization/Business", "Misc")
    ))

domains <- links |> 
    group_by(condition) |> 
    count(domain, sort = TRUE) |> 
    mutate(p = n/sum(n)) |> 
    mutate(cp = cumsum(p)) |> 
    ungroup() |> 
    left_join(tranco, by = "domain") |> 
    left_join(gond, by = "domain") |> 
    mutate(across(tranco_rank_inv, \(x) ifelse(is.na(x), 0, x))) |> 
    left_join(
        manual_coding |> group_by(domain) |> filter(!is.na(category)) |> arrange(coder) |> slice(1) |> filter(!is.na(category)), 
        by = "domain"
    ) |> 
    distinct()

domains |> 
    filter(!is.na(gond_lang)) |> 
    group_by(condition) |> 
    count(gond_lang, sort = TRUE) |> 
    mutate(p = n/sum(n)) |> 
    ungroup() |> 
    pivot_wider(names_from = condition, values_from = p, id_cols = "gond_lang") |> 
    gt() |> 
    fmt_percent(-gond_lang)

domains |>
    count(gond_type)

top_journalistic_domains <- domains |> 
    group_by(domain) |> 
    summarise(n = sum(n)) |> 
    filter(domain %in% gond$domain) |> 
    arrange(-n)
communities_diverse <- links |> 
    mutate(condition = get_condition(uid)) |> 
    filter(condition == "Diverse") |> 
    filter(domain %in% top_journalistic_domains$domain) |> 
    group_by(uid) |> 
    count(domain, sort = TRUE) |> 
    pivot_wider(names_from = domain, values_from = n, values_fill = 0) |> 
    ungroup() |> 
    tibble::column_to_rownames("uid")
traits_diverse <- tibble(domain = names(communities_diverse)) |> 
    left_join(gond) |> 
    tibble::column_to_rownames("domain") |> 
    select(-gond_lang) |> 
    mutate(gond_type = as.numeric(factor(gond_type)))
res_diverse <- rao.diversity(
    communities_diverse, 
    traits = traits_diverse
)
communities_regular <- links |> 
    mutate(condition = get_condition(uid)) |> 
    filter(condition == "Regular") |> 
    filter(domain %in% top_journalistic_domains$domain) |> 
    group_by(uid) |> 
    count(domain, sort = TRUE) |> 
    pivot_wider(names_from = domain, values_from = n, values_fill = 0) |> 
    ungroup() |> 
    tibble::column_to_rownames("uid")
traits_regular <- tibble(domain = names(communities_regular)) |> 
    left_join(gond) |> 
    tibble::column_to_rownames("domain") |> 
    select(-gond_lang) |> 
    mutate(gond_type = as.numeric(factor(gond_type)))
res_regular <- rao.diversity(
    communities_regular, 
    traits = traits_regular
)
t.test(res_diverse$FunRao, res_regular$FunRao)

res_regular$FunRao