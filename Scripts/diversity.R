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

# Functions and colors ----
options(ggplot2.discrete.colour = c("#4e79a7", "#f28e2b", "#e15759", "#76b7b2", "#59a14f", "#edc948", "#b07aa1", "#ff9da7", "#9c755f", "#bab0ac"))
options(ggplot2.discrete.fill = c("#4e79a7", "#f28e2b", "#e15759", "#76b7b2", "#59a14f", "#edc948", "#b07aa1", "#ff9da7", "#9c755f", "#bab0ac"))

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
get_condition <- function(uid) {
    factor(
        str_extract(uid, "[AB]"), 
        levels = c("A", "B"),
        labels = c("Current news", "Current news, diverse")
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
    mutate(url = str_remove(url, fixed("?utm_source=openai"))) |> 
    mutate(condition = get_condition(uid)) |> 
    distinct(uid, date, experiment, condition, url, domain)

domains <- links |> 
    group_by(condition) |> 
    count(domain, sort = TRUE) |> 
    mutate(p = n/sum(n)) |> 
    mutate(cp = cumsum(p)) |> 
    ungroup()

domains |> 
    filter(n >= 10) |> 
    mutate(category = NA) |> 
    select(domain, category) |> 
    mutate(domain = sprintf("https://%s", domain)) |> 
    export(file = "Output/manual_coding.csv")

domains |> 
    group_by(condition) |> 
    mutate(x = (1:n())) |>
    ggplot(aes(x = x, y = cp, color = condition)) +
    geom_line() +
    scale_y_continuous(labels = scales::percent, breaks = seq(0, 1, .2)) +
    scale_x_continuous(labels = scales::number, breaks = \(lim) {seq(0, lim[2], 100)}) +
    labs(
        title = "",
        subtitle = "The top ",
        y = "Share of linked Domains",
        x = "Domains",
    )

domains |> 
    mutate(count_cat = case_when(
        n == 1 ~ "1",
        n <= 5 ~ "2-5",
        n <= 10 ~ "6-10",
        n <= 50 ~ "11-50",
        n <= 100 ~ "51-100",
        n > 100 ~ "> 100"
    )) |> 
    mutate(count_cat = factor(count_cat, levels = c("1","2-5","6-10","11-50","51-100","> 100"))) |> 
    group_by(condition) |> 
    count(count_cat, name = "n") |> 
    mutate(p = n/sum(n)) |> 
    ungroup() |> 
    ggplot(aes(x = count_cat, y = p, fill = condition)) +
    geom_col(position = "dodge") +
    scale_y_continuous(labels = percent)

links |> 
    group_by(condition) |> 
    count(domain) |> 
    mutate(p = (n/sum(n))*100) |> 
    arrange(-n) |> 
    summarise(
        hhi = hhi(n),
        sdi = sdi(n),
        unique_sources = sum(n)
    )

links |> 
    select(domain, condition) |> 
    group_by(domain) |> 
    summarise(both = n()==2)

links |> 
    group_by(date = floor_date(date, unit = "1 day"), condition) |> 
    count(domain, sort = TRUE) |> 
    summarise(
        hhi = hhi(n),
        sdi = sdi(n),
        unique_sources = n()
    ) |> 
    ungroup() |> 
    pivot_longer(hhi:unique_sources) |> 
    mutate(name = factor(name, levels = c("hhi", "sdi", "n"))) |> 
    ggplot(aes(x = as.POSIXct(date), y = value, color = condition)) +
    geom_line(aes(group = date), color = "black") +
    geom_point(size = 2) +
    facet_wrap(vars(name), scales = "free") +
    labs(
        title = "Source diversity over time",
        subtitle = "Shannon Diversity Index (sdi): Higher values mean higher diversity,\nHerfindahl–Hirschman index (hhi): Higher values mean higher concentration",
        x = "Days",
        y = ""
    )

links |> 
    group_by(date = floor_date(date, unit = "week"), condition) |> 
    count(domain, sort = TRUE) |> 
    summarise(
        hhi = hhi(n),
        sdi = sdi(n),
        n = n()
    ) |> 
    ungroup() |> 
    pivot_longer(hhi:n) |> 
    mutate(name = factor(name, levels = c("hhi", "sdi", "n"))) |> 
    ggplot(aes(x = as.POSIXct(date), y = value, color = condition)) +
    geom_line(aes(group = date), color = "black") +
    geom_point(size = 2) +
    facet_wrap(vars(name), scales = "free") +
    labs(
        title = "Source diversity over time",
        subtitle = "Shannon Diversity Index (sdi): Higher values mean higher diversity,\nHerfindahl–Hirschman index (hhi): Higher values mean higher concentration",
        x = "Weeks",
        y = ""
    )

