library(rio)
library(dplyr)
library(tidyr)
library(widyr)
library(stringr)
library(lubridate)
library(gt)
library(ggplot2)
library(scales)
library(jsonlite)
library(igraph)
library(ggraph)

library(urltools)
library(httr2)
library(rvest)

library(ktools)

options(ggplot2.discrete.colour = c("#4e79a7", "#f28e2b", "#e15759", "#76b7b2", "#59a14f", "#edc948", "#b07aa1", "#ff9da7", "#9c755f", "#bab0ac"))
options(ggplot2.discrete.fill = c("#4e79a7", "#f28e2b", "#e15759", "#76b7b2", "#59a14f", "#edc948", "#b07aa1", "#ff9da7", "#9c755f", "#bab0ac"))




sdi <- function(counts) {
    props <- counts / sum(counts)
    -sum(props * log(props))
}
## implement hhi4 - hhi100
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
    distinct(uid, date, experiment, url, domain) |> 
    mutate(experiment = factor(
        experiment, 
        levels = c("A", "B"),
        labels = c("Current news", "Current news, diverse")
    ))

# Presserat als Kriterium
domains <- links |> 
    count(domain, sort = TRUE) |> 
    mutate(cum_p = cumsum(n/sum(n)))

domains |> 
    filter(n >= 10) |> 
    mutate(category = NA) |> 
    select(domain, category) |> 
    mutate(domain = sprintf("https://%s", domain)) |> 
    export(file = "Output/manual_coding.csv")

domains |> 
    mutate(x = (1:n())/n()) |> 
    ggplot(aes(x = x, y = cum_p)) +
    geom_line() +
    scale_y_continuous(labels = scales::percent) +
    scale_x_continuous(labels = scales::percent)

url_dates <- import(file = "Output/urls.RDS") |> 
    mutate(url = unlist(V1)) |> 
    mutate(date = unlist(V2)) |> 
    mutate(date = as.POSIXct(as.numeric(date)*24*60*60, tz = "UTC")) |> 
    select(url, date) |> 
    as_tibble()

links |> 
    group_by(experiment) |> 
    count(domain) |> 
    mutate(count_cat = case_when(
        n == 1 ~ "1: 1",
        n <= 5 ~ "2: <= 5",
        n <= 10 ~ "3: <= 10",
        n <= 50 ~ "4: <= 50",
        n <= 100 ~ "5: <= 100",
        n > 100 ~ "6: > 100"
    )) |> 
    count(count_cat, name = "n") |> 
    mutate(p = n/sum(n)) |> 
    ungroup() |> 
    ggplot(aes(x = count_cat, y = p, fill = experiment)) +
    geom_col(position = "dodge") +
    scale_y_continuous(labels = percent)

links |> 
    group_by(experiment) |> 
    count(domain, sort = TRUE) |> 
    ungroup() |> 
    ggplot(aes(x = n, fill = experiment)) +
    geom_histogram(position = "dodge") +
    scale_y_log10()

links |> 
    group_by(experiment) |> 
    count(domain) |> 
    mutate(p = (n/sum(n))*100) |> 
    arrange(-n) |> 
    summarise(
        hhi = hhi(n),
        hhi_4 = hhi(n, 4),
        hhi_100 = hhi(n, 100),
        sdi = sdi(n),
        n = sum(n)
    )

links |> 
    group_by(uid) |> 
    count(domain, sort = TRUE) |> 
    summarise(
        hhi = hhi(n),
        hhi_4 = hhi(n, 4),
        hhi_100 = hhi(n, 100),
        sdi = sdi(n),
        n = n()
    ) |> 
    pivot_longer(-uid) |> 
    mutate(experiment = get_condition(uid)) |> 
    ggplot(aes(x = name, y = value, fill = experiment)) +
    geom_boxplot() +
    facet_wrap(vars(name), scales = "free")

links |> 
    mutate(experiment = get_condition(uid)) |> 
    group_by(date = floor_date(date, unit = "week"), experiment) |> 
    count(domain, sort = TRUE) |> 
    summarise(
        hhi = hhi(n),
        hhi_4 = hhi(n, 4),
        hhi_100 = hhi(n, 100),
        sdi = sdi(n),
        n = n()
    ) |> 
    ungroup() |> 
    pivot_longer(hhi:n) |> 
    ggplot(aes(x = as.POSIXct(date), y = value, color = experiment)) +
    geom_line(aes(group = date), color = "black") +
    geom_point(size = 2) +
    facet_wrap(vars(name), scales = "free")

g.domains <- links |> 
    select(uid, domain) |> 
    pairwise_count(item = domain, feature = uid, upper = FALSE, diag = FALSE) |> 
    igraph::graph_from_data_frame(vertices = count(links, domain))

g.domains |> 
    ggraph() +
    geom_edge_link0(aes(edge_alpha = n)) +
    geom_node_point(aes(size = n)) +
    theme_void()

 tibble(
     n = V(g.domains)$name,
     b = betweenness(g.domains),
     d = degree(g.domains)
 ) |> 
     arrange(-b) |> 
     slice(1:10) |> 
     mutate(n = factor(n, n)) |> 
     ggplot(aes(x = n)) +
     geom_col(aes(y = d))

links |> 
    count(domain, sort = TRUE) |> 
    gt()

