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

gui_links_regular <- list.files(file.path("data", "gui", "regular"), pattern = ".txt$", full.names = TRUE) |> 
    lapply(FUN = \(f) {
        tibble(links = readLines(f) |> str_extract_all(pattern = "https?:.*", simplify = TRUE) |> unique() |> as.vector(), file = f)
    }) |> 
    do.call(what = rbind) |> 
    filter(links != "")

gui_links_diverse <- list.files(file.path("data", "gui", "diverse"), pattern = ".txt$", full.names = TRUE, recursive = TRUE) |> 
    lapply(FUN = \(f) {
        tibble(links = readLines(f) |> str_extract_all(pattern = "https?:.*", simplify = TRUE) |> unique() |> as.vector(), file = f)
    }) |> 
    do.call(what = rbind) |> 
    filter(links != "")

gui_links <- rbind(
        gui_links_diverse |> mutate(condition = "Diverse"), 
        gui_links_regular |> mutate(condition = "Regular")
    ) |> 
    mutate(date = str_extract(file, "\\d{8}")) |> 
    mutate(date = lubridate::ymd(date)) |> 
    mutate(domain = urltools::url_parse(links)) |> 
    unnest(domain) |> 
    mutate(domain = str_remove(domain, "^www\\.")) |> 
    mutate(domain = str_remove(domain, "^www1\\.")) |> 
    filter(!str_detect(links, "favicons")) |> 
    distinct(file, date, condition, links, domain) |> 
    mutate(condition = factor(condition, levels = c("Regular", "Diverse"), labels = c("Regular", "Diverse")))

gui_domains <- gui_links |> 
    group_by(condition) |> 
    count(domain, sort = TRUE) |> 
    mutate(p = n/sum(n)) |> 
    mutate(cp = cumsum(p)) |> 
    ungroup() |> 
    left_join(tranco, by = "domain") |> 
    left_join(gond, by = "domain") |> 
    mutate(across(tranco_rank_inv, \(x) ifelse(is.na(x), 0, x))) |> 
    distinct()

gui_domains |> 
    group_by(domain) |> 
    mutate(total_n = sum(n)) |> 
    ungroup() |> 
    pivot_wider(
        names_from = condition, 
        values_from = p, 
        id_cols = c(domain, total_n), 
        values_fill = 0
    ) |> 
    mutate(d = Diverse / Regular) |> 
    mutate(d = log(d)) |> 
    mutate(condition = case_when(
        d < -0.05 ~ "Regular",
        d > 0.05 ~ "Diverse",
        between(d, -.05, .05) ~ "Both"
    )) |>
    mutate(label = ifelse(between(d, -.75, .75), NA, domain)) |> 
    mutate(label = ifelse(total_n < 100, NA, label)) |> 
    ggplot(aes(x = Diverse, y = Regular)) +
    geom_abline(slope = 1, intercept = 0) +
    geom_point(aes(size = total_n, color = condition)) +
    geom_label_repel(aes(label = label, fill = condition), min.segment.length = 0) +
    scale_color_manual(values = c("Regular"="#4e79a7", "Diverse"="#f28e2b", "Both"="gray")) +
    scale_fill_manual(values = c("Regular"="#4e79a7", "Diverse"="#f28e2b", "Both"="gray")) +
    scale_y_log10(labels = scales::percent) +
    scale_x_log10(labels = scales::percent) +
    theme(legend.position = "bottom")

gui_links |> 
    group_by(file) |> 
    count(domain) |> 
    mutate(p = (n/sum(n))*100) |> 
    arrange(-n) |> 
    summarise(
        hhi = hhi(n),
        sdi = sdi(n),
        unique_sources = sum(n)
    ) |> 
    pivot_longer(-file) |> 
    mutate(condition = str_extract(file, "Regular|Diverse")) |> 
    mutate(condition = factor(condition, levels = c("Regular", "Diverse"), labels = c("Regular", "Diverse"))) |> 
    group_by(name) |> 
    summarise(
        t_test = broom::tidy(t.test(value ~ condition)),
        n = n()
    ) |> 
    unnest(t_test) |> 
    mutate(sig = ifelse(p.value < .001, "< .001", "not sig.")) |> 
    select(name, estimate1, estimate2, sig, n) |> 
    rename(Diverse = estimate2, Regular = estimate1) |> 
    gt() |> 
    fmt_number(Regular:Diverse) |> 
    cols_label(name = "Indicator", sig = "p.value") |> 
    tab_footnote("hhi: Herfindahl-Hirschman Index, sdi: Shannon Diversity Index, significance based on two-sided t-tests")

gui_dates <- gui_links |> 
    count(date = floor_date(date, "1 day")) |> 
    pull(date) |> 
    as.character()

df <- links |> 
    filter(as.character(floor_date(date, "1 day")) %in% gui_dates) |> 
    group_by(condition) |> 
    count(domain) |> 
    mutate(p = n/sum(n)) |> 
    arrange(-n) |> 
    ungroup() |> 
    left_join(
        gui_domains |> 
            filter(condition == "diverse") |> 
            group_by(domain) |> 
            summarise(gui_n = sum(n)) |> 
            mutate(gui_p = gui_n/sum(gui_n))
    ) |> 
    mutate(domain = factor(domain, unique(domain))) |> 
    select(domain, condition, p, gui_p) |> 
    filter(!is.na(gui_p))
wilcox.test(df$p, df$gui_p, paired = TRUE)
cor.test(df$p, df$gui_p, method = "spearman")
df |> 
    mutate(api_pp = p-gui_p) |> 
    pivot_wider(names_from = condition, values_from = api_pp, id_cols = domain) |> 
    arrange(-Regular) |> 
    filter(abs(Regular) > .02 | abs(Diverse) > .02) |> 
    select(domain, Regular, Diverse) |> 
    gt() |> 
    cols_align("right", domain) |> 
    fmt_percent(Diverse:Regular)
df |> 
    ggplot(aes(x = p, y = gui_p, color = condition)) +
    geom_point() +
    geom_line(aes(group = domain)) +
    scale_y_log10("log(gui %)") +
    scale_x_log10("log(api %)")



