# Dependencies ----
library(rio)
library(dplyr)
library(tidyr)
library(widyr)
library(tibble)

library(stringr)
library(lubridate)

library(tidytext)
library(stopwords)
library(igraph)
library(ggraph)

library(gt)
library(ggplot2)
library(scales)
library(ggrepel)

# Functions and colors ----
options(ggplot2.discrete.colour = c("#4e79a7", "#f28e2b", "#e15759", "#76b7b2", "#59a14f", "#edc948", "#b07aa1", "#ff9da7", "#9c755f", "#bab0ac"))
options(ggplot2.discrete.fill = c("#4e79a7", "#f28e2b", "#e15759", "#76b7b2", "#59a14f", "#edc948", "#b07aa1", "#ff9da7", "#9c755f", "#bab0ac"))

# Read data ----
source("scripts/read_data.R")

responses <- api_df |> 
    filter(role == "assistant") |> 
    mutate(response = str_remove_all(response, regex("\\[.*?\\]\\(.*?\\)"))) |> 
    mutate(response = str_remove_all(response, regex("\\b\\d+\\b"))) |> 
    unnest_tokens("paragraphs", response, token = "paragraphs", drop = TRUE) |> 
    group_by(id) |> 
    mutate(pid = sprintf("p_%s_%s", id, 1:n())) |> 
    ungroup() |> 
    unnest_tokens("token", paragraphs, drop = FALSE) |> 
    filter(!token %in% stopwords(language = "de"))

token_counts <- responses |> 
    filter(str_length(token) >= 2) |> 
    count(token, sort = TRUE) |> 
    mutate(p = n/sum(n)) |> 
    mutate(is_pruned = !between(cumsum(p), .3, .85))

token_counts |> 
    mutate(rank = 1:n()) |> 
    ggplot(aes(x = rank, y = n)) +
    geom_point(aes(color = is_pruned)) + 
    scale_x_log10() +
    scale_y_log10()

cooc_edges <- responses |> 
    select(pid, token) |> 
    left_join(token_counts) |> 
    filter(!is_pruned) |> 
    pairwise_count(token, pid, upper = FALSE, diag = FALSE) |> 
    arrange(-n) |> 
    left_join(rename(token_counts, item1_n = n), by = c("item1"="token")) |> 
    select(item1:item1_n) |> 
    left_join(rename(token_counts, item2_n = n), by = c("item2"="token")) |> 
    select(item1:item2_n) |> 
    mutate(across(n:item2_n, \(i) {i/sum(i)})) |> 
    mutate(NPMI = log(n/(item1_n*item2_n))/(-log(n))) |> 
    select(item1, item2, n, NPMI)

cooc_graph <- cooc_edges |> 
    graph_from_data_frame(
        directed = FALSE, 
        vertices = token_counts |> filter(!is_pruned)
    )

cls.l <- cluster_louvain(cooc_graph, weights = E(cooc_graph)$NPMI, resolution = 2)
V(cooc_graph)$cluster <- membership(cls.l)
token_clusters <- membership(cls.l) |> 
    enframe(name = "token", value = "cluster")  |> 
    mutate(cluster = as.numeric(cluster)) |> 
    left_join(token_counts) |> 
    add_count(cluster, name = "cluster_count")

token_clusters |> 
    distinct(cluster, cluster_count) |> 
    arrange(-cluster_count)

token_clusters |> 
    filter(cluster_count >= 5) |> 
    group_by(cluster) |> 
    arrange(-n) |> 
    summarise(
        tokens = paste(na.omit(token[1:15]), collapse = ", "),
        token_count = sum(n),
        cluster_size = unique(cluster_count)
    ) |>
    arrange(-token_count) |> 
    gt()


responses |> 
    left_join(select(token_clusters, token, cluster, cluster_count)) |> 
    filter(!is.na(cluster)) |> 
    filter(cluster_count >= 5)

token_tf_idf <- responses |>    
    group_by(experiment) |>         
    count(token) |>                 
    ungroup() |>                    
    bind_tf_idf(token, experiment, n)  |> 
    arrange(-tf_idf)                      

token_tf_idf |> 
    group_by(experiment) |> 
    slice_max(tf_idf, n = 20) |> 
    ungroup() |> 
    mutate(token = factor(token, levels = rev(token))) |> 
    ggplot(aes(x = tf_idf, y = token, fill = experiment)) +
    geom_col(show.legend = FALSE) +
    facet_wrap(vars(experiment), scales = "free")
