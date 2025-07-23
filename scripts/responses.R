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
    arrange(-n)

cooc_graph <- cooc_edges |> 
    filter(n >= 100) |> 
    graph_from_data_frame(
        directed = FALSE, 
        vertices = token_counts |> filter(!is_pruned)
    )

cls.wt <- cluster_walktrap(cooc_graph, weights = E(cooc_graph)$n)
V(cooc_graph)$cluster <- membership(cls.wt)
token_clusters <- membership(cls.wt) |> 
    enframe(name = "token", value = "cluster")  |> 
    mutate(cluster = as.numeric(cluster)) |> 
    left_join(token_counts) |> 
    add_count(cluster, name = "cluster_count")
    
token_clusters |> 
    filter(cluster_count >= 5) |> 
    group_by(cluster) |> 
    arrange(-n) |> 
    slice(1:10) |> 
    gt()

tmp <- cooc_graph |> 
    delete_vertices(v = !(V(cooc_graph)$cluster %in% (token_clusters |> filter(cluster_count >= 5) |> pull(cluster) |> unique())))
tmp <- tmp |> delete_vertices(v = V(tmp)$n < 1000)
tmp |> 
    delete_edges(E(tmp)[E(tmp)$n < 100]) |> 
    ggraph() +
    geom_node_point(aes(size = n, color = as.character(cluster))) +
    theme_void()

