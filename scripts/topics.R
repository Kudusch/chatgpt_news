set.seed(42069)
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

# Read data ----
source("scripts/read_data.R")
source("scripts/functions_and_vars.R")

# API co-occurrence ----
api_tokens <- api_responses |> 
    filter(role == "assistant") |> 
    mutate(response = str_remove_all(response, regex("\\[.*?\\]\\(.*?\\)"))) |> 
    mutate(response = str_remove_all(response, regex("\\b\\d+\\b"))) |> 
    unnest_tokens("lines", response, token = "lines", drop = TRUE) |> 
    group_by(uid) |> 
    mutate(pid = sprintf("p_%s_%s", uid, 1:n())) |> 
    ungroup() |> 
    unnest_tokens("token", lines, drop = FALSE) |> 
    filter(!token %in% stopwords(language = "de"))

api_token_tf_idf <- api_tokens |>    
    group_by(uid, condition) |>         
    count(token) |>                 
    ungroup() |>                    
    bind_tf_idf(token, uid, n)  |> 
    group_by(token) |> 
    summarise(tf_idf = mean(tf_idf)) |> 
    mutate(is_pruned_if_idf = tf_idf <= quantile(tf_idf, .0025, na.rm = TRUE))

api_token_counts <- api_tokens |> 
    filter(str_length(token) >= 2) |> 
    count(token, sort = TRUE) |> 
    mutate(p = n/sum(n)) |> 
    mutate(is_pruned = !between(cumsum(p), .2, .85)) |> 
    left_join(api_token_tf_idf)
api_token_counts |> 
    mutate(rank = 1:n()) |> 
    ggplot(aes(x = rank, y = n)) +
    geom_point(aes(color = is_pruned | is_pruned_if_idf)) + 
    scale_x_log10() +
    scale_y_log10() +
    facet_wrap(vars(is_pruned | is_pruned_if_idf))

api_cooc_edges <- api_tokens |> 
    select(pid, token) |> 
    left_join(api_token_counts) |> 
    filter(!is_pruned & !is_pruned_if_idf) |> 
    pairwise_count(token, pid, upper = FALSE, diag = FALSE) |> 
    arrange(-n) |> 
    left_join(rename(api_token_counts, item1_n = n), by = c("item1"="token")) |> 
    select(item1:item1_n) |> 
    left_join(rename(api_token_counts, item2_n = n), by = c("item2"="token")) |> 
    select(item1:item2_n) |> 
    mutate(across(n:item2_n, \(i) {i/sum(i)})) |> 
    mutate(NPMI = log(n/(item1_n*item2_n))/(-log(n))) |> 
    select(item1, item2, n, NPMI) |> 
    arrange(NPMI)

api_cooc_graph <- api_cooc_edges |> 
    graph_from_data_frame(
        directed = FALSE, 
        vertices = api_token_counts |> filter(!is_pruned & !is_pruned_if_idf)
    )

api_cls.l <- cluster_louvain(api_cooc_graph, weights = E(api_cooc_graph)$NPMI, resolution = 5)
V(api_cooc_graph)$cluster <- membership(api_cls.l)
api_token_clusters <- membership(api_cls.l) |> 
    enframe(name = "token", value = "cluster")  |> 
    mutate(cluster = as.numeric(cluster)) |> 
    left_join(api_token_counts) |> 
    add_count(cluster, name = "cluster_count")
api_token_clusters <- api_token_clusters |> 
    left_join(
        api_tokens |> 
            left_join(
                api_token_clusters |> select(token, cluster)
            ) |> 
            filter(!is.na(cluster)) |> 
            group_by(uid, condition) |> 
            count(cluster) |> 
            mutate(p = n/sum(n)) |> 
            ungroup() |> 
            group_by(cluster, condition) |> 
            summarise(doc_share = mean(p)) |> 
            pivot_wider(names_from = condition, values_from = doc_share, values_fill = 0)
    ) |> 
    left_join(api_token_tf_idf) |> 
    select(-is_pruned_if_idf)

api_token_clusters |> 
    distinct(cluster, cluster_count) |> 
    arrange(-cluster_count)

api_topics_fig <- api_tokens |> 
    left_join(
        api_token_clusters |> 
            filter(cluster_count >= 5) |> 
            select(token, cluster)
    ) |> 
    filter(!is.na(cluster)) |> 
    group_by(date = floor_date(date, "1 day")) |> 
    count(cluster) |> 
    mutate(p = n/sum(n)) |> 
    ungroup() |> 
    arrange(date) |> 
    group_by(cluster) |> 
    mutate(sd = sd(p)) |> 
    mutate(kurt = f.kurt(p)) |> 
    mutate(spike = kurt>5) |> 
    ungroup()

api_topics_tbl <- api_token_clusters |> 
    filter(cluster_count >= 5) |> 
    group_by(cluster) |> 
    arrange(-n) |> 
    summarise(
        tokens = paste(na.omit(token[1:15]), collapse = ", "),
        token_count = sum(n),
        cluster_size = unique(cluster_count),
        Regular = unique(Regular),
        Diverse = unique(Diverse),
        mean_tf_idf = mean(tf_idf)
    ) |>
    arrange(-token_count) |> 
    select(-mean_tf_idf, -token_count) |> 
    left_join(
        api_tokens |> 
            left_join(
                api_token_clusters |> 
                    filter(cluster_count >= 5) |> 
                    select(token, cluster)
            ) |> 
            filter(!is.na(cluster)) |> 
            group_by(date = floor_date(date, "1 day")) |> 
            count(cluster) |> 
            mutate(p = n/sum(n)) |> 
            ungroup() |> 
            arrange(date) |> 
            group_by(cluster) |> 
            summarise(kurt = f.kurt(p), spike = kurt>5) |> 
            ungroup()
    ) |> 
    filter(spike) |> 
    select(-c(kurt:spike))
rio::export(api_topics_tbl, file = "output/api_topics_tbl.RDS")
rio::export(api_topics_fig, file = "output/api_topics_fig.RDS")
# GUI co-occurrence ----
gui_tokens <- gui_responses |> 
    mutate(response = str_remove_all(response, "https?:\\S+")) |> 
    mutate(response = str_remove(response, "Zeige mir die wichtigsten Nachrichten von heute.\n")) |> 
    mutate(response = str_remove(response, "Zeige mir die wichtigsten Nachrichten von heute. Bitte achte dabei besonders auf eine diverse Auswahl an Nachrichtenquellen, inklusive öffentlich-rechtlicher, konservativer, liberaler, regionaler, kleiner, investigativer, unabhängiger, kritischer und alternativer Angebote. Sortiere die Nachrichten nach Relevanz. Markiere, welche öffentlich-rechtliche, konservative, liberale, regionale, kleine, investigative, unabhängige, kritische und alternative Angebote sind.\n")) |> 
    mutate(response = str_remove(response, "##### Du:.*?######")) |> 
    mutate(response = str_remove_all(response, regex("\\[.*?\\]\\(.*?\\)"))) |> 
    mutate(response = str_remove_all(response, regex("\\b\\d+\\b"))) |> 
    unnest_tokens("lines", response, token = "lines", drop = TRUE) |> 
    group_by(file) |> 
    mutate(pid = sprintf("p_%s_%s", file, 1:n())) |> 
    ungroup() |> 
    unnest_tokens("token", lines, drop = FALSE) |> 
    filter(!token %in% stopwords(language = "de"))

gui_token_tf_idf <- gui_tokens |>    
    group_by(file, condition) |>         
    count(token) |>                 
    ungroup() |>                    
    bind_tf_idf(token, file, n)  |> 
    group_by(token) |> 
    summarise(tf_idf = mean(tf_idf)) |> 
    mutate(is_pruned_if_idf = tf_idf <= quantile(tf_idf, .0025, na.rm = TRUE))

gui_token_counts <- gui_tokens |> 
    filter(str_length(token) >= 2) |> 
    count(token, sort = TRUE) |> 
    mutate(p = n/sum(n)) |> 
    mutate(is_pruned = !between(cumsum(p), .2, .85)) |> 
    left_join(gui_token_tf_idf)
gui_token_counts |> 
    mutate(rank = 1:n()) |> 
    ggplot(aes(x = rank, y = n)) +
    geom_point(aes(color = is_pruned | is_pruned_if_idf)) + 
    scale_x_log10() +
    scale_y_log10() +
    facet_wrap(vars(is_pruned | is_pruned_if_idf))

gui_cooc_edges <- gui_tokens |> 
    select(pid, token) |> 
    left_join(gui_token_counts) |> 
    filter(!is_pruned) |> 
    pairwise_count(token, pid, upper = FALSE, diag = FALSE) |> 
    arrange(-n) |> 
    left_join(rename(gui_token_counts, item1_n = n), by = c("item1"="token")) |> 
    select(item1:item1_n) |> 
    left_join(rename(gui_token_counts, item2_n = n), by = c("item2"="token")) |> 
    select(item1:item2_n) |> 
    mutate(across(n:item2_n, \(i) {i/sum(i)})) |> 
    mutate(NPMI = log(n/(item1_n*item2_n))/(-log(n))) |> 
    select(item1, item2, n, NPMI)

gui_cooc_graph <- gui_cooc_edges |> 
    graph_from_data_frame(
        directed = FALSE, 
        vertices = gui_token_counts |> filter(!is_pruned)
    )

gui_cls.l <- cluster_louvain(gui_cooc_graph, weights = E(gui_cooc_graph)$NPMI, resolution = 5)
V(gui_cooc_graph)$cluster <- membership(gui_cls.l)
gui_token_clusters <- membership(gui_cls.l) |> 
    enframe(name = "token", value = "cluster")  |> 
    mutate(cluster = as.numeric(cluster)) |> 
    left_join(gui_token_counts) |> 
    add_count(cluster, name = "cluster_count")
gui_token_clusters <- gui_token_clusters |> 
    left_join(
        gui_tokens |> 
            left_join(
                gui_token_clusters |> select(token, cluster)
            ) |> 
            filter(!is.na(cluster)) |> 
            group_by(file, condition) |> 
            count(cluster) |> 
            mutate(p = n/sum(n)) |> 
            ungroup() |> 
            group_by(cluster, condition) |> 
            summarise(doc_share = mean(p)) |> 
            pivot_wider(names_from = condition, values_from = doc_share, values_fill = 0)
    ) |> 
    left_join(gui_token_tf_idf) |> 
    select(-is_pruned_if_idf)

gui_token_clusters |> 
    distinct(cluster, cluster_count) |> 
    arrange(-cluster_count)

gui_topics_fig <- gui_tokens |> 
    left_join(
        gui_token_clusters |> 
            filter(cluster_count >= 5) |> 
            select(token, cluster)
    ) |> 
    filter(!is.na(cluster)) |> 
    group_by(date = floor_date(date, "1 day")) |> 
    count(cluster) |> 
    mutate(p = n/sum(n)) |> 
    ungroup() |> 
    arrange(date) |> 
    group_by(cluster) |> 
    mutate(sd = sd(p)) |> 
    mutate(kurt = kurt(p)) |> 
    mutate(spike = kurt>5) |> 
    ungroup()

gui_topics_tbl <- gui_token_clusters |> 
    filter(cluster_count >= 5) |> 
    group_by(cluster) |> 
    arrange(-n) |> 
    summarise(
        tokens = paste(na.omit(token[1:15]), collapse = ", "),
        token_count = sum(n),
        cluster_size = unique(cluster_count),
        Regular = unique(Regular),
        Diverse = unique(Diverse),
        mean_tf_idf = mean(tf_idf)
    ) |>
    arrange(-token_count) |> 
    select(-mean_tf_idf, -token_count) |> 
    left_join(
        gui_tokens |> 
            left_join(
                gui_token_clusters |> 
                    filter(cluster_count >= 5) |> 
                    select(token, cluster)
            ) |> 
            filter(!is.na(cluster)) |> 
            group_by(date = floor_date(date, "1 day")) |> 
            count(cluster) |> 
            mutate(p = n/sum(n)) |> 
            ungroup() |> 
            arrange(date) |> 
            group_by(cluster) |> 
            summarise(kurt = kurt(p), spike = kurt>5) |> 
            ungroup()
    ) |> 
    filter(spike) |> 
    select(-c(kurt:spike))
rio::export(gui_topics_tbl, file = "output/gui_topics_tbl.RDS")
rio::export(gui_topics_fig, file = "output/gui_topics_figs.RDS")