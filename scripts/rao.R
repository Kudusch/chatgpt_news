top_journalistic_domains_api <- api_domains |> 
    group_by(domain) |> 
    summarise(n = sum(n)) |> 
    filter(domain %in% gond$domain) |> 
    arrange(-n)
communities_diverse_api <- api_links |> 
    mutate(condition = f.get_condition(uid)) |> 
    filter(condition == "Diverse") |> 
    filter(domain %in% top_journalistic_domains_api$domain) |> 
    group_by(uid) |> 
    count(domain, sort = TRUE) |> 
    pivot_wider(names_from = domain, values_from = n, values_fill = 0) |> 
    ungroup() |> 
    tibble::column_to_rownames("uid")
traits_diverse_api <- tibble(domain = names(communities_diverse_api)) |> 
    left_join(gond) |> 
    tibble::column_to_rownames("domain") |> 
    select(-gond_lang) |> 
    mutate(gond_type = as.numeric(factor(gond_type)))
res_diverse_api <- f.rao.diversity(
    communities_diverse_api |> select(rownames(traits_diverse_api)[rownames(traits_diverse_api) %in% names(communities_diverse_api)]), 
    traits = traits_diverse_api
)
communities_regular_api <- api_links |> 
    mutate(condition = f.get_condition(uid)) |> 
    filter(condition == "Regular") |> 
    filter(domain %in% top_journalistic_domains_api$domain) |> 
    group_by(uid) |> 
    count(domain, sort = TRUE) |> 
    pivot_wider(names_from = domain, values_from = n, values_fill = 0) |> 
    ungroup() |> 
    tibble::column_to_rownames("uid")
traits_regular_api <- tibble(domain = names(communities_regular_api)) |> 
    left_join(gond) |> 
    tibble::column_to_rownames("domain") |> 
    select(-gond_lang) |> 
    mutate(gond_type = as.numeric(factor(gond_type)))
res_regular_api <- f.rao.diversity(
    communities_regular_api |> select(rownames(traits_regular_api)[rownames(traits_regular_api) %in% names(communities_regular_api)]),
    traits = traits_regular_api
)

top_journalistic_domains_gui <- gui_domains |> 
    group_by(domain) |> 
    summarise(n = sum(n)) |> 
    filter(domain %in% gond$domain) |> 
    arrange(-n)
communities_diverse_gui <- gui_links |> 
    filter(condition == "Diverse") |> 
    filter(domain %in% top_journalistic_domains_gui$domain) |> 
    group_by(uid) |> 
    count(domain, sort = TRUE) |> 
    pivot_wider(names_from = domain, values_from = n, values_fill = 0) |> 
    ungroup() |> 
    tibble::column_to_rownames("uid")
traits_diverse_gui <- tibble(domain = names(communities_diverse_gui)) |> 
    left_join(gond) |> 
    tibble::column_to_rownames("domain") |> 
    select(-gond_lang) |> 
    mutate(gond_type = as.numeric(factor(gond_type)))
res_diverse_gui <- f.rao.diversity(
    communities_diverse_gui |> select(rownames(traits_diverse_gui)[rownames(traits_diverse_gui) %in% names(communities_diverse_gui)]), 
    traits = traits_diverse_gui
)
communities_regular_gui <- gui_links |> 
    filter(condition == "Regular") |> 
    filter(domain %in% top_journalistic_domains_gui$domain) |> 
    group_by(uid) |> 
    count(domain, sort = TRUE) |> 
    pivot_wider(names_from = domain, values_from = n, values_fill = 0) |> 
    ungroup() |> 
    tibble::column_to_rownames("uid")
traits_regular_gui <- tibble(domain = names(communities_regular_gui)) |> 
    left_join(gond) |> 
    tibble::column_to_rownames("domain") |> 
    select(-gond_lang) |> 
    mutate(gond_type = as.numeric(factor(gond_type)))
res_regular_gui <- f.rao.diversity(
    communities_regular_gui |> select(rownames(traits_regular_gui)[rownames(traits_regular_gui) %in% names(communities_regular_gui)]), 
    traits = traits_regular_gui
)

t.test(res_diverse_gui$FunRao, res_regular_gui$FunRao)
t.test(res_diverse_api$FunRao, res_regular_api$FunRao)

rbind(
    res_diverse_gui$FunRao |> 
        tibble::enframe(name = "uid", value = "rao"),
    res_diverse_api$FunRao |> 
        tibble::enframe(name = "uid", value = "rao"),
    res_regular_gui$FunRao |> 
        tibble::enframe(name = "uid", value = "rao"),
    res_regular_api$FunRao |> 
        tibble::enframe(name = "uid", value = "rao")
) |> rio::export(file = "output/rao_results.RDS")
