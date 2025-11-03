f.test_domain <- function(data, test_domain) {
    fit <- data |> 
        mutate(domain = domain == test_domain) |> 
        select(domain, condition) |>
        filter(if_all(everything(), \(x) !is.na(x))) |> 
        count(domain, condition) |> 
        pivot_wider(names_from = condition, values_from = n, values_fill = 0) |> 
        tibble::column_to_rownames("domain") |> 
        fisher.test()
    list(
        "domain" = test_domain, 
        "p.value" = fit$p.value, 
        "odds.ratio" = as.numeric(fit$estimate), 
        "log.odds.ratio" = log(as.numeric(fit$estimate))
    )
}

api_fisher <- api_domains |> 
    pull(domain) |> 
    unique() |> 
    lapply(X = _, \(test_domain) f.test_domain(api_links, test_domain)) |> 
    do.call(what = "rbind") |> 
    as_tibble() |> 
    mutate(across(everything(), unlist)) |> 
    mutate(is_sig = p.value <= 0.01)
gui_fisher <- gui_domains |> 
    pull(domain) |> 
    unique() |> 
    lapply(X = _, \(test_domain) f.test_domain(gui_links, test_domain)) |> 
    do.call(what = "rbind") |> 
    as_tibble() |> 
    mutate(across(everything(), unlist)) |> 
    mutate(is_sig = p.value <= 0.01)


f.test_domain(api_links, "tagesschau.de")

api_fisher |> 
    mutate(cat = case_when(
        !is_sig ~ "both",
        log.odds.ratio > 0 ~ "diverse",
        log.odds.ratio < 0 ~ "regular"
    )) |> 
    count(cat)

gui_fisher |> 
    mutate(cat = case_when(
        !is_sig ~ "both",
        log.odds.ratio > 0 ~ "diverse",
        log.odds.ratio < 0 ~ "regular"
    )) |> 
    count(cat)


api_fisher |> 
    ggplot(aes(x = log.odds.ratio, fill = is_sig)) +
    geom_histogram()
