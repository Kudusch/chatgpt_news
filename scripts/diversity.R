rbind(
    api_links |> select(uid, condition, domain) |> mutate(interface = "API"),
    gui_links |> select(uid, condition, domain) |> mutate(interface = "GUI")
) |> 
    group_by(uid, interface, condition) |> 
    count(domain) |> 
    mutate(p = (n/sum(n))*100) |> 
    arrange(-n) |> 
    summarise(
        hhi = f.hhi(n),
        sdi = f.sdi(n),
        unique_sources = sum(n)
    ) |> 
    group_by(interface, condition) |> 
    summarise(
        hhi_se = sd(hhi)/sqrt(n()),
        hhi = mean(hhi),
        sdi_se = sd(sdi)/sqrt(n()),
        sdi = mean(sdi),
        unique_sources_se = sd(unique_sources)/sqrt(n()),
        unique_sources = mean(unique_sources)
    ) |> 
    pivot_longer(c(hhi, sdi, unique_sources)) |> 
    mutate(se = case_when(
        name == "hhi" ~ hhi_se,
        name == "sdi" ~ sdi_se,
        name == "unique_sources" ~ unique_sources_se
    )) |> 
    ggplot(aes(interface, value, fill = condition)) +
    geom_col(position = "dodge") +
    geom_errorbar(
        aes(ymin = value + (1.96 * se), ymax = value - (1.96 * se)), 
        position=position_dodge(.9), 
        width = .5
    ) +
    facet_wrap(vars(name), scales = "free") +
    labs(
        title = "Source diversity over time",
        subtitle = "Shannon Diversity Index (sdi): Higher values mean higher diversity,\nHerfindahl–Hirschman index (hhi): Higher values mean higher concentration",
        x = "Days",
        y = ""
    ) +
    theme(legend.position = "bottom")

ggsave(
    filename = '/Users/kudusch/Work/6_Hamburg/Projekte/DGPuK 2026/ChatGPT News/test.png', 
    plot = last_plot(),
    width = 1000,
    height = 700,
    scale = 2,
    units = "px"
)
