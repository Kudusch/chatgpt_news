library(rollama)
library(tibble)
library(dplyr)
library(stringr)
library(irr)

library(pbapply)

zero_shot <- function(instruction, content, model = NA) {
    if (is.na(model)) {
        model <- "phi4"
    }
    q <- tribble(
        ~role,    ~content,
        "system", instruction,
        "user",   sprintf("text: %s", content)
    )
    res <- query(q, model = model, screen = FALSE, output = "data.frame")
    res$response <- res$response |> str_squish() |> str_to_lower()
    res$query <- content
    return(res)
}
zero_shots <- function(instruction, content_list, model = NA) {
    pblapply(content_list, \(content) zero_shot(instruction, content, model)) |> 
        do.call(what = rbind) |> 
        as_tibble()
}
n_shot <- function(instruction, content, examples, model = NA) {
    if (is.na(model)) {
        model <- "phi4"
    }
    q <- tribble(
        ~role,    ~content,
        "system", instruction,
        "user", sprintf("text: %s", examples$content),
        "assistant", sprintf("Category: %s", examples$answer),
        "user", sprintf("text: %s", content)
    )
    res <- query(q, model = model, screen = FALSE, output = "data.frame")
    res$response <- res$response |> str_squish() |> str_to_lower()
    res$query <- content
    return(res)
}
compare_models_zero <- function(instruction, content, models = NA) {
    if (any(is.na(models))) {
        models <- c("llama3.1:latest", "mistral:latest")
    }
    responses <- lapply(models, \(model) {
        print(sprintf("Running model: %s", model))
        return(zero_shot(instruction=instruction, content=content, model=model))
    }) |> 
        do.call(what = "rbind")
    
    test_data <- responses |>
        select(response) |>
        mutate(response = as.numeric(factor(response)))
    
    return(list(
        "agree"=irr::agree(t(test_data))$value,
        "kripp.alpha"=irr::kripp.alpha(as.matrix(test_data))$value,
        "responses"=select(responses, -role)
    ))
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
domains <- links |> 
    count(domain, sort = TRUE)

# Ownership (public service, private, independent/cooperative)
# Media Type (legacy print, broadcaster, online born)
# Geographic scope (local, regional, national, foreign-national, global)


instruction <- paste(
    "You sort websites into categories.",
    "There are 7 categories:",
    "public_broadcaster : A website that is connected to a public broadcaster, e.g. tageschschau.de or bbc.com",
    "private_newsmedia : A website that is connected to a privately owned news/media organization, e.g. rtl.de or spiegel.de",
    "local_media : A website that is connected to a local news/media organization, e.g. rtl.de or spiegel.de",
    "encyclopedias : A website that is connected to an encyclopedia or online reference, e.g. wikipedia.org or duden.de",
    "alternative_media : A website that is connected to a (news) media organization that defines itself as an alternative to hegemonial 'mainstream', e.g. tichyseinblick.de, uebermedien.de",
    "organization : A website that is connected to an organization, institution, buisness or body of government, e.g. bundestag.de, sparkasse.de",
    "other : Any other website that does not fit into the other categories",
    "News websites periodically publish content that has public relevance.",
    "Given a link to a website, answer either 'news' or 'not news'."
)
intro <- "To what category does this website belong to? Answer public_broadcaster, private_newsmedia, encyclopedias, alternative_media, organization, other\n%s"

res <- zero_shots(instruction, sprintf(intro, domains$domain)) |> 
    mutate(url = str_extract(query, "\\n(.*)", 1)) |> 
    mutate(cat = str_extract(response, "public_broadcaster|private_newsmedia|encyclopedias|alternative_media|organization|other")) |> 
    relocate(url, cat)

saveRDS(res, file = "domains.RDS")

# examples <- tribble(
#     ~content, ~answer,
#     "the pizza tastes terrible", "Category: Negative"
# )
# n_shot(instruction, content, examples, model = "mistral:latest")
# n_shot(instruction, content, examples, model = "llama3.1:latest")