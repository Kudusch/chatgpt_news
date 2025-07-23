library(rio)
library(dplyr)
library(tidyr)
library(widyr)
library(stringr)
library(lubridate)
library(jsonlite)

library(urltools)
library(httr2)
library(rvest)

safari_user_agent <- "Mozilla/5.0 (Macintosh; Intel Mac OS X 14_7_6) AppleWebKit/605.1.15 (KHTML, like Gecko) Version/18.4 Safari/605.1.15"

get_condition <- function(uid) {
    factor(
        str_extract(uid, "[AB]"), 
        levels = c("A", "B"),
        labels = c("Current news", "Current news, diverse")
    )
}

get_meta <- function(u) {
    message(sprintf("Getting %s", str_trunc(u, 100)))
    
    res <- try(
        request(u) |>
            req_user_agent(safari_user_agent) |> 
            req_cache("Cache") |> 
            req_error(is_error = \(resp) FALSE) |>
            req_retry(max_tries = 2) |> 
            req_perform(),
        FALSE
    )
    if (class(res) == "try-error") {
        return(list(u, NA, str(res)))
    }
    
    raw_dates <- try(
        res$body |> 
            read_html() |> 
            html_element("head") |> 
            html_elements("meta[property='article:published_time'], meta[name='date']") |> 
            html_attr("content") |> 
            unique(),
        FALSE
    )
    if (class(raw_dates) == "try-error") {
        return(list(u, NA, res$status_code))
    }
    
    d <- try(
        lubridate::as_date(raw_dates),
        FALSE
    )
    if (isTRUE(is.na(d))) {
        d <- try(
            lubridate::dmy(raw_dates),
            FALSE
        )
    }
    if (class(d) == "try-error") {
        return(list(u, NA, res$status_code))
    }
    list(u, d[1], res$status_code)
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

urls <- links$url |> unique()
meta <- lapply(urls, get_meta)
meta |> 
    do.call(what = rbind) |> 
    as_tibble() |> 
    unnest(everything()) |> 
    rename(url = 1, date = 2, status_code = 3)
