library(httr2)
http_statuses <- c(
    "100" = "Continue",
    "101" = "Switching Protocols",
    "102" = "Processing",
    "103" = "Early Hints",
    "200" = "OK",
    "201" = "Created",
    "202" = "Accepted",
    "203" = "Non-Authoritative Information",
    "204" = "No Content",
    "205" = "Reset Content",
    "206" = "Partial Content",
    "207" = "Multi-Status",
    "208" = "Already Reported",
    "226" = "IM Used",
    "300" = "Multiple Choice",
    "301" = "Moved Permanently",
    "302" = "Found",
    "303" = "See Other",
    "304" = "Not Modified",
    "305" = "Use Proxy",
    "307" = "Temporary Redirect",
    "308" = "Permanent Redirect",
    "400" = "Bad Request",
    "401" = "Unauthorized",
    "402" = "Payment Required",
    "403" = "Forbidden",
    "404" = "Not Found",
    "405" = "Method Not Allowed",
    "406" = "Not Acceptable",
    "407" = "Proxy Authentication Required",
    "408" = "Request Timeout",
    "409" = "Conflict",
    "410" = "Gone",
    "411" = "Length Required",
    "412" = "Precondition Failed",
    "413" = "Payload Too Large",
    "414" = "URI Too Long",
    "415" = "Unsupported Media Type",
    "416" = "Range Not Satisfiable",
    "417" = "Expectation Failed",
    "418" = "I'm a teapot",
    "421" = "Misdirected Request",
    "422" = "Unprocessable Entity",
    "423" = "Locked",
    "424" = "Failed Dependency",
    "425" = "Too Early",
    "426" = "Upgrade Required",
    "428" = "Precondition Required",
    "429" = "Too Many Requests",
    "451" = "Unavailable For Legal Reasons",
    "500" = "Internal Server Error",
    "501" = "Not Implemented",
    "502" = "Bad Gateway",
    "503" = "Service Unavailable",
    "504" = "Gateway Timeout",
    "505" = "HTTP Version Not Supported",
    "506" = "Variant Also Negotiates",
    "507" = "Insufficient Storage",
    "508" = "Loop Detected",
    "510" = "Not Extended",
    "511" = "Network Authentication Required"
)
safari_user_agent <- "Mozilla/5.0 (Macintosh; Intel Mac OS X 14_7_6) AppleWebKit/605.1.15 (KHTML, like Gecko) Version/18.4 Safari/605.1.15"
status_codes <- domains |> 
    pull(domain) |> 
    unique() |> 
    lapply(FUN = \(u) {
        res <- tryCatch(
            request(sprintf("http://%s", u)) |> 
                req_method("HEAD") |>
                req_cache("cache") |> 
                req_user_agent(safari_user_agent) |> 
                req_error(is_error = \(resp) FALSE) |>
                req_retry(max_tries = 2) |> 
                req_perform() |> 
                resp_status(),
            error = function(e) NA
        )
        print(sprintf("%s - %s", res, u))
        return(list(u, res))
    }) |> 
    do.call(what = rbind)

status_codes <- status_codes |> 
    as_tibble() |> 
    mutate(across(everything(), unlist)) |> 
    rename(domain = V1, status_code = V2) |> 
    mutate(status_description = lapply(
        status_code, 
        \(x) http_statuses[as.character(x)]
        ) |> unlist()
    )
export(status_codes, file = "Output/status_codes.csv")