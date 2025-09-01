# Functions and colors ----
options(ggplot2.discrete.colour = c("#4e79a7", "#f28e2b", "#e15759", "#76b7b2", "#59a14f", "#edc948", "#b07aa1", "#ff9da7", "#9c755f", "#bab0ac"))
options(ggplot2.discrete.fill = c("#4e79a7", "#f28e2b", "#e15759", "#76b7b2", "#59a14f", "#edc948", "#b07aa1", "#ff9da7", "#9c755f", "#bab0ac"))

##
springer_domains <- c("welt.de", "upday.com", "bild.de", "finanzen.net", "businessinsider.de", "bz-berlin.de", "autobild.de")

## Shannon Diversity Index: Higher values mean higher diversity
f.sdi <- function(counts) {
    props <- counts / sum(counts)
    -sum(props * log(props))
}
## Herfindahl–Hirschman index: Higher values mean higher concentration
f.hhi <- function(counts, limit=NA) {
    if (!is.na(limit)) {
        counts <- counts[1:limit]
    }
    props <- (counts/sum(counts, na.rm = TRUE))
    sum(props^2, na.rm = TRUE)
}
f.skew <- function(x) {
    sum(((x - mean(x))^3))/((length(x)-1)*(sd(x)^3))
}
f.get_condition <- function(uid) {
    factor(
        str_extract(uid, "[AB]"), 
        levels = c("A", "B"),
        labels = c("Regular", "Diverse")
    )
}