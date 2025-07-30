options(repos = c(CRAN = "https://cloud.r-project.org")) 

if (!requireNamespace("groundhog", quietly = TRUE)) {
  install.packages("groundhog")
}

pkgs <- c("dplyr","tidyr","survey","haven","stringr")

groundhog::groundhog.library(pkg = pkgs, date = "2025-07-01")
rm(pkgs)


### REUTERS DATA (not shared for legal reasons)

data <- read_sav("data/Reuters DNR2025 Germany_newsbrands.sav")
data$Q1F <- as.numeric(data$Q1F)
data_weighted <- svydesign(ids = ~1, weights = ~weight, data = data)

#Q5B: used medium last week
#Q5BI: used medium at least on three days in the last week

# Initialize result vector
source_names <- c("bbc.com","cnn.com","nytimes.com","theguardian.com","msn.com","yahoo.com","bild.de","spiegel.de",
                  "n-tv.de","welt.de","focus.de","sueddeutsche.de","faz.net","stern.de","zeit.de","ard.de","zdf.de",
                  "rtl.de","prosieben.de","Local_TV_news_websites","Stadtanzeiger_online","t-online.de","gmx.net","web.de",
                  "FUNK","local_news_websites", "regional_public_tv_news_websites","other_foreign_news")
#,"other1","other2","dont know","none of these")

# Careful! "prosieben.de" should also include "sat1.de", Furthermore, tagesschau should be added to ard

# get usage per media source (which is stored in two separate variables)
data <- data |> 
  bind_cols(
    1:28 |> 
      purrr::map_dfc(\(i) {
        b_col <- paste0("Q5B", str_pad(i, 2, pad = "0"))
        bi_col <- paste0("Q5BI", str_pad(i, 2, pad = "0"))
        new_col <- paste0("media_use", str_pad(i, 2, pad = "0"))
        
        tibble(!!new_col := data[[b_col]] + data[[bi_col]])
      })
)

# Calculate political leaning per media source  (1 = left; 7 = right)
# Give participants with the more intense media use twice the weight
media_leanings <- 1:28 |> purrr::map_dbl(function(i) {
  media_col <- paste0("media_use", str_pad(i, 2, pad = "0"))
  
  # Extract weights: 2 if media_use == 2, 1 if media_use == 1, else 0
  weights <- case_when(
    data[[media_col]] == 2 ~ 2, #change this weight if desired
    data[[media_col]] == 1 ~ 1,
    TRUE ~ 0
  )

  # Also consider survey weights
  combined_weight <- weights * data$weight
  
  # Calculate weighted mean of Q1F with combined weights
  weighted.mean(data$Q1F, w = combined_weight, na.rm = TRUE)
})

# Get popularity of each media source according to the Reuters data
# Take weights instead of raw n
popularity <- 1:28 |> purrr::map_dbl(function(i) {
  media_col <- paste0("media_use", str_pad(i, 2, pad = "0"))
  
  sum(data$weight[data[[media_col]] != 0], na.rm = TRUE)
})

# Results as df and named for clarity
media_leanings <- rbind(media_leanings, popularity)
# Convert to data frame and transpose to have one row per metric and columns = sources
media_leanings <- as.data.frame(media_leanings)

# Assign column names (media sources)
colnames(media_leanings) <- source_names

#subset to relevant sources
media_leanings <- media_leanings[, -c(20,21,25,26,27,28)]

# Assign rank based on popularity
media_leanings["rank", ] <- rank(-as.numeric(media_leanings["popularity", ]), ties.method = "min")


# adjust sat1/prosieben and ard/tagesschau to get correct matches
media_leanings$"ard.de/tagesschau.de" <- media_leanings$ard.de
media_leanings$"sat1.de/prosieben.de" <- media_leanings$prosieben.de
media_leanings$ard.de <- NULL
media_leanings$prosieben.de <- NULL

# Export data
write.csv(media_leanings, "data/reuters.csv", row.names = TRUE)


# Descriptives on the whole sample
svymean(~Q1F, data_weighted, na.rm = T)
sqrt(svyvar(~Q1F, data_weighted, na.rm = T))


