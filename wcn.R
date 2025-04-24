library(readr)
library(dplyr)

new_parties <- read_excel("data/new_parties_final.xlsx", 
                          sheet = "new dataset") %>% 
    mutate(election_year = as.numeric(election_year), 
           parlgov_id = as.numeric(parlgov_id))
novelty <- read_csv("data/sikk_koker/slate_novelty_dropout.csv") %>% 
    mutate(country = case_when(
        country == "BG" ~ "BGR",
        country == "CZ" ~ "CZE", 
        country == "EE" ~ "EST", 
        country == "HU" ~ "HUN", 
        country == "LT" ~ "LTU", 
        country == "LV" ~ "LVA",
        country == "PL" ~ "POL",
        country == "SI" ~ "SVN", 
        country == "SK" ~ "SVK"
    )) %>% 
    select(country, year, slate, pg_code, wcn, wcd)

new_parties_novelty <- left_join(new_parties, novelty, 
          by = c("country_name_short"="country",
                 "election_year"="year", 
                 # "party_name_short"="slate", 
                 "parlgov_id"="pg_code"))
    
writexl::write_xlsx(new_parties_novelty, "data/new_parties_novelty.xlsx")

bind_cols(
    new_parties_novelty %>% 
        filter(gnp_1 == 1) %>% 
        summarise(`WCN, genuinely new parties, 1% threshold` = mean(wcn, na.rm = TRUE)), 
    new_parties_novelty %>% 
        filter(gnp_leg == 1) %>% 
        summarise(`WCN, genuinely new parties, leg. threshold` = mean(wcn, na.rm = TRUE)),
    new_parties_novelty %>% 
        filter(pnp_1 == 1) %>% 
        summarise(`WCN, partially new parties, 1% threshold` = mean(wcn, na.rm = TRUE)),
    new_parties_novelty %>% 
        filter(pnp_leg == 1) %>% 
        summarise(`WCN, partially new parties, leg. threshold` = mean(wcn, na.rm = TRUE))
) %>% 
    knitr::kable(., digits = 2, format = "html", 
                 caption = "Weighted candidate novelty of new parties") %>% 
    writeLines("figs/wcn.html")

