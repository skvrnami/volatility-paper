library(dplyr)
library(readxl)

new_parties <- read_excel("data/new_parties", 
                          sheet = "new dataset")

all_elections <- c(
    "BGR_1991", "BGR_1994", "BGR_1997", "BGR_2001", "BGR_2005", "BGR_2009",
    "BGR_2013", "BGR_2014", "BGR_2017", "CZE_1992", "CZE_1996", "CZE_1998",
    "CZE_2002", "CZE_2006", "CZE_2010", "CZE_2013", "CZE_2017", "EST_1995",
    "EST_1999", "EST_2003", "EST_2007", "EST_2011", "EST_2015", "EST_2019",
    "HRV_2003", "HRV_2007", "HRV_2011", "HRV_2015", "HRV_2016", "HRV_2020",
    "HUN_1994", "HUN_1998", "HUN_2002", "HUN_2006", "HUN_2010", "HUN_2014",
    "HUN_2018", "LTU_1996", "LTU_2000", "LTU_2004", "LTU_2008", "LTU_2012",
    "LTU_2016", "LTU_2020", "LVA_1995", "LVA_1998", "LVA_2002", "LVA_2006",
    "LVA_2010", "LVA_2011", "LVA_2014", "LVA_2018", "POL_1993", "POL_1997",
    "POL_2001", "POL_2005", "POL_2007", "POL_2011", "POL_2015", "POL_2019",
    "ROU_1992", "ROU_1996", "ROU_2000", "ROU_2004", "ROU_2008", "ROU_2012",
    "ROU_2016", "ROU_2020", "SVK_1992", "SVK_1994", "SVK_1998", "SVK_2002",
    "SVK_2006", "SVK_2010", "SVK_2012", "SVK_2016", "SVK_2020", "SVN_1996",
    "SVN_2000", "SVN_2004", "SVN_2008", "SVN_2011", "SVN_2014", "SVN_2018"
)

all_elections_df <- tibble(
    country_year = all_elections, 
)

cv_1 <- new_parties %>% 
    group_by(country_name_short, election_year) %>% 
    filter(gnp_1 == 1) %>% 
    summarise(
        np_share_cv_1 = sum(vote_share), 
        np_number_cv_1 = n()
    )

pnp_1 <- new_parties %>% 
    group_by(country_name_short, election_year) %>% 
    filter(pnp_1 == 1) %>% 
    summarise(
        np_share_pnp_1 = sum(vote_share), 
        np_number_pnp_1 = n()
    )

cv_leg <- new_parties %>% 
    group_by(country_name_short, election_year) %>% 
    filter(gnp_leg == 1) %>% 
    summarise(
        np_share_cv_leg = sum(vote_share), 
        np_number_cv_leg = n()
    )

pnp_leg <- new_parties %>% 
    group_by(country_name_short, election_year) %>% 
    filter(pnp_leg == 1) %>% 
    summarise(
        np_share_pnp_leg = sum(vote_share), 
        np_number_pnp_leg = n()
    )

new_parties_data <- purrr::reduce(
    list(cv_1, pnp_1, cv_leg, pnp_leg),
    function(x, y) full_join(x, y, by = c("country_name_short", "election_year"))
) %>%
    mutate(country_year = paste0(country_name_short, "_", election_year)) %>% 
    full_join(all_elections_df, ., by = c("country_year")) %>% 
    mutate(across(starts_with("np"), ~if_else(is.na(.x), 0, .x))) %>%
    mutate(
        np_share_nc_leg = np_share_cv_leg + np_share_pnp_leg,
        np_share_nc_1 = np_share_cv_1 + np_share_pnp_1,
        np_number_nc_leg = np_number_cv_leg + np_number_pnp_leg,
        np_number_nc_1 = np_number_cv_1 + np_number_pnp_1,
        election_year = as.numeric(election_year)
    ) %>%
    mutate(across(starts_with("np_share"), ~round(.x, 2))) 

punish_data <- readRDS("data/gov_punish_data.rds")

final_data <- left_join(new_parties_data, punish_data, by = "country_year") %>% 
    mutate(country_name_short = stringr::str_extract(country_year, "[A-Z]+"), 
           election_year = as.numeric(stringr::str_extract(country_year, "[0-9]+")))

saveRDS(final_data, "data/data_final.rds")
