# Merge data on new party support with Lipcean dataset on party finances

library(dplyr)

final_data <- readRDS("data/data_final.rds")
lipcean <- read.csv("data/lipcean2021/dpf_dataset.csv") %>% 
    mutate(country_year = paste0(country_id, "_", year)) %>% 
    filter(country_year %in% final_data$country_year) %>% 
    select(-c(country_name, country_id, year))

final_data_lipcean <- left_join(final_data, lipcean, by = "country_year") %>% 
    mutate(year = as.numeric(stringr::str_extract(country_year, "[0-9]+")), 
           country_name_short = stringr::str_extract(country_year, "[A-Z]+"),
           post_crisis = as.numeric(year > 2008),
           region_type = case_when(
               country_name_short %in% c("HRV", "CZE", "HUN", 
                                         "ROU", "SVN") ~ "Once stable region", 
               country_name_short %in% c("EST", "LTU", "LVA", "POL", 
                                         "BGR", "SVK") ~ "Never stable region"
           )) %>% 
    group_by(country_name_short) %>% 
    mutate(election_no = row_number() + 1) %>% 
    ungroup %>% 
    group_by(country_name_short, post_crisis) %>% 
    mutate(
        post_crisis_election2 = case_when(
            post_crisis == 0 ~ "Before crisis", 
            post_crisis == 1 & row_number() %in% 1:2 ~ "First/Second post-crisis election", 
            post_crisis == 1 ~ "Other post-crisis election"
        ),
        time_2008 = row_number()
    ) %>% 
    ungroup %>% 
    mutate(
        r_year = year - 1991, 
        crisis_election = as.numeric(
            post_crisis_election2 == "First/Second post-crisis election"
        ), 
        fst_snd_election = case_when(
            country_name_short == "BGR" & year %in% c(1990, 1991) ~ TRUE, 
            country_name_short == "CZE" & year %in% c(1990, 1992) ~ TRUE, 
            country_name_short == "EST" & year %in% c(1990, 1992) ~ TRUE, 
            country_name_short == "HRV" & year %in% c(1990, 1992) ~ TRUE, 
            country_name_short == "HUN" & year %in% c(1990, 1994) ~ TRUE, 
            country_name_short == "LTU" & year %in% c(1990, 1993) ~ TRUE, 
            country_name_short == "LVA" & year %in% c(1990, 1993) ~ TRUE, 
            country_name_short == "POL" & year %in% c(1991, 1993) ~ TRUE, 
            country_name_short == "ROU" & year %in% c(1990, 1992) ~ TRUE, 
            country_name_short == "SVK" & year %in% c(1990, 1992) ~ TRUE, 
            country_name_short == "SVN" & year %in% c(1990, 1992) ~ TRUE, 
            TRUE ~ FALSE
        ), 
        generation = case_when(
            country_name_short == "BGR" & year %in% c(1994, 1997) ~ "2nd generation",
            country_name_short == "BGR" & year %in% c(2001, 2005) ~ "3rd generation",
            country_name_short == "CZE" & year %in% c(1996, 1998) ~ "2nd generation",
            country_name_short == "CZE" & year %in% c(2002, 2006) ~ "3rd generation",
            country_name_short == "CZE" & year %in% c(2010) ~ "4th generation",
            country_name_short == "EST" & year %in% c(1995) ~ "2nd generation",
            country_name_short == "EST" & year %in% c(1999, 2003) ~ "3rd generation",
            country_name_short == "HRV" & year %in% c(2003, 2007, 2011) ~ "3rd generation",
            country_name_short == "HRV" & year %in% c(2015) ~ "4th generation",
            country_name_short == "HUN" & year %in% c(1994) ~ "2nd generation",
            country_name_short == "HUN" & year %in% c(1998, 2002, 2006) ~ "3rd generation",
            country_name_short == "HUN" & year %in% c(2010) ~ "4th generation",
            country_name_short == "LTU" & year %in% c(1996) ~ "2nd generation",
            country_name_short == "LTU" & year %in% c(2000, 2004) ~ "3rd generation",
            country_name_short == "LVA" & year %in% c(1995) ~ "2nd generation",
            country_name_short == "LVA" & year %in% c(1998, 2002) ~ "3rd generation",
            country_name_short == "POL" & year %in% c(1993) ~ "2nd generation",
            country_name_short == "POL" & year %in% c(1997, 2001) ~ "3rd generation",
            country_name_short == "ROU" & year %in% c(1996) ~ "2nd generation",
            country_name_short == "ROU" & year %in% c(2000, 2004, 2008) ~ "3rd generation",
            country_name_short == "ROU" & year %in% c(2012) ~ "4th generation",
            country_name_short == "SVK" & year %in% c(1994, 1998) ~ "2nd generation",
            country_name_short == "SVK" & year %in% c(2002, 2006) ~ "3rd generation",
            country_name_short == "SVN" & year %in% c(1996, 2000, 2004) ~ "2nd generation",
            country_name_short == "SVN" & year %in% c(2008) ~ "3rd generation",
            country_name_short == "SVN" & year %in% c(2011) ~ "4th generation", 
            post_crisis_election2 == "First/Second post-crisis election" ~ "4th generation", 
            post_crisis_election2 == "Other post-crisis election" ~ "Other post-crisis"
        )
    )

saveRDS(final_data_lipcean, "data/data_final_lipcean.rds")    
haven::write_sav(final_data_lipcean, "data/data_final_lipcean.sav")    
