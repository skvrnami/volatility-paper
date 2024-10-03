library(dplyr)
library(rvest)
library(readxl)

lipcean_data <- readRDS("data/data_final_lipcean.rds")
gdp_power <- readxl::read_excel("data/currency_data/gdp_pp.xlsx") %>% 
    janitor::clean_names() %>% 
    select(country, x2025:x1990) %>% 
    mutate(across(x2025:x1990 & where(is.character), 
                  function(x) as.numeric(na_if(x, "-")))) %>% 
    tidyr::pivot_longer(., cols = 2:ncol(.), names_to = "year", 
                        values_to = "gdp_ppp") %>% 
    mutate(year = as.numeric(stringr::str_extract(year, "[0-9]{4}")), 
           country_code = case_when(
               country == "Bulgaria" ~ "BGR",
               country == "Czechia" ~ "CZE",
               country == "Estonia" ~ "EST",
               country == "Croatia" ~ "HRV",
               country == "Hungary" ~ "HUN",
               country == "Lithuania" ~ "LTU",
               country == "Latvia" ~ "LVA",
               country == "Poland" ~ "POL",
               country == "Romania" ~ "ROU", 
               country == "Slovakia" ~ "SVK",
               country == "Slovenia" ~ "SVN"
            )) %>% 
    filter(!is.na(country_code)) %>% 
    select(-country)

exchange_rates <- read_excel("data/currency_data/ert_h_eur_a_page_spreadsheet.xlsx", 
                             sheet = 3, skip = 8, na = ":") %>% 
    tidyr::pivot_longer(., 2:ncol(.), 
                        names_to = "year", 
                        values_to = "exchange_rate") %>% 
    filter(!is.na(exchange_rate)) %>% 
    filter(year %in% as.character(1990:2023)) %>% 
    mutate(across(c(year, exchange_rate), as.numeric)) %>% 
    filter(!is.na(exchange_rate)) %>% 
    rename(currency = TIME) %>% 
    mutate(currency = case_when(
        currency == "Estonian Kroon" ~ "EST",
        currency == "Lithuanian litas" ~ "LTU",
        currency == "Latvian lats" ~ "LVA",
        currency == "Slovenian tolar" ~ "SVN",
        currency == "Slovak koruna" ~ "SVK",
        TRUE ~ NA_character_
    )) %>% 
    filter(!is.na(currency)) %>% 
    filter(
        (currency == "EST" & year < 2011) |
        (currency == "LTU" & year < 2015) | 
        (currency == "LVA" & year < 2014) | 
        (currency == "SVN" & year < 2007) | 
        (currency == "SVK" & year < 2009)
    )

lipcean_data_adjusted <- lipcean_data %>% 
    left_join(., exchange_rates, by = c("country_name_short"="currency", "year")) %>% 
    mutate(exchange_rate = if_else(is.na(exchange_rate), 1, exchange_rate)) %>% 
    left_join(., gdp_power, by = c("country_name_short"="country_code", "year")) %>%
    mutate(
        dpf_statutory_eur = dpf_statutory_nc / exchange_rate, 
        dpf_statutory_eur_2 = dpf_statutory_nc_2 / exchange_rate, 
        dpf_elections_eur = dpf_elections_nc / exchange_rate, 
        dpf_elections_eur_2 = dpf_elections_nc_2 / exchange_rate, 
        dpf_statutory_pp = dpf_statutory_eur / gdp_ppp, 
        dpf_statutory_pp_2 = dpf_statutory_eur_2 / gdp_ppp, 
        dpf_elections_pp = dpf_elections_eur / gdp_ppp, 
        dpf_elections_pp_2 = dpf_elections_eur_2 / gdp_ppp
    ) %>% 
    relocate(starts_with("dpf"), .after = last_col())

library(ggplot2)
ggplot(lipcean_data_adjusted, aes(x = year, y = dpf_statutory_usd, 
                                  colour = country_name_short)) + 
    geom_line() + 
    geom_point()

ggplot(lipcean_data_adjusted, aes(x = year, y = dpf_statutory_eur, 
                                  colour = country_name_short)) + 
    geom_line() + 
    geom_point()

ggplot(lipcean_data_adjusted, aes(x = year, y = dpf_statutory_eur_2, 
                                  colour = country_name_short)) + 
    geom_line() + 
    geom_point()

ggplot(lipcean_data_adjusted, aes(x = year, y = dpf_elections_eur, 
                                  colour = country_name_short)) + 
    geom_line() + 
    geom_point()

ggplot(lipcean_data_adjusted, aes(x = year, y = dpf_elections_eur_2, 
                                  colour = country_name_short)) + 
    geom_line() + 
    geom_point()
