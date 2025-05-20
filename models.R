library(tidyr)
library(prais)
library(broom)
library(psych)
library(dplyr)
library(haven)
library(lmtest)
library(readxl)
library(ggplot2)
library(janitor)
library(estimatr)
library(ggeffects)
library(modelsummary)
    
new_parties_data <- readRDS("data/data_final.rds")

new_parties_data |> 
    select(country_name_short, election_year, 
            contains("share"), contains("number")) |> 
    writexl::write_xlsx(path = "data/new_parties_aggregated.xlsx")

data <- new_parties_data %>% 
    mutate(year = as.numeric(stringr::str_extract(country_year, "[0-9]+")), 
           country_name_short = stringr::str_extract(country_year, "[A-Z]+"),
           post_crisis = as.numeric(year > 2008),
           post_accession = case_when(
            country_name_short %in% c("CZE", "EST", "LTU", "LVA", "POL", "HUN", "SVK", "SVN") &
                year >= 2004 ~ 1,
            country_name_short %in% c("BGR", "ROU") & year >= 2007 ~ 1,
            country_name_short == "HRV" & year >= 2013 ~ 1,
            TRUE ~ 0
           ),
           crisis_election_2016 = as.numeric(year > 2008 & year <= 2016),
           crisis_election_2014 = as.numeric(year > 2008 & year <= 2014), 
           crisis_election_2015 = as.numeric(year > 2008 & year <= 2015),
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
        post_crisis_election1 = case_when(
            post_crisis == 0 ~ "Before crisis", 
            post_crisis == 1 & row_number() == 1 ~ "First post-crisis election", 
            post_crisis == 1 ~ "Other post-crisis election"
        ),
        post_crisis_election2 = case_when(
            post_crisis == 0 ~ "Before crisis", 
            post_crisis == 1 & row_number() %in% 1:2 ~ "First/Second post-crisis election", 
            post_crisis == 1 ~ "Other post-crisis election"
        ),
        post_crisis_election3 = case_when(
            post_crisis == 0 ~ "Before crisis", 
            post_crisis == 1 & row_number() %in% 1:3~ "1-3 post-crisis election", 
            post_crisis == 1 ~ "Other post-crisis election"
        ),
        time_2008 = row_number(), 
        post_accession_election1 = case_when(
            post_accession == 0 ~ "Before accession", 
            post_accession == 1 & row_number() == 1 ~ "First post-accession election", 
            post_accession == 1 ~ "Other post-accession election"
        ),
        post_accession_election2 = case_when(
            post_accession == 0 ~ "Before accession", 
            post_accession == 1 & row_number() %in% 1:2 ~ "First/second post-accession election", 
            post_accession == 1 ~ "Other post-accession election"
        )
    ) %>% 
    ungroup %>% 
    mutate(
        r_year = year - 1991, 
        post_crisis_election1 = as.numeric(post_crisis_election1 == "First post-crisis election"),
        post_accession_election1 = as.numeric(post_accession_election1 == "First post-accession election"),
        post_accession_election2 = as.numeric(post_accession_election2 == "First/second post-accession election"),
        crisis_election = as.numeric(
            post_crisis_election2 == "First/Second post-crisis election"
        ), 
        crisis_election3 = as.numeric(
            post_crisis_election3 == "1-3 post-crisis election"
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
        )
    )

restrained_change <- read_dta("data/casal_bertoa_weber_data/restrained_change_repl.dta") %>% 
    as_factor() %>% 
    select(country, year = yearmonth, restraint_pre) %>% 
    mutate(year = as.numeric(stringr::str_extract(year, "[0-9]{4}")), 
           country = as.character(country)) %>% 
    mutate(country = case_when(
        country == "BUL" ~ "BGR",
        country == "CRO" ~ "HRV",
        country == "LAT" ~ "LVA",
        country == "LIT" ~ "LTU",
        country == "ROM" ~ "ROU",
        country == "SLK" ~ "SVK",
        country == "SLV" ~ "SVN",
        TRUE ~ country
    ))
    

restraint_data <- restrained_change %>% 
    filter(year > 1990) %>% 
    select(country, restraint_pre) %>% 
    unique() %>% 
    filter(country %in% data$country_name_short) %>% 
    mutate(restraint_pre_norm = restraint_pre - mean(restraint_pre))

data_restrained <- data %>% 
    mutate(country_name_short = factor(country_name_short)) %>% 
    rename(rank_election_within_country = election_no) %>% 
    group_by(country_name_short) %>% 
    mutate(n_postcrisis = cumsum(post_crisis)) %>% 
    ungroup %>% 
    group_by(country_name_short, post_crisis) %>% 
    arrange(desc(year)) %>% 
    mutate(
        row = row_number(),
        n_precrisis = if_else(
        post_crisis == 0, row, 0
    )) %>% 
    select(-row) %>% 
    ungroup %>% 
    left_join(., restraint_data, by = c("country_name_short"="country"))
    
# np_share_nc_1 = všechny nové strany
# np_share_cv_1 = úplně nové strany
# np_share_pnp_1 = částečně nové strany

used_data <- data %>% 
    mutate(country_name_short = factor(country_name_short)) %>% 
    rename(rank_election_within_country = election_no) %>% 
    select(country_name_short, year, everything())

data_restrained <- data_restrained %>% 
    select(country_name_short, year, everything())

# Models included in paper -----------
## Tab 1 -----------------------------
m1a_pw <- prais_winsten(np_share_nc_1 ~ r_year, data = used_data, 
                        index = c("country_name_short", "year"), 
                        twostep = TRUE, panelwise = TRUE, rhoweight = "T1") %>% 
    coeftest(., vcov. = vcovPC(., pairwise = TRUE), save = TRUE)
m1b_pw <- prais_winsten(np_share_nc_1 ~ r_year + crisis_election, 
              data = used_data, index = c("country_name_short", "year"), 
              twostep = TRUE, panelwise = TRUE, rhoweight = "T1") %>% 
    coeftest(., vcov. = vcovPC(., pairwise = TRUE), save = TRUE)
m1c_pw <- prais_winsten(np_share_nc_1 ~ r_year + crisis_election3, 
                        data = used_data, index = c("country_name_short", "year"), 
                        twostep = TRUE, panelwise = TRUE, rhoweight = "T1") %>% 
    coeftest(., vcov. = vcovPC(., pairwise = TRUE), save = TRUE)

m2a_pw <- prais_winsten(np_share_cv_1 ~ r_year, data = used_data, 
                        index = c("country_name_short", "year"), 
                        twostep = TRUE, panelwise = TRUE, rhoweight = "T1") %>% 
    coeftest(., vcov. = vcovPC(., pairwise = TRUE), save = TRUE)
m2b_pw <- prais_winsten(np_share_cv_1 ~ r_year + crisis_election, 
                        data = used_data, 
                        index = c("country_name_short", "year"), 
                        twostep = TRUE, panelwise = TRUE, rhoweight = "T1") %>% 
    coeftest(., vcov. = vcovPC(., pairwise = TRUE), save = TRUE)
m2c_pw <- prais_winsten(np_share_cv_1 ~ r_year + crisis_election3, 
                        data = used_data, 
                        index = c("country_name_short", "year"), 
                        twostep = TRUE, panelwise = TRUE, rhoweight = "T1") %>% 
    coeftest(., vcov. = vcovPC(., pairwise = TRUE), save = TRUE)

m3a_pw <- prais_winsten(np_share_pnp_1 ~ r_year, data = used_data, 
                        index = c("country_name_short", "year"), 
                        twostep = TRUE, panelwise = TRUE, rhoweight = "T1") %>% 
    coeftest(., vcov. = vcovPC(., pairwise = TRUE), save = TRUE)
m3b_pw <- prais_winsten(np_share_pnp_1 ~ r_year + crisis_election, 
                        data = used_data, index = c("country_name_short", "year"), 
                        twostep = TRUE, panelwise = TRUE, rhoweight = "T1") %>% 
    coeftest(., vcov. = vcovPC(., pairwise = TRUE), save = TRUE)
m3c_pw <- prais_winsten(np_share_pnp_1 ~ r_year + crisis_election3, 
                        data = used_data, index = c("country_name_short", "year"), 
                        twostep = TRUE, panelwise = TRUE, rhoweight = "T1") %>% 
    coeftest(., vcov. = vcovPC(., pairwise = TRUE), save = TRUE)

PW_NOTE <- "Models estimated using Prais-Winsten regression with panel-corrected standard errors."
modelsummary(
    list("All new parties" = m1a_pw, 
         "All new parties" = m1b_pw, 
         "All new parties" = m1c_pw, 
         "Genuinely new parties" = m2a_pw, 
         "Genuinely new parties" = m2b_pw, 
         "Genuinely new parties" = m2c_pw, 
         "Partially new parties" = m3a_pw, 
         "Partially new parties" = m3b_pw, 
         "Partially new parties" = m3c_pw), 
    coef_rename = c(
        "r_year"="Year (0 = 1991)", 
        "crisis_election"="Two elections after 2008", 
        "crisis_election3"="Three elections after 2008"
    ),
    stars = TRUE, 
    fmt = 2,
    gof_map = c("nobs", "r.squared", "adj.r.squared"),
    notes = PW_NOTE,
    output = "figs/tab1_final.html"
)

## Tab 2 --------------------------------------------------
m10_pw_nc_a <- prais_winsten(np_share_nc_1 ~ r_year + crisis_election,
    data = data_restrained, 
    index = c("country_name_short", "year"), 
    twostep = TRUE, panelwise = TRUE, rhoweight = "T1") %>% 
coeftest(., vcov. = vcovPC(., pairwise = TRUE), save = TRUE)


m10_pw_nc <- prais_winsten(np_share_nc_1 ~ r_year + crisis_election * restraint_pre_norm,
                        data = data_restrained, 
                        index = c("country_name_short", "year"), 
                        twostep = TRUE, panelwise = TRUE, rhoweight = "T1") %>% 
    coeftest(., vcov. = vcovPC(., pairwise = TRUE), save = TRUE)

m10_pw_cv_a <- prais_winsten(np_share_cv_1 ~ r_year + crisis_election,
    data = data_restrained, 
    index = c("country_name_short", "year"), 
    twostep = TRUE, panelwise = TRUE, rhoweight = "T1") %>% 
coeftest(., vcov. = vcovPC(., pairwise = TRUE), save = TRUE)


m10_pw_cv <- prais_winsten(np_share_cv_1 ~ r_year + crisis_election * restraint_pre_norm,
                       data = data_restrained, 
                       index = c("country_name_short", "year"), 
                       twostep = TRUE, panelwise = TRUE, rhoweight = "T1") %>% 
    coeftest(., vcov. = vcovPC(., pairwise = TRUE), save = TRUE)

m10_pw_pnp_a <- prais_winsten(np_share_pnp_1 ~ r_year + crisis_election,
    data = data_restrained, 
    index = c("country_name_short", "year"), 
    twostep = TRUE, panelwise = TRUE, rhoweight = "T1") %>% 
coeftest(., vcov. = vcovPC(., pairwise = TRUE), save = TRUE)

m10_pw_pnp <- prais_winsten(np_share_pnp_1 ~ r_year + crisis_election * restraint_pre_norm,
                           data = data_restrained, 
                           index = c("country_name_short", "year"), 
                           twostep = TRUE, panelwise = TRUE, rhoweight = "T1") %>% 
    coeftest(., vcov. = vcovPC(., pairwise = TRUE), save = TRUE)

modelsummary(
    list(
        "All new parties" = m10_pw_nc_a, 
        "All new parties" = m10_pw_nc, 
        "Partially new parties" = m10_pw_pnp_a,
        "Partially new parties" = m10_pw_pnp,
        "Genuinely new parties" = m10_pw_cv_a, 
        "Genuinely new parties" = m10_pw_cv
         ), 
    coef_rename = c(
        "r_year"="Year (0 = 1991)", 
        "crisis_election"="Two elections after 2008", 
        "region_typeOnce stable region" = "Once stable region", 
        "restraint_pre_norm" = "Party restraint (pre-crisis)", 
        "crisis_election:restraint_pre_norm" = "Two elections after 2008 × Party restraint (pre-crisis)"
    ),
    stars = TRUE,
    notes = PW_NOTE,
    fmt = 2,
    gof_map = c("nobs", "r.squared", "adj.r.squared"),
    output = "figs/tab2_2elections_after.html"
)

## Tab 2 - crisis 2016 ------------------------------------
m10_pw_nc_2016 <- prais_winsten(np_share_nc_1 ~ r_year + crisis_election_2016 * restraint_pre_norm,
                           data = data_restrained, 
                           index = c("country_name_short", "year"), 
                           twostep = TRUE, panelwise = TRUE, rhoweight = "T1") %>% 
    coeftest(., vcov. = vcovPC(., pairwise = TRUE), save = TRUE)

m10_pw_cv_2016 <- prais_winsten(np_share_cv_1 ~ r_year + crisis_election_2016 * restraint_pre_norm,
                           data = data_restrained, 
                           index = c("country_name_short", "year"), 
                           twostep = TRUE, panelwise = TRUE, rhoweight = "T1") %>% 
    coeftest(., vcov. = vcovPC(., pairwise = TRUE), save = TRUE)

m10_pw_pnp_2016 <- prais_winsten(np_share_pnp_1 ~ r_year + crisis_election_2016 * restraint_pre_norm,
                            data = data_restrained, 
                            index = c("country_name_short", "year"), 
                            twostep = TRUE, panelwise = TRUE, rhoweight = "T1") %>% 
    coeftest(., vcov. = vcovPC(., pairwise = TRUE), save = TRUE)

modelsummary(
    list("All new parties" = m10_pw_nc_2016, 
         "Genuinely new parties" = m10_pw_cv_2016, 
         "Partially new parties" = m10_pw_pnp_2016), 
    coef_rename = c(
        "r_year"="Year (0 = 1991)", 
        "crisis_election_2016"="Crisis elections (2009-2016)", 
        "region_typeOnce stable region" = "Once stable region", 
        "restraint_pre_norm" = "Party restraint (pre-crisis)", 
        "crisis_election_2016:restraint_pre_norm" = "Crisis elections (2009-2016) × Party restraint (pre-crisis)"
    ),
    stars = TRUE,
    notes = PW_NOTE,
    fmt = 2,
    gof_map = c("nobs", "r.squared", "adj.r.squared"),
    output = "figs/tab2_2016.html"
)

## Tab 2 - 3 elections ------------------------------------

m10_pw_nc <- prais_winsten(np_share_nc_1 ~ r_year + crisis_election3,
                           data = data_restrained, 
                           index = c("country_name_short", "year"), 
                           twostep = TRUE, panelwise = TRUE, rhoweight = "T1") %>% 
    coeftest(., vcov. = vcovPC(., pairwise = TRUE), save = TRUE)

m10_pw_cv <- prais_winsten(np_share_cv_1 ~ r_year + crisis_election3,
                           data = data_restrained, 
                           index = c("country_name_short", "year"), 
                           twostep = TRUE, panelwise = TRUE, rhoweight = "T1") %>% 
    coeftest(., vcov. = vcovPC(., pairwise = TRUE), save = TRUE)

m10_pw_pnp <- prais_winsten(np_share_pnp_1 ~ r_year + crisis_election3,
                            data = data_restrained, 
                            index = c("country_name_short", "year"), 
                            twostep = TRUE, panelwise = TRUE, rhoweight = "T1") %>% 
    coeftest(., vcov. = vcovPC(., pairwise = TRUE), save = TRUE)

m11_pw_nc <- prais_winsten(np_share_nc_1 ~ r_year + crisis_election3 * restraint_pre_norm,
                           data = data_restrained, 
                           index = c("country_name_short", "year"), 
                           twostep = TRUE, panelwise = TRUE, rhoweight = "T1") %>% 
    coeftest(., vcov. = vcovPC(., pairwise = TRUE), save = TRUE)

m11_pw_cv <- prais_winsten(np_share_cv_1 ~ r_year + crisis_election3 * restraint_pre_norm,
                           data = data_restrained, 
                           index = c("country_name_short", "year"), 
                           twostep = TRUE, panelwise = TRUE, rhoweight = "T1") %>% 
    coeftest(., vcov. = vcovPC(., pairwise = TRUE), save = TRUE)

m11_pw_pnp <- prais_winsten(np_share_pnp_1 ~ r_year + crisis_election3 * restraint_pre_norm,
                            data = data_restrained, 
                            index = c("country_name_short", "year"), 
                            twostep = TRUE, panelwise = TRUE, rhoweight = "T1") %>% 
    coeftest(., vcov. = vcovPC(., pairwise = TRUE), save = TRUE)

modelsummary(
    list(
        "All new parties" = m10_pw_nc, 
         "All new parties" = m11_pw_nc, 
         "Genuinely new parties" = m10_pw_cv, 
         "Genuinely new parties" = m11_pw_cv, 
         "Partially new parties" = m10_pw_pnp,
         "Partially new parties" = m11_pw_pnp), 
    coef_rename = c(
        "r_year"="Year (0 = 1991)", 
        "crisis_election3"="Three elections after 2008", 
        "region_typeOnce stable region" = "Once stable region", 
        "restraint_pre_norm" = "Party restraint (pre-crisis)", 
        "crisis_election3:restraint_pre_norm" = "Three elections after 2008 × Party restraint (pre-crisis)"
    ),
    stars = TRUE,
    notes = PW_NOTE,
    fmt = 2,
    gof_map = c("nobs", "r.squared", "adj.r.squared"),
    output = "figs/tab2_3elections_after.html"
)

## Tab 3: Restrained change --------------------
econ_data <- read_excel("data/API_NY.GDP.PCAP.KD.ZG_DS2_en_excel_v2_306.xls", 
                        skip = 3) %>% 
    clean_names() %>% 
    filter(country_name %in% c("Bulgaria", "Croatia", "Czechia",
                          "Estonia", "Hungary", "Latvia", 
                          "Lithuania", "Poland", "Romania", 
                          "Slovak Republic", "Slovenia")) %>% 
    select(country_code, starts_with("x")) %>%
    pivot_longer(., cols = starts_with("x"), 
                 names_to = "year", values_to = "gdp_growth") %>% 
    mutate(year = as.numeric(gsub("x", "", year))) %>% 
    filter(!is.na(gdp_growth))

econ_growth <- econ_data %>% 
    mutate(gdp_growth_rates = 1 + gdp_growth / 100) %>% 
    filter(year >= 2007)

avg_growth_rates_since_2007 <- purrr::map_df(2008:2020, function(x) {
    econ_growth %>% 
        filter(year <= x) %>% 
        group_by(country_code) %>% 
        summarise(avg_growth_pct = (geometric.mean(gdp_growth_rates) - 1) * 100) %>% 
        ungroup %>% 
        mutate(election_year = x)
})

avg_pre_crisis_support <- data %>% 
    group_by(country_name_short, post_crisis) %>% 
    mutate(n_election_before_crisis = case_when(
        post_crisis == 0 ~ n() - row_number() + 1,
        TRUE ~ NA_integer_
    )) %>% 
    ungroup %>% 
    filter(n_election_before_crisis <= 3) %>% 
    group_by(country_name_short) %>% 
    summarise(
        across(c("np_share_nc_1", "np_share_cv_1", "np_share_pnp_1", 
                 "np_share_nc_leg", "np_share_cv_leg", "np_share_pnp_leg",
                 "np_number_nc_1", "np_number_cv_1", "np_number_pnp_1", 
                 "np_number_nc_leg", "np_number_cv_leg", "np_number_pnp_leg"), 
               ~mean(.x), .names = "pre_crisis_{.col}")
    )

post_crisis_elections <- data %>% 
    filter(year >= 2009) %>% 
    left_join(., restraint_data, by = c("country_name_short"="country")) %>% 
    left_join(avg_pre_crisis_support, by = "country_name_short") %>% 
    mutate(
        diff_np_share_nc_1 = np_share_nc_1 - pre_crisis_np_share_nc_1,
        diff_np_share_cv_1 = np_share_cv_1 - pre_crisis_np_share_cv_1,
        diff_np_share_pnp_1 = np_share_pnp_1 - pre_crisis_np_share_pnp_1,
        diff_np_share_nc_leg = np_share_nc_leg - pre_crisis_np_share_nc_leg,
        diff_np_share_cv_leg = np_share_cv_leg - pre_crisis_np_share_cv_leg,
        diff_np_share_pnp_leg = np_share_pnp_leg - pre_crisis_np_share_pnp_leg,
        
        diff_np_number_nc_1 = np_number_nc_1 - pre_crisis_np_number_nc_1,
        diff_np_number_cv_1 = np_number_cv_1 - pre_crisis_np_number_cv_1,
        diff_np_number_pnp_1 = np_number_pnp_1 - pre_crisis_np_number_pnp_1,
        diff_np_number_nc_leg = np_number_nc_leg - pre_crisis_np_number_nc_leg,
        diff_np_number_cv_leg = np_number_cv_leg - pre_crisis_np_number_cv_leg,
        diff_np_number_pnp_leg = np_number_pnp_leg - pre_crisis_np_number_pnp_leg
    ) %>% 
    left_join(., avg_growth_rates_since_2007, 
              by = c("country_name_short"="country_code", "election_year")) %>% 
    group_by(country_name_short) %>% 
    mutate(n_election_post_crisis = row_number()) %>% 
    ungroup %>% 
    select(country_name_short, year, starts_with("diff"), avg_growth_pct, 
           everything()) 

post_crisis_elections3 <- post_crisis_elections %>% 
    filter(n_election_post_crisis <= 3) 

mr0_np <- prais_winsten(diff_np_share_nc_1 ~ 1,
                        data = post_crisis_elections3, 
                        index = c("country_name_short", "year"), 
                        twostep = TRUE, panelwise = TRUE, rhoweight = "T1") %>%
    coeftest(., vcov. = vcovPC(., pairwise = TRUE), save = TRUE)

mr0_pnp <- prais_winsten(diff_np_share_pnp_1 ~ 1,
                         data = post_crisis_elections3, 
                         index = c("country_name_short", "year"), 
                         twostep = TRUE, panelwise = TRUE, rhoweight = "T1") %>%
    coeftest(., vcov. = vcovPC(., pairwise = TRUE), save = TRUE)

mr0_cv <- prais_winsten(diff_np_share_cv_1 ~ 1,
                        data = post_crisis_elections3, 
                        index = c("country_name_short", "year"), 
                        twostep = TRUE, panelwise = TRUE, rhoweight = "T1") %>%
    coeftest(., vcov. = vcovPC(., pairwise = TRUE), save = TRUE)

mr1_np <- prais_winsten(diff_np_share_nc_1 ~ avg_growth_pct + restraint_pre,
              data = post_crisis_elections3, 
              index = c("country_name_short", "year"), 
              twostep = TRUE, panelwise = TRUE, rhoweight = "T1") %>%
    coeftest(., vcov. = vcovPC(., pairwise = TRUE), save = TRUE)

mr1_pnp <- prais_winsten(diff_np_share_pnp_1 ~ avg_growth_pct + restraint_pre,
                     data = post_crisis_elections3, 
                     index = c("country_name_short", "year"), 
                     twostep = TRUE, panelwise = TRUE, rhoweight = "T1") %>%
    coeftest(., vcov. = vcovPC(., pairwise = TRUE), save = TRUE)

mr1_cv <- prais_winsten(diff_np_share_cv_1 ~ avg_growth_pct + restraint_pre,
                         data = post_crisis_elections3, 
                         index = c("country_name_short", "year"), 
                         twostep = TRUE, panelwise = TRUE, rhoweight = "T1") %>%
    coeftest(., vcov. = vcovPC(., pairwise = TRUE), save = TRUE)

post_crisis_elections3 %>% 
    filter(country_name_short == "LVA") %>% 
    select(avg_growth_pct, restraint_pre, year)

modelsummary(list("Δ Genuinely new parties" = mr0_cv,
                  "Δ Partially new parties" = mr0_pnp, 
                  "Δ All new parties" = mr0_np, 
                  "Δ Genuinely new parties" = mr1_cv,
                  "Δ Partially new parties" = mr1_pnp, 
                  "Δ All new parties" = mr1_np), 
             stars = TRUE, 
             coef_rename = c("avg_growth_pct"="Avg. growth", 
                             "restraint_pre"="Party restraint"), 
             output = "figs/tab3.html")

## Tab 3 - 2 elections window -----------------------------
avg_pre_crisis_support2 <- data %>% 
    group_by(country_name_short, post_crisis) %>% 
    mutate(n_election_before_crisis = case_when(
        post_crisis == 0 ~ n() - row_number() + 1,
        TRUE ~ NA_integer_
    )) %>% 
    ungroup %>% 
    filter(n_election_before_crisis <= 2) %>% 
    group_by(country_name_short) %>% 
    summarise(
        across(c("np_share_nc_1", "np_share_cv_1", "np_share_pnp_1", 
                 "np_share_nc_leg", "np_share_cv_leg", "np_share_pnp_leg",
                 "np_number_nc_1", "np_number_cv_1", "np_number_pnp_1", 
                 "np_number_nc_leg", "np_number_cv_leg", "np_number_pnp_leg"), 
               ~mean(.x), .names = "pre_crisis_{.col}")
    )

post_crisis_elections2 <- data %>% 
    filter(year >= 2009) %>% 
    left_join(., restraint_data, by = c("country_name_short"="country")) %>% 
    left_join(avg_pre_crisis_support2, by = "country_name_short") %>% 
    mutate(
        diff_np_share_nc_1 = np_share_nc_1 - pre_crisis_np_share_nc_1,
        diff_np_share_cv_1 = np_share_cv_1 - pre_crisis_np_share_cv_1,
        diff_np_share_pnp_1 = np_share_pnp_1 - pre_crisis_np_share_pnp_1,
        diff_np_share_nc_leg = np_share_nc_leg - pre_crisis_np_share_nc_leg,
        diff_np_share_cv_leg = np_share_cv_leg - pre_crisis_np_share_cv_leg,
        diff_np_share_pnp_leg = np_share_pnp_leg - pre_crisis_np_share_pnp_leg,
        
        diff_np_number_nc_1 = np_number_nc_1 - pre_crisis_np_number_nc_1,
        diff_np_number_cv_1 = np_number_cv_1 - pre_crisis_np_number_cv_1,
        diff_np_number_pnp_1 = np_number_pnp_1 - pre_crisis_np_number_pnp_1,
        diff_np_number_nc_leg = np_number_nc_leg - pre_crisis_np_number_nc_leg,
        diff_np_number_cv_leg = np_number_cv_leg - pre_crisis_np_number_cv_leg,
        diff_np_number_pnp_leg = np_number_pnp_leg - pre_crisis_np_number_pnp_leg
    ) %>% 
    left_join(., avg_growth_rates_since_2007, 
              by = c("country_name_short"="country_code", "election_year")) %>% 
    group_by(country_name_short) %>% 
    mutate(n_election_post_crisis = row_number()) %>% 
    ungroup %>% 
    select(country_name_short, year, starts_with("diff"), avg_growth_pct, 
           everything()) 

post_crisis_elections2 <- post_crisis_elections2 %>% 
    filter(n_election_post_crisis <= 2) 

mr0_np2 <- prais_winsten(diff_np_share_nc_1 ~ 1,
                        data = post_crisis_elections2, 
                        index = c("country_name_short", "year"), 
                        twostep = TRUE, panelwise = TRUE, rhoweight = "T1") %>%
    coeftest(., vcov. = vcovPC(., pairwise = TRUE), save = TRUE)

mr0_pnp2 <- prais_winsten(diff_np_share_pnp_1 ~ 1,
                         data = post_crisis_elections2, 
                         index = c("country_name_short", "year"), 
                         twostep = TRUE, panelwise = TRUE, rhoweight = "T1") %>%
    coeftest(., vcov. = vcovPC(., pairwise = TRUE), save = TRUE)

mr0_cv2 <- prais_winsten(diff_np_share_cv_1 ~ 1,
                        data = post_crisis_elections2, 
                        index = c("country_name_short", "year"), 
                        twostep = TRUE, panelwise = TRUE, rhoweight = "T1") %>%
    coeftest(., vcov. = vcovPC(., pairwise = TRUE), save = TRUE)

mr1_np2 <- prais_winsten(diff_np_share_nc_1 ~ avg_growth_pct + restraint_pre,
                        data = post_crisis_elections2, 
                        index = c("country_name_short", "year"), 
                        twostep = TRUE, panelwise = TRUE, rhoweight = "T1") %>%
    coeftest(., vcov. = vcovPC(., pairwise = TRUE), save = TRUE)

mr1_pnp2 <- prais_winsten(diff_np_share_pnp_1 ~ avg_growth_pct + restraint_pre,
                         data = post_crisis_elections2, 
                         index = c("country_name_short", "year"), 
                         twostep = TRUE, panelwise = TRUE, rhoweight = "T1") %>%
    coeftest(., vcov. = vcovPC(., pairwise = TRUE), save = TRUE)

mr1_cv2 <- prais_winsten(diff_np_share_cv_1 ~ avg_growth_pct + restraint_pre,
                        data = post_crisis_elections2, 
                        index = c("country_name_short", "year"), 
                        twostep = TRUE, panelwise = TRUE, rhoweight = "T1") %>%
    coeftest(., vcov. = vcovPC(., pairwise = TRUE), save = TRUE)

modelsummary(list("Δ Genuinely new parties" = mr0_cv2,
                  "Δ Partially new parties" = mr0_pnp2, 
                  "Δ All new parties" = mr0_np2, 
                  "Δ Genuinely new parties" = mr1_cv2,
                  "Δ Partially new parties" = mr1_pnp2, 
                  "Δ All new parties" = mr1_np2), 
             stars = TRUE, 
             coef_rename = c("avg_growth_pct"="Avg. growth", 
                             "restraint_pre"="Party restraint"), 
             output = "figs/tab3_2elections.html")


## Tab 3 - OLS w/ clustered SEs ----------------------------
mr0_np <- lm_robust(diff_np_share_nc_1 ~ 1,
                    data = post_crisis_elections3, 
                    clusters = country_name_short, 
                    se_type = "stata")

mr0_pnp <- lm_robust(diff_np_share_pnp_1 ~ 1,
                     data = post_crisis_elections3, 
                     clusters = country_name_short, 
                     se_type = "stata")

mr0_cv <- lm_robust(diff_np_share_cv_1 ~ 1,
                    data = post_crisis_elections3, 
                    clusters = country_name_short, 
                    se_type = "stata")

mr1_np <- lm_robust(diff_np_share_nc_1 ~ avg_growth_pct + restraint_pre,
                    data = post_crisis_elections3, 
                    clusters = country_name_short, 
                    se_type = "stata")

mr1_pnp <- lm_robust(diff_np_share_pnp_1 ~ avg_growth_pct + restraint_pre,
                     data = post_crisis_elections3, 
                     clusters = country_name_short, 
                     se_type = "stata")

mr1_cv <- lm_robust(diff_np_share_cv_1 ~ avg_growth_pct + restraint_pre,
                    data = post_crisis_elections3, 
                    clusters = country_name_short, 
                    se_type = "stata")

modelsummary(list("Δ Genuinely new parties" = mr0_cv,
                  "Δ Partially new parties" = mr0_pnp, 
                  "Δ All new parties" = mr0_np, 
                  "Δ Genuinely new parties" = mr1_cv,
                  "Δ Partially new parties" = mr1_pnp, 
                  "Δ All new parties" = mr1_np), 
             stars = TRUE,
             coef_rename = c("avg_growth_pct"="Avg. growth",
                             "restraint_pre"="Party restraint"),
             # output = "figs/tab3.html"
             )

# models with legislative threshold -------------------------------
## Tab 1 -----------------------------
app1_m1a_pw <- prais_winsten(np_share_nc_leg ~ r_year, data = used_data, 
                        index = c("country_name_short", "year"), 
                        twostep = TRUE, panelwise = TRUE, rhoweight = "T1") %>% 
    coeftest(., vcov. = vcovPC(., pairwise = TRUE), save = TRUE)
app1_m1b_pw <- prais_winsten(np_share_nc_leg ~ r_year + crisis_election, 
                        data = used_data, index = c("country_name_short", "year"), 
                        twostep = TRUE, panelwise = TRUE, rhoweight = "T1") %>% 
    coeftest(., vcov. = vcovPC(., pairwise = TRUE), save = TRUE)
app1_m1c_pw <- prais_winsten(np_share_nc_leg ~ r_year + crisis_election3, 
                        data = used_data, index = c("country_name_short", "year"), 
                        twostep = TRUE, panelwise = TRUE, rhoweight = "T1") %>% 
    coeftest(., vcov. = vcovPC(., pairwise = TRUE), save = TRUE)

app1_m2a_pw <- prais_winsten(np_share_cv_leg ~ r_year, data = used_data, 
                        index = c("country_name_short", "year"), 
                        twostep = TRUE, panelwise = TRUE, rhoweight = "T1") %>% 
    coeftest(., vcov. = vcovPC(., pairwise = TRUE), save = TRUE)
app1_m2b_pw <- prais_winsten(np_share_cv_leg ~ r_year + crisis_election, 
                        data = used_data, 
                        index = c("country_name_short", "year"), 
                        twostep = TRUE, panelwise = TRUE, rhoweight = "T1") %>% 
    coeftest(., vcov. = vcovPC(., pairwise = TRUE), save = TRUE)
app1_m2c_pw <- prais_winsten(np_share_cv_leg ~ r_year + crisis_election3, 
                        data = used_data, 
                        index = c("country_name_short", "year"), 
                        twostep = TRUE, panelwise = TRUE, rhoweight = "T1") %>% 
    coeftest(., vcov. = vcovPC(., pairwise = TRUE), save = TRUE)

app1_m3a_pw <- prais_winsten(np_share_pnp_leg ~ r_year, data = used_data, 
                        index = c("country_name_short", "year"), 
                        twostep = TRUE, panelwise = TRUE, rhoweight = "T1") %>% 
    coeftest(., vcov. = vcovPC(., pairwise = TRUE), save = TRUE)
app1_m3b_pw <- prais_winsten(np_share_pnp_leg ~ r_year + crisis_election, 
                        data = used_data, index = c("country_name_short", "year"), 
                        twostep = TRUE, panelwise = TRUE, rhoweight = "T1") %>% 
    coeftest(., vcov. = vcovPC(., pairwise = TRUE), save = TRUE)
app1_m3c_pw <- prais_winsten(np_share_pnp_leg ~ r_year + crisis_election3, 
                        data = used_data, index = c("country_name_short", "year"), 
                        twostep = TRUE, panelwise = TRUE, rhoweight = "T1") %>% 
    coeftest(., vcov. = vcovPC(., pairwise = TRUE), save = TRUE)

PW_NOTE <- "Models estimated using Prais-Winsten regression with panel-corrected standard errors.
Dependent variable: Share of votes for new parties (above legislative threshold)"
modelsummary(
    list("All new parties" = app1_m1a_pw, 
         "Partially new parties" = app1_m3a_pw, 
         "Genuinely new parties" = app1_m2a_pw), 
    coef_rename = c(
        "r_year"="Year (0 = 1991)", 
        "crisis_election"="Two elections after 2008", 
        "crisis_election3"="Three elections after 2008"
    ),
    stars = TRUE, 
    fmt = 2,
    gof_map = c("nobs", "r.squared", "adj.r.squared"),
    notes = PW_NOTE,
    output = "figs/app1_tab1.html"
)

## Tab 2 --------------------------------------------------
app1_m10_pw_nc <- prais_winsten(np_share_nc_leg ~ r_year + crisis_election3 * restraint_pre,
                           data = data_restrained, 
                           index = c("country_name_short", "year"), 
                           twostep = TRUE, panelwise = TRUE, rhoweight = "T1") %>% 
    coeftest(., vcov. = vcovPC(., pairwise = TRUE), save = TRUE)

app1_m10_pw_cv <- prais_winsten(np_share_cv_leg ~ r_year + crisis_election3 * restraint_pre,
                           data = data_restrained, 
                           index = c("country_name_short", "year"), 
                           twostep = TRUE, panelwise = TRUE, rhoweight = "T1") %>% 
    coeftest(., vcov. = vcovPC(., pairwise = TRUE), save = TRUE)

app1_m10_pw_pnp <- prais_winsten(np_share_pnp_leg ~ r_year + crisis_election3 * restraint_pre,
                            data = data_restrained, 
                            index = c("country_name_short", "year"), 
                            twostep = TRUE, panelwise = TRUE, rhoweight = "T1") %>% 
    coeftest(., vcov. = vcovPC(., pairwise = TRUE), save = TRUE)

modelsummary(
    list(
        "All new parties" = app1_m1c_pw, 
        "All new parties" = app1_m10_pw_nc, 
        "Partially new parties" = app1_m3c_pw, 
        "Partially new parties" = app1_m10_pw_pnp,
        "Genuinely new parties" = app1_m2c_pw,
        "Genuinely new parties" = app1_m10_pw_cv), 
    coef_rename = c(
        "r_year"="Year (0 = 1991)", 
        "crisis_election3"="Three elections after 2008", 
        "restraint_pre" = "Party restraint (pre-crisis)", 
        "crisis_election3:restraint_pre" = "Three elections after 2008 × Party restraint (pre-crisis)"
    ),
    stars = TRUE,
    notes = PW_NOTE,
    fmt = 2,
    gof_map = c("nobs", "r.squared", "adj.r.squared"),
    output = "figs/app1_tab3.html"
)

app1_m11_pw_nc <- prais_winsten(np_share_nc_leg ~ r_year + crisis_election3 * restraint_pre_norm,
                           data = data_restrained, 
                           index = c("country_name_short", "year"), 
                           twostep = TRUE, panelwise = TRUE, rhoweight = "T1") %>% 
    coeftest(., vcov. = vcovPC(., pairwise = TRUE), save = TRUE)

app1_m11_pw_cv <- prais_winsten(np_share_cv_leg ~ r_year + crisis_election3 * restraint_pre_norm,
                           data = data_restrained, 
                           index = c("country_name_short", "year"), 
                           twostep = TRUE, panelwise = TRUE, rhoweight = "T1") %>% 
    coeftest(., vcov. = vcovPC(., pairwise = TRUE), save = TRUE)

app1_m11_pw_pnp <- prais_winsten(np_share_pnp_leg ~ r_year + crisis_election3 * restraint_pre_norm,
                            data = data_restrained, 
                            index = c("country_name_short", "year"), 
                            twostep = TRUE, panelwise = TRUE, rhoweight = "T1") %>% 
    coeftest(., vcov. = vcovPC(., pairwise = TRUE), save = TRUE)


## Tab 3 -----------------------------------
app1_mr0_np <- prais_winsten(diff_np_share_nc_leg ~ 1,
                        data = post_crisis_elections3, 
                        index = c("country_name_short", "year"), 
                        twostep = TRUE, panelwise = TRUE, rhoweight = "T1") %>%
    coeftest(., vcov. = vcovPC(., pairwise = TRUE), save = TRUE)

app1_mr0_pnp <- prais_winsten(diff_np_share_pnp_leg ~ 1,
                         data = post_crisis_elections3, 
                         index = c("country_name_short", "year"), 
                         twostep = TRUE, panelwise = TRUE, rhoweight = "T1") %>%
    coeftest(., vcov. = vcovPC(., pairwise = TRUE), save = TRUE)

app1_mr0_cv <- prais_winsten(diff_np_share_cv_leg ~ 1,
                        data = post_crisis_elections3, 
                        index = c("country_name_short", "year"), 
                        twostep = TRUE, panelwise = TRUE, rhoweight = "T1") %>%
    coeftest(., vcov. = vcovPC(., pairwise = TRUE), save = TRUE)

app1_mr1_np <- prais_winsten(diff_np_share_nc_leg ~ avg_growth_pct + restraint_pre,
                        data = post_crisis_elections3, 
                        index = c("country_name_short", "year"), 
                        twostep = TRUE, panelwise = TRUE, rhoweight = "T1") %>%
    coeftest(., vcov. = vcovPC(., pairwise = TRUE), save = TRUE)

app1_mr1_pnp <- prais_winsten(diff_np_share_pnp_leg ~ avg_growth_pct + restraint_pre,
                         data = post_crisis_elections3, 
                         index = c("country_name_short", "year"), 
                         twostep = TRUE, panelwise = TRUE, rhoweight = "T1") %>%
    coeftest(., vcov. = vcovPC(., pairwise = TRUE), save = TRUE)

app1_mr1_cv <- prais_winsten(diff_np_share_cv_leg ~ avg_growth_pct + restraint_pre,
                        data = post_crisis_elections3, 
                        index = c("country_name_short", "year"), 
                        twostep = TRUE, panelwise = TRUE, rhoweight = "T1") %>%
    coeftest(., vcov. = vcovPC(., pairwise = TRUE), save = TRUE)

modelsummary(list("Δ All new parties" = app1_mr0_np, 
                  "Δ All new parties" = app1_mr1_np,
                  "Δ Partially new parties" = app1_mr0_pnp, 
                  "Δ Partially new parties" = app1_mr1_pnp, 
                  "Δ Genuinely new parties" = app1_mr0_cv,
                  "Δ Genuinely new parties" = app1_mr1_cv), 
             stars = TRUE, 
             coef_rename = c("avg_growth_pct"="Avg. growth", 
                             "restraint_pre"="Party restraint"), 
             output = "figs/app1_tab2.html")


# party number, 1% threshold -----------------------------------
## Tab 1 -----------------------------
app2_m1a_pw <- prais_winsten(np_number_nc_1 ~ r_year, data = used_data, 
                             index = c("country_name_short", "year"), 
                             twostep = TRUE, panelwise = TRUE, rhoweight = "T1") %>% 
    coeftest(., vcov. = vcovPC(., pairwise = TRUE), save = TRUE)
app2_m1b_pw <- prais_winsten(np_number_nc_1 ~ r_year + crisis_election, 
                             data = used_data, index = c("country_name_short", "year"), 
                             twostep = TRUE, panelwise = TRUE, rhoweight = "T1") %>% 
    coeftest(., vcov. = vcovPC(., pairwise = TRUE), save = TRUE)
app2_m1c_pw <- prais_winsten(np_number_nc_1 ~ r_year + crisis_election3, 
                             data = used_data, index = c("country_name_short", "year"), 
                             twostep = TRUE, panelwise = TRUE, rhoweight = "T1") %>% 
    coeftest(., vcov. = vcovPC(., pairwise = TRUE), save = TRUE)

app2_m2a_pw <- prais_winsten(np_number_cv_1 ~ r_year, data = used_data, 
                             index = c("country_name_short", "year"), 
                             twostep = TRUE, panelwise = TRUE, rhoweight = "T1") %>% 
    coeftest(., vcov. = vcovPC(., pairwise = TRUE), save = TRUE)
app2_m2b_pw <- prais_winsten(np_number_cv_1 ~ r_year + crisis_election, 
                             data = used_data, 
                             index = c("country_name_short", "year"), 
                             twostep = TRUE, panelwise = TRUE, rhoweight = "T1") %>% 
    coeftest(., vcov. = vcovPC(., pairwise = TRUE), save = TRUE)
app2_m2c_pw <- prais_winsten(np_number_cv_1 ~ r_year + crisis_election3, 
                             data = used_data, 
                             index = c("country_name_short", "year"), 
                             twostep = TRUE, panelwise = TRUE, rhoweight = "T1") %>% 
    coeftest(., vcov. = vcovPC(., pairwise = TRUE), save = TRUE)

app2_m3a_pw <- prais_winsten(np_number_pnp_1 ~ r_year, data = used_data, 
                             index = c("country_name_short", "year"), 
                             twostep = TRUE, panelwise = TRUE, rhoweight = "T1") %>% 
    coeftest(., vcov. = vcovPC(., pairwise = TRUE), save = TRUE)
app2_m3b_pw <- prais_winsten(np_number_pnp_1 ~ r_year + crisis_election, 
                             data = used_data, index = c("country_name_short", "year"), 
                             twostep = TRUE, panelwise = TRUE, rhoweight = "T1") %>% 
    coeftest(., vcov. = vcovPC(., pairwise = TRUE), save = TRUE)
app2_m3c_pw <- prais_winsten(np_number_pnp_1 ~ r_year + crisis_election3, 
                             data = used_data, index = c("country_name_short", "year"), 
                             twostep = TRUE, panelwise = TRUE, rhoweight = "T1") %>% 
    coeftest(., vcov. = vcovPC(., pairwise = TRUE), save = TRUE)

PW_NOTE <- "Models estimated using Prais-Winsten regression with panel-corrected standard errors.
Dependent variable: Number of new parties (above 1% threshold)"
modelsummary(
    list("All new parties" = app2_m1a_pw, 
         "Partially new parties" = app2_m3a_pw, 
         "Genuinely new parties" = app2_m2a_pw), 
    coef_rename = c(
        "r_year"="Year (0 = 1991)", 
        "crisis_election"="Two elections after 2008", 
        "crisis_election3"="Three elections after 2008"
    ),
    stars = TRUE, 
    fmt = 2,
    gof_map = c("nobs", "r.squared", "adj.r.squared"),
    notes = PW_NOTE,
    output = "figs/app2_tab1.html"
)

## Tab 2 --------------------------------------------------
app2_m10_pw_nc <- prais_winsten(np_number_nc_1 ~ r_year + crisis_election3 * restraint_pre,
                                data = data_restrained, 
                                index = c("country_name_short", "year"), 
                                twostep = TRUE, panelwise = TRUE, rhoweight = "T1") %>% 
    coeftest(., vcov. = vcovPC(., pairwise = TRUE), save = TRUE)

app2_m10_pw_cv <- prais_winsten(np_number_cv_1 ~ r_year + crisis_election3 * restraint_pre,
                                data = data_restrained, 
                                index = c("country_name_short", "year"), 
                                twostep = TRUE, panelwise = TRUE, rhoweight = "T1") %>% 
    coeftest(., vcov. = vcovPC(., pairwise = TRUE), save = TRUE)

app2_m10_pw_pnp <- prais_winsten(np_number_pnp_1 ~ r_year + crisis_election3 * restraint_pre,
                                 data = data_restrained, 
                                 index = c("country_name_short", "year"), 
                                 twostep = TRUE, panelwise = TRUE, rhoweight = "T1") %>% 
    coeftest(., vcov. = vcovPC(., pairwise = TRUE), save = TRUE)


modelsummary(
    list(
        "All new parties" = app2_m1c_pw, 
        "All new parties" = app2_m10_pw_nc, 
        "Partially new parties" = app2_m3c_pw, 
        "Partially new parties" = app2_m10_pw_pnp,
        "Genuinely new parties" = app2_m2c_pw, 
        "Genuinely new parties" = app2_m10_pw_cv), 
    coef_rename = c(
        "r_year"="Year (0 = 1991)", 
        "crisis_election3"="Three elections after 2008", 
        "restraint_pre" = "Party restraint (pre-crisis)", 
        "crisis_election3:restraint_pre" = "Three elections after 2008 × Party restraint (pre-crisis)"
    ),
    stars = TRUE,
    notes = PW_NOTE,
    fmt = 2,
    gof_map = c("nobs", "r.squared", "adj.r.squared"),
    output = "figs/app2_tab3.html"
)

## Tab 3 -----------------------------------
app2_mr0_np <- prais_winsten(diff_np_number_nc_1 ~ 1,
                             data = post_crisis_elections3, 
                             index = c("country_name_short", "year"), 
                             twostep = TRUE, panelwise = TRUE, rhoweight = "T1") %>%
    coeftest(., vcov. = vcovPC(., pairwise = TRUE), save = TRUE)

app2_mr0_pnp <- prais_winsten(diff_np_number_pnp_1 ~ 1,
                              data = post_crisis_elections3, 
                              index = c("country_name_short", "year"), 
                              twostep = TRUE, panelwise = TRUE, rhoweight = "T1") %>%
    coeftest(., vcov. = vcovPC(., pairwise = TRUE), save = TRUE)

app2_mr0_cv <- prais_winsten(diff_np_number_cv_1 ~ 1,
                             data = post_crisis_elections3, 
                             index = c("country_name_short", "year"), 
                             twostep = TRUE, panelwise = TRUE, rhoweight = "T1") %>%
    coeftest(., vcov. = vcovPC(., pairwise = TRUE), save = TRUE)

app2_mr1_np <- prais_winsten(diff_np_number_nc_1 ~ avg_growth_pct + restraint_pre,
                             data = post_crisis_elections3, 
                             index = c("country_name_short", "year"), 
                             twostep = TRUE, panelwise = TRUE, rhoweight = "T1") %>%
    coeftest(., vcov. = vcovPC(., pairwise = TRUE), save = TRUE)

app2_mr1_pnp <- prais_winsten(diff_np_number_pnp_1 ~ avg_growth_pct + restraint_pre,
                              data = post_crisis_elections3, 
                              index = c("country_name_short", "year"), 
                              twostep = TRUE, panelwise = TRUE, rhoweight = "T1") %>%
    coeftest(., vcov. = vcovPC(., pairwise = TRUE), save = TRUE)

app2_mr1_cv <- prais_winsten(diff_np_number_cv_1 ~ avg_growth_pct + restraint_pre,
                             data = post_crisis_elections3, 
                             index = c("country_name_short", "year"), 
                             twostep = TRUE, panelwise = TRUE, rhoweight = "T1") %>%
    coeftest(., vcov. = vcovPC(., pairwise = TRUE), save = TRUE)

modelsummary(list(
    "Δ All new parties" = app2_mr0_np, 
    "Δ All new parties" = app2_mr1_np,
    "Δ Partially new parties" = app2_mr0_pnp, 
    "Δ Partially new parties" = app2_mr1_pnp, 
    "Δ Genuinely new parties" = app2_mr0_cv,
    "Δ Genuinely new parties" = app2_mr1_cv
    ), 
             stars = TRUE, 
             coef_rename = c("avg_growth_pct"="Avg. growth", 
                             "restraint_pre"="Party restraint"), 
             output = "figs/app2_tab2.html")

# party number, leg threshold ----------------------------
## Tab 1 -----------------------------
app3_m1a_pw <- prais_winsten(np_number_nc_leg ~ r_year, data = used_data, 
                             index = c("country_name_short", "year"), 
                             twostep = TRUE, panelwise = TRUE, rhoweight = "T1") %>% 
    coeftest(., vcov. = vcovPC(., pairwise = TRUE), save = TRUE)
app3_m1b_pw <- prais_winsten(np_number_nc_leg ~ r_year + crisis_election, 
                             data = used_data, index = c("country_name_short", "year"), 
                             twostep = TRUE, panelwise = TRUE, rhoweight = "T1") %>% 
    coeftest(., vcov. = vcovPC(., pairwise = TRUE), save = TRUE)
app3_m1c_pw <- prais_winsten(np_number_nc_leg ~ r_year + crisis_election3, 
                             data = used_data, index = c("country_name_short", "year"), 
                             twostep = TRUE, panelwise = TRUE, rhoweight = "T1") %>% 
    coeftest(., vcov. = vcovPC(., pairwise = TRUE), save = TRUE)

app3_m2a_pw <- prais_winsten(np_number_cv_leg ~ r_year, data = used_data, 
                             index = c("country_name_short", "year"), 
                             twostep = TRUE, panelwise = TRUE, rhoweight = "T1") %>% 
    coeftest(., vcov. = vcovPC(., pairwise = TRUE), save = TRUE)
app3_m2b_pw <- prais_winsten(np_number_cv_leg ~ r_year + crisis_election, 
                             data = used_data, 
                             index = c("country_name_short", "year"), 
                             twostep = TRUE, panelwise = TRUE, rhoweight = "T1") %>% 
    coeftest(., vcov. = vcovPC(., pairwise = TRUE), save = TRUE)
app3_m2c_pw <- prais_winsten(np_number_cv_leg ~ r_year + crisis_election3, 
                             data = used_data, 
                             index = c("country_name_short", "year"), 
                             twostep = TRUE, panelwise = TRUE, rhoweight = "T1") %>% 
    coeftest(., vcov. = vcovPC(., pairwise = TRUE), save = TRUE)

app3_m3a_pw <- prais_winsten(np_number_pnp_leg ~ r_year, data = used_data, 
                             index = c("country_name_short", "year"), 
                             twostep = TRUE, panelwise = TRUE, rhoweight = "T1") %>% 
    coeftest(., vcov. = vcovPC(., pairwise = TRUE), save = TRUE)
app3_m3b_pw <- prais_winsten(np_number_pnp_leg ~ r_year + crisis_election, 
                             data = used_data, index = c("country_name_short", "year"), 
                             twostep = TRUE, panelwise = TRUE, rhoweight = "T1") %>% 
    coeftest(., vcov. = vcovPC(., pairwise = TRUE), save = TRUE)
app3_m3c_pw <- prais_winsten(np_number_pnp_leg ~ r_year + crisis_election3, 
                             data = used_data, index = c("country_name_short", "year"), 
                             twostep = TRUE, panelwise = TRUE, rhoweight = "T1") %>% 
    coeftest(., vcov. = vcovPC(., pairwise = TRUE), save = TRUE)

PW_NOTE <- "Models estimated using Prais-Winsten regression with panel-corrected standard errors.
Dependent variable: Number of new parties (above legislative threshold)"
modelsummary(
    list("All new parties" = app3_m1a_pw, 
         "Partially new parties" = app3_m3a_pw, 
         "Genuinely new parties" = app3_m2a_pw), 
    coef_rename = c(
        "r_year"="Year (0 = 1991)", 
        "crisis_election"="Two elections after 2008", 
        "crisis_election3"="Three elections after 2008"
    ),
    stars = TRUE, 
    fmt = 2,
    gof_map = c("nobs", "r.squared", "adj.r.squared"),
    notes = PW_NOTE,
    output = "figs/app3_tab1.html"
)

## Tab 2 --------------------------------------------------
app3_m10_pw_nc <- prais_winsten(np_number_nc_leg ~ r_year + crisis_election3 * restraint_pre,
                                data = data_restrained, 
                                index = c("country_name_short", "year"), 
                                twostep = TRUE, panelwise = TRUE, rhoweight = "T1") %>% 
    coeftest(., vcov. = vcovPC(., pairwise = TRUE), save = TRUE)

app3_m10_pw_cv <- prais_winsten(np_number_cv_leg ~ r_year + crisis_election3 * restraint_pre,
                                data = data_restrained, 
                                index = c("country_name_short", "year"), 
                                twostep = TRUE, panelwise = TRUE, rhoweight = "T1") %>% 
    coeftest(., vcov. = vcovPC(., pairwise = TRUE), save = TRUE)

app3_m10_pw_pnp <- prais_winsten(np_number_pnp_leg ~ r_year + crisis_election3 * restraint_pre,
                                 data = data_restrained, 
                                 index = c("country_name_short", "year"), 
                                 twostep = TRUE, panelwise = TRUE, rhoweight = "T1") %>% 
    coeftest(., vcov. = vcovPC(., pairwise = TRUE), save = TRUE)

modelsummary(
    list(
        "All new parties" = app3_m1c_pw, 
        "All new parties" = app3_m10_pw_nc, 
        "Partially new parties" = app3_m3c_pw, 
         "Partially new parties" = app3_m10_pw_pnp,
         "Genuinely new parties" = app3_m2c_pw, 
         "Genuinely new parties" = app3_m10_pw_cv), 
    coef_rename = c(
        "r_year"="Year (0 = 1991)", 
        "crisis_election3"="Three elections after 2008", 
        "restraint_pre" = "Party restraint (pre-crisis)", 
        "crisis_election3:restraint_pre" = "Three elections after 2008 × Party restraint (pre-crisis)"
    ),
    stars = TRUE,
    notes = PW_NOTE,
    fmt = 2,
    gof_map = c("nobs", "r.squared", "adj.r.squared"),
    output = "figs/app3_tab3.html"
)

## Tab 3 -----------------------------------
app3_mr0_np <- prais_winsten(diff_np_number_nc_leg ~ 1,
                             data = post_crisis_elections3, 
                             index = c("country_name_short", "year"), 
                             twostep = TRUE, panelwise = TRUE, rhoweight = "T1") %>%
    coeftest(., vcov. = vcovPC(., pairwise = TRUE), save = TRUE)

app3_mr0_pnp <- prais_winsten(diff_np_number_pnp_leg ~ 1,
                              data = post_crisis_elections3, 
                              index = c("country_name_short", "year"), 
                              twostep = TRUE, panelwise = TRUE, rhoweight = "T1") %>%
    coeftest(., vcov. = vcovPC(., pairwise = TRUE), save = TRUE)

app3_mr0_cv <- prais_winsten(diff_np_number_cv_leg ~ 1,
                             data = post_crisis_elections3, 
                             index = c("country_name_short", "year"), 
                             twostep = TRUE, panelwise = TRUE, rhoweight = "T1") %>%
    coeftest(., vcov. = vcovPC(., pairwise = TRUE), save = TRUE)

app3_mr1_np <- prais_winsten(diff_np_number_nc_leg ~ avg_growth_pct + restraint_pre,
                             data = post_crisis_elections3, 
                             index = c("country_name_short", "year"), 
                             twostep = TRUE, panelwise = TRUE, rhoweight = "T1") %>%
    coeftest(., vcov. = vcovPC(., pairwise = TRUE), save = TRUE)

app3_mr1_pnp <- prais_winsten(diff_np_number_pnp_leg ~ avg_growth_pct + restraint_pre,
                              data = post_crisis_elections3, 
                              index = c("country_name_short", "year"), 
                              twostep = TRUE, panelwise = TRUE, rhoweight = "T1") %>%
    coeftest(., vcov. = vcovPC(., pairwise = TRUE), save = TRUE)

app3_mr1_cv <- prais_winsten(diff_np_number_cv_leg ~ avg_growth_pct + restraint_pre,
                             data = post_crisis_elections3, 
                             index = c("country_name_short", "year"), 
                             twostep = TRUE, panelwise = TRUE, rhoweight = "T1") %>%
    coeftest(., vcov. = vcovPC(., pairwise = TRUE), save = TRUE)

modelsummary(list(
    "Δ All new parties" = app3_mr0_np, 
    "Δ All new parties" = app3_mr1_np,
    "Δ Partially new parties" = app3_mr0_pnp, 
    "Δ Partially new parties" = app3_mr1_pnp, 
    "Δ Genuinely new parties" = app3_mr0_cv,
    "Δ Genuinely new parties" = app3_mr1_cv
    ), 
             stars = TRUE, 
             coef_rename = c("avg_growth_pct"="Avg. growth", 
                             "restraint_pre"="Party restraint"), 
             output = "figs/app3_tab2.html")

# Robustness checks ---------------------------------
## Election order ----------

app4_m1a_pw <- prais_winsten(np_share_nc_1 ~ rank_election_within_country, data = data_restrained, 
                        index = c("country_name_short", "year"), 
                        twostep = TRUE, panelwise = TRUE, rhoweight = "T1") %>% 
    coeftest(., vcov. = vcovPC(., pairwise = TRUE), save = TRUE)
app4_m1b_pw <- prais_winsten(np_share_nc_1 ~ rank_election_within_country + crisis_election3, 
                        data = data_restrained, index = c("country_name_short", "year"), 
                        twostep = TRUE, panelwise = TRUE, rhoweight = "T1") %>% 
    coeftest(., vcov. = vcovPC(., pairwise = TRUE), save = TRUE)
app4_m1c_pw <- prais_winsten(np_share_nc_1 ~ rank_election_within_country + crisis_election3 * restraint_pre_norm, 
                        data = data_restrained, index = c("country_name_short", "year"), 
                        twostep = TRUE, panelwise = TRUE, rhoweight = "T1") %>% 
    coeftest(., vcov. = vcovPC(., pairwise = TRUE), save = TRUE)

app4_m2a_pw <- prais_winsten(np_share_cv_1 ~ rank_election_within_country, 
                             data = data_restrained, 
                        index = c("country_name_short", "year"), 
                        twostep = TRUE, panelwise = TRUE, rhoweight = "T1") %>% 
    coeftest(., vcov. = vcovPC(., pairwise = TRUE), save = TRUE)
app4_m2b_pw <- prais_winsten(np_share_cv_1 ~ rank_election_within_country + crisis_election3, 
                        data = data_restrained, 
                        index = c("country_name_short", "year"), 
                        twostep = TRUE, panelwise = TRUE, rhoweight = "T1") %>% 
    coeftest(., vcov. = vcovPC(., pairwise = TRUE), save = TRUE)
app4_m2c_pw <- prais_winsten(np_share_cv_1 ~ rank_election_within_country + crisis_election3 * restraint_pre_norm, 
                        data = data_restrained, 
                        index = c("country_name_short", "year"), 
                        twostep = TRUE, panelwise = TRUE, rhoweight = "T1") %>% 
    coeftest(., vcov. = vcovPC(., pairwise = TRUE), save = TRUE)

app4_m3a_pw <- prais_winsten(np_share_pnp_1 ~ rank_election_within_country, data = data_restrained, 
                        index = c("country_name_short", "year"), 
                        twostep = TRUE, panelwise = TRUE, rhoweight = "T1") %>% 
    coeftest(., vcov. = vcovPC(., pairwise = TRUE), save = TRUE)
app4_m3b_pw <- prais_winsten(np_share_pnp_1 ~ rank_election_within_country + crisis_election3, 
                        data = data_restrained, index = c("country_name_short", "year"), 
                        twostep = TRUE, panelwise = TRUE, rhoweight = "T1") %>% 
    coeftest(., vcov. = vcovPC(., pairwise = TRUE), save = TRUE)
app4_m3c_pw <- prais_winsten(np_share_pnp_1 ~ rank_election_within_country + crisis_election3 * restraint_pre_norm, 
                        data = data_restrained, index = c("country_name_short", "year"), 
                        twostep = TRUE, panelwise = TRUE, rhoweight = "T1") %>% 
    coeftest(., vcov. = vcovPC(., pairwise = TRUE), save = TRUE)

PW_NOTE <- "Models estimated using Prais-Winsten regression with panel-corrected standard errors. Models with election order instead of election year."
modelsummary(
    list("All new parties" = app4_m1a_pw, 
         # "All new parties" = app4_m1b_pw, 
         "All new parties" = app4_m1c_pw, 
         "Partially new parties" = app4_m3a_pw, 
         # "Partially new parties" = app4_m3b_pw, 
         "Partially new parties" = app4_m3c_pw,
         "Genuinely new parties" = app4_m2a_pw, 
         # "Genuinely new parties" = app4_m2b_pw, 
         "Genuinely new parties" = app4_m2c_pw), 
    coef_rename = c(
        "rank_election_within_country"="Election order",
        "crisis_election3"="Three elections after 2008", 
        "restraint_pre_norm"="Party restraint",
        "crisis_election3:restraint_pre_norm" = "Three elections after 2008 × Party restraint (pre-crisis)"
    ),
    stars = TRUE, 
    fmt = 2,
    gof_map = c("nobs", "r.squared", "adj.r.squared"),
    notes = PW_NOTE,
    output = "figs/app4_tab1.html"
)

## without 1st+2nd election -----------------------
data_after_2nd_election <- data_restrained %>% 
    filter(!fst_snd_election)

app5_m1a_pw <- prais_winsten(np_share_nc_1 ~ r_year, data = data_after_2nd_election, 
                        index = c("country_name_short", "year"), 
                        twostep = TRUE, panelwise = TRUE, rhoweight = "T1") %>% 
    coeftest(., vcov. = vcovPC(., pairwise = TRUE), save = TRUE)
app5_m1b_pw <- prais_winsten(np_share_nc_1 ~ r_year + crisis_election3, 
                        data = data_after_2nd_election, index = c("country_name_short", "year"), 
                        twostep = TRUE, panelwise = TRUE, rhoweight = "T1") %>% 
    coeftest(., vcov. = vcovPC(., pairwise = TRUE), save = TRUE)
app5_m1c_pw <- prais_winsten(np_share_nc_1 ~ r_year + crisis_election3 * restraint_pre_norm, 
                        data = data_after_2nd_election, index = c("country_name_short", "year"), 
                        twostep = TRUE, panelwise = TRUE, rhoweight = "T1") %>% 
    coeftest(., vcov. = vcovPC(., pairwise = TRUE), save = TRUE)

app5_m2a_pw <- prais_winsten(np_share_cv_1 ~ r_year, data = data_after_2nd_election, 
                        index = c("country_name_short", "year"), 
                        twostep = TRUE, panelwise = TRUE, rhoweight = "T1") %>% 
    coeftest(., vcov. = vcovPC(., pairwise = TRUE), save = TRUE)
app5_m2b_pw <- prais_winsten(np_share_cv_1 ~ r_year + crisis_election3, 
                        data = data_after_2nd_election, 
                        index = c("country_name_short", "year"), 
                        twostep = TRUE, panelwise = TRUE, rhoweight = "T1") %>% 
    coeftest(., vcov. = vcovPC(., pairwise = TRUE), save = TRUE)
app5_m2c_pw <- prais_winsten(np_share_cv_1 ~ r_year + crisis_election3 * restraint_pre_norm, 
                        data = data_after_2nd_election, 
                        index = c("country_name_short", "year"), 
                        twostep = TRUE, panelwise = TRUE, rhoweight = "T1") %>% 
    coeftest(., vcov. = vcovPC(., pairwise = TRUE), save = TRUE)

app5_m3a_pw <- prais_winsten(np_share_pnp_1 ~ r_year, data = data_after_2nd_election, 
                        index = c("country_name_short", "year"), 
                        twostep = TRUE, panelwise = TRUE, rhoweight = "T1") %>% 
    coeftest(., vcov. = vcovPC(., pairwise = TRUE), save = TRUE)
app5_m3b_pw <- prais_winsten(np_share_pnp_1 ~ r_year + crisis_election3, 
                        data = data_after_2nd_election, index = c("country_name_short", "year"), 
                        twostep = TRUE, panelwise = TRUE, rhoweight = "T1") %>% 
    coeftest(., vcov. = vcovPC(., pairwise = TRUE), save = TRUE)
app5_m3c_pw <- prais_winsten(np_share_pnp_1 ~ r_year + crisis_election3 * restraint_pre_norm, 
                        data = data_after_2nd_election, index = c("country_name_short", "year"), 
                        twostep = TRUE, panelwise = TRUE, rhoweight = "T1") %>% 
    coeftest(., vcov. = vcovPC(., pairwise = TRUE), save = TRUE)

PW_NOTE <- "Models estimated using Prais-Winsten regression with panel-corrected standard errors. Models based on data without first and second election in each country."
modelsummary(
    list("All new parties" = app5_m1a_pw, 
         # "All new parties" = app5_m1b_pw, 
         "All new parties" = app5_m1c_pw, 
         "Partially new parties" = app5_m3a_pw, 
         # "Partially new parties" = app5_m3b_pw, 
         "Partially new parties" = app5_m3c_pw,
         "Genuinely new parties" = app5_m2a_pw, 
         # "Genuinely new parties" = app5_m2b_pw, 
         "Genuinely new parties" = app5_m2c_pw), 
    coef_rename = c(
        "r_year"="Year (0 = 1991)", 
        "crisis_election3"="Three elections after 2008", 
        "restraint_pre_norm"="Party restraint",
        "crisis_election3:restraint_pre_norm" = "Three elections after 2008 × Party restraint (pre-crisis)"
    ),
    stars = TRUE, 
    fmt = 2,
    gof_map = c("nobs", "r.squared", "adj.r.squared"),
    notes = PW_NOTE,
    output = "figs/app5_tab1.html"
)

## Tab 2 - crisis 2014 ------------------------------------
m10_pw_nc_2014a <- prais_winsten(np_share_nc_1 ~ r_year + crisis_election_2014,
    data = data_restrained, 
    index = c("country_name_short", "year"), 
    twostep = TRUE, panelwise = TRUE, rhoweight = "T1") %>% 
coeftest(., vcov. = vcovPC(., pairwise = TRUE), save = TRUE)

m10_pw_nc_2014 <- prais_winsten(np_share_nc_1 ~ r_year + crisis_election_2014 * restraint_pre_norm,
    data = data_restrained, 
    index = c("country_name_short", "year"), 
    twostep = TRUE, panelwise = TRUE, rhoweight = "T1") %>% 
coeftest(., vcov. = vcovPC(., pairwise = TRUE), save = TRUE)

m10_pw_cv_2014a <- prais_winsten(np_share_cv_1 ~ r_year + crisis_election_2014,
    data = data_restrained, 
    index = c("country_name_short", "year"), 
    twostep = TRUE, panelwise = TRUE, rhoweight = "T1") %>% 
coeftest(., vcov. = vcovPC(., pairwise = TRUE), save = TRUE)

m10_pw_cv_2014 <- prais_winsten(np_share_cv_1 ~ r_year + crisis_election_2014 * restraint_pre_norm,
    data = data_restrained, 
    index = c("country_name_short", "year"), 
    twostep = TRUE, panelwise = TRUE, rhoweight = "T1") %>% 
coeftest(., vcov. = vcovPC(., pairwise = TRUE), save = TRUE)

m10_pw_pnp_2014a <- prais_winsten(np_share_pnp_1 ~ r_year + crisis_election_2014,
    data = data_restrained, 
    index = c("country_name_short", "year"), 
    twostep = TRUE, panelwise = TRUE, rhoweight = "T1") %>% 
coeftest(., vcov. = vcovPC(., pairwise = TRUE), save = TRUE)

m10_pw_pnp_2014 <- prais_winsten(np_share_pnp_1 ~ r_year + crisis_election_2014 * restraint_pre_norm,
     data = data_restrained, 
     index = c("country_name_short", "year"), 
     twostep = TRUE, panelwise = TRUE, rhoweight = "T1") %>% 
coeftest(., vcov. = vcovPC(., pairwise = TRUE), save = TRUE)

modelsummary(
list(
    "All new parties" = m10_pw_nc_2014a, 
    "All new parties" = m10_pw_nc_2014, 
    "Partially new parties" = m10_pw_pnp_2014a,
    "Partially new parties" = m10_pw_pnp_2014,
    "Genuinely new parties" = m10_pw_cv_2014a, 
    "Genuinely new parties" = m10_pw_cv_2014
), 
coef_rename = c(
"r_year"="Year (0 = 1991)", 
"crisis_election_2014"="Crisis elections (2009-2014)", 
"region_typeOnce stable region" = "Once stable region", 
"restraint_pre_norm" = "Party restraint (pre-crisis)", 
"crisis_election_2014:restraint_pre_norm" = "Crisis elections (2009-2014) × Party restraint (pre-crisis)"
),
stars = TRUE,
notes = PW_NOTE,
fmt = 2,
gof_map = c("nobs", "r.squared", "adj.r.squared"),
output = "figs/tab2_2014.html"
)

## Tab 2 - crisis 2015 ------------------------------------
m10_pw_nc_2015a <- prais_winsten(np_share_nc_1 ~ r_year + crisis_election_2015,
    data = data_restrained, 
    index = c("country_name_short", "year"), 
    twostep = TRUE, panelwise = TRUE, rhoweight = "T1") %>% 
coeftest(., vcov. = vcovPC(., pairwise = TRUE), save = TRUE)

m10_pw_nc_2015 <- prais_winsten(np_share_nc_1 ~ r_year + crisis_election_2015 * restraint_pre_norm,
    data = data_restrained, 
    index = c("country_name_short", "year"), 
    twostep = TRUE, panelwise = TRUE, rhoweight = "T1") %>% 
coeftest(., vcov. = vcovPC(., pairwise = TRUE), save = TRUE)

m10_pw_cv_2015a <- prais_winsten(np_share_cv_1 ~ r_year + crisis_election_2015,
    data = data_restrained, 
    index = c("country_name_short", "year"), 
    twostep = TRUE, panelwise = TRUE, rhoweight = "T1") %>% 
coeftest(., vcov. = vcovPC(., pairwise = TRUE), save = TRUE)

m10_pw_cv_2015 <- prais_winsten(np_share_cv_1 ~ r_year + crisis_election_2015 * restraint_pre_norm,
    data = data_restrained, 
    index = c("country_name_short", "year"), 
    twostep = TRUE, panelwise = TRUE, rhoweight = "T1") %>% 
coeftest(., vcov. = vcovPC(., pairwise = TRUE), save = TRUE)

m10_pw_pnp_2015a <- prais_winsten(np_share_pnp_1 ~ r_year + crisis_election_2015,
    data = data_restrained, 
    index = c("country_name_short", "year"), 
    twostep = TRUE, panelwise = TRUE, rhoweight = "T1") %>% 
coeftest(., vcov. = vcovPC(., pairwise = TRUE), save = TRUE)

m10_pw_pnp_2015 <- prais_winsten(np_share_pnp_1 ~ r_year + crisis_election_2015 * restraint_pre_norm,
     data = data_restrained, 
     index = c("country_name_short", "year"), 
     twostep = TRUE, panelwise = TRUE, rhoweight = "T1") %>% 
coeftest(., vcov. = vcovPC(., pairwise = TRUE), save = TRUE)

modelsummary(
list(
    "All new parties" = m10_pw_nc_2015a, 
    "All new parties" = m10_pw_nc_2015, 
    "Partially new parties" = m10_pw_pnp_2015a, 
    "Partially new parties" = m10_pw_pnp_2015, 
    "Genuinely new parties" = m10_pw_cv_2015a, 
    "Genuinely new parties" = m10_pw_cv_2015
), 
coef_rename = c(
"r_year"="Year (0 = 1991)", 
"crisis_election_2015"="Crisis elections (2009-2015)", 
"region_typeOnce stable region" = "Once stable region", 
"restraint_pre_norm" = "Party restraint (pre-crisis)", 
"crisis_election_2015:restraint_pre_norm" = "Crisis elections (2009-2015) × Party restraint (pre-crisis)"
),
stars = TRUE,
notes = PW_NOTE,
fmt = 2,
gof_map = c("nobs", "r.squared", "adj.r.squared"),
output = "figs/tab2_2015.html"
)

## Tab 2 - one election ------------------------------------
m10_pw_nc_1a <- prais_winsten(np_share_nc_1 ~ r_year + post_crisis_election1,
    data = data_restrained, 
    index = c("country_name_short", "year"), 
    twostep = TRUE, panelwise = TRUE, rhoweight = "T1") %>% 
coeftest(., vcov. = vcovPC(., pairwise = TRUE), save = TRUE)

m10_pw_nc_1 <- prais_winsten(np_share_nc_1 ~ r_year + post_crisis_election1 * restraint_pre_norm,
    data = data_restrained, 
    index = c("country_name_short", "year"), 
    twostep = TRUE, panelwise = TRUE, rhoweight = "T1") %>% 
coeftest(., vcov. = vcovPC(., pairwise = TRUE), save = TRUE)

m10_pw_cv_1a <- prais_winsten(np_share_cv_1 ~ r_year + post_crisis_election1,
    data = data_restrained, 
    index = c("country_name_short", "year"), 
    twostep = TRUE, panelwise = TRUE, rhoweight = "T1") %>% 
coeftest(., vcov. = vcovPC(., pairwise = TRUE), save = TRUE)

m10_pw_cv_1 <- prais_winsten(np_share_cv_1 ~ r_year + post_crisis_election1 * restraint_pre_norm,
    data = data_restrained, 
    index = c("country_name_short", "year"), 
    twostep = TRUE, panelwise = TRUE, rhoweight = "T1") %>% 
coeftest(., vcov. = vcovPC(., pairwise = TRUE), save = TRUE)

m10_pw_pnp_1a <- prais_winsten(np_share_pnp_1 ~ r_year + post_crisis_election1,
    data = data_restrained, 
    index = c("country_name_short", "year"), 
    twostep = TRUE, panelwise = TRUE, rhoweight = "T1") %>% 
coeftest(., vcov. = vcovPC(., pairwise = TRUE), save = TRUE)

m10_pw_pnp_1 <- prais_winsten(np_share_pnp_1 ~ r_year + post_crisis_election1 * restraint_pre_norm,
     data = data_restrained, 
     index = c("country_name_short", "year"), 
     twostep = TRUE, panelwise = TRUE, rhoweight = "T1") %>% 
coeftest(., vcov. = vcovPC(., pairwise = TRUE), save = TRUE)

modelsummary(
list(
    "All new parties" = m10_pw_nc_1a, 
    "All new parties" = m10_pw_nc_1, 
    "Partially new parties" = m10_pw_pnp_1a, 
    "Partially new parties" = m10_pw_pnp_1, 
    "Genuinely new parties" = m10_pw_cv_1a, 
    "Genuinely new parties" = m10_pw_cv_1
), 
coef_rename = c(
"r_year"="Year (0 = 1991)", 
"post_crisis_election1"="First election after crisis", 
"region_typeOnce stable region" = "Once stable region", 
"restraint_pre_norm" = "Party restraint (pre-crisis)", 
"post_crisis_election1:restraint_pre_norm" = "First election after crisis × Party restraint (pre-crisis)"
),
stars = TRUE,
notes = PW_NOTE,
fmt = 2,
gof_map = c("nobs", "r.squared", "adj.r.squared"),
output = "figs/tab2_1election.html"
)

## Tab 2 - one election after accession ------------------------------------
eu_pw_nc_1a <- prais_winsten(np_share_nc_1 ~ r_year + post_accession_election1,
    data = data_restrained, 
    index = c("country_name_short", "year"), 
    twostep = TRUE, panelwise = TRUE, rhoweight = "T1") %>% 
coeftest(., vcov. = vcovPC(., pairwise = TRUE), save = TRUE)

eu_pw_cv_1a <- prais_winsten(np_share_cv_1 ~ r_year + post_accession_election1,
    data = data_restrained, 
    index = c("country_name_short", "year"), 
    twostep = TRUE, panelwise = TRUE, rhoweight = "T1") %>% 
coeftest(., vcov. = vcovPC(., pairwise = TRUE), save = TRUE)

eu_pw_pnp_1a <- prais_winsten(np_share_pnp_1 ~ r_year + post_accession_election1,
    data = data_restrained, 
    index = c("country_name_short", "year"), 
    twostep = TRUE, panelwise = TRUE, rhoweight = "T1") %>% 
coeftest(., vcov. = vcovPC(., pairwise = TRUE), save = TRUE)

modelsummary(
list(
    "All new parties" = eu_pw_nc_1a, 
    "Partially new parties" = eu_pw_pnp_1a, 
    "Genuinely new parties" = eu_pw_cv_1a
), 
coef_rename = c(
"r_year"="Year (0 = 1991)", 
"post_accession_election1"="First election after EU accession", 
"restraint_pre_norm" = "Party restraint (pre-crisis)", 
"post_accession_election1:restraint_pre_norm" = "First election after EU accession × Party restraint (pre-crisis)"
),
stars = TRUE,
notes = PW_NOTE,
fmt = 2,
gof_map = c("nobs", "r.squared", "adj.r.squared"),
output = "figs/tab3_eu_1election.html"
)

## Tab 2 - two elections after accession ------------------------------------
eu_pw_nc_2a <- prais_winsten(np_share_nc_1 ~ r_year + post_accession_election2,
    data = data_restrained, 
    index = c("country_name_short", "year"), 
    twostep = TRUE, panelwise = TRUE, rhoweight = "T1") %>% 
coeftest(., vcov. = vcovPC(., pairwise = TRUE), save = TRUE)

eu_pw_cv_2a <- prais_winsten(np_share_cv_1 ~ r_year + post_accession_election2,
    data = data_restrained, 
    index = c("country_name_short", "year"), 
    twostep = TRUE, panelwise = TRUE, rhoweight = "T1") %>% 
coeftest(., vcov. = vcovPC(., pairwise = TRUE), save = TRUE)

eu_pw_pnp_2a <- prais_winsten(np_share_pnp_1 ~ r_year + post_accession_election2,
    data = data_restrained, 
    index = c("country_name_short", "year"), 
    twostep = TRUE, panelwise = TRUE, rhoweight = "T1") %>% 
coeftest(., vcov. = vcovPC(., pairwise = TRUE), save = TRUE)

modelsummary(
list(
    "All new parties" = eu_pw_nc_2a, 
    "Partially new parties" = eu_pw_pnp_2a, 
    "Genuinely new parties" = eu_pw_cv_2a
), 
coef_rename = c(
"r_year"="Year (0 = 1991)", 
"post_accession_election2"="Two elections after EU accession", 
"restraint_pre_norm" = "Party restraint (pre-crisis)"
),
stars = TRUE,
notes = PW_NOTE,
fmt = 2,
gof_map = c("nobs", "r.squared", "adj.r.squared"),
output = "figs/tab3_eu_2election.html"
)
