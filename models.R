library(prais)
library(broom)
library(dplyr)
library(haven)
library(lmtest)
library(ggplot2)
library(ggeffects)
library(estimatr)
    
new_parties_data <- readRDS("data/data_final.rds")

data <- new_parties_data %>% 
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
        post_crisis_election3 = case_when(
            post_crisis == 0 ~ "Before crisis", 
            post_crisis == 1 & row_number() %in% 1:3~ "1-3 post-crisis election", 
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
    mutate(year = as.numeric(year), 
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
    

head(data)
unique(data$country_name_short)[!unique(data$country_name_short) %in% unique(restrained_change$country)]

data_restrained <- data %>% 
    left_join(restrained_change, by = c("country_name_short"="country", 
                                        "year")) %>% 
    mutate(country_name_short = factor(country_name_short)) %>% 
    rename(rank_election_within_country = election_no) %>% 
    group_by(country_name_short) %>% 
    mutate(n_postcrisis = cumsum(post_crisis)) %>% 
    mutate(restraint_pre = zoo::na.locf(restraint_pre, na.rm = FALSE) %>% 
               zoo::na.locf0(., fromLast = TRUE)) %>% 
    ungroup %>% 
    group_by(country_name_short, post_crisis) %>% 
    arrange(desc(year)) %>% 
    mutate(
        row = row_number(),
        n_precrisis = if_else(
        post_crisis == 0, row, 0
    )) %>% 
    select(-row) %>% 
    ungroup
    

# np_share_nc_1 = všechny nové strany
# np_share_cv_1 = úplně nové strany
# np_share_pnp_1 = částečně nové strany

used_data <- data %>% 
    mutate(country_name_short = factor(country_name_short)) %>% 
    rename(rank_election_within_country = election_no)

# Tab 1
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
modelsummary::modelsummary(
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

m7_pw <- prais_winsten(np_share_nc_1 ~ r_year + crisis_election, 
                       data = used_data %>% filter(region_type == "Never stable region"), 
                       index = c("country_name_short", "year"), 
                       twostep = TRUE, panelwise = TRUE, rhoweight = "T1") %>% 
    coeftest(., vcov. = vcovPC(., pairwise = TRUE), save = TRUE)
m8_pw <- prais_winsten(np_share_nc_1 ~ r_year + crisis_election,
                       data = used_data %>% filter(region_type == "Once stable region"), 
                       index = c("country_name_short", "year"), 
                       twostep = TRUE, panelwise = TRUE, rhoweight = "T1") %>% 
    coeftest(., vcov. = vcovPC(., pairwise = TRUE), save = TRUE)
m9_pw <- prais_winsten(np_share_nc_1 ~ r_year * region_type + crisis_election,
                       data = used_data, 
                       index = c("country_name_short", "year"), 
                       twostep = TRUE, panelwise = TRUE, rhoweight = "T1") %>% 
    coeftest(., vcov. = vcovPC(., pairwise = TRUE), save = TRUE)
m10_pw <- prais_winsten(np_share_nc_1 ~ r_year + crisis_election * region_type,
                       data = used_data, 
                       index = c("country_name_short", "year"), 
                       twostep = TRUE, panelwise = TRUE, rhoweight = "T1") %>% 
    coeftest(., vcov. = vcovPC(., pairwise = TRUE), save = TRUE)

modelsummary::modelsummary(
    list("Never stable" = m7_pw, 
         "Once stable" = m8_pw, 
         "All" = m9_pw, 
         "All" = m10_pw), 
    coef_rename = c(
        "r_year"="Year (0 = 1991)", 
        "crisis_election"="Two elections after 2008", 
        "region_typeOnce stable region" = "Once stable region", 
        "r_year:region_typeOnce stable region" = "Year × Once stable region", 
        "crisis_election:region_typeOnce stable region" = "Two elections after 2008 × Once stable region"
    ),
    stars = TRUE,
    notes = PW_NOTE,
    fmt = 2,
    gof_map = c("nobs", "r.squared", "adj.r.squared"),
    output = "figs/tab2.html"
)

## models with election order ---------------------------------
m1a_2pw <- prais_winsten(np_share_nc_1 ~ rank_election_within_country,
                         data = used_data, 
                         index = c("country_name_short", "year"), 
                         twostep = TRUE, panelwise = TRUE, rhoweight = "T1") %>% 
    coeftest(., vcov. = vcovPC(., pairwise = TRUE), save = TRUE)
m1b_2pw <- prais_winsten(np_share_nc_1 ~ rank_election_within_country + crisis_election,
                         data = used_data, 
                         index = c("country_name_short", "year"), 
                         twostep = TRUE, panelwise = TRUE, rhoweight = "T1") %>% 
    coeftest(., vcov. = vcovPC(., pairwise = TRUE), save = TRUE)

m2a_2pw <- prais_winsten(np_share_cv_1 ~ rank_election_within_country,
                         data = used_data, 
                         index = c("country_name_short", "year"), 
                         twostep = TRUE, panelwise = TRUE, rhoweight = "T1") %>% 
    coeftest(., vcov. = vcovPC(., pairwise = TRUE), save = TRUE)
m2b_2pw <- prais_winsten(np_share_cv_1 ~ rank_election_within_country + crisis_election, 
                         data = used_data, 
                         index = c("country_name_short", "year"), 
                         twostep = TRUE, panelwise = TRUE, rhoweight = "T1") %>% 
    coeftest(., vcov. = vcovPC(., pairwise = TRUE), save = TRUE)

m3a_2pw <- prais_winsten(np_share_pnp_1 ~ rank_election_within_country,
                         data = used_data, 
                         index = c("country_name_short", "year"), 
                         twostep = TRUE, panelwise = TRUE, rhoweight = "T1") %>% 
    coeftest(., vcov. = vcovPC(., pairwise = TRUE), save = TRUE)
m3b_2pw <- prais_winsten(np_share_pnp_1 ~ rank_election_within_country + crisis_election, 
                         data = used_data, 
                         index = c("country_name_short", "year"), 
                         twostep = TRUE, panelwise = TRUE, rhoweight = "T1") %>% 
    coeftest(., vcov. = vcovPC(., pairwise = TRUE), save = TRUE)

modelsummary::modelsummary(
    list("All new parties" = m1a_2pw, 
         "All new parties" = m1b_2pw, 
         "Genuinely new parties" = m2a_2pw, 
         "Genuinely new parties" = m2b_2pw, 
         "Partially new parties" = m3a_2pw, 
         "Partially new parties" = m3b_2pw), 
    coef_rename = c(
        "rank_election_within_country"="Election number", 
        "crisis_election"="Two elections after 2008"
    ),
    stars = TRUE, 
    notes = PW_NOTE,
    fmt = 2,
    gof_map = c("nobs", "r.squared", "adj.r.squared"),
    output = "figs/tab1_election_rank.html"
)

## models without the first and second elections ----------------------
data_after_2nd_election <- used_data %>% 
    filter(!fst_snd_election)

# Tab 1
m1a_thd <- prais_winsten(np_share_nc_1 ~ r_year, 
                         data = data_after_2nd_election, 
                         index = c("country_name_short", "year"), 
                         twostep = TRUE, panelwise = TRUE, rhoweight = "T1") %>% 
    coeftest(., vcov. = vcovPC(., pairwise = TRUE), save = TRUE)
m1b_thd <- prais_winsten(np_share_nc_1 ~ r_year + crisis_election, 
                         data = data_after_2nd_election, 
                         index = c("country_name_short", "year"), 
                         twostep = TRUE, panelwise = TRUE, rhoweight = "T1") %>% 
    coeftest(., vcov. = vcovPC(., pairwise = TRUE), save = TRUE)

m2a_thd <- prais_winsten(np_share_cv_1 ~ r_year, 
                         data = data_after_2nd_election, 
                         index = c("country_name_short", "year"), 
                         twostep = TRUE, panelwise = TRUE, rhoweight = "T1") %>% 
    coeftest(., vcov. = vcovPC(., pairwise = TRUE), save = TRUE)
m2b_thd <- prais_winsten(np_share_cv_1 ~ r_year + crisis_election, 
                         data = data_after_2nd_election, 
                         index = c("country_name_short", "year"), 
                         twostep = TRUE, panelwise = TRUE, rhoweight = "T1") %>% 
    coeftest(., vcov. = vcovPC(., pairwise = TRUE), save = TRUE)

m3a_thd <- prais_winsten(np_share_pnp_1 ~ r_year, 
                         data = data_after_2nd_election, 
                         index = c("country_name_short", "year"), 
                         twostep = TRUE, panelwise = TRUE, rhoweight = "T1") %>% 
    coeftest(., vcov. = vcovPC(., pairwise = TRUE), save = TRUE)
m3b_thd <- prais_winsten(np_share_pnp_1 ~ r_year + crisis_election, 
                         data = data_after_2nd_election, 
                         index = c("country_name_short", "year"), 
                         twostep = TRUE, panelwise = TRUE, rhoweight = "T1") %>% 
    coeftest(., vcov. = vcovPC(., pairwise = TRUE), save = TRUE)

modelsummary::modelsummary(
    list("All new parties" = m1a_thd, 
         "All new parties" = m1b_thd, 
         "Genuinely new parties" = m2a_thd, 
         "Genuinely new parties" = m2b_thd, 
         "Partially new parties" = m3a_thd, 
         "Partially new parties" = m3b_thd), 
    coef_rename = c(
        "r_year"="Year (0 = 1991)", 
        "crisis_election"="Two elections after 2008"
    ),
    output = "figs/tab1_since_third_election.html",
    notes = PW_NOTE,
    fmt = 2,
    gof_map = c("nobs", "r.squared", "adj.r.squared"),
    stars = TRUE
)

m7_thd <- prais_winsten(np_share_nc_1 ~ r_year + crisis_election, 
                        data = data_after_2nd_election %>% filter(region_type == "Never stable region"), 
                        index = c("country_name_short", "year"), 
                        twostep = TRUE, panelwise = TRUE, rhoweight = "T1") %>% 
    coeftest(., vcov. = vcovPC(., pairwise = TRUE), save = TRUE)
m8_thd <- prais_winsten(np_share_nc_1 ~ r_year + crisis_election, 
                        data = data_after_2nd_election %>% filter(region_type == "Once stable region"), 
                        index = c("country_name_short", "year"), 
                        twostep = TRUE, panelwise = TRUE, rhoweight = "T1") %>% 
    coeftest(., vcov. = vcovPC(., pairwise = TRUE), save = TRUE)
m9_thd <- prais_winsten(np_share_nc_1 ~ r_year * region_type + crisis_election, 
                        data = data_after_2nd_election, 
                        index = c("country_name_short", "year"), 
                        twostep = TRUE, panelwise = TRUE, rhoweight = "T1") %>% 
    coeftest(., vcov. = vcovPC(., pairwise = TRUE), save = TRUE)
m10_thd <- prais_winsten(np_share_nc_1 ~ r_year + crisis_election * region_type, 
                         data = data_after_2nd_election, 
                        index = c("country_name_short", "year"), 
                        twostep = TRUE, panelwise = TRUE, rhoweight = "T1") %>% 
    coeftest(., vcov. = vcovPC(., pairwise = TRUE), save = TRUE)

modelsummary::modelsummary(
    list("Never stable" = m7_thd, 
         "Once stable" = m8_thd, 
         "All" = m9_thd, 
         "All" = m10_thd), 
    coef_rename = c(
        "r_year"="Year (0 = 1991)", 
        "crisis_election"="Two elections after 2008", 
        "region_typeOnce stable region" = "Once stable region", 
        "r_year:region_typeOnce stable region" = "Year × Once stable region", 
        "crisis_election:region_typeOnce stable region" = "Two elections after 2008 × Once stable region"
    ),
    stars = TRUE, 
    notes = PW_NOTE,
    fmt = 2,
    gof_map = c("nobs", "r.squared", "adj.r.squared"),
    output = "figs/tab2_since_third_election.html"
)

## models with legislative threshold -------------------------------
app4_m1a <- prais_winsten(np_share_nc_leg ~ r_year, data = used_data, 
                         index = c("country_name_short", "year"), 
                         twostep = TRUE, panelwise = TRUE, rhoweight = "T1") %>% 
    coeftest(., vcov. = vcovPC(., pairwise = TRUE), save = TRUE)
app4_m1b <- prais_winsten(np_share_nc_leg ~ r_year + crisis_election, 
                          data = used_data,
                          index = c("country_name_short", "year"), 
                          twostep = TRUE, panelwise = TRUE, rhoweight = "T1") %>% 
    coeftest(., vcov. = vcovPC(., pairwise = TRUE), save = TRUE)

app4_m2a <- prais_winsten(np_share_cv_leg ~ r_year, data = used_data, 
                          index = c("country_name_short", "year"), 
                          twostep = TRUE, panelwise = TRUE, rhoweight = "T1") %>% 
    coeftest(., vcov. = vcovPC(., pairwise = TRUE), save = TRUE)
app4_m2b <- prais_winsten(np_share_cv_leg ~ r_year + crisis_election, 
                          data = used_data,
                          index = c("country_name_short", "year"), 
                          twostep = TRUE, panelwise = TRUE, rhoweight = "T1") %>% 
    coeftest(., vcov. = vcovPC(., pairwise = TRUE), save = TRUE)

app4_m3a <- prais_winsten(np_share_pnp_leg ~ r_year, data = used_data, 
                          index = c("country_name_short", "year"), 
                          twostep = TRUE, panelwise = TRUE, rhoweight = "T1") %>% 
    coeftest(., vcov. = vcovPC(., pairwise = TRUE), save = TRUE)
app4_m3b <- prais_winsten(np_share_pnp_leg ~ r_year + crisis_election, 
                          data = used_data,
                          index = c("country_name_short", "year"), 
                          twostep = TRUE, panelwise = TRUE, rhoweight = "T1") %>% 
    coeftest(., vcov. = vcovPC(., pairwise = TRUE), save = TRUE)

modelsummary::modelsummary(
    list("All new parties" = app4_m1a, 
         "All new parties" = app4_m1b, 
         "Genuinely new parties" = app4_m2a, 
         "Genuinely new parties" = app4_m2b, 
         "Partially new parties" = app4_m3a, 
         "Partially new parties" = app4_m3b), 
    coef_rename = c(
        "r_year"="Year (0 = 1991)", 
        "crisis_election"="Two elections after 2008"
    ),
    title = "Models with new party share (legislative threshold)",
    notes = PW_NOTE,
    fmt = 2,
    output = "figs/tab1_np_share_leg.html",
    gof_map = c("nobs", "r.squared", "adj.r.squared"),
    stars = TRUE
)

app4_m7 <- prais_winsten(np_share_nc_leg ~ r_year + crisis_election, 
                          data = used_data %>% filter(region_type == "Never stable region"), 
                          index = c("country_name_short", "year"), 
                          twostep = TRUE, panelwise = TRUE, rhoweight = "T1") %>% 
    coeftest(., vcov. = vcovPC(., pairwise = TRUE), save = TRUE)

app4_m8 <- prais_winsten(np_share_nc_leg ~ r_year + crisis_election, 
                         data = used_data %>% filter(region_type == "Once stable region"), 
                         index = c("country_name_short", "year"), 
                         twostep = TRUE, panelwise = TRUE, rhoweight = "T1") %>% 
    coeftest(., vcov. = vcovPC(., pairwise = TRUE), save = TRUE)

app4_m9 <- prais_winsten(np_share_nc_leg ~ r_year * region_type + crisis_election, 
                         data = used_data, 
                         index = c("country_name_short", "year"), 
                         twostep = TRUE, panelwise = TRUE, rhoweight = "T1") %>% 
    coeftest(., vcov. = vcovPC(., pairwise = TRUE), save = TRUE)

app4_m10 <- prais_winsten(np_share_nc_leg ~ r_year + crisis_election * region_type, 
                         data = used_data, 
                         index = c("country_name_short", "year"), 
                         twostep = TRUE, panelwise = TRUE, rhoweight = "T1") %>% 
    coeftest(., vcov. = vcovPC(., pairwise = TRUE), save = TRUE)

modelsummary::modelsummary(
    list("Never stable" = app4_m7, 
         "Once stable" = app4_m8, 
         "All" = app4_m9, 
         "All" = app4_m10), 
    coef_rename = c(
        "r_year"="Year (0 = 1991)", 
        "crisis_election"="Two elections after 2008", 
        "region_typeOnce stable region" = "Once stable region", 
        "r_year:region_typeOnce stable region" = "Year × Once stable region", 
        "crisis_election:region_typeOnce stable region" = "Two elections after 2008 × Once stable region"
    ),
    stars = TRUE, 
    title = "Models with new party share (legislative threshold)",
    notes = PW_NOTE,
    fmt = 2,
    gof_map = c("nobs", "r.squared", "adj.r.squared"),
    output = "figs/tab2_np_share_leg.html"
)

## models with party number -----------------------------------
app5_m1a <- prais_winsten(np_number_nc_1 ~ r_year,
                          data = used_data, 
                          index = c("country_name_short", "year"), 
                          twostep = TRUE, panelwise = TRUE, rhoweight = "T1") %>% 
    coeftest(., vcov. = vcovPC(., pairwise = TRUE), save = TRUE)
app5_m1b <- prais_winsten(np_number_nc_1 ~ r_year + crisis_election, 
                          data = used_data, 
                          index = c("country_name_short", "year"), 
                          twostep = TRUE, panelwise = TRUE, rhoweight = "T1") %>% 
    coeftest(., vcov. = vcovPC(., pairwise = TRUE), save = TRUE)

app5_m2a <- prais_winsten(np_number_cv_1 ~ r_year,
                          data = used_data, 
                          index = c("country_name_short", "year"), 
                          twostep = TRUE, panelwise = TRUE, rhoweight = "T1") %>% 
    coeftest(., vcov. = vcovPC(., pairwise = TRUE), save = TRUE)
app5_m2b <- prais_winsten(np_number_cv_1 ~ r_year + crisis_election, 
                          data = used_data, 
                          index = c("country_name_short", "year"), 
                          twostep = TRUE, panelwise = TRUE, rhoweight = "T1") %>% 
    coeftest(., vcov. = vcovPC(., pairwise = TRUE), save = TRUE)

app5_m3a <- prais_winsten(np_number_pnp_1 ~ r_year,
                          data = used_data, 
                          index = c("country_name_short", "year"), 
                          twostep = TRUE, panelwise = TRUE, rhoweight = "T1") %>% 
    coeftest(., vcov. = vcovPC(., pairwise = TRUE), save = TRUE)
app5_m3b <- prais_winsten(np_number_pnp_1 ~ r_year + crisis_election, 
                          data = used_data, 
                          index = c("country_name_short", "year"), 
                          twostep = TRUE, panelwise = TRUE, rhoweight = "T1") %>% 
    coeftest(., vcov. = vcovPC(., pairwise = TRUE), save = TRUE)

modelsummary::modelsummary(
    list("All new parties" = app5_m1a, 
         "All new parties" = app5_m1b, 
         "Genuinely new parties" = app5_m2a, 
         "Genuinely new parties" = app5_m2b, 
         "Partially new parties" = app5_m3a, 
         "Partially new parties" = app5_m3b), 
    coef_rename = c(
        "r_year"="Year (0 = 1991)", 
        "crisis_election"="Two elections after 2008"
    ),
    title = "Models of new party count",
    notes = PW_NOTE,
    fmt = 2,
    output = "figs/tab1_np_number.html",
    gof_map = c("nobs", "r.squared", "adj.r.squared"),
    stars = TRUE
)

app5_m7 <- prais_winsten(np_number_nc_1 ~ r_year + crisis_election, 
                         data = used_data %>% filter(region_type == "Never stable region"), 
                         index = c("country_name_short", "year"), 
                         twostep = TRUE, panelwise = TRUE, rhoweight = "T1") %>% 
    coeftest(., vcov. = vcovPC(., pairwise = TRUE), save = TRUE)
app5_m8 <- prais_winsten(np_number_nc_1 ~ r_year + crisis_election, 
                         data = used_data %>% filter(region_type == "Once stable region"), 
                         index = c("country_name_short", "year"), 
                         twostep = TRUE, panelwise = TRUE, rhoweight = "T1") %>% 
    coeftest(., vcov. = vcovPC(., pairwise = TRUE), save = TRUE)
app5_m9 <- prais_winsten(np_number_nc_1 ~ r_year * region_type + crisis_election, 
                         data = used_data, 
                         index = c("country_name_short", "year"), 
                         twostep = TRUE, panelwise = TRUE, rhoweight = "T1") %>% 
    coeftest(., vcov. = vcovPC(., pairwise = TRUE), save = TRUE)
app5_m10 <- prais_winsten(np_number_nc_1 ~ r_year + crisis_election * region_type, 
                         data = used_data, 
                         index = c("country_name_short", "year"), 
                         twostep = TRUE, panelwise = TRUE, rhoweight = "T1") %>% 
    coeftest(., vcov. = vcovPC(., pairwise = TRUE), save = TRUE)

modelsummary::modelsummary(
    list("Never stable" = app5_m7, 
         "Once stable" = app5_m8, 
         "All" = app5_m9, 
         "All" = app5_m10), 
    coef_rename = c(
        "r_year"="Year (0 = 1991)", 
        "crisis_election"="Two elections after 2008", 
        "region_typeOnce stable region" = "Once stable region", 
        "r_year:region_typeOnce stable region" = "Year × Once stable region", 
        "crisis_election:region_typeOnce stable region" = "Two elections after 2008 × Once stable region"
    ),
    stars = TRUE, 
    title = "Models of new party count",
    notes = PW_NOTE,
    fmt = 2,
    gof_map = c("nobs", "r.squared", "adj.r.squared"),
    output = "figs/tab2_np_number.html"
)

app5b_m1a <- prais_winsten(np_number_nc_leg ~ r_year,
                          data = used_data, 
                          index = c("country_name_short", "year"), 
                          twostep = TRUE, panelwise = TRUE, rhoweight = "T1") %>% 
    coeftest(., vcov. = vcovPC(., pairwise = TRUE), save = TRUE)
app5b_m1b <- prais_winsten(np_number_nc_leg ~ r_year + crisis_election, 
                          data = used_data, 
                          index = c("country_name_short", "year"), 
                          twostep = TRUE, panelwise = TRUE, rhoweight = "T1") %>% 
    coeftest(., vcov. = vcovPC(., pairwise = TRUE), save = TRUE)

app5b_m2a <- prais_winsten(np_number_cv_leg ~ r_year,
                          data = used_data, 
                          index = c("country_name_short", "year"), 
                          twostep = TRUE, panelwise = TRUE, rhoweight = "T1") %>% 
    coeftest(., vcov. = vcovPC(., pairwise = TRUE), save = TRUE)
app5b_m2b <- prais_winsten(np_number_cv_leg ~ r_year + crisis_election, 
                          data = used_data, 
                          index = c("country_name_short", "year"), 
                          twostep = TRUE, panelwise = TRUE, rhoweight = "T1") %>% 
    coeftest(., vcov. = vcovPC(., pairwise = TRUE), save = TRUE)

app5b_m3a <- prais_winsten(np_number_pnp_leg ~ r_year,
                          data = used_data, 
                          index = c("country_name_short", "year"), 
                          twostep = TRUE, panelwise = TRUE, rhoweight = "T1") %>% 
    coeftest(., vcov. = vcovPC(., pairwise = TRUE), save = TRUE)
app5b_m3b <- prais_winsten(np_number_pnp_leg ~ r_year + crisis_election, 
                          data = used_data, 
                          index = c("country_name_short", "year"), 
                          twostep = TRUE, panelwise = TRUE, rhoweight = "T1") %>% 
    coeftest(., vcov. = vcovPC(., pairwise = TRUE), save = TRUE)

modelsummary::modelsummary(
    list("All new parties" = app5b_m1a, 
         "All new parties" = app5b_m1b, 
         "Genuinely new parties" = app5b_m2a, 
         "Genuinely new parties" = app5b_m2b, 
         "Partially new parties" = app5b_m3a, 
         "Partially new parties" = app5b_m3b), 
    coef_rename = c(
        "r_year"="Year (0 = 1991)", 
        "crisis_election"="Two elections after 2008"
    ),
    title = "Models of new party count",
    notes = PW_NOTE,
    fmt = 2,
    gof_map = c("nobs", "r.squared", "adj.r.squared"),
    output = "figs/tab3_np_number.html",
    stars = TRUE
)

app5b_m7 <- prais_winsten(np_number_nc_leg ~ r_year + crisis_election, 
                         data = used_data %>% filter(region_type == "Never stable region"), 
                         index = c("country_name_short", "year"), 
                         twostep = TRUE, panelwise = TRUE, rhoweight = "T1") %>% 
    coeftest(., vcov. = vcovPC(., pairwise = TRUE), save = TRUE)
app5b_m8 <- prais_winsten(np_number_nc_leg ~ r_year + crisis_election, 
                         data = used_data %>% filter(region_type == "Once stable region"), 
                         index = c("country_name_short", "year"), 
                         twostep = TRUE, panelwise = TRUE, rhoweight = "T1") %>% 
    coeftest(., vcov. = vcovPC(., pairwise = TRUE), save = TRUE)
app5b_m9 <- prais_winsten(np_number_nc_leg ~ r_year * region_type + crisis_election, 
                         data = used_data, 
                         index = c("country_name_short", "year"), 
                         twostep = TRUE, panelwise = TRUE, rhoweight = "T1") %>% 
    coeftest(., vcov. = vcovPC(., pairwise = TRUE), save = TRUE)
app5b_m10 <- prais_winsten(np_number_nc_leg ~ r_year + crisis_election * region_type, 
                          data = used_data, 
                          index = c("country_name_short", "year"), 
                          twostep = TRUE, panelwise = TRUE, rhoweight = "T1") %>% 
    coeftest(., vcov. = vcovPC(., pairwise = TRUE), save = TRUE)

modelsummary::modelsummary(
    list("Never stable" = app5b_m7, 
         "Once stable" = app5b_m8, 
         "All" = app5b_m9, 
         "All" = app5b_m10), 
    coef_rename = c(
        "r_year"="Year (0 = 1991)", 
        "crisis_election"="Two elections after 2008", 
        "region_typeOnce stable region" = "Once stable region", 
        "r_year:region_typeOnce stable region" = "Year × Once stable region", 
        "crisis_election:region_typeOnce stable region" = "Two elections after 2008 × Once stable region"
    ),
    stars = TRUE, 
    output = "figs/tab4_np_number.html",
    fmt = 2,
    gof_map = c("nobs", "r.squared", "adj.r.squared"),
    title = "Models of new party count", 
    notes = PW_NOTE
)



m10_pw <- prais_winsten(np_share_nc_1 ~ r_year + crisis_election * restraint_pre,
                        data = data_restrained, 
                        index = c("country_name_short", "year"), 
                        twostep = TRUE, panelwise = TRUE, rhoweight = "T1") %>% 
    coeftest(., vcov. = vcovPC(., pairwise = TRUE), save = TRUE)

modelsummary::modelsummary(m10_pw, stars = TRUE)

tidy(m10_pw)

predicted_values <- tidyr::expand_grid(
    restraint_pre = c(-2, -1, 0),
    crisis_election = c(0, 1)
) %>% 
    mutate(year = 16) %>% 
    mutate(predicted_value = 8.24 + -0.4 * year + 24.2 * crisis_election + -15.8*restraint_pre + 22.1 * crisis_election * restraint_pre)

ggplot(predicted_values, aes(x = restraint_pre, y = predicted_value, colour = factor(crisis_election))) + 
    geom_point() + 
    geom_line() + 
    theme_minimal()

m10_pw3 <- prais_winsten(np_share_nc_1 ~ r_year + crisis_election * restraint_pre,
                        data = data_restrained %>% 
                            filter((post_crisis == 0 & n_precrisis <= 3) | 
                                       (post_crisis == 1 & n_postcrisis <= 3)), 
                        index = c("country_name_short", "year"), 
                        twostep = TRUE, panelwise = TRUE, rhoweight = "T1") %>% 
    coeftest(., vcov. = vcovPC(., pairwise = TRUE), save = TRUE)

modelsummary::modelsummary(m10_pw3, stars = TRUE)

tidy(m10_pw3)

predicted_values3 <- tidyr::expand_grid(
    restraint_pre = c(-2, -1, 0),
    crisis_election = c(0, 1)
) %>% 
    mutate(year = 16) %>% 
    mutate(predicted_value = 10.0 + -0.46 * year + 23.2 * crisis_election + -12.7*restraint_pre + 17.5 * crisis_election * restraint_pre)

ggplot(predicted_values3, aes(x = restraint_pre, y = predicted_value, colour = factor(crisis_election))) + 
    geom_point() + 
    geom_line() + 
    theme_minimal()


m10_pw_b <- prais_winsten(np_share_nc_1 ~ r_year + crisis_election * region_type,
                        data = data_restrained, 
                        index = c("country_name_short", "year"), 
                        twostep = TRUE, panelwise = TRUE, rhoweight = "T1") %>% 
    coeftest(., vcov. = vcovPC(., pairwise = TRUE), save = TRUE)

modelsummary::modelsummary(m10_pw_b, stars = TRUE)

tidy(m10_pw_b)

predicted_values_b <- tidyr::expand_grid(
    once_stable = c(0, 1),
    crisis_election = c(0, 1)
) %>% 
    mutate(year = 16) %>% 
    mutate(predicted_value = 25.4 + -0.35 * year + -2.64 * crisis_election + -11.9*once_stable + 20.6 * crisis_election * once_stable)

ggplot(predicted_values_b, aes(x = crisis_election, y = predicted_value, colour = factor(once_stable))) + 
    geom_point() + 
    geom_line() + 
    theme_minimal()

