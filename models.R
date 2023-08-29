library(dplyr)
library(ggplot2)
library(ggeffects)
library(estimatr)

data <- readRDS("data/elects_unique_cee.rds") %>% 
    mutate(year = lubridate::year(election_date), 
           post_crisis = as.numeric(year > 2008),
           region_type = case_when(
               country_name %in% c("Croatia", "Czech Republic", "Hungary", 
                                   "Romania", "Slovenia") ~ "Once stable region", 
               country_name %in% c("Estonia", "Lithuania", "Latvia", "Poland", 
                                   "Bulgaria", "Slovakia") ~ "Never stable region"
           )) %>% 
    group_by(country_name, post_crisis) %>% 
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
        )
    )

# np_share_nc_1 = všechny nové strany
# np_share_cv_1 = úplně nové strany
# np_share_pnp_1 = částečně nové strany

# Tab 1
m1a <- lm_robust(np_share_nc_1 ~ r_year, data = data, 
                 clusters = country_name_short)
m1b <- lm_robust(np_share_nc_1 ~ r_year + crisis_election, 
                 data = data, clusters = country_name_short)

m2a <- lm_robust(np_share_cv_1 ~ r_year, data = data, 
                 clusters = country_name_short)
m2b <- lm_robust(np_share_cv_1 ~ r_year + crisis_election, 
                 data = data, clusters = country_name_short)

m3a <- lm_robust(np_share_pnp_1 ~ r_year, data = data, 
                 clusters = country_name_short)
m3b <- lm_robust(np_share_pnp_1 ~ r_year + crisis_election, 
                 data = data, clusters = country_name_short)

modelsummary::modelsummary(
    list("All new parties" = m1a, 
         "All new parties" = m1b, 
         "Genuinely new parties" = m2a, 
         "Genuinely new parties" = m2b, 
         "Partially new parties" = m3a, 
         "Partially new parties" = m3b), 
    coef_rename = c(
        "r_year"="Year (0 = 1991)", 
        "crisis_election"="Two elections after 2008"
    ),
    gof_omit = "Std.Errors", # Omit "Std.Errors" clustered
    stars = TRUE
    # output = "figs/tab1.html"
)

m7 <- lm_robust(np_share_nc_1 ~ r_year + crisis_election, 
                data = data %>% filter(region_type == "Never stable region"), 
                clusters = country_name_short)
m8 <- lm_robust(np_share_nc_1 ~ r_year + crisis_election, 
                data = data %>% filter(region_type == "Once stable region"), 
                clusters = country_name_short)
m9 <- lm_robust(np_share_nc_1 ~ r_year * region_type + crisis_election, 
                data = data, 
                clusters = country_name_short)
m10 <- lm_robust(np_share_nc_1 ~ r_year + crisis_election * region_type, 
                data = data, 
                clusters = country_name_short)

modelsummary::modelsummary(
    list("Never stable" = m7, 
         "Once stable" = m8, 
         "All" = m9, 
         "All" = m10), 
    coef_rename = c(
        "r_year"="Year (0 = 1991)", 
        "crisis_election"="Two elections after 2008", 
        "region_typeOnce stable region" = "Once stable region", 
        "r_year:region_typeOnce stable region" = "Year × Once stable region", 
        "crisis_election:region_typeOnce stable region" = "Two elections after 2008 × Once stable region"
    ),
    stars = TRUE, 
    gof_omit = "Std.Errors",
    output = "figs/tab2.html"
)

## modely s pořadím voleb ---------------------------------
m1a_2 <- lm_robust(np_share_nc_1 ~ rank_election_within_country, data = data, 
                 clusters = country_name_short)
m1b_2 <- lm_robust(np_share_nc_1 ~ rank_election_within_country + crisis_election, 
                 data = data, clusters = country_name_short)

m2a_2 <- lm_robust(np_share_cv_1 ~ rank_election_within_country, data = data, 
                 clusters = country_name_short)
m2b_2 <- lm_robust(np_share_cv_1 ~ rank_election_within_country + crisis_election, 
                 data = data, clusters = country_name_short)

m3a_2 <- lm_robust(np_share_pnp_1 ~ rank_election_within_country, data = data, 
                 clusters = country_name_short)
m3b_2 <- lm_robust(np_share_pnp_1 ~ rank_election_within_country + crisis_election, 
                 data = data, clusters = country_name_short)

modelsummary::modelsummary(
    list("All new parties" = m1a_2, 
         "All new parties" = m1b_2, 
         "Genuinely new parties" = m2a_2, 
         "Genuinely new parties" = m2b_2, 
         "Partially new parties" = m3a_2, 
         "Partially new parties" = m3b_2), 
    coef_rename = c(
        "rank_election_within_country"="Election number", 
        "crisis_election"="Two elections after 2008"
    ),
    stars = TRUE, 
    gof_omit = "Std.Errors",
    output = "figs/election_rank_tab1.html"
)

## modely bez prvních a druhých voleb ----------------------
data_after_2nd_election <- data %>% 
    filter(!fst_snd_election)

# Tab 1
m1a_thd <- lm_robust(np_share_nc_1 ~ r_year, data = data_after_2nd_election, 
                 clusters = country_name_short)
m1b_thd <- lm_robust(np_share_nc_1 ~ r_year + crisis_election, 
                 data = data_after_2nd_election, 
                 clusters = country_name_short)

m2a_thd <- lm_robust(np_share_cv_1 ~ r_year, data = data_after_2nd_election, 
                 clusters = country_name_short)
m2b_thd <- lm_robust(np_share_cv_1 ~ r_year + crisis_election, 
                 data = data_after_2nd_election, 
                 clusters = country_name_short)

m3a_thd <- lm_robust(np_share_pnp_1 ~ r_year, data = data_after_2nd_election, 
                 clusters = country_name_short)
m3b_thd <- lm_robust(np_share_pnp_1 ~ r_year + crisis_election, 
                 data = data_after_2nd_election, 
                 clusters = country_name_short)

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
    gof_omit = "Std.Errors", # Omit "Std.Errors" clustered
    output = "figs/tab1_since_third_election.html",
    stars = TRUE
)

m7_thd <- lm_robust(np_share_nc_1 ~ r_year + crisis_election, 
                data = data_after_2nd_election %>% filter(region_type == "Never stable region"), 
                clusters = country_name_short)
m8_thd <- lm_robust(np_share_nc_1 ~ r_year + crisis_election, 
                data = data_after_2nd_election %>% filter(region_type == "Once stable region"), 
                clusters = country_name_short)
m9_thd <- lm_robust(np_share_nc_1 ~ r_year * region_type + crisis_election, 
                data = data_after_2nd_election, 
                clusters = country_name_short)
m10_thd <- lm_robust(np_share_nc_1 ~ r_year + crisis_election * region_type, 
                 data = data_after_2nd_election, 
                 clusters = country_name_short)

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
    output = "figs/tab2_since_third_election.html",
    gof_omit = "Std.Errors"
)

## modely s threshold -------------------------------------
app4_m1a <- lm_robust(np_share_nc_leg ~ r_year, data = data, 
                 clusters = country_name_short)
app4_m1b <- lm_robust(np_share_nc_leg ~ r_year + crisis_election, 
                 data = data, clusters = country_name_short)

app4_m2a <- lm_robust(np_share_cv_leg ~ r_year, data = data, 
                 clusters = country_name_short)
app4_m2b <- lm_robust(np_share_cv_leg ~ r_year + crisis_election, 
                 data = data, clusters = country_name_short)

app4_m3a <- lm_robust(np_share_pnp_leg ~ r_year, data = data, 
                 clusters = country_name_short)
app4_m3b <- lm_robust(np_share_pnp_leg ~ r_year + crisis_election, 
                 data = data, clusters = country_name_short)

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
    output = "figs/np_share_leg_tab1.html",
    gof_omit = "Std.Errors",
    stars = TRUE
)

app4_m7 <- lm_robust(np_share_nc_leg ~ r_year + crisis_election, 
                data = data %>% filter(region_type == "Never stable region"), 
                clusters = country_name_short)
app4_m8 <- lm_robust(np_share_nc_leg ~ r_year + crisis_election, 
                data = data %>% filter(region_type == "Once stable region"), 
                clusters = country_name_short)
app4_m9 <- lm_robust(np_share_nc_leg ~ r_year * region_type + crisis_election, 
                data = data, 
                clusters = country_name_short)
app4_m10 <- lm_robust(np_share_nc_leg ~ r_year + crisis_election * region_type, 
                 data = data, 
                 clusters = country_name_short)

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
    gof_omit = "Std.Errors",
    output = "figs/np_share_leg_tab2.html"
)

## modely s počty stran -----------------------------------
app5_m1a <- lm_robust(np_number_nc_1 ~ r_year, data = data, 
                      clusters = country_name_short)
app5_m1b <- lm_robust(np_number_nc_1 ~ r_year + crisis_election, 
                      data = data, clusters = country_name_short)

app5_m2a <- lm_robust(np_number_cv_1 ~ r_year, data = data, 
                      clusters = country_name_short)
app5_m2b <- lm_robust(np_number_cv_1 ~ r_year + crisis_election, 
                      data = data, clusters = country_name_short)

app5_m3a <- lm_robust(np_number_pnp_1 ~ r_year, data = data, 
                      clusters = country_name_short)
app5_m3b <- lm_robust(np_number_pnp_1 ~ r_year + crisis_election, 
                      data = data, clusters = country_name_short)

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
    output = "figs/np_number_tab1.html",
    gof_omit = "Std.Errors",
    stars = TRUE
)

app5_m7 <- lm_robust(np_number_nc_1 ~ r_year + crisis_election, 
                     data = data %>% filter(region_type == "Never stable region"), 
                     clusters = country_name_short)
app5_m8 <- lm_robust(np_number_nc_1 ~ r_year + crisis_election, 
                     data = data %>% filter(region_type == "Once stable region"), 
                     clusters = country_name_short)
app5_m9 <- lm_robust(np_number_nc_1 ~ r_year * region_type + crisis_election, 
                     data = data, 
                     clusters = country_name_short)
app5_m10 <- lm_robust(np_number_nc_1 ~ r_year + crisis_election * region_type, 
                      data = data, 
                      clusters = country_name_short)

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
    output = "figs/np_number_tab2.html",
    gof_omit = "Std.Errors",
    title = "Models of new party count"
)

# # Poisson regression
# app5_m1a_p <- glm(np_number_nc_1 ~ r_year, data = data, family = poisson)
# app5_m1b_p <- glm(np_number_nc_1 ~ r_year + crisis_election, 
#                   data = data, family = poisson)
# 
# app5_m2a_p <- glm(np_number_cv_1 ~ r_year, data = data, 
#                   family = poisson)
# app5_m2b_p <- glm(np_number_cv_1 ~ r_year + crisis_election, 
#                   data = data, family = poisson)
# 
# app5_m3a_p <- glm(np_number_pnp_1 ~ r_year, data = data, 
#                   family = poisson)
# app5_m3b_p <- glm(np_number_pnp_1 ~ r_year + crisis_election, 
#                   data = data, family = poisson)
# 
# modelsummary::modelsummary(
#     list("All new parties" = app5_m1a_p, 
#          "All new parties" = app5_m1b_p, 
#          "Genuinely new parties" = app5_m2a_p, 
#          "Genuinely new parties" = app5_m2b_p, 
#          "Partially new parties" = app5_m3a_p, 
#          "Partially new parties" = app5_m3b_p), 
#     coef_rename = c(
#         "r_year"="Year (0 = 1991)", 
#         "crisis_election"="Two elections after 2008"
#     ),
#     title = "Models with new party count",
#     # output = "figs/np_number_tab1.html",
#     gof_omit = "Std.Errors",
#     stars = TRUE
# )

app5b_m1a <- lm_robust(np_number_nc_leg ~ r_year, data = data, 
                      clusters = country_name_short)
app5b_m1b <- lm_robust(np_number_nc_leg ~ r_year + crisis_election, 
                      data = data, clusters = country_name_short)

app5b_m2a <- lm_robust(np_number_cv_leg ~ r_year, data = data, 
                      clusters = country_name_short)
app5b_m2b <- lm_robust(np_number_cv_leg ~ r_year + crisis_election, 
                      data = data, clusters = country_name_short)

app5b_m3a <- lm_robust(np_number_pnp_leg ~ r_year, data = data, 
                      clusters = country_name_short)
app5b_m3b <- lm_robust(np_number_pnp_leg ~ r_year + crisis_election, 
                      data = data, clusters = country_name_short)

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
    output = "figs/np_number_tab3.html",
    gof_omit = "Std.Errors",
    stars = TRUE
)

app5b_m7 <- lm_robust(np_number_nc_leg ~ r_year + crisis_election, 
                     data = data %>% filter(region_type == "Never stable region"), 
                     clusters = country_name_short)
app5b_m8 <- lm_robust(np_number_nc_leg ~ r_year + crisis_election, 
                     data = data %>% filter(region_type == "Once stable region"), 
                     clusters = country_name_short)
app5b_m9 <- lm_robust(np_number_nc_leg ~ r_year * region_type + crisis_election, 
                     data = data, 
                     clusters = country_name_short)
app5b_m10 <- lm_robust(np_number_nc_leg ~ r_year + crisis_election * region_type, 
                      data = data, 
                      clusters = country_name_short)

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
    output = "figs/np_number_tab4.html",
    gof_omit = "Std.Errors",
    title = "Models of new party count"
)

# TODO: připravit data

