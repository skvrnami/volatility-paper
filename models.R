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
    group_by(country_name) %>% 
    mutate(
        election_no = row_number()
    ) %>% 
    ungroup %>% 
    group_by(country_name, post_crisis) %>% 
    mutate(
        post_crisis_election = case_when(
            post_crisis == 0 ~ "Before crisis", 
            post_crisis == 1 & row_number() == 1 ~ "First post-crisis election", 
            post_crisis == 1 & row_number() == 2 ~ "Second post-crisis election", 
            post_crisis == 1 ~ "Other post-crisis election"
        ) %>% factor(., levels = c("Before crisis", "First post-crisis election", 
                                   "Second post-crisis election", 
                                   "Other post-crisis election")), 
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
        )
    )

cor(data$np_number_cv_1, data$np_share_cv_1, use = "complete.obs")
cor(data$np_number_nc_1, data$np_share_nc_1, use = "complete.obs")

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
         "Completely new parties" = m2a, 
         "Completely new parties" = m2b, 
         "Partially new parties" = m3a, 
         "Partially new parties" = m3b), 
    coef_rename = c(
        "r_year"="Year (0 = 1991)", 
        "crisis_election"="Two elections after 2008"
    ),
    stars = TRUE, 
    output = "tab1.html"
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
    output = "tab2.html"
)

## modely s pořadím voleb ---------------------------------
m1a_2 <- lm_robust(np_share_nc_1 ~ election_no, data = data, 
                 clusters = country_name_short)
m1b_2 <- lm_robust(np_share_nc_1 ~ election_no + crisis_election, 
                 data = data, clusters = country_name_short)

m2a_2 <- lm_robust(np_share_cv_1 ~ election_no, data = data, 
                 clusters = country_name_short)
m2b_2 <- lm_robust(np_share_cv_1 ~ election_no + crisis_election, 
                 data = data, clusters = country_name_short)

m3a_2 <- lm_robust(np_share_pnp_1 ~ election_no, data = data, 
                 clusters = country_name_short)
m3b_2 <- lm_robust(np_share_pnp_1 ~ election_no + crisis_election, 
                 data = data, clusters = country_name_short)

modelsummary::modelsummary(
    list("All new parties" = m1a_2, 
         "All new parties" = m1b_2, 
         "Completely new parties" = m2a_2, 
         "Completely new parties" = m2b_2, 
         "Partially new parties" = m3a_2, 
         "Partially new parties" = m3b_2), 
    coef_rename = c(
        "election_no"="Election number", 
        "crisis_election"="Two elections after 2008"
    ),
    stars = TRUE, 
    output = "tab1b.html"
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
         "Completely new parties" = app4_m2a, 
         "Completely new parties" = app4_m2b, 
         "Partially new parties" = app4_m3a, 
         "Partially new parties" = app4_m3b), 
    coef_rename = c(
        "r_year"="Year (0 = 1991)", 
        "crisis_election"="Two elections after 2008"
    ),
    title = "Models with new party share (legislative threshold)",
    output = "app4_tab1.html",
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
    output = "app4_tab2.html"
)

## modely s počty stran -----------------------------------
# TODO: OLS nebo count model?
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
         "Completely new parties" = app5_m2a, 
         "Completely new parties" = app5_m2b, 
         "Partially new parties" = app5_m3a, 
         "Partially new parties" = app5_m3b), 
    coef_rename = c(
        "r_year"="Year (0 = 1991)", 
        "crisis_election"="Two elections after 2008"
    ),
    title = "Models with new party count (legislative threshold)",
    # output = "app4_tab1.html",
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
    # output = "app4_tab2.html",
    title = "Models with new party share (legislative threshold)"
)


# TODO: připravit data

# m0 <- lm(np_share_cv_1 ~ election_no + post_crisis, data = data) 
# m1 <- lm(np_share_cv_1 ~ election_no + post_crisis * region_type, data = data) 
# m2 <- lm(np_share_cv_1 ~ election_no + post_crisis_election * region_type, data = data) 
# m3 <- lm(np_share_cv_1 ~ election_no + post_crisis_election2 * region_type, data = data) 
# 
# m4 <- lm(np_share_cv_1 ~ time_2008 + region_type, data = data) 
# 
# summary(m0)
# summary(m1)
# summary(m2)
# summary(m3)
# 
# ggeffect(m1, c("region_type", "post_crisis")) %>% plot()
# ggeffect(m2, c("region_type", "post_crisis_election")) %>% plot()
# ggeffect(m3, c("region_type", "post_crisis_election2")) %>% plot()
# 
# m0 <- lm(np_share_nc_1 ~ year + post_crisis, data = data) 
# m1 <- lm(np_share_nc_1 ~ year + post_crisis * region_type, data = data) 
# m2 <- lm(np_share_nc_1 ~ year + first_post_crisis * region_type + 
#              second_post_crisis * region_type, data = data) 
# 
# summary(m0)
# summary(m1)
# summary(m2)
# 
# ggeffect(m1, c("region_type", "post_crisis")) %>% plot()

