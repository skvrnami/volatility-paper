library(dplyr)
library(tidyr)
library(scales)
library(forcats)
library(ggplot2)
library(lemon)
library(patchwork)


elects_unique_cee <- readRDS("data/elects_unique_cee.rds")

np_share_hist_en <- 
    elects_unique_cee %>% 
    select(country_name_short, election_year, contains("np_share")) %>% 
    pivot_longer(cols = c(starts_with("np_"))) %>% 
    filter(!is.na(value)) %>%
    mutate(name = case_when(name == "np_share_cv_1" ~ "Genuinely new parties, 1%",
                            name == "np_share_pnp_1" ~ "Partially new parties, 1%",
                            name == "np_share_nc_1" ~ "All new parties, 1%",
                            name == "np_share_cv_leg" ~ "Genuinely new parties, threshold",
                            name == "np_share_pnp_leg" ~ "Partially new parties, threshold",
                            name == "np_share_nc_leg" ~ "All new parties, threshold")) 

chart3_data <- np_share_hist_en %>% 
    group_by(name) %>% 
    mutate(mean = mean(value)) %>% 
    ungroup() %>%
    mutate(name = fct_relevel(name, 
                              "All new parties, 1%",
                              "All new parties, threshold",
                              "Genuinely new parties, 1%",
                              "Genuinely new parties, threshold",
                              "Partially new parties, 1%",
                              "Partially new parties, threshold")) %>% 
    filter(grepl("1%", name))

# Graf 3
(np_share_hist_plot_en <-
    chart3_data %>% 
    ggplot(aes(x = value, group = name))+
    geom_histogram(bins = 12, breaks = seq(-0.05,0.5,0.05), color = "white")+
    geom_text(aes(x = mean + 0.06, y = 30, label = sprintf("%1.1f%%", mean*100)), 
              data = chart3_data %>% select(country_name_short, mean, name) %>% 
                  unique()) +
    geom_vline(aes(xintercept = mean), linetype = "dashed", alpha = 0.5)+
    scale_x_continuous(labels = label_percent())+
    scale_y_continuous(minor_breaks = seq(2,10,2), breaks = seq(0,40,10), 
                       limits = c(0, 35))+
    labs(x = "Electoral results of new parties",
         y = "" #, # y = "Počet výskytů v 83 volbách", 
         #caption = "Historgram je definován takovým způsobem, že první sloupec obsahuje jen volby s nulovým ziskem.\nVertikální přerušovaná čára představuje průměrný volební zisk nových stran.")+
    )+
    facet_wrap(~name, ncol = 1)+
    theme_bw()+
    theme(legend.position = "none"))

(ch3_all_parties <- chart3_data %>% 
    filter(name == "All new parties, 1%") %>% 
    ggplot(aes(x = value))+
    geom_histogram(bins = 12, breaks = seq(-0.05,0.5,0.05), color = "white")+
    geom_text(aes(x = 0.164 + 0.06, y = 25, label = sprintf("%1.1f%%", 16.4))) +
    geom_vline(aes(xintercept = mean), linetype = "dashed", alpha = 0.5)+
    scale_x_continuous(labels = label_percent())+
    scale_y_continuous(minor_breaks = seq(2,10,2), breaks = seq(0,40,10), 
                       limits = c(0, 35))+
    labs(x = "",
         y = "" #, # y = "Počet výskytů v 83 volbách", 
         #caption = "Historgram je definován takovým způsobem, že první sloupec obsahuje jen volby s nulovým ziskem.\nVertikální přerušovaná čára představuje průměrný volební zisk nových stran.")+
    )+
    facet_wrap(~name) + 
    theme_bw()+
    theme(legend.position = "none"))

(ch3_gn_parties <- chart3_data %>% 
        filter(name == "Genuinely new parties, 1%") %>% 
        ggplot(aes(x = value))+
        geom_histogram(bins = 12, breaks = seq(-0.05,0.5,0.05), color = "white")+
        geom_text(aes(x = 0.103 + 0.06, y = 25, label = sprintf("%1.1f%%", 10.3))) +
        geom_vline(aes(xintercept = mean), linetype = "dashed", alpha = 0.5)+
        scale_x_continuous(labels = label_percent())+
        scale_y_continuous(minor_breaks = seq(2,10,2), breaks = seq(0,40,10), 
                           limits = c(0, 35))+
        labs(x = "",
             y = "" #, # y = "Počet výskytů v 83 volbách", 
             #caption = "Historgram je definován takovým způsobem, že první sloupec obsahuje jen volby s nulovým ziskem.\nVertikální přerušovaná čára představuje průměrný volební zisk nových stran.")+
        )+
        facet_wrap(~name) + 
        theme_bw()+
        theme(legend.position = "none"))

(ch3_pnp_parties <- chart3_data %>% 
        filter(name == "Partially new parties, 1%") %>% 
        ggplot(aes(x = value))+
        geom_histogram(bins = 12, breaks = seq(-0.05,0.5,0.05), color = "white")+
        geom_text(aes(x = 0.06 + 0.06, y = 30, label = sprintf("%1.1f%%", 6))) +
        geom_vline(aes(xintercept = mean), linetype = "dashed", alpha = 0.5)+
        scale_x_continuous(labels = label_percent())+
        scale_y_continuous(minor_breaks = seq(2,10,2), breaks = seq(0,40,10), 
                           limits = c(0, 35))+
        labs(x = "Electoral results of new parties",
             y = "" #, # y = "Počet výskytů v 83 volbách", 
             #caption = "Historgram je definován takovým způsobem, že první sloupec obsahuje jen volby s nulovým ziskem.\nVertikální přerušovaná čára představuje průměrný volební zisk nových stran.")+
        )+
        facet_wrap(~name) + 
        theme_bw()+
        theme(legend.position = "none"))

new_parties <- 
    elects_unique_cee %>%
    select(country_name_short, election_date, election_year, starts_with("np_"))

new_parties <- 
    new_parties %>% 
    select(country_name_short, election_year,starts_with("np_share")) %>% 
    rename("Genuinely new parties, threshold" = np_share_cv_leg,
           "Genuinely new parties, 1%" = np_share_cv_1,
           "All new parties, threshold" = np_share_nc_leg,
           "All new parties, 1%" = np_share_nc_1,
           "Partially new parties, threshold" = np_share_pnp_leg,
           "Partially new parties, 1%" = np_share_pnp_1)

new_party_share <- new_parties %>% 
    pivot_longer(cols = -c(country_name_short, election_year)) %>% 
    mutate(name = fct_relevel(name, 
                              "All new parties, 1%",
                              "All new parties, threshold",
                              "Genuinely new parties, 1%",
                              "Genuinely new parties, threshold",
                              "Partially new parties, 1%",
                              "Partially new parties, threshold")) %>% 
    filter(!is.na(value)) %>% 
    filter(grepl("1%", name))

(np_share_facet <- 
        new_party_share %>% 
        ggplot(aes(x = election_year, y = value))+
        geom_point(alpha=0.5)+
        geom_smooth(se = FALSE, method = "loess", aes(group = 1), linetype = "solid", color = "goldenrod3")+
        scale_y_continuous(labels = percent_format(accuracy = 1, suffix = " %")) + 
        scale_x_continuous(breaks = seq(1990, 2020, 2), guide = guide_axis(angle = 90))+
        labs(x=element_blank(), 
             y=element_blank(),
             # caption = "Volební výsledky z databáze Parlgov.org, určení nových stran a výpočty autoři.\nJednotlivá pozorování vyjadřují podíl volebního zisku v jedněch volbách.\nČeské volby vyneseny červeným trojúhelníkem.\nTrendová křivka vytvořena algoritmem LOESS."
        ) +
        scale_color_manual(values = c("black", "firebrick2" ))+
        guides(color = "none") + 
        facet_rep_wrap(~name, repeat.tick.labels = 'all', ncol = 1)+
        theme_bw()+
        theme(panel.spacing = unit(1, "lines"),
              legend.position = "none"))

(share_all_chart <- new_party_share %>% 
        filter(name == "All new parties, 1%") %>% 
        ggplot(aes(x = election_year, y = value))+
        geom_point(alpha=0.5)+
        geom_smooth(se = FALSE, method = "loess", aes(group = 1), 
                    linetype = "solid", color = "goldenrod3")+
        scale_y_continuous(labels = percent_format(accuracy = 1, suffix = " %")) + 
        scale_x_continuous(breaks = seq(1990, 2020, 2), guide = guide_axis(angle = 90))+
        labs(x=element_blank(), 
             y=element_blank(),
             # caption = "Volební výsledky z databáze Parlgov.org, určení nových stran a výpočty autoři.\nJednotlivá pozorování vyjadřují podíl volebního zisku v jedněch volbách.\nČeské volby vyneseny červeným trojúhelníkem.\nTrendová křivka vytvořena algoritmem LOESS."
        ) +
        facet_wrap(~name) + 
        guides(color = "none") + 
        theme_bw())

(share_gn_chart <- new_party_share %>% 
        filter(name == "Genuinely new parties, 1%") %>% 
        ggplot(aes(x = election_year, y = value))+
        geom_point(alpha=0.5)+
        geom_smooth(se = FALSE, method = "loess", aes(group = 1), 
                    linetype = "solid", color = "goldenrod3")+
        scale_y_continuous(labels = percent_format(accuracy = 1, suffix = " %")) + 
        scale_x_continuous(breaks = seq(1990, 2020, 2), guide = guide_axis(angle = 90))+
        labs(x=element_blank(), 
             y=element_blank(),
             # caption = "Volební výsledky z databáze Parlgov.org, určení nových stran a výpočty autoři.\nJednotlivá pozorování vyjadřují podíl volebního zisku v jedněch volbách.\nČeské volby vyneseny červeným trojúhelníkem.\nTrendová křivka vytvořena algoritmem LOESS."
        ) +
        facet_wrap(~name) + 
        guides(color = "none") + 
        theme_bw())

(share_pnp_chart <- new_party_share %>% 
        filter(name == "Partially new parties, 1%") %>% 
        ggplot(aes(x = election_year, y = value))+
        geom_point(alpha=0.5)+
        geom_smooth(se = FALSE, method = "loess", aes(group = 1), 
                    linetype = "solid", color = "goldenrod3")+
        scale_y_continuous(labels = percent_format(accuracy = 1, suffix = " %")) + 
        scale_x_continuous(breaks = seq(1990, 2020, 2), guide = guide_axis(angle = 90))+
        labs(x=element_blank(), 
             y=element_blank(),
             # caption = "Volební výsledky z databáze Parlgov.org, určení nových stran a výpočty autoři.\nJednotlivá pozorování vyjadřují podíl volebního zisku v jedněch volbách.\nČeské volby vyneseny červeným trojúhelníkem.\nTrendová křivka vytvořena algoritmem LOESS."
        ) +
        facet_wrap(~name) + 
        guides(color = "none") + 
        theme_bw())

(ch3_all_parties + share_all_chart) /
(ch3_gn_parties + share_gn_chart) /
(ch3_pnp_parties + share_pnp_chart) +
    plot_annotation(tag_levels = list(c("A", "B")))

ggsave("figs/chart3.png",
       plot = last_plot(),
       width = 6,
       height = 7,
       units = "in",
       dpi = 300,
       type = "cairo")

# Appendix ------------------------------------------------
# Graf 3

chart3_data_app <- np_share_hist_en %>% 
    group_by(name) %>% 
    mutate(mean = mean(value)) %>% 
    ungroup() %>%
    mutate(name = fct_relevel(name, 
                              "All new parties, 1%",
                              "All new parties, threshold",
                              "Genuinely new parties, 1%",
                              "Genuinely new parties, threshold",
                              "Partially new parties, 1%",
                              "Partially new parties, threshold")) %>% 
    filter(grepl("threshold", name))

# Graf 3
(ch3_all_parties_app <- chart3_data_app %>% 
        filter(name == "All new parties, threshold") %>% 
        ggplot(aes(x = value))+
        geom_histogram(bins = 12, breaks = seq(-0.05,0.5,0.05), color = "white")+
        geom_text(aes(x = 0.148 + 0.06, y = 25, label = sprintf("%1.1f%%", 14.8))) +
        geom_vline(aes(xintercept = 0.148), linetype = "dashed", alpha = 0.5)+
        scale_x_continuous(labels = label_percent())+
        scale_y_continuous(minor_breaks = seq(2,10,2), breaks = seq(0,40,10), 
                           limits = c(0, 60))+
        labs(x = "",
             y = "" #, # y = "Počet výskytů v 83 volbách", 
             #caption = "Historgram je definován takovým způsobem, že první sloupec obsahuje jen volby s nulovým ziskem.\nVertikální přerušovaná čára představuje průměrný volební zisk nových stran.")+
        )+
        facet_wrap(~name) + 
        theme_bw()+
        theme(legend.position = "none"))

(ch3_gn_parties_app <- chart3_data_app %>% 
        filter(name == "Genuinely new parties, threshold") %>% 
        ggplot(aes(x = value))+
        geom_histogram(bins = 12, breaks = seq(-0.05,0.5,0.05), color = "white")+
        geom_text(aes(x = 0.106 + 0.06, y = 25, label = sprintf("%1.1f%%", 10.6))) +
        geom_vline(aes(xintercept = mean), linetype = "dashed", alpha = 0.5)+
        scale_x_continuous(labels = label_percent())+
        scale_y_continuous(minor_breaks = seq(2,10,2), breaks = seq(0,40,10), 
                           limits = c(0, 60))+
        labs(x = "",
             y = "" #, # y = "Počet výskytů v 83 volbách", 
             #caption = "Historgram je definován takovým způsobem, že první sloupec obsahuje jen volby s nulovým ziskem.\nVertikální přerušovaná čára představuje průměrný volební zisk nových stran.")+
        )+
        facet_wrap(~name) + 
        theme_bw()+
        theme(legend.position = "none"))

(ch3_pnp_parties_app <- chart3_data_app %>% 
        filter(name == "Partially new parties, threshold") %>% 
        ggplot(aes(x = value))+
        geom_histogram(bins = 12, breaks = seq(-0.05,0.5,0.05), color = "white")+
        geom_text(aes(x = 0.042 + 0.06, y = 30, label = sprintf("%1.1f%%", 4.2))) +
        geom_vline(aes(xintercept = mean), linetype = "dashed", alpha = 0.5)+
        scale_x_continuous(labels = label_percent())+
        scale_y_continuous(minor_breaks = seq(2,10,2), breaks = seq(0,40,10), 
                           limits = c(0, 60))+
        labs(x = "",
             y = "" #, # y = "Počet výskytů v 83 volbách", 
             #caption = "Historgram je definován takovým způsobem, že první sloupec obsahuje jen volby s nulovým ziskem.\nVertikální přerušovaná čára představuje průměrný volební zisk nových stran.")+
        )+
        facet_wrap(~name) + 
        theme_bw()+
        theme(legend.position = "none"))

new_party_share_app <- new_parties %>% 
    pivot_longer(cols = -c(country_name_short, election_year)) %>% 
    mutate(name = fct_relevel(name, 
                              "All new parties, 1%",
                              "All new parties, threshold",
                              "Genuinely new parties, 1%",
                              "Genuinely new parties, threshold",
                              "Partially new parties, 1%",
                              "Partially new parties, threshold")) %>% 
    filter(!is.na(value)) %>% 
    filter(grepl("threshold", name))

(share_all_chart_app <- new_party_share_app %>% 
        filter(name == "All new parties, threshold") %>% 
        ggplot(aes(x = election_year, y = value))+
        geom_point(alpha=0.5)+
        geom_smooth(se = FALSE, method = "loess", aes(group = 1), 
                    linetype = "solid", color = "goldenrod3")+
        scale_y_continuous(labels = percent_format(accuracy = 1, suffix = " %")) + 
        scale_x_continuous(breaks = seq(1990, 2020, 2), guide = guide_axis(angle = 90))+
        labs(x=element_blank(), 
             y=element_blank(),
             # caption = "Volební výsledky z databáze Parlgov.org, určení nových stran a výpočty autoři.\nJednotlivá pozorování vyjadřují podíl volebního zisku v jedněch volbách.\nČeské volby vyneseny červeným trojúhelníkem.\nTrendová křivka vytvořena algoritmem LOESS."
        ) +
        facet_wrap(~name) + 
        guides(color = "none") + 
        theme_bw())

(share_gn_chart_app <- new_party_share_app %>% 
        filter(name == "Genuinely new parties, threshold") %>% 
        ggplot(aes(x = election_year, y = value))+
        geom_point(alpha=0.5)+
        geom_smooth(se = FALSE, method = "loess", aes(group = 1), 
                    linetype = "solid", color = "goldenrod3")+
        scale_y_continuous(labels = percent_format(accuracy = 1, suffix = " %")) + 
        scale_x_continuous(breaks = seq(1990, 2020, 2), guide = guide_axis(angle = 90))+
        labs(x=element_blank(), 
             y=element_blank(),
             # caption = "Volební výsledky z databáze Parlgov.org, určení nových stran a výpočty autoři.\nJednotlivá pozorování vyjadřují podíl volebního zisku v jedněch volbách.\nČeské volby vyneseny červeným trojúhelníkem.\nTrendová křivka vytvořena algoritmem LOESS."
        ) +
        facet_wrap(~name) + 
        guides(color = "none") + 
        theme_bw())

(share_pnp_chart_app <- new_party_share_app %>% 
        filter(name == "Partially new parties, threshold") %>% 
        ggplot(aes(x = election_year, y = value))+
        geom_point(alpha=0.5)+
        geom_smooth(se = FALSE, method = "loess", aes(group = 1), 
                    linetype = "solid", color = "goldenrod3")+
        scale_y_continuous(labels = percent_format(accuracy = 1, suffix = " %")) + 
        scale_x_continuous(breaks = seq(1990, 2020, 2), guide = guide_axis(angle = 90))+
        labs(x=element_blank(), 
             y=element_blank(),
             # caption = "Volební výsledky z databáze Parlgov.org, určení nových stran a výpočty autoři.\nJednotlivá pozorování vyjadřují podíl volebního zisku v jedněch volbách.\nČeské volby vyneseny červeným trojúhelníkem.\nTrendová křivka vytvořena algoritmem LOESS."
        ) +
        facet_wrap(~name) + 
        guides(color = "none") + 
        theme_bw())

(ch3_all_parties_app + share_all_chart_app) /
    (ch3_gn_parties_app + share_gn_chart_app) /
    (ch3_pnp_parties_app + share_pnp_chart_app) +
    plot_annotation(tag_levels = list(c("A", "B")))

ggsave("figs/chart3_appendix.png",
       plot = last_plot(),
       width = 6,
       height = 7,
       units = "in",
       dpi = 300,
       type = "cairo")

# Bar chart (average over generations) ---------------------
bar_chart_data <- elects_unique_cee %>% 
    mutate(
        region_type = case_when(
            country_name %in% c("Croatia", "Czech Republic", "Hungary", 
                                "Romania", "Slovenia") ~ "Once stable region", 
            country_name %in% c("Estonia", "Lithuania", "Latvia", "Poland", 
                                "Bulgaria", "Slovakia") ~ "Never stable region"
        ),
        generation = case_when(
            country_name_short == "BGR" & election_year %in% c(1994, 1997) ~ "2nd generation",
            country_name_short == "BGR" & election_year %in% c(2001, 2005) ~ "3rd generation",
            country_name_short == "CZE" & election_year %in% c(1996, 1998) ~ "2nd generation",
            country_name_short == "CZE" & election_year %in% c(2002, 2006) ~ "3rd generation",
            country_name_short == "CZE" & election_year %in% c(2010) ~ "4th generation",
            country_name_short == "EST" & election_year %in% c(1995) ~ "2nd generation",
            country_name_short == "EST" & election_year %in% c(1999, 2003) ~ "3rd generation",
            country_name_short == "HRV" & election_year %in% c(2003, 2007, 2011) ~ "3rd generation",
            country_name_short == "HRV" & election_year %in% c(2015) ~ "4th generation",
            country_name_short == "HUN" & election_year %in% c(1994) ~ "2nd generation",
            country_name_short == "HUN" & election_year %in% c(1998, 2002, 2006) ~ "3rd generation",
            country_name_short == "HUN" & election_year %in% c(2010) ~ "4th generation",
            country_name_short == "LTU" & election_year %in% c(1996) ~ "2nd generation",
            country_name_short == "LTU" & election_year %in% c(2000, 2004) ~ "3rd generation",
            country_name_short == "LVA" & election_year %in% c(1995) ~ "2nd generation",
            country_name_short == "LVA" & election_year %in% c(1998, 2002) ~ "3rd generation",
            country_name_short == "POL" & election_year %in% c(1993) ~ "2nd generation",
            country_name_short == "POL" & election_year %in% c(1997, 2001) ~ "3rd generation",
            country_name_short == "ROU" & election_year %in% c(1996) ~ "2nd generation",
            country_name_short == "ROU" & election_year %in% c(2000, 2004, 2008) ~ "3rd generation",
            country_name_short == "ROU" & election_year %in% c(2012) ~ "4th generation",
            country_name_short == "SVK" & election_year %in% c(1994, 1998) ~ "2nd generation",
            country_name_short == "SVK" & election_year %in% c(2002, 2006) ~ "3rd generation",
            country_name_short == "SVN" & election_year %in% c(1996, 2000, 2004) ~ "2nd generation",
            country_name_short == "SVN" & election_year %in% c(2008) ~ "3rd generation",
            country_name_short == "SVN" & election_year %in% c(2011) ~ "4th generation"
        )
    ) %>% 
    group_by(region_type, generation) %>%
    summarise(
        across(any_of(c("np_share_nc_1", "np_share_cv_1", "reward_nc_pm_long")), 
               ~mean(.x, na.rm = TRUE))
    ) %>% 
    mutate(
        np_share_nc_1 = np_share_nc_1 * 100,
        np_share_cv_1 = np_share_cv_1 * 100,
    ) %>% 
    tidyr::pivot_longer(., cols = 3:5) %>% 
    mutate(name = case_when(
        name == "np_share_nc_1" ~ "New parties share (%)", 
        name == "np_share_cv_1" ~ "Genuinely new parties vote share (%)",
        name == "reward_nc_pm_long" ~ "PM's party change (percentage points)"
    ))

bar1 <- bar_chart_data %>% 
    filter(!is.na(generation)) %>% 
    mutate(region_type = factor(region_type, levels = c("Once stable region", "Never stable region"))) %>% 
    ggplot(., aes(x = generation, y = value, fill = name)) + 
    geom_bar(stat = "identity", position = "dodge2") + 
    facet_wrap(~region_type + generation, scales = "free_x") + 
    scale_fill_viridis_d(option = "D", direction = -1) + 
    theme_bw() + 
    geom_hline(yintercept = 0, linewidth = 0.2) +
    theme(legend.position = "top", panel.grid.major.x = element_blank(), 
          axis.text.x = element_blank(), 
          axis.ticks.x = element_blank()) + 
    labs(x = "", y = "", fill = "")

bar1

ggsave("figs/bar_generations.png", 
       plot = bar1,
       width = 7,
       height = 6,
       units = "in",
       dpi = 300,
       type = "cairo")

# bar_chart_data %>% 
#     filter(!is.na(generation)) %>% 
#     mutate(region_type = factor(region_type, levels = c("Once stable region", "Never stable region"))) %>% 
#     ggplot(., aes(x = generation, y = value, fill = name)) + 
#     geom_bar(stat = "identity", position = "dodge2") + 
#     facet_grid(region_type ~ ., scales = "free_x") + 
#     theme_bw() + 
#     theme(legend.position = "top", panel.grid.major.x = element_blank()) + 
#     labs(x = "", y = "", fill = "")    

bar_chart_data2 <- elects_unique_cee %>% 
    mutate(post_crisis = as.numeric(election_year > 2008)) %>% 
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
        region_type = case_when(
            country_name %in% c("Croatia", "Czech Republic", "Hungary", 
                                "Romania", "Slovenia") ~ "Once stable region", 
            country_name %in% c("Estonia", "Lithuania", "Latvia", "Poland", 
                                "Bulgaria", "Slovakia") ~ "Never stable region"
        ),
        generation = case_when(
            country_name_short == "BGR" & election_year %in% c(1994, 1997) ~ "2nd generation",
            country_name_short == "BGR" & election_year %in% c(2001, 2005) ~ "3rd generation",
            country_name_short == "CZE" & election_year %in% c(1996, 1998) ~ "2nd generation",
            country_name_short == "CZE" & election_year %in% c(2002, 2006) ~ "3rd generation",
            country_name_short == "CZE" & election_year %in% c(2010) ~ "4th generation",
            country_name_short == "EST" & election_year %in% c(1995) ~ "2nd generation",
            country_name_short == "EST" & election_year %in% c(1999, 2003) ~ "3rd generation",
            country_name_short == "HRV" & election_year %in% c(2003, 2007, 2011) ~ "3rd generation",
            country_name_short == "HRV" & election_year %in% c(2015) ~ "4th generation",
            country_name_short == "HUN" & election_year %in% c(1994) ~ "2nd generation",
            country_name_short == "HUN" & election_year %in% c(1998, 2002, 2006) ~ "3rd generation",
            country_name_short == "HUN" & election_year %in% c(2010) ~ "4th generation",
            country_name_short == "LTU" & election_year %in% c(1996) ~ "2nd generation",
            country_name_short == "LTU" & election_year %in% c(2000, 2004) ~ "3rd generation",
            country_name_short == "LVA" & election_year %in% c(1995) ~ "2nd generation",
            country_name_short == "LVA" & election_year %in% c(1998, 2002) ~ "3rd generation",
            country_name_short == "POL" & election_year %in% c(1993) ~ "2nd generation",
            country_name_short == "POL" & election_year %in% c(1997, 2001) ~ "3rd generation",
            country_name_short == "ROU" & election_year %in% c(1996) ~ "2nd generation",
            country_name_short == "ROU" & election_year %in% c(2000, 2004, 2008) ~ "3rd generation",
            country_name_short == "ROU" & election_year %in% c(2012) ~ "4th generation",
            country_name_short == "SVK" & election_year %in% c(1994, 1998) ~ "2nd generation",
            country_name_short == "SVK" & election_year %in% c(2002, 2006) ~ "3rd generation",
            country_name_short == "SVN" & election_year %in% c(1996, 2000, 2004) ~ "2nd generation",
            country_name_short == "SVN" & election_year %in% c(2008) ~ "3rd generation",
            country_name_short == "SVN" & election_year %in% c(2011) ~ "4th generation", 
            post_crisis_election2 == "First/Second post-crisis election" ~ "4th generation"
        )
    ) %>% 
    select(region_type, generation, np_share_nc_1, np_share_cv_1,
           reward_nc_pm_long, everything()) %>%
    View
    group_by(region_type, generation) %>%
    summarise(
        across(any_of(c("np_share_nc_1", "np_share_cv_1", "reward_nc_pm_long")), 
               ~mean(.x, na.rm = TRUE))
    ) %>% 
    mutate(
        np_share_nc_1 = np_share_nc_1 * 100,
        np_share_cv_1 = np_share_cv_1 * 100,
    ) %>% 
    tidyr::pivot_longer(., cols = 3:5) %>% 
    mutate(name = case_when(
        name == "np_share_nc_1" ~ "New parties share (%)", 
        name == "np_share_cv_1" ~ "Genuinely new parties vote share (%)",
        name == "reward_nc_pm_long" ~ "PM's party change (percentage points)"
    ))

bar_chart_data2 %>% 
    filter(!is.na(generation)) %>% 
    mutate(region_type = factor(region_type, levels = c("Once stable region", "Never stable region"))) %>% 
    ggplot(., aes(x = generation, y = value, fill = name)) + 
    geom_bar(stat = "identity", position = "dodge2") + 
    facet_wrap(~region_type + generation, scales = "free_x") + 
    scale_fill_viridis_d(option = "D", direction = -1) + 
    theme_bw() + 
    geom_hline(yintercept = 0, linewidth = 0.2) +
    theme(legend.position = "top", panel.grid.major.x = element_blank(), 
          axis.text.x = element_blank(), 
          axis.ticks.x = element_blank()) + 
    labs(x = "", y = "", fill = "")


chart_data2 <- elects_unique_cee %>% 
    mutate(post_crisis = as.numeric(election_year > 2008)) %>% 
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
        region_type = case_when(
            country_name %in% c("Croatia", "Czech Republic", "Hungary", 
                                "Romania", "Slovenia") ~ "Once stable region", 
            country_name %in% c("Estonia", "Lithuania", "Latvia", "Poland", 
                                "Bulgaria", "Slovakia") ~ "Never stable region"
        ),
        generation = case_when(
            country_name_short == "BGR" & election_year %in% c(1994, 1997) ~ "2nd generation",
            country_name_short == "BGR" & election_year %in% c(2001, 2005) ~ "3rd generation",
            country_name_short == "CZE" & election_year %in% c(1996, 1998) ~ "2nd generation",
            country_name_short == "CZE" & election_year %in% c(2002, 2006) ~ "3rd generation",
            # country_name_short == "CZE" & election_year %in% c(2010) ~ "4th generation",
            country_name_short == "EST" & election_year %in% c(1995) ~ "2nd generation",
            country_name_short == "EST" & election_year %in% c(1999, 2003) ~ "3rd generation",
            country_name_short == "HRV" & election_year %in% c(2003, 2007, 2011) ~ "3rd generation",
            # country_name_short == "HRV" & election_year %in% c(2015) ~ "4th generation",
            country_name_short == "HUN" & election_year %in% c(1994) ~ "2nd generation",
            country_name_short == "HUN" & election_year %in% c(1998, 2002, 2006) ~ "3rd generation",
            # country_name_short == "HUN" & election_year %in% c(2010) ~ "4th generation",
            country_name_short == "LTU" & election_year %in% c(1996) ~ "2nd generation",
            country_name_short == "LTU" & election_year %in% c(2000, 2004) ~ "3rd generation",
            country_name_short == "LVA" & election_year %in% c(1995) ~ "2nd generation",
            country_name_short == "LVA" & election_year %in% c(1998, 2002) ~ "3rd generation",
            country_name_short == "POL" & election_year %in% c(1993) ~ "2nd generation",
            country_name_short == "POL" & election_year %in% c(1997, 2001) ~ "3rd generation",
            country_name_short == "ROU" & election_year %in% c(1996) ~ "2nd generation",
            country_name_short == "ROU" & election_year %in% c(2000, 2004, 2008) ~ "3rd generation",
            # country_name_short == "ROU" & election_year %in% c(2012) ~ "4th generation",
            country_name_short == "SVK" & election_year %in% c(1994, 1998) ~ "2nd generation",
            country_name_short == "SVK" & election_year %in% c(2002, 2006) ~ "3rd generation",
            country_name_short == "SVN" & election_year %in% c(1996, 2000, 2004) ~ "2nd generation",
            country_name_short == "SVN" & election_year %in% c(2008) ~ "3rd generation",
            # country_name_short == "SVN" & election_year %in% c(2011) ~ "4th generation", 
            post_crisis_election2 == "First/Second post-crisis election" ~ "4th generation (2 post-crisis el.)",
            post_crisis_election2 == "Other post-crisis election" ~ "Other post-crisis"
        )
    ) %>% 
    mutate(
        np_share_nc_1 = np_share_nc_1 * 100,
        np_share_cv_1 = np_share_cv_1 * 100,
    )

chart_data2 %>% 
    select(election_year, region_type, generation, np_share_nc_1, np_share_cv_1,
           reward_nc_pm_long) %>% 
    tidyr::pivot_longer(., cols = 4:6) %>% 
    mutate(name = case_when(
        name == "np_share_nc_1" ~ "New parties share (%)", 
        name == "np_share_cv_1" ~ "Genuinely new parties vote share (%)",
        name == "reward_nc_pm_long" ~ "PM's party change (percentage points)"
    )) %>% 
    filter(!is.na(generation)) %>% 
    mutate(region_type = factor(region_type, levels = c("Once stable region", "Never stable region"))) %>% 
    ggplot(., aes(x = name, y = value, colour = name)) + 
    geom_boxplot(outlier.shape = NA) + 
    geom_jitter(width = 0.1) + 
    facet_wrap(~region_type + generation, scales = "free_x", 
               nrow = 2) + 
    theme_bw() + 
    geom_hline(yintercept = 0, linewidth = 0.2) +
    theme(legend.position = "top", panel.grid.major.x = element_blank(), 
          axis.text.x = element_blank(), 
          axis.ticks.x = element_blank()) + 
    labs(x = "", y = "", colour = "")

ggsave("figs/generations5.png", 
       width = 9,
       height = 6,
       units = "in",
       dpi = 300,
       type = "cairo")

