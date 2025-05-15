library(dplyr)
library(tidyr)
library(scales)
library(forcats)
library(ggplot2)
library(lemon)
library(patchwork)

final_data <- readRDS("data/data_final.rds")

np_share_hist_en <- 
    final_data %>% 
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
    geom_histogram(bins = 12, breaks = seq(-5,50,5), color = "white")+
    geom_text(aes(x = mean + 0.06, y = 30, label = sprintf("%1.1f%%", mean)), 
              data = chart3_data %>% select(country_name_short, mean, name) %>% 
                  unique()) +
    geom_vline(aes(xintercept = mean), linetype = "dashed", alpha = 0.5)+
    scale_x_continuous(labels = label_percent(scale = 1))+
    scale_y_continuous(minor_breaks = seq(2,10,2), breaks = seq(0,40,10), 
                       limits = c(0, 40))+
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
    geom_histogram(bins = 12, breaks = seq(-5,50,5), color = "white") +
    geom_text(aes(x = 15.4 + 4, y = 25, label = sprintf("%1.1f%%", 15.4))) +
    geom_vline(aes(xintercept = mean), linetype = "dashed", alpha = 0.5)+
    scale_x_continuous(labels = label_percent(scale = 1))+
    scale_y_continuous(minor_breaks = seq(2,10,2), breaks = seq(0,40,10), 
                       limits = c(0, 40))+
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
        geom_histogram(bins = 12, breaks = seq(-5,50,5), color = "white")+
        geom_text(aes(x = 10.1 + 4, y = 25, label = sprintf("%1.1f%%", 10.1))) +
        geom_vline(aes(xintercept = mean), linetype = "dashed", alpha = 0.5)+
        scale_x_continuous(labels = label_percent(scale = 1))+
        scale_y_continuous(minor_breaks = seq(2,10,2), breaks = seq(0,40,10), 
                           limits = c(0, 40))+
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
        geom_histogram(bins = 12, breaks = seq(-5,50,5), color = "white") +
        geom_text(aes(x = 5.3 + 3, y = 30, label = sprintf("%1.1f%%", 5.3))) +
        geom_vline(aes(xintercept = mean), linetype = "dashed", alpha = 0.5)+
        scale_x_continuous(labels = label_percent(scale = 1))+
        scale_y_continuous(breaks = seq(0, 40, 10), minor_breaks = seq(2, 10, 2), 
                           limits = c(0, 40)) + 
        labs(x = "Electoral results of new parties",
             y = "" #, # y = "Počet výskytů v 83 volbách", 
             #caption = "Historgram je definován takovým způsobem, že první sloupec obsahuje jen volby s nulovým ziskem.\nVertikální přerušovaná čára představuje průměrný volební zisk nových stran.")+
        )+
        facet_wrap(~name) + 
        theme_bw()+
        theme(legend.position = "none"))

new_parties <- 
    final_data %>%
    select(country_name_short, election_year, starts_with("np_"))

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
        scale_y_continuous(labels = percent_format(accuracy = 1, suffix = " %", scale = 1)) + 
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
        scale_y_continuous(labels = percent_format(accuracy = 1, suffix = " %", 
                                                   scale = 1)) + 
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
        scale_y_continuous(labels = percent_format(accuracy = 1, suffix = " %", 
                                                   scale = 1)) + 
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
        scale_y_continuous(labels = percent_format(accuracy = 1, suffix = " %", 
                                                   scale = 1)) + 
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

ggsave("figs/fig1.png",
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
        geom_histogram(bins = 12, breaks = seq(-5,50,5), color = "white")+
        geom_text(aes(x = 14.5 + 4, y = 25, label = sprintf("%1.1f%%", 14.5))) +
        geom_vline(aes(xintercept = 14.5), linetype = "dashed", alpha = 0.5)+
        scale_x_continuous(labels = label_percent(scale = 1))+
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
        geom_histogram(bins = 12, breaks = seq(-5,50,5), color = "white")+
        geom_text(aes(x = 10.3 + 4, y = 25, label = sprintf("%1.1f%%", 10.3))) +
        geom_vline(aes(xintercept = mean), linetype = "dashed", alpha = 0.5)+
        scale_x_continuous(labels = label_percent(scale = 1))+
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
        geom_histogram(bins = 12, breaks = seq(-5,50,5), color = "white")+
        geom_text(aes(x = 4.1 + 4, y = 30, label = sprintf("%1.1f%%", 4.1))) +
        geom_vline(aes(xintercept = mean), linetype = "dashed", alpha = 0.5)+
        scale_x_continuous(labels = label_percent(scale = 1))+
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
        scale_y_continuous(labels = percent_format(scale= 1, suffix = "%")) + 
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
        scale_y_continuous(labels = percent_format(scale = 1, suffix = "%")) + 
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
        scale_y_continuous(labels = percent_format(scale = 1, suffix = "%")) + 
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

ggsave("figs/fig_a1.png",
       plot = last_plot(),
       width = 6,
       height = 7,
       units = "in",
       dpi = 300,
       type = "cairo")

# Bar chart (average over generations) ---------------------
bar_chart_data <- final_data %>% 
    mutate(
        region_type = case_when(
            country_name_short %in% c("HRV", "CZE", "HUN", 
                                "ROU", "SVN") ~ "Formerly stable region", 
            country_name_short %in% c("EST", "LTU", "LVA", "POL", 
                                "BGR", "SVK") ~ "Formerly unstable region"
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
    tidyr::pivot_longer(., cols = 3:5) %>% 
    mutate(name = case_when(
        name == "np_share_nc_1" ~ "New parties share (%)", 
        name == "np_share_cv_1" ~ "Genuinely new parties vote share (%)",
        name == "reward_nc_pm_long" ~ "PM's party change (percentage points)"
    ))

bar1 <- bar_chart_data %>% 
    filter(!is.na(generation)) %>% 
    mutate(region_type = factor(region_type, levels = c("Formerly stable region", "Formerly unstable region"))) %>% 
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

bar_chart_data2 <- final_data %>% 
    mutate(post_crisis = as.numeric(election_year > 2008)) %>% 
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
        region_type = case_when(
            country_name_short %in% c("HRV", "CZE", "HUN", 
                                      "ROU", "SVN") ~ "Formerly stable region", 
            country_name_short %in% c("EST", "LTU", "LVA", "POL", 
                                      "BGR", "SVK") ~ "Formerly unstable region"
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
    group_by(region_type, generation) %>%
    summarise(
        across(any_of(c("np_share_nc_1", "np_share_cv_1", "reward_nc_pm_long")), 
               ~mean(.x, na.rm = TRUE))
    ) %>% 
    tidyr::pivot_longer(., cols = 3:5) %>% 
    mutate(name = case_when(
        name == "np_share_nc_1" ~ "New parties share (%)", 
        name == "np_share_cv_1" ~ "Genuinely new parties vote share (%)",
        name == "reward_nc_pm_long" ~ "PM's party change (percentage points)"
    ))

bar_chart_data2 %>% 
    filter(!is.na(generation)) %>% 
    mutate(region_type = factor(region_type, levels = c("Formerly stable region", "Formerly unstable region"))) %>% 
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


chart_data2 <- final_data %>% 
    mutate(post_crisis = as.numeric(election_year > 2008)) %>% 
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
        region_type = case_when(
            country_name_short %in% c("HRV", "CZE", "HUN", 
                                      "ROU", "SVN") ~ "Formerly stable region", 
            country_name_short %in% c("EST", "LTU", "LVA", "POL", 
                                      "BGR", "SVK") ~ "Formely unstable region"
        ),
        generation = case_when(
            country_name_short == "BGR" & election_year %in% c(1991, 1994, 1997) ~ "2nd generation",
            country_name_short == "BGR" & election_year %in% c(2001, 2005) ~ "3rd generation",
            country_name_short == "CZE" & election_year %in% c(1992, 1996, 1998) ~ "2nd generation",
            country_name_short == "CZE" & election_year %in% c(2002, 2006) ~ "3rd generation",
            # country_name_short == "CZE" & election_year %in% c(2010) ~ "4th generation",
            country_name_short == "EST" & election_year %in% c(1992, 1995) ~ "2nd generation",
            country_name_short == "EST" & election_year %in% c(1999, 2003, 2007) ~ "3rd generation",
            country_name_short == "HRV" & election_year %in% c(2000) ~ "2nd generation",
            country_name_short == "HRV" & election_year %in% c(2003, 2007) ~ "3rd generation",
            # country_name_short == "HRV" & election_year %in% c(2015) ~ "4th generation",
            country_name_short == "HUN" & election_year %in% c(1994) ~ "2nd generation",
            country_name_short == "HUN" & election_year %in% c(1998, 2002, 2006) ~ "3rd generation",
            # country_name_short == "HUN" & election_year %in% c(2010) ~ "4th generation",
            country_name_short == "LTU" & election_year %in% c(1992, 1996) ~ "2nd generation",
            country_name_short == "LTU" & election_year %in% c(2000, 2004, 2008) ~ "3rd generation",
            country_name_short == "LVA" & election_year %in% c(1993, 1995) ~ "2nd generation",
            country_name_short == "LVA" & election_year %in% c(1998, 2002, 2006) ~ "3rd generation",
            country_name_short == "POL" & election_year %in% c(1993) ~ "2nd generation",
            country_name_short == "POL" & election_year %in% c(1997, 2001, 2005, 2007) ~ "3rd generation",
            country_name_short == "ROU" & election_year %in% c(1992, 1996) ~ "2nd generation",
            country_name_short == "ROU" & election_year %in% c(2000, 2004, 2008) ~ "3rd generation",
            # country_name_short == "ROU" & election_year %in% c(2012) ~ "4th generation",
            country_name_short == "SVK" & election_year %in% c(1992, 1994, 1998) ~ "2nd generation",
            country_name_short == "SVK" & election_year %in% c(2002, 2006) ~ "3rd generation",
            country_name_short == "SVN" & election_year %in% c(1992, 1996, 2000, 2004) ~ "2nd generation",
            country_name_short == "SVN" & election_year %in% c(2008) ~ "3rd generation",
            # country_name_short == "SVN" & election_year %in% c(2011) ~ "4th generation", 
            post_crisis_election2 == "First/Second post-crisis election" ~ "4th generation (2 post-crisis el.)",
            post_crisis_election2 == "Other post-crisis election" ~ "Other post-crisis"
        )
    )

# generations_check <- chart_data2 %>% 
#     select(country_year, generation) 
# 
# medians <- chart_data2 %>% 
#     group_by(generation, region_type) %>% 
#     summarise(across(c(np_share_nc_1, np_share_cv_1, 
#                        reward_nc_pm_long, reward_nc_cab_long), 
#                      ~median(.x, na.rm = TRUE))) %>% 
#     filter(!is.na(generation))
# 
# openxlsx::write.xlsx(
#     list(
#         "generace" = generations_check, 
#         "medians" = medians
#     ), 
#     "kontrola.xlsx"
# )

chart_data2 %>% 
    select(election_year, region_type, generation, np_share_nc_1, np_share_cv_1,
           reward_nc_pm_long, reward_nc_cab_long) %>% 
    tidyr::pivot_longer(., cols = 4:7) %>% 
    mutate(name = case_when(
        name == "np_share_nc_1" ~ "New parties share (%)", 
        name == "np_share_cv_1" ~ "Genuinely new parties vote share (%)",
        name == "reward_nc_pm_long" ~ "PM's party change (percentage points)",
        name == "reward_nc_cab_long" ~ "Government parties change (percentage points)"
    )) %>% 
    filter(!is.na(generation)) %>% 
    mutate(region_type = factor(region_type, levels = c("Formerly stable region", "Formely unstable region")), 
           name = factor(name, levels = c("New parties share (%)", "Genuinely new parties vote share (%)",
                                          "PM's party change (percentage points)",
                                          "Government parties change (percentage points)"))) %>% 
    ggplot(., aes(x = name, y = value, colour = name)) + 
    geom_boxplot(outlier.shape = NA) + 
    geom_jitter(width = 0.1) + 
    facet_wrap(~region_type + generation, scales = "free_x", 
               nrow = 2) + 
    scale_colour_viridis_d() + 
    theme_bw() + 
    geom_hline(yintercept = 0, linewidth = 0.2) +
    guides(colour = guide_legend(nrow = 2, byrow = TRUE)) + 
    theme(legend.position = "top", 
          panel.grid.major.x = element_blank(), 
          axis.text.x = element_blank(), 
          axis.ticks.x = element_blank()) + 
    labs(x = "", y = "", colour = "")

ggsave("figs/generations5_4vars.png", 
       width = 9,
       height = 6,
       units = "in",
       dpi = 300,
       type = "cairo")

new_parties <- 
    final_data %>%
    select(country_name_short, election_year, starts_with("np_"))

new_parties_eng <- 
    new_parties %>% 
    select(country_name_short, election_year, starts_with("np_share")) %>% 
    rename("Genuinely new parties, threshold" = np_share_cv_leg,
           "Genuinely new parties, 1%" = np_share_cv_1,
           "All new parties, threshold" = np_share_nc_leg,
           "All new parties, 1%" = np_share_nc_1,
           "Partially new parties, threshold" = np_share_pnp_leg,
           "Partially new parties, 1%" = np_share_pnp_1) %>% 
    mutate(region = case_when(
        country_name_short %in% c("POL", "EST", "LVA", "SVK", "LTU", "BGR") ~ "'Formerly unstable' region (Baltic states, Bulgaria, Poland, Slovakia)", 
        country_name_short %in% c("SVN", "CZE", "HRV", "ROU", "HUN") ~ "'Formerly stable' region (Croatia, Czechia, Hungary, Romania, Slovenia)"
    ))

new_parties_eng %>% 
    # mutate(is_czech = if_else(country_name_short == "CZE", 1,0)) %>% 
    pivot_longer(cols = -c(country_name_short, election_year, region)) %>% 
    mutate(name = fct_relevel(name, 
                              "All new parties, 1%",
                              "All new parties, threshold",
                              "Genuinely new parties, 1%",
                              "Genuinely new parties, threshold",
                              "Partially new parties, 1%",
                              "Partially new parties, threshold")) %>% 
    filter(grepl("1%", name)) %>% 
    filter(!is.na(value)) %>% 
    ggplot(aes(x = election_year, y = value))+
    geom_point(alpha=0.5, aes(colour = region))+
    geom_smooth(se = FALSE, method = "loess", aes(colour = region), linetype = "solid")+
    scale_y_continuous(labels = percent_format(accuracy = 1, scale = 1)) + 
    scale_x_continuous(breaks = seq(1990, 2020, 2), guide = guide_axis(angle = 90))+
    scale_colour_viridis_d(option = "D") + 
    # scale_colour_manual(values = c("#f1a340", "#998ec3")) +
    labs(x=element_blank(), 
         y=element_blank(),
         colour = "") + 
    facet_rep_wrap(~name, repeat.tick.labels = 'all', ncol = 3)+
    theme_bw() +
    theme(panel.spacing = unit(1, "lines"),
          legend.position = "top") + 
    guides(colour = guide_legend(nrow=2,byrow=TRUE))


ggsave("figs/regions.png", width = 8, height = 4)


# Generations - all 2nd generation election

chart_data2b <- final_data %>% 
    mutate(post_crisis = as.numeric(election_year > 2008)) %>% 
    group_by(country_name_short) %>% 
    mutate(election_no = row_number()) %>% 
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
        region_type = case_when(
            country_name_short %in% c("HRV", "CZE", "HUN", 
                                      "ROU", "SVN") ~ "Formerly stable region", 
            country_name_short %in% c("EST", "LTU", "LVA", "POL", 
                                      "BGR", "SVK") ~ "Formely unstable region"
        ),
        generation = case_when(
            country_name_short == "BGR" & election_no > 1 & election_year <= 1997 ~ "2nd generation",
            country_name_short == "BGR" & election_year %in% c(2001, 2005) ~ "3rd generation",
            country_name_short == "CZE" & election_no > 1 & election_year <= 1998 ~ "2nd generation",
            country_name_short == "CZE" & election_year %in% c(2002, 2006) ~ "3rd generation",
            # country_name_short == "CZE" & election_year %in% c(2010) ~ "4th generation",
            country_name_short == "EST" & election_no > 1 & election_year %in% c(1995) ~ "2nd generation",
            country_name_short == "EST" & election_year %in% c(1999, 2003) ~ "3rd generation",
            country_name_short == "HRV" & election_year %in% c(2003, 2007, 2011) ~ "3rd generation",
            # country_name_short == "HRV" & election_year %in% c(2015) ~ "4th generation",
            country_name_short == "HUN" & election_no > 1 & election_year <= 1994 ~ "2nd generation",
            country_name_short == "HUN" & election_year %in% c(1998, 2002, 2006) ~ "3rd generation",
            # country_name_short == "HUN" & election_year %in% c(2010) ~ "4th generation",
            country_name_short == "LTU" & election_no > 1 & election_year <= 1996 ~ "2nd generation",
            country_name_short == "LTU" & election_year %in% c(2000, 2004) ~ "3rd generation",
            country_name_short == "LVA" & election_no > 1 & election_year <= 1995 ~ "2nd generation",
            country_name_short == "LVA" & election_year %in% c(1998, 2002) ~ "3rd generation",
            country_name_short == "POL" & election_no > 1 & election_year <= 1993 ~ "2nd generation",
            country_name_short == "POL" & election_year %in% c(1997, 2001) ~ "3rd generation",
            country_name_short == "ROU" & election_no > 1 & election_year <= 1996 ~ "2nd generation",
            country_name_short == "ROU" & election_year %in% c(2000, 2004, 2008) ~ "3rd generation",
            # country_name_short == "ROU" & election_year %in% c(2012) ~ "4th generation",
            country_name_short == "SVK" & election_no > 1 & election_year <= 1998 ~ "2nd generation",
            country_name_short == "SVK" & election_year %in% c(2002, 2006) ~ "3rd generation",
            country_name_short == "SVN" & election_no > 1 & election_year <= 2004 ~ "2nd generation",
            country_name_short == "SVN" & election_year %in% c(2008) ~ "3rd generation",
            # country_name_short == "SVN" & election_year %in% c(2011) ~ "4th generation", 
            post_crisis_election2 == "First/Second post-crisis election" ~ "4th generation (2 post-crisis el.)",
            post_crisis_election2 == "Other post-crisis election" ~ "Other post-crisis"
        )
    )


chart_data2b %>% 
    select(election_year, region_type, generation, np_share_nc_1, np_share_cv_1,
           reward_nc_pm_long, reward_nc_cab_long) %>% 
    tidyr::pivot_longer(., cols = 4:7) %>% 
    mutate(name = case_when(
        name == "np_share_nc_1" ~ "New parties share (%)", 
        name == "np_share_cv_1" ~ "Genuinely new parties vote share (%)",
        name == "reward_nc_pm_long" ~ "PM's party change (percentage points)",
        name == "reward_nc_cab_long" ~ "Government parties change (percentage points)"
    )) %>% 
    filter(!is.na(generation)) %>% 
    mutate(region_type = factor(region_type, levels = c("Formerly stable region", "Formely unstable region")), 
           name = factor(name, levels = c("New parties share (%)", "Genuinely new parties vote share (%)",
                                          "PM's party change (percentage points)",
                                          "Government parties change (percentage points)"))) %>% 
    ggplot(., aes(x = name, y = value, colour = name)) + 
    geom_boxplot(outlier.shape = NA) + 
    geom_jitter(width = 0.1) + 
    facet_wrap(~region_type + generation, scales = "free_x", 
               nrow = 2) + 
    scale_colour_viridis_d() + 
    theme_bw() + 
    geom_hline(yintercept = 0, linewidth = 0.2) +
    guides(colour = guide_legend(nrow = 2, byrow = TRUE)) + 
    theme(legend.position = "top", 
          panel.grid.major.x = element_blank(), 
          axis.text.x = element_blank(), 
          axis.ticks.x = element_blank()) + 
    labs(x = "", y = "", colour = "")

ggsave("figs/generations5_all_2nd_gen.png", 
       width = 9,
       height = 6,
       units = "in",
       dpi = 300,
       type = "cairo")

# Appendix -----------------------------------------------
np_share_hist_en <- 
    final_data %>% 
    select(country_name_short, election_year, contains("np_share")) %>% 
    pivot_longer(cols = c(starts_with("np_"))) %>% 
    filter(!is.na(value)) %>%
    mutate(name = case_when(name == "np_share_cv_1" ~ "Genuinely new parties, 1%",
                            name == "np_share_pnp_1" ~ "Partially new parties, 1%",
                            name == "np_share_nc_1" ~ "All new parties, 1%",
                            name == "np_share_cv_leg" ~ "Genuinely new parties, threshold",
                            name == "np_share_pnp_leg" ~ "Partially new parties, threshold",
                            name == "np_share_nc_leg" ~ "All new parties, threshold")) 

app4_data <- np_share_hist_en %>% 
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


(np_share_hist_plot_en_app <-
        app4_data %>% 
        ggplot(aes(x = value, group = name))+
        geom_histogram(bins = 12, breaks = seq(-5,50,5), color = "white")+
        geom_text(aes(x = mean + 0.06, y = 30, label = sprintf("%1.1f%%", mean)), 
                  data = app4_data %>% select(country_name_short, mean, name) %>% 
                      unique()) +
        geom_vline(aes(xintercept = mean), linetype = "dashed", alpha = 0.5)+
        scale_x_continuous(labels = label_percent(scale = 1))+
        scale_y_continuous(minor_breaks = seq(2,10,2), breaks = seq(0,40,10), 
                           limits = c(0, 60))+
        labs(x = "Electoral results of new parties",
             y = "" #, # y = "Počet výskytů v 83 volbách", 
             #caption = "Historgram je definován takovým způsobem, že první sloupec obsahuje jen volby s nulovým ziskem.\nVertikální přerušovaná čára představuje průměrný volební zisk nových stran.")+
        )+
        facet_wrap(~name, ncol = 1)+
        theme_bw()+
        theme(legend.position = "none"))

(ch3_all_parties_app <- app4_data %>% 
        filter(name == "All new parties, threshold") %>% 
        ggplot(aes(x = value))+
        geom_histogram(bins = 12, breaks = seq(-5,50,5), color = "white") +
        geom_text(aes(x = 14.7 + 4, y = 25, label = sprintf("%1.1f%%", 14.7))) +
        geom_vline(aes(xintercept = mean), linetype = "dashed", alpha = 0.5)+
        scale_x_continuous(labels = label_percent(scale = 1))+
        scale_y_continuous(minor_breaks = seq(2,10,2), breaks = seq(0,40,10), 
                           limits = c(0, 60))+
        labs(x = "",
             y = "" #, # y = "Počet výskytů v 83 volbách", 
             #caption = "Historgram je definován takovým způsobem, že první sloupec obsahuje jen volby s nulovým ziskem.\nVertikální přerušovaná čára představuje průměrný volební zisk nových stran.")+
        )+
        facet_wrap(~name) + 
        theme_bw()+
        theme(legend.position = "none"))

(ch3_gn_parties_app <- app4_data %>% 
        filter(name == "Genuinely new parties, threshold") %>% 
        ggplot(aes(x = value))+
        geom_histogram(bins = 12, breaks = seq(-5,50,5), color = "white")+
        geom_text(aes(x = 10.6 + 4, y = 25, label = sprintf("%1.1f%%", 10.6))) +
        geom_vline(aes(xintercept = mean), linetype = "dashed", alpha = 0.5)+
        scale_x_continuous(labels = label_percent(scale = 1))+
        scale_y_continuous(minor_breaks = seq(2,10,2), breaks = seq(0,40,10), 
                           limits = c(0, 40))+
        labs(x = "",
             y = "" #, # y = "Počet výskytů v 83 volbách", 
             #caption = "Historgram je definován takovým způsobem, že první sloupec obsahuje jen volby s nulovým ziskem.\nVertikální přerušovaná čára představuje průměrný volební zisk nových stran.")+
        )+
        facet_wrap(~name) + 
        theme_bw()+
        theme(legend.position = "none"))

(ch3_pnp_parties_app <- app4_data %>% 
        filter(name == "Partially new parties, threshold") %>% 
        ggplot(aes(x = value))+
        geom_histogram(bins = 12, breaks = seq(-5,50,5), color = "white") +
        geom_text(aes(x = 4.4 + 3, y = 30, label = sprintf("%1.1f%%", 4.4))) +
        geom_vline(aes(xintercept = mean), linetype = "dashed", alpha = 0.5)+
        scale_x_continuous(labels = label_percent(scale = 1))+
        scale_y_continuous(breaks = seq(0, 60, 10), minor_breaks = seq(2, 10, 2), 
                           limits = c(0, 60)) + 
        labs(x = "Electoral results of new parties",
             y = "" #, # y = "Počet výskytů v 83 volbách", 
             #caption = "Historgram je definován takovým způsobem, že první sloupec obsahuje jen volby s nulovým ziskem.\nVertikální přerušovaná čára představuje průměrný volební zisk nových stran.")+
        )+
        facet_wrap(~name) + 
        theme_bw()+
        theme(legend.position = "none"))

new_parties <- 
    final_data %>%
    select(country_name_short, election_year, starts_with("np_"))

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
    filter(grepl("threshold", name))

(np_share_facet <- 
        new_party_share %>% 
        ggplot(aes(x = election_year, y = value))+
        geom_point(alpha=0.5)+
        geom_smooth(se = FALSE, method = "loess", aes(group = 1), linetype = "solid", color = "goldenrod3")+
        scale_y_continuous(labels = percent_format(accuracy = 1, suffix = "%", scale = 1)) + 
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
        filter(name == "All new parties, threshold") %>% 
        ggplot(aes(x = election_year, y = value))+
        geom_point(alpha=0.5)+
        geom_smooth(se = FALSE, method = "loess", aes(group = 1), 
                    linetype = "solid", color = "goldenrod3")+
        scale_y_continuous(labels = percent_format(accuracy = 1, suffix = "%", 
                                                   scale = 1)) + 
        scale_x_continuous(breaks = seq(1990, 2020, 2), guide = guide_axis(angle = 90))+
        labs(x=element_blank(), 
             y=element_blank(),
             # caption = "Volební výsledky z databáze Parlgov.org, určení nových stran a výpočty autoři.\nJednotlivá pozorování vyjadřují podíl volebního zisku v jedněch volbách.\nČeské volby vyneseny červeným trojúhelníkem.\nTrendová křivka vytvořena algoritmem LOESS."
        ) +
        facet_wrap(~name) + 
        guides(color = "none") + 
        theme_bw())

(share_gn_chart <- new_party_share %>% 
        filter(name == "Genuinely new parties, threshold") %>% 
        ggplot(aes(x = election_year, y = value))+
        geom_point(alpha=0.5)+
        geom_smooth(se = FALSE, method = "loess", aes(group = 1), 
                    linetype = "solid", color = "goldenrod3")+
        scale_y_continuous(labels = percent_format(accuracy = 1, suffix = "%", 
                                                   scale = 1)) + 
        scale_x_continuous(breaks = seq(1990, 2020, 2), guide = guide_axis(angle = 90))+
        labs(x=element_blank(), 
             y=element_blank(),
             # caption = "Volební výsledky z databáze Parlgov.org, určení nových stran a výpočty autoři.\nJednotlivá pozorování vyjadřují podíl volebního zisku v jedněch volbách.\nČeské volby vyneseny červeným trojúhelníkem.\nTrendová křivka vytvořena algoritmem LOESS."
        ) +
        facet_wrap(~name) + 
        guides(color = "none") + 
        theme_bw())

(share_pnp_chart <- new_party_share %>% 
        filter(name == "Partially new parties, threshold") %>% 
        ggplot(aes(x = election_year, y = value))+
        geom_point(alpha=0.5)+
        geom_smooth(se = FALSE, method = "loess", aes(group = 1), 
                    linetype = "solid", color = "goldenrod3")+
        scale_y_continuous(labels = percent_format(accuracy = 1, suffix = "%", 
                                                   scale = 1)) + 
        scale_x_continuous(breaks = seq(1990, 2020, 2), guide = guide_axis(angle = 90))+
        labs(x=element_blank(), 
             y=element_blank(),
             # caption = "Volební výsledky z databáze Parlgov.org, určení nových stran a výpočty autoři.\nJednotlivá pozorování vyjadřují podíl volebního zisku v jedněch volbách.\nČeské volby vyneseny červeným trojúhelníkem.\nTrendová křivka vytvořena algoritmem LOESS."
        ) +
        facet_wrap(~name) + 
        guides(color = "none") + 
        theme_bw())

(ch3_all_parties_app + share_all_chart) /
    (ch3_gn_parties_app + share_gn_chart) /
    (ch3_pnp_parties_app + share_pnp_chart) +
    plot_annotation(tag_levels = list(c("A", "B")))

ggsave("figs/app_a4A.png",
       plot = last_plot(),
       width = 6,
       height = 7,
       units = "in",
       dpi = 300,
       type = "cairo")

## A4B
np_number_hist_en <- 
    final_data %>% 
    select(country_name_short, election_year, contains("np_number")) %>% 
    pivot_longer(cols = c(starts_with("np_"))) %>% 
    filter(!is.na(value)) %>%
    mutate(name = case_when(name == "np_number_cv_1" ~ "Genuinely new parties, 1%",
                            name == "np_number_pnp_1" ~ "Partially new parties, 1%",
                            name == "np_number_nc_1" ~ "All new parties, 1%",
                            name == "np_number_cv_leg" ~ "Genuinely new parties, threshold",
                            name == "np_number_pnp_leg" ~ "Partially new parties, threshold",
                            name == "np_number_nc_leg" ~ "All new parties, threshold")) 

np_number_hist_en %>% 
    group_by(name) %>% 
    mutate(mean = mean(value)) %>% 
    ungroup() %>% 
    ggplot(aes(x = value, group = name))+
    geom_histogram(bins = 10, color = "white") + 
    geom_text(aes(x = mean + 1, y = 30, label = sprintf("%1.2f", mean)),
              data = . %>% select(name, mean) %>%
                  unique()) +
    geom_vline(aes(xintercept = mean), linetype = "dashed", alpha = 0.5)+
    facet_wrap(~name, ncol = 2) +
    scale_x_continuous(breaks = seq(0, 12, by = 3))+
    theme_bw()+
    labs(x = "Number of new parties",
         y = "" #, # y = "Počet výskytů v 83 volbách", 
         #caption = "Historgram je definován takovým způsobem, že první sloupec obsahuje jen volby s nulovým ziskem.\nVertikální přerušovaná čára představuje průměrný volební zisk nových stran.")+
    )
    
ggsave("figs/fig_a2.png",
       plot = last_plot(),
       width = 6,
       height = 7,
       units = "in",
       dpi = 300,
       type = "cairo")

## A4C
new_parties <- 
    final_data %>%
    select(country_name_short, election_year, starts_with("np_"))

new_parties_number <- 
    new_parties %>% 
    select(country_name_short, election_year,starts_with("np_number")) %>% 
    rename("Genuinely new parties, threshold" = np_number_cv_leg,
           "Genuinely new parties, 1%" = np_number_cv_1,
           "All new parties, threshold" = np_number_nc_leg,
           "All new parties, 1%" = np_number_nc_1,
           "Partially new parties, threshold" = np_number_pnp_leg,
           "Partially new parties, 1%" = np_number_pnp_1) %>% 
    pivot_longer(cols = -c(country_name_short, election_year)) %>% 
    mutate(name = fct_relevel(name, 
                              "All new parties, 1%",
                              "All new parties, threshold",
                              "Genuinely new parties, 1%",
                              "Genuinely new parties, threshold",
                              "Partially new parties, 1%",
                              "Partially new parties, threshold")) %>% 
    filter(!is.na(value))

new_parties_number %>% 
    ggplot(aes(x = election_year, y = value))+
    geom_point(alpha=0.5)+
    geom_smooth(se = FALSE, method = "loess", aes(group = 1), 
                linetype = "solid", color = "goldenrod3")+
    scale_y_continuous(breaks = seq(0, 9, by = 3), 
                       minor_breaks = 1:12) +
    scale_x_continuous(breaks = seq(1990, 2020, 2), guide = guide_axis(angle = 90))+
    labs(x=element_blank(), 
         y=element_blank(),
         # caption = "Volební výsledky z databáze Parlgov.org, určení nových stran a výpočty autoři.\nJednotlivá pozorování vyjadřují podíl volebního zisku v jedněch volbách.\nČeské volby vyneseny červeným trojúhelníkem.\nTrendová křivka vytvořena algoritmem LOESS."
    ) +
    facet_wrap(~name, ncol = 2) + 
    guides(color = "none") + 
    theme_bw()

ggsave("figs/fig_a3.png",
       plot = last_plot(),
       width = 6,
       height = 7,
       units = "in",
       dpi = 300,
       type = "cairo")

## A4D
new_parties <- 
    final_data %>%
    select(country_name_short, election_year, starts_with("np_"))

new_parties_eng <- 
    new_parties %>% 
    select(country_name_short, election_year, starts_with("np_number")) %>% 
    rename("Genuinely new parties, threshold" = np_number_cv_leg,
           "Genuinely new parties, 1%" = np_number_cv_1,
           "All new parties, threshold" = np_number_nc_leg,
           "All new parties, 1%" = np_number_nc_1,
           "Partially new parties, threshold" = np_number_pnp_leg,
           "Partially new parties, 1%" = np_number_pnp_1) %>% 
    mutate(region = case_when(
        country_name_short %in% c("POL", "EST", "LVA", "SVK", "LTU", "BGR") ~ "'Formely unstable' region (Baltic states, Bulgaria, Poland, Slovakia)", 
        country_name_short %in% c("SVN", "CZE", "HRV", "ROU", "HUN") ~ "'Formerly stable' region (Croatia, Czechia, Hungary, Romania, Slovenia)"
    )) %>% 
    mutate(region = factor(region, levels = c("'Formerly stable' region (Croatia, Czechia, Hungary, Romania, Slovenia)", 
                                              "'Formely unstable' region (Baltic states, Bulgaria, Poland, Slovakia)")))

new_parties_eng %>% 
    # mutate(is_czech = if_else(country_name_short == "CZE", 1,0)) %>% 
    pivot_longer(cols = -c(country_name_short, election_year, region)) %>% 
    mutate(name = fct_relevel(name, 
                              "All new parties, 1%",
                              "All new parties, threshold",
                              "Genuinely new parties, 1%",
                              "Genuinely new parties, threshold",
                              "Partially new parties, 1%",
                              "Partially new parties, threshold")) %>% 
    filter(!is.na(value)) %>% 
    ggplot(aes(x = election_year, y = value))+
    geom_point(alpha=0.5, aes(colour = region))+
    geom_smooth(se = FALSE, method = "loess", aes(colour = region), linetype = "solid")+
    scale_y_continuous(breaks = seq(0, 9, by = 3), 
                       minor_breaks = 1:12) + 
    scale_x_continuous(breaks = seq(1990, 2020, 2), guide = guide_axis(angle = 90))+
    scale_colour_viridis_d(option = "D") + 
    # scale_colour_manual(values = c("#f1a340", "#998ec3")) +
    labs(x=element_blank(), 
         y=element_blank(),
         colour = "") + 
    facet_rep_wrap(~name, repeat.tick.labels = 'all', ncol = 2)+
    theme_bw() +
    theme(panel.spacing = unit(1, "lines"),
          legend.position = "top") + 
    guides(colour = guide_legend(nrow=2,byrow=TRUE))

ggsave("figs/app_a4D.png",
       plot = last_plot(),
       width = 6,
       height = 7,
       units = "in",
       dpi = 300,
       type = "cairo")
