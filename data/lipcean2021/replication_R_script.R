########################################
########################################
########################################
#####R version 4.0.4 (2021-02-15)#######

setwd("Set your working directory") # Setting the working directory

# Download R packages 

library(tidyverse)# Version tidyverse_1.3.0
library(scales) # Version scales_1.1.1
library(ggthemes) # Version ggthemes_4.2.4
library(ggrepel) # Version ggrepel_0.9.1 
library(ggpubr) # Version ggpubr_0.4.0
library(svglite) # Version svglite_2.0.0
library(cowplot) # Version cowplot_1.1.1  

####################################
### Import the dataset from the working directory 

dpf_dataset <- read_csv("dpf_dataset.csv")

####################################
### Custom theme used in some figures

custom_theme <- theme_few(base_size = 12, base_family = "serif") +
  theme(panel.border = element_rect(color = "gray60"),
        panel.grid.major = element_line(color = "grey70", size= 0.1, linetype = 3))

###################################
###################################

### Figure 1 - The level of DPF across post-communist regimes by the method setting the amount of and funding type for DPFRV and DPFV 1990-2020

### Data manipulation for Figure 1

figure_1_data <- dpf_dataset %>%
  select(country_id, year, method_amount_st, method_amount_el, contains(c("dpfrv", "dpfv"))) %>%
  pivot_longer(-c(country_id, year, method_amount_st, method_amount_el), names_to = "dpf_type", values_to = "dpf_amount") %>%
  mutate(dpf_type = factor(dpf_type, levels = c("dpfrv_stat_usd", "dpfv_stat_usd", "dpfrv_elect_usd", "dpfv_elect_usd", "dpfrv_total", "dpfv_total"), labels = c("DPFRV Statutory", "DPFV Statutory", "DPFRV Elections", "DPFV Elections", "DPFRV Total", "DPFV Total"))) 

### Figure 1 - Left-hand panel: Distribution of DPF by method for party statutory activities

figure_1_A <- figure_1_data %>%
  filter(dpf_type %in% c("DPFRV Statutory", "DPFV Statutory")) %>%
  drop_na(method_amount_st) %>%
  ggplot(aes(x = dpf_type, y = dpf_amount, colour = method_amount_st)) +
  geom_boxplot(varwidth = TRUE, notch = TRUE, outlier.shape = 1) +
  scale_y_continuous(breaks = seq(0,15,3))+
  scale_color_grey("", start = 0.2, end = 0.7) +
  guides(colour = guide_legend("", ncol = 1)) +
  custom_theme +
  theme(title = element_text(size = 10),
        legend.position = c(0.25, 0.9),
        legend.text = element_text(size = 11),
        legend.background = element_blank(),
        legend.key.size = unit(1, 'lines'),
        plot.margin = unit(c(0.5, 0.05, 0, 0), "lines")) +
  labs(title = "A: Method determining the DPF level \n for statutory activities",x = "", y = "USD")
figure_1_A
### Figure 1 - Right-hand panel: Distribution of DPF by method for election activities

figure_1_B <- figure_1_data %>%
  filter(dpf_type %in% c("DPFRV Elections", "DPFV Elections")) %>%
  drop_na(method_amount_st) %>%
  ggplot(aes(x = dpf_type, y = dpf_amount, colour = method_amount_st)) +
  geom_boxplot(varwidth = TRUE, notch = TRUE, outlier.shape = 1) +
  scale_y_continuous(breaks = seq(0,15,3), limits = c(0, 15)) +
  scale_color_grey("", start = 0.2, end = 0.7) +
  guides(colour = guide_legend("", ncol = 1)) +
  custom_theme +
  theme(title = element_text(size = 10),
        legend.position = c(0.25, 0.9),
        legend.text = element_text(size = 11),
        legend.background = element_blank(),
        legend.key.size = unit(1, 'lines'),
        plot.margin = unit(c(0.5, 0.2, 0, -0.25), "lines")) +
  labs(title = "B: Method determining the DPF level \n for election activities", x = "", y = "")
figure_1_B
### Combine the left-hand and the right-hand panels of Figure 1
plot_grid(figure_1_A, figure_1_B)

### Export Figure 1
ggsave("figure_1.svg",width=7, height=4, units = "in")
dev.off()


######################################
######################################

### Figure 2. Relationship between the level of DPF for party statutory and election financing 

### Figure 2 - Left-hand panel: Scatterplot of DPFRV for party statutory and election financing

figure_2_A <- dpf_dataset %>%
  filter(dpfrv_total >0) %>%
  ggplot(aes(x = dpfrv_stat_usd, 
             y = dpfrv_elect_usd, 
             shape = country_id)) +
  geom_jitter(width = 0.05, height = 0.05, color = "gray40", fill = "gray70") +
  scale_shape_manual("", values = c(0:25, 35)) +
  scale_x_continuous(n.breaks = 7, trans=pseudo_log_trans(base = 10), limits = c(0,12)) +
  scale_y_continuous(n.breaks = 7, trans=pseudo_log_trans(base = 10), limits = c(0,9)) +
  theme_few(base_size = 11, 
            base_family = "serif") +
  theme(legend.position = "none") +
  labs(title = "A: DPFRV", x = "DPFRV Statutory US$ (log10 axis)", y = "DPFRV Elections US$ (log10 axis)")

### Figure 2 - Right-hand panel: Scatterplot of DPFV for party statutory and election financing

figure_2_B <- dpf_dataset %>%
  filter(dpfrv_total >0) %>%
  ggplot(aes(x = dpfv_stat_usd, 
             y = dpfv_elect_usd, 
             shape = country_id)) +
  geom_jitter(width = 0.05, height = 0.05, color = "gray40", fill = "gray70") +
  scale_shape_manual("", values = c(0:25, 35)) +
  scale_x_continuous(n.breaks = 7, trans=pseudo_log_trans(base = 10)) +
  scale_y_continuous(n.breaks = 7, trans=pseudo_log_trans(base = 10), limits = c(0,9)) +
  theme_few(base_size = 11, 
            base_family = "serif") +
  labs(title = "B: DPFV", x = "DPFV Statutory US$ (log10 axis)", y = "DPFV Elections US$ (log10 axis)")

### Combining the left-hand and the right-hand panels of Figure 2
plot_grid(figure_2_A, figure_2_B, rel_widths = c(1, 1.5))

### Export figure 2
ggsave("figure_2.svg",width=8, height=3.5, units = "in")
dev.off()  

####################################
####################################

### Figure 3 - Variation over time in the level of DPF by country and type 1990-2020

### Data manipulation for figure 3 

figure_3_data <- dpf_dataset %>%
  select(country_name:elections_date, dpfrv_total, dpfv_total) %>%
  pivot_longer(-c(1:4), names_to = "dpf_type", values_to = "amount") %>%
  mutate(dpf_type = factor(dpf_type, 
                           levels = c("dpfrv_total", "dpfv_total"), 
                           labels = c("DPFRV", "DPFV"))) %>%
  group_by(country_name) %>%
  mutate(dpf_country = sum(amount, na.rm = TRUE)) %>%
  filter(country_id != "KGZ")

### Plot figure 3

ggplot(figure_3_data) +
  geom_line(aes(x = year, y = amount, linetype = dpf_type,
                color = dpf_type)) +
  scale_y_continuous(labels = number_format(accuracy = 0.1)) +
  scale_linetype_manual("", values = c(5,1)) +
  scale_color_manual("", values = c("gray40", "gray60"), labels = c("DPFRV", "DPFV")) +
  facet_wrap(~fct_rev(fct_reorder(country_name, dpf_country)), ncol = 4, scales = "free_y") +
  theme_few(base_size = 11, base_family = "serif")+
  theme(legend.position = c(0.65,0.06),
        legend.background = element_blank(),
        panel.spacing.x = unit(0,"lines"),
        panel.spacing.y = unit(0.05,"lines"))+ 
  labs(x = "", y = "USD")

### Export Figure 3
ggsave("figure_3.svg",width=7, height=8, units = "in")
dev.off()  

####################################
####################################

### Figure 4 - Cross-national and within-country variation in access rules to DPF for party statutory and election financing

### Data manipulation for Figure 4: statutory funding

figure_4_data <- dpf_dataset %>%
  select(country_id, year, elections_date, access_statutory, access_elections) %>%
  pivot_longer(-c(country_id, year, elections_date), names_to = "type_access", values_to = "threshold_access") %>%
  mutate(type_access = factor(type_access, labels = c("Election financing", "Statutory financing"))) %>%
  group_by(country_id, type_access) %>%
  mutate(n = sum(threshold_access, na.rm = TRUE)) 

### Text labels for the figure_4_data on the access requirements to party statutory funding
access_labels <- figure_4_data %>%
  filter(type_access =="Statutory financing") %>%
  group_by(country_id, threshold_access) %>%
  filter(year == min(year)) %>%
  distinct(country_id, year) %>%
  drop_na(threshold_access)

### Figure 4 - Left-hand panel: Eligibility threshold for party statutory funding 

figure_4_A <- figure_4_data %>%
  filter(type_access =="Statutory financing" & !is.na(threshold_access)) %>%
  ggplot(aes(x = year, y = fct_rev(country_id), fill = threshold_access)) +
  geom_raster() +
  scale_fill_gradient(low = "gray95", high = "gray60", breaks = c(0, 1, 2, 2.5,3, 3.6, 4, 5, 7)) +
  geom_text(data = access_labels, aes(x = year, y = country_id, label = threshold_access), size = 3, nudge_x = .25, family = "serif") +
  scale_x_continuous(breaks = seq(1990, 2020, 2)) +
  theme_few(base_family = "serif", base_size = 12) +
  theme(legend.position = "none",
        plot.title = element_text(size = 12),
        axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  labs(title = "A: Statutory financing", x="", y="")
figure_4_A


### Data manipulation for Figure 4: election funding

figure_4_data_elections <- figure_4_data %>%
  filter(type_access =="Election financing") %>%
  drop_na(elections_date) %>%
  group_by(country_id) %>%
  mutate(n_elections = row_number(),
         n_elections2 = factor(n_elections, levels = c(1:12), labels = c("first", "second","third", "fourth","fifth","sixth","seventh", "eighth", "ninth", "tenth", "eleventh", "twelvth"))) %>%
  ungroup() %>%
  drop_na(threshold_access) %>%
  filter(threshold_access < 50)

### Text labels for the figure_4_data on the access to elections funding

access_label_elections <- figure_4_data_elections %>%
  group_by(country_id, threshold_access) %>%
  filter(n_elections == min(n_elections)) %>%
  distinct(country_id, n_elections) 

### Figure 4 - Right-hand panel: Eligibility threshold for election funding

figure_4_B <- figure_4_data_elections %>%
  ggplot(aes(x = n_elections2, y = fct_rev(country_id), fill = threshold_access)) +
  geom_raster() +
  scale_fill_gradient(low = "gray96", high = "gray60", breaks = c(0, 0.5, 0.7, 1.5, 2, 2.5, 3, 3.6, 4, 5, 6)) +
  geom_text(data = access_label_elections, aes(x = n_elections, label = threshold_access), size = 3, nudge_x = 0, family = "serif") +
  theme_few(base_family = "serif", base_size = 12) +
  theme(legend.position = "none",
        plot.title = element_text(size = 12),
        axis.text.x = element_text(angle = 90, hjust = 0, vjust = 0.5)) +
  labs(title = "B: Election financing", x="", y="")
figure_4_B

### Combine left-hand and right-hand panels
plot_grid(figure_4_A, figure_4_B)

### Export Figure 4
ggsave("figure_4.svg",width=7, height=4, units = "in")
dev.off() 

###################################
###################################

# Figure 5 - Variation over time in the allocation rules for DPF for party statutory and election financing 1990-2020

### Data manipulation for the figure 5 

figure_5_data <- dpf_dataset %>%
  select(country_id, year, elections_date, distrib_statutory, distrib_elections) %>%
  pivot_longer(-c(country_id, year, elections_date), names_to = "type_allocation", values_to = "concentr_allocation") %>%
  mutate(type_allocation = factor(type_allocation, labels = c("Election financing", "Statutory financing"))) %>%
  group_by(country_id, type_allocation) %>%
  mutate(n = sum(concentr_allocation, na.rm = TRUE))
 
### Text labels for the figure_5_data on the allocation rules for party statutory funding

allocation_labels <- figure_5_data %>%
  filter(type_allocation =="Statutory financing") %>%
  group_by(country_id, concentr_allocation) %>%
  filter(year == min(year)) %>%
  distinct(country_id, year) %>%
  drop_na(concentr_allocation)

### Figure 5 - Left-hand panel: Allocation rules of DPF for party statutory funding

figure_5_A <- figure_5_data %>%
  filter(type_allocation =="Statutory financing" & !is.na(concentr_allocation)) %>%
  ggplot(aes(x = year, y = fct_rev(country_id), fill = concentr_allocation)) +
  geom_raster() +
  scale_fill_gradient(low = "gray60", high = "gray95", breaks = c(0.5, 0.6, 0.66, 0.7,0.75, 0.8, 0.85, 0.9, 1)) +
  geom_text(data = allocation_labels, aes(x = year, y = country_id, label = concentr_allocation), size = 3, nudge_x = .5, family = "serif") +
  scale_x_continuous(breaks = seq(1990, 2020, 2)) +
  theme_few(base_family = "serif", base_size = 12) +
  theme(legend.position = "none",
        plot.title = element_text(size = 12),
        axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  labs(title = "A: Statutory financing", x="", y="")
figure_5_A

### Data manipulation for Figure 5 on allocation rules for election funding

figure_5_data_elections <- figure_5_data %>% 
  filter(type_allocation == "Election financing") %>%
  drop_na(elections_date) %>%
  group_by(country_id) %>%
  mutate(n_elections = row_number(),
         n_elections2 = factor(n_elections, levels = c(1:12), labels = c("first", "second","third", "fourth","fifth","sixth","seventh", "eighth", "ninth", "tenth", "eleventh", "twelvth"))) %>%
  ungroup() %>%
  drop_na(concentr_allocation)

### Text labels for the figure_5_data on the allocation rules for election financing

allocation_labels_elections <- figure_5_data_elections %>%
  group_by(country_id, concentr_allocation) %>%
  filter(n_elections == min(n_elections)) %>%
  distinct(country_id, n_elections) 

### Figure 5 - Right-hand panel: Allocation rules of DPF for election funding

figure_5_B <- figure_5_data_elections %>%
  ggplot(aes(x = n_elections2, y = fct_rev(country_id), fill = concentr_allocation)) +
  geom_raster() +
  scale_fill_gradient(name = "DF", low = "gray60", high = "gray95", breaks = c(0, 0.33, 0.5, 0.6, 0.67, 0.7, 0.8, 1)) +
  geom_text(data = allocation_labels_elections, aes(x = n_elections, label = concentr_allocation), size = 3, nudge_x = 0, family = "serif") +
  theme_few(base_family = "serif", base_size = 12) +
  theme(legend.position = "none",
        plot.title = element_text(size = 12),
        axis.text.x = element_text(angle = 90, hjust = 0, vjust = 0.5)) +
  labs(title = "B: Election financing", x="", y="")
figure_5_B

### Combine left-hand and right-hand panels
plot_grid(figure_5_A, figure_5_B)

### Export figure 5
ggsave("figure_5.svg",width=7, height=4, units = "in")
dev.off() 

###################################
###################################

### Figure 6.A - Relationship between the expert scores of the significance of DPF and the DPFV

set.seed(224)

expert_amount <- dpf_dataset %>% 
  ggplot(aes(x = dpfv_total, y = v2elpubfin)) +
  geom_jitter(width = 0.5, height = 0.25, size = 2, shape = 1, alpha = 0.5, color = "gray30") +
  scale_x_continuous(breaks = seq(0,15,3), limits = c(0,15)) +
  scale_y_continuous(breaks = seq(-2.5,3.5,1.5), limits = c(-2.5, 3.5)) +
  custom_theme +
  theme(plot.title = element_text(size = 12)) +
  labs(title = "A: Expert scores vs. real DPFV", y = "Low / Significance of DPF / High", x = "DPF per vote US$")
expert_amount
### Figure 6.B - Relationship between the expert scores of equitable access and pay-out threshold

expert_access <- dpf_dataset %>%
  mutate(labels_access_el = str_c(country_id, year, sep = "-")) %>% ### Create country-year labels
  filter(access_elections < 10) %>%
  ggplot(aes(x = access_elections, y = subsidies_eip)) +
  geom_point(color = "gray30", shape = 1, size = 2) +
  #geom_smooth() +
  geom_text_repel(aes(label = labels_access_el), size = 3, family = "serif") +
  scale_x_continuous(breaks = seq(0,6,1)) +
  #scale_y_continuous(breaks = seq(-2.5,3.5,1.5), limits = c(-2.5, 3.5)) +
  custom_theme +
  theme(plot.title = element_text(size = 12)) +
  labs(title = "B: Expert scores vs. legal access threshold", y = "Disagree / Equitable access to DPF / Agree", x = "Access to DPF / % of votes/seats")
expert_access

# Combine 6.A and 6.B
plot_grid(expert_amount, expert_access)
# Export Figure 6
ggsave("figure_6.svg",width=8, height=4, units = "in")
dev.off()

#################################
#################################

### Given the fact that data used to draw Figure 7 is not my data and is not in open access but this article and associated data will be available for free, I provide here only the code for the figures without data. For those who are interested in data shown in Figure 7, I will provide it by request

### Figure 7 - Relationship between the level of DPF per voter and the share of DPF in the structure of party income 

### Data import 

### dpfrv_dependence <- read_csv("dpf_dependence.csv")

### Formula for including regression model on the plot
xy_formula <- y ~ x

### Figure 7 - Left-hand panel: Bivariate relationship between DPFRV vs. state dependence rate, raw data

figure_7_A <- dpfrv_dependence %>%
  ggplot(aes(x = dpf_voter, dpf_percent)) +
  geom_point(aes(shape = author, color = author)) +
  stat_smooth(method = "lm", color = "grey50", fill = "gray80", formula = xy_formula, se = TRUE) +
  geom_text_repel(aes(label = country_id), family = "serif", size = 3, max.overlaps = 20, segment.color = "grey50", segment.size = 0.1) +
  scale_color_grey("", start = 0.1, end = 0.5) +
  scale_shape_manual("", values = c(1, 5, 6)) +
  scale_x_continuous(breaks = seq(0,20,3)) +
  stat_regline_equation(
    aes(label =  paste(..eq.label.., ..adj.rr.label.., sep = "~")), label.x = 0.5, label.y = 110, family = "serif", formula = xy_formula
  ) +
  stat_cor(method = "pearson", label.x = 0.5, label.y = 104, family = "serif") +
  custom_theme +
  theme(legend.position = c(0.75, 0.15),
        title = element_text(size = 11),
        legend.key = element_blank(),
        legend.title = element_blank(),
        legend.background = element_blank()) +
  labs(title = "A: DPFRV vs % DPF in party income", x = "DPF per voter", y = "% DPF in party income")
figure_7_A

### Figure 7 - Right-hand panel: Bivariate relationship between DPFRV vs. state dependence rate, transformed data

figure_7_B <- dpfrv_dependence %>%
  ggplot(aes(x = dpf_voter, dpf_percent)) +
  geom_point(aes(shape = author, color = author)) +
  stat_smooth(method = "lm", color = "grey50", fill = "gray80", formula = xy_formula, se = TRUE) +
  geom_text_repel(aes(label = country_id), family = "serif", size = 3, max.overlaps = 20, segment.color = "grey50", segment.size = 0.1) +
  scale_color_grey("", start = 0.1, end = 0.5) +
  scale_shape_manual("", values = c(1, 5, 6)) +
  scale_x_log10(breaks = c(0,1,2,5,10, 20)) +
  stat_regline_equation(
    aes(label =  paste(..eq.label.., ..adj.rr.label.., sep = "~")), label.x = -1.1, label.y = 95, family = "serif", formula = xy_formula
  ) +
  stat_cor(method = "pearson", label.x = -1.1, label.y = 89, family = "serif") +
  custom_theme +
  theme(legend.position = c(0.75, 0.15),
        title = element_text(size = 11),
        legend.key = element_blank(),
        legend.title = element_blank(),
        legend.background = element_blank()) +
  labs(title = "B: DPFRV (log10) vs % DPF in party income",x = "DPF per voter (log10 axis)", y = "")
figure_7_B

### Combine left-hand and right-hand panels
plot_grid(figure_7_A, figure_7_B)

ggsave("figure_7.svg", width=9 , height=4.5, units = "in")
dev.off()

####################################
####################################


