#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#                                                                                ##
# Pycno Feeding Preference Cafeteria Experiment                                  ##
# Data are current as of 2022-08-04                                              ##
# Data source: Ross Whippo - OIMB/FHL                                            ##
# R code prepared by Ross Whippo                                                 ##
# Last updated 2022-08-04                                                        ##
#                                                                                ##
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# SUMMARY:


# Required Files (check that script is loading latest version):
# PycnoFeeding_cafeteria.csv

# Associated Scripts:
# FILE.R

# TO DO 

# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# TABLE OF CONTENTS                                                            ####
#                                                                                 +
# RECENT CHANGES TO SCRIPT                                                        +
# LOAD PACKAGES                                                                   +
# READ IN AND PREPARE DATA                                                        +
# MANIPULATE DATA                                                                 +
#                                                                                 +
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# RECENT CHANGES TO SCRIPT                                                     ####
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# 2022-08-04 Script created

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# LOAD PACKAGES                                                                ####
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

library(tidyverse)
library(viridis)
library(vegan)
library(rstatix)
library(ggpubr)
library(reshape2)

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# READ IN AND PREPARE DATA                                                     ####
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

cafeteria <- read_csv("Data/PycnoFeeding_cafeteria.csv")
PycnoMetrics <- read_csv("Data/PycnoMetrics.csv")

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# MANIPULATE DATA                                                              ####
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# create column for counts of consumed critters
consumed <- cafeteria %>%
  mutate(consumed = as.integer(!(is.na(consumed_date)))) %>%
  group_by(pycnoID, item) %>%
  summarise(eaten = sum(consumed))

consumed$item <- factor(consumed$item, levels = c("green", "purple", "red", "mussel", "cucumber", "chiton"))

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# FIGURES                                                                      ####
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# boxplot of number of items consumed per pycno

consumed %>%
  ggplot(aes(x = item, y = eaten)) +
  geom_boxplot() +
  theme_minimal()

# mean and sterror of consumption for urchins and mussels

#filter(consumed, item %in% c("green", "purple", "red", "mussel")) %>%
consumed %>%
ggplot(aes(x = item, y = eaten)) +
  geom_jitter(col = "grey", size = 3, width = 0.15) +
  scale_color_viridis(discrete = TRUE,
                      begin = 0.3,
                      end = 0.7,
                      option = "magma") +
  stat_summary(
    geom = "point",
    fun = "mean",
    size = 6,
    shape = 19
  ) +
  geom_errorbar(stat="summary", fun.data="mean_se", size = 1) +
  theme_minimal() +
  labs(x = "Prey Item", y = "Number Consumed") +
  theme(axis.text.x = element_text(vjust = 5))

consumed %>%
  group_by(item) %>%
  summarise(mean(eaten), sd(eaten))

# barplot of consumption by pycno
consumed %>% filter(item %in% c('green', 'mussel', 'red', 'purple', 'cucumber')) %>%
  ggplot(aes(x = item, y = eaten, fill = item)) +
  geom_col() +
  scale_fill_viridis(discrete = TRUE, option = 5, begin = 0.1, end = 0.8) +
  facet_wrap(pycnoID~.) +
  theme_bw() +
  labs(x = "", y = "Number Consumed") +
  theme(axis.text.x=element_blank(), axis.ticks.x = element_blank()) +
  theme(legend.position = c(.9, .16))

# proportional barplot of consumption per pycno
totals<- consumed %>%
  group_by(pycnoID) %>%
  summarise(total=sum(eaten))
  
consumed %>%
  filter(item %in% c('green', 'purple', 'red', 'mussel', 'cucumber')) %>%
  ggplot() +
  geom_col(aes(x = reorder(pycnoID, eaten, function(x){ sum(x)}), y = eaten, fill = item), position = "fill") +
  scale_fill_viridis(discrete = TRUE, option = 5, end = 0.9) +
  theme_bw() + 
  scale_x_discrete(label = c('A', 'B', 'C', 'A', 'A', 'D', 'D', 'E', 'A', 'A', 'A')) +
  geom_text(data = totals, aes(x = pycnoID, y= 1.05, label = total, fill = NULL)) +
  labs(x = "", y = "Proportion Consumed") +
  guides(fill=guide_legend(title="Prey"))

# barplot of prey sizes
cafeteria %>%
  ggplot(aes(x = item, y = size)) +
  geom_boxplot()

# consumed prey by size
cafeteria %>%
  mutate(consumed = as.character(as.integer(!(is.na(consumed_date))))) %>%
  mutate(consumed = recode(consumed, '1' = 'consumed', '0' = 'not consumed')) %>%
  ggplot(aes(x = item, y = size, color = consumed)) +
  geom_point(size = 2, position = "jitter") +
  scale_color_viridis(discrete = TRUE, begin = 0.2, end = 0.8, option = "D")

# consumed urchins by size with points and boxplots
urchin_plot <- cafeteria %>%
  mutate(consumed = as.character(as.integer(!(is.na(consumed_date))))) %>%
  mutate(consumed = recode(consumed, '1' = 'consumed', '0' = 'not consumed')) %>%
  filter(item %in% c("green", "red", "purple")) %>%
  unite(urchin_species, item, consumed, sep = " ", remove = FALSE)

  ggplot(urchin_plot, aes(item, size), fill = consumed) +
  geom_boxplot(aes(fill = consumed), alpha = 0.2) +
  geom_point(size = 2, aes(color = consumed), position = position_jitterdodge(jitter.width = 0.2)) +
  scale_color_viridis(name = NULL, discrete = TRUE, begin = 0.2, end = 0.8, option = "D") +
  scale_fill_viridis(name = NULL, discrete = TRUE, begin = 0.2, end = 0.8, option = "D") +
  xlab("urchin species") +
  ylab("test diameter (mm)") +
  theme_classic() 




consumed_stack <- consumed %>%
  filter(item != "chiton") %>%
  group_by(pycnoID, item) %>%
  summarise(`Number Consumed` = sum(eaten))

# total number of prey items eaten across all trials
consumed_stack %>%
  ggplot(aes(x = item, y = `Number Consumed`, fill = pycnoID)) +
  geom_col() +
  scale_fill_viridis(discrete = TRUE, option = 7) +
  theme_minimal() +
  labs(x = "Prey Item") +
  theme(axis.text.x = element_text(vjust = 5), axis.text.y = element_text(hjust = 2)) +
  guides(fill=guide_legend(title="Pycnopodia ID"))

consumed_stack %>%
  group_by(item) %>%
  summarise(sum(`Number Consumed`))

# Pycno Sizes
PycnoMetrics %>%
  ggplot(aes(x = pycnoID, y = diameter_cm)) +
  geom_point(size = 4)

PycnoMetrics %>%
  summarise(mean(diameter_cm), sd(diameter_cm))

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# STATS                                                                        ####
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# use S-W diversity measures - diversity and evenness of preferences

# make dataset wide
consumed_wide <- consumed %>%
  filter(item != "chiton") %>%
  pivot_wider(names_from = item, values_from = eaten)

# shannon
diet_div <- diversity(consumed_wide[,2:6])
# evenness
diet_even <- diet_div/log(specnumber(consumed_wide))

# plot data
diet_plot <- consumed_wide %>%
  select(pycnoID) %>%
  add_column(diet_div, diet_even)
diet_plot <- diet_plot %>%
  pivot_longer(diet_div:diet_even, names_to = "measure", values_to = "value")
ggplot(diet_plot) +
  geom_boxplot(aes(x = measure, y = value, fill = measure)) +
  scale_fill_viridis(discrete = TRUE, begin = 0.4, end = 0.8, option = "F") +
  theme_bw() +
  scale_x_discrete(label = c('S-W Diversity', 'Evenness')) +
  labs(x = "Measure", y = "Value") +
  theme(legend.position = "none")




# rogers index of food preference

consumed_rodgers <- cafeteria %>%
  filter(!is.na(enveloped_date)) %>%
  select(pycnoID, item, enveloped_date) %>%
  mutate(quantity = 1) %>%
  group_by(pycnoID, item) %>%
  mutate(day = case_when(enveloped_date == "2022-07-08" ~ 1,
                         enveloped_date == "2022-07-09" ~ 2,
                         enveloped_date == "2022-07-10" ~ 3,
                         enveloped_date == "2022-07-11" ~ 4,
                         enveloped_date == "2022-07-12" ~ 5,
                         enveloped_date == "2022-07-13" ~ 6,
                         enveloped_date == "2022-07-14" ~ 7,
                         enveloped_date == "2022-07-15" ~ 8,
                         enveloped_date == "2022-07-16" ~ 9,
                         enveloped_date == "2022-07-17" ~ 10,
                         enveloped_date == "2022-07-18" ~ 11,
                         enveloped_date == "2022-07-28" ~ 1,
                         enveloped_date == "2022-07-29" ~ 2,
                         enveloped_date == "2022-07-30" ~ 3,
                         enveloped_date == "2022-07-31" ~ 4,
                         enveloped_date == "2022-08-01" ~ 5,
                         enveloped_date == "2022-08-02" ~ 6,
                         enveloped_date == "2022-08-03" ~ 7,
                         enveloped_date == "2022-08-04" ~ 8,
                         enveloped_date == "2022-08-05" ~ 9,
                         enveloped_date == "2022-08-06" ~ 10,
                         enveloped_date == "2022-08-07" ~ 11,
                         enveloped_date == "2022-08-14" ~ 1,
                         enveloped_date == "2022-08-15" ~ 2,
                         enveloped_date == "2022-08-16" ~ 3,
                         enveloped_date == "2022-08-17" ~ 4,
                         enveloped_date == "2022-08-18" ~ 5,
                         enveloped_date == "2022-08-19" ~ 6,
                         enveloped_date == "2022-08-20" ~ 7,
                         enveloped_date == "2022-08-21" ~ 8,
                         enveloped_date == "2022-08-22" ~ 9,
                         enveloped_date == "2022-08-23" ~ 10,
                         enveloped_date == "2022-08-24" ~ 11,
                         enveloped_date == "2022-09-08" ~ 1,
                         enveloped_date == "2022-09-09" ~ 2,
                         enveloped_date == "2022-09-10" ~ 3,
                         enveloped_date == "2022-09-11" ~ 4,
                         enveloped_date == "2022-09-12" ~ 5,
                         enveloped_date == "2022-09-13" ~ 6,
                         enveloped_date == "2022-09-14" ~ 7,
                         enveloped_date == "2022-09-15" ~ 8,
                         enveloped_date == "2022-09-16" ~ 9,
                         enveloped_date == "2022-09-17" ~ 10,
                         enveloped_date == "2022-09-18" ~ 11,
                         enveloped_date == "2022-09-29" ~ 1,
                         enveloped_date == "2022-09-30" ~ 2,
                         enveloped_date == "2022-10-01" ~ 3,
                         enveloped_date == "2022-10-02" ~ 4,
                         enveloped_date == "2022-10-03" ~ 5,
                         enveloped_date == "2022-10-04" ~ 6,
                         enveloped_date == "2022-10-05" ~ 7,
                         enveloped_date == "2022-10-06" ~ 8,
                         enveloped_date == "2022-10-07" ~ 9,
                         enveloped_date == "2022-10-08" ~ 10,
                         enveloped_date == "2022-10-09" ~ 11)) %>%
  complete(day = c(1:11)) %>%
  replace_na(list(quantity = 0)) %>%
  ungroup() %>%
  group_by(pycnoID, item, day) %>%
  summarise(quantity = sum(quantity)) %>%
  reframe(quantity = cumsum(quantity)) %>%
  mutate(day = rep(1:11, 34)) %>%
  mutate(item = factor(item, levels = c("green", "purple", "red", "mussel", "cucumber"))) %>%
  mutate(Prey = item) %>%
  mutate(Site = case_when(pycnoID == "Eagle1" ~ "A (Eagle Cove 1)",
                          pycnoID == "Eagle2" ~ "A (Eagle Cove 2)",
                          pycnoID == "Eagle3" ~ "A (Eagle Cove 3)",
                          pycnoID == "Eagle4" ~ "A (Eagle Cove 4)",
                          pycnoID == "Eagle5" ~ "A (Eagle Cove 5)",
                          pycnoID == "Eagle6" ~ "A (Eagle Cove 6)",
                          pycnoID == "ONeal1" ~ "B (O'Neal Is.)",
                          pycnoID == "PtCaution2" ~ "C (Pt. Caution)",
                          pycnoID == "Goose1" ~ "D (Goose Is. 1)",
                          pycnoID == "Goose2" ~ "D (Goose Is. 2)",
                          pycnoID == "Docks3" ~ "E (FHL Docks)"))



consumed_rodgers %>%
  ggplot(aes(x = day, y = quantity, color = Prey, fill = Prey)) + 
  geom_line() +
  geom_point() +
  scale_fill_viridis(discrete = TRUE, option = 5, end = 0.9) +
  scale_color_viridis(discrete = TRUE, option = 5, end = 0.9) +
  scale_y_continuous(limits = c(0,16)) +
  facet_wrap(Site~.) +
  theme_bw() +
  xlab("Day") +
  ylab("Count")



# area under curve

Area <- sm_auc_all(subjects = 'pycnoID', conditions = 'Prey', 
                   x = 'day', values = 'quantity',
                   data = consumed_rodgers) %>%
  group_by(pycnoID) %>%
  arrange(desc(AUC_quantity), .by_group = TRUE)

Area1 <- Area %>%
  group_by(Prey) %>%
  summarise("AUC (mean)" = mean(AUC_quantity), "AUC (st.dev.)" = sd(AUC_quantity)) %>%
  arrange(desc(`AUC (mean)`))

write_csv(Area, "area_uc_csv.csv")
write_csv(Area1, "area1_uc_csv.csv")




####
#<<<<<<<<<<<<<<<<<<<<<<<<<<END OF SCRIPT>>>>>>>>>>>>>>>>>>>>>>>>#

# SCRATCH PAD ####

# test rodgers with time enveloped


