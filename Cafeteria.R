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

filter(consumed, item %in% c("green", "purple", "red", "mussel")) %>%
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

# Pycno Sizes
PycnoMetrics %>%
  ggplot(aes(x = pycnoID, y = diameter_cm)) +
  geom_point(size = 4)



####
#<<<<<<<<<<<<<<<<<<<<<<<<<<END OF SCRIPT>>>>>>>>>>>>>>>>>>>>>>>>#

# SCRATCH PAD ####

here <- cafeteria %>%
  mutate(consumed = as.integer(!(is.na(consumed_date))))

