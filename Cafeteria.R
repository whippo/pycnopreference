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

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# MANIPULATE DATA                                                              ####
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# create column for counts of consumed critters
consumed <- cafeteria %>%
  mutate(consumed = as.integer(!(is.na(consumed_date)))) %>%
  group_by(pycnoID, item) %>%
  summarise(eaten = sum(consumed))

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# FIGURES                                                                      ####
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# boxplot of number of items consumed per pycno

consumed %>%
  ggplot(aes(x = item, y = eaten)) +
  geom_boxplot() +
  theme_minimal()

# barplot of consumption by pycno
consumed %>% filter(item %in% c('green', 'mussel', 'red', 'purple')) %>%
  ggplot(aes(x = item, y = eaten, fill = item)) +
  geom_col() +
  scale_fill_viridis(discrete = TRUE) +
  facet_grid(pycnoID~.)

# barplot of prey sizes
cafeteria %>%
  ggplot(aes(x = item, y = size)) +
  geom_boxplot()


####
#<<<<<<<<<<<<<<<<<<<<<<<<<<END OF SCRIPT>>>>>>>>>>>>>>>>>>>>>>>>#

# SCRATCH PAD ####