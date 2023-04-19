#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#                                                                                ##
# Pycno wild diet figure for pycno preference MS                                 ##
# Script created: 2023-04-03                                                     ##
# Data source: Sarah Gravem/IUCN                                                 ##
# R code prepared by Ross Whippo                                                 ##
# Last updated: 2023-04-03                                                       ##
#                                                                                ##
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# SUMMARY:

# Creates figure of pycno wild diet proportions based on previous studies.


# Required Files (check that script is loading latest version):
# PycnoDiet.csv

# Associated Scripts:
# NONE

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

# 2023-04-03 Created script by extracting diet code from DirectPycnoMSfigure.R

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# LOAD PACKAGES                                                                ####
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

library(tidyverse)
library(viridis)
library(maps)
library(scatterpie)
library(data.table)
library(ggpubr)
library(ggh4x)

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# READ IN AND PREPARE DATA                                                     ####
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


PycnoDiet <- read_csv("Data/PycnoDiet.csv")
# create less groupings
PycnoDiet_step1 <- PycnoDiet %>%
  mutate(item = case_when(Item == "Balanus spp." ~ "Barnacles",
                          Item == "Barnacles" ~ "Barnacles",
                          Item == "Bivalves" ~ "Bivalves",
                          Item == "Bryozoans" ~ "Other",
                          Item == "Chitons" ~ "Other Molluscs",
                          Item == "Crabs" ~ "Crustaceans",
                          Item == "Crustaceans" ~ "Crustaceans",
                          Item == "Echinoderms" ~ "Other Echinoderms",
                          Item == "Fishes" ~ "Other",
                          Item == "Gastropods" ~ "Gastropods",
                          Item == "Holothurians" ~ "Other Echinoderms",
                          Item == "Hydroids" ~ "Other",
                          Item == "Molluscs Cephalopods" ~ "Other Molluscs",
                          Item == "Molluscs Gastropods" ~ "Gastropods",
                          Item == "Molluscs Pelecypoda" ~ "Bivalves",
                          Item == "Mytilus edulis" ~ "Bivalves",
                          Item == "Ophiuroids" ~ "Other Echinoderms",
                          Item == "Other" ~ "Other",
                          Item == "Other Echinoderms" ~ "Other Echinoderms",
                          Item == "Polychaetes" ~ "Other",
                          Item == "Salps" ~ "Other",
                          Item == "Sipunculids" ~ "Other",
                          Item == "Sponges" ~ "Other",
                          Item == "Unidentified" ~ "Other",
                          Item == "Urchins" ~ "Urchins"))
# create unique sample ID to parse sub- inter-tidal
PycnoDiet_step2 <- PycnoDiet_step1 %>%
  unite("sampleID", Location, Depth, sep = "_") %>%
  group_by(sampleID)
# add urchin in Bamfield samples to the exposed site
PycnoDiet_step2$sampleID <- PycnoDiet_step2$sampleID %>%
  recode("Bamfield_Subtidal" = "Bamfield_Subtidal Exposed")
# bring Bamfield values up to 100 percent with 'Other'
PycnoDiet_step2 <- ungroup(PycnoDiet_step2) %>%
  add_row(Region = c("British Columbia", "British Columbia", "British Columbia"),
          sampleID = c("Bamfield_Subtidal Exposed", "Bamfield_Subtidal Intermediate", "Bamfield_Subtidal Protected"),
          Lat = c(48.8262097, 48.8262097, 48.8262097),
          Lon = c(-125.1359127, -125.1359127, -125.1359127),
          item = c("Other", "Other", "Other"),
          N = c(67, 120, 45),
          value = c(27.2, 43.1, 42.3))
# adjust lat long to plot pies seperately
PycnoDiet_step3 <- PycnoDiet_step2 %>%
  mutate(new_long = case_when(sampleID == "Torch Bay_Subtidal" ~ Lon - 1.5,
                              sampleID == "Torch Bay_Intertidal" ~ Lon + 1.2,
                              sampleID == "Pacific Grove_Subtidal" ~ Lon - 3,
                              sampleID == "Bamfield_Subtidal Exposed" ~ Lon - 5,
                              sampleID == "Bamfield_Subtidal Intermediate" ~ Lon - 4.5,
                              sampleID == "Bamfield_Subtidal Protected" ~ Lon - 4,
                              sampleID == "Gabriola Island_Subtidal" ~ Lon - 1,
                              sampleID == "Prince William Sound_Intertidal" ~ Lon + 1.5,
                              sampleID == "Prince William Sound_Subtidal" ~ Lon - 1.5,
                              sampleID == "San Juan Islands_Subtidal Vadas" ~ Lon + 2,
                              sampleID == "San Juan Islands_Subtidal Mauzey" ~ Lon + 2.5,
                              sampleID == "Outer Coast_Intertidal Paine" ~ Lon - 3,
                              sampleID == "Outer Coast_Intertidal Dayton" ~ Lon - 1.5))
PycnoDiet_step4 <- PycnoDiet_step3 %>%
  mutate(new_lat = case_when(sampleID == "Torch Bay_Subtidal" ~ Lat - 2,
                             sampleID == "Torch Bay_Intertidal" ~ Lat - 3.5,
                             sampleID == "Pacific Grove_Subtidal" ~ Lat,
                             sampleID == "Bamfield_Subtidal Exposed" ~ Lat + 3.5,
                             sampleID == "Bamfield_Subtidal Intermediate" ~ Lat + 0.5,
                             sampleID == "Bamfield_Subtidal Protected" ~ Lat - 2.5,
                             sampleID == "Gabriola Island_Subtidal" ~ Lat + 2,
                             sampleID == "Prince William Sound_Intertidal" ~ Lat - 2,
                             sampleID == "Prince William Sound_Subtidal" ~ Lat - 2,
                             sampleID == "San Juan Islands_Subtidal Vadas" ~ Lat + 1.5,
                             sampleID == "San Juan Islands_Subtidal Mauzey" ~ Lat - 1.5,
                             sampleID == "Outer Coast_Intertidal Paine" ~ Lat - 5,
                             sampleID == "Outer Coast_Intertidal Dayton" ~ Lat - 7.5))
# summarise totals of each prey type per site 
PycnoDiet_step5 <- PycnoDiet_step4 %>%
  group_by(sampleID, item, new_lat, new_long, Lat, Lon, N) %>%
  summarise(value = sum(value))
# standardize all prey values to percent of total
PycnoDiet_step6 <- PycnoDiet_step5 %>%
  group_by(sampleID) %>%
  summarise(total = sum(value))
PycnoDiet_step7 <- PycnoDiet_step5 %>%
  left_join(PycnoDiet_step6, by = "sampleID")
PycnoDiet_step8 <- PycnoDiet_step7 %>%
  mutate(value = 100 * (value / total))
# round value column to 2 digits
PycnoDiet_step8$value <- round(PycnoDiet_step8$value, digits = 2)
# create subtidal intertidal column
PycnoDiet_step9 <- PycnoDiet_step8 %>%
  mutate(Depth = ifelse(sampleID %like% "Subtidal", "Subtidal", "Intertidal"))
# USE step 9 for FIGURE 1

# continue for FIGURE 2
PycnoDiet_step10 <- PycnoDiet_step9 %>%
  select(sampleID, item, Lat, N, value, Depth) %>%
  group_by(sampleID, Lat, Depth, item, N) %>%
  summarise(summed = sum(value)) %>%
  mutate(weighted = (summed * log10(N)))
PycnoDiet_step11 <- PycnoDiet_step10 %>%
  group_by(item, N, Depth) %>%
  summarise(grand_sum = sum(weighted)/245.8069)

# separate to compare AUC in JMP
PycnoDiet_step12 <- PycnoDiet_step10 %>%
  group_by(sampleID, item, N, Depth) %>%
  summarise(grand_sum = sum(weighted)/245.8069)

write_csv(PycnoDiet_step12, "WCF.csv")

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# DESCRIPTIVE STATS                                                            ####
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# n for each study, mean, sd
n_values <- PycnoDiet %>%
  select(Location, N, Depth) %>%
  distinct() %>%
  summarise(mean(N), sd(N))

# mean weighted values for each food type intertidal and subtidal

PycnoDiet_step11 %>%
  group_by(Depth, item) %>%
  summarise(mean(grand_sum), sd(grand_sum))


#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# DIET FIGURE                                                                  ####
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


# create values of N to add to the plot
annotations <- data.frame (N  = c("73^b", "425^b", "311^f", "162^f", "67^c", "120^c", "45^c", "7^d", "24^a", "102^a", "51^a", "93^a", "41^e"),
                           Lat = c(56, 56.1, 53.6, 52.3, 52.5, 49, 46, 53.9, 50, 47, 43, 40.5, 36.8),
                           Lon = c(-149, -146, -139, -135.8, -133, -132.9, -132, -125, -118, -117.3, -130.5, -129, -127.8))


world <- map_data("world")
NorthAm <- world %>%
  filter(region %in% c("USA", "Canada", "Mexico")) %>%
  filter(-158 < long & long < -110) %>%
  filter(25 < lat & lat < 75)
diet_plot <- ggplot(NorthAm, aes(long, lat)) +
  geom_map(map=NorthAm, aes(map_id=region), fill="grey80", color="grey25") +
  ylim(c(33,65)) +
  xlim(c(-153, -115)) +
  theme_bw() +
  geom_segment(data = PycnoDiet_step9,
               mapping = aes(x = new_long, xend = Lon, y = new_lat, yend = Lat),
               color = "black",
               size = 1) +
  geom_point(data = PycnoDiet_step9, aes(new_long, new_lat, color = Depth), size = 40) +
  guides(color = guide_legend(override.aes = list(size = 15))) +
  geom_scatterpie(data = PycnoDiet_step9,
                  aes(new_long, new_lat, r = 1.45),
                  cols = "item", long_format= TRUE) +
  scale_color_viridis(discrete = TRUE, begin = 0.4, end = 0.9, direction = -1) +
  scale_fill_viridis(discrete = TRUE, option = "H", begin = 0.2, name = "Prey Item") +
  theme(legend.key.size = unit(1, 'cm'),
        legend.title = element_text(size=20),
        legend.text = element_text(size=16)) +
  geom_text(data = annotations, aes(x = Lon, y = Lat, label = N), size = 10, parse = TRUE) +
  annotate("text", x = -120, y = 63, label = "N", size = 8) +
  annotate("segment", x = -120, xend = -120, y = 59, yend = 62, arrow=arrow(length=unit(0.2,"cm"))) +
  labs(x = "Longitude", y = "Latitude") +
  theme(axis.text=element_text(size=14),
        axis.title = element_text(size=20)) +
  coord_fixed()
diet_plot

# set for 1600 x 1450


# Weighted sums plot
strip_col_sub <- viridis(1, alpha = 1, begin = 0.4, end = 0.2, option = "D")
strip_col_inter <- viridis(1, alpha = 1, begin = 0.9, end = 0.9, option = "D")
# Only colour strips in x-direction
strip <- strip_themed(background_x = elem_list_rect(fill = c(strip_col_inter, strip_col_sub)))


weighted_sum <- ggplot(PycnoDiet_step11, aes(x = item, y = grand_sum, color = item)) +
  stat_summary(
  geom = "point",
   fun = "mean",
  size = 9,
  shape = 19
  ) +
  geom_errorbar(stat="summary", fun.data="mean_se", linewidth = 2) +
  scale_color_viridis(discrete = TRUE, option = "H", begin = 0.2, name = "Prey Item") +
  facet_wrap2(Depth~., strip= strip) +
  theme_bw() +
  xlab("") +
  ylab("Weighted Contribution") +
  theme(axis.title.y = element_text(size = 20)) + 
  theme(axis.text.y = element_text(size = 14)) +
  theme(strip.background = element_rect(fill = c(strip_col_inter, strip_col_sub)))+
  theme(axis.text.x = element_blank()) +
  theme(axis.ticks.x = element_blank()) +
  theme(strip.text.x = element_text(size = 20)) +
  ylim(c(0,1.000001)) +
  #theme(axis.text.x = element_text(angle = 270, hjust = 0, vjust = 0.25, size = 20)) +
  coord_fixed(ratio = 26)
weighted_sum




DietFigure <-ggarrange(diet_plot, NULL, weighted_sum,
                                 labels = c("A", "", "B"),
                       font.label=list(color="black",size=40),
                                 ncol = 3, nrow = 1,
                       widths = c(1, 0.05, 0.5),
                                 common.legend = TRUE,
                       legend = "right")
DietFigure

# 2500 x 1800

annotate_figure(FigureMDS, top = text_grob("2D stress = 0.12", size = 10))


############### SUBSECTION HERE

####
#<<<<<<<<<<<<<<<<<<<<<<<<<<END OF SCRIPT>>>>>>>>>>>>>>>>>>>>>>>>#