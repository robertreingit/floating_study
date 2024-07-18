# libs
library(tidyverse)
library(patchwork)
library(summarytools)

# load data
float <- read_delim('../data/LotteryHealthFloatin_DATA_LABELS_2024-03-07_1024.csv')

names(float)[50] <- "d"
names(float)[59] <- "vo2_180"
names(float)[52] <- "vc"
names(float)[13] <- 'height'
float <- float |>
  mutate(d = as.numeric(d),
         age_f = cut_interval(Age, 2),
         height = if_else(height > 3, height/100, height))

float |> select(Age, Sex) |> group_by(Sex) |>
  descr(stats=c('mean','med','sd','n.valid'))

float_dummy <- float |> select(d, age_f, Sex, vo2_180) |> na.omit()  

float_dummy |> 
ggplot(aes(d)) +
  geom_histogram() +
  geom_density() +
  facet_grid(age_f~Sex)

float_dummy |> 
  ggplot(aes(d,vo2_180)) +
  geom_smooth(method = MASS::rlm) +
  geom_point() +
  facet_grid(age_f~Sex)

table(float$`Primary Ethnicity`)

ggplot(float, aes(Age)) + 
  geom_histogram() +
  facet_grid(~Sex)

ggplot(float, aes(USG)) + 
  geom_histogram() +
  geom_vline(xintercept = 1.015, color = 'red', linetype = 'dashed') 

ggplot(float, aes(vc)) +
  geom_histogram() +
  facet_grid(age_f~Sex)

ggplot(float, aes(Age, vc, color=Sex)) +
  geom_point() +
  geom_smooth()
