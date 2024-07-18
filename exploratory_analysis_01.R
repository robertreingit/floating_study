# libs
library(tidyverse)
library(patchwork)

# load data
float <- read_delim('../data/LotteryHealthFloatin_DATA_LABELS_2024-03-07_1024.csv')

# clean and process data
vo2 <- float |> select('Record ID', 'Age', starts_with('Floating'))
names(vo2) <- c('id','age',str_replace(names(vo2)[-1], 'Floating VO2 ','vo2_'))
names(vo2)[-(1:2)] <- str_split_i(names(vo2)[-(1:2)],'-',1) 
head(vo2)

vo2_norm <- vo2 |> pivot_longer(cols = starts_with('vo2'))
vo2_norm <- vo2_norm |> group_by(id) |>
  mutate(value = value - value[1],
         vo2_perf = mean(value),
         vo2_sd = sd(value)) |> 
  ungroup() |> 
  mutate(perf_f = cut_interval(vo2_perf, 6),
         vo2_sd_f = cut_interval(vo2_sd, 6))
head(vo2_norm,20)
vo2_norm <- vo2_norm |>
  mutate(age_f = cut_width(age, 10),
         id = paste0('P',id),
         time = as.numeric(str_split_i(name,'_',2)))

# plot data
p1 <- ggplot(vo2_norm, aes(time,value)) +
  geom_point() +
  geom_line(aes(color=id)) + 
  geom_smooth() +
  facet_grid(~age_f) +
  guides(color = "none")

p2 <- ggplot(vo2_norm, aes(time,value)) +
  geom_point() +
  geom_line(aes(color=id)) + 
  geom_smooth() +
  facet_grid(~perf_f) +
  guides(color = "none")
p2
p3 <- ggplot(vo2_norm, aes(time,value)) +
  geom_point() +
  geom_line(aes(color=id)) + 
  geom_smooth() +
  facet_grid(~vo2_sd_f) +
  guides(color = "none")
p3
p1 /p2 / p3
