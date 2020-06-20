# Libraries ---------------------------------------------------------------

library(infer)
library(tidyquant)
library(tidyverse)
library(usmap)

# Data --------------------------------------------------------------------

elec_results <-
  read.csv("https://raw.githubusercontent.com/tonmcg/US_County_Level_Election_Results_08-16/master/2016_US_County_Level_Presidential_Results.csv")
bigfoot <-
  read.csv(
    "data/bfro_reports_geocoded.csv"
  )

# Setting up some extra elements
usa <- map_data("usa")
states = map_data('state')

# Data Cleaning -----------------------------------------------------------

# Preparing the data to be joined

county_winner_tbl <- elec_results %>%
  as_tibble() %>%
  filter(state_abbr != "AK") %>%
  mutate(
    lead = ifelse(per_gop > per_dem, "Donald Trump", "Hillary Clinton"),
    state = openintro::abbr2state(state_abbr)
  ) %>%
  select(state, county = county_name, lead)

bigfoot_simple_tbl <- bigfoot %>%
  as_tibble() %>%
  mutate(date = lubridate::ymd(date)) %>%
  filter(date <= "2016-11-08") %>%
  select(county, state, latitude, longitude)

# Joining the two datasets

sightings_by_county_tbl <- county_winner_tbl %>%
  left_join(bigfoot_simple_tbl %>%
              group_by(state, county) %>%
              count(name = "sightings"),
            by = c("state", "county")) %>%
  mutate(sightings = replace_na(sightings, 0))

# Are Bigfoot sightings a significant predictor of county-level election results? ----------------------------------------------

# Number of counties per candidate
sightings_by_county_tbl %>%
  count(lead) %>%
  mutate(label = scales::comma(n)) %>%
  ggplot(aes(x = lead, y = n, fill = lead, label = label)) +
  geom_col() +
  geom_label(fill = "white") +
  theme_tq() +
  scale_y_continuous(labels = scales::comma) +
  scale_fill_tq() +
  labs(
    title = "Trump won far more counties than Clinton",
    x = "",
    y = "Counties Won"
  ) +
  theme(legend.position = "none")

# What proportion of those counties had a Bigfoot sighting (map)
sightings_by_county_tbl %>%
  mutate(sightings_bin = ifelse(sightings >= 1, 1, 0)) %>%
  group_by(lead) %>%
  summarize(prop = mean(sightings_bin)) %>%
  mutate(label = scales::percent(prop)) %>%
  ggplot(aes(x = lead, y = prop, fill = lead, label = label)) +
  geom_col() +
  geom_label(fill = "white") +
  theme_tq() +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_tq() +
  labs(
    title = "Both candidates won a similar proportion of counties with Bigfoot sightings",
    x = "",
    y = "Counties with Bigfoot Sightings (%)"
  ) +
  theme(legend.position = "none")

# Is there a difference in the average number of Bigfoot sightings per county between the candidates
sightings_by_county_tbl %>%
  group_by(lead) %>%
  summarize(avg_sightings = mean(sightings)) %>%
  mutate(label = scales::number(avg_sightings, accuracy = 0.1)) %>%
  ggplot(aes(x = lead, y = avg_sightings, fill = lead, label = label)) +
  geom_col() +
  geom_label(fill = "white") +
  theme_tq() +
  scale_fill_tq() +
  labs(
    title = "Clinton's counties had more Bigfoot sightings on average than Trump's",
    x = "",
    y = "Average Number of Sightings"
  ) +
  theme(legend.position = "none")

# Statistical Tests -------------------------------------------------------

# Is there a significant difference in the proportion of counties with Bigfoot sightings won by the two candidates?

sightings_binary_tbl <- sightings_by_county_tbl %>%
  mutate(sightings_bin = ifelse(sightings >= 1, 1, 0)) %>%
  select(county, lead, sightings_bin)

bin_t_test_tbl <- sightings_binary_tbl %>%
  t_test(sightings_bin ~ lead, order = c("Donald Trump", "Hillary Clinton"))

obs_diff_bin <- sightings_binary_tbl %>%
  specify(sightings_bin ~ lead) %>%
  calculate(stat = "diff in means", order = c("Donald Trump", "Hillary Clinton"))

null_perm_bin <- sightings_binary_tbl %>%
  specify(sightings_bin ~ lead) %>%
  hypothesize(null = "independence") %>%
  generate(reps = 1000, type = "permute") %>%
  calculate(stat = "diff in means", order = c("Donald Trump", "Hillary Clinton"))

visualize(null_perm_bin) +
  shade_p_value(
    obs_stat = obs_diff_bin,
    direction = "two_sided",
    color = palette_dark()[1],
    fill = palette_dark()[2]
  ) +
  theme_tq() +
  labs(
    title = "Too many of the permuted differences are as or more extreme than the observed difference",
    subtitle = "Translation: We fail to reject the null hypothesis",
    x = "Permuted Difference In Proportions of Counties",
    y = "Count"
  )

# Is there a significant difference in the average number of Bigfoot sightings per county between the candidates?

sightings_count_tbl <- sightings_by_county_tbl %>%
  select(county, winner, sightings)

t_test_tbl <- sightings_count_tbl %>%
  t_test(sightings ~ winner, order = c("Donald Trump", "Hillary Clinton"))

obs_diff <- sightings_count_tbl %>%
  specify(sightings ~ lead) %>%
  calculate(stat = "diff in means", order = c("Donald Trump", "Hillary Clinton"))

null_perm <- sightings_count_tbl %>%
  specify(sightings ~ lead) %>%
  hypothesize(null = "independence") %>%
  generate(reps = 1000, type = "permute") %>%
  calculate(stat = "diff in means", order = c("Donald Trump", "Hillary Clinton"))

visualize(null_perm) +
  shade_p_value(
    obs_stat = obs_diff,
    direction = "two_sided",
    color = palette_dark()[1],
    fill = palette_dark()[2]
  ) +
  theme_tq() +
  labs(
    title = "Too many of the permuted differences are as or more extreme than the observed difference",
    subtitle = "Translation: We fail to reject the null hypothesis",
    x = "Permuted Difference In Bigfoot Sightings per County",
    y = "Count"
  )

# Plots ----

# Getting sighting coordinates

lat_long_tbl <- county_winner_tbl %>%
  left_join(bigfoot_simple_tbl,
            by = c("state", "county"))

# Plotting the Bigfoot sightings

lat_long_tbl %>%
  filter(longitude > -135) %>%
  ggplot(aes(x = longitude, y = latitude, color = winner)) +
  geom_polygon(
    data = states,
    aes(x = long, y = lat, group = group),
    fill = NA,
    color = 'grey',
    show.legend = FALSE
  ) +
  geom_jitter(alpha = .5) +
  theme_tq() +
  scale_color_tq() +
  labs(title = "Bigfoot Sightings (1869-2017)",
       x = "",
       y = "",
       color = "") +
  theme(
    axis.text = element_blank(),
    legend.text = element_text(size = 15),
    line = element_blank(),
    plot.title = element_text(hjust = .5),
    plot.subtitle = element_text(hjust = .5),
    rect = element_blank()
  )
