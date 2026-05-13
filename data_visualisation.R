library(tidyverse)
library(jsonlite)

# Define colours and theme
my_colours <- c("#FCE4EC","#F8BBD0","#F48FB1","#EC407A","#AD1457","#FFF7F0")

my_theme <- theme_minimal(base_size = 13) +
  theme(plot.background = element_rect(fill = my_colours[6], colour = NA),
        panel.background = element_rect(fill = my_colours[6], colour = NA),
        panel.grid.minor = element_blank(),
        plot.title = element_text(colour = my_colours[5], face = "bold", size = 16),
        axis.title = element_text(colour = my_colours[5]),
        axis.text = element_text(colour = my_colours[5]),
        legend.title = element_text(colour = my_colours[5], face = "bold"),
        legend.text = element_text(colour = my_colours[5]))


logged_data <- read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vRN9AF-pSgnJVhgvE2_20Mk6fSXnUhEzyTjzqnUq6ZtqNPVDSoa6JLDELfHgluUU5zzZ4SFrSbGOiKw/pub?output=csv") %>%
  rename(timestamp = 1,
         main_color = 2,
         location = 3,
         clothing_type = 4,
         visible_clothing_count = 5,
         weather = 6,
         sleeve_length = 7,
         time_period = 8)

names(logged_data)

# create plot 1: clothing_type * weather 
clothing_weather_summarised <- logged_data %>%
  mutate(clothing_type = str_to_title(clothing_type),
         weather = str_to_title(weather)) %>%
  filter(!is.na(clothing_type), !is.na(weather)) %>%
  group_by(clothing_type, weather) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(clothing_type) %>%
  mutate(total = sum(n)) %>%
  ungroup()

plot1 <- clothing_weather_summarised %>%
  ggplot(aes(y = reorder(clothing_type, total),x = n,fill = weather)) +
  geom_col() +
  scale_fill_manual(values = my_colours[1:5]) +
  labs(title = "Weather Conditions by Clothing Type",
       x = "Number of observations",
       y = "Clothing type",
       fill = "Weather condition") +
  my_theme
plot1

# create plot 2: main_color * time_period
color_time_summarised <- logged_data %>%
  mutate(main_color = str_to_title(main_color),
         time_period = str_to_title(time_period)) %>%
  filter(!is.na(main_color), !is.na(time_period)) %>%
  group_by(main_color, time_period) %>%
  summarize(n = n(), .groups = "drop") %>%
  group_by(main_color) %>%
  mutate(total = sum(n)) %>%
  ungroup()

plot2 <- color_time_summarised %>%
  ggplot(aes(x = time_period,y = reorder(main_color, total),size = n,fill = n)) +
  geom_point(shape = 21,alpha = 0.85,colour = my_colours[5],stroke = 1) +
  scale_fill_gradient(low = my_colours[1],high = my_colours[4]) +
  labs(title = "Main Clothing Color by Time Period",
       x = "Time period",
       y = "Main clothing color",
       size = "Number of observations",
       fill = "Number of observations") +
  my_theme
plot2

# create plot 3: weather * sleeve_length
weather_sleeve_summarised <- logged_data %>%
  mutate(weather = str_to_title(weather),
         sleeve_length = str_to_title(sleeve_length)) %>%
  filter(!is.na(weather), !is.na(sleeve_length)) %>%
  group_by(weather, sleeve_length) %>%
  summarize(n = n(), .groups = "drop")

plot3 <- weather_sleeve_summarised %>%
  ggplot(aes(x = weather,y = n,fill = sleeve_length)) +
  geom_col(position = "dodge") +
  scale_fill_manual(values = my_colours[1:5]) +
  scale_y_continuous(breaks = seq(0, max(weather_sleeve_summarised$n), by = 1)) +
  labs(title = "Sleeve Length by Weather Condition",
       x = "Weather condition",
       y = "Number of observations",
       fill = "Sleeve length") +
  my_theme
plot3

# create plot 4: timestamp * sleeve length

plot4 <- logged_data %>%
  mutate(timestamp = dmy_hms(timestamp),
         observation_hour = hour(timestamp) + minute(timestamp) / 60,
         sleeve_length = str_to_title(sleeve_length)) %>%
  filter(!is.na(observation_hour), !is.na(sleeve_length)) %>%
  ggplot(aes(x = observation_hour,
             y = sleeve_length,
             colour = sleeve_length)) +
  geom_boxplot(fill = "transparent",
               coef = 1000) +
  geom_jitter(height = 0.15,alpha = 0.35) +
  guides(colour = "none") +
  scale_x_continuous(limits = c(0, 24),breaks = seq(0, 24, 1)) +
  labs(title = "Observation time by sleeve length",
       x = "Hour of observation",
       y = "Sleeve length") +
  my_theme


plot4_data <-logged_data %>%
  mutate(timestamp = dmy_hms(timestamp),
         observation_hour = hour(timestamp) + minute(timestamp) / 60,
         sleeve_length = str_to_title(sleeve_length)) %>%
  filter(!is.na(observation_hour), !is.na(sleeve_length))

plot4 <- plot4_data %>%
  ggplot(aes(x = observation_hour,
             y = sleeve_length,
             colour = sleeve_length)) +
  geom_boxplot(fill = "transparent",coef = 100,outlier.shape = NA) +
  geom_jitter(height = 0.15,alpha = 0.35) +
  scale_colour_manual(values = my_colours[3:5]) +
  guides(colour = "none") +
  scale_x_continuous(limits = c(0, 24),breaks = seq(0, 24, 1)) +
  labs(title = "Observation Time by Sleeve Length",
       x = "Hour of observation",
       y = "Sleeve length") +
  my_theme

plot4

ggsave("plot1.png", plot1, width = 8, height = 5)
ggsave("plot2.png", plot2, width = 8, height = 5)
ggsave("plot3.png", plot3, width = 8, height = 5)
ggsave("plot4.png", plot4, width = 8, height = 5)
