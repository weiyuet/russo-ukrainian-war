###########################
# 2022 Ukraine Russia War #
###########################

#### Setup ####
# Load Libraries
library(tidyverse)
library(scales)
library(glue)
library(paletteer)

#### Russian Casualties ####
# Load Data
personnel_destination_path <- "data/russia_losses_personnel.csv"

# Read local file
russia_losses_personnel <- read_csv(personnel_destination_path)

# Clean Data
# Data to tidy format
russia_losses_personnel_tidy <- russia_losses_personnel %>%
  select(-`personnel*`) %>%
  pivot_longer(cols = c(personnel, POW),
               names_to = "casualties",
               values_to = "value")

# Plot Data
# Create right side labels
label_data <- russia_losses_personnel_tidy %>% 
  filter(date == max(russia_losses_personnel_tidy$date))

# How many Russian casualties since the beginning of the war?
russia_losses_personnel_tidy %>%
  drop_na() %>%
  ggplot(aes(x = date,
             y = value,
             colour = casualties)) +
  geom_step(linewidth = 1.1) +
  geom_point(data = label_data,
             aes(x = date,
                 y = value),
             show.legend = FALSE) +
  scale_x_date(date_breaks = "3 months",
               labels = label_date_short()) +
  scale_y_log10(breaks = c(0,
                           10,
                           100,
                           1000,
                           10000,
                           100000,
                           1000000,
                           10000000),
                labels = label_number(big.mark = ",")) +
  scale_colour_paletteer_d("ggsci::default_jco",
                           labels = c("Personnel", "POWs\nnumbers not reported from 2022-04-27")) +
  annotate(geom = "text",
           x = as.Date(glue("{max(russia_losses_personnel$date)}")),
           y = max(russia_losses_personnel$personnel) - 500000,
           label = glue("{max(russia_losses_personnel$personnel)}"),
           size = 3) +
  labs(x = "",
       y = "",
       colour = "",
       title = glue("Russian Casualties (Day {max(russia_losses_personnel$day)}, updated {max(russia_losses_personnel$date)})"),
       caption = "Data: Armed Forces of Ukraine, Ministry of Defense of Ukraine | Graphic: @weiyuet") +
  theme_classic() +
  theme(legend.position = "inside",
        legend.position.inside = c(0.8, 0.6),
        plot.caption = element_text(hjust = 0))

# Save Image
ggsave("figures/russia-losses-personnel.png",
       width = 8,
       height = 5)

#### Russian Equipment Lost ####
# Load Data
equipment_destination_path <- "data/russia_losses_equipment.csv"

# Read local file
russia_losses_equipment <- read_csv(equipment_destination_path)

# Clean Data
# Data to tidy format
russia_losses_equipment_tidy <- russia_losses_equipment %>%
  select(-"greatest losses direction",
         -"military auto",
         -"fuel tank",
         -"mobile SRBM system") %>%
  pivot_longer(cols = 3:15,
               names_to = "equipment", 
               values_to = "value")

# Plot Data
# How many Russian equipment lost since the beginning of the war?
russia_losses_equipment_tidy %>%
  ggplot(aes(x = date,
             y = value)) +
  geom_step(colour = "black") +
  facet_wrap(vars(equipment),
             ncol = 3,
             scales = "free") +
  scale_x_date(date_breaks = "6 months", 
               labels = label_date_short()) +
  scale_y_continuous(labels = label_number(big.mark = "",
                                           accuracy = 1)) +
  labs(x = "",
       y = "",
       title = glue("Russian Equipment Lost (Day {max(russia_losses_equipment$day)}, updated {max(russia_losses_equipment$date)})"),
       caption = "Data: Armed Forces of Ukraine, Ministry of Defense of Ukraine | Graphic: @weiyuet") +
  theme_classic() +
  theme(plot.caption = element_text(hjust = 0))

# Save Image
ggsave("figures/russia-losses-equipment.png",
       width = 8,
       height = 8)

#### Russian Equipment Lost Cumulatively ####
# How many Russian equipment lost cumulatively since the beginning of the war?
russia_losses_equipment_tidy %>%
  group_by(equipment) %>%
  summarize(cumulative_total = max(value, na.rm = TRUE)) %>%
  mutate(equipment = fct_reorder(equipment, cumulative_total)) %>%
  ggplot(aes(x = cumulative_total,
             y = equipment)) +
  geom_col(colour = "black",
           fill = "gray40") +
  scale_x_continuous(labels = label_number(big.mark = ","),
                     breaks = seq(0, 100000, 10000),
                     expand = c(0.01, 0)) +
  labs(x = "",
       y = "",
       title = glue("Russian Equipment Lost - Cumulative (Day {max(russia_losses_equipment$day)}, updated {max(russia_losses_equipment$date)})"),
       caption = "Data: Armed Forces of Ukraine, Ministry of Defense of Ukraine | Graphic: @weiyuet") +
  theme_classic() +
  theme(plot.caption = element_text(hjust = 0))

# Save Image
ggsave("figures/russia-losses-equipment-cumulative.png",
       width = 8,
       height = 8)

# End