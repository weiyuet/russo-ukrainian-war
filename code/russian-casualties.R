###########################
# 2022 Ukraine Russia War #
###########################

#### Setup ####
# Load Libraries
library(tidyverse)
library(scales)
library(glue)
library(paletteer)
library(jsonlite)

#### Russian Casualties ####
# Load Data
russia_losses_personnel_url <- "https://raw.githubusercontent.com/PetroIvaniuk/2022-Ukraine-Russia-War-Dataset/refs/heads/main/data/russia_losses_personnel.json"

russia_losses_personnel_raw <- russia_losses_personnel_url %>%
       fromJSON(simplifyDataFrame = TRUE) %>%
       as_tibble()

# Prep Data
# Convert data to appropriate formats and make into tidy format
russia_losses_personnel_clean <- russia_losses_personnel_raw %>%
       mutate(
              date = ymd(date),
              across(.cols = c(day, personnel, POW), .fns = as.numeric)
       ) %>%
       select(-`personnel*`) %>%
       pivot_longer(
              cols = c(personnel, POW),
              names_to = "casualties",
              values_to = "value"
       )

# Plot Data
# Create right side labels
label_data <- russia_losses_personnel_clean %>%
       drop_na() %>%
       filter(date == max(russia_losses_personnel_clean$date))

formatted_plot_number <- format(
       max(label_data$value),
       big.mark = ",",
       scientific = FALSE
)

# How many Russian casualties since the beginning of the war?
russia_losses_personnel_clean %>%
       drop_na() %>%
       ggplot(aes(x = date, y = value, colour = casualties)) +
       geom_step(linewidth = 1.1) +
       geom_point(
              data = label_data,
              aes(x = date, y = value),
              show.legend = FALSE
       ) +
       scale_x_date(date_breaks = "3 months", labels = label_date_short()) +
       scale_y_log10(
              breaks = c(0, 10, 100, 1000, 10000, 100000, 1000000, 10000000),
              labels = label_number(big.mark = ",")
       ) +
       scale_colour_paletteer_d(
              "ggsci::default_jco",
              labels = c(
                     "Personnel",
                     "POWs\nnumbers not reported from 2022-04-27"
              )
       ) +
       annotate(
              geom = "text",
              x = as.Date(glue("{max(label_data$date)}")),
              y = max(label_data$value) - 500000,
              label = formatted_plot_number,
              size = 11,
              size.unit = "pt"
       ) +
       labs(
              title = glue(
                     "Russian Casualties (Day {max(russia_losses_personnel_clean$day)}, updated {max(russia_losses_personnel_clean$date)})"
              ),
              caption = "Data: Armed Forces of Ukraine, Ministry of Defense of Ukraine | Graphic: @weiyuet"
       ) +
       theme_classic() +
       theme(
              axis.title = element_blank(),
              legend.title = element_blank(),
              legend.position = "inside",
              legend.position.inside = c(0.8, 0.6),
              plot.caption = element_text(hjust = 0)
       )

# Save Image
ggsave("figures/russia-losses-personnel.png", width = 8, height = 5)

#### Russian Equipment Lost ####
# Load Data
russia_losses_equipment_url <- "https://raw.githubusercontent.com/PetroIvaniuk/2022-Ukraine-Russia-War-Dataset/refs/heads/main/data/russia_losses_equipment.json"

russia_losses_equipment_raw <- russia_losses_equipment_url %>%
       fromJSON(simplifyDataFrame = TRUE) %>%
       as_tibble()

# Prep Data
# Convert data to appropriate formats and make into tidy format
russia_losses_equipment_clean <- russia_losses_equipment_raw %>%
       mutate(date = ymd(date), across(.cols = c(2:19), .fns = as.numeric)) %>%
       select(
              -"greatest losses direction",
              -"military auto",
              -"fuel tank",
              -"mobile SRBM system"
       ) %>%
       pivot_longer(cols = 3:15, names_to = "equipment", values_to = "value")

# Plot Data
# How many Russian equipment lost since the beginning of the war?
russia_losses_equipment_clean %>%
       ggplot(aes(x = date, y = value)) +
       geom_step(colour = "black") +
       facet_wrap(vars(equipment), ncol = 3, scales = "free") +
       scale_x_date(date_breaks = "6 months", labels = label_date_short()) +
       scale_y_continuous(labels = label_number(big.mark = "", accuracy = 1)) +
       labs(
              title = glue(
                     "Russian Equipment Lost (Day {max(russia_losses_equipment_clean$day)}, updated {max(russia_losses_equipment_clean$date)})"
              ),
              caption = "Data: Armed Forces of Ukraine, Ministry of Defense of Ukraine | Graphic: @weiyuet"
       ) +
       theme_classic() +
       theme(
              axis.title = element_blank(),
              plot.caption = element_text(hjust = 0)
       )

# Save Image
ggsave("figures/russia-losses-equipment.png", width = 8, height = 8)

#### Russian Equipment Lost Cumulatively ####
# How many Russian equipment lost cumulatively since the beginning of the war?
russia_losses_equipment_clean %>%
       group_by(equipment) %>%
       summarize(cumulative_total = max(value, na.rm = TRUE)) %>%
       mutate(equipment = fct_reorder(equipment, cumulative_total)) %>%
       ggplot(aes(x = cumulative_total, y = equipment)) +
       geom_col(colour = "black", fill = "gray40") +
       scale_x_continuous(
              labels = label_number(big.mark = ","),
              breaks = seq(0, 100000, 10000),
              expand = c(0, 0.05)
       ) +
       labs(
              title = glue(
                     "Russian Equipment Lost - Cumulative (Day {max(russia_losses_equipment_clean$day)}, updated {max(russia_losses_equipment_clean$date)})"
              ),
              caption = "Data: Armed Forces of Ukraine, Ministry of Defense of Ukraine | Graphic: @weiyuet"
       ) +
       theme_classic() +
       theme(
              axis.title = element_blank(),
              plot.caption = element_text(hjust = 0)
       )

# Save Image
ggsave("figures/russia-losses-equipment-cumulative.png", width = 8, height = 8)

# End
