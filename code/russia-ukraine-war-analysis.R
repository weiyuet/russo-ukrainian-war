####################################
# 2022 Russia-Ukraine War Analysis #
####################################

# 1.0 Setup ----
# 1.1 Load Libraries
library(tidyverse)
library(jsonlite)
library(scales)
library(glue)
library(zoo)
library(rstanarm)
library(bayesplot)

# 1.2 Shared plotting configurations
base_theme <- function() {
        theme_minimal(base_family = "sans") +
                theme(
                        plot.title = element_text(face = "bold", size = 16),
                        plot.subtitle = element_text(
                                color = "gray30",
                                size = 11,
                                lineheight = 1.2
                        ),
                        panel.grid.minor = element_blank(),
                        plot.caption = element_text(
                                color = "gray50",
                                hjust = 0,
                                size = 9
                        ),
                        legend.position = "top",
                        legend.title = element_text(face = "bold")
                )
}

shared_caption <- "Data: Armed Forces of Ukraine | Graphic: @weiyuet"

# 2.0 Russian Casualties & Analysis ----
# 2.1 Load casualty data
casualties_url <- "https://raw.githubusercontent.com/PetroIvaniuk/2022-Ukraine-Russia-War-Dataset/refs/heads/main/data/russia_losses_personnel.json"

# Type conversion and cleaning
casualties_df <- casualties_url %>%
        fromJSON(simplifyDataFrame = TRUE) %>%
        as_tibble() %>%
        mutate(date = ymd(date), across(c(day, personnel, POW), as.numeric)) %>%
        select(date, day, personnel, POW) %>%
        arrange(date) %>%
        mutate(
                daily_personnel = personnel -
                        lag(personnel, default = first(personnel)),
                personnel_7d_avg = rollmean(
                        daily_personnel,
                        k = 7,
                        fill = NA,
                        align = "right"
                )
        )

# Exploratory plot
plot_casualty_exploratory <- casualties_df %>%
        ggplot(aes(x = date, y = personnel)) +
        geom_area(fill = "#D62728", alpha = 0.15) +
        geom_line(color = "#D62728", linewidth = 1.2) +
        scale_x_date(date_breaks = "6 months") +
        scale_y_continuous(labels = label_number(big.mark = ",")) +
        labs(
                title = "Total Reported Russian Casualties from 2022",
                x = NULL,
                y = NULL,
                caption = shared_caption
        ) +
        base_theme()

print(plot_casualty_exploratory)

# 3.0 Plot 1 Velocity of Daily Casualties ----
# Extract latest date for titles
latest_date <- casualties_df %>% pull(date) %>% max(na.rm = TRUE)
latest_day <- casualties_df %>% pull(day) %>% max(na.rm = TRUE)

# Isolate latest data point for plot label
latest_casualty_metric <- casualties_df %>%
        drop_na(personnel_7d_avg) %>%
        slice_max(order_by = date, n = 1)

plot_conflict_intensity <- casualties_df %>%
        drop_na(personnel_7d_avg) %>%
        ggplot(aes(x = date)) +
        geom_col(aes(y = daily_personnel), fill = "gray40", alpha = 0.5) +
        geom_line(
                aes(y = personnel_7d_avg),
                color = "#D62728",
                linewidth = 1.2
        ) +
        geom_point(
                data = latest_casualty_metric,
                aes(y = personnel_7d_avg),
                color = "#D62728",
                size = 3
        ) +
        geom_text(
                data = latest_casualty_metric,
                aes(
                        y = personnel_7d_avg,
                        label = comma(round(personnel_7d_avg))
                ),
                vjust = -2,
                hjust = 0.5,
                color = "#D62728",
                fontface = "bold",
                size = 4.5
        ) +
        scale_x_date(date_breaks = "3 months", labels = label_date_short()) +
        scale_y_continuous(
                breaks = seq(0, 3000, 500),
                labels = label_number(big.mark = ",")
        ) +
        labs(
                title = "Intensity of Conflict: Russian Personnel Casualties",
                subtitle = glue(
                        "Daily reported casualties (gray bars) with 7-day rolling average (red line). (Day {latest_day}, updated {latest_date})"
                ),
                x = NULL,
                y = NULL,
                caption = shared_caption
        ) +
        base_theme()

print(plot_conflict_intensity)

# 4.0 Plot 2 Bayesian Analysis of Casualty Rates ----
# 4.1 Isolate the last 90 days of casualty data
recent_90d_df <- casualties_df %>%
        slice_max(order_by = date, n = 90) %>%
        drop_na(daily_personnel)

# 4.2 Fit Bayesian Intercept-Only Model (Estimating the Mean)
bayes_casualty_model <- stan_glm(
        daily_personnel ~ 1,
        data = recent_90d_df,
        family = gaussian(),
        prior_intercept = normal(
                location = 800,
                scale = 400,
                autoscale = FALSE
        ),
        chains = 4,
        iter = 2000,
        seed = 123,
        refresh = 0
)

# 4.3 Extract the 95% Credible Interval and Estimated Mean for chart subtitle
casualty_summary <- posterior_interval(bayes_casualty_model, prob = 0.95)
print(casualty_summary)

casualty_ci_lower <- round(casualty_summary[1, 1])
casualty_ci_upper <- round(casualty_summary[1, 2])
casualty_mean <- round(mean(as.matrix(bayes_casualty_model)))

bayes_casualty_subtitle <-
        glue(
                "Over the last 90 days, the estimated true mean is {(casualty_mean)} casualties per day.\n",
                "95% confident that the underlying average falls between {(casualty_ci_lower)} and {(casualty_ci_upper)}. (Day {latest_day}, updated {latest_date})"
        )

# 4.4 Visualize the Posterior Distribution
plot_bayes_casualties <- mcmc_areas(
        as.matrix(bayes_casualty_model),
        pars = "(Intercept)",
        prob = 0.95,
        point_est = "mean"
) +
        labs(
                title = "Bayesian Estimate of Daily Russian Casualties",
                subtitle = bayes_casualty_subtitle,
                x = "Estimated Mean Daily Casualties",
                y = NULL,
                caption = shared_caption
        ) +
        base_theme() +
        theme(
                axis.text.y = element_blank(),
                panel.grid.major.y = element_blank()
        )

print(plot_bayes_casualties)

# 5.0 Equipment Losses & Analysis ----
# 5.1 Load equipment data
equipment_url <- "https://raw.githubusercontent.com/PetroIvaniuk/2022-Ukraine-Russia-War-Dataset/refs/heads/main/data/russia_losses_equipment.json"

equipment_df <- equipment_url %>%
        fromJSON(simplifyDataFrame = TRUE) %>%
        as_tibble() %>%
        mutate(date = ymd(date), across(2:19, as.numeric)) %>%
        select(
                -`greatest losses direction`,
                -`military auto`,
                -`fuel tank`,
                -`mobile SRBM system`
        ) %>%
        pivot_longer(
                cols = -c(date, day),
                names_to = "equipment",
                values_to = "cumulative"
        ) %>%
        arrange(equipment, date) %>%
        group_by(equipment) %>%
        mutate(
                daily_loss = cumulative -
                        lag(cumulative, default = first(cumulative)),
                loss_7d_avg = rollmean(
                        daily_loss,
                        k = 7,
                        fill = NA,
                        align = "right"
                )
        ) %>%
        ungroup() %>%
        mutate(
                category = case_when(
                        equipment %in% c("tank", "APC") ~ "Armor",
                        equipment %in%
                                c("field artillery", "MRL") ~ "Artillery",
                        equipment %in% c("aircraft", "helicopter") ~ "Air",
                        equipment %in% c("naval ship", "submarine") ~ "Sea",
                        equipment %in% c("drone") ~ "Drone",
                        TRUE ~ "Support/Other"
                )
        )

# Exploratory plot
plot_equipment_exploratory <- equipment_df %>%
        ggplot(aes(x = date, y = cumulative)) +
        geom_line(color = "#1F77B4", linewidth = 1) +
        facet_wrap(vars(str_to_title(equipment)), scales = "free_y", ncol = 4) +
        scale_x_date(date_breaks = "1 year", labels = label_date_short()) +
        scale_y_continuous(
                labels = label_number(big.mark = ",", accuracy = 1)
        ) +
        labs(
                title = "Total Reported Russian Equipment Losses from 2022",
                x = NULL,
                y = NULL,
                caption = shared_caption
        ) +
        base_theme() +
        theme(
                legend.position = "none",
                strip.text = element_text(face = "bold", size = 10),
                panel.spacing = unit(1, "lines"),
                axis.text.x = element_text(size = 8)
        )

print(plot_equipment_exploratory)

# 6.0 Plot 3 Shifting War Dynamics (Armor vs Artillery vs Drones) ----
# 6.1 Calculating 7-day average losses by Category
category_trend_df <- equipment_df %>%
        filter(category %in% c("Armor", "Artillery", "Drone")) %>%
        group_by(date, category) %>%
        summarize(
                daily_category_loss = sum(daily_loss, na.rm = TRUE),
                .groups = "drop"
        ) %>%
        group_by(category) %>%
        mutate(
                category_7d_avg = rollmean(
                        daily_category_loss,
                        k = 7,
                        fill = NA,
                        align = "right"
                )
        ) %>%
        ungroup() %>%
        drop_na(category_7d_avg)

plot_equipment_shift <- category_trend_df %>%
        ggplot(aes(x = date, y = category_7d_avg, color = category)) +
        geom_line(linewidth = 1.2) +
        scale_x_date(date_breaks = "4 months", labels = label_date_short()) +
        scale_y_log10(labels = label_number(big.mark = ",")) +
        scale_color_manual(
                values = c(
                        "Armor" = "#1F77B4",
                        "Artillery" = "#D62728",
                        "Drone" = "#FF7F0E"
                )
        ) +
        labs(
                title = "The Evolution of Warfare: Armor, Artillery, and Drones",
                subtitle = glue(
                        "7-day rolling average daily losses (Log Scale). Shift: Early Armor losses -> Mid-war Artillery dominance -> Late-war Drone parabolics.\n(Day {latest_day}, updated {latest_date})"
                ),
                x = NULL,
                y = NULL,
                color = "Equipment",
                caption = shared_caption
        ) +
        base_theme()

print(plot_equipment_shift)

# 7.0 Plot 4 Bayesian Analysis of Artillery Burn Rate ----
# 7.1 Isolate last 90 days of field artillery data
artillery_90d_df <- equipment_df %>%
        filter(equipment == "field artillery") %>%
        slice_max(order_by = date, n = 90) %>%
        drop_na(daily_loss)

# 7.2 Fit Bayesian Model
bayes_artillery_model <- stan_glm(
        daily_loss ~ 1,
        data = artillery_90d_df,
        family = gaussian(),
        prior_intercept = normal(location = 60, scale = 40, autoscale = FALSE),
        chains = 4,
        iter = 2000,
        seed = 123,
        refresh = 0
)

# 7.3 Extract Intervals and Mean for subtitle
artillery_summary <- posterior_interval(bayes_artillery_model, prob = 0.95)
print(artillery_summary)

artillery_ci_lower <- round(artillery_summary[1, 1], 1)
artillery_ci_upper <- round(artillery_summary[1, 2], 1)
artillery_mean <- round(mean(as.matrix(bayes_artillery_model)), 1)

bayes_artillery_subtitle <- glue(
        "Over the last 90 days, the estimated true mean is {artillery_mean} artillery pieces lost per day.\n",
        "95% confident the underlying average falls between {artillery_ci_lower} and {artillery_ci_upper}. (Day {latest_day}, updated {latest_date})"
)

# 7.4 Visualize the Posterior Distribution
plot_bayes_artillery <- mcmc_areas(
        as.matrix(bayes_artillery_model),
        pars = "(Intercept)",
        prob = 0.95,
        point_est = "mean"
) +
        labs(
                title = "Bayesian Estimate of Daily Russian Artillery Losses",
                subtitle = bayes_artillery_subtitle,
                x = "Estimated Mean Daily Artillery Losses",
                y = NULL,
                caption = shared_caption
        ) +
        base_theme() +
        theme(
                axis.text.y = element_blank(),
                panel.grid.major.y = element_blank()
        )

print(plot_bayes_artillery)

# 8.0 Export & Save Plots ----
all_plots <- list(
        "01-casualty-exploratory" = plot_casualty_exploratory,
        "02-equipment-exploratory" = plot_equipment_exploratory,
        "03-conflict-intensity" = plot_conflict_intensity,
        "04-bayes-casualty" = plot_bayes_casualties,
        "05-equipment-shift" = plot_equipment_shift,
        "06-bayes-artillery" = plot_bayes_artillery
)

iwalk(
        all_plots,
        ~ ggsave(
                filename = paste0("figures/", .y, ".png"),
                plot = .x,
                width = 10,
                height = 6,
                dpi = 300,
                bg = "white"
        )
)

cat("\nAnalysis complete. Plots saved to 'figures' directory.\n")

# End
