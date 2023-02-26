# Types of checkouts over the past 6 years 

library(dplyr)
library(stringr)
library(ggplot2)
library(scales)

# Read in data
spl_10_checkout <- read.csv("~/Desktop/INFO201/2017-2023-10-Checkouts-SPL-Data.csv", stringsAsFactors = FALSE)

# Filter data to last 6 years of where people chose to checkout (Overdrive, Horizon, Freegal, Hoopla)
spl_10_checkout <- spl_10_checkout %>%
  filter(CheckoutYear >= as.numeric(format(Sys.Date(), "%Y")) - 6) %>%
  filter(str_detect(CheckoutType, "Overdrive|Horizon|Freegal|Hoopla")) %>%
  mutate(date = as.Date(paste0(CheckoutYear, "-", CheckoutMonth, "-01"), format = "%Y-%m-%d"))

overdrive_checkout <- spl_10_checkout %>%
  filter(str_detect(CheckoutType, "Overdrive")) %>%
  group_by(date) %>%
  summarise(overdrive_checkouts = sum(Checkouts, na.rm = TRUE))

horizon_checkout <- spl_10_checkout %>%
  filter(str_detect(CheckoutType, "Horizon")) %>%
  group_by(date) %>%
  summarise(horizon_checkouts = sum(Checkouts, na.rm = TRUE))

freegal_checkout <- spl_10_checkout %>%
  filter(str_detect(CheckoutType, "Freegal")) %>%
  group_by(date) %>%
  summarise(freegal_checkouts = sum(Checkouts, na.rm = TRUE))

hoopla_checkout <- spl_10_checkout %>%
  filter(str_detect(CheckoutType, "Hoopla")) %>%
  group_by(date) %>%
  summarise(hoopla_checkouts = sum(Checkouts, na.rm = TRUE))

# Combine checkout data for each type
combined_checkout <- spl_10_checkout %>%
  group_by(date, CheckoutType) %>%
  summarise(checkouts = sum(Checkouts, na.rm = TRUE))

# Create chart showing checkouts for each type
ggplot(combined_checkout, aes(x = date, y = checkouts, color = CheckoutType)) +
  geom_line(size = 1) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y", limits = c(as.Date("2016-12-01"), as.Date("2022-01-01"))) +
  scale_y_continuous(labels = comma) +
  labs(x = "Year", y = "Checkouts", color = "Checkout Type") +
  ggtitle("Types of Checkouts Over the Past 5 Years") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5), legend.position = "top")

