library(dplyr)
library(stringr)
library(ggplot2)
library(scales)

# Read in data
spl_10_checkout <- read.csv("~/Desktop/INFO201/2017-2023-10-Checkouts-SPL-Data.csv", stringsAsFactors = FALSE)

spl_10_checkout <- spl_10_checkout %>%
  filter(CheckoutYear >= as.numeric(format(Sys.Date(), "%Y")) - 4) %>%
  filter(str_detect(Creator, "Rick Riodarn|Suzanne Collins|Veronica Roth")) %>%
  mutate(date = as.Date(paste0(CheckoutYear, "-", CheckoutMonth, "-01"), format = "%Y-%m-%d"))

rick_checkout <- spl_10_checkout %>%
  filter(str_detect(Creator, "Rick Riodarn")) %>%
  group_by(date) %>%
  summarise(date = date, rick_checkouts = sum(Checkouts, na.rm = TRUE)) %>%
  select(date, rick_checkouts)

suzanna_checkout <- spl_10_checkout %>%
  filter(str_detect(Creator, "Suzanne Collins")) %>%
  group_by(date) %>%
  summarise(date = date, suzanne_checkouts = sum(Checkouts, na.rm = TRUE)) %>%
  select(date, suzanne_checkouts)

veronica_checkout <- spl_10_checkout %>%
  filter(str_detect(Creator, "Veronica Roth")) %>%
  group_by(date) %>%
  summarise(date = date, veronica_checkouts = sum(Checkouts, na.rm = TRUE)) %>%
  select(date, veronica_checkouts)

# Combine the data frames for the three authors
author_checkouts <- full_join(rick_checkout, suzanna_checkout, by = "date") %>%
  full_join(veronica_checkout, by = "date")

# Create a line chart
ggplot(author_checkouts, aes(x = date)) +
  geom_line(aes(y = rick_checkouts, color = "Rick Riodarn"), size = 1.2) +
  geom_line(aes(y = suzanne_checkouts, color = "Suzanne Collins"), size = 1.2) +
  geom_line(aes(y = veronica_checkouts, color = "Veronica Roth"), size = 1.2) +
  scale_color_manual(name = "Authors", values = c("Rick Riodarn" = "red", "Suzanne Collins" = "blue", "Veronica Roth" = "green")) +
  labs(x = "Date", y = "Checkouts", title = "Checkouts of Masterpiece Authors") +
  theme_minimal()