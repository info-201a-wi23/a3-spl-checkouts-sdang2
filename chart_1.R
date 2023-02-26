# Physical or Virtual Book Checkouts over the past 4 years (before pandemic, during, and after)

library(dplyr)
library(stringr)
library(ggplot2)
library(scales)

# Read in data
spl_10_checkout <- read.csv("~/Desktop/INFO201/2017-2023-10-Checkouts-SPL-Data.csv", stringsAsFactors = FALSE)

# Filter data to last 4 years of Book or EBook Records
spl_10_checkout <- spl_10_checkout %>%
  filter(CheckoutYear >= as.numeric(format(Sys.Date(), "%Y")) - 4) %>%
  filter(str_detect(MaterialType, "BOOK|EBOOK")) %>%
  mutate(date = as.Date(paste0(CheckoutYear, "-", CheckoutMonth, "-01"), format = "%Y-%m-%d"))

physicalbook_checkout <- spl_10_checkout %>%
  filter(str_detect(MaterialType, "BOOK")) %>%
  group_by(date) %>%
  summarise(physical_checkouts = sum(Checkouts, na.rm = TRUE))

virtualbook_checkout <- spl_10_checkout %>%
  filter(str_detect(MaterialType, "EBOOK")) %>%
  group_by(date) %>%
  summarise(virtual_checkouts = sum(Checkouts, na.rm = TRUE))

# Merge data
book_checkouts <- merge(physicalbook_checkout, virtualbook_checkout, by = "date", all = TRUE)

# Create time chart
ggplot(book_checkouts, aes(x = date)) +
  geom_line(aes(y = physical_checkouts, color = "Physical Books"), size = 1) +
  geom_line(aes(y = virtual_checkouts, color = "Virtual Books"), size = 1) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_color_manual(name = "Book Type", values = c("Physical Books" = "blue", "Virtual Books" = "green")) +
  labs(title = "Checkout Trends for Physical and Virtual Books",
       x = "Year", y = "Number of Checkouts",
       color = "Book Type") +
  theme(plot.title = element_text(hjust = 0.5))




