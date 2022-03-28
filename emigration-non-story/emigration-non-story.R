# Plots for "An emigration non-story"

# load packages
library(dplyr)
library(ggplot2)
library(scales)
library(ggthemes)

# load data
emi <- read.csv("1710004001_databaseLoadingData.csv", stringsAsFactors = FALSE)

# process data
emi <- emi %>%
  filter(Components.of.population.growth == "Emigrants")
emi_last_5 <- emi %>%
  filter(grepl("2017|2018|2019|2020|2021", .data$REF_DATE)) %>%
  transmute(quarter = expand.grid(2017:2021, c("Q1", "Q2", "Q3", "Q4")) %>%
           .[order(.$Var1), ] %>%
           {paste(.$Var1, .$Var2)},
           emigrants = VALUE)
emi_annual <- emi %>%
  transmute(
    year = as.Date(paste0(substr(REF_DATE, 1, 4), "-01-01")),
    emigrants = VALUE
  ) %>%
  group_by(year) %>%
  summarize(emigrants = sum(emigrants), .groups = "drop")

# plot data - emigration 1970 to 2021
ggplot(data = emi_annual, aes(x = year, y = emigrants)) +
  geom_bar(stat = "identity") +
  scale_x_date(breaks = seq.Date(
    from = as.Date("1970-01-01"), to = as.Date("2020-01-01"), by = "5 years"), date_labels = "%Y") +
  scale_y_continuous(label = scales::comma) +
  theme_pander() +
  labs(x = "Year", y = "Emigrants", title = "Yearly emigrants, Canada (1970–2021)",
       caption = "Source: Statistics Canada Table 17-10-0040-01")

# save plot
ggsave("emigrants_total.png")

# plot data - last 5 years by quarter
ggplot(data = emi_last_5, aes(x = quarter, y = emigrants)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(label = scales::comma) +
  theme_pander() +
  labs(x = "Quarter", y = "Emigrants", title = "Quarterly emigrants, Canada (2017–2021)",
       caption = "Source: Statistics Canada Table 17-10-0040-01") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

# save plot
ggsave("emigrants_last_5.png")
