# Plots for "Why COVID-19 case rates are highest in boosted individuals" #

# load packages
library(dplyr)
library(readr)
library(jsonlite)
library(curl)
library(ggplot2)
library(ggpubr)

# # list archived files
# files <- jsonlite::fromJSON("https://api.opencovid.ca/archive?uuid=123ad392-02cc-48ef-b328-cadf34e4d224")
# 
# # remove duplicated files
# files <- files %>%
#   filter(file_etag_duplicate == 0)
# 
# # download files
# dir.create("raw", showWarnings = FALSE)
# for (i in 1:nrow(files)) {
#   curl::curl_download(files[i, "file_url"], file.path("raw", paste0(files[i, "file_date_true"], ".csv")))
# }

# load datasets
ds <- lapply(list.files("raw", full.names = TRUE), function(x) {
  read_csv(x, show_col_types = FALSE, progress = FALSE) %>%
    mutate(date = as.Date(sub("\\.csv", "", basename(x))), .before = 1)
}) %>%
  bind_rows()

# make English category names consistent
ds <- ds %>%
  mutate(
    `label-en` = case_when(
      `label-en` == "Partially protected" ~ "Partially vaccinated",
      `label-en` == "Fully protected" ~ "Fully vaccinated",
      TRUE ~ `label-en`
    )
  )

# keep relevant data and convert label to factor
ds <- ds %>%
  transmute(
    date,
    category = factor(
      `label-en`,
      levels = c("Unvaccinated", "Cases not yet protected", "Partially vaccinated",
                 "Fully vaccinated", "Fully vaccinated with an additional dose")),
    prop_cases
  )

# plot data
ggplot(data = ds, aes(x = date, colour = category, group = category, y = prop_cases)) +
  scale_x_date(date_labels = "%b %Y") +
  geom_line() +
  geom_point() +
  geom_vline(xintercept = as.Date("2022-01-01"), linetype = "dashed") +
  theme_pubr() +
  theme(
    legend.title = element_blank(),
    plot.title = element_text(size = 20),
    axis.title = element_text(size = 16)
  ) +
  guides(colour = guide_legend(nrow = 2, byrow = TRUE)) +
  labs(x = "Date of report", y = "Proportion of cases (%)", title = "Distribution of cases by vaccination status",
       caption = "Data represent 1,997,129 cases 5 years or older with vaccine history since December 14, 2020 up to April 3, 2022.")

# save plot
ggsave("proportion_of_cases_by_vaccination_status.png", height = 800 * 3, width = 744 * 3, units = "px", dpi = 300)
