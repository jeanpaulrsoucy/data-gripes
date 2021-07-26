# Extract "Number of Daily cases by Zones" tables from OCR PDF
# performed with tabulizer version 0.2.2

# load packages
library(tabulizer) # requires rJava
library(tibble)
library(dplyr)
library(tidyr)
library(readr)

# define column names
cols <- c("Date", "Far North West", "Far North Central", "Far North East",
          "North West", "North Central", "North East", "Saskatoon",
          "Central West", "Central East", "Regina", "South West",
          "South Central", "South East", "Not Assigned", "Total")

# extract tables
# note that columns with no numbers are dropped
p1 <- extract_tables("raw/sk-foi-ocr.pdf", pages = 1, guess = FALSE,
               area = list(c(126.06338, 36.12676, 516.10563, 488.66197))) %>%
  .[[1]] %>%
  as.data.frame %>%
  setNames(cols[c(-3)])
p2 <- extract_tables("raw/sk-foi-ocr.pdf", pages = 2, guess = FALSE,
               area = list(c(129.2958, 35.0493, 521.4930, 480.0423))) %>%
  .[[1]] %>%
  as.data.frame %>%
  setNames(cols[c(-3, -4, -7, -14)])
p3 <- extract_tables("raw/sk-foi-ocr.pdf", pages = 3, guess = FALSE,
               area = list(c(130.37324, 32.89437, 518.26056, 481.11972))) %>%
  .[[1]] %>%
  as.data.frame %>%
  setNames(cols[c(-3, -4)])
p4 <- extract_tables("raw/sk-foi-ocr.pdf", pages = 4, guess = FALSE,
               area = list(c(127.14085, 22.11972, 290.91549, 480.04225))) %>%
  .[[1]] %>%
  as.data.frame %>%
  setNames(cols[c(-2, -3, -4, -14, -15)])
col_totals <- extract_tables("raw/sk-foi-ocr.pdf", pages = 4, guess = FALSE,
                             area = list(c(287.68310, 29.66197, 298.45775, 475.73239))) %>%
  .[[1]] %>%
  as.data.frame %>%
  setNames(cols) %>%
  mutate(across(c(-Date), function(x) {
    x %>%
      sub("o", "0", .) %>%
      as.integer()
  }))

# combine and process tables

## combine and process
tab <- bind_rows(p1, p2, p3, p4) %>%
  mutate(Date = as.Date(Date, "%m/%d/%Y")) %>%
  add_column("Far North Central" = 0, .after = "Far North West") %>% # blank column
  add_row(Date = as.Date("2020-05-11"), `Far North West` = "5", Total = "5", .after = 61) %>% # row got dropped somehow
  add_row(Date = as.Date("2020-05-21"), `Far North West` = "5", Total = "5", .after = 71) %>% # row got dropped somehow
  mutate(across(c(-Date), function(x) {
    x %>%
      sub("o", "0", .) %>%
      gsub("\\.", "", .) %>%
      gsub(" ", "", .) %>%
      sub("", "0", .) %>%
      replace_na(list(x = 0)) %>%
      as.integer()
    }))

# verify that row and column totals agree

## compare row sums to "total" column
all(rowSums(tab[2:15]) == tab[16]) # should return TRUE

## compare column sums to "total" row
all(colSums(tab[2:16]) == col_totals[2:16]) # should return TRUE

# write CSV
write.csv(tab, "data/sk-foi-data.csv", row.names = FALSE)
