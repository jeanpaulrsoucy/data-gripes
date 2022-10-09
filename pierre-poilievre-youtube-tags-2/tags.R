library(jsonlite)
library(curl)
library(dplyr)
library(stringr)
library(rvest)

# load URLs
# yt-dlp --flat-playlist --print "%(url)s" "https://www.youtube.com/c/PierrePoilievreMP" > Videos.txt
# yt-dlp --print "%(upload_date>%Y-%m-%d)s" "https://www.youtube.com/c/PierrePoilievreMP" > Videos_dates.txt
vids <- data.frame(
  url = readLines("Videos.txt"),
  date = as.Date(readLines("Videos_dates.txt"))
)

# get URLs for archived webpages
urls <- lapply(1:nrow(vids), function(x) {
  print(paste(x, vids[x, "url"], sep = ": "))
  api <- paste0("https://archive.org/wayback/available?url=", vids[x, "url"], "&timestamp=20000101")
  resp <- fromJSON(api)
  if (identical(resp$archived_snapshots, list())) {
    NULL
  } else {
    resp$archived_snapshots$closest$url
  }
})

# save URLs for archived webpages
write_json(urls, "archive_urls.json")

# read URLs and join to data frame
urls <- fromJSON("archive_urls.json")
urls[sapply(urls, function(x) length(x) == 0L)] <- NA
vids$archive_url <- unlist(urls)

# blank out bad URL
vids[815, "archive_url"] <- NA

# download archived webpages
dir.create("webpages", showWarnings = FALSE)
vids$archive_file_path <- ifelse(is.na(vids$archive_url), NA,
                                 file.path("webpages", paste0(str_extract(vids$url, "(?<=watch\\?v=).*"), ".html")))
for (i in 1:nrow(vids)) {
  if (is.na(vids[i, "archive_url"])) {
    print(paste(i, "No archived URL available, skipping...", sep = ": "))
  } else {
    print(paste(i, vids[i, "archive_url"], sep = ": "))
    curl_download(vids[i, "archive_url"], vids[i, "archive_file_path"])
  }
}

# extract tags from webpages
tags <- lapply(1:nrow(vids), function(x) {
  path <- vids[x, "archive_file_path"]
  if (is.na(path)) {
    print(paste(x, "No archived URL available, skipping...", sep = ": "))
    return(NA)
  } else {
    print(paste0(x, ": Extracting tags from archived webpage - ", path))
    html <- read_html(path)
    html %>%
      html_elements("meta[property=og\\:video\\:tag]") %>%
      html_attr("content") %>%
      gsub("â|\u0080|\u009c|\u009d|;", "", .)
  }
})

# create final dataset and save
dat <- tibble(
  url = vids$url,
  date = vids$date,
  tags = unlist(lapply(tags, function(x) paste(x, collapse = ",")))
)
write.csv(dat, "videos_pp_tags.csv", row.names = FALSE)

# what are the most popular tags?
# some tags appear twice because they have quoted and non-quoted versions
# but quotes were stripped in the processing
unlist(tags) %>% table() %>% sort(decreasing = TRUE)

# when did the "mgtow" tag first appear?
sapply(tags, function(x) "mgtow" %in% x) %>% which %>% last %>% vids[., ] # 2018-02-27

# subset to videos with tag info from first instance of "mgtow" tag on
names(tags) <- paste(vids$url, vids$date, sep = " / ")
tags_2 <- tags[sapply(tags, function(x) !identical(x, NA) & !length(x) == 0) & vids$date >= as.Date("2018-02-27")]
tags_2 <- tags_2[1:length(tags_2) - 1] # first video from 2018-02-27 lacks the tag

# the characteristic mix of "mgtow" and "Ben Shapiro" tags first appears in a video on 2018-03-02
# which other videos share this exact set of tags?
sapply(seq_along(tags_2), function(x) {
  identical(tags_2[[x]], tags_2[[702]])
})
