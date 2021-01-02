###########################################################
# yk_job_postings.R
# 
# Generate a few nice charts from YK Government
# job postings over the year.
########################################################### ----

# Load libraries
library(tidyverse)
library(lubridate)
library(DBI)
library(ggplot2)
library(plotly)

# Set working directory
setwd("~/Documents/yk_jobs_analysis")

# Get postings from SQLite database file
db <- dbConnect(RSQLite::SQLite(), "postings.db")
postings <- dbGetQuery(
  db,
  "SELECT * FROM postings"
) %>%
  mutate(close_date = as.Date(close_date)) %>%
  mutate(close_day  = wday(close_date, label = TRUE))
dbDisconnect(db)

# A bit of processing
postings <- postings %>%
  mutate(all_locations = str_extract_all(location, "[ A-Za-z]+, [A-Z]{2}")) %>%
  mutate(department1 = str_extract(department, "^[^-]+? -") %>% gsub(" -","",.))


########################################################### 
# Save data as CSV
########################################################### ----
write_csv(postings, "yk_job_postings.csv")


########################################################### 
# Analysis
########################################################### ----

###########
# Locations ----
total_postings <- nrow(postings)

locations <- postings %>%
  unnest_longer(all_locations) %>%
  group_by(all_locations) %>%
  summarize(count = n()) %>%
  arrange(desc(count)) %>%
  ungroup() %>%
  mutate(all_locations = factor(all_locations, levels=all_locations, ordered = TRUE)) %>%
  ungroup() %>%
  mutate(perc = count / total_postings)

locations %>%
  ggplot(aes(x=all_locations, y=count)) +
  geom_bar(stat="identity", fill="steelblue") +
  geom_label(aes(label = paste0(round(perc*100, digits = 1), "%")), size = 3) +
  labs(title = "YK Government Job Postings by City",
       subtitle = "February 2020 to December 2020",
       x = "City", y = "Job Post Count") +
  theme_bw() +
  theme(
    text = element_text(colour = "white"),
    plot.title = element_text(),
    axis.title = element_text(size = 15, colour="white"),
    axis.text = element_text(size = 15, colour="white"),
    legend.background = element_rect(colour = "gray30", fill = "gray30"),
    legend.text = element_text(size=15, colour="white"),
    legend.key = element_rect(colour = "gray30", fill = "gray30"),
    panel.background = element_rect(colour = "gray30", fill = "gray30"),
    plot.background = element_rect(colour = "gray30", fill = "gray30"),
    axis.text.x = element_text(angle = 45, hjust=1)
  )
ggsave("yk_job_locations.png", width = 10, height = 6)


###########
# Department ----
departments <- postings %>%
  group_by(department1) %>%
  summarize(count = n()) %>%
  ungroup() %>%
  mutate(department1 = if_else(is.na(department1), "Unknown", department1)) %>%
  arrange(desc(count)) %>%
  mutate(department1 = factor(department1, levels=department1, ordered = TRUE)) %>%
  ungroup() %>%
  mutate(perc = count / sum(count))

ggplot(departments, aes(x=department1, y=count)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_label(aes(label = paste0(round(perc*100, digits = 1), "%")), size = 3) +
  labs(title = "YK Government Job Postings by Department",
       subtitle = "February 2020 to December 2020",
       x = "Department", y = "Job Post Count") +
  theme_bw() +
  theme(
    text = element_text(colour = "white"),
    plot.title = element_text(),
    axis.title = element_text(size = 15, colour="white"),
    axis.text = element_text(size = 15, colour="white"),
    legend.background = element_rect(colour = "gray30", fill = "gray30"),
    legend.text = element_text(size=15, colour="white"),
    legend.key = element_rect(colour = "gray30", fill = "gray30"),
    panel.background = element_rect(colour = "gray30", fill = "gray30"),
    plot.background = element_rect(colour = "gray30", fill = "gray30"),
    axis.text.x = element_text(angle = 45, hjust=1),
    plot.margin = margin(10, 10, 10, 55)
  )
ggsave("yk_job_departments.png", width = 10, height = 6)


###########
# Sunburst job title by department ----
title_by_department <- postings %>%
  group_by(department1, title) %>%
  summarize(count = n()) %>%
  ungroup()

department_total <- title_by_department %>%
  group_by(department1) %>%
  summarize(count = sum(count))

fig <- plot_ly(
  labels = c(
    department_total$department1,
    title_by_department %>% pull(title)),
  parents = c(
    rep("", length(department_total$department1)),
    title_by_department %>% pull(department1)),
  values = c(
    department_total$count,
    title_by_department %>% pull(count)),
  type = "sunburst",
  branchvalues = 'total',
  insidetextorientation='radial'
)
fig
htmlwidgets::saveWidget(fig, "yk_job_sunburst.html")
