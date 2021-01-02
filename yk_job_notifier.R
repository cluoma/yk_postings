# Clean environment
rm(list=ls())

# Load libraries
library(dplyr)
library(tidyr)
library(rvest)
library(DBI)
library(RSQLite)
library(emayili)
library(xtable)

# Functions
mail_dataframe <- function(df, sender, recipient, password)
{
  email <- envelope() %>%
    from(sender) %>%
    to(recipient) %>%
    subject("YK Postings Mailer") %>%
    html(xtable(new_postings) %>% print("html") %>% as.character())
  
  smtp <- server(host = "smtp.gmail.com",
                 port = 465,
                 username = sender,
                 password = password)
  
  tryCatch(smtp(email, verbose = TRUE),
           error = function(e) {},
           finally = {})
}

# Load webpage
url <- "https://yukongovernment.hua.hrsmart.com/hr/ats/JobSearch/viewAll/jobSearchPaginationExternal_pageSize:100/jobSearchPaginationExternal_page:1"
page <- read_html(url)

# Parse job postings
postings <- page %>%
  html_node("table") %>%
  html_table() %>%
  select(id         =`Req. #`,
         title      =`Job Title`,
         department =`Department`,
         location   =`Location`,
         close_date =`Close Date`) %>%
  mutate(close_date = as.character(as.Date(close_date, "%d %B %Y"))) %>%
  mutate(scrape_date = as.character(Sys.Date()))


# Open sqlite db connection
con <- dbConnect(RSQLite::SQLite(), "postings.db")

if ( dbExistsTable(con, "postings") ) {
  # If tabe exists do logic
  
  # Get currently saved jobs
  cur_data <- dbGetQuery(
    con,
    "SELECT id, close_date FROM postings;"
  )
  # Filter new postings to ones we don't have
  new_postings <- postings %>%
    anti_join(cur_data, by=c("id","close_date"))
  
  
  # Do stuff only if we have new postings
  if (nrow(new_postings) > 0) {
    mail_dataframe(new_postings,
                   "from@email.com",
                   "to@email.com",
                   "password")
    
    # Append new data to data
    dbWriteTable(con, "postings", new_postings, append = TRUE)
  }
} else {
  # Make table if it doesn't exist
  dbWriteTable(con, "postings", postings)
}

# Close DB
dbDisconnect(con)
