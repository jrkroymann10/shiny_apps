library(RMariaDB)
library(tidyverse)
library(reactable)
library(shiny)

# [Database] - Connection ----
db <- dbConnect(RMariaDB::MariaDB(), user = ********, password = *********,
                dbname = "reading_tracker", host = "localhost")

# [Database] - Query Collection + Selection ----
return_table_query <- function(input) {
  return(paste0("SELECT * FROM ", input, ";"))
}

summary_stats <- "SELECT reader.first_name, SUM(session.min_read) AS min_read, SUM(session.pages_read) AS pages_read, SUM(session.book_start) AS books_started, SUM(session.book_end) AS books_finished, SUM(session.cat_on_lap) AS cat_laps
                  FROM reader
                  JOIN session
                  ON reader.reader_id = session.reader_id
                  GROUP BY(first_name);"