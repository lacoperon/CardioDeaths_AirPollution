install.packages("rvest")

library("rvest")

# The following function scrapes the table at index j, as well as num_tables
# following tables, from the specified URL.
# This code will only work on CDC formatted paper tables.
# Input:
#     j   - table offset (ie number of tables to skip before scraping)
#     url - URL to scrape from
#     column_names - the column names of the scraped tables
# Output:
#     Returns a dataframe with the specified data inside, potentially with
#     other information formatted within the CDC table

get_next_5_tables <- function(url, j=0, column_names, num_tables=5) {
  for (i in 1:5) {
    table <- url %>%
      html() %>%
      html_nodes(xpath=paste0('//*[@id="table-', i+j, '"]')) %>%
      html_table()
    if (i == 1) {
      curr_table <- table[[1]]
      curr_table <- curr_table[-1,]
      colnames(curr_table) <- column_names
    } else {
      table <- table[[1]]
      colnames(table) <- column_names
      table <- table[-1,]
      curr_table <- rbind(curr_table, table)
    }
  }
  return(curr_table)
}

url <- "https://www.cdc.gov/mmwr/preview/mmwrhtml/ss6309a1.htm?s_cid=ss6309a1_e"
column_names <- c("MMSA(s)", "Sample Size", "%", "SE", "95%")
adults_good_better_health <- get_next_5_tables(url, j=1, column_names)
