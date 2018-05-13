
# -- Gets CDC Data ---
library(rvest)

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
  for (i in 1:num_tables) {
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
adults_good_better_health <- get_next_5_tables(url, j=1, column_names) %>% select("MMSA(s)", "%")
adults_doctor_12_months <- get_next_5_tables(url, j=21, column_names) %>% select("%")
adults_cholesterol_checked <- get_next_5_tables(url, j=51, column_names)%>% select("%")
adults_exercise_moderately <- get_next_5_tables(url, j=71, column_names)%>% select("%")
adults_smoke <- get_next_5_tables(url, j=101, column_names) %>% select("%")
adults_binge_drink <- get_next_5_tables(url, j=111, column_names) %>% select("%")
adults_asthma <- get_next_5_tables(url, j=181, column_names) %>% select("%")

df_combined <- cbind(adults_good_better_health, adults_doctor_12_months, 
         adults_cholesterol_checked, adults_exercise_moderately,
         adults_smoke, adults_binge_drink, adults_asthma)

colnames(df_combined) <- c("MetroArea", "GoodHealth", "SeenDoctor12mo",
                           "CholestCheck", "Exercise", "Smoke", "BingeDrink", "Asthma")

write_csv(df_combined, "./data/metro_cdc_data.csv")
