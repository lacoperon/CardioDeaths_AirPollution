library(readr)
library(dplyr)

df <- read_csv("pollution_us_2000_2016.csv")

df$City <- sapply(df$City, function(x) {
  if(x == "Indianapolis (Remainder)") {
    return("Indianapolis")
  } else {
    return(x)
  }
})
sort(unique(df$City))

city_list <- read_lines("cities_of_interest.txt")
city_list <- gsub("(.*),.*", "\\1", city_list)

df_oi <- filter(df, City %in% city_list)


library(dplyr)
df2 <- filter(df, City=="Cleveland", grepl("2008", `Date Local`))
unique(df2$`Date Local`)
  

df3 <- read_csv("epa_air_quality_annual_summary.csv") %>%
  filter(city_name=="Chicago")
