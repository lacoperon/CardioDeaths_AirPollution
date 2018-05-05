library(readr)

df <- read_csv("pollution_us_2000_2016.csv")
sort(unique(df$City))

library(dplyr)
df2 <- filter(df, City=="Cleveland", grepl("2008", `Date Local`))
unique(df2$`Date Local`)
  

df3 <- read_csv("epa_air_quality_annual_summary.csv") %>%
  filter(city_name=="Chicago")
