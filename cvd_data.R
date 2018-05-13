library(readr)
library(jsonlite)

L <- fromJSON("cardiovascular2.json")
n <- length(L[[1]])
cvd_df <- structure(L, row.names = c(NA, -n), class = "data.frame")
cvd_df <- data.frame(t(cvd_df))
colnames(cvd_df) <- 1980:2014
cvd_df$county <- rownames(cvd_df)

library(tidyr)
cvd_df_long <- gather(cvd_df, year, CVDpercapita, `1980`:`2014`)

city_mapping <- read_csv("./constructed/CountyMapping.csv")

fips_to_city <- read_csv("./data/fips_codes_website.csv") %>%
  filter(`Entity Description`=="city")

fips_to_city$State <- state.name[match(fips_to_city$`State Abbreviation`, state.abb)]

fips_mapping <- inner_join(fips_to_city, city_mapping, by=c("GU Name"="ASSOCIATED CITY", "State")) %>%
  mutate(FIPS = paste0(`State FIPS Code`, `County FIPS Code`)) %>%
  distinct(`GU Name`, FIPS)

cvd_data_city <- inner_join(cvd_df_long, fips_mapping, by=c("county"="FIPS")) %>%
  group_by(`GU Name`, year) %>%
  summarize(CVDpercapita = mean(CVDpercapita))

colnames(cvd_data_city) <- c("City", "year", "CVDperCapita")

write_csv(cvd_data_city, "./data/cvd_data_panel.csv")
