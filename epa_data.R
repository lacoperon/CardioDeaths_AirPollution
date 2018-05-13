library(dplyr)
library(readr)

epa_df <- read_csv("./data/pollution_us_2000_2016.csv")

foo <- strsplit(as.character(epa_df$`Date Local`), '-', fixed=TRUE)
epa_df$year  <- as.numeric(sapply(foo, function(x) { return(x[1])}))
epa_df$month <- as.numeric(sapply(foo, function(x) { return(x[2])}))
epa_df$day   <- as.numeric(sapply(foo, function(x) { return(x[3])}))

df_city <- read_csv("./constructed/CountyMapping.csv") %>%
  mutate(citystate = paste(`ASSOCIATED CITY`, State))

epa_grouped_df <- epa_df %>%
  group_by(`City`, `State`, `year`) %>%
  summarize(`CO Mean` =mean(`CO Mean`, na.rm=T),
            `NO2 Mean` =mean(`NO2 Mean`, na.rm=T),
            `O3 Mean` =mean(`O3 Mean`, na.rm=T),
            `SO2 Mean` =mean(`SO2 Mean`, na.rm=T)) %>%
  mutate(citystate = paste(City, State)) %>%
  filter(citystate %in% unique(df_city$citystate)) %>%
  filter(year > 2007 & year < 2015) %>%
  select(-citystate)

colnames(epa_grouped_df) <- c("City", "State", "year", "CO", "NO2", "O3", "SO2")

write_csv(epa_grouped_df, "./data/epa_panel_data.csv")


