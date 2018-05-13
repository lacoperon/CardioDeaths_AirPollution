library(readr)
library(dplyr)

cdc_data_tagged <- read_csv("./data/metro_cdc_data_citytagged.csv") %>%
  filter(! is.na(City))  

acs_data <- read_csv("./data/acs_panel_data.csv")

rm(cdc_data_combined)
for(year in 2008:2014){
   if (! exists("cdc_data_combined")) {
     cdc_data_combined <- mutate(cdc_data_tagged, year=year)
   } else {
     cdc_data_combined <- rbind(cdc_data_combined, mutate(cdc_data_tagged, year=year))
   }
}

epa_data <- read_csv("./data/epa_panel_data.csv")

cvd_data <- read_csv("./data/cvd_data_panel.csv")


combined_data <- plyr::join_all(
                    list(acs_data, epa_data, cdc_data_combined, cvd_data),
                    by=c("City", "year")) %>%
  dplyr::select(-MetroArea, -State) %>%
  dplyr::mutate(Smoke = as.numeric(Smoke), BingeDrink = as.numeric(BingeDrink))  %>%
  dplyr::mutate(City = as.factor(City)) %>%
  filter(! is.na(CVDperCapita))

combined_data_wo_pollution <- plyr::join_all(
  list(acs_data, cdc_data_combined),
  by=c("City", "year")) %>%
  dplyr::select(-MetroArea) %>%
  dplyr::mutate(Smoke = as.numeric(Smoke), BingeDrink = as.numeric(BingeDrink)) %>%
  dplyr::mutate(City = as.factor(City)) %>%
  

  
# TODO: Do analysis on covariates
library(plm)
p_combined_data <- pdata.frame(combined_data, index=c("City", "year"))
grun.fe <- plm(CVDperCapita ~ income + racaian + racasn + racblk + racsor +
                 racwht + selfcare_diff + ind_liv_diff + amb_diff +
                 is_medicare + is_va + public_assist_inc + ret_income + 
                 avg_schl + ss_income + age + disabled + is_insured +
                 is_married + inc_pov_ratio + avg_weight + CO + 
                 NO2 + O3 + SO2 + GoodHealth +
                 SeenDoctor12mo + CholestCheck + Exercise + Smoke + 
                 BingeDrink, data = p_combined_data, model = "within")

grun.re <- plm(CVDperCapita ~ income + racaian + racasn + racblk + racsor +
                 racwht + is_va + avg_schl +
                 disabled + is_insured +
                 is_married + inc_pov_ratio + avg_weight + CO +
                 NO2 + O3 + SO2 + GoodHealth +
                 SeenDoctor12mo + CholestCheck + Exercise + Smoke +
                 BingeDrink, data = p_combined_data, model = "random")
summary(grun.re)

phtest(grun.fe, grun.re)

cd_wofactors <- select(combined_data, -City, -year)
cor_matrix <- cor(cd_wofactors, use = "complete.obs")

#https://www.ncbi.nlm.nih.gov/pmc/articles/PMC3854161/
#https://www.ncbi.nlm.nih.gov/pmc/articles/PMC3392899/

# Also, perhaps add a lag term for the pollutants too
