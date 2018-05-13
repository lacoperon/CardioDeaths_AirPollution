# We should try to get more cities!!!

library(readr)
library(dplyr)
library(plm)

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

combined_data2 <- plyr::join_all(
  list(acs_data, cdc_data_combined, cvd_data),
  by=c("City", "year")) %>%
  dplyr::select(-MetroArea, -State) %>%
  dplyr::mutate(Smoke = as.numeric(Smoke), BingeDrink = as.numeric(BingeDrink)) %>%
  dplyr::mutate(City = as.factor(City)) %>%
  filter(! is.na(CVDperCapita))
  

cd_wofactors <- select(combined_data, -City, -year)
cor_matrix <- cor(cd_wofactors, use = "complete.obs")
library(corrplot)
## corrplot 0.84 loaded
corrplot(cor_matrix, method = "square")

cor_matrix > 0.7

cd_f <- select(combined_data, -ind_liv_diff, -amb_diff, -disabled, 
                  -ss_income, -CholestCheck, -CO, -is_married,
                  -inc_pov_ratio)

cd_f2 <- select(combined_data2, -ind_liv_diff, -amb_diff, -disabled, 
               -ss_income, -CholestCheck, -is_married,
               -inc_pov_ratio)


cd_filt <- select(cd_f, -City, -year)

cor_matrix2 <- cor(cd_filt, use = "complete.obs")
library(corrplot)
## corrplot 0.84 loaded
corrplot(cor_matrix2, method = "square")
  
  
# TODO: Do analysis on covariates

p_combined_data <- pdata.frame(cd_f, index=c("City", "year"))
p_combined_data2 <- pdata.frame(cd_f2, index=c("City", "year"))

grun.pool <- plm(( CVDperCapita ~ income + racaian + racasn + racblk + racsor +
                     racwht + selfcare_diff + is_medicare + is_va + public_assist_inc +
                     ret_income + avg_schl + age + is_insured + avg_weight + NO2 +
                     O3 + SO2 + GoodHealth + SeenDoctor12mo + Exercise + Smoke +
                     BingeDrink ) , data = p_combined_data, model = "pooling")

plmtest(grun.pool, effect="time", type="kw") #poolability test -- time doesn't matter
# null hypothesis of zero variance in time-specific errors fails to be rejected
plmtest(grun.pool, effect="individual", type="kw") #poolability test -- city does
# null hypothesis of zero variance in time-specific errors is rejected
plmtest(grun.pool, effect="twoway", type="ghm") # Two way is significant -- only bc of city

# therefore, heterogeneity among individuals may be significant.

# Random effects estimator are reliable under the assumption that individual characteristics 
# (heterogeneity) are exogenous, that is, they are independent with respect to the
# regressors in the random effects equation. The same Hausman test for endogeneity
# we have already used in another chapter can be used here as well, with the null 
# hypothesis that individual random effects are exogenous. 
# The test function phtest() compares the fixed effects and the random 
# effects models; the next code lines estimate the random effects model and 
# performs the Hausman endogeneity test.
#https://bookdown.org/ccolonescu/RPoE4/panel-data-models.html

grun.fe <- plm(( CVDperCapita ~ income + racaian + racasn + racblk + racsor +
                   racwht + selfcare_diff + is_medicare + is_va + public_assist_inc +
                   ret_income + avg_schl + age + is_insured + avg_weight + NO2 +
                   O3 + SO2 + GoodHealth + SeenDoctor12mo + Exercise + Smoke +
                   BingeDrink ) , data = p_combined_data, model = "within", effect="individual")

grun.re <- plm(( CVDperCapita ~ income + racaian + racasn + racblk + racsor +
                   racwht +
                   is_va +
                   ret_income + avg_schl + age + is_insured + avg_weight + NO2 +
                   O3 + SO2 + GoodHealth + SeenDoctor12mo + Exercise + Smoke +
                   BingeDrink ) , data = p_combined_data, model = "random", effect="individual")
# Same thing when keeping data, removing pollution vals
grun.fe2 <- plm(( CVDperCapita ~ income + racaian + racasn + racblk + racsor +
                    racwht + selfcare_diff + is_medicare + is_va + public_assist_inc +
                    ret_income + avg_schl + age + is_insured + avg_weight + NO2 +
                    O3 + SO2 + GoodHealth + SeenDoctor12mo + Exercise + Smoke +
                    BingeDrink ) , data = p_combined_data2, model = "within", effect="individual")


# No significant squared relationship of SO2, there is w avg_weight, age, etc.
grun.feb <- plm(( CVDperCapita ~ income + racaian + poly(racasn, 2, raw=T) + racblk + racsor +
                   racwht +
                   poly(is_va, 2, raw=T) +
                   ret_income + avg_schl + poly(age, 2, raw=T) + is_insured  + NO2 +
                   O3 + poly(SO2, 2, raw=T)+ poly(avg_weight, 2, raw=T) + 
                   GoodHealth + SeenDoctor12mo + Exercise + Smoke +
                   BingeDrink ) , data = p_combined_data, model = "within", effect="individual")

grun.fec <- plm(( CVDperCapita ~ income + racaian + racasn + racblk + racsor +
                    racwht + selfcare_diff + is_medicare + is_va + public_assist_inc +
                    ret_income + avg_schl + poly(age, 2, raw=T) + is_insured + poly(avg_weight, 2, raw=T) + NO2 +
                    O3 + SO2 + GoodHealth + SeenDoctor12mo + Exercise + Smoke +
                    BingeDrink ) , data = p_combined_data, model = "within", effect="individual")

lsdv <- glm(CVDperCapita ~ income + racaian + racasn + racblk + racsor +
      racwht + selfcare_diff + is_medicare + is_va + public_assist_inc +
      ret_income + avg_schl + poly(age, 2, raw=T) + is_insured + poly(avg_weight, 2, raw=T) + NO2 +
      O3 + SO2 + GoodHealth + SeenDoctor12mo + Exercise + Smoke +
      BingeDrink + City, data = combined_data)

summary(lsdv) #LSDV shows that cities are important -- 
#              also that more data would be helpful


phtest(grun.fe, grun.re) # we should use fixed effects instead

summary(grun.fe)
summary(grun.fe2)
summary(grun.feb)
summary(grun.fec) # final model, shows a bunch of significant factors

library(ggplot2)
res <- data.frame(residuals(grun.fe))
colnames(res) <- "Res"
ggplot(res, aes(Res)) + geom_density(adjust=0.4, color="darkblue", fill="darkblue", alpha=0.3) + xlim(-20,20) +
  labs(title="Residual Density Plot of Fixed Effects Model", 
       subtitle="One way wrt Panel Unit (not Time)", 
       x = "Residual Value", y="Frequency")

# maybe should do residual plot over time -- but perhaps not worthwhile, bc
# it wasn't statistically significant in the first place



# https://www.stata.com/statalist/archive/2003-09/msg00595.html
# The null is that the two estimation methods are both OK and that therefore 
# they should yield coefficients that are "similar".  The alternative 
# hypothesis is that the fixed effects estimation is OK and the random 
# effects estimation is not; if this is the case, then we would expect to 
# see differences between the two sets of coefficients.
# 
# This is because the random effects estimator makes an assumption (the 
#                                                                   random effects are orthogonal to the regressors) that the fixed effects 
# estimator does not.  If this assumption is wrong, the random effects 
# estimator will be inconsistent, but the fixed effects estimator is 
# unaffected.  Hence, if the assumption is wrong, this will be reflected in 
# a difference between the two set of coefficients.  The bigger the 
# difference (the less similar are the two sets of coefficients), the bigger 
# the Hausman statistic.
# 
# A large and significant Hausman statistic means a large and significant 
# difference, and so you reject the null that the two methods are OK in 
# favour of the alternative hypothesis that one is OK (fixed effects) and 
# one isn't (random effects).
# 
# Your Hausman stat is very big, and you can see why - the differences 
# between some of the coefficients are big enough to be visible to the naked 
# eye, so to speak - and so you can reject random effects as inconsistent 
# and go with fixed effects instead.

#https://www.ncbi.nlm.nih.gov/pmc/articles/PMC3854161/
#https://www.ncbi.nlm.nih.gov/pmc/articles/PMC3392899/

# Also, perhaps add a lag term for the pollutants too
