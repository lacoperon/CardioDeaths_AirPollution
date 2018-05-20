# Authored by Elliot Williams
# May 12th, 2018
# If you use my code, please attribute me (and star my repo!)
# https://github.com/lacoperon/QAC311FinalProject

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
  dplyr::mutate(Smoke = as.numeric(Smoke), BingeDrink = as.numeric(BingeDrink),
                Asthma = as.numeric(Asthma))  %>%
  dplyr::mutate(City = as.factor(City)) %>%
  filter(! is.na(CVDperCapita))

combined_data2 <- plyr::join_all(
  list(acs_data, cdc_data_combined, cvd_data),
  by=c("City", "year")) %>%
  dplyr::select(-MetroArea) %>%
  dplyr::mutate(Smoke = as.numeric(Smoke), BingeDrink = as.numeric(BingeDrink)) %>%
  dplyr::mutate(City = as.factor(City)) %>%
  filter(! is.na(CVDperCapita))
  

cd_wofactors <- select(combined_data, -City, -year)
cor_matrix <- cor(cd_wofactors, use = "complete.obs")
library(corrplot)
## corrplot 0.84 loaded
corrplot(cor_matrix, method = "square")

abs(cor_matrix) > 0.7 # Recheck this

cd_f <- select(combined_data, -ind_liv_diff, -amb_diff, -disabled, 
                  -ss_income, -CholestCheck, -CO, -is_married,
                  -inc_pov_ratio, -is_medicare)

cd_f2 <- select(combined_data2, -ind_liv_diff, -amb_diff, -disabled, 
               -ss_income, -CholestCheck, -is_married,
               -inc_pov_ratio, is_medicare)


cd_filt <- select(cd_f, -City, -year)

cor_matrix2 <- cor(cd_f, use = "complete.obs")
abs(cor_matrix2) > 0.7
library(corrplot)
## corrplot 0.84 loaded
corrplot(cor_matrix2, method = "square")
  
  
# TODO: Do analysis on covariates

p_combined_data <- pdata.frame(cd_f, index=c("City", "year"))
p_combined_data2 <- pdata.frame(cd_f2, index=c("City", "year"))

grun.pool <- plm(( CVDperCapita ~ income + racaian + racasn + racblk + racsor +
                     racwht + selfcare_diff + is_va + public_assist_inc +
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
                   racwht + selfcare_diff + is_va + public_assist_inc +
                   ret_income + avg_schl + age + is_insured + avg_weight + NO2 +
                   O3 + SO2 + GoodHealth + SeenDoctor12mo + Exercise + Smoke +
                   BingeDrink + Asthma) , data = p_combined_data, model = "within", effect="individual")

grun.re <- plm(( CVDperCapita ~ income + racaian + racasn + racblk + racsor +
                   racwht +
                   is_va + racwht + public_assist_inc +
                   ret_income + avg_schl + age + is_insured + avg_weight + NO2 +
                   O3 + SO2 + GoodHealth + SeenDoctor12mo + Exercise + Smoke +
                   BingeDrink) , data = p_combined_data, model = "random", effect="individual")

phtest(grun.fe, grun.re) # we could use random effects, 
                         # but we will instead use fixed effects
                         # because getting 'singular matrix' error ==> too little data'
                         # (Both are appropriate under null hyp, so this is fine)

# Same thing when keeping data, removing pollution vals
grun.fe2 <- plm(( CVDperCapita ~ income + racaian + racasn + racblk + racsor +
                    racwht + selfcare_diff + is_va + public_assist_inc +
                    ret_income + avg_schl + age + is_insured + avg_weight  +
                    GoodHealth + SeenDoctor12mo + Exercise + Smoke +
                    BingeDrink + Asthma ) , data = p_combined_data2, model = "within", effect="individual")


# No significant squared relationship of SO2, there is w avg_weight, age, etc.
grun.feb <- plm(( CVDperCapita ~ income + racaian + poly(racasn, 2, raw=T) + racblk + racsor +
                   racwht + Asthma +
                   poly(is_va, 2, raw=T) +
                   ret_income + avg_schl + poly(age, 2, raw=T) + is_insured  + NO2 +
                   O3 + poly(SO2, 2, raw=T)+ poly(avg_weight, 2, raw=T) + 
                   GoodHealth + SeenDoctor12mo + Exercise + Smoke +
                   BingeDrink ) , data = p_combined_data, model = "within", effect="individual")

grun.feb2 <- plm(( CVDperCapita ~ income + racaian + poly(racasn, 2, raw=T) + racblk + racsor +
                    racwht + Asthma + Asthma * age +
                    poly(is_va, 2, raw=T) +
                    ret_income + avg_schl + poly(age, 2, raw=T) + is_insured  + NO2 +
                    O3 + poly(SO2, 2, raw=T)+ poly(avg_weight, 2, raw=T) + 
                    GoodHealth + SeenDoctor12mo + Exercise + Smoke +
                    BingeDrink ) , data = p_combined_data, model = "within", effect="individual")
# no interaction bw asthma, age
grun.feb3 <- plm(( CVDperCapita ~ income + racaian + poly(racasn, 2, raw=T) + racblk + racsor +
                     racwht + Asthma + Asthma * SO2 +
                     poly(is_va, 2, raw=T) +
                     ret_income + avg_schl + poly(age, 2, raw=T) + is_insured  + NO2 +
                     O3 + poly(SO2, 2, raw=T)+ poly(avg_weight, 2, raw=T) + 
                     GoodHealth + SeenDoctor12mo + Exercise + Smoke +
                     BingeDrink ) , data = p_combined_data, model = "within", effect="individual")
# nor asthma, SO2

# This is our final model, then
grun.fec <- plm(( CVDperCapita ~ income + racaian + racasn + racblk + racsor +
                    racwht + selfcare_diff + is_va + public_assist_inc +
                    ret_income + avg_schl + poly(age, 2, raw=T) + is_insured + poly(avg_weight, 2, raw=T) + NO2 +
                    O3 + SO2 + GoodHealth + SeenDoctor12mo + Exercise + Smoke +
                    BingeDrink + Asthma ) , data = p_combined_data, model = "within", effect="individual")

lsdv <- glm(CVDperCapita ~ income + racaian + racasn + racblk + racsor +
              racwht + selfcare_diff + is_va + public_assist_inc +
              ret_income + avg_schl + poly(age, 2, raw=T) + is_insured + poly(avg_weight, 2, raw=T) + NO2 +
              O3 + SO2 + GoodHealth + SeenDoctor12mo + Exercise + Smoke +
              BingeDrink + Asthma + City, data = combined_data)

summary(lsdv) #LSDV shows that cities are important -- 
#              also that more data would be helpful
summary(grun.fec) # final model, shows a bunch of significant factors
# 66.2% adjusted R-squared -- that's awesome!

library(ggplot2)
res <- data.frame(residuals(grun.fe))
colnames(res) <- "Res"
ggplot(res, aes(Res)) + 
  geom_density(adjust=0.4, color="hotpink", fill="hotpink", alpha=0.3) + 
  xlim(-20,20) +
  labs(title="Residual Density Plot of Fixed Effects Model", 
       subtitle="One way wrt Panel Unit (not Time)", 
       x = "Residual Value", y="Frequency")

# not worthwile doing a plot against time -- test showed that it wasn't
# a significant factor


# https://www.stata.com/statalist/archive/2003-09/msg00595.html
# The null is that the two estimation methods are both OK and that therefore 
# they should yield coefficients that are "similar".  The alternative 
# hypothesis is that the fixed effects estimation is OK and the random 
# effects estimation is not; if this is the case, then we would expect to 
# see differences between the two sets of coefficients.
# 
# This is because the random effects estimator makes an assumption (the 
# random effects are orthogonal to the regressors) that the fixed effects 
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

xs <- seq(18, 100, 1)
ys <- 1.0316e+00 * xs^2 - 7.8739e+01 * xs

xy <- data.frame(xs, ys)
library(ggplot2)
ggplot(xy, aes(xs, ys)) + geom_line() +
  labs(title="Effect of average age on CVD risk", 
       y="Additional CVD deaths per 100K", x="Average age") +
  xlim(20, 50) # risk increases for young, old, consistent with literature

xs <- seq(18, 100, 1)
ys <- 1.0316e+00 * xs^2 - 7.8739e+01 * xs

xy <- data.frame(xs, ys)
library(ggplot2)
ggplot(xy, aes(xs, ys)) + geom_line() +
  labs(title="Effect of average age on CVD risk", 
       y="Additional CVD deaths per 100K", x="Average age") +
  xlim(20, 50) # risk increases for young, old, consistent with literature

xs2 <- seq(80, 180, 1)
ys2 <- -6.9618e-03 * xs2 ^ 2 + 2.0270e+00 * xs2

xy2 <- data.frame(xs2, ys2)

ggplot(xy2, aes(xs2, ys2)) + geom_line() +
  labs(title="Effect of average weight on CVD risk", 
       y="Additional CVD deaths per 100K", x="Average weight") +
  xlim(80, 180) # CVD risk increases with avg weight, 
                # up to certain point (diminishing effect)


# regular mean
x1 <- mean(combined_data$SO2, na.rm=T)
# mean in which all measurements were taken below 1.5
x2 <- mean(sapply(combined_data$SO2, function(x) { min(x, 1.5)}), na.rm=T)
x1 - x2
# Back of envelope calculations
# 400 PUMA regions, 100K ppl each approx, w coeff of 5.2328 for SO2
400 * 5.7699 * (x1-x2)

combined_data %>% filter(SO2 > 1.5) %>% group_by(City) %>% summarize(n=n()) %>%
  arrange(desc(n))

# SO2 is emitted from heating oils -- NYC has enacted laws that would phase out
# the heating oils in question

# https://www.healthypeople.gov/2020/healthy-people-in-action/story/new-york-city-air-quality-programs-reduce-harmful-air-pollutants

comb_data_freq <- select(combined_data, SO2, NO2, CO, O3, year, City)

# -- Let's see when data on the various NAAQS Pollutants is available, by City
# SO2
SO2_avail <- data.frame(unique(combined_data$City))
for (year in 2008:2014) {
  x <- sapply(unique(combined_data$City), 
         function(x) { 
           is.na((comb_data_freq %>% filter(year==year, City==x))$SO2[year-2007]) 
           })
  print(length(x))
  SO2_avail <- cbind(SO2_avail, x)
}
colnames(SO2_avail) <- c("City", 2008:2014)
SO2_avail_year <- colSums(SO2_avail[,-1])

# NO2
NO2_avail <- data.frame(unique(combined_data$City))
for (year in 2008:2015) {
  x <- sapply(unique(combined_data$City), 
              function(x) { 
                is.na((comb_data_freq %>% filter(year==year, City==x))$NO2[year-2008]) 
              })
  print(length(x))
  NO2_avail <- cbind(NO2_avail, x)
}
colnames(NO2_avail) <- c("City", 2008:2015)
NO2_avail_year <- colSums(NO2_avail[,-1])

# CO
CO_avail <- data.frame(unique(combined_data$City))
for (year in 2008:2015) {
  x <- sapply(unique(combined_data$City), 
              function(x) { 
                is.na((comb_data_freq %>% filter(year==year, City==x))$CO[year-2008]) 
              })
  print(length(x))
  CO_avail <- cbind(CO_avail, x)
}
colnames(CO_avail) <- c("City", 2008:2015)
CO_avail_year <- colSums(CO_avail[,-1])

# O3
O3_avail <- data.frame(unique(combined_data$City))
for (year in 2008:2015) {
  x <- sapply(unique(combined_data$City), 
              function(x) { 
                is.na((comb_data_freq %>% filter(year==year, City==x))$O3[year-2008]) 
              })
  print(length(x))
  O3_avail <- cbind(O3_avail, x)
}
O3_avail_year <- colSums(O3_avail[,-1])

# Now, let's visualize the number of observations of pollutants over year
naaqs_values <- comb_data_freq %>%
  group_by(year) %>%
  summarize(obs_SO2=sum(! is.na(SO2)),
            obs_NO2=sum(! is.na(NO2)),
            obs_CO=sum(! is.na(CO)),
            obs_O3=sum(! is.na(O3)))


# They all have the same number of values, so let's graph that over year
ggplot(naaqs_values, aes(x=year, y=obs_SO2)) + 
  geom_bar(stat="identity", fill="blue", color="black") +
  labs(title="NAAQS Pollutant Observations", 
       subtitle="Observations available in cities by year",
       x = "Year", y="Number of Observations") +
  scale_x_discrete("Year", breaks=2008:2014, limits=2008:2014, expand=c(0.05,0)) + 
                     theme(text = element_text(size=20))

write_csv(combined_data, "data/complete_panel_data.csv")


