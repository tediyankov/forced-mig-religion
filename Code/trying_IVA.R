

## data
data = dat_imputed %>% 
  filter (year >= 2021)

mci = read_csv ("MCI.csv")
mci = mci %>% rename (orig = `ISO Code`,
                      year = Year,
                      mci = Index)

data2 = data %>%
  left_join (mci, by = c("orig", "year"))

data3 = data2 %>% dplyr::select (orig, dest, year, forced_mig, scaled_sci_2021, mci) %>% drop_na (mci)


data4 = data2 %>% drop_na (mci)

cor.test(data3$mci, data3$scaled_sci_2021, method = "pearson")

library(AER)

# Assuming df is your data frame
# Install and load necessary packages if not already installed/loaded

# First stage regression
first_stage <- lm (forced_mig ~ mci, data = data3)

# Check first stage results
summary(first_stage)

# Extract the predicted values of the endogenous variable (social connectivity index)
data3$predicted_sci <- fitted(first_stage)

# Second stage regression
second_stage <- lm(forced_mig ~ scaled_sci_2021 + predicted_sci, data = data3)

# Check second stage results
summary(second_stage)

# Calculate F-statistic for instrument strength
f_statistic <- (summary(first_stage)$fstatistic[1]) / (summary(first_stage)$fstatistic[2])
f_statistic

summary(first_stage)$fstatistic[1]
summary(first_stage)$fstatistic[2]


modelivreg = ivreg (log1p(forced_mig) ~ log(scaled_sci_2021) | mci, data = data3)
coeftest(modelivreg, vcov = vcovHC, type = "HC1")

library (car)
vif (first_stage)

summary (modelivreg)


modelivreg2 = ivreg (log1p(forced_mig) ~ log1p(scaled_sci_2021) + log1p(dist) +
                       
                       ## demographic covars
                       log1p (population_orig) + 
                       
                       # political covars (origin)
                       log1p (political_stability_orig) + 
                       log1p (freedom_killings_orig) +
                       log1p (class_equality_lib_orig) +
                       log1p (group_equality_lib_orig) +
                       log1p (civilsoc_repression_orig) + 
                       log1p (relig_org_repression_orig) + 
                       log1p (freedom_religion_orig) +
                       log1p (equality_law_orig) + 
                       log1p (conflict_international_orig) + 
                       log1p (conflict_internal_orig) + 
                       log1p (equal_protection_orig) +
                       
                       # political covars (destination)
                       log1p (political_stability_dest) + 
                       log1p (freedom_torture_dest) + 
                       log1p (freedom_killings_dest) + 
                       log1p (group_equality_lib_dest) + 
                       log1p (relig_org_repression_dest) + 
                       log1p (freedom_religion_dest) + 
                       log1p (equality_law_dest) + 
                       log1p (conflict_international_dest) + 
                       log1p (conflict_internal_dest) + 
                       log1p (equal_protection_dest) + 
                       
                       # bilateral covars
                       log1p (contig) +
                       log1p (scaled_sci_2021) +
                       
                       # econ covars
                       log1p (gdp_o) +
                       log1p (gdp_d) + 
                       log1p (labour_part_rate_male_orig) + 
                       log1p (labour_part_rate_female_orig) +
                       log1p (labour_part_rate_total_dest) + 
                       log1p (labour_part_rate_male_dest) + 
                       log1p (labour_part_rate_female_dest) +
                       
                       # cultural-historical covars
                       log1p (comlang_off) +
                       log1p (comcol) + 
                       log1p (comrelig) + 
                       log1p (col_dep) + 
                       log1p (col_dep_ever) + 
                       log1p (sibling) +
                       log1p (sibling_ever) + 
                       
                       # year fixed effects
                       as.factor (year)
                       
                       | mci +
                       
                       ## demographic covars
                       log1p (population_orig) + 
                       
                       # political covars (origin)
                       log1p (political_stability_orig) + 
                       log1p (freedom_killings_orig) +
                       log1p (class_equality_lib_orig) +
                       log1p (group_equality_lib_orig) +
                       log1p (civilsoc_repression_orig) + 
                       log1p (relig_org_repression_orig) + 
                       log1p (freedom_religion_orig) +
                       log1p (equality_law_orig) + 
                       log1p (conflict_international_orig) + 
                       log1p (conflict_internal_orig) + 
                       log1p (equal_protection_orig) +
                       
                       # political covars (destination)
                       log1p (political_stability_dest) + 
                       log1p (freedom_torture_dest) + 
                       log1p (freedom_killings_dest) + 
                       log1p (group_equality_lib_dest) + 
                       log1p (relig_org_repression_dest) + 
                       log1p (freedom_religion_dest) + 
                       log1p (equality_law_dest) + 
                       log1p (conflict_international_dest) + 
                       log1p (conflict_internal_dest) + 
                       log1p (equal_protection_dest) + 
                       
                       # bilateral covars
                       log1p (contig) +
                       log1p (scaled_sci_2021) +
                       
                       # econ covars
                       log1p (gdp_o) +
                       log1p (gdp_d) + 
                       log1p (labour_part_rate_male_orig) + 
                       log1p (labour_part_rate_female_orig) +
                       log1p (labour_part_rate_total_dest) + 
                       log1p (labour_part_rate_male_dest) + 
                       log1p (labour_part_rate_female_dest) +
                       
                       # cultural-historical covars
                       log1p (comlang_off) +
                       log1p (comcol) + 
                       log1p (comrelig) + 
                       log1p (col_dep) + 
                       log1p (col_dep_ever) + 
                       log1p (sibling) +
                       log1p (sibling_ever) + 
                       
                       # year fixed effects
                       as.factor (year), data = data4)


summary (modelivreg2)
