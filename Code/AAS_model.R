
## AAS summative code: modelss =================================================

## preliminaries ---------------------------------------------------------------

# clearing global environment
rm (list = ls())

## installing packages
pacman::p_load (tidyverse, countrycode, naniar, ggthemes, arm, BAS, nnet, mlogit, 
                gridExtra, lmtest, sandwich)

## data imports
dat_imputed_clean = read_csv ("dat_imputed_clean.csv")

## multinomial linear regression ------------------------------------------------

## training model
linearModel = lm (log1p(forced_mig) ~ log1p(dist) +
                    
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
                    as.factor (year),
                  
                  data = dat_imputed_clean
                  )

## model summary
summary (linearModel)

linearModel_base = lm (log1p(forced_mig) ~ log1p(comrelig),
                       data = dat_imputed_clean
)

bptest (linearModel_base, studentize = T)

## residuals plot --------------------------------------------------------------

## function for plotting random sample of them
residualPlot = function (model, title) {
  
  ## residuals and fitted values
  residuals <- residuals (model)
  fitted_values <- fitted (model)
  
  ## extracting sample indices
  sample_indices <- sample (length(residuals), 2000)
  
  ## sample data
  sampled_data <- data.frame(
    residuals = residuals [sample_indices],
    fitted_values = fitted_values [sample_indices]
  )
  
  ## plotting
  ggplot (sampled_data, aes (x = fitted_values, y = residuals)) +
    geom_point (size = 0.5) +
    geom_smooth (method = "lm", col = "red", se = FALSE) +
    labs(
      x = "Fitted Values",
      y = "Residuals",
      title = title
    ) + 
    theme_few()
  
}

## creating 3 plots
plot1 <- residualPlot (linearModel, "Residuals vs. Fitted Values (Sample 1)")
plot2 <- residualPlot (linearModel, "Residuals vs. Fitted Values (Sample 2)")
plot3 <- residualPlot (linearModel, "Residuals vs. Fitted Values (Sample 3)")

## arranging into one figure
grid.arrange (plot1, plot2, plot3, ncol = 3)

## Breusch-Pagan Test for heteroscedasticity
bptest (linearModel, studentize = T)

## re-visiting model to fix heteroscedasticity ---------------------------------

## Weighted Least Squares
weights <- 1 / residuals (linearModel)^2
WLSModel = lm (log1p(forced_mig) ~ log1p(dist) +
                 
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
                 as.factor (year),
               
               data = dat_imputed_clean,
               weights = weights
)

## summary
summary (WLSModel)

## redoing the residuals
plot1.2 <- residualPlot (WLSModel, "Residuals vs. Fitted Values (Sample 1)")
plot2.2 <- residualPlot (WLSModel, "Residuals vs. Fitted Values (Sample 2)")
plot3.2 <- residualPlot (WLSModel, "Residuals vs. Fitted Values (Sample 3)")

## arranging into one figure
grid.arrange (plot1.2, plot2.2, plot3.2, ncol = 3)

## Breusch-Pagan Test for heteroscedasticity
bptest (WLSModel, studentize = T)

## robust standard errors
robust_se <- sqrt(diag(vcovHC(WLSModel)))
comrelig_coef <- coef (WLSModel)["log1p(comrelig)"]
robust_t_stat_comrelig <- comrelig_coef / robust_se["log1p(comrelig)"]
p_value_comrelig <- 2 * (1 - pt(abs(robust_t_stat_comrelig), df = df.residual(WLSModel)))
conf_interval_comrelig <- comrelig_coef + cbind(qnorm(0.025), qnorm(0.975)) * robust_se["log1p(comrelig)"]
summary(WLSModel)
cbind(robust_t_stat_comrelig, p_value_comrelig, conf_interval_comrelig)

## cases zoom in ---------------------------------------------------------------

## function for training model
modelTrain = function (data) {
  
  ## linear model
  linearModel = lm (log1p(forced_mig) ~ log1p(dist) +
                      
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
                      as.factor (year),
                    
                    data = data
  )
  
  ## Weighted model
  ## Weighted Least Squares
  weights <- 1 / residuals (linearModel)^2
  WLSModel = lm (log1p(forced_mig) ~ log1p(dist) +
                   
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
                   as.factor (year),
                 
                 data = data,
                 weights = weights
  )
  
  return (WLSModel)
  
}

## Rohingya refugee crisis

# prepping data 
rohingya_dat = dat_imputed_clean %>%
  
  # filtering for only MMR in orig
  filter (orig == "MMR")

# model
mmr_model = modelTrain (rohingya_dat)
summary (mmr_model)

## Syrian civil war

# prepping data 
syrian_dat = dat_imputed_clean %>%
  
  # filtering for only MMR in orig
  filter (orig == "SYR") %>%
  filter (year >= 2011)

# model
syr_model = modelTrain (syrian_dat)
summary (syr_model)

## Somalian war

# prepping data 
somalian_dat = dat_imputed_clean %>%
  
  # filtering for only MMR in orig
  filter (orig == "SOM") %>%
  filter (year >= 1991)

# model
som_model = modelTrain (somalian_dat)
summary (som_model)


## trying with only log transformed data
dat_imputed_clean = dat_imputed %>%
  
  # removing variables with PIP > 0.5
  dplyr::select (-c(freedom_torture_orig,
                    class_equality_lib_dest,
                    civilsoc_repression_dest,
                    population_dest,
                    gdpcap_o,
                    gdpcap_d,
                    comlang_ethno,
                    labour_part_rate_total_orig))




