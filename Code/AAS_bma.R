h
## AAS summative code: data visualisations =====================================

## preliminaries ---------------------------------------------------------------

## clearing global environment
rm (list = ls())

## installing packages
pacman::p_load (tidyverse, countrycode, naniar, ggthemes, arm, BAS)

## data imports
dat_imputed = read_csv ("dat_imputed.csv")

## fitting baseline model using imputed data -----------------------------------

## covars object
covars <- names (dat_imputed)[-c(1:5)]

## model formula string
formula_string <- paste ("forced_mig ~", paste (covars, collapse = " + "))

## fitting model
bayes_model = bayesglm (formula_string, 
                        data = dat_imputed)

## Bayesian model selection ----------------------------------------------------

## fitting with BIC prior
fit_bms_bic <- bas.lm (bayes_model, prior = "BIC")

## Summary and plot  -----------------------------------------------------------

## summary of BMS results 
summary (fit_bms_bic)

## visualising
bms_fig <- image (fit_bms_bic, 
                  rotate = FALSE, 
                  color = "rainbow", 
                  main = "Figure XX: Bayesian Model Selection results \n\n\n",
                  cex.axis = 0.8)

