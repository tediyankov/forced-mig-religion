
## AAS summative code: exploratory analysis pre-modelling ======================

## preliminaries ---------------------------------------------------------------

## clearing global environment
rm (list = ls())

## installing packages
pacman::p_load (tidyverse, countrycode, naniar, ggthemes, corrplot, robustbase, gridExtra)
library (robustbase)

## data imports
dat_imputed = read_csv ("dat_imputed.csv")
dat_unimputed = read_csv ("merged_dat.csv")

## NAs matrix pre-imputation ---------------------------------------------------

## code for matrix plot
vis_miss (dat_unimputed, warn_large_data = FALSE) + 
  labs (title = "Figure XX: unimputed data NaNs matrix") + 
  ggthemes::theme_few() + 
  theme (axis.text.x.top = element_text (angle = 90, hjust = 1, vjust = 0.5))

## post-BMS data cleaning ------------------------------------------------------
## (see AAS_bma.R)

## removing some vars from data
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

## standardising
dat_imputed_clean[, -c(1:5)] <- scale (dat_imputed_clean[, -c(1:5)])
dat_imputed_clean[, -c(1:5)] <- scale (dat_imputed_clean[, -c(1:5)], 
                                       center = FALSE, 
                                       scale = apply (dat_imputed_clean[, -c(1:5)], 2, max) - 
                                         apply (dat_imputed_clean[, -c(1:5)], 2, min))

columns_to_scale = 6:ncol(dat_imputed_clean)

dat_imputed_clean[, columns_to_scale] <- scale(
  dat_imputed_clean[, columns_to_scale], 
  center = apply(dat_imputed_clean[, columns_to_scale], 2, min), 
  scale = apply(dat_imputed_clean[, columns_to_scale], 2, max) - apply(dat_imputed_clean[, columns_to_scale], 2, min)
)


## correlation matrix ----------------------------------------------------------

## preparing data for correlation matrix
dat_imputed_clean_corplot = dat_imputed_clean %>% 
  
  # removing id-vars
  dplyr::select (-c(1:5)) 

## correlation matrix 
cormatrix <- cor (dat_imputed_clean_corplot)

## colour palette for plotting
col <- colorRampPalette (
  c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA")
  )

corrplot (cormatrix, 
          method = "color", 
          col = col(200),  
          type = "upper", 
          order = "hclust",
          tl.col = "black", 
          tl.srt = 45, 
          sig.level = 0.01, 
          insig = "blank", 
          diag = FALSE 
)

## histogram of main vars of interest ------------------------------------------
  
## forced_mig

# column name
column_name <- "forced_mig"

# computing quantiles for the middle 90%
lower_quantile <- quantile (dat_imputed[[column_name]], 0.05)
upper_quantile <- quantile (dat_imputed[[column_name]], 0.95)

# plotting histogram for the middle 90%
ggplot (data = dat_imputed, 
        aes (x = dat_imputed[[column_name]])) +
  geom_histogram (aes(y = ..density..), bins = 100) +
  geom_density (color = "lightblue", linewidth = 1) +
  ggthemes::theme_few() +
  xlim (lower_quantile, upper_quantile) + 
  ylim (0, 0.00008) + 
  labs (title = "Figure XX: Density plot of forced_mig", 
        x = "Number of forced migrants")

## comrelig

# column name
column_name <- "comrelig"

# computing quantiles for the middle 90%
lower_quantile <- quantile (dat_imputed[[column_name]], 0.05)
upper_quantile <- quantile (dat_imputed[[column_name]], 0.95)

# plotting histogram for the middle 90%
ggplot (data = dat_imputed, 
        aes (x = comrelig)) +
  geom_histogram (aes(y = ..density..), bins = 100) +
  geom_density (color = "lightblue", linewidth = 1) +
  ggthemes::theme_few() +
  #xlim (lower_quantile, upper_quantile) + 
  #ylim (0, 0.00008) + 
  labs (title = "Figure XX: Density plot of comrelig", 
        x = "Religious Proximity Index")

lower_quantile <- quantile (dat_imputed$scaled_sci_2021, 0.05)
upper_quantile <- quantile (dat_imputed$scaled_sci_2021, 0.95)

ggplot (data = dat_imputed, 
        aes (x = scaled_sci_2021)) +
  geom_histogram (aes(y = ..density..), bins = 100) +
  geom_density (color = "lightblue", linewidth = 1) +
  ggthemes::theme_few() +
  xlim (lower_quantile, upper_quantile) + 
  #ylim (0, 0.00008) + 
  labs (title = "Figure XX: Density plot of scaled_sci_2021", 
        x = "Religious Proximity Index")

## exporting clean df
write.csv (dat_imputed_clean, "dat_imputed_clean.csv", row.names = F)

## scatter plot of forced_mig and comrelig
scatter1 <- ggplot (data = dat_imputed_clean, 
        aes (x = comrelig, y = forced_mig)) + 
  geom_jitter () +  
  geom_smooth (method = "lm", col = "red", se = FALSE) +  
  labs (
    title = "Figure XX: Scatter plot of forced_mig vs comrelig",
    x = "Religious Proximity Index, scaled",
    y = "Forced migrant count, scaled"
  ) + 
  theme_few()
  
## scatter plot of forced_mig and comrelig
scatter2 <- ggplot (data = dat_imputed_clean, 
        aes (x = log1p(comrelig), y = log1p(forced_mig))) + 
  geom_jitter () +  
  geom_smooth (method = "lm", col = "red", se = FALSE) +  
  labs (
    title = "Figure XX: Scatter plot of forced_mig vs comrelig",
    x = "Religious Proximity Index, scaled + log transformed",
    y = "Forced migrant count, scaled + log transformed"
  ) +
theme_few()

scatter3 <- ggplot (data = dat_imputed, 
                    aes (x = comrelig, y = forced_mig)) + 
  geom_jitter () +  
  geom_smooth (method = "lm", col = "red", se = FALSE) +  
  labs (
    title = "Figure XX: Scatter plot of forced_mig vs comrelig",
    x = "Religious Proximity Index, scaled + log transformed",
    y = "Forced migrant count, scaled + log transformed"
  ) +
  theme_few()

scatter4 <- ggplot (data = dat_imputed, 
                    aes (x = log1p(comrelig), y = log1p(forced_mig))) + 
  geom_jitter () +  
  geom_smooth (method = "lm", col = "red", se = FALSE) +  
  labs (
    title = "Figure XX: Scatter plot of forced_mig vs comrelig",
    x = "Religious Proximity Index, scaled + log transformed",
    y = "Forced migrant count, scaled + log transformed"
  ) +
  theme_few()

## plotting as one figure
grid.arrange (scatter1, scatter2, scatter3, scatter4, ncol = 4)

 












