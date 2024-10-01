
# Bound by Faith? Exploring the Association Between Religious Proximity and Forced Migrant Count

## Motivation
This research investigates the relationship between religious proximity and forced migration flows. Understanding how religious ties influence migration patterns is crucial for informing humanitarian policies and forecasting frameworks. The study challenges existing scholarship by suggesting that current models may overlook significant factors, particularly the role of religious connections in shaping forced migration.

## Data
The analysis uses a comprehensive dataset covering bilateral forced migrant flows from origin country $i$ to destination country $j$ over the period from 1977 to 2023. The dataset includes measures of religious proximity, geographical distance, and various socio-political and economic covariates. Due to the absence of a pre-existing dataset, I constructed this dataset from reputable sources, addressing asymmetry issues in UNHCR data and employing iterative imputation to handle missing values.

## Method
I use a Zero-Inflated Negative Binomial (ZINB) regression model to analyse the data. This method is suitable for count data that exhibit overdispersion and excess zeros, which are common in migration studies. The model accounts for various covariates reflecting push, pull, and interaction factors influencing migration flows. 

## Findings
A one unit increase in the Religious Proximity Index between $i$ and $j$ is associated with an increase in the count of forced migrants between $i$ and $j$ by a factor of 2.435. With a p-value below 2 * 10^−16, this result is statistically significant at the 95% confidence level. Despite the model's limitations, including sensitivity to imputation methods and the lack of time variation, the findings contribute to a deeper understanding of the factors driving forced migration. The study calls for a methodological review of existing forecasting frameworks to incorporate religious ties as a critical factor in predicting migration flows.

## Future Research
The paper highlights the need for further research to explore the dynamics of forced migration in relation to religious ties, particularly considering temporal variations and the robustness of different modeling approaches.
