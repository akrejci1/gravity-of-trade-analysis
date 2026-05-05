# The Gravity of Trade: An Empirical Analysis of Czech Exports and Imports

This project applies the Gravity Model of International Trade to empirically analyze the factors influencing the volume of Czech exports and imports. By examining cross-sectional data of 177 trading partners for the year 2024, the analysis evaluates how economic mass (GDP), geographic friction (distance), and institutional integration (EU membership) dictate bilateral trade flows.

## Project Description

The Czech Republic is a highly open, export-oriented economy deeply integrated into international value chains. This project seeks to answer three core questions:
1. To what extent do economic size, distance, and EU integration explain absolute trade volumes?
2. What determinants elevate a country into the "Elite Club" (top 20%) of strategic trading partners?
3. Do gravitational forces operate symmetrically for exports and imports?

## Methodologies Applied

The empirical strategy utilizes four econometric models (two for exports, two for imports) to capture both absolute volumes and the probability of strategic partnership.

**1. Log-Linear Gravity Equation (OLS)**
Used to model the absolute volume of trade flows:
```math
\log(\text{trade}_i) = \beta_0 + \beta_1 \log(\text{GDP}_i) + \beta_2 \log(\text{distance}_i) + \beta_3 \text{is\_eu}_i + \varepsilon_i
```

**2. Logistic Regression (Logit)**
Used to model the probability that a country becomes a top-tier (top 20%) strategic partner:
\text{logit}(P(\text{big\_partner}_i = 1)) = \beta_0 + \beta_1 \log(\text{GDP}_i) + \beta_2 \log(\text{distance}_i) + \beta_3 \text{is\_eu}_i $$

Diagnostic tests, including the Breusch-Pagan test, revealed heteroskedasticity. Consequently, HC3 robust standard errors were implemented for OLS estimations, and HC1 robust standard errors for the Logit models.

## Key Findings

* **Asymmetric Distance Friction:** Geographic distance acts as a stronger barrier for exports (-1.428) than for imports (-0.866). Czech exporters face high sensitivity to transportation costs, whereas the economy is willing to source necessary inputs from much more distant global suppliers.
* **The "EU Bonus" in Imports:** For absolute trade volumes, EU membership significantly boosts imports (+274%) but does not mechanically generate a statistically significant "extra" volume of total exports once market size and distance are controlled for. This highlights deep supply chain integration with neighboring states.
* **Gateway to Strategic Partnerships:** While EU membership does not guarantee high absolute export volumes, it drastically increases the log-odds of a country becoming a top 20% strategic export partner. Conversely, for imports, EU membership does not guarantee entry into the strategic elite, reflecting a more globally diversified import dependency.

## Data Sources

* **UN Comtrade:** Export and import FOB volumes (2024).
* **World Bank:** Nominal GDP in USD.
* **CEPII:** Geographic distance between most populated cities.

## How to Run

1. Clone the repository and ensure the `Data/` folder contains the necessary `.csv` and `.xls` files.
2. Install the required R packages if they are not already installed on your system.
3. Run `gravity_model.R` to execute the data extraction, econometric modeling, and visualization generation.

## Technologies

* R
* `tidyverse` (Data manipulation)
* `lmtest` & `car` (Diagnostic testing)
* `sandwich` (Robust covariance matrix estimators)
* `stargazer` (Econometric table generation)
* `pscl` (Pseudo-R² for Logit models)
* `ggplot2` (Data visualization)
