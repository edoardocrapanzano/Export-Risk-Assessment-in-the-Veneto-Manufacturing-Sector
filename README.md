# Export Risk Assessment in the Veneto Manufacturing Sector 🏭

## 📌 Project Overview
This project investigates the structural determinants of export monetary value and pricing power within the Veneto manufacturing ecosystem. By integrating the Gravity Model of international trade with firm-level network topology and institutional risk analysis, the study isolates the drivers of high-value-added exports. The theoretical foundation of this research lies at the intersection of internationalization risk, firm-level heterogeneity, and network topology.

## 🎯 Research Questions
* What are the structural, topological, and institutional determinants of export value in the Veneto manufacturing sector?
* To what extent does the position within the global trade network act as a predictor of the firm’s foreign pricing power?
* How does institutional risk distort the Alchian-Allen quality sorting effect in the Veneto manufacturing sector?

## 🛠️ Tech Stack & Methodology
* **Language:** R version 4.4.1 
* **Libraries/Packages:** tidyverse, readxl, WDI, rnaturalearth, rnaturalearthdata, geosphere, igraph, ineq, fixest, lme4, lmerTest, quantreg, akima, broom, xgboost, rsample, ggplot2, scales, patchwork, plotly.
* **Methodologies:** Principal Component Analysis, Statistical and Econometric Models (LMMs and PPMLs), Machine Learning (XGBoost), Network Analysis, Institutional Risk Analysis.

## 📊 Data
The empirical analysis was conducted using R software and utilized a highly granular, cross-sectional dataset of bilateral export flows from the Veneto region for the 2024 fiscal year. The primary trade microdata, capturing the physical quantity (kg) and monetary value (€) at the firm-destination level, were sourced from the Italian National Institute of Statistics (ISTAT). To account for macroeconomic scale and spatial friction, these flows were merged with country-level GDP data retrieved via the World Bank’s World Development Indicators (WDI) and geodetic distances computed from the Veneto economic centroid using the geosphere R package. Finally, country risk profiles—encompassing both credit risks (Sovereign, Banking, and Corporate) and political risks (Expropriation, Transfer, and Political Violence)—were integrated from SACE (the Italian Export Credit Agency). Rigorous cleaning protocols were implemented to ensure the integrity of the multi-source dataset, ensuring only perfectly matched bilateral observations were retained. To maintain internal consistency, GDP data originally denominated in USD were converted to EUR using the 2024 average exchange rate of 1.08 EUR/USD. To mitigate the extreme right-skewness characteristic of trade data, logarithmic transformations were applied to monetary value and quantity variables. Notably, extreme upper-tail outliers were preserved and managed through non-linear transformations during the feature engineering phase to prevent significant information loss. Data visualizations were generated using Qlik.

## 💡 Key Findings
Structurally, traditional macro-gravity forces retain significant control over absolute trade mass; a destination market’s economic acts as the primary pull factor, while geographical distance imposes a highly stable transport penalty across all specifications. Topologically, a firm’s individual network architecture acts as a vital volume multiplier. A broad extensive margin (the number of foreign countries reached) is an absolute prerequisite for scale accumulation, but the truest driver of long-term export value is firm’s network prestige. Crucially, the impact of network prestige only emerges once high-dimensional fixed effects sweep out the unobserved background advantages of specialized industrial districts. This confirms that individual network capital, rather than mere geographic clustering, dictates modern export success. Institutionally, country-level risk indicators display a positive semi-elasticity, revealing that Veneto's strategic manufacturing sectors actively exploit high-risk/high-yield developing economies, where localized structural gaps generate heavy demand for direct Veneto’s capital goods imports.

## 🚀 How to run the code
The main analysis can be found in the `export_risk_assesment.R` file which can be executed with RStudio. 

[📄 Read the full report within the Master's Thesis (PDF)](./Master_Thesis.pdf)
