#Uploading libraries 
if (!require("readxl")) install.packages("readxl")
if (!require("rnaturalearth")) install.packages("rnaturalearth") # For geographic coordinates
if (!require("geosphere")) install.packages("geosphere") # For distance calculation 
if (!require("WDI")) install.packages("WDI") # For GDP data (World Development Indicators)
if (!require("tidyverse")) install.packages("tidyverse") # Collection of data science packages
if (!require("igraph")) install.packages("igraph")# Collection of network analysis tools
if (!require("scales")) install.packages("scales")
if (!require("plotly")) install.packages("plotly") #3D visualizations
if (!require("mgcv")) install.packages("mgcv")
if (!require("lubridate")) install.packages("lubridate")
if (!require("lme4")) install.packages("lme4")  # For Linear Mixed Models (lmer)
if (!require("lmerTest")) install.packages("lmerTest") # To get p-values in the lmer summary
if(!require(c("xgboost", "rsample", "purrr"))) install.packages(c("xgboost", "rsample", "purrr"))
if (!require("fixest")) install.packages("fixest") # The gold-standard for fast PPML with high-dimensional FEs

library(xgboost)
library(rsample)
library(readxl)
library(tidyverse)
library(rnaturalearth)
library(rnaturalearthdata)
library(geosphere)     
library(WDI)          
library(igraph)
library(scales)
library(plotly)
library(mgcv)
library(lubridate)
library(lme4)     
library(lmerTest) 
library(xgboost)
library(rsample)
library(purrr)
library(quantreg)
library(broom)
library(patchwork)
library(ineq)
library(fixest)
library(akima)



# ----- Data Preparation & Cleaning -----#

# Uploading data 
dataset_2024 = read.csv("VEN_2024_anonymous.csv")
risk_indicators = read_excel("Storico Indicatori country risk 2025.xlsx", sheet="Sintesi")
credit_risk_2024 = read_excel("Storico Indicatori country risk 2025.xlsx", sheet="credito24")
politic_risk_2024 = read_excel("Storico Indicatori country risk 2025.xlsx", sheet="politico24")
country_ID = read.delim("Codici_Paesi.txt")

# Adding country name to the anonymous data set
matching_country = function(code, dictionary) {
  positions = match(code, dictionary$PAESE)
  return(dictionary$DESCR_PAE[positions])
}

dataset_2024 = dataset_2024 %>%
  mutate(Paese = matching_country(paese, country_ID))%>%
  rename(Paese_ID = paese)

# Converting some country names for good matching in the country risk data set (where names vary little bit)
dataset_2024 = dataset_2024 %>%
  mutate(Paese = recode(Paese, "Repubblica ceca" = "Ceca (Repubblica)", "Slovacchia" = "Slovacchia,Repubblica",
                        "Sud Africa" = "Sudafricana (Repubblica)", "Stati Uniti" = "Stati Uniti D'america",
                        "Repubblica moldova" = "Moldavia", "Macedonia del Nord" = "Macedonia",
                        "Kazakhstan" = "Kazakistan", "Costa d'Avorio" = "Costa D'avorio",
                        "Maurizio" = "Mauritius Isole", "Repubblica islamica dell'Iran" = "Iran",
                        "Corea del Sud" = "Corea Del Sud", "Bahrein" = "Bahrain",
                        "Repubblica democratica del Congo" = "Congo (Repubblica Democratica Del)",
                        "Repubblica dominicana" = "Dominicana (Repubblica)",
                        "Trinidad e Tobago" = "Trinidad E Tobago", "Isole Cayman" = "Cayman (Isole)"))

# Changing letters of country names for appropriate matching
risk_indicators$Paese = str_to_title(risk_indicators$Paese)

# Adding country ISO (3 letters) to credit and politic risk data sets
matching_ISO = function(code, dictionary) {
  positions = match(code, dictionary$Paese)
  return(dictionary$ISO[positions])
}

credit_risk_2024 = credit_risk_2024 %>%
  mutate(ISO = matching_ISO(Paese, risk_indicators))

# Adding ISO where there is a missing value
credit_risk_2024 = credit_risk_2024 %>%
  select(-`ISO 2`) %>%
  mutate(ISO = case_when(!is.na(ISO) & ISO != "" ~ ISO,
    Paese == "Andorra" ~ "AND", Paese == "Groenlandia" ~ "GRL",
    Paese == "Liechtenstein" ~ "LIE", Paese == "Macedonia" ~ "MKD",
    Paese == "Russia" ~ "RUS", Paese == "Sahara Occidentale" ~ "ESH",
    Paese == "St. Kitts E Nevis" ~ "KNA"))

politic_risk_2024 = politic_risk_2024 %>%
  mutate(ISO = matching_ISO(Paese, risk_indicators))

# Adding ISO where there is a missing value
politic_risk_2024 = politic_risk_2024 %>%
  select(-`ISO 2`) %>%
  mutate(ISO = case_when(!is.na(ISO) & ISO != "" ~ ISO,
    Paese == "Andorra" ~ "AND", Paese == "Groenlandia" ~ "GRL",
    Paese == "Liechtenstein" ~ "LIE", Paese == "Macedonia" ~ "MKD",
    Paese == "Russia" ~ "RUS", Paese == "Sahara Occidentale" ~ "ESH",
    Paese == "St. Kitts E Nevis" ~ "KNA"))
                         

# Creating the full data set  
dataset_2024 = dataset_2024 %>%
  left_join(credit_risk_2024, by = "Paese")%>%
  left_join(politic_risk_2024, by = c("Paese", "ISO"))


# Define the ATECO 2007 table 
ateco_df = data.frame(Sezione = c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M", "N", "O", "P", "Q", "R", "S", "T", "U"),
  Codici_2_digit = c("01-03", "05-09", "10-33", "35", "36-39", "41-43", "45-47", "49-53", "55-56", "58-63", "64-66", "68", "69-75", "77-82", "84", "85", "86-88", "90-93", "94-96", "97-98", "99"),
  Dizione_ATECO_2007 = c("Agricoltura, silvicoltura e pesca", "Estrazione di minerali da cave e miniere", "Attività manifatturiere", 
    "Fornitura di energia elettrica, gas, vapore e aria condizionata", "Fornitura di acqua; reti fognarie, gestione rifiuti e risanamento", 
    "Costruzioni", "Commercio all’ingrosso e al dettaglio; riparazione di autoveicoli e motocicli", "Trasporto e magazzinaggio", 
    "Attività dei servizi di alloggio e di ristorazione", "Servizi di informazione e comunicazione", "Attività finanziarie e assicurative", 
    "Attività immobiliari", "Attività professionali, scientifiche e tecniche", "Noleggio, agenzie di viaggio, servizi di supporto alle imprese", 
    "Amministrazione pubblica e difesa; assicurazione sociale obbligatoria", "Istruzione", "Sanità e assistenza sociale", 
    "Attività artistiche, sportive, di intrattenimento e divertimento", "Altre attività di servizi", 
    "Attività di famiglie e convivenze come datori di lavoro; produzione per uso proprio", "Organizzazioni ed organismi extraterritoriali"),
  ATECO_Settore = c("Agricoltura", "Estrazione", "Manifattura", "Energia", "Ambiente", "Costruzioni", "Commercio",
    "Trasporti", "Turismo", "Comunicazione", "Finanza", "Immobili", "Professioni", "Servizi",
    "Pubblica amm.", "Istruzione", "Sanità", "Cultura", "Servizi", "Famiglie", "Organizzazioni"))

# Expand the dataframe to make a 1-to-1 lookup key
ateco_lookup = ateco_df %>%
  rowwise() %>%
  mutate(limits = list(as.numeric(unlist(strsplit(Codici_2_digit, "[-–]")))), # Split the string by dash or en-dash. If no dash (e.g., "35"), it just returns the number.
    codice_num = list(seq(limits[1], limits[length(limits)]))) %>%  # Generate the sequence of numbers (e.g., limits c(10, 33) becomes 10, 11, ..., 33)
  unnest(codice_num) %>%  # Expand the list into separate rows
  mutate(codice_2_digit_match = sprintf("%02d", codice_num)) %>%  # Format as a 2-character string with leading zeros (e.g., 1 -> "01") to match text matching
  select(-limits, -codice_num) %>%
  ungroup()

head(ateco_lookup) 


dataset_2024 = dataset_2024 %>%
  mutate(ateco_prefix = substr(str_pad(ateco2007, width = 4, pad = "0"), 1, 2)) %>%  # Ensure the ateco code is padded to 4 characters (in case of leading zeros dropped) and extract the first 2 characters
  left_join(ateco_lookup, by = c("ateco_prefix" = "codice_2_digit_match"))
      



# ----- Data Reduction & Aggregation -----#

# Filtering out Export data (movement type 9) for trade network analysis and sum up all the values 
# for each Firm-Country relationship occurred in 2024 regardless of the quarter
df_export = dataset_2024 %>%
  filter(movim == 9 & ATECO_Settore == 'Manifattura') %>%  # Keep only export flows in manufacturing sector
  select(firm_ID, Paese, ISO, ateco2007, provinci, val, qua, Sovrano, Bancario, Corporate,media_credito, Esproprio, Trasferimento, Violenza, media_politico)%>%
  group_by(firm_ID, Paese, ISO) %>%
  summarise(total_val = sum(val, na.rm = TRUE), total_qua = sum(qua, na.rm = TRUE), 
            Sovrano = mean(Sovrano, na.rm = TRUE), Bancario = mean(Bancario, na.rm = TRUE),
            Corporate = mean(Corporate, na.rm = TRUE), media_credito = mean(media_credito, na.rm = TRUE),
            Esproprio = mean(Esproprio, na.rm = TRUE), Trasferimento = mean(Trasferimento, na.rm = TRUE), 
            Violenza = mean(Violenza, na.rm = TRUE), media_politico = mean(media_politico, na.rm = TRUE),
            .groups = "drop")

df_export$firm_ID = as.character(df_export$firm_ID)


# Adding Gravity data: geographical distance (km) from Veneto and GDP(Usd) of destination country
iso_codes = credit_risk_2024$ISO

# Define Veneto Central Coordinates (Long, Lat)
# We use a point near Padua (approx. 11.87, 45.41) as the regional center
Veneto_coords = c(11.87, 45.41)

# Prepare Global Geographic Data
# We create a mapping column to resolve ISO code conflicts
world_geo = ne_countries(scale = "medium", returnclass = "sf") %>%
  as.data.frame() %>%
  select(iso_a3, lon = label_x, lat = label_y)

# Retrieve GDP Data (World Development Indicators)
#options(timeout = 300)
#gdp_info = WDI(indicator = "NY.GDP.MKTP.CD", country = "all", start = 2024, end =2024) %>%
  #select(iso_a3 = iso3c, GDP_Usd = NY.GDP.MKTP.CD)

# Retrieve Population Data for 2024
#pop_info = WDI(indicator = "SP.POP.TOTL", country = "all", start = 2024, end = 2024) %>%
  #select(iso_a3 = iso3c, Population = SP.POP.TOTL)

# Saving the GDP data in the case in which the API doesn't work 
# write.csv(gdp_info, "WDI_GDP_2024.csv", row.names = FALSE) 
# write.csv(pop_info, "WDI_POP_2024.csv", row.names = FALSE) 
gdp_info = read.csv("WDI_GDP_2024.csv")
pop_info = read.csv("WDI_POP_2024.csv")

# Create the data frame with the ISO codes, distances and GDP values
final_df = data.frame(ISO = iso_codes) %>%
  left_join(world_geo, by = c("ISO" = "iso_a3")) %>%
  left_join(gdp_info, by = c("ISO" = "iso_a3")) %>%
  rowwise() %>%
  mutate(Distance_km = if_else(!is.na(lon), round(distHaversine(Veneto_coords, c(lon, lat)) / 1000, 0), NA_real_)) %>%
  ungroup() %>%
  select(ISO, Distance_km, GDP_Usd)

# Adding missing distances manually (since the package doesn't recognize FRA, UVK, NOR, WBG)
final_df = final_df %>%
  mutate(Distance_km = case_when(ISO == "FRA" ~ 740, ISO == "NOR" ~ 1630,
    ISO == "UVK" ~ 800, ISO == "WBG" ~ 2400, TRUE ~ Distance_km))

# Adding missing GDPs and Pop manually (since the package doesn't retrieve different foreign countries)
final_df = final_df %>%
  mutate(GDP_Usd = case_when(ISO == "AFG" ~ 1.808e10, ISO == "BTN" ~ 3.1e9,
                             ISO == "CYM" ~ 7.14e9, ISO == "PRK" ~ 1.645e10,
                             ISO == "CUB" ~ 2.0199e11, ISO == "ERI" ~ 2.28e9,
                             ISO == "GRL" ~ 3.33e9, ISO == "UVK" ~ 1.115e10,
                             ISO == "LBN" ~ 2.828e10, ISO == "LIE" ~ 8.5e9,
                             ISO == 'WBG' ~ 1.371e10, ISO == "ESH" ~ 1.115e10,
                             ISO == "SYR" ~ 1.999e10, ISO == "SSD" ~ 4.65e9,
                             ISO == 'TWN' ~ 8.84e11, ISO == "YEM" ~ 1.91e10,
                             TRUE ~ GDP_Usd))

final_df = final_df %>%
  left_join(pop_info, by = c("ISO" = "iso_a3")) %>%
  mutate(Population = case_when(ISO == "UVK" ~ 1.6e6, ISO == "ESH" ~ 5.9e5,
                                ISO == "WBG" ~ 5.5e6, ISO == "TWN" ~ 2.39e7, 
                                TRUE ~ Population))

# Adding the GDP values expressed in Euro (Average Eur/Usd exchange rate in 2024: 1.08 ---> this could be updated)
rate_eur_usd = 1.08

final_df = final_df %>%
  mutate(GDP_Eur = GDP_Usd/rate_eur_usd)


# Joining distances and GDP values to our Export data set
df_export = df_export %>%
  left_join(final_df, by = 'ISO')


str(df_export)
summary(df_export)


# Plot Log-Transformed Total Value because raw data follow Power-Law distribution with extreme right skewness
ggplot(df_export, aes(x = log(total_val+1))) +
  geom_histogram(bins = 60, fill = "#CD3333", color = "white", alpha = 0.8) +
  theme_minimal() +
  labs(
    x = "Log(Monetary Value + 1)",
    y = "Frequency")

# Plot Log-Transformed Total Quantity because raw data follow Power-Law distribution with extreme right skewness
ggplot(df_export, aes(x = log(total_qua+1))) +
  geom_histogram(bins = 60, fill = "#007BA7", color = "white", alpha = 0.8) +
  theme_minimal() +
  labs(title = "Log-Transformed Export Traded Quantity",
       subtitle = "Approaching Normal Distribution",
    x = "Log(Physical Quantity + 1)",
    y = "Frequency")

# Log-Log Plot
ggplot(df_export, aes(x = log(total_qua+1), y = log(total_val+1))) +
  geom_point(alpha = 0.1, color = "darkblue", size = 0.5) +
  geom_smooth(method = "lm", color = "red", linewidth = 1, se = FALSE) +
  theme_minimal() +
  labs(title = "Log-Log Relationship between Export Quantity and Value",
    subtitle = "Visualizing the Elasticity of Bilateral Trade Flows of Veneto's Firms",
    x = "Log of Physical Quantity",
    y = "Log of Monetary Value")




# ----- iGraph Construction -----#

# Create the graph from the edge list
g = graph_from_data_frame(df_export, directed = TRUE)

# Assign Node Types: get the unique list of all countries in our dataset
unique_countries = unique(df_export$Paese)
unique_firms = unique(df_export$firm_ID)


# If the node's name is in the country list, it's a "Country", otherwise it's a "Firm"
V(g)$node_type = ifelse(V(g)$name %in% unique_countries, "Country", "Firm")


# Centrality measures (out_degree, strength, closeness, eigenvector) measure a crucial concept
# in the Trade Network which is the "Network Influence and Topology".

# OUT-DEGREE: Number of distinct countries a firm exports to
out_degree = degree(g, mode = "out")

# OUT-STRENGTH: Total quantity exported by a firm (Weighted Out-Degree)
out_strength = strength(g, mode = "out", weights = E(g)$total_qua) 

# CLOSENESS: it has an econometric value because it acts as a proxy for "market integration." 
# Firms with high closeness might be more resilient to localized economic shocks 
# because they are highly integrated into the core global network. 
closeness = closeness(g, mode = "all", normalized = TRUE)

# EIGEN-VECTOR: this is an excellent indicator of "quality of connections" or "market influence." 
# It differentiates a firm that exports small amounts to many minor countries from a firm that is 
# deeply entrenched in the most lucrative, dominant global supply chains.
eigenvector = eigen_centrality(g, weights = E(g)$total_val)$vector # weighted by monetary value of export

# Define the function to extract Top 5 nodes 
get_top_5 = function(metric_vector) {
  sorted_metric = sort(metric_vector, decreasing = TRUE)
  top_5 = head(sorted_metric, 5)
  ids = names(top_5)
  return(list(ID = ids, Value = round(top_5, 3)))
}

# Extract Top 5 for each metric
top_deg = get_top_5(out_degree)
top_strength = get_top_5(out_strength)
top_clo = get_top_5(closeness)
top_eig = get_top_5(eigenvector)

# Create a comparison table for the current quarter
comparison_table = data.frame(Rank = 1:5, Deg_ID = top_deg$ID, Deg_Value = top_deg$Value,
                              Strength_ID = top_strength$ID, Strength_Value = top_strength$Value,
                              Close_ID = top_clo$ID, Close_Value = top_clo$Value,
                              Eigen_ID = top_eig$ID, Eigen_Val = top_eig$Value)
# Print the results
row.names(comparison_table) = NULL
print(comparison_table)

# Extract and Separate the Metrics
# Create a master dataframe of all node metrics
node_metrics = data.frame(firm_ID = V(g)$name, out_degree = out_degree, out_strength = out_strength,
                          closeness = closeness, eigenvector = eigenvector)
  



# ----- Multicollinearity & Dimensionality Reduction (PCA) ----- #

## Isolate the unique country risk profiles of 2024
country_risk = df_export %>%
  select(Paese, Sovrano, Bancario, Corporate, Esproprio, Trasferimento, Violenza) %>%
  distinct() %>%  # Keep only unique Country-Year combinations
  drop_na()       # Remove rows with NA values because PCA algorithms cannot handle NA values natively

# Separate the identifiers from the numeric variables to check for multicollinearity
risk_vars = country_risk %>% 
  select(-Paese)

# Calculate the correlation matrix
cor_matrix = cor(risk_vars)

# Reshape the matrix into a "long" format for ggplot
cor_long_df1 = cor_matrix %>%
  as.data.frame() %>%
  rownames_to_column(var = "Var1") %>%
  pivot_longer(cols = -Var1, names_to = "Var2", values_to = "Correlation")

# Plot the Heatmap
ggplot(cor_long_df1, aes(x = Var1, y = Var2, fill = Correlation)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0, limit = c(-1,1)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Correlation Heatmap of country risk indicators", x = "", y = "")


# Run the PCA (centering and scaling the country risk indicators)
# The 8 risk variables all measure one concept: "Macroeconomic/Political Danger" and are highly correlated
# so we need to create a principal component that is a combination of original features/variables 
pca_result = prcomp(risk_vars, center = TRUE, scale. = TRUE)

# Check the variance explained by each component
summary(pca_result)

# Check how the variables are weighted
pca_result$rotation

# Extract the first principal component (PC1) scores
country_risk$Risk_Index_PC1 = pca_result$x[, 1]

# Standardize the new PC1 index (z-score) for the econometric model
country_risk = country_risk %>%
  mutate(Risk_Index_Standardized = scale(Risk_Index_PC1)[,1])



# Extract just the centrality measures 
centrality_vars = node_metrics %>%
  select(out_degree, out_strength, closeness, eigenvector) %>%
  drop_na() # Ensure no missing values

# Calculate and view the correlation matrix
cor_matrix_cent = cor(centrality_vars)

# Reshape the matrix into a "long" format for ggplot
cor_long_df2 = cor_matrix_cent %>%
  as.data.frame() %>%
  rownames_to_column(var = "Var1") %>%
  pivot_longer(cols = -Var1, names_to = "Var2", values_to = "Correlation")

# Plot the Heatmap
ggplot(cor_long_df2, aes(x = Var1, y = Var2, fill = Correlation)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0, limit = c(-1,1)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Correlation Heatmap of centrality measures", x = "", y = "")




#----- Standardization & Econometric Modeling -----

# We plan to use a Linear Mixed Model (LMM) with val as the dependent variable and we need to standardize 
# our independent variables, especially since we are including quantities traded, distances of foreign countries,
# GDP of foreign countries, PCA scores (capture information about country risk indicators) and centrality measures. 
# This ensures our model coefficients are directly comparable, allowing to say, "a 1 standard deviation increase in 
# PC1 (Risk) has this effect compared to a 1 standard deviation increase in network centrality. 
# We apply a Linear Mixed Model (LMM) because we have multiple firms trading with multiple countries
# and we use random intercepts for firm_ID and Paese_ID to account for unobserved idiosyncratic heterogeneity and 
# serial correlation (e.g., some firms are just inherently better at exporting, 
# and some countries are inherently harder to export to, regardless of the risk indicators).

# Merge the Export data (subset of original data set), firm centrality measures, and country risk PC1
final_model_data = df_export %>%
  left_join(node_metrics, by = "firm_ID") %>%
  left_join(country_risk %>% select(Paese, Risk_Index_Standardized), by = "Paese")

# Scale the remaining independent variables so that their coefficients are comparable 
final_model_data = final_model_data %>%
  mutate(out_degree_std = scale(out_degree)[,1], 
         out_strength_std = scale(out_strength)[,1], 
         eigen_std = scale(eigenvector)[,1])

# Drop any rows where the PCA risk index is missing so the regression runs smoothly
final_model_data = final_model_data %>% filter(!is.na(Risk_Index_Standardized))

final_model_data = final_model_data %>% filter(!is.na(total_val) & is.finite(total_val))
                           
# Fit the Linear Mixed Model : log-transform because export values/distances/GDP values are highly skewed
model_1 = lmer(log(total_val) ~  log(Distance_km) + log(GDP_Eur) + (1 | firm_ID)  + (1 | Paese), data = final_model_data, REML = FALSE)

summary(model_1)

model_2 = lmer(log(total_val) ~ log(Distance_km) + log(GDP_Eur) + Risk_Index_Standardized + 
                   out_degree_std + out_strength_std + eigen_std + (1 | firm_ID)  + (1 | Paese), data = final_model_data, REML = FALSE)

summary(model_2)

#Comparison between the nested models with export value as response variable
anova(model_1, model_2)


# Extract Fitted values and Residuals from the LMM
# We create a new dataframe specifically for the diagnostics
diag_data = data.frame(Fitted = fitted(model_2), Residuals = resid(model_2))

# Plot 1: Residuals vs. Fitted (Checking Homoscedasticity)
png(filename = "Residuals vs Fitted.png", width = 1000, height = 600)
ggplot(diag_data, aes(x = Fitted, y = Residuals)) +
  geom_point(alpha = 0.3, color = "black") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red", linewidth = 1) +
  theme_minimal() +
  labs(title = "Residuals vs Fitted Values", subtitle = "Checking for Homoscedasticity of Residuals",
    x = "Fitted Values (Predicted Export Value)", y = "Residuals" )
dev.off()

# Plot 2: Q-Q Plot (Checking Normality of Residuals)
png(filename = "Q-Q Plot.png", width = 1000, height = 600)
ggplot(diag_data, aes(sample = Residuals)) +
  stat_qq(alpha = 0.3, color = "black") +
  stat_qq_line(color = "red", linewidth = 1) +
  theme_minimal() +
  labs(title = "Normal Q-Q Plot", subtitle = "Checking for Normality of Residuals",
    x = "Theoretical Quantiles", y = "Sample Quantiles (Residuals)")
dev.off()


# Shifting our dependent variable from total export value to export unit value (price per kg), 
# so moving from a standard gravity model to a model that explicitly tests for pricing power, 
# export quality, and the Alchian-Allen effect (shipping the good apples out: states that 
# a fixed per-unit cost, like shipping or taxes, applied to both high- and low-quality goods 
# makes the high-quality good relatively cheaper. This induces consumers to consume a higher ratio 
# of high-quality goods in distant markets compared to local ones).
final_model_data = final_model_data %>%
  mutate(unit_value = total_val/total_qua,
         GDP_per_capita = GDP_Eur/Population)

# Dropping missing export unit values and ensuring no infinite unit values (from dividing by zero quantity)
final_model_data = final_model_data %>% 
  filter(!is.na(unit_value) & is.finite(unit_value))

model_unit_1 = lmer(log(unit_value) ~ log(total_qua) + log(Distance_km) + log(GDP_per_capita) +
                      (1 | firm_ID)  + (1 | Paese), data = final_model_data, REML = FALSE)

summary(model_unit_1)


# Adding geographical distances and GDP values
model_unit_2 = lmer(log(unit_value) ~ log(total_qua) + log(Distance_km) + log(GDP_per_capita) + Risk_Index_Standardized + 
                      out_degree_std + out_strength_std + eigen_std + (1 | firm_ID)  + (1 | Paese), data = final_model_data, REML = FALSE)

summary(model_unit_2)


# Comparison of the nested models
anova(model_unit_1, model_unit_2)




#----- Data set splitting and XGBoost training -----


# Create the Machine Learning data set
# We train on the log-transformed unit value to handle the power-law skewness
ml_data = final_model_data %>%
  mutate(target_log_val = log(unit_value)) %>%
  select(target_log_val, total_qua, GDP_Eur, Distance_km, 
         Risk_Index_Standardized, out_degree_std, out_strength_std, eigen_std) %>%
  filter(complete.cases(.)) # Ensure no missing values

set.seed(2025) # Set seed for exact academic reproducibility

# Step A: Split 60% Training, 40% Temporary
split_initial = initial_split(ml_data, prop = 0.60)
train_data = training(split_initial)
temp_data = testing(split_initial)

# Step B: Split the 40% Temporary exactly in half (20% Validation, 20% Test)
split_temp = initial_split(temp_data, prop = 0.50)
val_data = training(split_temp)
test_data = testing(split_temp)

# Separate predictors (X) and target (Y) as matrices
x_train = train_data %>% select(-target_log_val) %>% as.matrix()
y_train = train_data$target_log_val

x_val = val_data %>% select(-target_log_val) %>% as.matrix()
y_val = val_data$target_log_val

x_test = test_data %>% select(-target_log_val) %>% as.matrix()
y_test = test_data$target_log_val

# Convert to xgb.DMatrix (XGBoost's highly optimized internal data structure)
dtrain = xgb.DMatrix(data = x_train, label = y_train)
dval = xgb.DMatrix(data = x_val, label = y_val)
dtest = xgb.DMatrix(data = x_test, label = y_test)



# Train the XGBoost Algorithm

# Define the baseline hyperparameters
params = list(
  objective = "reg:squarederror", # Standard regression objective
  eta = 0.1,                      # Learning rate (step size)
  max_depth = 6,                  # Depth of each tree (captures complex interactions)
  subsample = 0.8,                # Use 80% of data per tree (prevents overfitting)
  colsample_bytree = 0.8)         # Use 80% of features per tree (prevents overfitting)


# Set up a watchlist to monitor performance on both train and validation sets
watchlist = list(train = dtrain, eval = dval)

cat("\nTraining XGBoost Model...\n")

# Train the algorithm
xgb_model = xgb.train(params = params, data = dtrain, nrounds = 500,
  watchlist = watchlist, early_stopping_rounds = 20, print_every_n = 50)


# Generate Predictions and Exponentiate Back to Raw Scale (Euros/kg)

# Predict on log scale
pred_train_log = predict(xgb_model, dtrain)
pred_val_log = predict(xgb_model, dval)
pred_test_log = predict(xgb_model, dtest)

# Transform back to raw unit value (Euros/kg)
actual_train = exp(y_train) 
pred_train = exp(pred_train_log) 

actual_val = exp(y_val) 
pred_val = exp(pred_val_log) 

actual_test = exp(y_test) 
pred_test = exp(pred_test_log) 


# Define and Calculate Error Metrics

# Custom function for Predictive R-Squared: proportion of variance which is explained by our learning algorithm
calc_r2 = function(actual, predicted, train_actual) {
  ss_res = sum((actual - predicted)^2)
  ss_tot = sum((actual - mean(train_actual))^2)
  return(1 - (ss_res / ss_tot))
}

# Custom function for MAE
calc_mae = function(actual, predicted) { mean(abs(actual - predicted)) }

# Custom function for RMSE
calc_rmse = function(actual, predicted) { sqrt(mean((actual - predicted)^2)) }

# Compile results into a clean dataframe
results = data.frame(
  Dataset = c("In-Sample (Train 60%)", "Out-of-Sample (Validation 20%)", "Hold-Out (Test 20%)"),
  MAE = c(calc_mae(actual_train, pred_train), calc_mae(actual_val, pred_val), calc_mae(actual_test, pred_test)),
  RMSE = c(calc_rmse(actual_train, pred_train), calc_rmse(actual_val, pred_val), calc_rmse(actual_test, pred_test)),
  R2 = c(calc_r2(actual_train, pred_train, actual_train), calc_r2(actual_val, pred_val, actual_train), calc_r2(actual_test, pred_test, actual_train)))

# Print beautifully rounded results
print(knitr::kable(results, digits = 3, caption = "XGBoost Predictive Performance (Raw Scale: Euros/kg)"))



# Train the regularized XGBoost Algorithm
# Define the hyper-parameter grid to handle over-fitting
set.seed(2025)
params_regularized = list(
  objective = "reg:squarederror",
  
  # 1. Shrinkage & Depth (Preventing Memorization)
  eta = 0.05,               # Reduced Learning Rate that forces the model to learn gradually
  max_depth = 3,            # Shallower trees. Stops it from isolating specific outlier firms.
  
  # 2. Randomness (Forcing Generalization)
  subsample = 0.7,          # Uses only 70% of rows per tree
  colsample_bytree = 0.7,   # Uses only 70% of features per tree
  
  # 3. Explicit Regularization Penalties
  min_child_weight = 20,    # Requires at least ~20 firms in a "leaf" to make a rule. Ignores single extreme outliers.
  gamma = 1,                # Minimum loss reduction required to make a split.
  alpha = 0.5,              # L1 Regularization (Lasso) on leaf weights.
  lambda = 1.0)             # L2 Regularization (Ridge) on leaf weights.


cat("\nTraining Regularized XGBoost Model...\n")

# Train the model with the new parameters
xgb_model_reg = xgb.train(params = params_regularized, data = dtrain, 
                          nrounds = 1000, watchlist = watchlist, 
                          early_stopping_rounds = 50,print_every_n = 100)


# Generate Predictions and Exponentiate Back to Raw Scale (Euros/kg)
# Predict on log scale
pred_train_log = predict(xgb_model_reg, dtrain)
pred_val_log = predict(xgb_model_reg, dval)
pred_test_log = predict(xgb_model_reg, dtest)

actual_train = exp(y_train) 
pred_train = exp(pred_train_log) 

actual_val = exp(y_val) 
pred_val = exp(pred_val_log) 

actual_test = exp(y_test) 
pred_test = exp(pred_test_log) 

# Compile results into a clean dataframe
results_reg = data.frame(
  Dataset = c("In-Sample (Train 60%)", "Out-of-Sample (Validation 20%)", "Hold-Out (Test 20%)"),
  MAE = c(calc_mae(actual_train, pred_train), calc_mae(actual_val, pred_val), calc_mae(actual_test, pred_test)),
  RMSE = c(calc_rmse(actual_train, pred_train), calc_rmse(actual_val, pred_val), calc_rmse(actual_test, pred_test)),
  R2 = c(calc_r2(actual_train, pred_train, actual_train), calc_r2(actual_val, pred_val, actual_train), calc_r2(actual_test, pred_test, actual_train)))

print(knitr::kable(results_reg, digits = 3, caption = "Regularized XGBoost Predictive Performance (Raw Scale: Euros/kg)"))

# Calculate the importance of each feature in your regularized model
importance_matrix = xgb.importance(model = xgb_model_reg)

# Plot the beautiful Feature Importance Graph
xgb.plot.importance(importance_matrix, top_n = 8, measure = "Gain",
                    main = "XGBoost Feature Importance (Information Gain)")



# Applying K-Fold Cross-Validation of Error Measures with K=5
set.seed(2025)
cv_folds = vfold_cv(ml_data, v = 5)

# Initialize an empty dataframe to store results
cv_results = data.frame(Fold = integer(), MAE = numeric(), RMSE = numeric(), R2 = numeric())

cat("\nStarting 5-Fold Cross-Validation. This will train 5 separate models...\n")

# Loop through each fold
for(i in 1:nrow(cv_folds)) {
  cat(sprintf("Training Fold %d of 5...\n", i))
  
  # Extract training and testing sets for this specific fold
  train_fold <- training(cv_folds$splits[[i]])
  test_fold  <- testing(cv_folds$splits[[i]])
  
  # Prepare matrices
  x_train = train_fold %>% select(-target_log_val) %>% as.matrix()
  y_train = train_fold$target_log_val
  
  x_test = test_fold %>% select(-target_log_val) %>% as.matrix()
  y_test = test_fold$target_log_val
  
  dtrain = xgb.DMatrix(data = x_train, label = y_train)
  dtest = xgb.DMatrix(data = x_test, label = y_test)
  
  # Train the model (using a fixed 500 rounds since we are purely cross-validating)
  xgb_cv_model = xgb.train(params = params_regularized, data = dtrain,
    nrounds = 500, verbose = 0) # Silent mode so it doesn't flood the console
  
  
  # Predict and convert back to Euros/kg
  pred_log = predict(xgb_cv_model, dtest)
  
  actual_train_euros = exp(y_train) 
  actual_test_euros = exp(y_test) 
  pred_test_euros = exp(pred_log) 
  
  # Calculate metrics for this fold
  fold_mae = calc_mae(actual_test_euros, pred_test_euros)
  fold_rmse = calc_rmse(actual_test_euros, pred_test_euros)
  fold_r2 = calc_r2(actual_test_euros, pred_test_euros, actual_train_euros)
  
  # Store results
  cv_results = rbind(cv_results, data.frame(Fold = i, MAE_Test = fold_mae,
    RMSE_Test = fold_rmse, R2_Test = fold_r2))
}

# Calculate the Final Averages (The "Gold Standard" Metrics)
cv_summary = cv_results %>%
  summarise(Fold = "AVERAGE (CV)", MAE_Test = mean(MAE_Test),
    RMSE_Test = mean(RMSE_Test), R2_Test = mean(R2_Test))

# Convert the Fold column in cv_results to character before binding
cv_results = cv_results %>%
  mutate(Fold = as.character(Fold))

# Bind the average to the bottom of the table
final_cv_table = bind_rows(cv_results, cv_summary)

cat("\n--- 5-Fold Cross-Validation Results ---\n")
print(knitr::kable(final_cv_table, digits = 3))





## ----- Chapter 5 -----


# Considering the ATECO codes of the selected strategic sectors that we want to compare

df_strategic_sectors = dataset_2024 %>%
  filter(movim == 9 & ateco_prefix %in% c('10', '15', '26', '28', '30', '32')) %>%
  filter(!ateco2007 %in% c('32200', '32300', '32401', '32402', '32501', '32502', '32503',
                           '32504', '32505', '32910', '32991', '32992', '32993', '32994', '32999')) %>%
  group_by(firm_ID, Paese, ISO, provinci, ateco_prefix) %>%
  summarise(total_val = sum(val, na.rm = TRUE), total_qua = sum(qua, na.rm = TRUE), 
            Sovrano = mean(Sovrano, na.rm = TRUE), Bancario = mean(Bancario, na.rm = TRUE),
            Corporate = mean(Corporate, na.rm = TRUE), media_credito = mean(media_credito, na.rm = TRUE),
            Esproprio = mean(Esproprio, na.rm = TRUE), Trasferimento = mean(Trasferimento, na.rm = TRUE), 
            Violenza = mean(Violenza, na.rm = TRUE), media_politico = mean(media_politico, na.rm = TRUE),
            .groups = "drop")


df_strategic_sectors$firm_ID = as.character(df_strategic_sectors$firm_ID)
df_strategic_sectors$ateco_prefix = as.character(df_strategic_sectors$ateco_prefix)

df_strategic_sectors = df_strategic_sectors %>%
  left_join(final_df, by = 'ISO') %>%
  left_join(node_metrics, by = "firm_ID") %>%
  left_join(country_risk %>% select(Paese, Risk_Index_Standardized), by = "Paese")

df_strategic_sectors = df_strategic_sectors %>%
  mutate(out_degree_std = scale(out_degree)[,1], 
         out_strength_std = scale(out_strength)[,1], 
         eigen_std = scale(eigenvector)[,1],
         unit_val = total_val/total_qua,
         GDP_per_capita = GDP_Eur/Population)


# Recoding the ATECO prefixes with explicit descriptive names for the thesis layout
plot_df = df_strategic_sectors %>%
  filter(!is.na(ateco_prefix)) %>%
  mutate(Sector_Label = case_when(
    ateco_prefix == "10" ~ "ATECO 10\n(Food industry)",
    ateco_prefix == "15" ~ "ATECO 15\n(Leather items)",
    ateco_prefix == "26" ~ "ATECO 26\n(Electronics & Optics)",
    ateco_prefix == "28" ~ "ATECO 28\n(Precision machinery)",
    ateco_prefix == "30" ~ "ATECO 30\n(Transport & Aerospace)",
    ateco_prefix == "32" ~ "ATECO 32.1\n(Goldsmith production)",
    TRUE ~ ateco_prefix))

# Panel A: Total Log-Export Value Distribution
p_box_val = ggplot(plot_df, aes(x = Sector_Label, y = log(total_val), fill = Sector_Label)) +
  geom_boxplot(outlier.size = 0.8, outlier.alpha = 0.3, width = 0.6, color = "#2F4F4F") +
  scale_fill_manual(values = c("#2297E6", "#E76F51", "#F4A261", "#E9C46A", "#61D04F", "#CD0BBC")) +
  labs(title = "Export value profile", x = "Strategic sector", y = "Log(total export value in EUR)") +
  theme_minimal(base_size = 11) +
  theme(legend.position = "none", panel.grid.minor = element_blank())

# Panel B: Log-Unit Value Distribution (Quality Proxy)
p_box_unit = ggplot(plot_df, aes(x = Sector_Label, y = log(unit_val), fill = Sector_Label)) +
  geom_boxplot(outlier.size = 0.8, outlier.alpha = 0.3, width = 0.6, color = "#2F4F4F") +
  scale_fill_manual(values = c("#2297E6", "#E76F51", "#F4A261", "#E9C46A", "#61D04F", "#CD0BBC")) +
  labs(title = "Export unit value profile", x = "Strategic sector", y = "Log(unit value - EUR / Kg)") +
  theme_minimal(base_size = 11) +
  theme(legend.position = "none", panel.grid.minor = element_blank())

# Combine panels side-by-side
combined_boxplots = p_box_val + p_box_unit + 
  plot_annotation(title = "Structural Asymmetries Across Veneto's Strategic Spine",
                  subtitle = "Comparative Distributional Profiles of Value and Qualitative Complexity",
    theme = theme(plot_title = element_text(face = "bold", size = 14)))

print(combined_boxplots)


# Calculate total absolute sector values in billions of Euros
tot_export_value = sum(final_model_data$total_val, na.rm = TRUE) / 1e9 

sector_summary = plot_df %>%
  group_by(Sector_Label) %>%
  summarise(aggregate_val = sum(total_val, na.rm = TRUE) / 1e9,
            percent_val = aggregate_val / tot_export_value*100)


# Generate the sector's total export value histogram
ggplot(sector_summary, aes(x = reorder(Sector_Label, -aggregate_val), y = aggregate_val, fill = Sector_Label)) +
  geom_col(width = 0.5, color = "grey30", alpha = 0.85) +
  geom_text(aes(label = paste0(round(aggregate_val, 2), " B€")), vjust = -0.5, fontface = "bold", size = 3.5) +
  scale_fill_manual(values = c("#2297E6", "#E76F51", "#F4A261", "#E9C46A", "#61D04F", "#CD0BBC")) +
  labs(title = "Absolute financial weight of the strategic sectors in 2024", 
    x = "Strategic sector",
    y = "Export value in EUR") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "none", panel.grid.major.x = element_blank())

ggplot(sector_summary, aes(x = reorder(Sector_Label, -percent_val), y = percent_val, fill = Sector_Label)) +
  geom_col(width = 0.5, color = "grey30", alpha = 0.85) +
  geom_text(aes(label = paste0(round(percent_val, 2), " %")), vjust = -0.5, fontface = "bold", size = 3.5) +
  scale_fill_manual(values = c("#2297E6", "#E76F51", "#F4A261", "#E9C46A", "#61D04F", "#CD0BBC")) +
  labs(title = "Percent financial weight of the strategic sectors in 2024", 
       x = "Strategic sector",
       y = "Export value (%)") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "none", panel.grid.major.x = element_blank())


# Calculate firm-level aggregate export value within each sector
firm_aggregates = df_strategic_sectors %>%
  group_by(firm_ID, ateco_prefix) %>%
  summarise(firm_total_val = sum(total_val, na.rm = TRUE), .groups = "drop") %>%
  filter(firm_total_val > 0) %>%
  mutate(Sector_Label = case_when(
    ateco_prefix == "10" ~ "ATECO 10 (Food industry)",
    ateco_prefix == "15" ~ "ATECO 15 (Leather items)",
    ateco_prefix == "26" ~ "ATECO 26 (Electronics & Optics)",
    ateco_prefix == "28" ~ "ATECO 28 (Precision machinery)",
    ateco_prefix == "30" ~ "ATECO 30 (Transport & Aerospace)",
    ateco_prefix == "32" ~ "ATECO 32.1 (Gold & Jewellery)"))

# Compute exact Gini coefficients for each strategic sector 
gini_scores = firm_aggregates %>%
  group_by(Sector_Label) %>%
  summarise(Gini_Coefficient = ineq(firm_total_val, type = "Gini"))

print(gini_scores)

# Generate Coordinate Matrices for the Lorenz Curves
lorenz_data = firm_aggregates %>%
  split(.$Sector_Label) %>%
  purrr::map_df(function(sub_df) {
    # Compute the cumulative population and value fractions
    Lc_res = Lc(sub_df$firm_total_val)
    data.frame(p = Lc_res$p,       # Cumulative proportion of firms (0 to 1)
               L = Lc_res$L,       # Cumulative proportion of export value (0 to 1)
               Sector = unique(sub_df$Sector_Label))
  })


# Dynamic Lorenz Curve Visual Canvas
ggplot(lorenz_data, aes(x = p, y = L, color = Sector)) +
  geom_line(size = 1, alpha = 1) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "black") + # Line of perfect equality
  scale_color_manual(values = c("#2297E6", "#E76F51", "#F4A261", "#E9C46A", "#61D04F", "#CD0BBC")) +
  labs(title = "Signs of market concentration (Lorenz curves)",
       subtitle = "Empirical proof of the 'superstar exporter' paradigm across strategic sectors",
    x = "Cumulative proportion of firms (ranked by export value)",
    y = "Cumulative proportion of total export value") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom", legend.title = element_blank(),
        panel.grid.minor = element_blank()) +
  coord_cartesian(xlim = c(0.75, 1)) # Zooms the x-axis from 0.75 to 1

# Calculate the exact export market share held by the top 10%, top 5% and top 1% elite tier
superstar_shares = firm_aggregates %>%
  group_by(Sector_Label) %>%
  arrange(desc(firm_total_val)) %>%
  mutate(cum_value = cumsum(as.numeric(firm_total_val)),
    total_sector_value = sum(as.numeric(firm_total_val)),
    firm_rank_pct = row_number() / n()) %>%
  # Isolate the threshold representing the top 5%
  summarise(Top_10pct = max(cum_value[firm_rank_pct <= 0.10]) / unique(total_sector_value) * 100,
    Top_5pct  = max(cum_value[firm_rank_pct <= 0.05]) / unique(total_sector_value) * 100,
    Top_1pct  = max(cum_value[firm_rank_pct <= 0.01]) / unique(total_sector_value) * 100,
    .groups = "drop")

print(superstar_shares)



# Compute the Firm-Level Intensive Margin
firm_intensive_margins = df_strategic_sectors %>%
  group_by(firm_ID, ateco_prefix) %>%
  summarise(
    # Total aggregate export value across all foreign countries for this firm
    total_firm_val = sum(total_val, na.rm = TRUE),
    # Leverage the pre-computed extensive margin (out_degree)
    # We take the mean since out_degree is identical across rows for the same firm
    extensive_margin = mean(out_degree, na.rm = TRUE),.groups = "drop" ) %>%
  # Filter out rows with zero or missing margins to protect the log transformation
  filter(total_firm_val > 0 & extensive_margin > 0) %>%
  mutate(
    # Compute the intensive margin: average euros generated per target market (foreign country)
    intensive_margin = total_firm_val / extensive_margin,
    
    # Recoding ATECO prefixes into clean academic labels for the final layout
    Sector_Label = case_when(
      ateco_prefix == "10" ~ "ATECO 10 (Food industry)",
      ateco_prefix == "15" ~ "ATECO 15 (Leather items)",
      ateco_prefix == "26" ~ "ATECO 26 (Electronics & optics)",
      ateco_prefix == "28" ~ "ATECO 28 (Precision machinery)",
      ateco_prefix == "30" ~ "ATECO 30 (Transport & Aerospace)",
      ateco_prefix == "32" ~ "ATECO 32.1 (Goldsmith production)"))

intensive_summary_table = firm_intensive_margins %>%
  group_by(Sector_Label) %>%
  summarise(Active_Firms = n(),
    Mean_Intensive_Margin = mean(intensive_margin, na.rm = TRUE),
    Median_Intensive_Margin = median(intensive_margin, na.rm = TRUE),
    Max_Intensive_Margin = max(intensive_margin, na.rm = TRUE),
    SD_Intensive_Margin = sd(intensive_margin, na.rm = TRUE)) %>%
  mutate(across(where(is.numeric) & !Active_Firms, ~ round(., 2)))

print(intensive_summary_table)

 


## ----- Quantile regression -----


# Filtering out export data (movement type 9) of the manufacturing sector 28
df_ateco28 = dataset_2024 %>%
  filter(movim == 9 & ateco_prefix == '28') %>%  # Keep only export trade flows of precision mechanics
  select(firm_ID, Paese, ISO, ateco2007, provinci, val, qua, Sovrano, Bancario, Corporate,media_credito, Esproprio, Trasferimento, Violenza, media_politico)%>%
  group_by(firm_ID, Paese, ISO, ateco2007) %>%
  summarise(total_val = sum(val, na.rm = TRUE), total_qua = sum(qua, na.rm = TRUE), 
            Sovrano = mean(Sovrano, na.rm = TRUE), Bancario = mean(Bancario, na.rm = TRUE),
            Corporate = mean(Corporate, na.rm = TRUE), media_credito = mean(media_credito, na.rm = TRUE),
            Esproprio = mean(Esproprio, na.rm = TRUE), Trasferimento = mean(Trasferimento, na.rm = TRUE), 
            Violenza = mean(Violenza, na.rm = TRUE), media_politico = mean(media_politico, na.rm = TRUE),
            .groups = "drop")

df_ateco28$firm_ID = as.character(df_ateco28$firm_ID)

df_ateco28 = df_ateco28 %>%
  left_join(final_df, by = 'ISO') %>%
  left_join(node_metrics, by = "firm_ID") %>%
  left_join(country_risk %>% select(Paese, Risk_Index_Standardized), by = "Paese")

df_ateco28 = df_ateco28 %>%
  mutate(out_degree_std = scale(out_degree)[,1], 
         out_strength_std = scale(out_strength)[,1], 
         eigen_std = scale(eigenvector)[,1],
         unit_val = total_val/total_qua,
         GDP_per_capita = GDP_Eur/Population)

df_ateco28 = df_ateco28 %>% filter(!is.na(Risk_Index_Standardized))
df_ateco28 = df_ateco28 %>% filter(!is.na(total_val) & is.finite(total_val))
df_ateco28 = df_ateco28 %>% filter(!is.na(unit_val) & is.finite(unit_val))


# Quantile regression formula: capturing traditional gravity and topological networks
qr_formula = log(total_val) ~ Risk_Index_Standardized + log(Distance_km) + 
  log(GDP_Eur) + out_degree_std + out_strength_std + eigen_std


# Here we fit a sequence to see the 'trend'
qr_28 = rq(qr_formula, tau = seq(0.1, 0.9, by = 0.05), data = df_ateco28)
sum_qr_28 = summary(qr_seq) 

# Kernel density estimator to compute the sandwich matrix that handles heteroscedasticity
# without the extreme latency of 'nid' or the loop overhead of 'boot'
tidy_qr_28 = tidy(qr_28, conf.int = TRUE, se = 'ker') %>% 
  filter(term != "(Intercept)")

# Generate the marginal effects plot 
par(mfrow = c(3,2))
ggplot(tidy_qr_28, aes(x = tau, y = estimate)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), fill = "royalblue", alpha = 0.17) +
  geom_line(color = "darkblue", size = 1) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  facet_wrap(~term, scales = "free_y", labeller = labeller(term = c(
    "Risk_Index_Standardized" = "Institutional risk",
    "log(Distance_km)" = "Geodetic distance",
    "log(GDP_Eur)" = "Economic mass",
    "out_degree_std" = "Market reach",
    "out_strength_std" = "Logistical mass",
    "eigen_std" = "Network prestige")), ncol = 2) +
  labs(x = "Performance quantile", y = "Marginal effect on export value" ) +
  theme_minimal(base_size = 12) +
  theme(strip.text = element_text(face = "bold", size = 10, color = "black"),
        panel.spacing = unit(3, "lines"),
        panel.border = element_rect(color = "grey90", fill = NA))



# Quantile regression with export unit value as response variable
qr_unit = log(unit_val) ~ log(total_qua) + Risk_Index_Standardized + log(Distance_km) + 
  log(GDP_per_capita) + out_degree_std + out_strength_std + eigen_std

qr_28_unit = rq(qr_unit, tau = seq(0.1, 0.9, by = 0.05), data = df_ateco28)
sum_qr_28_unit = summary(qr_28_unit) 

# Kernel density estimator to compute the sandwich matrix that handles heteroscedasticity
# without the extreme latency and computational cost of 'nid' or the loop overhead of 'boot'
tidy_qr_28_unit = tidy(qr_28_unit, conf.int = TRUE, se = 'ker') %>% 
  filter(term != "(Intercept)")

# Generate the marginal effects plot 
par(mfrow = c(3,2))
ggplot(tidy_qr_28_unit, aes(x = tau, y = estimate)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), fill = "royalblue", alpha = 0.17) +
  geom_line(color = "darkblue", size = 1) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  facet_wrap(~term, scales = "free_y", labeller = labeller(term = c(
    "log(total_qua)" = "Phisical quantity",
    "Risk_Index_Standardized" = "Institutional risk",
    "log(Distance_km)" = "Geodetic distance",
    "log(GDP_per_capita)" = "Economic wealth",
    "out_degree_std" = "Market reach",
    "out_strength_std" = "Logistical mass",
    "eigen_std" = "Network prestige")), ncol = 2) +
  labs(x = "Performance quantile", y = "Marginal effect on export unit value" ) +
  theme_minimal(base_size = 12) +
  theme(strip.text = element_text(face = "bold", size = 10, color = "black"),
        panel.spacing = unit(2, "lines"),
        panel.border = element_rect(color = "grey90", fill = NA))


# Fit a simultaneous quantile regression for the extreme quantiles with export value as response variable
# We must use the joint rq fitting function to allow quantile covariance calculation
joint_model = rq(qr_formula, tau = c(0.1, 0.9), data = df_ateco28)

# Execute the Robust Wald Test (ANOVA)
# joint = FALSE tests each variable's slope equality independently across the quantiles
# se = "ker" ensures high-speed, robust covariance calculation
wald_test_results = anova(joint_model, joint = FALSE, se = "ker")

# Display the formal hypothesis test matrix
print(wald_test_results)


# Fit a simultaneous quantile regression for the extreme quantiles with export unit value as response variable
joint_model_unit = rq(qr_unit, tau = c(0.1, 0.9), data = df_ateco28)

# Execute the Robust Wald Test (ANOVA)
# joint = FALSE tests each variable's slope equality independently across the quantiles
# se = "ker" ensures high-speed, robust covariance calculation
wald_test_results_unit = anova(joint_model_unit, joint = FALSE, se = "ker")

# Display the formal hypothesis test matrix
print(wald_test_results_unit)




## ----- Poisson Pseudo Maximum Likelihood Models -----

# Ensure zero or missing values do not drop raw total_val
df_PPML = df_strategic_sectors %>%
  filter(!is.na(total_val) & total_val >= 0)

# Model specification 1: ATECO strategic sectors as fixed effects
PPML_1 = feglm(total_val ~ log(Distance_km) + log(GDP_Eur) + Risk_Index_Standardized +
                 out_degree_std + out_strength_std + eigen_std | ateco_prefix, 
               family = "poisson", data = df_PPML)

# Model specification 2: ATECO strategic sectors and provinces as fixed effects
PPML_2 = feglm(total_val ~ log(Distance_km) + log(GDP_Eur) + Risk_Index_Standardized +
                 out_degree_std + out_strength_std + eigen_std | provinci^ateco_prefix, 
               family = "poisson", data = df_PPML)


# Standard errors must be multi-way clustered at both the province and destination country level
# This ensures p-values are robust to spatial and district correlation channels
summary_1 = summary(PPML_1, cluster = c("provinci", "Paese"))
summary_2 = summary(PPML_2, cluster = c("provinci", "Paese"))

# Print comparative results table side by side for inspection
etable(PPML_1, PPML_2, cluster = c("provinci", "Paese"), 
       headers = c("Sectoral FE", "Province-Sector FE"))



#Poisson Pseudo Maximum Likelihood with export unit value as response variable
PPML_3 = feglm(unit_val ~ log(Distance_km) + log(GDP_per_capita) + Risk_Index_Standardized +
                 out_degree_std + out_strength_std + eigen_std | provinci^ateco_prefix, 
               family = "poisson", data = df_PPML)

summary_3 = summary(PPML_3, cluster = c("provinci", "Paese"))




## ----- Stress testing the strategic sectors -----


# Aligned baseline and shock predictor loop
df_stress = df_PPML %>%
  drop_na %>%
  mutate(y_hat_baseline = predict(PPML_2, type = "response")) %>%
  filter(!is.na(y_hat_baseline))

beta_risk = coef(PPML_2)["Risk_Index_Standardized"]

df_stress = df_stress %>%
  mutate(y_hat_shock_2s = y_hat_baseline * exp(beta_risk * 2)) # +2 standard deviation risk shock (simulation vector)

# Dimensionality reduction for the horizontal base grid matrix
pca_matrix = df_stress %>%
  select(Distance_km, GDP_Eur, out_degree, out_strength, eigenvector) %>%
  scale()

pca_results = prcomp(pca_matrix)
df_stress$PC1 = pca_results$x[, 1]
df_stress$PC2 = pca_results$x[, 2]

# Extract the variance contribution metrics
pca_summary = summary(pca_results)
importance_table = pca_summary$importance

cat("--- VARIANCE CONTRIBUTION MATRIX ---\n")
print(importance_table)

# Extract the raw rotation loadings (Eigenvectors)
# The $rotation matrix contains the weights assigned to each standardized variable
loadings_matrix = pca_results$rotation

cat("\n--- RAW ROTATION LOADINGS (EIGENVECTORS) ---\n")
print(round(loadings_matrix[, 1:2], 4)) # Focus explicitly on PC1 and PC2

# Visualize the loadings array via a Scree Plot 
plot(pca_results, type = "l", main = "Scree plot: Variance Decomposition")

# Regular grid matrix interpolation (required for surface plotting)
# Set up a 20x20 resolution grid across the PCA landscape
grid_resolution = 20
grid_x = seq(min(df_stress$PC1), max(df_stress$PC1), length.out = grid_resolution)
grid_z = seq(min(df_stress$PC2), max(df_stress$PC2), length.out = grid_resolution)

# Interpolate surface A: normal / baseline conditions (green surface)
interp_baseline = interp(x = df_stress$PC1, y = df_stress$PC2, 
                         z = df_stress$y_hat_baseline, xo = grid_x, 
                         yo = grid_z, duplicate = "mean")

# Interpolate surface B: +2σ institutional shock conditions (red surface)
interp_shock_2s = interp(x = df_stress$PC1, y = df_stress$PC2, 
                      z = df_stress$y_hat_shock_2s, xo = grid_x, 
                      yo = grid_z, duplicate = "mean")

# Isolate the interpolated matrix planes
matrix_z_baseline = interp_baseline$z
matrix_z_shock = interp_shock_2s$z

# Render the dual-surface dynamic interactive Plotly widget
stress_surface_plot = plot_ly() %>%
  
  # Surface 1: normal conditions (coded in dark green palette)
  add_surface(x = ~grid_x, y = ~grid_z, z = ~matrix_z_baseline,
    name = "Baseline Conditions (Unshocked Scenario)", colorscale = list(c(0, 1), c('#134611', '#3A8635')),
    showscale = TRUE, colorbar = list(title = "Unshocked scenario"), opacity = 0.75 ) %>%
  
  # Surface 2: geopolitical volatility shock (+2σ Risk - coded in red palette)
  add_surface(x = ~grid_x, y = ~grid_z, z = ~matrix_z_shock,
    name = "Institutional Shock Scenario (+2σ Risk)", colorscale = list(c(0, 1), c('#8B0000', '#FF4C4C')),
    showscale = TRUE, colorbar = list(title = "Shocked scenario (+2σ Risk)"), opacity = 0.5) %>%
  
  layout( scene = list(xaxis = list(title = "PC1 (Firm & Network Scale Capacity)", gridcolor = "black"),
      yaxis = list(title = "PC2 (Spatial Friction & Target Market Scale)", gridcolor = "black"),
      zaxis = list(title = "Predicted export value in EUR", gridcolor = "black"),
      bgcolor = "white"), paper_bgcolor = "white",font = list(color = "black"), margin = list(r = 100))
     

# Display interactive HTML widget
stress_surface_plot
