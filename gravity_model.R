  library(readxl)
  library(car)
  library(tidyverse)
  library(WDI)
  library(lmtest)
  library(sandwich)
  library(stargazer)
  library(pscl)
  library(ggplot2)
  
  # Clear environment and console
  rm(list = ls())
  cat("\014")
  
  # 1. DATA PREPARATION & CLEANING -----------------------------------------------
  
  # Load UN Comtrade Data (Export)
  df_export <- read_csv("Data/comtrade_export_data.csv")
  trade_clean <- df_export[, c("partnerISO", "partnerDesc", "fobvalue")]
  colnames(trade_clean) <- c("iso3", "country_name", "export")
  trade_clean <- subset(trade_clean, iso3 != "W00" & iso3 != "_X" & export > 0)
  
  # Load UN Comtrade Data (Import)
  df_import <- read_csv("Data/comtrade_import_data.csv")
  import_clean <- df_import[, c("partnerISO", "fobvalue")]
  colnames(import_clean) <- c("iso3", "import_val")
  import_clean <- subset(import_clean, iso3 != "W00" & iso3 != "_X" & import_val > 0)
  
  # Define EU Members and Shared Borders
  eu_list <- c("AUT", "BEL", "BGR", "HRV", "CYP", "DNK", "EST", "FIN", "FRA", "DEU", 
               "GRC", "HUN", "IRL", "ITA", "LVA", "LTU", "LUX", "MLT", "NLD", "POL", 
               "PRT", "ROU", "SVK", "SVN", "ESP", "SWE")
  border_iso <- c("SVK", "DEU", "POL", "AUT")
  
  trade_clean$is_eu <- ifelse(trade_clean$iso3 %in% eu_list, 1, 0)
  
  # Load GDP Data (World Bank)
  if (file.exists("Data/world_bank_gdp.csv")) {
    gdp_data <- read.csv("Data/world_bank_gdp.csv")
    message("Data loaded from local file.")
  } else {
    gdp_data <- WDI(indicator = "NY.GDP.MKTP.CD", country = "all", start = 2023, end = 2023)
    write.csv(gdp_data, "Data/world_bank_gdp.csv", row.names = FALSE)
    message("Data downloaded from World Bank API and saved locally.")
  }
  
  names(gdp_data)[grep("NY.GDP", names(gdp_data))] <- "gdp"
  
  # Load Distance Data (CEPII)
  dist_data <- read_excel("Data/dist_cepii.xls")
  dist_cze <- subset(dist_data, iso_o == "CZE")[, c("iso_d", "dist")]
  colnames(dist_cze) <- c("iso3", "distance")
  
  # Final Merge
  final_data <- merge(trade_clean, gdp_data, by.x = "iso3", by.y = "iso3c")
  final_data <- merge(final_data, dist_cze, by = "iso3")
  final_data <- merge(final_data, import_clean, by = "iso3") 
  final_data <- na.omit(final_data)
  
  # Feature Engineering
  final_data$log_export <- log(final_data$export)
  final_data$log_import <- log(final_data$import_val)
  final_data$log_gdp    <- log(final_data$gdp)
  final_data$log_dist   <- log(final_data$distance)
  final_data$border     <- ifelse(final_data$iso3 %in% border_iso, 1, 0)
  
  # Define Strategic Partners (Top 20% of Export / Import Value)
  threshold_exp <- quantile(final_data$export, 0.80, na.rm = TRUE)
  final_data$big_partner <- ifelse(final_data$export >= threshold_exp, 1, 0)
  
  threshold_imp <- quantile(final_data$import_val, 0.80, na.rm = TRUE)
  final_data$big_importer <- ifelse(final_data$import_val >= threshold_imp, 1, 0)
  
  
  # 2. ECONOMETRIC MODELS & DIAGNOSTICS ------------------------------------------
  
  model_exp_ols <- lm(log_export ~ log_gdp + log_dist + is_eu, data = final_data)
  model_exp_logit <- glm(big_partner ~ log_gdp + log_dist + is_eu, 
                         data = final_data, family = binomial(link = "logit"))
  
  model_imp_ols <- lm(log_import ~ log_gdp + log_dist + is_eu, data = final_data)
  model_imp_logit <- glm(big_importer ~ log_gdp + log_dist + is_eu, 
                         data = final_data, family = binomial(link = "logit"))
  
  summary(model_exp_ols)
  summary(model_exp_logit)
  summary(model_imp_ols)
  summary(model_imp_logit)
  
  # --- B) ROBUSTNESS CHECKS & DIAGNOSTICS (Export) ---
  bptest(model_exp_ols); vif(model_exp_ols); resettest(model_exp_ols)
  pR2(model_exp_logit)["McFadden"]
  
  model_exp_inter <- lm(log_export ~ log_gdp + log_dist * is_eu, data = final_data)
  model_exp_sq <- lm(log_export ~ log_gdp + log_dist + I(log_dist^2) + is_eu, data = final_data)
  model_exp_border <- lm(log_export ~ log_gdp + log_dist + is_eu + border, data = final_data)
  
  summary(model_exp_inter)
  summary(model_exp_sq)
  summary(model_exp_border)
  
  # --- C) ROBUSTNESS CHECKS & DIAGNOSTICS (Import) ---
  bptest(model_imp_ols); vif(model_imp_ols); resettest(model_imp_ols)
  pR2(model_imp_logit)["McFadden"]
  
  model_imp_inter <- lm(log_import ~ log_gdp + log_dist * is_eu, data = final_data)
  model_imp_sq <- lm(log_import ~ log_gdp + log_dist + I(log_dist^2) + is_eu, data = final_data)
  model_imp_border <- lm(log_import ~ log_gdp + log_dist + is_eu + border, data = final_data)
  
  summary(model_imp_inter)
  summary(model_imp_sq)
  summary(model_imp_border)
  
  # Calculate Robust Standard Errors for the Baseline models
  robust_se_exp_ols <- sqrt(diag(vcovHC(model_exp_ols, type = "HC3")))
  robust_se_imp_ols <- sqrt(diag(vcovHC(model_imp_ols, type = "HC3")))
  robust_se_exp_logit <- sqrt(diag(vcovHC(model_exp_logit, type = "HC1")))
  robust_se_imp_logit <- sqrt(diag(vcovHC(model_imp_logit, type = "HC1")))
  
  
  # 3. RESULTS TABLE (STARGAZER) -------------------------------------------------
  
  stargazer(model_exp_ols, model_imp_ols, model_exp_logit, model_imp_logit, 
            type = "html",
            se = list(robust_se_exp_ols, robust_se_imp_ols, robust_se_exp_logit, robust_se_imp_logit),
            intercept.bottom = FALSE,
            covariate.labels = c("Constant", "log(GDP)", "log(Distance)", "EU Member"),
            dep.var.labels = c("log(Export)", "log(Import)", "Top 20% Export", "Top 20% Import"),
            column.labels = c("OLS Export", "OLS Import", "Logit Export", "Logit Import"),
            digits = 3,
            model.numbers = FALSE,
            title = "Table 1: Gravity Models of Czech Exports and Imports",
            notes = c("Standard errors are HC-robust.", "*p < 0.1; **p < 0.05; ***p < 0.01"),
            notes.append = FALSE,
            out = "Results/final_table.html")
  
  write.csv(final_data, "Data/final_trade_data_czechia.csv", row.names = FALSE)
  
  
  # 4. DATA VISUALIZATION (GGPLOT2) ----------------------------------------------
  #Export
  
  export_plot <- ggplot(final_data, aes(x = log_dist, y = big_partner, color = as.factor(is_eu))) +
    geom_jitter(aes(size = gdp), alpha = 0.5, height = 0.04, width = 0) +
    
    stat_smooth(method = "glm", method.args = list(family = "binomial"), 
                se = FALSE, linewidth = 1.2, fullrange = TRUE) +
    
    scale_size_continuous(range = c(2, 25), guide = "none") + 
    
    labs(title = "The Gravity Trap: Distance vs. EU Membership",
         subtitle = "Probability of being a Strategic Export Partner (Bubble size = GDP)",
         x = "Distance from Prague (log km)",
         y = "Probability (1 = Top Export Partner)",
         color = "EU Member") +
    
    scale_color_manual(values = c("0" = "red", "1" = "blue"), 
                       labels = c("No", "Yes")) +
    
    theme_minimal() +
    theme(legend.position = "right",
          plot.title = element_text(face = "bold", size = 14))
  
  print(export_plot)
  ggsave("Results/Gravity_Plot_Export.png", plot = export_plot, width = 8, height = 6, dpi = 300)
  
  #IMPORT
  
  import_plot <- ggplot(final_data, aes(x = log_dist, y = big_importer, color = as.factor(is_eu))) +
    geom_jitter(aes(size = gdp), alpha = 0.5, height = 0.04, width = 0) +
    
    stat_smooth(method = "glm", method.args = list(family = "binomial"), 
                se = FALSE, linewidth = 1.2, fullrange = TRUE) +
    
    scale_size_continuous(range = c(2, 25), guide = "none") + 
    
    labs(title = "The Gravity Trap: Imports & EU Membership",
         subtitle = "Probability of being a Strategic Import Partner (Bubble size = GDP)",
         x = "Distance from Prague (log km)",
         y = "Probability (1 = Top Import Partner)",
         color = "EU Member") +
    
    scale_color_manual(values = c("0" = "red", "1" = "blue"), 
                       labels = c("No", "Yes")) +
    
    theme_minimal() +
    theme(legend.position = "right",
          plot.title = element_text(face = "bold", size = 14))
  
  print(import_plot)
  ggsave("Results/Gravity_Plot_Import.png", plot = import_plot, width = 8, height = 6, dpi = 300)
  
  # 4. DATA VISUALIZATION (GGPLOT2) - PRECIZNÍ PREDIKCE Z MODELU -----------------
  
  # KROK 1: Vytvoříme umělá data pro křivky (HDP zafixujeme na průměru)
  grid_data <- expand.grid(
    log_dist = seq(min(final_data$log_dist), max(final_data$log_dist), length.out = 100),
    is_eu = c(0, 1),
    log_gdp = mean(final_data$log_gdp) # Zde držíme HDP konstantní!
  )
  
  # KROK 2: Necháme naše reálné modely spočítat přesnou pravděpodobnost
  grid_data$prob_exp <- predict(model_exp_logit, newdata = grid_data, type = "response")
  grid_data$prob_imp <- predict(model_imp_logit, newdata = grid_data, type = "response")
  
  # --- GRAF 1: EXPORT ---
  gravity_plot_exp <- ggplot() +
    # Bubliny (skutečná data)
    geom_jitter(data = final_data, aes(x = log_dist, y = big_partner, color = as.factor(is_eu), size = gdp), 
                alpha = 0.5, height = 0.04, width = 0) +
    
    # Křivky (predikce z našeho modelu při průměrném HDP)
    geom_line(data = grid_data, aes(x = log_dist, y = prob_exp, color = as.factor(is_eu)), 
              linewidth = 1.2) +
    
    scale_size_continuous(range = c(2, 25), guide = "none") + 
    labs(title = "The Gravity Trap: Distance vs. EU Membership (Export)",
         subtitle = "Model predictions holding GDP at sample mean",
         x = "Distance from Prague (log km)",
         y = "Probability (1 = Top Export Partner)",
         color = "EU Member") +
    scale_color_manual(values = c("0" = "red", "1" = "blue"), labels = c("No", "Yes")) +
    theme_minimal() +
    theme(legend.position = "right", plot.title = element_text(face = "bold", size = 14))
  
  print(gravity_plot_exp)
  ggsave("Results/Figure_Gravity_Plot_Export.png", plot = gravity_plot_exp, width = 8, height = 6, dpi = 300)
  
  
  # --- GRAF 2: IMPORT ---
  gravity_plot_imp <- ggplot() +
    geom_jitter(data = final_data, aes(x = log_dist, y = big_importer, color = as.factor(is_eu), size = gdp), 
                alpha = 0.5, height = 0.04, width = 0) +
    
    geom_line(data = grid_data, aes(x = log_dist, y = prob_imp, color = as.factor(is_eu)), 
              linewidth = 1.2) +
    
    scale_size_continuous(range = c(2, 25), guide = "none") + 
    labs(title = "The Gravity Trap: Distance vs. EU Membership (Import)",
         subtitle = "Model predictions holding GDP at sample mean",
         x = "Distance from Prague (log km)",
         y = "Probability (1 = Top Import Partner)",
         color = "EU Member") +
    scale_color_manual(values = c("0" = "red", "1" = "blue"), labels = c("No", "Yes")) +
    theme_minimal() +
    theme(legend.position = "right", plot.title = element_text(face = "bold", size = 14))
  
  print(gravity_plot_imp)
  ggsave("Results/Figure_Gravity_Plot_Import.png", plot = gravity_plot_imp, width = 8, height = 6, dpi = 300)
  
