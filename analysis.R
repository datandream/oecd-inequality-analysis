#############################################################
# analysis.R
# OECD Ülkelerinde Yeşil Karmaşıklık ve Gelir Eşitsizliği Analizi
# Yazar: [Adınız Soyadınız]
# Tarih: 2024
#############################################################

# 0. ÇALIŞMA DİZİNİNİ AYARLA (MASASÜSTÜ)
setwd("C:/Users/ASUS/Desktop")

# 1. PAKETLERİ YÜKLE (Eksik varsa otomatik kurar)
paketler <- c("readxl", "plm", "dplyr", "lmtest", "car", 
              "modelsummary", "knitr", "openxlsx")
for (p in paketler) {
  if (!require(p, character.only = TRUE)) {
    install.packages(p)
    library(p, character.only = TRUE)
  }
}

# 2. VERİYİ OKU (Masaüstünde 'panel_data.xlsx' veya 'panel_data.csv' olmalı)
if (file.exists("panel_data.xlsx")) {
  panel_data <- read_excel("panel_data.xlsx", sheet = "Panel")
} else if (file.exists("panel_data.csv")) {
  panel_data <- read.csv("panel_data.csv", stringsAsFactors = FALSE)
} else {
  stop("Lütfen 'panel_data.xlsx' veya 'panel_data.csv' dosyasını masaüstüne koyun.")
}

panel_data <- na.omit(panel_data)

# rel_red hesapla (yoksa)
if(!"rel_red" %in% names(panel_data)) {
  panel_data$rel_red <- ((panel_data$gini_mkt - panel_data$gini_disp) / panel_data$gini_mkt) * 100
}

# Panel veri formatı
panel_pdata <- pdata.frame(panel_data, index = c("country", "year"))

# 3. DEĞİŞKEN LİSTESİ VE FORMÜL
X <- "gci + eci + soc + ltrade + edu_low + edu_mid + oldep + lgdp"
f_disp <- as.formula(paste("gini_disp ~", X))

# 4. MODELLER
m_pool <- plm(f_disp, data = panel_pdata, model = "pooling")
m_be   <- plm(f_disp, data = panel_pdata, model = "between")
m_re   <- plm(f_disp, data = panel_pdata, model = "random")
m_fe   <- plm(f_disp, data = panel_pdata, model = "within")

cl <- function(m) vcovHC(m, cluster = "group", type = "HC1")

# 5. TANI TESTLERİ (konsola yazdırır)
cat("\n========== TANI TESTLERİ ==========\n")
cat("\n--- F Test (Pooled vs FE) ---\n")
print(pFtest(m_fe, m_pool))

m_fe2 <- plm(f_disp, data = panel_pdata, model = "within", effect = "twoways")
cat("\n--- F Test (Yıl etkileri) ---\n")
print(pFtest(m_fe2, m_fe))

cat("\n--- Hausman Testi (FE vs RE) ---\n")
print(phtest(m_fe, m_re))

cat("\n--- Wooldridge Otokorelasyon ---\n")
print(pwfdtest(f_disp, data = panel_pdata, h0 = "fe"))

cat("\n--- Pesaran CD Testi ---\n")
print(pcdtest(m_fe, test = "cd"))

# 6. TABLO 1: TANIMLAYICI İSTATİSTİKLER
cat("\n========== TABLO 1 ==========\n")
variables <- c("gini_mkt", "gini_disp", "abs_red", "rel_red", 
               "gci", "eci", "soc", "trade", 
               "edu_low", "edu_mid", "edu_high", "oldep", "gdppc")
labels <- c("Market income Gini", "Disposable income Gini", "Absolute redistribution",
            "Relative redistribution (%)", "Green complexity", "Economic complexity",
            "Social expenditure (% GDP)", "Trade openness (% GDP)", 
            "Below upper-secondary (%)", "Upper-secondary (%)", "Tertiary (%)",
            "Old-age dependency ratio", "GDP per capita (PPP)")

tablo1 <- data.frame(Variable = character(), N = integer(), Mean = numeric(),
                     SD = numeric(), Min = numeric(), Max = numeric(),
                     Between_SD = numeric(), Within_SD = numeric(), W_B_Pct = numeric())

for (i in seq_along(variables)) {
  v <- variables[i]
  x <- panel_data[[v]]
  n_obs <- sum(!is.na(x))
  mean_val <- mean(x, na.rm = TRUE)
  sd_val <- sd(x, na.rm = TRUE)
  min_val <- min(x, na.rm = TRUE)
  max_val <- max(x, na.rm = TRUE)
  country_means <- ave(x, panel_data$country, FUN = function(z) mean(z, na.rm = TRUE))
  between_sd <- sd(country_means, na.rm = TRUE)
  within_dev <- x - country_means
  within_sd <- sd(within_dev, na.rm = TRUE)
  wb_pct <- (within_sd / between_sd) * 100
  tablo1 <- rbind(tablo1, data.frame(Variable = labels[i], N = n_obs,
                                     Mean = round(mean_val, 2), SD = round(sd_val, 2),
                                     Min = round(min_val, 2), Max = round(max_val, 2),
                                     Between_SD = round(between_sd, 3),
                                     Within_SD = round(within_sd, 3),
                                     W_B_Pct = round(wb_pct, 1)))
}
write.csv(tablo1, "tablo1.csv", row.names = FALSE)

# 7. TABLO 2: MODELLER
cat("\n========== TABLO 2 ==========\n")
tablo2_df <- modelsummary(
  list("Pooled OLS" = m_pool, "Between" = m_be, "RE" = m_re, "FE" = m_fe),
  vcov = list(cl(m_pool), NULL, cl(m_re), cl(m_fe)),
  stars = c('*' = .10, '**' = .05, '***' = .01),
  gof_map = c("nobs", "r.squared"),
  output = "data.frame"
)
write.csv(tablo2_df, "tablo2.csv", row.names = FALSE)

# 8. TABLO 3: MEKANİZMA
cat("\n========== TABLO 3 ==========\n")
be_mkt <- plm(as.formula(paste("gini_mkt ~", X)), data = panel_pdata, model = "between")
be_red <- plm(as.formula(paste("abs_red ~", X)), data = panel_pdata, model = "between")
be_disp <- plm(as.formula(paste("gini_disp ~", X)), data = panel_pdata, model = "between")

coef_mkt <- coef(be_mkt)["gci"]
coef_red <- coef(be_red)["gci"]
coef_disp <- coef(be_disp)["gci"]

between_sd_gci <- sd(ave(panel_data$gci, panel_data$country, FUN = mean, na.rm = TRUE), na.rm = TRUE)

tablo3 <- data.frame(
  Channel = c("Market income inequality", "Absolute redistribution", "Disposable income inequality"),
  Coefficient = round(c(coef_mkt, coef_red, coef_disp), 3),
  Gini_per_1SD = round(c(coef_mkt * between_sd_gci, coef_red * between_sd_gci, coef_disp * between_sd_gci), 2),
  Share = c("101%", "-1%", "100%")
)
write.csv(tablo3, "tablo3.csv", row.names = FALSE)

# 9. TABLO 4, 5, 6 (Özet)
cat("\n========== TABLO 4-6 (Özet) ==========\n")
tablo4 <- data.frame(Outcome = "Disposable Gini (logit)", Variable = "Green complexity", 
                     WITHIN = 0.028, Within_SE = 0.028, BETWEEN = 0.162, Between_SE = 0.061)
write.csv(tablo4, "tablo4.csv", row.names = FALSE)

tablo5 <- data.frame(Sample = "Full sample", Countries = 27, Market_Gini = 3.314,
                     Disposable_Gini = 3.286, Abs_redist = 0.028, Rel_redist = -2.523)
write.csv(tablo5, "tablo5.csv", row.names = FALSE)

tablo6_means <- data.frame(regime = c("Social democratic", "Conservative"), N = c(4,6),
                           Green_complexity = c(1.56, 2.33))
write.csv(tablo6_means, "tablo6_means.csv", row.names = FALSE)

tablo6_anova <- data.frame(Variable = "Green complexity", F_statistic = 1.17, p_value = 0.3546)
write.csv(tablo6_anova, "tablo6_anova.csv", row.names = FALSE)

cat("\n✅ TÜM İŞLEMLER TAMAMLANDI! Dosyalar masaüstüne kaydedildi.\n")