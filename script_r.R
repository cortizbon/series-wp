# leer los datos
library(readr)
library(reshape2)
library(ggplot2)
library(dplyr)
library(gridExtra)
library(moments)
library(tseries)
library(ConnectednessApproach)
library(zoo)
library(kableExtra)
library(vars)

rets <- read_csv("~/series-wp/exchange_rates_returns.csv")
vals <- read_csv("~/series-wp/exchange_rates_values.csv")

rets <- select(rets, -c(PAB))
vals <- select(vals, -c(PAB))

# graficar los datos

vals$Date <- as.Date(vals$Date)
rets$Date <- as.Date(rets$Date)


vals_long <- melt(vals, id.vars = "Date", variable.name = "Series", value.name = "Value")


ggplot(vals_long, aes(x = Date, y = Value)) +
  geom_line() +
  facet_wrap(~ Series, nrow = 4, ncol = 4, scales = "free_y") +
  theme_minimal() +
  labs(title = "Time Series Grid", x = "Date", y = "Value")


rets_long <- melt(rets, id.vars = "Date", variable.name = "Series", value.name = "Value")


ggplot(rets_long, aes(x = Date, y = Value)) +
  geom_line() +
  facet_wrap(~ Series, nrow = 4, ncol = 4, scales = "free_y") +
  theme_minimal() +
  labs(title = "Time Series Grid", x = "Date", y = "Value")


# estadístico descriptivo de todos los datos

series_names <- c("ARS", "BRL", "CLP", "COP", "CRC", "DOP",
                  "GTQ", "HNL", "MXN", "NIO", "PEN", "PYG", "UYU")

stats <- data.frame(
  Statistic = c("Mean", "Variance", "Min", "Max", "Median", "Skewness", "Kurtosis"),
  ARS = c(mean(vals$ARS), var(vals$ARS), min(vals$ARS), max(vals$ARS), median(vals$ARS), skewness(vals$ARS), kurtosis(vals$ARS)),
  BRL = c(mean(vals$BRL), var(vals$BRL), min(vals$BRL), max(vals$BRL), median(vals$BRL), skewness(vals$BRL), kurtosis(vals$BRL)),
  CLP = c(mean(vals$CLP), var(vals$CLP), min(vals$CLP), max(vals$CLP), median(vals$CLP), skewness(vals$CLP), kurtosis(vals$CLP)),
  COP = c(mean(vals$COP), var(vals$COP), min(vals$COP), max(vals$COP), median(vals$COP), skewness(vals$COP), kurtosis(vals$COP)),
  CRC = c(mean(vals$CRC), var(vals$CRC), min(vals$CRC), max(vals$CRC), median(vals$CRC), skewness(vals$CRC), kurtosis(vals$CRC)),
  DOP = c(mean(vals$DOP), var(vals$DOP), min(vals$DOP), max(vals$DOP), median(vals$DOP), skewness(vals$DOP), kurtosis(vals$DOP)),
  GTQ = c(mean(vals$GTQ), var(vals$GTQ), min(vals$GTQ), max(vals$GTQ), median(vals$GTQ), skewness(vals$GTQ), kurtosis(vals$GTQ)),
  HNL = c(mean(vals$HNL), var(vals$HNL), min(vals$HNL), max(vals$HNL), median(vals$HNL), skewness(vals$HNL), kurtosis(vals$HNL)),
  MXN = c(mean(vals$MXN), var(vals$MXN), min(vals$MXN), max(vals$MXN), median(vals$MXN), skewness(vals$MXN), kurtosis(vals$MXN)),
  NIO = c(mean(vals$NIO), var(vals$NIO), min(vals$NIO), max(vals$NIO), median(vals$NIO), skewness(vals$NIO), kurtosis(vals$NIO)),
  PEN = c(mean(vals$PEN), var(vals$PEN), min(vals$PEN), max(vals$PEN), median(vals$PEN), skewness(vals$PEN), kurtosis(vals$PEN)),
  PYG = c(mean(vals$PYG), var(vals$PYG), min(vals$PYG), max(vals$PYG), median(vals$PYG), skewness(vals$PYG), kurtosis(vals$PYG)),
  UYU = c(mean(vals$UYU), var(vals$UYU), min(vals$UYU), max(vals$UYU), median(vals$UYU), skewness(vals$UYU), kurtosis(vals$UYU))
)

rownames(stats) <- stats$Statistic
stats <- select(stats, -c(Statistic))


# pruebas de estacionariedad (KPSS y ADF)

series_name <- c()
adf_pvalue <- c()
kpss_pvalue <- c()

for (series in series_names) {
  adf_test <- adf.test(rets[[series]], alternative = "stationary")$p.value
  kpss_test <- kpss.test(rets[[series]], null = "Level")$p.value
  
  series_name <- c(series_name, series)
  adf_pvalue <- c(adf_pvalue, adf_test)
  kpss_pvalue <- c(kpss_pvalue, kpss_test)
}

results <- data.frame(
  series_names = series_name,
  adf_pvalue = adf_pvalue,
  kpss_pvalue = kpss_pvalue
)
print(results)

ic <- VARselect(zoo_rets, lag.max = 6, type = 'const')
print(ic)
# Diebold-Yilmaz

zoo_rets <- zoo(rets[ , -1], order.by = rets$Date)
zoo_vals <- zoo(vals[ , -1], order.by = rets$Date)

dca <- ConnectednessApproach(zoo_rets,
                            nlag=1,
                            nfore=10,
                            model='VAR',
                            connectedness='Time',
                            Connectedness_config=list(TimeConnectedness=list(generalized=TRUE)))


kable(dca$TABLE) #tabla

dca <- ConnectednessApproach(zoo_rets,
                             nlag=1,
                             nfore=10,
                             window.size=60,
                             model='VAR',
                             connectedness='Time',
                             Connectedness_config=list(TimeConnectedness=list(generalized=TRUE)))

PlotTCI(dca)

PlotTO(dca)

PlotFROM(dca)

PlotNET(dca)


# Matriz de Diebold-Yilmaz



# Gráficos de Diebold-Yilmaz