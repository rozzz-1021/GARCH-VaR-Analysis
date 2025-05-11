#Load Required Packages
library(tidyverse)
library(lubridate)
# Read NASDAQ 100 data
nasdaq <- read_csv("data/nasdaq100.csv") %>%
  select(Date, Price) %>%
  mutate(Date = mdy(Date),
         Price = as.numeric(gsub(",", "", Price))) %>%
  arrange(Date) %>%
  mutate(log_return = log(Price / lag(Price))) %>%
  drop_na()
getwd()
list.files("data")
# Read xle
xle <- read_csv("data/xle.csv") %>%
  select(Date, Price) %>%
  mutate(Date = mdy(Date),
         Price = as.numeric(gsub(",", "", Price))) %>%
  arrange(Date) %>%
  mutate(log_return = log(Price / lag(Price))) %>%
  drop_na()
# Read xlf
xlf <- read_csv("data/xlf.csv") %>%
  select(Date, Price) %>%
  mutate(Date = mdy(Date),
         Price = as.numeric(gsub(",", "", Price))) %>%
  arrange(Date) %>%
  mutate(log_return = log(Price / lag(Price))) %>%
  drop_na()
# Read xli
xli <- read_csv("data/xli.csv") %>%
  select(Date, Price) %>%
  mutate(Date = mdy(Date),
         Price = as.numeric(gsub(",", "", Price))) %>%
  arrange(Date) %>%
  mutate(log_return = log(Price / lag(Price))) %>%
  drop_na()
# Draw Time-Series Trend
library(ggplot2)
install.packages("ggpubr")
library(ggpubr)
p1 <- ggplot(nasdaq, aes(Date, log_return)) +
  geom_line(color = "#2E86AB") +
  labs(title = "Log Return of NASDAQ 100",
       x = "Date", y = "Log Return") +
  theme_minimal()
p2 <- ggplot(xlf, aes(x = Date, y = log_return)) +
  geom_line(color = "#E67E22") +
  labs(title = "Log Return of XLF (Financial)", x = "Date", y = "Log Return") +
  theme_minimal()
p3 <- ggplot(xli, aes(x = Date, y = log_return)) +
  geom_line(color = "#566573") +
  labs(title = "Log Return of XLI (Industrial)", x = "Date", y = "Log Return") +
  theme_minimal()
p4 <- ggplot(xle, aes(x = Date, y = log_return)) +
  geom_line(color = "#27AE60") +
  labs(title = "Log Return of XLE (Energy)", x = "Date", y = "Log Return") +
  theme_minimal()
combined <- ggarrange(p1, p2, p3, p4, ncol = 2, nrow = 2)
ggsave("log_return_combined.png", combined, width = 12, height = 8, dpi = 3000)
# Build GARcH(1, 1) model
install.packages("rugarch")   
library(rugarch)
spec_std <- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
  mean.model = list(armaOrder = c(0, 0), include.mean = TRUE),
  distribution.model = "std"
)
fit_nasdaq <- ugarchfit(spec = spec_std, data = nasdaq$log_return)
fit_xlf = ugarchfit(spec = spec_std, data = xlf$log_return)
fit_xli    <- ugarchfit(spec = spec_std, data = xli$log_return)
fit_xle    <- ugarchfit(spec = spec_std, data = xle$log_return)
# Get Conditional Volatility
nasdaq_vol <- sigma(fit_nasdaq)
xlf_vol    <- sigma(fit_xlf)
xli_vol    <- sigma(fit_xli)
xle_vol    <- sigma(fit_xle)
dates_nasdaq <- tail(nasdaq$Date, length(nasdaq_vol))
dates_xlf    <- tail(xlf$Date, length(xlf_vol))
dates_xli    <- tail(xli$Date, length(xli_vol))
dates_xle    <- tail(xle$Date, length(xle_vol))

plot(dates_nasdaq, nasdaq_vol, type = "l", col = "#2E86AB",
     ylim = range(c(nasdaq_vol, xlf_vol, xli_vol, xle_vol)),
     main = "GARCH(1,1) Volatility with t-distribution",
     xlab = "Date", ylab = "Conditional Volatility")

lines(dates_xlf, xlf_vol, col = "#E67E22")
lines(dates_xli, xli_vol, col = "#566573")
lines(dates_xle, xle_vol, col = "#27AE60")

legend("topright", legend = c("NASDAQ", "XLF", "XLI", "XLE"),
       col = c("#2E86AB", "#E67E22", "#566573", "#27AE60"), lty = 1)
alpha_95 <- 0.05
alpha_99 <- 0.01
calculate_and_plot_var <- function(data, fit, name, color95, color99) {
  sigma_hat <- sigma(fit)
  returns <- tail(data$log_return, length(sigma_hat))
  dates <- tail(data$Date, length(sigma_hat))
  nu <- coef(fit)["shape"]  # degree of freedom
  
  q_95 <- qt(alpha_95, df = nu)
  q_99 <- qt(alpha_99, df = nu)
  
  VaR_95 <- -sigma_hat * q_95
  VaR_99 <- -sigma_hat * q_99
  
  # plot
  plot(dates, VaR_95, type = "l", col = color95,
       main = paste(name, "GARCH-t VaR (95% & 99%)"),
       xlab = "Date", ylab = "VaR")
  lines(dates, VaR_99, col = color99)
  legend("topright", legend = c("VaR 95%", "VaR 99%"),
         col = c(color95, color99), lty = 1)
  
  return(data.frame(Date = dates, VaR_95 = VaR_95, VaR_99 = VaR_99))
}

# calculation and plot
var_nasdaq <- calculate_and_plot_var(nasdaq, fit_nasdaq, "NASDAQ", "#2E86AB", "#154360")
var_xlf    <- calculate_and_plot_var(xlf, fit_xlf, "XLF", "#E67E22", "#A04000")
var_xli    <- calculate_and_plot_var(xli, fit_xli, "XLI", "#566573", "#17202A")
var_xle    <- calculate_and_plot_var(xle, fit_xle, "XLE", "#27AE60", "#145A32")
# BackTesting
sigma_nasdaq <- sigma(fit_nasdaq)
ret_nasdaq <- tail(nasdaq$log_return, length(sigma_nasdaq))
nu_nasdaq <- coef(fit_nasdaq)["shape"]
q_95_nasdaq <- qt(0.05, df = nu_nasdaq)
VaR_95_nasdaq <- -sigma_nasdaq * q_95_nasdaq
violation_95_nasdaq <- as.numeric(ret_nasdaq < -VaR_95_nasdaq)
count_nasdaq <- sum(violation_95_nasdaq)
rate_nasdaq <- count_nasdaq / length(ret_nasdaq)
cat("NASDAQ 100 GARCH-t VaR Backtesting (95% confidence level)\n")
cat("Violations:", count_nasdaq, "out of", length(ret_nasdaq),
    "| Violation rate:", round(rate_nasdaq * 100, 2), "%\n\n")
sigma_xlf <- sigma(fit_xlf)
ret_xlf <- tail(xlf$log_return, length(sigma_xlf))
nu_xlf <- coef(fit_xlf)["shape"]
q_95_xlf <- qt(0.05, df = nu_xlf)
VaR_95_xlf <- -sigma_xlf * q_95_xlf
violation_95_xlf <- as.numeric(ret_xlf < -VaR_95_xlf)
count_xlf <- sum(violation_95_xlf)
rate_xlf <- count_xlf / length(ret_xlf)
cat("XLF (Financial) GARCH-t VaR Backtesting (95% confidence level)\n")
cat("Violations:", count_xlf, "out of", length(ret_xlf),
    "| Violation rate:", round(rate_xlf * 100, 2), "%\n\n")
sigma_xli <- sigma(fit_xli)
ret_xli <- tail(xli$log_return, length(sigma_xli))
nu_xli <- coef(fit_xli)["shape"]
q_95_xli <- qt(0.05, df = nu_xli)
VaR_95_xli <- -sigma_xli * q_95_xli
violation_95_xli <- as.numeric(ret_xli < -VaR_95_xli)
count_xli <- sum(violation_95_xli)
rate_xli <- count_xli / length(ret_xli)
cat("XLI (Industrial) GARCH-t VaR Backtesting (95% confidence level)\n")
cat("Violations:", count_xli, "out of", length(ret_xli),
    "| Violation rate:", round(rate_xli * 100, 2), "%\n\n")
sigma_xle <- sigma(fit_xle)
ret_xle <- tail(xle$log_return, length(sigma_xle))
nu_xle <- coef(fit_xle)["shape"]
q_95_xle <- qt(0.05, df = nu_xle)
VaR_95_xle <- -sigma_xle * q_95_xle
violation_95_xle <- as.numeric(ret_xle < -VaR_95_xle)
count_xle <- sum(violation_95_xle)
rate_xle <- count_xle / length(ret_xle)
cat("XLE (Energy) GARCH-t VaR Backtesting (95% confidence level)\n")
cat("Violations:", count_xle, "out of", length(ret_xle),
    "| Violation rate:", round(rate_xle * 100, 2), "%\n\n")
violation_df <- data.frame(
  Index = c("NASDAQ 100", "XLF (Financial)", "XLI (Industrial)", "XLE (Energy)"),
  Violations = c(38, 44, 41, 53),
  Total = c(1285, 1285, 1285, 1285)
)
violation_df$`Violation Rate (%)` <- round(violation_df$Violations / violation_df$Total * 100, 2)
install.packages("knitr")
library(knitr)
kable(violation_df, format = "markdown", caption = "GARCH(1,1)-t VaR Violation Summary (95% Confidence Level)")