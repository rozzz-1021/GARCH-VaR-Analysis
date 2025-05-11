# GARCH(1,1)-t Volatility and VaR Analysis of Sector ETFs

This project analyzes the volatility and Value at Risk (VaR) of major sector indices using the GARCH(1,1) model with t-distributed errors. The aim is to assess risk dynamics in different industries and evaluate the accuracy of the VaR predictions via backtesting.

---

## Dataset

We used historical daily adjusted closing prices (2019–2024) of the following ETFs:

- **NASDAQ 100** (representing tech-heavy composite index)
- **XLF**: Financial Select Sector SPDR Fund
- **XLI**: Industrial Select Sector SPDR Fund
- **XLE**: Energy Select Sector SPDR Fund

Data was sourced from Yahoo Finance and preprocessed using `tidyverse` and `lubridate`.

---

## Methodology

1. **Log Return Calculation**
2. **GARCH(1,1)-t Model Fitting** using `rugarch`
3. **Conditional Volatility Visualization**
4. **VaR Estimation** at 95% confidence level
5. **Backtesting** violation rates to assess model reliability

---

## Visualizations

### Log Returns

| | |
|--|--|
| ![NASDAQ](plots/Log Return of NASDAQ 100.png) | ![XLE](plots/Log Return of XLE.png) |
| ![XLF](plots/Log Return of XLF.png) | ![XLI](plots/Log Return of XLI.png) |

### GARCH(1,1)-t Conditional Volatility

![Volatility](plots/GARCH volatility.png)

---

## Backtesting Summary

| Index           | Violations | Total Observations | Violation Rate (%) |
|----------------|------------|---------------------|---------------------|
| NASDAQ 100     | 38         | 1285                | 2.96%               |
| XLF (Financial)| 44         | 1285                | 3.42%               |
| XLI (Industrial)| 41         | 1285                | 3.19%               |
| XLE (Energy)   | 53         | 1285                | 4.12%               |

---

## Insights

- The model performed well across most sectors, producing conservative estimates for NASDAQ.
- The highest violation rate appeared in the **XLE (Energy)** sector, suggesting higher volatility and potential model underestimation.
- GARCH(1,1)-t models are suitable for capturing volatility clustering and fat tails in financial returns.

---

## R Packages Used

- `tidyverse`
- `lubridate`
- `rugarch`
- `ggplot2`
- `knitr` / `gt` (for tables)

---

##  Project Structure

GARCH-VaR-Analysis/
├── data/ # Cleaned CSVs
├── plots/ # Exported plots (.png)
├── analysis.R # Main R script with comments
├── README.md # This file

##  Author

Jo-Tzu Lu 
Master of Finance – University of Southampton  
[GitHub Profile](https://github.com/your-rozzz-1021)


