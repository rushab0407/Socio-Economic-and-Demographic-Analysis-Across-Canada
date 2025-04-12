# 🇨🇦 Socio-Economic and Demographic Analysis Across Canada

A comprehensive R-based project that analyzes the socio-economic landscape of Canada using Census 2021 data and employment statistics (2000–2023).  
The study explores key themes such as population distribution, education levels, employment trends, commuting behavior, and migration patterns, with visualizations and statistical modeling to support insights.

**Authors:** Rushab Arram, Maheshwar Reddy Gujjula  
**Institution:** Department of Applied Modelling and Quantitative Methods, Trent University  

---

## 📁 Project Structure


---

## 🔬 Methodologies Used

- Exploratory Data Analysis (EDA)
- ANOVA and Tukey HSD Post-Hoc Testing
- Multiple Linear Regression
- Time Series Forecasting (Holt-Winters, ARIMA, Ensemble)
- Data visualization using `ggplot2`, `RColorBrewer`, `scales`

---

## 📜 Scripts

### 🔹 `empdata.R` — Employment & Education Analysis and Forecasting

This script processes and models Canadian employment rate data across time, education levels, and gender.

**Main Features:**
- Cleans and encodes demographic and categorical data
- Builds a multiple linear regression model with 10-fold cross-validation
- Tests the effect of education level on employment using ANOVA and Tukey's HSD
- Groups similar education levels for clearer analysis
- Implements time series forecasting for 2024–2025 using:
  - Holt-Winters
  - ARIMA
  - Ensemble averaging
- Produces comparative employment forecast plots

📈 **Key Output**: Employment forecasts and insights on education-employment relationships.

---

### 🔹 `popfile.R` — Census-Based Demographic and Transportation Analysis

This script explores Canada's demographic and transportation data from the 2021 Census.

**Main Features:**
- Cleans census data and renames variables for clarity
- Visualizes:
  - Population by province (bar & pie chart)
  - Age group distribution
  - Educational attainment percentages
  - Population percentage changes from 2016 to 2021
  - Mode of transport usage per province
  - Immigration trends over time
- Aggregates data for grouped summaries by province

📊 **Key Output**: Clear visual breakdowns of demographic, educational, and transport trends across Canada.

---

![Dashboard Screenshot](assets/v1234.png)

## 📊 Forecasting Summary

| Year | Holt-Winters | ARIMA | Ensemble Avg |
|------|--------------|--------|--------------|
| 2024 | 73.23%       | 72.42% | 72.83%       |
| 2025 | 73.37%       | 72.14% | 72.76%       |

---

## 📚 Data Sources

- [Statistics Canada – Census 2021](https://www12.statcan.gc.ca/census-recensement/2021/)
- [Labour Force Survey (LFS)](https://www150.statcan.gc.ca/n1/en/subjects/labour/)
- [Canadian Income Survey](https://www150.statcan.gc.ca/n1/daily-quotidien/240426/dq240426a-eng.htm)

---

## ✅ Requirements

- R (>= 4.0.0)
- RStudio
- Packages: `tidyverse`, `caret`, `forecast`, `ggplot2`, `reshape2`, `RColorBrewer`, `scales`, `ggtext`, `tseries`

---

## 📄 License

This project is for academic and research purposes under the guidance of Trent University's Department of Applied Modelling and Quantitative Methods.

