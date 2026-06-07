![R](https://img.shields.io/badge/R-4.6+-blue)
![Shiny](https://img.shields.io/badge/Shiny-Dashboard-green)
![License](https://img.shields.io/badge/License-Apache%202.0-orange)

# Procurement Risk Analytics (Polish Public Procurement Data)

An end-to-end procurement risk analytics framework built in R and Shiny using publicly available Polish procurement data.

The project combines statistical anomaly detection, buyer and vendor risk scoring, relationship analysis, competition indicators and interactive dashboards to identify procurement patterns that may warrant additional review.

---

## Data Source

This project uses public procurement data provided by Atlas Przetargów.

Data source:

* https://atlasprzetargow.pl/
* https://github.com/atlasprzetargow

The data remains the property of its original providers and is used here for educational, analytical and research purposes.

---

## Citation

This project uses procurement data provided by Atlas Przetargów.

Citation:

Atlas Przetargów. (2026). *Polish Public Tenders Dataset (BZP + TED)* (Version 2026.Q2) [Data set]. https://doi.org/10.5281/zenodo.19634050

## Project Goals

The framework analyzes more than 367,000 procurement notices and approximately 188,000 buyer-vendor relationships. The objective is not to determine wrongdoing.

Instead, the framework aims to:

* Identify statistical anomalies
* Prioritize procurement entities for review
* Detect unusual buyer-vendor relationships
* Measure competition levels
* Highlight concentration risks
* Support procurement audit and investigation workflows

The project follows a risk-based approach similar to those used in:

* Internal Audit
* Fraud Analytics
* Financial Crime Compliance
* Procurement Risk Management
* Public Sector Oversight

---

## Key Capabilities

### 1. Data Preparation

* Procurement notice ingestion
* Buyer enrichment
* Vendor enrichment
* Relationship construction
* Value normalization
* Data quality checks

---

### 2. Benford Analysis

The framework evaluates first-digit distributions against Benford's Law.

Features:

* Overall Benford analysis
* Province-level analysis
* CPV division analysis
* Buyer-level analysis
* Chi-square testing
* MAD (Mean Absolute Deviation)

Outputs:

* Actual vs Expected distributions
* Statistical deviation metrics
* Review candidates

---

### 3. Vendor Risk Scoring

Vendor scores are calculated using indicators such as:

* Single-offer contracts
* Low competition exposure
* Round-number contract values
* High-value awards
* Buyer concentration
* Relationship concentration

Outputs:

* Vendor risk score
* Percentile ranking
* Risk band classification

Risk Bands:

* Critical
* High
* Medium
* Low
* Unknown

---

### 4. Buyer Risk Scoring

Buyer-level scoring includes:

* Vendor concentration
* Repeat vendor relationships
* Competition metrics
* Procurement volume patterns
* High-value procurement exposure

Outputs:

* Buyer risk score
* Percentile ranking
* Risk band classification

---

### 5. Relationship Analytics

The framework constructs buyer-vendor networks and evaluates individual relationships.

Metrics include:

* Relationship contract count
* Relationship value
* Relationship concentration
* Single-offer relationship exposure
* Competition indicators

Outputs:

* Relationship risk score
* Top risky relationships
* Buyer-vendor overlap analysis

---

### 6. Concentration Analysis

Concentration analysis evaluates dependency and market concentration risks.

Examples:

* Buyer dependence on vendors
* Vendor dependence on buyers
* Value concentration
* Contract concentration
* HHI-style concentration indicators

---

### 7. Interactive Dashboard

The Shiny application provides:

* Risk summaries
* Vendor scorecards
* Buyer scorecards
* Relationship analysis
* Benford visualizations
* Procurement concentration metrics
* Interactive Plotly charts

---

# Project Architecture

```text
Procurement-Risk-Analytics-Shiny/
│
├── app.R
├── deploy_shinyapps.R
├── walkthrough.R
├── README.md
├── LICENSE
├── Procurement-Risk-Analytics-Shiny.Rproj
│
├── data/
│
├── outputs/
│   
└── R/
    ├── 01_load_data.R
    ├── 02_clean_prepare.R
    ├── 03_benford_analysis.R
    ├── 04_risk_rules.R
    ├── 05_vendor_scoring.R
    ├── 06_buyer_scoring.R
    ├── 07_relationship_analysis.R
    ├── 08_concentration_analysis.R
    └── 09_plots.R
```

### Script Overview

| Script                      | Purpose                                     |
| --------------------------- | ------------------------------------------- |
| 01_load_data.R              | Data ingestion and validation               |
| 02_clean_prepare.R          | Data preparation and enrichment             |
| 03_benford_analysis.R       | Benford Law anomaly detection               |
| 04_risk_rules.R             | Risk indicator construction                 |
| 05_vendor_scoring.R         | Vendor risk scorecard generation            |
| 06_buyer_scoring.R          | Buyer risk scorecard generation             |
| 07_relationship_analysis.R  | Buyer–vendor network analytics              |
| 08_concentration_analysis.R | Concentration and dependency metrics        |
| 09_plots.R                  | Plotly visualizations and dashboard helpers |

```
```

---

## Running the Project

Open the project in RStudio:

```r
source("R/01_load_data.R")
source("R/02_clean_prepare.R")
source("R/03_benford_analysis.R")
source("R/04_risk_rules.R")
source("R/05_vendor_scoring.R")
source("R/06_buyer_scoring.R")
source("R/07_relationship_analysis.R")
source("R/08_concentration_analysis.R")
source("R/09_plots.R")
```

Run the dashboard:

```r
shiny::runApp(launch.browser = TRUE)
```

---

## Example Use Cases

This framework can support:

* Procurement audit reviews
* Public procurement monitoring
* Vendor screening
* Competition analysis
* Fraud risk assessments
* Collusion screening
* Internal control testing
* Risk-based investigations

---

## Disclaimer

This project is intended for educational, analytical and risk-screening purposes.

Risk scores, anomaly indicators, concentration metrics and Benford deviations do not constitute evidence of misconduct.

All outputs should be treated as review signals requiring further investigation and professional judgment.

## Dataset Availability

This repository does not redistribute the original procurement dataset.

Users should obtain the source data directly from Atlas Przetargów:

- https://atlasprzetargow.pl/
- https://github.com/atlasprzetargow

Please refer to the original data provider for licensing terms, updates and usage conditions.