# Run this script step by step from the project root.
# Raw parquet files are expected under data/.
# Processed RDS outputs are written under outputs/.

install.packages("arrow")

source("R/00_config.R")

source("R/01_load_data.R")

data <- load_procurement_data()
profile_loaded_data(data)

glimpse(data$tenders)

source("R/02_clean_prepare.R")

prepared <- prepare_procurement_data(data)
profile_prepared_data(prepared)
glimpse(prepared$result_notices)

prepared$result_notices |>
  count(first_digit, sort = TRUE)

prepared$result_notices |>
  count(is_round_number)

source("R/03_benford_analysis.R")

benford <- run_overall_benford(prepared)

#Test:
benford$summary
benford$chi_square
benford$mad

#Grouped test:
benford_by_province <- run_benford_by_province(prepared, min_n = 1000)
benford_by_province

benford_by_cpv <- run_benford_by_cpv_division(prepared, min_n = 1000)
benford_by_cpv

#Buyer based test:
benford_by_buyer <- run_benford_by_buyer(prepared, min_n = 500)
head(benford_by_buyer, 20)

source("R/04_risk_rules.R")

risk <- run_risk_rules(prepared)

profile_risk_rules(risk)

risk$contractor_summary |>
  arrange(desc(low_competition_rate), desc(contractor_contracts)) |>
  head(20)

risk$buyer_summary |>
  arrange(desc(max_vendor_value_share), desc(buyer_contracts)) |>
  head(20)

risk$relationships |>
  arrange(desc(relationship_contracts), desc(relationship_value)) |>
  head(20)  

source("R/05_vendor_scoring.R")

vendor_scorecard <- run_vendor_scoring(risk, prepared)

profile_vendor_scorecard(vendor_scorecard)

get_top_risky_vendors(vendor_scorecard, 20)

vendor_scorecard |>
  select(
    contractor_id_clean,
    contractor_name,
    contractor_contracts,
    contractor_total_value,
    vendor_risk_score,
    risk_band,
    single_offer_rate,
    low_competition_rate,
    round_number_rate,
    max_buyer_value_share_for_vendor
  ) |>
  arrange(desc(vendor_risk_score)) |>
  head(20)

source("R/06_buyer_scoring.R")

buyer_scorecard <- run_buyer_scoring(
  risk,
  prepared
)

profile_buyer_scorecard(
  buyer_scorecard
)

get_top_risky_buyers(
  buyer_scorecard,
  20
)

source("R/07_relationship_analysis.R")

relationship_analysis <- run_relationship_analysis(
  risk,
  vendor_scorecard,
  buyer_scorecard,
  min_network_contracts = 10
)

profile_relationship_analysis(relationship_analysis)

get_top_risky_relationships(relationship_analysis, 20)

get_top_buyer_vendor_overlaps(relationship_analysis, 20)

source("R/08_concentration_analysis.R")

concentration_analysis <- run_concentration_analysis(
  risk,
  buyer_scorecard,
  vendor_scorecard
)

profile_concentration_analysis(concentration_analysis)

get_top_buyer_concentration(concentration_analysis, 20)
get_top_vendor_dependency(concentration_analysis, 20)
get_top_dominant_relationships(concentration_analysis, 20)

source("R/09_plots.R")

plot_benford_distribution(benford)
plot_vendor_risk_bands(vendor_scorecard)
plot_buyer_risk_bands(buyer_scorecard)
plot_top_vendors(vendor_scorecard)
plot_top_buyers(buyer_scorecard)
plot_top_relationships(relationship_analysis)

plot_top_vendors(vendor_scorecard, min_contracts = 3)
plot_relationship_value_vs_count(relationship_analysis)

plot_top_dominant_relationships(
  concentration_analysis,
  n = 15,
  min_contracts = 2
)

#For Shiny:
saveRDS(benford, "outputs/benford.rds")
saveRDS(risk, "outputs/risk.rds")
saveRDS(vendor_scorecard, "outputs/vendor_scorecard.rds")
saveRDS(buyer_scorecard, "outputs/buyer_scorecard.rds")
saveRDS(relationship_analysis, "outputs/relationship_analysis.rds")
saveRDS(concentration_analysis, "outputs/concentration_analysis.rds")


install.packages(c(
  "shiny",
  "shinydashboard",
  "plotly",
  "DT",
  "dplyr",
  "scales",
  "pagedown",
  "htmltools"
))

options(shiny.launch.browser = TRUE)

shiny::runApp()

#Or

shiny::runApp(
  launch.browser = TRUE
)

#Deploy
source("deploy_shinyapps.R")