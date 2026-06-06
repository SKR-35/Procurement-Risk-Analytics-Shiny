# ============================================================
# 05_vendor_scoring.R
# Vendor scorecard model
# ============================================================

source("R/04_risk_rules.R")

# ----------------------------
# Helper: min-max scaling
# ----------------------------

scale_0_1 <- function(x) {
  x_num <- suppressWarnings(as.numeric(x))

  min_x <- min(x_num, na.rm = TRUE)
  max_x <- max(x_num, na.rm = TRUE)

  if (is.infinite(min_x) || is.infinite(max_x) || max_x == min_x) {
    return(rep(0, length(x_num)))
  }

  (x_num - min_x) / (max_x - min_x)
}

# ----------------------------
# Helper: risk band
# ----------------------------

assign_risk_band <- function(score) {
  dplyr::case_when(
    is.na(score) ~ "Unknown",
    score >= 80 ~ "Critical",
    score >= 60 ~ "High",
    score >= 30 ~ "Medium",
    TRUE ~ "Low"
  )
}

# ----------------------------
# Contractor concentration from relationships
# ----------------------------

calculate_contractor_concentration <- function(relationships) {
  relationships |>
    dplyr::group_by(contractor_id_clean) |>
    dplyr::summarise(
      max_buyer_value_share_for_vendor = max(
        safe_divide(relationship_value, sum(relationship_value, na.rm = TRUE)),
        na.rm = TRUE
      ),
      max_buyer_contract_share_for_vendor = max(
        safe_divide(relationship_contracts, sum(relationship_contracts, na.rm = TRUE)),
        na.rm = TRUE
      ),
      top_buyer_contracts = max(relationship_contracts, na.rm = TRUE),
      top_buyer_value = max(relationship_value, na.rm = TRUE),
      .groups = "drop"
    )
}

# ----------------------------
# Build vendor scorecard
# ----------------------------

build_vendor_scorecard <- function(risk) {
  contractor_concentration <- calculate_contractor_concentration(risk$relationships)

  scorecard <- risk$contractor_summary |>
    dplyr::left_join(contractor_concentration, by = "contractor_id_clean") |>
    dplyr::mutate(
      high_value_score = scale_0_1(high_value_rate) * 10,
      round_number_score = scale_0_1(round_number_rate) * 15,
      single_offer_score = scale_0_1(single_offer_rate) * 30,
      low_competition_score = scale_0_1(low_competition_rate) * 25,
      concentration_score = scale_0_1(max_buyer_value_share_for_vendor) * 20,

      vendor_risk_score =
        high_value_score +
        round_number_score +
        single_offer_score +
        low_competition_score +
        concentration_score,

      vendor_risk_score = round(vendor_risk_score, 2),
      risk_band = assign_risk_band(vendor_risk_score)
    ) |>
    dplyr::arrange(dplyr::desc(vendor_risk_score))

  scorecard
}

# ----------------------------
# Add contractor names from contractors table
# ----------------------------

enrich_vendor_scorecard <- function(vendor_scorecard, contractors) {
  contractors_clean <- contractors |>
    dplyr::mutate(contractor_id_clean = clean_nip(nip)) |>
    dplyr::select(
      contractor_id_clean,
      contractor_name = name,
      contractor_city = city,
      contractor_province = province,
      contractor_country = country,
      contractor_profile_total_wins = total_wins,
      contractor_profile_total_value = total_value
    )

  vendor_scorecard |>
    dplyr::left_join(contractors_clean, by = "contractor_id_clean") |>
    dplyr::relocate(
      contractor_name,
      contractor_city,
      contractor_province,
      contractor_country,
      .after = contractor_id_clean
    )
}

# ----------------------------
# Main wrapper
# ----------------------------

run_vendor_scoring <- function(risk, prepared) {
  vendor_scorecard <- risk |>
    build_vendor_scorecard() |>
    enrich_vendor_scorecard(prepared$contractors)

  vendor_scorecard
}

# ----------------------------
# Quick profiling
# ----------------------------

profile_vendor_scorecard <- function(vendor_scorecard) {
  vendor_scorecard |>
    dplyr::count(risk_band, name = "vendors") |>
    dplyr::arrange(dplyr::desc(vendors))
}

# ----------------------------
# Top vendors helper
# ----------------------------

get_top_risky_vendors <- function(vendor_scorecard, n = 25) {
  vendor_scorecard |>
    dplyr::arrange(dplyr::desc(vendor_risk_score)) |>
    dplyr::slice_head(n = n)
}