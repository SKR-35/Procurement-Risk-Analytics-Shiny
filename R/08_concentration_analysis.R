# ============================================================
# 08_concentration_analysis.R
# Concentration and dependency analysis
# ============================================================

source("R/07_relationship_analysis.R")

# ----------------------------
# Helper: HHI interpretation
# ----------------------------

assign_hhi_band <- function(hhi) {
  dplyr::case_when(
    is.na(hhi) ~ "Unknown",
    hhi >= 0.50 ~ "Very High",
    hhi >= 0.25 ~ "High",
    hhi >= 0.15 ~ "Moderate",
    TRUE ~ "Low"
  )
}

# ----------------------------
# Buyer-side vendor concentration
# ----------------------------

calculate_buyer_vendor_concentration <- function(risk, buyer_scorecard) {
  buyer_light <- buyer_scorecard |>
    dplyr::select(
      buyer_nip_clean,
      buyer_name,
      buyer_risk_score,
      buyer_risk_band = risk_band
    )

  risk$relationships |>
    dplyr::filter(
      !is.na(buyer_nip_clean),
      !is.na(contractor_id_clean),
      !is.na(relationship_value),
      relationship_value > 0
    ) |>
    dplyr::group_by(buyer_nip_clean) |>
    dplyr::mutate(
      buyer_total_relationship_value = sum(relationship_value, na.rm = TRUE),
      buyer_total_relationship_contracts = sum(relationship_contracts, na.rm = TRUE),
      vendor_value_share = safe_divide(relationship_value, buyer_total_relationship_value),
      vendor_contract_share = safe_divide(relationship_contracts, buyer_total_relationship_contracts)
    ) |>
    dplyr::summarise(
      buyer_relationships = dplyr::n(),
      buyer_total_relationship_value = dplyr::first(buyer_total_relationship_value),
      buyer_total_relationship_contracts = dplyr::first(buyer_total_relationship_contracts),
      max_vendor_value_share = max(vendor_value_share, na.rm = TRUE),
      max_vendor_contract_share = max(vendor_contract_share, na.rm = TRUE),
      hhi_vendor_value = sum(vendor_value_share^2, na.rm = TRUE),
      hhi_vendor_contracts = sum(vendor_contract_share^2, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      hhi_vendor_value_band = assign_hhi_band(hhi_vendor_value),
      hhi_vendor_contracts_band = assign_hhi_band(hhi_vendor_contracts),
      buyer_vendor_concentration_score = round(
        100 * (
          0.45 * hhi_vendor_value +
            0.25 * hhi_vendor_contracts +
            0.20 * max_vendor_value_share +
            0.10 * max_vendor_contract_share
        ),
        2
      ),
      buyer_vendor_concentration_band = assign_risk_band(
        buyer_vendor_concentration_score
      )
    ) |>
    dplyr::left_join(buyer_light, by = "buyer_nip_clean") |>
    dplyr::arrange(dplyr::desc(buyer_vendor_concentration_score))
}

# ----------------------------
# Vendor-side buyer dependency
# ----------------------------

calculate_vendor_buyer_dependency <- function(risk, vendor_scorecard) {
  vendor_light <- vendor_scorecard |>
    dplyr::select(
      contractor_id_clean,
      contractor_name,
      vendor_risk_score,
      vendor_risk_band = risk_band
    )

  risk$relationships |>
    dplyr::filter(
      !is.na(contractor_id_clean),
      !is.na(buyer_nip_clean),
      !is.na(relationship_value),
      relationship_value > 0
    ) |>
    dplyr::group_by(contractor_id_clean) |>
    dplyr::mutate(
      vendor_total_relationship_value = sum(relationship_value, na.rm = TRUE),
      vendor_total_relationship_contracts = sum(relationship_contracts, na.rm = TRUE),
      buyer_value_share = safe_divide(relationship_value, vendor_total_relationship_value),
      buyer_contract_share = safe_divide(relationship_contracts, vendor_total_relationship_contracts)
    ) |>
    dplyr::summarise(
      vendor_relationships = dplyr::n(),
      vendor_total_relationship_value = dplyr::first(vendor_total_relationship_value),
      vendor_total_relationship_contracts = dplyr::first(vendor_total_relationship_contracts),
      max_buyer_value_share = max(buyer_value_share, na.rm = TRUE),
      max_buyer_contract_share = max(buyer_contract_share, na.rm = TRUE),
      hhi_buyer_value = sum(buyer_value_share^2, na.rm = TRUE),
      hhi_buyer_contracts = sum(buyer_contract_share^2, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      hhi_buyer_value_band = assign_hhi_band(hhi_buyer_value),
      hhi_buyer_contracts_band = assign_hhi_band(hhi_buyer_contracts),
      vendor_buyer_dependency_score = round(
        100 * (
          0.45 * hhi_buyer_value +
            0.25 * hhi_buyer_contracts +
            0.20 * max_buyer_value_share +
            0.10 * max_buyer_contract_share
        ),
        2
      ),
      vendor_buyer_dependency_band = assign_risk_band(
        vendor_buyer_dependency_score
      )
    ) |>
    dplyr::left_join(vendor_light, by = "contractor_id_clean") |>
    dplyr::arrange(dplyr::desc(vendor_buyer_dependency_score))
}

# ----------------------------
# Top dominant buyer-vendor pairs
# ----------------------------

calculate_dominant_relationships <- function(risk) {
  buyer_totals <- risk$relationships |>
    dplyr::group_by(buyer_nip_clean) |>
    dplyr::summarise(
      buyer_total_value = sum(relationship_value, na.rm = TRUE),
      buyer_total_contracts = sum(relationship_contracts, na.rm = TRUE),
      .groups = "drop"
    )

  vendor_totals <- risk$relationships |>
    dplyr::group_by(contractor_id_clean) |>
    dplyr::summarise(
      vendor_total_value = sum(relationship_value, na.rm = TRUE),
      vendor_total_contracts = sum(relationship_contracts, na.rm = TRUE),
      .groups = "drop"
    )

  risk$relationships |>
    dplyr::left_join(buyer_totals, by = "buyer_nip_clean") |>
    dplyr::left_join(vendor_totals, by = "contractor_id_clean") |>
    dplyr::mutate(
      buyer_value_share = safe_divide(relationship_value, buyer_total_value),
      buyer_contract_share = safe_divide(relationship_contracts, buyer_total_contracts),
      vendor_value_share = safe_divide(relationship_value, vendor_total_value),
      vendor_contract_share = safe_divide(relationship_contracts, vendor_total_contracts),
      mutual_dependency_score = round(
        100 * (
          0.35 * buyer_value_share +
            0.20 * buyer_contract_share +
            0.30 * vendor_value_share +
            0.15 * vendor_contract_share
        ),
        2
      ),
      mutual_dependency_band = assign_risk_band(mutual_dependency_score)
    ) |>
    dplyr::arrange(dplyr::desc(mutual_dependency_score))
}

# ----------------------------
# Main wrapper
# ----------------------------

run_concentration_analysis <- function(
    risk,
    buyer_scorecard,
    vendor_scorecard) {

  buyer_vendor_concentration <- calculate_buyer_vendor_concentration(
    risk,
    buyer_scorecard
  )

  vendor_buyer_dependency <- calculate_vendor_buyer_dependency(
    risk,
    vendor_scorecard
  )

  dominant_relationships <- calculate_dominant_relationships(risk)

  list(
    buyer_vendor_concentration = buyer_vendor_concentration,
    vendor_buyer_dependency = vendor_buyer_dependency,
    dominant_relationships = dominant_relationships
  )
}

# ----------------------------
# Profiling
# ----------------------------

profile_concentration_analysis <- function(concentration_analysis) {
  tibble::tibble(
    metric = c(
      "buyers_with_concentration_metrics",
      "vendors_with_dependency_metrics",
      "dominant_relationships",
      "high_or_critical_buyer_concentration",
      "high_or_critical_vendor_dependency",
      "high_or_critical_dominant_relationships"
    ),
    value = c(
      nrow(concentration_analysis$buyer_vendor_concentration),
      nrow(concentration_analysis$vendor_buyer_dependency),
      nrow(concentration_analysis$dominant_relationships),
      sum(
        concentration_analysis$buyer_vendor_concentration$buyer_vendor_concentration_band %in%
          c("High", "Critical"),
        na.rm = TRUE
      ),
      sum(
        concentration_analysis$vendor_buyer_dependency$vendor_buyer_dependency_band %in%
          c("High", "Critical"),
        na.rm = TRUE
      ),
      sum(
        concentration_analysis$dominant_relationships$mutual_dependency_band %in%
          c("High", "Critical"),
        na.rm = TRUE
      )
    )
  )
}

# ----------------------------
# Convenience helpers
# ----------------------------

get_top_buyer_concentration <- function(concentration_analysis, n = 25) {
  concentration_analysis$buyer_vendor_concentration |>
    dplyr::arrange(dplyr::desc(buyer_vendor_concentration_score)) |>
    dplyr::slice_head(n = n)
}

get_top_vendor_dependency <- function(concentration_analysis, n = 25) {
  concentration_analysis$vendor_buyer_dependency |>
    dplyr::arrange(dplyr::desc(vendor_buyer_dependency_score)) |>
    dplyr::slice_head(n = n)
}

get_top_dominant_relationships <- function(concentration_analysis, n = 25) {
  concentration_analysis$dominant_relationships |>
    dplyr::arrange(dplyr::desc(mutual_dependency_score)) |>
    dplyr::slice_head(n = n)
}