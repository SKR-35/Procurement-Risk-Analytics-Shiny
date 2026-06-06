# ============================================================
# 04_risk_rules.R
# Rule-based procurement risk signals
# ============================================================

source("R/03_benford_analysis.R")

# ----------------------------
# Helper: percentile rank
# ----------------------------

percentile_rank_safe <- function(x) {
  dplyr::percent_rank(x)
}

# ----------------------------
# Helper: high value threshold
# ----------------------------

calculate_high_value_threshold <- function(df, value_col = estimated_value_num, q = 0.99) {
  value_col <- rlang::ensym(value_col)

  df |>
    dplyr::filter(!is.na(!!value_col), !!value_col > 0) |>
    dplyr::summarise(threshold = stats::quantile(!!value_col, probs = q, na.rm = TRUE)) |>
    dplyr::pull(threshold)
}

# ----------------------------
# Add basic transaction-level risk rules
# ----------------------------

add_transaction_risk_rules <- function(df) {
  high_value_threshold <- calculate_high_value_threshold(df, estimated_value_num, q = 0.99)

  df |>
    dplyr::mutate(
      value_percentile = percentile_rank_safe(estimated_value_num),

      risk_high_value = !is.na(estimated_value_num) &
        estimated_value_num >= high_value_threshold,

      risk_round_number = is_round_number,

      risk_single_offer = !is.na(offers_count_num) &
        offers_count_num == 1,

      risk_low_competition = !is.na(offers_count_num) &
        offers_count_num <= 2,

      risk_missing_offers_count = is.na(offers_count_num),

      risk_missing_contractor = !has_contractor,

      risk_missing_buyer = !has_buyer
    )
}

# ----------------------------
# Buyer-contractor relationship concentration
# ----------------------------

calculate_buyer_contractor_relationships <- function(result_notices) {
  result_notices |>
    dplyr::filter(
      has_buyer,
      has_contractor,
      !is.na(estimated_value_num),
      estimated_value_num > 0
    ) |>
    dplyr::group_by(buyer_nip_clean, contractor_id_clean) |>
    dplyr::summarise(
      relationship_contracts = dplyr::n(),
      relationship_value = sum(estimated_value_num, na.rm = TRUE),
      avg_relationship_value = mean(estimated_value_num, na.rm = TRUE),
      single_offer_contracts = sum(risk_single_offer, na.rm = TRUE),
      low_competition_contracts = sum(risk_low_competition, na.rm = TRUE),
      round_number_contracts = sum(risk_round_number, na.rm = TRUE),
      high_value_contracts = sum(risk_high_value, na.rm = TRUE),
      .groups = "drop"
    )
}

# ----------------------------
# Buyer-level concentration metrics
# ----------------------------

calculate_buyer_concentration <- function(relationships) {
  buyer_totals <- relationships |>
    dplyr::group_by(buyer_nip_clean) |>
    dplyr::summarise(
      buyer_relationships = dplyr::n(),
      buyer_total_contracts = sum(relationship_contracts, na.rm = TRUE),
      buyer_total_value = sum(relationship_value, na.rm = TRUE),
      .groups = "drop"
    )

  relationships |>
    dplyr::left_join(buyer_totals, by = "buyer_nip_clean") |>
    dplyr::mutate(
      relationship_value_share = safe_divide(relationship_value, buyer_total_value),
      relationship_contract_share = safe_divide(relationship_contracts, buyer_total_contracts)
    ) |>
    dplyr::group_by(buyer_nip_clean) |>
    dplyr::summarise(
      buyer_relationships = dplyr::first(buyer_relationships),
      buyer_total_contracts = dplyr::first(buyer_total_contracts),
      buyer_total_value = dplyr::first(buyer_total_value),
      max_vendor_value_share = max(relationship_value_share, na.rm = TRUE),
      max_vendor_contract_share = max(relationship_contract_share, na.rm = TRUE),
      hhi_value = sum(relationship_value_share^2, na.rm = TRUE),
      hhi_contracts = sum(relationship_contract_share^2, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      risk_high_vendor_value_concentration = max_vendor_value_share >= 0.50 &
        buyer_total_contracts >= 5,

      risk_high_vendor_contract_concentration = max_vendor_contract_share >= 0.50 &
        buyer_total_contracts >= 5,

      risk_high_hhi_value = hhi_value >= 0.25 &
        buyer_total_contracts >= 5
    )
}

# ----------------------------
# Contractor-level aggregation
# ----------------------------

calculate_contractor_risk_summary <- function(result_notices_with_rules) {
  result_notices_with_rules |>
    dplyr::filter(has_contractor) |>
    dplyr::group_by(contractor_id_clean) |>
    dplyr::summarise(
      contractor_contracts = dplyr::n(),
      contractor_total_value = sum(estimated_value_num, na.rm = TRUE),
      contractor_avg_value = mean(estimated_value_num, na.rm = TRUE),
      unique_buyers = dplyr::n_distinct(buyer_nip_clean, na.rm = TRUE),

      high_value_contracts = sum(risk_high_value, na.rm = TRUE),
      round_number_contracts = sum(risk_round_number, na.rm = TRUE),
      single_offer_contracts = sum(risk_single_offer, na.rm = TRUE),
      low_competition_contracts = sum(risk_low_competition, na.rm = TRUE),

      high_value_rate = safe_divide(high_value_contracts, contractor_contracts),
      round_number_rate = safe_divide(round_number_contracts, contractor_contracts),
      single_offer_rate = safe_divide(single_offer_contracts, contractor_contracts),
      low_competition_rate = safe_divide(low_competition_contracts, contractor_contracts),

      .groups = "drop"
    )
}

# ----------------------------
# Buyer-level aggregation
# ----------------------------

calculate_buyer_risk_summary <- function(result_notices_with_rules) {
  result_notices_with_rules |>
    dplyr::filter(has_buyer) |>
    dplyr::group_by(buyer_nip_clean) |>
    dplyr::summarise(
      buyer_contracts = dplyr::n(),
      buyer_total_value = sum(estimated_value_num, na.rm = TRUE),
      buyer_avg_value = mean(estimated_value_num, na.rm = TRUE),
      unique_contractors = dplyr::n_distinct(contractor_id_clean, na.rm = TRUE),

      high_value_contracts = sum(risk_high_value, na.rm = TRUE),
      round_number_contracts = sum(risk_round_number, na.rm = TRUE),
      single_offer_contracts = sum(risk_single_offer, na.rm = TRUE),
      low_competition_contracts = sum(risk_low_competition, na.rm = TRUE),

      high_value_rate = safe_divide(high_value_contracts, buyer_contracts),
      round_number_rate = safe_divide(round_number_contracts, buyer_contracts),
      single_offer_rate = safe_divide(single_offer_contracts, buyer_contracts),
      low_competition_rate = safe_divide(low_competition_contracts, buyer_contracts),

      .groups = "drop"
    )
}

# ----------------------------
# Risk rules wrapper
# ----------------------------

run_risk_rules <- function(prepared) {
  result_notices_with_rules <- prepared$result_notices |>
    add_transaction_risk_rules()

  relationships <- result_notices_with_rules |>
    calculate_buyer_contractor_relationships()

  buyer_concentration <- relationships |>
    calculate_buyer_concentration()

  contractor_summary <- result_notices_with_rules |>
    calculate_contractor_risk_summary()

  buyer_summary <- result_notices_with_rules |>
    calculate_buyer_risk_summary() |>
    dplyr::left_join(buyer_concentration, by = "buyer_nip_clean")

  list(
    result_notices = result_notices_with_rules,
    relationships = relationships,
    buyer_concentration = buyer_concentration,
    contractor_summary = contractor_summary,
    buyer_summary = buyer_summary
  )
}

# ----------------------------
# Quick profiling
# ----------------------------

profile_risk_rules <- function(risk) {
  tibble::tibble(
    metric = c(
      "result_notices",
      "relationships",
      "contractors",
      "buyers",
      "high_value_contracts",
      "round_number_contracts",
      "single_offer_contracts",
      "low_competition_contracts"
    ),
    value = c(
      nrow(risk$result_notices),
      nrow(risk$relationships),
      nrow(risk$contractor_summary),
      nrow(risk$buyer_summary),
      sum(risk$result_notices$risk_high_value, na.rm = TRUE),
      sum(risk$result_notices$risk_round_number, na.rm = TRUE),
      sum(risk$result_notices$risk_single_offer, na.rm = TRUE),
      sum(risk$result_notices$risk_low_competition, na.rm = TRUE)
    )
  )
}