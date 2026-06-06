# ============================================================
# 07_relationship_analysis.R
# Buyer-vendor relationship risk analysis
# ============================================================

source("R/06_buyer_scoring.R")

# ----------------------------
# Helper: coalesce possible column names
# ----------------------------

coalesce_existing_cols <- function(df, new_col, candidates) {
  existing <- intersect(candidates, names(df))

  if (length(existing) == 0) {
    df[[new_col]] <- NA_real_
    return(df)
  }

  df[[new_col]] <- dplyr::coalesce(!!!df[existing])
  df
}

# ----------------------------
# Build relationship scorecard
# ----------------------------

build_relationship_scorecard <- function(risk, vendor_scorecard, buyer_scorecard) {
  vendor_light <- vendor_scorecard |>
    dplyr::select(
      contractor_id_clean,
      contractor_name,
      vendor_risk_score,
      vendor_risk_band = risk_band
    )

  buyer_light <- buyer_scorecard |>
    dplyr::select(
      buyer_nip_clean,
      buyer_name,
      buyer_risk_score,
      buyer_risk_band = risk_band
    )

  risk$relationships |>
    dplyr::left_join(vendor_light, by = "contractor_id_clean") |>
    dplyr::left_join(buyer_light, by = "buyer_nip_clean") |>
    dplyr::mutate(
      relationship_contract_share_score = scale_0_1(relationship_contracts) * 20,
      relationship_value_score = scale_0_1(relationship_value) * 25,

      single_offer_relationship_score =
        scale_0_1(safe_divide(single_offer_contracts, relationship_contracts)) * 20,

      low_competition_relationship_score =
        scale_0_1(safe_divide(low_competition_contracts, relationship_contracts)) * 15,

      round_number_relationship_score =
        scale_0_1(safe_divide(round_number_contracts, relationship_contracts)) * 10,

      high_value_relationship_score =
        scale_0_1(safe_divide(high_value_contracts, relationship_contracts)) * 10,

      relationship_risk_score =
        relationship_contract_share_score +
        relationship_value_score +
        single_offer_relationship_score +
        low_competition_relationship_score +
        round_number_relationship_score +
        high_value_relationship_score,

      relationship_risk_score = round(relationship_risk_score, 2),
      relationship_risk_band = assign_risk_band(relationship_risk_score)
    ) |>
    dplyr::arrange(dplyr::desc(relationship_risk_score))
}

# ----------------------------
# Find entities appearing as both buyer and vendor
# ----------------------------

find_buyer_vendor_overlap <- function(buyer_scorecard, vendor_scorecard) {
  buyer_scorecard <- buyer_scorecard |>
    coalesce_existing_cols(
      "buyer_total_value_clean",
      c("buyer_total_value", "buyer_total_value.x", "buyer_total_value.y", "total_value")
    )

  vendor_scorecard <- vendor_scorecard |>
    coalesce_existing_cols(
      "contractor_total_value_clean",
      c("contractor_total_value", "contractor_total_value.x", "contractor_total_value.y", "total_value")
    )

  buyers <- buyer_scorecard |>
    dplyr::select(
      nip = buyer_nip_clean,
      buyer_name,
      buyer_risk_score,
      buyer_risk_band = risk_band,
      buyer_contracts,
      buyer_total_value = buyer_total_value_clean
    )

  vendors <- vendor_scorecard |>
    dplyr::select(
      nip = contractor_id_clean,
      contractor_name,
      vendor_risk_score,
      vendor_risk_band = risk_band,
      contractor_contracts,
      contractor_total_value = contractor_total_value_clean
    )

  buyers |>
    dplyr::inner_join(vendors, by = "nip") |>
    dplyr::mutate(
      both_sides_high_risk =
        buyer_risk_band %in% c("High", "Critical") &
        vendor_risk_band %in% c("High", "Critical"),

      combined_risk_score =
        round((buyer_risk_score + vendor_risk_score) / 2, 2)
    ) |>
    dplyr::arrange(dplyr::desc(combined_risk_score))
}

# ----------------------------
# Build lightweight network edges
# ----------------------------

build_relationship_network_edges <- function(relationship_scorecard, min_contracts = 5) {
  relationship_scorecard |>
    dplyr::filter(
      relationship_contracts >= min_contracts,
      !is.na(buyer_nip_clean),
      !is.na(contractor_id_clean)
    ) |>
    dplyr::transmute(
      from = paste0("buyer_", buyer_nip_clean),
      to = paste0("vendor_", contractor_id_clean),
      buyer_nip_clean,
      contractor_id_clean,
      buyer_name,
      contractor_name,
      weight = relationship_contracts,
      value = relationship_value,
      relationship_risk_score,
      relationship_risk_band
    )
}

# ----------------------------
# Build lightweight network nodes
# ----------------------------

build_relationship_network_nodes <- function(edges, buyer_scorecard, vendor_scorecard) {
  buyer_nodes <- edges |>
    dplyr::distinct(id = from, buyer_nip_clean, buyer_name) |>
    dplyr::left_join(
      buyer_scorecard |>
        dplyr::select(
          buyer_nip_clean,
          risk_score = buyer_risk_score,
          risk_band
        ),
      by = "buyer_nip_clean"
    ) |>
    dplyr::mutate(
      label = dplyr::coalesce(buyer_name, buyer_nip_clean),
      type = "Buyer"
    ) |>
    dplyr::select(id, label, type, risk_score, risk_band)

  vendor_nodes <- edges |>
    dplyr::distinct(id = to, contractor_id_clean, contractor_name) |>
    dplyr::left_join(
      vendor_scorecard |>
        dplyr::select(
          contractor_id_clean,
          risk_score = vendor_risk_score,
          risk_band
        ),
      by = "contractor_id_clean"
    ) |>
    dplyr::mutate(
      label = dplyr::coalesce(contractor_name, contractor_id_clean),
      type = "Vendor"
    ) |>
    dplyr::select(id, label, type, risk_score, risk_band)

  dplyr::bind_rows(buyer_nodes, vendor_nodes) |>
    dplyr::distinct(id, .keep_all = TRUE)
}

# ----------------------------
# Main wrapper
# ----------------------------

run_relationship_analysis <- function(
    risk,
    vendor_scorecard,
    buyer_scorecard,
    min_network_contracts = 5) {

  relationship_scorecard <- build_relationship_scorecard(
    risk,
    vendor_scorecard,
    buyer_scorecard
  )

  buyer_vendor_overlap <- find_buyer_vendor_overlap(
    buyer_scorecard,
    vendor_scorecard
  )

  network_edges <- build_relationship_network_edges(
    relationship_scorecard,
    min_contracts = min_network_contracts
  )

  network_nodes <- build_relationship_network_nodes(
    network_edges,
    buyer_scorecard,
    vendor_scorecard
  )

  list(
    relationship_scorecard = relationship_scorecard,
    buyer_vendor_overlap = buyer_vendor_overlap,
    network_edges = network_edges,
    network_nodes = network_nodes
  )
}

# ----------------------------
# Profiling
# ----------------------------

profile_relationship_analysis <- function(relationship_analysis) {
  tibble::tibble(
    metric = c(
      "relationships",
      "buyer_vendor_overlap",
      "both_sides_high_risk",
      "network_edges",
      "network_nodes"
    ),
    value = c(
      nrow(relationship_analysis$relationship_scorecard),
      nrow(relationship_analysis$buyer_vendor_overlap),
      sum(relationship_analysis$buyer_vendor_overlap$both_sides_high_risk, na.rm = TRUE),
      nrow(relationship_analysis$network_edges),
      nrow(relationship_analysis$network_nodes)
    )
  )
}

# ----------------------------
# Top relationship helpers
# ----------------------------

get_top_risky_relationships <- function(relationship_analysis, n = 25) {
  relationship_analysis$relationship_scorecard |>
    dplyr::arrange(dplyr::desc(relationship_risk_score)) |>
    dplyr::slice_head(n = n)
}

get_top_buyer_vendor_overlaps <- function(relationship_analysis, n = 25) {
  relationship_analysis$buyer_vendor_overlap |>
    dplyr::arrange(dplyr::desc(combined_risk_score)) |>
    dplyr::slice_head(n = n)
}