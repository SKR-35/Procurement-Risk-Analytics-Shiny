# ============================================================
# 06_buyer_scoring.R
# Buyer scorecard model
# ============================================================

source("R/05_vendor_scoring.R")

# ----------------------------
# Build buyer scorecard
# ----------------------------

build_buyer_scorecard <- function(risk) {

  scorecard <- risk$buyer_summary |>
    dplyr::mutate(

      high_value_score =
        scale_0_1(high_value_rate) * 10,

      round_number_score =
        scale_0_1(round_number_rate) * 15,

      single_offer_score =
        scale_0_1(single_offer_rate) * 25,

      low_competition_score =
        scale_0_1(low_competition_rate) * 20,

      concentration_score =
        scale_0_1(max_vendor_value_share) * 20,

      hhi_score =
        scale_0_1(hhi_value) * 10,

      buyer_risk_score =
        high_value_score +
        round_number_score +
        single_offer_score +
        low_competition_score +
        concentration_score +
        hhi_score,

      buyer_risk_score =
        round(buyer_risk_score, 2),

      risk_band =
        assign_risk_band(buyer_risk_score)
    ) |>
    dplyr::arrange(
      dplyr::desc(buyer_risk_score)
    )

  scorecard
}

# ----------------------------
# Add buyer profile info
# ----------------------------

enrich_buyer_scorecard <- function(
    buyer_scorecard,
    buyers) {

  buyers_clean <- buyers |>
    dplyr::mutate(
      buyer_nip_clean = clean_nip(nip)
    ) |>
    dplyr::select(
      buyer_nip_clean,
      buyer_name = name,
      buyer_city = city,
      buyer_province = province,
      buyer_country = country
    )

  buyer_scorecard |>
    dplyr::left_join(
      buyers_clean,
      by = "buyer_nip_clean"
    ) |>
    dplyr::relocate(
      buyer_name,
      buyer_city,
      buyer_province,
      buyer_country,
      .after = buyer_nip_clean
    )
}

# ----------------------------
# Main wrapper
# ----------------------------

run_buyer_scoring <- function(
    risk,
    prepared) {

  risk |>
    build_buyer_scorecard() |>
    enrich_buyer_scorecard(
      prepared$buyers
    )
}

# ----------------------------
# Profiling
# ----------------------------

profile_buyer_scorecard <- function(
    buyer_scorecard) {

  buyer_scorecard |>
    dplyr::count(
      risk_band,
      name = "buyers"
    ) |>
    dplyr::arrange(
      dplyr::desc(buyers)
    )
}

# ----------------------------
# Top risky buyers
# ----------------------------

get_top_risky_buyers <- function(
    buyer_scorecard,
    n = 25) {

  buyer_scorecard |>
    dplyr::arrange(
      dplyr::desc(
        buyer_risk_score
      )
    ) |>
    dplyr::slice_head(
      n = n
    )
}