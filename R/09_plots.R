# ============================================================
# 09_plots.R
# Plot and table helpers for Shiny dashboard
# ============================================================

source("R/07_relationship_analysis.R")

library(plotly)
library(DT)

make_dt <- function(df, page_length = 10) {
  DT::datatable(
    df,
    rownames = FALSE,
    filter = "top",
    options = list(
      pageLength = page_length,
      scrollX = TRUE,
      autoWidth = TRUE
    )
  )
}

plot_benford_distribution <- function(benford) {
  df <- benford$summary |>
    dplyr::select(first_digit, actual_pct, expected_pct) |>
    tidyr::pivot_longer(
      cols = c(actual_pct, expected_pct),
      names_to = "type",
      values_to = "pct"
    ) |>
    dplyr::mutate(
      type = dplyr::recode(
        type,
        actual_pct = "Actual",
        expected_pct = "Expected"
      )
    )

  plotly::plot_ly(
    df,
    x = ~first_digit,
    y = ~pct,
    color = ~type,
    type = "bar"
  ) |>
    plotly::layout(
      title = "Benford First-Digit Distribution",
      xaxis = list(title = "First digit"),
      yaxis = list(title = "Share", tickformat = ".1%"),
      barmode = "group"
    )
}

plot_vendor_risk_bands <- function(vendor_scorecard) {
  df <- vendor_scorecard |>
    dplyr::count(risk_band, name = "vendors") |>
    dplyr::mutate(
      risk_band = factor(
        risk_band,
        levels = c("Critical", "High", "Medium", "Low", "Unknown")
      )
    ) |>
    dplyr::arrange(risk_band)

  plotly::plot_ly(
    df,
    x = ~risk_band,
    y = ~vendors,
    type = "bar"
  ) |>
    plotly::layout(
      title = "Vendor Risk Bands",
      xaxis = list(title = "Risk band"),
      yaxis = list(title = "Vendors")
    )
}

plot_buyer_risk_bands <- function(buyer_scorecard) {
  df <- buyer_scorecard |>
    dplyr::count(risk_band, name = "buyers") |>
    dplyr::mutate(
      risk_band = factor(
        risk_band,
        levels = c("Critical", "High", "Medium", "Low", "Unknown")
      )
    ) |>
    dplyr::arrange(risk_band)

  plotly::plot_ly(
    df,
    x = ~risk_band,
    y = ~buyers,
    type = "bar"
  ) |>
    plotly::layout(
      title = "Buyer Risk Bands",
      xaxis = list(title = "Risk band"),
      yaxis = list(title = "Buyers")
    )
}

plot_top_vendors <- function(
    vendor_scorecard,
    n = 15,
    min_contracts = 3,
    include_anonymous = FALSE) {

  df <- vendor_scorecard |>
    dplyr::filter(
      !is.na(contractor_name),
      contractor_contracts >= min_contracts,
      !is.na(vendor_risk_score)
    )

  if (!include_anonymous) {
    df <- df |>
      dplyr::filter(contractor_name != "[Osoba fizyczna]")
  }

  df <- df |>
    dplyr::mutate(
      vendor_risk_percentile = dplyr::percent_rank(vendor_risk_score) * 100
    ) |>
    dplyr::slice_max(vendor_risk_percentile, n = n, with_ties = FALSE) |>
    dplyr::mutate(
      short_id = stringr::str_sub(contractor_id_clean, -5, -1),
      label = paste0(stringr::str_trunc(contractor_name, 32), " [", short_id, "]")
    )

  plotly::plot_ly(
    df,
    x = ~vendor_risk_percentile,
    y = ~stats::reorder(label, vendor_risk_percentile),
    type = "bar",
    orientation = "h",
    text = ~paste0(
      "Vendor: ", contractor_name,
      "<br>ID: ", contractor_id_clean,
      "<br>Raw score: ", round(vendor_risk_score, 2),
      "<br>Percentile score: ", round(vendor_risk_percentile, 1),
      "<br>Band: ", risk_band,
      "<br>Contracts: ", contractor_contracts,
      "<br>Total value: ", round(contractor_total_value, 0)
    ),
    hoverinfo = "text"
  ) |>
    plotly::layout(
      title = paste0("Top Risky Vendors (min ", min_contracts, " contracts)"),
      xaxis = list(title = "Risk percentile score", range = c(0, 100)),
      yaxis = list(title = ""),
      margin = list(l = 220)
    )
}

plot_top_buyers <- function(buyer_scorecard, n = 15, min_contracts = 5) {
  df <- buyer_scorecard |>
    dplyr::filter(
      !is.na(buyer_name),
      buyer_contracts >= min_contracts,
      !is.na(buyer_risk_score)
    ) |>
    dplyr::slice_max(buyer_risk_score, n = n, with_ties = FALSE) |>
    dplyr::mutate(
      short_id = stringr::str_sub(buyer_nip_clean, -5, -1),
      label = paste0(stringr::str_trunc(buyer_name, 32), " [", short_id, "]")
    )

  plotly::plot_ly(
    df,
    x = ~buyer_risk_score,
    y = ~stats::reorder(label, buyer_risk_score),
    type = "bar",
    orientation = "h",
    text = ~paste0(
      "Buyer: ", buyer_name,
      "<br>ID: ", buyer_nip_clean,
      "<br>Score: ", buyer_risk_score,
      "<br>Band: ", risk_band,
      "<br>Contracts: ", buyer_contracts
    ),
    hoverinfo = "text"
  ) |>
    plotly::layout(
      title = paste0("Top Risky Buyers (min ", min_contracts, " contracts)"),
      xaxis = list(title = "Buyer risk score"),
      yaxis = list(title = ""),
      margin = list(l = 220)
    )
}

plot_top_relationships <- function(relationship_analysis, n = 15, min_contracts = 2) {
  df <- relationship_analysis$relationship_scorecard |>
    dplyr::filter(relationship_contracts >= min_contracts) |>
    dplyr::slice_max(relationship_risk_score, n = n, with_ties = FALSE) |>
    dplyr::mutate(
      label = paste0(
        stringr::str_trunc(buyer_nip_clean, 12),
        " → ",
        stringr::str_trunc(contractor_id_clean, 12)
      )
    )

  plotly::plot_ly(
    df,
    x = ~relationship_risk_score,
    y = ~stats::reorder(label, relationship_risk_score),
    type = "bar",
    orientation = "h",
    text = ~paste0(
      "Buyer: ", buyer_name,
      "<br>Vendor: ", contractor_name,
      "<br>Contracts: ", relationship_contracts,
      "<br>Value: ", round(relationship_value, 0),
      "<br>Score: ", relationship_risk_score,
      "<br>Band: ", relationship_risk_band
    ),
    hoverinfo = "text"
  ) |>
    plotly::layout(
      title = paste0("Top Risky Buyer-Vendor Relationships (min ", min_contracts, " contracts)"),
      xaxis = list(title = "Relationship risk score"),
      yaxis = list(title = ""),
      margin = list(l = 180)
    )
}

plot_relationship_value_vs_count <- function(relationship_analysis) {
  df <- relationship_analysis$relationship_scorecard |>
    dplyr::filter(
      !is.na(relationship_value),
      relationship_value > 0,
      relationship_contracts > 0
    ) |>
    dplyr::mutate(
      log_relationship_value = log10(relationship_value + 1),
      log_relationship_contracts = log10(relationship_contracts + 1),
      relationship_risk_band = factor(
        relationship_risk_band,
        levels = c("Critical", "High", "Medium", "Low", "Unknown")
      )
    )

    plotly::plot_ly(
    df,
    x = ~log_relationship_contracts,
    y = ~log_relationship_value,
    color = ~relationship_risk_band,
    colors = c(
      "Critical" = "darkred",
      "High" = "red",
      "Medium" = "gold",
      "Low" = "green",
      "Unknown" = "gray"
    ),
    type = "scatter",
    mode = "markers",
    text = ~paste0(
      "Buyer: ", buyer_name,
      "<br>Vendor: ", contractor_name,
      "<br>Contracts: ", relationship_contracts,
      "<br>Value: ", round(relationship_value, 0),
      "<br>Score: ", relationship_risk_score,
      "<br>Band: ", relationship_risk_band
    ),
    hoverinfo = "text"
  ) |>
    plotly::layout(
      title = "Relationship Value vs Contract Count (Log Scale)",
      xaxis = list(title = "log10(Relationship contracts + 1)"),
      yaxis = list(title = "log10(Relationship value + 1)")
    )
}