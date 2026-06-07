# ============================================================
# 09_plots.R
# Plot and table helpers for Shiny dashboard
# ============================================================

source("R/07_relationship_analysis.R")

library(plotly)
library(DT)

# ----------------------------
# Generic helpers
# ----------------------------

make_dt <- function(df, page_length = 10, order_col = NULL, order_dir = "desc") {
  dt_options <- list(
    pageLength = page_length,
    scrollX = TRUE,
    autoWidth = TRUE,
    dom = "Blfrtip",
    buttons = list(
      list(
        extend = "csv",
        text = "Export CSV",
        filename = "procurement_risk_export",
        exportOptions = list(
          modifier = list(page = "all", search = "applied", order = "applied")
        )
      )
    )
  )

  if (!is.null(order_col)) {
    dt_options$order <- list(list(order_col, order_dir))
  }

  DT::datatable(
    df,
    rownames = FALSE,
    filter = "top",
    extensions = "Buttons",
    options = dt_options
  )
}

plotly_config <- function(p) {
  plotly::config(
    p,
    displaylogo = FALSE,
    responsive = TRUE
  )
}

safe_min_max <- function(x) {
  x <- as.numeric(x)
  x <- x[is.finite(x)]
  if (!length(x)) return(c(0, 1))
  rng <- range(x, na.rm = TRUE)
  if (rng[1] == rng[2]) rng <- c(rng[1] - 1, rng[2] + 1)
  rng
}

# ----------------------------
# Benford plot
# ----------------------------

plot_benford_distribution <- function(
    benford,
    actual_color = "#0072B2",
    expected_color = "#009E73") {

  df_actual <- benford$summary |>
    dplyr::select(first_digit, actual_pct)

  df_expected <- benford$summary |>
    dplyr::select(first_digit, expected_pct)

  plotly::plot_ly() |>
    plotly::add_bars(
      data = df_actual,
      x = ~first_digit,
      y = ~actual_pct,
      name = "Actual",
      marker = list(color = actual_color),
      hovertemplate = "Digit: %{x}<br>Actual: %{y:.1%}<extra></extra>"
    ) |>
    plotly::add_bars(
      data = df_expected,
      x = ~first_digit,
      y = ~expected_pct,
      name = "Expected",
      marker = list(color = expected_color),
      hovertemplate = "Digit: %{x}<br>Expected: %{y:.1%}<extra></extra>"
    ) |>
    plotly::layout(
      title = "Benford First-Digit Distribution",
      xaxis = list(title = "First digit"),
      yaxis = list(title = "Share", tickformat = ".1%"),
      barmode = "group"
    ) |>
    plotly_config()
}

# ----------------------------
# Risk band plots
# ----------------------------

plot_vendor_risk_bands <- function(vendor_scorecard, bar_color = "#0072B2") {
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
    type = "bar",
    marker = list(color = bar_color),
    hovertemplate = "Risk band: %{x}<br>Vendors: %{y:,}<extra></extra>"
  ) |>
    plotly::layout(
      title = "Vendor Risk Bands",
      xaxis = list(title = "Risk band"),
      yaxis = list(title = "Vendors")
    ) |>
    plotly_config()
}

plot_buyer_risk_bands <- function(buyer_scorecard, bar_color = "#0072B2") {
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
    type = "bar",
    marker = list(color = bar_color),
    hovertemplate = "Risk band: %{x}<br>Buyers: %{y:,}<extra></extra>"
  ) |>
    plotly::layout(
      title = "Buyer Risk Bands",
      xaxis = list(title = "Risk band"),
      yaxis = list(title = "Buyers")
    ) |>
    plotly_config()
}

# ----------------------------
# Top score plots
# ----------------------------

plot_top_vendors <- function(
    vendor_scorecard,
    n = 15,
    min_contracts = 3,
    include_anonymous = TRUE,
    bar_color = "#0072B2") {

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
    marker = list(color = bar_color),
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
    ) |>
    plotly_config()
}

plot_top_buyers <- function(
    buyer_scorecard,
    n = 15,
    min_contracts = 5,
    include_anonymous = TRUE,
    bar_color = "#0072B2") {

  df <- buyer_scorecard |>
    dplyr::filter(
      !is.na(buyer_name),
      buyer_contracts >= min_contracts,
      !is.na(buyer_risk_score)
    )

  if (!include_anonymous) {
    df <- df |>
      dplyr::filter(buyer_name != "[Osoba fizyczna]")
  }

  df <- df |>
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
    marker = list(color = bar_color),
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
    ) |>
    plotly_config()
}

plot_top_relationships <- function(
    relationship_analysis,
    n = 15,
    min_contracts = 2,
    bar_color = "#0072B2") {

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
    marker = list(color = bar_color),
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
    ) |>
    plotly_config()
}

# ----------------------------
# Relationship scatter, sampled for performance
# ----------------------------

sample_relationship_scatter_data <- function(
    relationship_analysis,
    max_points = 6000,
    low_sample_size = 4500,
    seed = 42) {

  df_all <- relationship_analysis$relationship_scorecard |>
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

  value_cut <- stats::quantile(df_all$relationship_value, 0.995, na.rm = TRUE)
  count_cut <- stats::quantile(df_all$relationship_contracts, 0.995, na.rm = TRUE)

  high_signal <- df_all |>
    dplyr::filter(
      as.character(relationship_risk_band) %in% c("Critical", "High", "Medium") |
        relationship_contracts >= count_cut |
        relationship_value >= value_cut
    )

  low_signal <- df_all |>
    dplyr::anti_join(
      high_signal |> dplyr::select(buyer_nip_clean, contractor_id_clean),
      by = c("buyer_nip_clean", "contractor_id_clean")
    )

  set.seed(seed)
  low_signal_sample <- low_signal |>
    dplyr::slice_sample(n = min(low_sample_size, nrow(low_signal)))

  high_keep <- high_signal |>
    dplyr::slice_max(relationship_risk_score, n = min(nrow(high_signal), max_points), with_ties = FALSE)

  remaining <- max(0, max_points - nrow(high_keep))
  low_keep <- low_signal_sample |>
    dplyr::slice_sample(n = min(remaining, nrow(low_signal_sample)))

  dplyr::bind_rows(high_keep, low_keep) |>
    dplyr::distinct(buyer_nip_clean, contractor_id_clean, .keep_all = TRUE) |>
    dplyr::mutate(
      relationship_risk_band = factor(
        as.character(relationship_risk_band),
        levels = intersect(
          c("Critical", "High", "Medium", "Low", "Unknown"),
          unique(as.character(relationship_risk_band))
        )
      )
    )
}

plot_relationship_value_vs_count <- function(
    relationship_analysis,
    max_points = 6000,
    low_sample_size = 4500,
    seed = 42) {

  df <- sample_relationship_scatter_data(
    relationship_analysis,
    max_points = max_points,
    low_sample_size = low_sample_size,
    seed = seed
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
    marker = list(size = 6, opacity = 0.68),
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
      title = "Relationship Value vs Contract Count (Log Scale, sampled)",
      xaxis = list(title = "log10(Relationship contracts + 1)"),
      yaxis = list(title = "log10(Relationship value + 1)"),
      annotations = list(
        list(
          x = 0,
          y = -0.24,
          xref = "paper",
          yref = "paper",
          text = "Performance note: all high-signal relationships are retained; lower-risk background points are sampled for responsiveness.",
          showarrow = FALSE,
          font = list(size = 11, color = "#4b5563")
        )
      ),
      margin = list(b = 125)
    ) |>
    plotly_config()
}

# ----------------------------
# Concentration plots
# ----------------------------

plot_top_dominant_relationships <- function(
    concentration_analysis,
    n = 15,
    min_contracts = 2,
    bar_color = "#0072B2") {

  df <- concentration_analysis$dominant_relationships |>
    dplyr::filter(
      relationship_contracts >= min_contracts,
      !is.na(mutual_dependency_score)
    ) |>
    dplyr::slice_max(mutual_dependency_score, n = n, with_ties = FALSE) |>
    dplyr::mutate(
      label = paste0(
        stringr::str_trunc(buyer_nip_clean, 12),
        " → ",
        stringr::str_trunc(contractor_id_clean, 12)
      )
    )

  plotly::plot_ly(
    df,
    x = ~mutual_dependency_score,
    y = ~stats::reorder(label, mutual_dependency_score),
    type = "bar",
    orientation = "h",
    marker = list(color = bar_color),
    text = ~paste0(
      "Buyer: ", buyer_nip_clean,
      "<br>Vendor: ", contractor_id_clean,
      "<br>Contracts: ", relationship_contracts,
      "<br>Value: ", round(relationship_value, 0),
      "<br>Buyer value share: ", round(buyer_value_share, 3),
      "<br>Vendor value share: ", round(vendor_value_share, 3),
      "<br>Score: ", mutual_dependency_score,
      "<br>Band: ", mutual_dependency_band
    ),
    hoverinfo = "text"
  ) |>
    plotly::layout(
      title = paste0("Top Dominant Buyer-Vendor Relationships (min ", min_contracts, " contracts)"),
      xaxis = list(title = "Mutual dependency score"),
      yaxis = list(title = ""),
      margin = list(l = 180)
    ) |>
    plotly_config()
}

# ----------------------------
# Voivodeship helpers
# ----------------------------

voivodeship_metadata <- function() {
  tibble::tibble(
    province = c(
      "PL02", "PL04", "PL06", "PL08",
      "PL10", "PL12", "PL14", "PL16",
      "PL18", "PL20", "PL22", "PL24",
      "PL26", "PL28", "PL30", "PL32"
    ),
    voivodeship_name = c(
      "Dolnośląskie", "Kujawsko-Pomorskie", "Lubelskie", "Lubuskie",
      "Łódzkie", "Małopolskie", "Mazowieckie", "Opolskie",
      "Podkarpackie", "Podlaskie", "Pomorskie", "Śląskie",
      "Świętokrzyskie", "Warmińsko-Mazurskie", "Wielkopolskie", "Zachodniopomorskie"
    ),
    name_key = c(
      "dolnoslaskie", "kujawsko-pomorskie", "lubelskie", "lubuskie",
      "lodzkie", "malopolskie", "mazowieckie", "opolskie",
      "podkarpackie", "podlaskie", "pomorskie", "slaskie",
      "swietokrzyskie", "warminsko-mazurskie", "wielkopolskie", "zachodniopomorskie"
    ),
    tile_x = c(3, 4, 7, 2, 5, 5, 6, 3, 7, 7, 4, 4, 6, 6, 3, 2),
    tile_y = c(2, 4, 2, 3, 3, 1, 3, 1, 1, 4, 5, 1, 2, 5, 3, 5),
    lon = c(16.9, 18.4, 22.9, 15.3, 19.5, 20.1, 21.0, 17.9, 22.1, 23.2, 18.2, 19.0, 20.7, 20.8, 17.0, 15.6),
    lat = c(51.1, 53.0, 51.3, 52.2, 51.7, 49.9, 52.2, 50.7, 49.9, 53.2, 54.2, 50.3, 50.8, 53.9, 52.4, 53.4)
  )
}

normalize_name <- function(x) {
  x <- tolower(x)
  x <- iconv(x, from = "UTF-8", to = "ASCII//TRANSLIT")
  x <- gsub("[^a-z0-9-]", "", x)
  x
}

metric_label_for <- function(metric) {
  dplyr::case_when(
    metric == "risk_density_score" ~ "Risk density score",
    metric == "single_offer_rate" ~ "Single-offer rate (%)",
    metric == "high_value_rate" ~ "High-value rate (%)",
    metric == "round_number_rate" ~ "Round-number rate (%)",
    TRUE ~ "Low-competition rate (%)"
  )
}

add_metric_value <- function(df, metric) {
  df |>
    dplyr::mutate(
      metric_value = dplyr::case_when(
        metric == "risk_density_score" ~ risk_density_score,
        metric == "single_offer_rate" ~ 100 * single_offer_rate,
        metric == "high_value_rate" ~ 100 * high_value_rate,
        metric == "round_number_rate" ~ 100 * round_number_rate,
        TRUE ~ 100 * low_competition_rate
      ),
      metric_label = metric_label_for(metric)
    )
}

province_risk_summary <- function(risk) {
  voivodeships <- voivodeship_metadata()

  risk$result_notices |>
    dplyr::filter(!is.na(province), province != "") |>
    dplyr::group_by(province) |>
    dplyr::summarise(
      result_notices = dplyr::n(),
      total_value = sum(estimated_value_num, na.rm = TRUE),
      low_competition_rate = safe_divide(sum(risk_low_competition, na.rm = TRUE), dplyr::n()),
      single_offer_rate = safe_divide(sum(risk_single_offer, na.rm = TRUE), dplyr::n()),
      high_value_rate = safe_divide(sum(risk_high_value, na.rm = TRUE), dplyr::n()),
      round_number_rate = safe_divide(sum(risk_round_number, na.rm = TRUE), dplyr::n()),
      risk_density_score = 100 * (
        0.40 * dplyr::coalesce(low_competition_rate, 0) +
          0.25 * dplyr::coalesce(single_offer_rate, 0) +
          0.20 * dplyr::coalesce(high_value_rate, 0) +
          0.15 * dplyr::coalesce(round_number_rate, 0)
      ),
      .groups = "drop"
    ) |>
    dplyr::right_join(voivodeships, by = "province") |>
    dplyr::mutate(
      label = paste0(province, " / ", voivodeship_name)
    ) |>
    dplyr::arrange(dplyr::desc(risk_density_score))
}

plot_province_risk_tile_map <- function(
    risk,
    metric = "low_competition_rate",
    fill_color = "#0072B2") {

  df <- province_risk_summary(risk) |>
    add_metric_value(metric)

  plotly::plot_ly(
    df,
    x = ~tile_x,
    y = ~tile_y,
    type = "scatter",
    mode = "markers+text",
    text = ~paste0(gsub("PL", "", province), " / ", voivodeship_name),
    textposition = "middle center",
    marker = list(
      symbol = "square",
      size = 88,
      color = ~metric_value,
      colorscale = list(list(0, "#F8FAFC"), list(1, fill_color)),
      line = list(color = "#FFFFFF", width = 2),
      colorbar = list(title = "Metric", x = 1.05)
    ),
    hovertext = ~paste0(
      "<b>", voivodeship_name, " (", province, ")</b>",
      "<br>Selected metric: ", metric_label,
      "<br>Metric value: ", round(metric_value, 2),
      "<br>Result notices: ", result_notices,
      "<br>Total value: ", round(total_value, 0),
      "<br>Low-competition rate: ", round(100 * low_competition_rate, 1), "%",
      "<br>Single-offer rate: ", round(100 * single_offer_rate, 1), "%",
      "<br>High-value rate: ", round(100 * high_value_rate, 2), "%",
      "<br>Round-number rate: ", round(100 * round_number_rate, 1), "%",
      "<br>Risk density score: ", round(risk_density_score, 1)
    ),
    hoverinfo = "text"
  ) |>
    plotly::layout(
      title = "Voivodeship Risk Density Tile Map",
      xaxis = list(title = "", showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
      yaxis = list(title = "", showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE, scaleanchor = "x"),
      margin = list(l = 20, r = 70, t = 60, b = 45),
      annotations = list(
        list(
          x = 0.5,
          y = -0.10,
          xref = "paper",
          yref = "paper",
          text = "Tile map approximation by Polish voivodeship. Darker tiles indicate higher selected risk metric.",
          showarrow = FALSE,
          font = list(size = 11, color = "#4b5563")
        )
      )
    ) |>
    plotly_config()
}

# ----------------------------
# Real voivodeship map from GeoJSON
# ----------------------------

load_poland_voivodeship_geojson <- function() {
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    stop("Package 'jsonlite' is required for the geographic map. Install with install.packages('jsonlite').")
  }

  cache_dir <- file.path("outputs", "map_cache")
  if (!dir.exists(cache_dir)) dir.create(cache_dir, recursive = TRUE)
  cache_file <- file.path(cache_dir, "polska-wojewodztwa.geojson")

  if (!file.exists(cache_file)) {
    url <- "https://raw.githubusercontent.com/andilabs/polska-wojewodztwa-geojson/master/polska-wojewodztwa.geojson"
    utils::download.file(url, cache_file, mode = "wb", quiet = TRUE)
  }

  jsonlite::fromJSON(cache_file, simplifyVector = FALSE)
}

extract_feature_name <- function(feature) {
  props <- feature$properties
  candidates <- c("name", "NAME", "nazwa", "JPT_NAZWA_", "wojewodztwo")
  for (nm in candidates) {
    if (!is.null(props[[nm]]) && nzchar(as.character(props[[nm]]))) {
      return(as.character(props[[nm]]))
    }
  }
  NA_character_
}

coords_to_rings <- function(geometry) {
  if (is.null(geometry) || is.null(geometry$coordinates)) return(list())

  if (identical(geometry$type, "Polygon")) {
    return(list(geometry$coordinates[[1]]))
  }

  if (identical(geometry$type, "MultiPolygon")) {
    return(lapply(geometry$coordinates, function(poly) poly[[1]]))
  }

  list()
}

ring_to_lonlat <- function(ring) {
  mat <- do.call(rbind, lapply(ring, function(pt) c(as.numeric(pt[[1]]), as.numeric(pt[[2]]))))
  colnames(mat) <- c("lon", "lat")
  mat
}

metric_color <- function(value, rng, fill_color) {
  if (!is.finite(value)) return("#F8FAFC")
  t <- (value - rng[1]) / (rng[2] - rng[1])
  t <- max(0, min(1, t))
  low_rgb <- grDevices::col2rgb("#F8FAFC")
  high_rgb <- grDevices::col2rgb(fill_color)
  out <- round(low_rgb + (high_rgb - low_rgb) * t)
  grDevices::rgb(out[1], out[2], out[3], maxColorValue = 255)
}

plot_province_risk_geo_map <- function(
    risk,
    metric = "low_competition_rate",
    fill_color = "#0072B2") {

  df <- province_risk_summary(risk) |>
    add_metric_value(metric)

  geo <- tryCatch(load_poland_voivodeship_geojson(), error = function(e) e)

  if (inherits(geo, "error")) {
    return(
      plotly::plot_ly() |>
        plotly::layout(
          title = "Voivodeship Risk Density Geographic Map",
          annotations = list(list(
            x = 0.5, y = 0.5, xref = "paper", yref = "paper",
            text = paste("Map could not be loaded:", conditionMessage(geo)),
            showarrow = FALSE
          ))
        ) |>
        plotly_config()
    )
  }

  features <- geo$features
  meta <- voivodeship_metadata()
  rng <- safe_min_max(df$metric_value)

  p <- plotly::plot_ly()
  label_df <- tibble::tibble()

  for (feature in features) {
    f_name <- extract_feature_name(feature)
    f_key <- normalize_name(f_name)
    meta_row <- meta |> dplyr::filter(name_key == f_key)

    if (nrow(meta_row) != 1) next

    row <- df |> dplyr::filter(province == meta_row$province[1])
    if (nrow(row) != 1) next

    fill <- metric_color(row$metric_value[1], rng, fill_color)
    rings <- coords_to_rings(feature$geometry)

    hover <- paste0(
      "<b>", row$voivodeship_name, " (", row$province, ")</b>",
      "<br>Selected metric: ", row$metric_label,
      "<br>Metric value: ", round(row$metric_value, 2),
      "<br>Result notices: ", row$result_notices,
      "<br>Total value: ", round(row$total_value, 0),
      "<br>Low-competition rate: ", round(100 * row$low_competition_rate, 1), "%",
      "<br>Single-offer rate: ", round(100 * row$single_offer_rate, 1), "%",
      "<br>High-value rate: ", round(100 * row$high_value_rate, 2), "%",
      "<br>Round-number rate: ", round(100 * row$round_number_rate, 1), "%",
      "<br>Risk density score: ", round(row$risk_density_score, 1)
    )

    for (ring in rings) {
      ll <- ring_to_lonlat(ring)
      p <- p |>
        plotly::add_trace(
          type = "scattergeo",
          mode = "lines",
          lon = ll[, "lon"],
          lat = ll[, "lat"],
          fill = "toself",
          fillcolor = fill,
          line = list(color = "#334155", width = 0.8),
          hoverinfo = "text",
          text = hover,
          showlegend = FALSE
        )
    }

    label_df <- dplyr::bind_rows(
      label_df,
      tibble::tibble(
        lon = meta_row$lon[1],
        lat = meta_row$lat[1],
        label = paste0(gsub("PL", "", row$province), " / ", row$voivodeship_name)
      )
    )
  }

  p |>
    plotly::add_trace(
      type = "scattergeo",
      mode = "markers",
      lon = c(13.8, 13.81),
      lat = c(48.8, 48.81),
      marker = list(
        size = 0.01,
        opacity = 0,
        color = rng,
        colorscale = list(list(0, "#F8FAFC"), list(1, fill_color)),
        cmin = rng[1],
        cmax = rng[2],
        showscale = TRUE,
        colorbar = list(title = "Metric", x = 1.02)
      ),
      hoverinfo = "skip",
      showlegend = FALSE
    ) |>
    plotly::add_trace(
      data = label_df,
      type = "scattergeo",
      mode = "text",
      lon = ~lon,
      lat = ~lat,
      text = ~label,
      textfont = list(size = 9, color = "#111827"),
      hoverinfo = "skip",
      showlegend = FALSE
    ) |>
    plotly::layout(
      title = "Voivodeship Risk Density Geographic Map",
      showlegend = FALSE,
      geo = list(
        scope = "europe",
        projection = list(type = "mercator"),
        center = list(lon = 19.2, lat = 52.1),
        lonaxis = list(range = c(13.8, 24.8)),
        lataxis = list(range = c(48.8, 55.1)),
        showland = FALSE,
        showcountries = FALSE,
        showsubunits = FALSE,
        showlakes = FALSE,
        bgcolor = "rgba(0,0,0,0)"
      ),
      margin = list(l = 20, r = 90, t = 60, b = 50),
      annotations = list(
        list(
          x = 0.5,
          y = -0.10,
          xref = "paper",
          yref = "paper",
          text = "Filled administrative voivodeship polygons; color reflects the selected risk metric.",
          showarrow = FALSE,
          font = list(size = 11, color = "#4b5563")
        )
      )
    ) |>
    plotly_config()
}
