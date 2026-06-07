# ============================================================
# app.R
# Procurement Risk Analytics Shiny Dashboard
# ============================================================

library(shiny)
library(shinydashboard)
library(plotly)
library(DT)
library(dplyr)
library(scales)
library(htmltools)
library(grid)

source("R/08_concentration_analysis.R")
source("R/09_plots.R")

# ----------------------------
# Load processed objects
# ----------------------------

benford <- readRDS("outputs/benford.rds")
risk <- readRDS("outputs/risk.rds")
vendor_scorecard <- readRDS("outputs/vendor_scorecard.rds")
buyer_scorecard <- readRDS("outputs/buyer_scorecard.rds")
relationship_analysis <- readRDS("outputs/relationship_analysis.rds")
concentration_analysis <- readRDS("outputs/concentration_analysis.rds")

# ----------------------------
# Helpers
# ----------------------------

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0 || all(is.na(x))) y else x
}

format_num <- function(x) scales::comma(x, accuracy = 1)

format_pln <- function(x) {
  x <- as.numeric(x)
  dplyr::case_when(
    is.na(x) ~ "",
    abs(x) >= 1e9 ~ paste0(scales::number(x / 1e9, accuracy = 0.1), "B PLN"),
    abs(x) >= 1e6 ~ paste0(scales::number(x / 1e6, accuracy = 0.1), "M PLN"),
    abs(x) >= 1e3 ~ paste0(scales::number(x / 1e3, accuracy = 0.1), "K PLN"),
    TRUE ~ paste0(scales::comma(x, accuracy = 1), " PLN")
  )
}

value_box <- function(value, subtitle, icon_name, color = "blue") {
  shinydashboard::valueBox(value = value, subtitle = subtitle, icon = icon(icon_name), color = color, width = 3)
}

info_banner <- function() {
  div(
    class = "source-banner",
    strong("Procurement Risk Analytics Dashboard"), br(),
    "Data source: ",
    tags$a(href = "https://atlasprzetargow.pl/", target = "_blank", "Atlas Przetargów main page"),
    " | ",
    tags$a(href = "https://github.com/atlasprzetargow", target = "_blank", "Atlas Przetargów GitHub"),
    br(),
    "Citation: Atlas Przetargów. (2026). ",
    tags$em("Polish Public Tenders Dataset (BZP + TED)"),
    " (Version 2026.Q2) [Data set]. ",
    tags$a(href = "https://doi.org/10.5281/zenodo.19634050", target = "_blank", "https://doi.org/10.5281/zenodo.19634050")
  )
}

how_to_box <- function(title, ..., width = 12) {
  box(width = width, title = title, status = "primary", solidHeader = TRUE, class = "compact-explain", div(class = "explain-box", ...))
}

# Violation Tracker-inspired palettes.
dashboard_palettes <- list(
  "Tableau Original" = list(primary = "#4E79A7", secondary = "#59A14F", accent = "#F28E2B", bg = "#FFFFFF"),
  "Modern Bright" = list(primary = "#2E6FBB", secondary = "#43A047", accent = "#FB8C00", bg = "#F7F9FB"),
  "Muted Audit" = list(primary = "#466A92", secondary = "#4F8A5B", accent = "#D5A021", bg = "#FFFFFF"),
  "Default" = list(primary = "#337AB7", secondary = "#5DA5DA", accent = "#2C3E50", bg = "#F7F9FB"),
  "SAP Blue" = list(primary = "#0A6ED1", secondary = "#2E90FA", accent = "#0854A0", bg = "#F7FBFF"),
  "Finance Green" = list(primary = "#2E7D32", secondary = "#43A047", accent = "#1B5E20", bg = "#F6FBF8"),
  "Executive Dark" = list(primary = "#6366F1", secondary = "#2563EB", accent = "#06B6D4", bg = "#F8FAFC"),
  "Warm Amber" = list(primary = "#D97706", secondary = "#F59E0B", accent = "#7C2D12", bg = "#FFFAF0"),
  "Ocean Teal" = list(primary = "#006D77", secondary = "#83C5BE", accent = "#E9C46A", bg = "#F6FCFC"),
  "Slate Professional" = list(primary = "#475569", secondary = "#64748B", accent = "#0F766E", bg = "#F8FAFC"),
  "Audit Pastel" = list(primary = "#78A083", secondary = "#A8DADC", accent = "#457B9D", bg = "#FFFFFF"),
  "High Contrast" = list(primary = "#0072B2", secondary = "#009E73", accent = "#E69F00", bg = "#FFFFFF")
)

get_palette <- function(name) dashboard_palettes[[name]] %||% dashboard_palettes[["Muted Audit"]]

palette_css <- function(palette_name = "Muted Audit") {
  p <- get_palette(palette_name)
  tags$style(HTML(sprintf("
    .content-wrapper, .right-side { background-color: %s; overflow-x: hidden; }
    .main-header .logo, .main-header .navbar, .skin-blue .main-header .logo,
    .skin-blue .main-header .navbar, .skin-blue .main-header .logo:hover { background-color: %s !important; }
    .main-header .logo { font-weight: 800; white-space: nowrap !important; width: 620px !important; text-align: left !important; padding-left: 56px !important; font-size: 18px !important; }
    .main-header .navbar { margin-left: 620px !important; }
    .main-header .navbar .sidebar-toggle,
    .main-header .sidebar-toggle {
      position: fixed !important;
      left: 8px !important;
      top: 8px !important;
      width: 42px !important;
      height: 42px !important;
      z-index: 20000 !important;
      background: transparent !important;
      text-align: center !important;
    }
    .main-header .navbar .sidebar-toggle:hover,
    .main-header .sidebar-toggle:hover { background: rgba(0,0,0,0.12) !important; }
    .skin-blue .main-sidebar, .skin-blue .left-side, .main-sidebar { background-color: %s !important; }
    .skin-blue .sidebar-menu > li > a { color: #ffffff !important; border-left: 3px solid transparent; }
    .skin-blue .sidebar-menu > li:hover > a, .skin-blue .sidebar-menu > li.active > a { background-color: rgba(0,0,0,0.18) !important; color: #ffffff !important; }
    .box { border-radius: 10px; border-top: 3px solid %s; box-shadow: 0 1px 4px rgba(0,0,0,0.08); }
    .box.box-primary { border-top-color: %s; }
    .box.box-solid.box-primary > .box-header { background-color: %s; border-color: %s; }
    .sidebar-menu > li.active > a { border-left-color: %s !important; }
    .source-banner, .explain-box { background: #ffffff; border-left: 5px solid %s; border-radius: 8px; padding: 12px 16px; margin-bottom: 15px; box-shadow: 0 1px 4px rgba(0,0,0,0.06); font-size: 14px; line-height: 1.45; word-break: break-word; overflow-wrap: anywhere; }
    .source-banner strong, .explain-box strong { color: %s; }
    .compact-explain .box-body { padding: 10px 12px !important; }
    .sidebar-export-block { padding: 12px 15px; border-top: 1px solid rgba(255,255,255,0.12); }
    .sidebar-export-block .shiny-input-container, .sidebar-export-block .form-group,
    .sidebar-export-block .selectize-control, .sidebar-export-block .selectize-input,
    .sidebar-export-block .btn, .sidebar-export-block .btn-default { width: 100%% !important; max-width: 100%% !important; box-sizing: border-box; }
    .sidebar-export-block .btn, .sidebar-export-block .btn-default { margin-bottom: 10px; text-align: left; border-radius: 7px; font-weight: 700; color: #1f2933 !important; background: #ffffff !important; border: 1px solid rgba(255,255,255,0.55) !important; opacity: 1 !important; }
    .observations-grid { display: grid; grid-template-columns: repeat(2, minmax(0, 1fr)); gap: 14px; }
    .observation-card { background: #ffffff; border-left: 5px solid %s; border-radius: 10px; padding: 14px 16px; box-shadow: 0 1px 5px rgba(0,0,0,0.08); min-height: 140px; }
    .observation-theme { color: %s; font-weight: 800; font-size: 13px; text-transform: uppercase; letter-spacing: 0.03em; margin-bottom: 8px; }
    .observation-title { font-size: 15px; font-weight: 650; line-height: 1.35; margin-bottom: 10px; }
    .observation-why { font-size: 13px; color: #4b5563; line-height: 1.35; }
    .dataTables_wrapper { font-size: 13px; }
    @media (max-width: 1100px) { .observations-grid { grid-template-columns: 1fr; } }
  ", p$bg, p$primary, p$primary, p$primary, p$primary, p$primary, p$primary, p$accent, p$accent, p$primary, p$accent, p$primary)))
}

# ----------------------------
# Observations
# ----------------------------

observations_df <- function() {
  tibble::tibble(
    theme = c(
      "Review universe", "Review universe", "Benford screening", "Competition risk",
      "Vendor scoring", "Buyer scoring", "Relationship risk", "Dependency and concentration",
      "Voivodeship-level monitoring", "Governance"
    ),
    observation = c(
      "The raw dataset contains more than 1.4 million procurement notices, while the dashboard focuses on result/award notices suitable for risk analytics.",
      "Result notices produced a review universe of 367,287 result notices and 188,788 buyer-vendor relationships.",
      "Overall Benford MAD was approximately 0.0075, which is an acceptable-conformity screening result rather than a severe portfolio-level anomaly.",
      "Low-competition and single-offer indicators are common enough to be meaningful portfolio-level monitoring metrics.",
      "Vendor scoring creates a small high-priority review population instead of asking auditors to inspect tens of thousands of vendors manually.",
      "Buyer scoring is useful, but one-contract buyers can look artificially risky. Minimum-contract filters make the score more stable.",
      "Relationship analytics revealed repeated buyer-vendor relationships, including high-count and high-value pairs that deserve review sampling.",
      "Concentration analysis found dependency patterns where buyers or vendors appear strongly linked to a small number of counterparties.",
      "Voivodeship-level monitoring can highlight regional variation in low-competition rate, single-offer rate and risk density.",
      "All results are review signals. They do not determine wrongdoing and should be combined with professional judgment and case-level investigation."
    ),
    why_it_matters = c(
      "It explains why the dashboard count is lower than the raw input row count.",
      "It defines the exact analytical population used by the dashboard.",
      "It prevents over-interpreting Benford p-values on very large datasets.",
      "They indicate possible competition-risk pockets, not automatically misconduct.",
      "Risk-based review works best when it narrows a large population into a small explainable review set.",
      "This improves model governance and avoids over-reliance on very small samples.",
      "Procurement risk is often relationship-based rather than entity-based.",
      "Dependency can be legitimate, but it is highly relevant for procurement review.",
      "Regional segmentation creates a useful management and audit-planning view.",
      "This keeps the dashboard analytically useful and legally cautious."
    )
  )
}

observation_cards <- function() {
  df <- observations_df()
  tagList(lapply(seq_len(nrow(df)), function(i) {
    div(class = "observation-card", div(class = "observation-theme", df$theme[i]), div(class = "observation-title", df$observation[i]), div(class = "observation-why", strong("Why it matters: "), df$why_it_matters[i]))
  }))
}

# ----------------------------
# Static PDF export
# ----------------------------

plot_static_relationship_scatter <- function() {
  df <- sample_relationship_scatter_data(relationship_analysis, max_points = 5000, low_sample_size = 3500)
  band <- as.character(df$relationship_risk_band)
  col <- ifelse(band == "Critical", "darkred",
                ifelse(band == "High", "red",
                       ifelse(band == "Medium", "goldenrod", "forestgreen")))

  op <- par(mar = c(5.2, 5.2, 4, 1.5))
  on.exit(par(op), add = TRUE)
  plot(
    df$log_relationship_contracts,
    df$log_relationship_value,
    pch = 16,
    cex = 0.32,
    col = col,
    xlab = "log10(Relationship contracts + 1)",
    ylab = "log10(Relationship value + 1)",
    main = "Relationship Value vs Contract Count"
  )

  present <- intersect(c("Critical", "High", "Medium", "Low", "Unknown"), unique(band))
  legend_labels <- ifelse(present %in% c("Low", "Unknown"), "Low/Other", present)
  legend_cols <- c(Critical = "darkred", High = "red", Medium = "goldenrod", Low = "forestgreen", Unknown = "forestgreen")[present]
  keep <- !duplicated(legend_labels)
  legend("topright", legend = legend_labels[keep], col = legend_cols[keep], pch = 16, bty = "n", cex = 0.8)
}

plot_static_voivodeship_bar <- function(pal) {
  df <- province_risk_summary(risk) |> arrange(desc(risk_density_score))
  labels <- paste0(gsub("PL", "", df$province), " / ", df$voivodeship_name)
  short_labels <- stringr::str_trunc(labels, 22)
  op <- par(mar = c(6, 5, 4, 1))
  on.exit(par(op), add = TRUE)
  bp <- barplot(
    df$risk_density_score,
    names.arg = rep("", nrow(df)),
    col = pal$primary,
    border = NA,
    main = "Voivodeship Risk Density Scores",
    ylab = "Risk density score"
  )
  text(bp, df$risk_density_score / 2, labels = short_labels, srt = 90, cex = 0.62, col = "white")
  axis(1, at = bp, labels = gsub("PL", "", df$province), las = 2, cex.axis = 0.65)
}

plot_static_voivodeship_geo <- function(pal, metric = "risk_density_score") {
  geo <- tryCatch(load_poland_voivodeship_geojson(), error = function(e) NULL)
  if (is.null(geo)) {
    plot.new()
    title("Voivodeship Geographic Map")
    text(0.5, 0.5, "GeoJSON map could not be loaded. See dashboard map tab.")
    return(invisible(NULL))
  }

  df <- province_risk_summary(risk) |> add_metric_value(metric)
  meta <- voivodeship_metadata()
  rng <- safe_min_max(df$metric_value)

  op <- par(mar = c(2, 2, 4, 5))
  on.exit(par(op), add = TRUE)
  plot.new()
  plot.window(xlim = c(14, 24.5), ylim = c(49, 55), asp = 1)
  title(paste0("Voivodeship Risk Density Geographic Map — ", metric_label_for(metric)))

  mtext(paste0("Metric: ", metric_label_for(metric)), side = 3, line = 0.2, cex = 0.8, col = pal$primary)
  for (feature in geo$features) {
    f_key <- normalize_name(extract_feature_name(feature))
    meta_row <- meta |> dplyr::filter(name_key == f_key)
    if (nrow(meta_row) != 1) next
    row <- df |> dplyr::filter(province == meta_row$province[1])
    if (nrow(row) != 1) next
    fill <- metric_color(row$metric_value[1], rng, pal$primary)
    rings <- coords_to_rings(feature$geometry)
    for (ring in rings) {
      ll <- ring_to_lonlat(ring)
      polygon(ll[, "lon"], ll[, "lat"], col = fill, border = "#334155", lwd = 0.7)
    }
    text(meta_row$lon[1], meta_row$lat[1], labels = paste0(gsub("PL", "", row$province), " / ", row$voivodeship_name), cex = 0.48)
  }
  box()
}

plot_static_bands <- function(df, value_col, title, ylab, pal) {
  tab <- df |> count(risk_band, name = "n") |> mutate(risk_band = factor(risk_band, levels = c("Critical", "High", "Medium", "Low", "Unknown"))) |> arrange(risk_band)
  barplot(tab$n, names.arg = tab$risk_band, col = pal$primary, border = NA, main = title, ylab = ylab)
}

plot_static_top_bar <- function(df, label_col, score_col, title, pal, n = 15) {
  d <- df |> slice_max({{ score_col }}, n = n, with_ties = FALSE) |> arrange({{ score_col }})
  labels <- stringr::str_trunc(as.character(d[[label_col]]), 44)
  score_name <- rlang::as_name(rlang::enquo(score_col))
  vals <- as.numeric(d[[score_name]])
  vals[!is.finite(vals)] <- 0
  max_val <- max(vals, na.rm = TRUE)
  if (!is.finite(max_val) || max_val <= 0) max_val <- 1

  op <- par(mar = c(4.5, 1.2, 3.8, 1.0), xpd = NA)
  on.exit(par(op), add = TRUE)

  bp <- barplot(
    vals,
    names.arg = rep("", length(vals)),
    horiz = TRUE,
    las = 1,
    col = pal$primary,
    border = NA,
    main = title,
    xlim = c(0, max_val * 1.10),
    axes = TRUE
  )

  text(
    x = pmax(max_val * 0.025, 0.6),
    y = bp,
    labels = labels,
    adj = c(0, 0.5),
    cex = 0.58,
    col = "white",
    font = 2
  )
}

write_dashboard_pdf <- function(file, pages, palette_name = "Muted Audit") {
  pal <- get_palette(palette_name)

  draw_pdf_header <- function(title) {
    grid::grid.text(title, x = 0.5, y = 0.955, gp = grid::gpar(fontsize = 20, fontface = "bold", col = pal$primary))
  }

  draw_profile_panel <- function(title, df, x, y, w, h) {
    grid::grid.roundrect(
      x = x, y = y, width = w, height = h,
      r = grid::unit(0.012, "npc"),
      gp = grid::gpar(fill = "#FFFFFF", col = pal$primary, lwd = 1)
    )
    grid::grid.rect(
      x = x, y = y + h / 2 - 0.035, width = w, height = 0.07,
      gp = grid::gpar(fill = pal$primary, col = pal$primary)
    )
    grid::grid.text(title, x = x - w / 2 + 0.018, y = y + h / 2 - 0.035, just = c("left", "center"),
                    gp = grid::gpar(fontsize = 11, fontface = "bold", col = "white"))

    d <- as.data.frame(df)
    d[] <- lapply(d, as.character)
    max_rows <- min(nrow(d), 8)
    y0 <- y + h / 2 - 0.095
    if (ncol(d) >= 2) {
      grid::grid.text(names(d)[1], x = x - w * 0.42, y = y0, just = c("left", "top"),
                      gp = grid::gpar(fontsize = 8.2, fontface = "bold", col = pal$primary))
      grid::grid.text(names(d)[2], x = x + w * 0.34, y = y0, just = c("right", "top"),
                      gp = grid::gpar(fontsize = 8.2, fontface = "bold", col = pal$primary))
      yy <- y0 - 0.035
      for (i in seq_len(max_rows)) {
        grid::grid.text(stringr::str_trunc(d[i, 1], 42), x = x - w * 0.42, y = yy, just = c("left", "top"),
                        gp = grid::gpar(fontsize = 7.8, col = "#111827"))
        grid::grid.text(stringr::str_trunc(d[i, 2], 20), x = x + w * 0.34, y = yy, just = c("right", "top"),
                        gp = grid::gpar(fontsize = 7.8, col = "#111827"))
        yy <- yy - 0.033
      }
    }
  }

  draw_observations_page <- function() {
    grid::grid.newpage()
    draw_pdf_header("Observations and Disclaimer")

    disclaimer <- c(
      "This project is intended for educational, analytical and risk-screening purposes.",
      "Risk scores, anomaly indicators, concentration metrics and Benford deviations do not constitute evidence of misconduct.",
      "All outputs should be treated as review signals requiring further investigation and professional judgment."
    )

    grid::grid.roundrect(
      x = 0.5, y = 0.875, width = 0.92, height = 0.115,
      r = grid::unit(0.012, "npc"),
      gp = grid::gpar(fill = "#F8FAFC", col = pal$primary, lwd = 1.1)
    )
    grid::grid.text(
      paste(disclaimer, collapse = "\n"),
      x = 0.07, y = 0.915, just = c("left", "top"),
      gp = grid::gpar(fontsize = 8.0, lineheight = 1.12, col = "#111827")
    )

    df <- observations_df()
    card_w <- 0.43
    card_h <- 0.125
    x_pos <- c(0.265, 0.735)
    y_top <- 0.725
    gap_y <- 0.014

    for (i in seq_len(nrow(df))) {
      col_id <- ifelse(i <= ceiling(nrow(df) / 2), 1, 2)
      row_id <- ifelse(col_id == 1, i, i - ceiling(nrow(df) / 2))
      x <- x_pos[col_id]
      y <- y_top - (row_id - 1) * (card_h + gap_y)

      grid::grid.roundrect(
        x = x, y = y, width = card_w, height = card_h,
        r = grid::unit(0.012, "npc"),
        gp = grid::gpar(fill = "#FFFFFF", col = "#D1D5DB", lwd = 0.8)
      )
      grid::grid.rect(
        x = x - card_w / 2 + 0.004, y = y, width = 0.008, height = card_h * 0.84,
        gp = grid::gpar(fill = pal$accent, col = pal$accent)
      )
      grid::grid.text(
        toupper(df$theme[i]),
        x = x - card_w / 2 + 0.018, y = y + card_h / 2 - 0.016,
        just = c("left", "top"),
        gp = grid::gpar(fontsize = 7.2, fontface = "bold", col = pal$primary)
      )
      grid::grid.text(
        paste(strwrap(df$observation[i], width = 72), collapse = "\n"),
        x = x - card_w / 2 + 0.018, y = y + card_h / 2 - 0.038,
        just = c("left", "top"),
        gp = grid::gpar(fontsize = 6.3, fontface = "bold", lineheight = 1.03, col = "#111827")
      )
      grid::grid.text(
        paste(strwrap(paste0("Why it matters: ", df$why_it_matters[i]), width = 82), collapse = "\n"),
        x = x - card_w / 2 + 0.018, y = y - card_h / 2 + 0.032,
        just = c("left", "top"),
        gp = grid::gpar(fontsize = 6.0, lineheight = 1.02, col = "#374151")
      )
    }
  }

  draw_profile_table_page <- function(title, df) {
    df <- as.data.frame(df)
    df[] <- lapply(df, as.character)
    n <- nrow(df)
    rows_per_page <- 18
    chunks <- split(seq_len(n), ceiling(seq_len(n) / rows_per_page))

    for (chunk_id in seq_along(chunks)) {
      grid::grid.newpage()
      title_txt <- if (length(chunks) > 1) paste0(title, " (", chunk_id, "/", length(chunks), ")") else title
      draw_pdf_header(title_txt)

      idx <- chunks[[chunk_id]]
      left <- 0.10
      right <- 0.90
      top <- 0.82
      row_h <- 0.038
      metric_x <- 0.14
      value_x <- 0.86

      grid::grid.rect(x = 0.5, y = top + 0.035, width = 0.82, height = 0.06,
                      gp = grid::gpar(fill = pal$primary, col = pal$primary))
      grid::grid.text(title, x = left, y = top + 0.035, just = c("left", "center"),
                      gp = grid::gpar(fontsize = 10, fontface = "bold", col = "white"))

      grid::grid.text(names(df)[1], x = metric_x, y = top - 0.025, just = c("left", "center"),
                      gp = grid::gpar(fontsize = 8.2, fontface = "bold", col = pal$primary))
      grid::grid.text(names(df)[2], x = value_x, y = top - 0.025, just = c("right", "center"),
                      gp = grid::gpar(fontsize = 8.2, fontface = "bold", col = pal$primary))
      grid::grid.lines(x = c(left, right), y = c(top - 0.048, top - 0.048), gp = grid::gpar(col = "#CBD5E1"))

      y <- top - 0.085
      for (i in idx) {
        grid::grid.text(stringr::str_trunc(df[i, 1], 58), x = metric_x, y = y, just = c("left", "center"),
                        gp = grid::gpar(fontsize = 7.6, col = "#111827"))
        grid::grid.text(stringr::str_trunc(df[i, 2], 34), x = value_x, y = y, just = c("right", "center"),
                        gp = grid::gpar(fontsize = 7.6, col = "#111827"))
        grid::grid.lines(x = c(left, right), y = c(y - row_h / 2, y - row_h / 2), gp = grid::gpar(col = "#E5E7EB"))
        y <- y - row_h
      }
    }
  }

  grDevices::pdf(file, width = 11.69, height = 8.27, onefile = TRUE)
  on.exit(grDevices::dev.off(), add = TRUE)

  # Cover page: fill the previously sparse page with the overview risk-band charts.
  plot.new()
  text(0.5, 0.94, "Procurement Risk Analytics Dashboard", cex = 1.75, font = 2, col = pal$primary)
  text(
    0.05, 0.84,
    "Atlas Przetargów public procurement data\nCitation: Atlas Przetargów. (2026). Polish Public Tenders Dataset (BZP + TED) (Version 2026.Q2) [Data set].\nhttps://doi.org/10.5281/zenodo.19634050",
    adj = c(0, 1), cex = 0.78
  )
  text(
    0.05, 0.70,
    paste0(
      "Result notices: ", format_num(nrow(risk$result_notices)),
      "    Buyer-vendor relationships: ", format_num(nrow(risk$relationships)),
      "    Vendors: ", format_num(nrow(vendor_scorecard)),
      "    Buyers: ", format_num(nrow(buyer_scorecard))
    ),
    adj = c(0, 1), cex = 0.90
  )
  par(fig = c(0.05, 0.49, 0.07, 0.58), new = TRUE)
  plot_static_bands(vendor_scorecard, vendors, "Vendor Risk Bands", "Vendors", pal)
  par(fig = c(0.54, 0.98, 0.07, 0.58), new = TRUE)
  plot_static_bands(buyer_scorecard, buyers, "Buyer Risk Bands", "Buyers", pal)
  par(fig = c(0, 1, 0, 1), new = FALSE)

  for (page in pages) {
    if (page == "Overview") {
      plot_static_relationship_scatter()
    } else if (page == "Benford") {
      actual <- benford$summary$actual_pct; expected <- benford$summary$expected_pct
      barplot(rbind(actual, expected), beside = TRUE, names.arg = benford$summary$first_digit, col = c(pal$primary, pal$secondary), main = "Benford First-Digit Distribution", ylab = "Share")
      legend("topright", legend = c("Actual", "Expected"), fill = c(pal$primary, pal$secondary), bty = "n")
    } else if (page == "Vendor Risk") {
      d <- vendor_scorecard |> filter(contractor_contracts >= 3, !is.na(vendor_risk_score)) |> mutate(label = coalesce(contractor_name, contractor_id_clean))
      plot_static_top_bar(d, "label", vendor_risk_score, "Top Vendor Risk Scores", pal)
    } else if (page == "Buyer Risk") {
      d <- buyer_scorecard |> filter(buyer_contracts >= 5, !is.na(buyer_risk_score)) |> mutate(label = coalesce(buyer_name, buyer_nip_clean))
      plot_static_top_bar(d, "label", buyer_risk_score, "Top Buyer Risk Scores", pal)
    } else if (page == "Relationships") {
      d <- relationship_analysis$relationship_scorecard |> filter(relationship_contracts >= 2, !is.na(relationship_risk_score)) |> mutate(label = paste0(buyer_nip_clean, " → ", contractor_id_clean))
      plot_static_top_bar(d, "label", relationship_risk_score, "Top Relationship Risk Scores", pal)
    } else if (page == "Concentration") {
      d <- concentration_analysis$dominant_relationships |> filter(relationship_contracts >= 2, !is.na(mutual_dependency_score)) |> mutate(label = paste0(buyer_nip_clean, " → ", contractor_id_clean))
      plot_static_top_bar(d, "label", mutual_dependency_score, "Top Dominant Relationships", pal)
    } else if (page == "Voivodeship Map") {
      voivodeship_metrics <- c(
        "risk_density_score",
        "low_competition_rate",
        "single_offer_rate",
        "high_value_rate",
        "round_number_rate"
      )
      for (metric in voivodeship_metrics) {
        plot_static_voivodeship_geo(pal, metric = metric)
      }
      plot_static_voivodeship_bar(pal)
    } else if (page == "Observations") {
      draw_observations_page()
    } else if (page == "Data Tables") {
      draw_profile_table_page("Risk Rule Profile", profile_risk_rules(risk))
      draw_profile_table_page("Relationship Analysis Profile", profile_relationship_analysis(relationship_analysis))
      draw_profile_table_page("Concentration Analysis Profile", profile_concentration_analysis(concentration_analysis))
    }
  }
}

# ----------------------------
# UI
# ----------------------------

ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(title = "Procurement Risk Analytics Dashboard", titleWidth = 620),
  dashboardSidebar(
    sidebarMenu(
      id = "tabs",
      menuItem("Overview", tabName = "overview", icon = icon("chart-line")),
      menuItem("Benford", tabName = "benford", icon = icon("calculator")),
      menuItem("Vendor Risk", tabName = "vendors", icon = icon("building")),
      menuItem("Buyer Risk", tabName = "buyers", icon = icon("landmark")),
      menuItem("Relationships", tabName = "relationships", icon = icon("project-diagram")),
      menuItem("Concentration", tabName = "concentration", icon = icon("compress-arrows-alt")),
      menuItem("Voivodeship Map", tabName = "province_map", icon = icon("map")),
      menuItem("Observations", tabName = "observations", icon = icon("lightbulb")),
      menuItem("Data Tables", tabName = "tables", icon = icon("table"))
    ),
    div(
      class = "sidebar-export-block",
      selectInput("palette_choice", "Color palette", choices = names(dashboard_palettes), selected = "Muted Audit"),
      downloadButton("export_all_pdf", "Export full dashboard PDF")
    )
  ),
  dashboardBody(
    uiOutput("palette_css"),
    tabItems(
      tabItem(tabName = "overview",
        info_banner(),
        fluidRow(
          value_box(format_num(nrow(risk$result_notices)), "Result notices", "file-contract", "blue"),
          value_box(format_num(nrow(risk$relationships)), "Buyer-vendor relationships", "network-wired", "purple"),
          value_box(format_num(nrow(vendor_scorecard)), "Vendors", "building", "green"),
          value_box(format_num(nrow(buyer_scorecard)), "Buyers", "landmark", "yellow")
        ),
        fluidRow(
          box(width = 6, title = "Vendor Risk Bands", status = "primary", solidHeader = TRUE, plotlyOutput("vendor_band_plot", height = "360px")),
          box(width = 6, title = "Buyer Risk Bands", status = "primary", solidHeader = TRUE, plotlyOutput("buyer_band_plot", height = "360px"))
        ),
        fluidRow(how_to_box("How to read the log-scale relationship chart", p("Each point represents one buyer-vendor relationship."), p("The x-axis shows the number of contracts and the y-axis shows total relationship value. Both values are transformed with log10, which makes very large procurement relationships and smaller repeated relationships visible in the same view."), p("High, medium and critical risk points may indicate relationships worth reviewing, especially when they combine high value, repeated awards, low competition or strong dependency."))),
        fluidRow(box(width = 12, title = "Relationship Value vs Contract Count", status = "primary", solidHeader = TRUE, plotlyOutput("relationship_scatter", height = "500px")))
      ),
      tabItem(tabName = "benford",
        fluidRow(how_to_box("How to read Benford analysis", p("Benford's Law gives an expected distribution for the first digit of naturally occurring numeric values."), p("Large deviations between actual and expected first-digit shares can indicate unusual value patterns. This is not proof of misconduct; it is a screening signal for further review."), p("MAD is the mean absolute deviation between actual and expected shares. Lower MAD means closer conformity to Benford's Law."))),
        fluidRow(value_box(round(benford$mad$value, 4), "Benford MAD", "wave-square", "blue"), value_box(round(benford$chi_square$statistic, 2), "Chi-square statistic", "calculator", "yellow"), value_box(format_num(benford$chi_square$total_n), "Benford eligible rows", "database", "green"), value_box(benford$chi_square$df, "Degrees of freedom", "list-ol", "purple")),
        fluidRow(box(width = 8, title = "Benford First-Digit Distribution", status = "primary", solidHeader = TRUE, plotlyOutput("benford_plot", height = "420px")), box(width = 4, title = "Benford Summary", status = "primary", solidHeader = TRUE, DTOutput("benford_table")))
      ),
      tabItem(tabName = "vendors",
        fluidRow(column(width = 3, box(width = 12, title = "Filters", status = "primary", solidHeader = TRUE, sliderInput("vendor_min_contracts", "Minimum contracts", min = 1, max = 20, value = 3, step = 1), checkboxInput("vendor_include_anonymous", "Include anonymized natural persons", value = TRUE)), how_to_box("How to read vendor risk", p("Vendor risk scores combine procurement risk signals such as single-offer exposure, low competition, round-number awards, high-value awards and buyer concentration."), p("Use the minimum-contract filter to reduce noise from vendors with only one or two awards."), p("A high score is a prioritization signal. It means the vendor deserves review; it does not imply wrongdoing."), width = 12)), column(width = 9, box(width = 12, title = "Top Risky Vendors", status = "primary", solidHeader = TRUE, plotlyOutput("top_vendor_plot", height = "520px")))) ,
        fluidRow(box(width = 12, title = "Vendor Scorecard", status = "primary", solidHeader = TRUE, DTOutput("vendor_table")))
      ),
      tabItem(tabName = "buyers",
        fluidRow(column(width = 3, box(width = 12, title = "Filters", status = "primary", solidHeader = TRUE, sliderInput("buyer_min_contracts", "Minimum contracts", min = 1, max = 50, value = 5, step = 1), checkboxInput("buyer_include_anonymous", "Include anonymized natural persons", value = TRUE)), how_to_box("How to read buyer risk", p("Buyer risk scores summarize procurement behaviour from the contracting authority side."), p("Signals include concentration with a small number of vendors, repeated relationships, single-offer awards, low competition and high-value exposure."), p("The minimum-contract filter helps focus on buyers with enough activity to make the score more stable."), width = 12)), column(width = 9, box(width = 12, title = "Top Risky Buyers", status = "primary", solidHeader = TRUE, plotlyOutput("top_buyer_plot", height = "520px")))) ,
        fluidRow(box(width = 12, title = "Buyer Scorecard", status = "primary", solidHeader = TRUE, DTOutput("buyer_table")))
      ),
      tabItem(tabName = "relationships",
        fluidRow(column(width = 3, box(width = 12, title = "Filters", status = "primary", solidHeader = TRUE, sliderInput("relationship_min_contracts", "Minimum relationship contracts", min = 1, max = 30, value = 2, step = 1)), how_to_box("How to read relationship risk", p("Relationship risk evaluates individual buyer-vendor pairs rather than buyers or vendors alone."), p("Repeated contracts, high total value, low competition and round-number awards increase the relationship score."), p("This view is useful for finding persistent relationships that may deserve procurement review or audit sampling."), width = 12)), column(width = 9, box(width = 12, title = "Top Risky Buyer-Vendor Relationships", status = "primary", solidHeader = TRUE, plotlyOutput("top_relationship_plot", height = "520px")))) ,
        fluidRow(box(width = 6, title = "Buyer-Vendor Overlap", status = "primary", solidHeader = TRUE, DTOutput("overlap_table")), box(width = 6, title = "Relationship Scorecard", status = "primary", solidHeader = TRUE, DTOutput("relationship_table")))
      ),
      tabItem(tabName = "concentration",
        fluidRow(how_to_box("How to read concentration analysis", p("Concentration analysis measures dependency and dominance in procurement relationships."), p("Buyer concentration asks whether a buyer relies heavily on a small number of vendors. Vendor dependency asks whether a vendor relies heavily on a small number of buyers."), p("Dominant relationships with high mutual dependency can be legitimate, but they are strong candidates for risk-based audit review."))),
        fluidRow(value_box(format_num(nrow(concentration_analysis$buyer_vendor_concentration)), "Buyers with concentration metrics", "landmark", "blue"), value_box(format_num(nrow(concentration_analysis$vendor_buyer_dependency)), "Vendors with dependency metrics", "building", "green"), value_box(format_num(nrow(concentration_analysis$dominant_relationships)), "Dominant relationships", "link", "purple"), value_box(format_num(sum(concentration_analysis$dominant_relationships$mutual_dependency_band %in% c("High", "Critical"), na.rm = TRUE)), "High/Critical relationships", "exclamation-triangle", "red")),
        fluidRow(box(width = 12, title = "Top Dominant Buyer-Vendor Relationships", status = "primary", solidHeader = TRUE, plotlyOutput("dominant_relationship_plot", height = "520px"))),
        fluidRow(box(width = 6, title = "Buyer Vendor Concentration", status = "primary", solidHeader = TRUE, DTOutput("buyer_concentration_table")), box(width = 6, title = "Vendor Buyer Dependency", status = "primary", solidHeader = TRUE, DTOutput("vendor_dependency_table")))
      ),
      tabItem(tabName = "province_map",
        fluidRow(
          column(width = 3,
            box(width = 12, title = "Map controls", status = "primary", solidHeader = TRUE,
                selectInput("province_metric", "Risk metric", choices = c("Risk density score" = "risk_density_score", "Low-competition rate" = "low_competition_rate", "Single-offer rate" = "single_offer_rate", "High-value rate" = "high_value_rate", "Round-number rate" = "round_number_rate"), selected = "risk_density_score")),
            how_to_box("How to read voivodeship risk density", p("This view summarizes selected risk metrics across Polish voivodeships."), p("Darker areas indicate higher values for the selected regional metric. Useful metrics include low-competition rate, single-offer rate, round-number rate and a composite risk-density score."), p("This is a regional monitoring view. It helps identify where to inspect further, not whether a region has misconduct."), width = 12)
          ),
          column(width = 9,
            box(width = 12, title = "Voivodeship Risk Density Map", status = "primary", solidHeader = TRUE,
                plotlyOutput("province_geo_map", height = "620px"))
          )
        ),
        fluidRow(box(width = 12, title = "Voivodeship Risk Summary", status = "primary", solidHeader = TRUE, DTOutput("province_risk_table")))
      ),
      tabItem(tabName = "observations",
        fluidRow(how_to_box("Observations and Disclaimer", p("This page summarizes the main analytical story from the dashboard. The objective is to identify review candidates and concentration patterns, not to determine wrongdoing."), p("This project is intended for educational, analytical and risk-screening purposes."), p("Risk scores, anomaly indicators, concentration metrics and Benford deviations do not constitute evidence of misconduct."), p("All outputs should be treated as review signals requiring further investigation and professional judgment."))),
        fluidRow(box(width = 12, title = "Key observations", status = "primary", solidHeader = TRUE, div(class = "observations-grid", observation_cards())))
      ),
      tabItem(tabName = "tables", fluidRow(box(width = 12, title = "Risk Rule Profile", status = "primary", solidHeader = TRUE, DTOutput("risk_profile_table"))), fluidRow(box(width = 12, title = "Relationship Analysis Profile", status = "primary", solidHeader = TRUE, DTOutput("relationship_profile_table"))), fluidRow(box(width = 12, title = "Concentration Analysis Profile", status = "primary", solidHeader = TRUE, DTOutput("concentration_profile_table"))))
    )
  )
)

# ----------------------------
# Server
# ----------------------------

server <- function(input, output, session) {
  active_palette <- reactive(get_palette(input$palette_choice))
  output$palette_css <- renderUI(palette_css(input$palette_choice))

  output$export_all_pdf <- downloadHandler(
    filename = function() "procurement-risk-dashboard-full.pdf",
    content = function(file) write_dashboard_pdf(file, c("Overview", "Benford", "Vendor Risk", "Buyer Risk", "Relationships", "Concentration", "Voivodeship Map", "Observations", "Data Tables"), input$palette_choice)
  )

  output$vendor_band_plot <- renderPlotly(plot_vendor_risk_bands(vendor_scorecard, bar_color = active_palette()$primary))
  output$buyer_band_plot <- renderPlotly(plot_buyer_risk_bands(buyer_scorecard, bar_color = active_palette()$primary))
  output$relationship_scatter <- renderPlotly(plot_relationship_value_vs_count(relationship_analysis))
  output$benford_plot <- renderPlotly(plot_benford_distribution(benford, actual_color = active_palette()$primary, expected_color = active_palette()$secondary))
  output$top_vendor_plot <- renderPlotly(plot_top_vendors(vendor_scorecard, n = 15, min_contracts = input$vendor_min_contracts, include_anonymous = input$vendor_include_anonymous, bar_color = active_palette()$primary))
  output$top_buyer_plot <- renderPlotly(plot_top_buyers(buyer_scorecard, n = 15, min_contracts = input$buyer_min_contracts, include_anonymous = input$buyer_include_anonymous, bar_color = active_palette()$primary))
  output$top_relationship_plot <- renderPlotly(plot_top_relationships(relationship_analysis, n = 15, min_contracts = input$relationship_min_contracts, bar_color = active_palette()$primary))
  output$dominant_relationship_plot <- renderPlotly(plot_top_dominant_relationships(concentration_analysis, n = 15, min_contracts = 2, bar_color = active_palette()$primary))
  output$province_geo_map <- renderPlotly(plot_province_risk_geo_map(risk, metric = input$province_metric, fill_color = active_palette()$primary))

  output$benford_table <- renderDT({ benford$summary |> mutate(across(where(is.numeric), ~round(.x, 4))) |> make_dt(page_length = 9) }, server = FALSE)

  output$vendor_table <- renderDT({
    vendor_df <- vendor_scorecard |> filter(contractor_contracts >= input$vendor_min_contracts, !is.na(vendor_risk_score))
    if (!isTRUE(input$vendor_include_anonymous)) vendor_df <- vendor_df |> filter(is.na(contractor_name) | contractor_name != "[Osoba fizyczna]")
    vendor_df |> select(contractor_id_clean, contractor_name, contractor_city, contractor_province, contractor_contracts, contractor_total_value, vendor_risk_score, risk_band, single_offer_rate, low_competition_rate, round_number_rate, max_buyer_value_share_for_vendor) |> arrange(desc(vendor_risk_score)) |> make_dt(page_length = 15, order_col = 6)
  }, server = FALSE)

  output$buyer_table <- renderDT({
    buyer_df <- buyer_scorecard |> filter(buyer_contracts >= input$buyer_min_contracts, !is.na(buyer_risk_score))
    if (!isTRUE(input$buyer_include_anonymous)) buyer_df <- buyer_df |> filter(is.na(buyer_name) | buyer_name != "[Osoba fizyczna]")
    buyer_df |> select(any_of(c("buyer_nip_clean", "buyer_name", "buyer_city", "buyer_province", "buyer_contracts", "buyer_total_value.x", "buyer_risk_score", "risk_band", "single_offer_rate", "low_competition_rate", "round_number_rate", "max_vendor_value_share", "hhi_value"))) |> arrange(desc(buyer_risk_score)) |> make_dt(page_length = 15, order_col = 6)
  }, server = FALSE)

  output$relationship_table <- renderDT({ relationship_analysis$relationship_scorecard |> filter(relationship_contracts >= input$relationship_min_contracts) |> select(buyer_nip_clean, buyer_name, contractor_id_clean, contractor_name, relationship_contracts, relationship_value, relationship_risk_score, relationship_risk_band, single_offer_contracts, low_competition_contracts, round_number_contracts, high_value_contracts) |> arrange(desc(relationship_risk_score)) |> make_dt(page_length = 15, order_col = 6) }, server = FALSE)
  output$overlap_table <- renderDT({ relationship_analysis$buyer_vendor_overlap |> select(nip, buyer_name, contractor_name, buyer_risk_score, buyer_risk_band, vendor_risk_score, vendor_risk_band, both_sides_high_risk, combined_risk_score) |> arrange(desc(combined_risk_score)) |> make_dt(page_length = 15, order_col = 8) }, server = FALSE)
  output$buyer_concentration_table <- renderDT({ concentration_analysis$buyer_vendor_concentration |> select(buyer_nip_clean, buyer_name, buyer_risk_score, buyer_risk_band, buyer_relationships, buyer_total_relationship_value, max_vendor_value_share, hhi_vendor_value, buyer_vendor_concentration_score, buyer_vendor_concentration_band) |> arrange(desc(buyer_vendor_concentration_score)) |> make_dt(page_length = 15, order_col = 8) }, server = FALSE)
  output$vendor_dependency_table <- renderDT({ concentration_analysis$vendor_buyer_dependency |> select(contractor_id_clean, contractor_name, vendor_risk_score, vendor_risk_band, vendor_relationships, vendor_total_relationship_value, max_buyer_value_share, hhi_buyer_value, vendor_buyer_dependency_score, vendor_buyer_dependency_band) |> arrange(desc(vendor_buyer_dependency_score)) |> make_dt(page_length = 15, order_col = 8) }, server = FALSE)
  output$province_risk_table <- renderDT({ province_risk_summary(risk) |> select(province, voivodeship_name, result_notices, total_value, low_competition_rate, single_offer_rate, high_value_rate, round_number_rate, risk_density_score) |> arrange(desc(risk_density_score)) |> make_dt(page_length = 16, order_col = 8) }, server = FALSE)
  output$risk_profile_table <- renderDT(profile_risk_rules(risk) |> make_dt(page_length = 10), server = FALSE)
  output$relationship_profile_table <- renderDT(profile_relationship_analysis(relationship_analysis) |> make_dt(page_length = 10), server = FALSE)
  output$concentration_profile_table <- renderDT(profile_concentration_analysis(concentration_analysis) |> make_dt(page_length = 10), server = FALSE)
}

shinyApp(ui, server)