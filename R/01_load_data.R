# ============================================================
# 01_load_data.R
# Data loading layer
# ============================================================

source("R/00_config.R")

# ----------------------------
# Load raw parquet files
# ----------------------------

load_tenders <- function(years = c(2024, 2025)) {
  paths <- c()

  if (2024 %in% years) paths <- c(paths, PATH_TENDERS_2024)
  if (2025 %in% years) paths <- c(paths, PATH_TENDERS_2025)

  invisible(lapply(paths, ensure_file_exists))

  tenders <- purrr::map_dfr(
    paths,
    ~ arrow::read_parquet(.x) |>
      dplyr::mutate(source_file = basename(.x))
  )

  tenders
}

load_buyers <- function() {
  ensure_file_exists(PATH_BUYERS)
  arrow::read_parquet(PATH_BUYERS)
}

load_contractors <- function() {
  ensure_file_exists(PATH_CONTRACTORS)
  arrow::read_parquet(PATH_CONTRACTORS)
}

load_city_cache <- function() {
  ensure_file_exists(PATH_CITY_CACHE)
  arrow::read_parquet(PATH_CITY_CACHE)
}

# ----------------------------
# Load all project data
# ----------------------------

load_procurement_data <- function(years = c(2024, 2025)) {
  ensure_project_dirs()
  validate_input_files()

  list(
    tenders = load_tenders(years),
    buyers = load_buyers(),
    contractors = load_contractors(),
    city_cache = load_city_cache()
  )
}

# ----------------------------
# Basic filtered views
# ----------------------------

get_deduplicated_tenders <- function(tenders) {
  tenders |>
    dplyr::filter(is.na(is_duplicate) | is_duplicate == FALSE)
}

get_result_notices <- function(tenders) {
  tenders |>
    get_deduplicated_tenders() |>
    dplyr::filter(notice_type %in% RESULT_NOTICE_TYPES)
}

get_contract_notices <- function(tenders) {
  tenders |>
    get_deduplicated_tenders() |>
    dplyr::filter(notice_type %in% CONTRACT_NOTICE_TYPES)
}

# ----------------------------
# Quick profiling
# ----------------------------

profile_loaded_data <- function(data) {
  tibble::tibble(
    table = c("tenders", "buyers", "contractors", "city_cache"),
    rows = c(
      nrow(data$tenders),
      nrow(data$buyers),
      nrow(data$contractors),
      nrow(data$city_cache)
    ),
    columns = c(
      ncol(data$tenders),
      ncol(data$buyers),
      ncol(data$contractors),
      ncol(data$city_cache)
    )
  )
}