# ============================================================
# 00_config.R
# Project-wide configuration
# ============================================================

library(dplyr)
library(readr)
library(arrow)
library(stringr)
library(lubridate)
library(purrr)
library(tibble)

# ----------------------------
# Paths
# ----------------------------

PROJECT_DIR <- getwd()
DATA_DIR <- file.path(PROJECT_DIR, "data")
OUTPUT_DIR <- file.path(PROJECT_DIR, "outputs")

PATH_TENDERS_2024 <- file.path(DATA_DIR, "tenders_2024.parquet")
PATH_TENDERS_2025 <- file.path(DATA_DIR, "tenders_2025.parquet")
PATH_BUYERS <- file.path(DATA_DIR, "buyers.parquet")
PATH_CONTRACTORS <- file.path(DATA_DIR, "contractors.parquet")
PATH_CITY_CACHE <- file.path(DATA_DIR, "city_cache.parquet")

# ----------------------------
# Analysis settings
# ----------------------------

RESULT_NOTICE_TYPES <- c(
  "TenderResultNotice",
  "ContractAwardNotice",
  "can-standard"
)

CONTRACT_NOTICE_TYPES <- c(
  "ContractNotice",
  "cn-standard"
)

DEFAULT_CURRENCY <- "PLN"
MIN_VALUE_FOR_BENFORD <- 1000

ROUND_NUMBER_MODULI <- c(
  1000,
  5000,
  10000,
  50000,
  100000
)

TOP_N_TABLE <- 25

# ----------------------------
# Utility checks
# ----------------------------

ensure_dir <- function(path) {
  if (!dir.exists(path)) {
    dir.create(path, recursive = TRUE)
  }
}

ensure_file_exists <- function(path) {
  if (!file.exists(path)) {
    stop("Missing file: ", path, call. = FALSE)
  }
}

ensure_project_dirs <- function() {
  ensure_dir(DATA_DIR)
  ensure_dir(OUTPUT_DIR)
}

validate_input_files <- function() {
  required_files <- c(
    PATH_TENDERS_2024,
    PATH_TENDERS_2025,
    PATH_BUYERS,
    PATH_CONTRACTORS,
    PATH_CITY_CACHE
  )

  invisible(lapply(required_files, ensure_file_exists))
}

# ----------------------------
# Small helpers
# ----------------------------

safe_divide <- function(x, y) {
  ifelse(is.na(y) | y == 0, NA_real_, x / y)
}

clean_nip <- function(x) {
  x |>
    as.character() |>
    stringr::str_replace_all("[^0-9A-Za-z-]", "") |>
    stringr::str_trim()
}