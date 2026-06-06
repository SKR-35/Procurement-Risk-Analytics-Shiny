# ============================================================
# 02_clean_prepare.R
# Cleaning and feature preparation layer
# ============================================================

source("R/01_load_data.R")

# ----------------------------
# Helper: CPV division
# ----------------------------

extract_cpv_division <- function(cpv_code) {
  cpv_code |>
    as.character() |>
    stringr::str_extract("^[0-9]{2}")
}

# ----------------------------
# Helper: first digit for Benford
# ----------------------------

extract_first_digit <- function(x) {
  x_abs <- abs(as.numeric(x))

  dplyr::case_when(
    is.na(x_abs) ~ NA_integer_,
    x_abs <= 0 ~ NA_integer_,
    TRUE ~ as.integer(substr(gsub("[^1-9]", "", as.character(floor(x_abs))), 1, 1))
  )
}

# ----------------------------
# Helper: round number flag
# ----------------------------

is_round_number <- function(x, moduli = ROUND_NUMBER_MODULI) {
  x_num <- suppressWarnings(as.numeric(x))

  purrr::map_lgl(x_num, function(v) {
    if (is.na(v) || v <= 0) {
      return(FALSE)
    }

    any(v %% moduli == 0)
  })
}

# ----------------------------
# Helper: value bucket
# ----------------------------

assign_value_bucket <- function(x) {
  dplyr::case_when(
    is.na(x) ~ NA_character_,
    x < 10000 ~ "<10k",
    x < 100000 ~ "10k-100k",
    x < 1000000 ~ "100k-1M",
    x < 10000000 ~ "1M-10M",
    TRUE ~ "10M+"
  )
}

# ----------------------------
# Prepare tenders
# ----------------------------

prepare_tenders <- function(tenders) {
  tenders |>
    dplyr::mutate(
      estimated_value_num = suppressWarnings(as.numeric(estimated_value)),
      offers_count_num = suppressWarnings(as.integer(offers_count)),

      date = as.Date(date),
      year = lubridate::year(date),
      month = lubridate::month(date),
      quarter = paste0("Q", lubridate::quarter(date)),

      buyer_nip_clean = clean_nip(buyer_nip),
      contractor_id_clean = clean_nip(contractor_national_id),

      cpv_division = extract_cpv_division(cpv_code),

      first_digit = extract_first_digit(estimated_value_num),
      is_benford_eligible = !is.na(estimated_value_num) &
        estimated_value_num >= MIN_VALUE_FOR_BENFORD &
        !is.na(first_digit),

      is_round_number = is_round_number(estimated_value_num),
      value_bucket = assign_value_bucket(estimated_value_num),

      has_buyer = !is.na(buyer_nip_clean) & buyer_nip_clean != "",
      has_contractor = !is.na(contractor_id_clean) & contractor_id_clean != "",

      is_result_notice = notice_type %in% RESULT_NOTICE_TYPES,
      is_contract_notice = notice_type %in% CONTRACT_NOTICE_TYPES
    )
}

# ----------------------------
# Prepare analysis dataset
# ----------------------------

prepare_procurement_data <- function(data) {
  tenders_clean <- data$tenders |>
    get_deduplicated_tenders() |>
    prepare_tenders()

  result_notices <- tenders_clean |>
    dplyr::filter(is_result_notice)

  contract_notices <- tenders_clean |>
    dplyr::filter(is_contract_notice)

  list(
    tenders = tenders_clean,
    result_notices = result_notices,
    contract_notices = contract_notices,
    buyers = data$buyers,
    contractors = data$contractors,
    city_cache = data$city_cache
  )
}

# ----------------------------
# Quick profiling
# ----------------------------

profile_prepared_data <- function(prepared) {
  tibble::tibble(
    metric = c(
      "deduplicated_tenders",
      "result_notices",
      "contract_notices",
      "benford_eligible_rows",
      "rows_with_buyer",
      "rows_with_contractor"
    ),
    value = c(
      nrow(prepared$tenders),
      nrow(prepared$result_notices),
      nrow(prepared$contract_notices),
      sum(prepared$tenders$is_benford_eligible, na.rm = TRUE),
      sum(prepared$tenders$has_buyer, na.rm = TRUE),
      sum(prepared$tenders$has_contractor, na.rm = TRUE)
    )
  )
}