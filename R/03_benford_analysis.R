# ============================================================
# 03_benford_analysis.R
# Benford analysis layer
# ============================================================

source("R/02_clean_prepare.R")

# ----------------------------
# Benford expected distribution
# ----------------------------

get_benford_expected <- function() {
  tibble::tibble(
    first_digit = 1:9,
    expected_pct = log10(1 + 1 / first_digit)
  )
}

# ----------------------------
# Actual first digit distribution
# ----------------------------

calculate_actual_digit_distribution <- function(df) {
  df |>
    dplyr::filter(is_benford_eligible) |>
    dplyr::count(first_digit, name = "actual_n") |>
    tidyr::complete(
      first_digit = 1:9,
      fill = list(actual_n = 0)
    ) |>
    dplyr::mutate(
      total_n = sum(actual_n),
      actual_pct = safe_divide(actual_n, total_n)
    )
}

# ----------------------------
# Benford summary table
# ----------------------------

calculate_benford_summary <- function(df) {
  actual <- calculate_actual_digit_distribution(df)
  expected <- get_benford_expected()

  actual |>
    dplyr::left_join(expected, by = "first_digit") |>
    dplyr::mutate(
      expected_n = total_n * expected_pct,
      deviation_pct = actual_pct - expected_pct,
      abs_deviation_pct = abs(deviation_pct),
      chi_square_component = dplyr::if_else(
        expected_n > 0,
        ((actual_n - expected_n)^2) / expected_n,
        NA_real_
      )
    )
}

# ----------------------------
# Chi-square statistic
# ----------------------------

calculate_benford_chi_square <- function(benford_summary) {
  statistic <- sum(benford_summary$chi_square_component, na.rm = TRUE)
  df <- 8
  p_value <- stats::pchisq(statistic, df = df, lower.tail = FALSE)

  tibble::tibble(
    test = "Benford first digit chi-square",
    statistic = statistic,
    df = df,
    p_value = p_value,
    total_n = unique(benford_summary$total_n)[1]
  )
}

# ----------------------------
# Mean absolute deviation
# ----------------------------

calculate_benford_mad <- function(benford_summary) {
  mad_value <- mean(benford_summary$abs_deviation_pct, na.rm = TRUE)

  tibble::tibble(
    metric = "Mean Absolute Deviation",
    value = mad_value
  )
}

# ----------------------------
# Benford analysis wrapper
# ----------------------------

run_benford_analysis <- function(df) {
  summary <- calculate_benford_summary(df)
  chi_square <- calculate_benford_chi_square(summary)
  mad <- calculate_benford_mad(summary)

  list(
    summary = summary,
    chi_square = chi_square,
    mad = mad
  )
}

# ----------------------------
# Grouped Benford analysis
# ----------------------------

run_grouped_benford_analysis <- function(df, group_col, min_n = 100) {
  group_col <- rlang::ensym(group_col)

  df |>
    dplyr::filter(is_benford_eligible) |>
    dplyr::group_by(!!group_col) |>
    dplyr::filter(dplyr::n() >= min_n) |>
    dplyr::group_modify(~ {
      result <- run_benford_analysis(.x)

      tibble::tibble(
        total_n = result$chi_square$total_n,
        chi_square = result$chi_square$statistic,
        p_value = result$chi_square$p_value,
        mad = result$mad$value
      )
    }) |>
    dplyr::ungroup() |>
    dplyr::arrange(dplyr::desc(mad))
}

# ----------------------------
# Convenience wrappers
# ----------------------------

run_overall_benford <- function(prepared) {
  run_benford_analysis(prepared$result_notices)
}

run_benford_by_buyer <- function(prepared, min_n = 100) {
  run_grouped_benford_analysis(
    prepared$result_notices,
    buyer_nip_clean,
    min_n = min_n
  )
}

run_benford_by_cpv_division <- function(prepared, min_n = 100) {
  run_grouped_benford_analysis(
    prepared$result_notices,
    cpv_division,
    min_n = min_n
  )
}

run_benford_by_province <- function(prepared, min_n = 100) {
  run_grouped_benford_analysis(
    prepared$result_notices,
    province,
    min_n = min_n
  )
}