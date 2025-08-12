summarize_single_crosstab <- function(
  data,
  question_col,
  crosstab_col,
  likert_dictionary,
  confidence_level
) {
  was_factor <- is.factor(data[[crosstab_col]])
  crosstab_levels <- if (was_factor) levels(data[[crosstab_col]]) else NULL

  summarized_data <- data |>
    dplyr::select(
      Crosstab = !!rlang::sym(crosstab_col),
      Response = !!rlang::sym(question_col)
    ) |>
    dplyr::count(Crosstab, Response) |>
    dplyr::group_by(Crosstab) |>
    dplyr::mutate(proportion = n / sum(n), total = sum(n)) |>
    dplyr::ungroup() |>
    dplyr::mutate(Crosstab = as.character(Crosstab)) |>
    tidyr::complete(
      Crosstab,
      Response,
      fill = list(n = 0, proportion = 0, total = 0)
    )

  summarized_data <- fix_response_levels(
    summarized_data,
    "Response",
    likert_dictionary
  )
  summarized_data <- fix_response_levels(
    summarized_data,
    "Crosstab",
    likert_dictionary
  )

  if (was_factor) {
    summarized_data <- summarized_data |>
      dplyr::mutate(Crosstab = factor(Crosstab, levels = crosstab_levels)) |>
      dplyr::arrange(Crosstab, Response)
  }

  summarized_data <- summarized_data |>
    dplyr::filter(n > 0)

  # Apply proportion test
  sumdata_list <- split(summarized_data, summarized_data$Response)
  summarized_data <- purrr::map_dfr(
    sumdata_list,
    ~ proptest_dataframe(.x, significance_level = confidence_level)
  ) |>
    dplyr::ungroup()

  summarized_data
}

summarize_battery_crosstab <- function(
  data,
  question_data,
  question_prefix,
  crosstab_col,
  likert_dictionary,
  confidence_level
) {
  was_factor <- is.factor(data[[crosstab_col]])
  crosstab_levels <- if (was_factor) levels(data[[crosstab_col]]) else NULL

  summarized_data <- data |>
    dplyr::select(
      Crosstab = !!rlang::sym(crosstab_col),
      dplyr::all_of(colnames(question_data))
    ) |>
    tidyr::pivot_longer(
      -Crosstab,
      names_to = "Subquestion",
      values_to = "Response"
    ) |>
    dplyr::mutate(
      Subquestion = stringr::str_match(
        Subquestion,
        "Q\\d+_\\d+ (.*?)\\[Question:"
      )[, 2]
    ) |>
    dplyr::group_by(Crosstab, Subquestion, Response) |>
    dplyr::summarize(n = dplyr::n(), .groups = "drop") |>
    dplyr::group_by(Crosstab, Subquestion) |>
    dplyr::mutate(proportion = n / sum(n)) |>
    dplyr::ungroup() |>
    # edge case when there's no agreement with a statement, Response must equal Subquestion so it doesn't get dropped on filter
    dplyr::left_join(
      data %>%
        dplyr::count(!!sym(crosstab_col), name = "total") |>
        dplyr::rename(Crosstab := !!rlang::sym(crosstab_col)),
      by = "Crosstab"
    ) |>
    dplyr::mutate(
      Response = dplyr::case_when(
        n == total & is.na(Response) ~ Subquestion,
        TRUE ~ Response
      )
    ) |>
    dplyr::filter(!is.na(Response)) |>
    dplyr::mutate(Crosstab = as.character(Crosstab)) |>
    tidyr::complete(
      Crosstab,
      Response,
      Subquestion,
      fill = list(n = 0, proportion = 0, total = 0)
    ) |>
    dplyr::mutate(
      Subquestion = ifelse(is.na(Subquestion), Response, Subquestion)
    )

  summarized_data <- fix_response_levels(
    summarized_data,
    "Response",
    likert_dictionary
  )
  summarized_data <- fix_response_levels(
    summarized_data,
    "Crosstab",
    likert_dictionary
  )

  if (was_factor) {
    summarized_data <- summarized_data |>
      dplyr::mutate(Crosstab = factor(Crosstab, levels = crosstab_levels)) |>
      dplyr::arrange(Crosstab, Response)
  }

  summarized_data <- summarized_data |>
    dplyr::filter(n > 0) |>
    # Fix edge case: if no one selected a response in this group, it may come through as n == total for NA
    dplyr::mutate(
      proportion = dplyr::case_when(n == total ~ 0, TRUE ~ proportion),
      n = dplyr::case_when(n == total ~ 0, TRUE ~ n),
    )

  summarized_data <- summarized_data |>
    dplyr::group_by(Crosstab) |>
    dplyr::filter(any(n > 0)) |> # ✅ Keep only Crosstab groups with at least one non-zero response
    dplyr::ungroup()

  sumdata_list <- split(summarized_data, summarized_data$Subquestion)
  summarized_data <- purrr::map_dfr(
    sumdata_list,
    ~ proptest_dataframe(.x, significance_level = confidence_level)
  ) |>
    dplyr::group_by(Subquestion) |>
    dplyr::ungroup()

  summarized_data
}


generate_table_with_crosstab <- function(
  data,
  question_data,
  question_prefix,
  crosstab_var,
  likert_dictionary,
  confidence_level
) {
  if (ncol(question_data) == 1) {
    summarize_single_crosstab(
      data,
      colnames(question_data),
      crosstab_var,
      likert_dictionary,
      confidence_level
    )
  } else {
    summarize_battery_crosstab(
      data,
      question_data,
      question_prefix,
      crosstab_var,
      likert_dictionary,
      confidence_level
    )
  }
}
