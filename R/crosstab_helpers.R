
summarize_single_crosstab <- function(data, question_col, crosstab_col, likert_dictionary, confidence_level) {
  was_factor <- is.factor(data[[crosstab_col]])
  crosstab_levels <- if (was_factor) levels(data[[crosstab_col]]) else NULL

  summarized_data <- data |> 
    select(Crosstab = !!sym(crosstab_col), Response = !!sym(question_col)) |> 
    count(Crosstab, Response) |> 
    group_by(Crosstab) |> 
    mutate(proportion = n / sum(n), total = sum(n)) |> 
    ungroup() |> 
    mutate(Crosstab = as.character(Crosstab)) |> 
    complete(Crosstab, Response, fill = list(n = 0, proportion = 0, total = 0))

  summarized_data <- fix_response_levels(summarized_data, "Response", likert_dictionary)
  summarized_data <- fix_response_levels(summarized_data, "Crosstab", likert_dictionary)

  if (was_factor) {
    summarized_data <- summarized_data |> 
      mutate(Crosstab = factor(Crosstab, levels = crosstab_levels)) |> 
      arrange(Crosstab, Response)
  }

  summarized_data <- summarized_data |> 
    filter(n > 0) 
  
  # Apply proportion test
  sumdata_list <- split(summarized_data, summarized_data$Response)
  summarized_data <- map_dfr(sumdata_list, ~ proptest_dataframe(.x, significance_level = confidence_level)) |> 
    ungroup()

  summarized_data
}

summarize_battery_crosstab <- function(data, question_data, question_prefix, crosstab_col, likert_dictionary, confidence_level) {
  was_factor <- is.factor(data[[crosstab_col]])
  crosstab_levels <- if (was_factor) levels(data[[crosstab_col]]) else NULL
  
  summarized_data <- data |> 
    select(Crosstab = !!sym(crosstab_col), all_of(colnames(question_data))) |> 
    pivot_longer(-Crosstab, names_to = "Subquestion", values_to = "Response") |> 
    mutate(Subquestion = str_match(Subquestion, "Q\\d+_\\d+ (.*?)\\[Question:")[, 2]) |> 
    group_by(Crosstab, Subquestion, Response) |> 
    summarize(n = n(), .groups = "drop") |> 
    group_by(Crosstab, Subquestion) |> 
    mutate(proportion = n / sum(n)) |>
    ungroup() |> 
    # edge case when there's no agreement with a statement, Response must equal Subquestion so it doesn't get dropped on filter
    left_join(data %>% count(!!sym(crosstab_col), name = "total") |> 
                rename(Crosstab := !!sym(crosstab_col)),
              by = "Crosstab") |> 
    mutate(Response = case_when(n == total & is.na(Response) ~ Subquestion,
                                TRUE ~ Response)) |> 
    filter(!is.na(Response)) |>
    mutate(Crosstab = as.character(Crosstab)) |> 
    complete(Crosstab, Response, Subquestion, fill = list(n = 0, proportion = 0, total = 0)) |> 
    mutate(Subquestion = ifelse(is.na(Subquestion), Response, Subquestion))
  
  summarized_data <- fix_response_levels(summarized_data, "Response", likert_dictionary)
  summarized_data <- fix_response_levels(summarized_data, "Crosstab", likert_dictionary)
  
  if (was_factor) {
    summarized_data <- summarized_data |> 
      mutate(Crosstab = factor(Crosstab, levels = crosstab_levels)) |> 
      arrange(Crosstab, Response)
  }
  
  summarized_data <- summarized_data |>  filter(n > 0) |> 
    # Fix edge case: if no one selected a response in this group, it may come through as n == total for NA 
    mutate(
      proportion = case_when(n == total ~ 0, TRUE ~ proportion),
      n = case_when(n == total ~ 0, TRUE ~ n),
    )
  
  summarized_data <- summarized_data |> 
    group_by(Crosstab) |> 
    filter(any(n > 0)) |>   # ✅ Keep only Crosstab groups with at least one non-zero response
    ungroup()
  
  sumdata_list <- split(summarized_data, summarized_data$Subquestion)
  summarized_data <- map_dfr(sumdata_list, ~ proptest_dataframe(.x, significance_level = confidence_level)) |>  
    group_by(Subquestion) |> 
    ungroup()
  
  summarized_data
}



generate_table_with_crosstab <- function(data, question_data, question_prefix, crosstab_var, likert_dictionary, confidence_level) {
  if (ncol(question_data) == 1) {
    summarize_single_crosstab(data, colnames(question_data), crosstab_var, likert_dictionary, confidence_level)
  } else {
    summarize_battery_crosstab(data, question_data, question_prefix, crosstab_var, likert_dictionary, confidence_level)
  }
}
