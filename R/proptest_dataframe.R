proptest_dataframe <- function(data, significance_level) {
  if (any(names(data) == "Subquestion")) {
    tmp <- data |>
      dplyr::arrange(Response, Subquestion) |>
      dplyr::group_by(Response, Subquestion) |>
      dplyr::mutate(id = LETTERS[1:n()]) # Assign capital letters as IDs
  } else {
    tmp <- data |>
      dplyr::arrange(Response) |>
      dplyr::group_by(Response) |>
      dplyr::mutate(id = LETTERS[1:n()]) # Assign capital letters as IDs
  }

  run_prop_test_significant <- function(i, j, data) {
    prop1 <- data$proportion[i]
    prop2 <- data$proportion[j]

    n1 <- data$n[i]
    n2 <- data$n[j]

    total1 <- data$total[i]
    total2 <- data$total[j]

    counts <- c(n1, n2)
    totals <- c(total1, total2)

    # new fix
    if (any(counts == 0) | any(totals == 0)) {
      return(FALSE)
    }

    test_result <- prop.test(counts, totals)

    return(test_result$p.value < significance_level && prop1 > prop2)
  }

  tmp$greater_than <- NA

  for (i in 1:nrow(tmp)) {
    greater_ids <- c()
    for (j in 1:nrow(tmp)) {
      if (i != j && run_prop_test_significant(i, j, tmp)) {
        greater_ids <- c(greater_ids, tmp$id[j])
      }
    }
    tmp$greater_than[i] <- paste(greater_ids, collapse = ", ")
  }

  # here i'm going to subscript my `greater_than` to show significance
  tmp <- tmp |>
    dplyr::mutate(
      brand_id = glue::glue("{Crosstab} {id}"),
      proportion_with_sub = ifelse(
        greater_than != "",
        glue::glue("{round(proportion * 100)}% <sub>({greater_than})</sub>"),
        glue::glue("{round(proportion * 100)}%")
      )
    )

  # check to see if subquestion is included in the data
  if (any(names(tmp) == "Subquestion")) {
    # widen the data
    tmp_wide <- tmp |>
      dplyr::select(Subquestion, Response, brand_id, proportion_with_sub) |>
      tidyr::pivot_wider(
        names_from = brand_id,
        values_from = proportion_with_sub,
        values_fn = list(proportion_with_sub = function(x) {
          paste(x, collapse = "")
        })
      )
  } else {
    # widen the data
    tmp_wide <- tmp |>
      dplyr::select(Response, brand_id, proportion_with_sub) |>
      tidyr::pivot_wider(
        names_from = brand_id,
        values_from = proportion_with_sub,
        values_fn = list(proportion_with_sub = function(x) {
          paste(x, collapse = "")
        })
      )
    #
  }

  return(tmp_wide)
}
