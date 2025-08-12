generate_table <- function(
  data,
  selected_question,
  selected_crosstab,
  date,
  nn,
  ci,
  tab_n
) {
  col_labels <- colnames(data) |>
    purrr::set_names() |>
    purrr::map(
      ~ gt::html(glue::glue("{sub(' (?=[^ ]*$)', '<br>', ., perl = TRUE)}"))
    )

  tbl <- data |>
    gt::gt() |>
    gt::tab_header(title = selected_question) |>
    gt::cols_label(.list = col_labels) |>
    gt::fmt_markdown(columns = everything()) |>
    gt::tab_style(
      style = gt::cell_text(weight = 'bold', size = px(13)),
      locations = list(
        gt::cells_column_labels(columns = gt::everything()),
        gt::cells_row_groups(groups = TRUE)
      )
    ) |>
    gt::tab_style(
      style = gt::cell_text(align = 'left', weight = 'bold', size = px(16)),
      locations = gt::cells_title(c("title"))
    ) |>
    gt::tab_style(
      style = gt::cell_text(align = 'left', weight = 'bold', size = px(14)),
      locations = gt::cells_title(c("subtitle"))
    ) |>
    gt::tab_style(
      style = gt::cell_text(align = 'center'),
      locations = gt::cells_column_labels(columns = -1) # Exclude first column
    ) |>
    gt::tab_style(
      style = gt::cell_text(align = 'left'),
      locations = gt::cells_column_labels(columns = 1) # Align first column left
    ) |>
    gt::tab_style(
      style = gt::cell_text(size = px(12)),
      locations = gt::cells_body(columns = gt::everything())
    ) |>
    gt::tab_style(
      style = gt::cell_text(align = "center"),
      locations = gt::cells_body(columns = -1)
    ) |>
    gt::tab_style(
      style = gt::cell_text(align = "left"),
      locations = gt::cells_body(columns = 1) # Align first column left
    ) |>
    gt::tab_options(
      data_row.padding = px(4),
      row_group.padding = px(4),
      source_notes.font.size = px(10),
      footnotes.font.size = px(10),
      footnotes.marks = "" # Empty footnote mark
    ) # Ensure CSS is applied

  # Format tables based on Crosstab availability
  if (selected_crosstab == "None") {
    tbl <- tbl |>
      gt::fmt_percent(
        columns = "proportion",
        decimals = 0
      ) |>
      gt::cols_label(
        n = "N",
        proportion = "Proportion"
      ) |>
      gt::tab_footnote(
        footnote = gt::html(glue::glue(
          "OnePulse survey; Survey date: {date}; N = {nn}"
        ))
      )
  } else {
    tbl <- tbl |>
      gt::tab_footnote(
        footnote = gt::html(glue::glue(
          "OnePulse survey; Survey date: {date}; N = {nn}<br>Crosstabbed with {selected_crosstab}; {tab_n}<br>(Letters) indicate significance at {scales::percent(as.numeric(ci))} confidence level"
        ))
      )
  }
  return(tbl)
}
