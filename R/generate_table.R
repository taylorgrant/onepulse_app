generate_table <- function(data, selected_question, selected_crosstab, date, nn, ci, tab_n) {
  col_labels <- colnames(data) |> 
    set_names() |>  
    map(~ html(glue::glue("{sub(' (?=[^ ]*$)', '<br>', ., perl = TRUE)}")))
  
  tbl <- data |> 
    gt()  |> 
    tab_header(title = selected_question) |> 
    cols_label(.list = col_labels) |>
    fmt_markdown(columns = everything()) |> 
    tab_style(
      style = cell_text(weight = 'bold', size = px(13)),
      locations = list(
        cells_column_labels(columns = everything()),
        cells_row_groups(groups = TRUE)
      )
    ) |> 
    tab_style(
      style = cell_text(align = 'left', weight = 'bold', size = px(16)),
      locations = cells_title(c("title"))
    ) |> 
    tab_style(
      style = cell_text(align = 'left', weight = 'bold', size = px(14)),
      locations = cells_title(c("subtitle"))
    ) |> 
    tab_style(
      style = cell_text(align = 'center'),
      locations = cells_column_labels(columns = -1)  # Exclude first column
    ) |> 
    tab_style(
      style = cell_text(align = 'left'),
      locations = cells_column_labels(columns = 1)  # Align first column left
    ) |> 
    tab_style(
      style = cell_text(size = px(12)),
      locations = cells_body(columns = everything())
    ) |> 
    tab_style(
      style = cell_text(align = "center"),
      locations = cells_body(columns = -1)
    ) |> 
    tab_style(
      style = cell_text(align = "left"),
      locations = cells_body(columns = 1)  # Align first column left
    ) |> 
    tab_options(
      data_row.padding = px(4),
      row_group.padding = px(4),
      source_notes.font.size = px(10),
      footnotes.font.size = px(10),
      footnotes.marks = ""  # Empty footnote mark
    )   # Ensure CSS is applied
  
  # Format tables based on Crosstab availability
  if (selected_crosstab == "None") {
    tbl <- tbl |> 
      fmt_percent(
        columns = "proportion",
        decimals = 0
      ) |> 
      cols_label(
        n = "N",
        proportion = "Proportion"
      ) |> 
      tab_footnote(
        footnote = html(glue::glue("OnePulse survey; Survey date: {date}; N = {nn}"))
      )
  } else {
    tbl <- tbl |> 
      tab_footnote(
        footnote = html(glue::glue("OnePulse survey; Survey date: {date}; N = {nn}<br>Crosstabbed with {selected_crosstab}; {tab_n}<br>(Letters) indicate significance at {scales::percent(as.numeric(ci))} confidence level"))
      )
  }
  return(tbl)
}

