source("R/crosstab_helpers.R")

# Helper: Get Likert levels or fallback to frequency order
fix_response_levels <- function(data, colname, likert_dictionary) {
  response_values <- na.omit(data[[colname]])
  
  if (is.logical(response_values) || is.factor(response_values)) {
    return(data)
  }
  
  matched_idx <- which(sapply(likert_dictionary, function(scale) all(response_values %in% scale)))
  
  if (length(matched_idx) == 1) {
    levels <- likert_dictionary[[matched_idx]]
  } else if (all(!is.na(suppressWarnings(as.numeric(response_values))))) {
    levels <- sort(unique(as.numeric(response_values)))
  } else {
    levels <- data |> 
      count(!!sym(colname)) |> 
      arrange(desc(n)) |> 
      pull(!!sym(colname))
  }
  
  data[[colname]] <- factor(data[[colname]], levels = levels)
  return(data)
}

# Helper: Summarize single-question no-crosstab
summarize_single_question <- function(data, question, likert_dictionary) {
  out <- data %>%
    count(Response = .[[question]]) |> 
    mutate(proportion = n / sum(n))  
    # arrange(Response)
  out <- fix_response_levels(out, "Response", likert_dictionary)
  out <- out |> arrange(Response)
  out
}

# Helper: Summarize battery no-crosstab
summarize_battery <- function(data, question_prefix, likert_dictionary) {
  out <- data |> 
    # select(starts_with(question_prefix)) |>
    pivot_longer(everything(), names_to = "Subquestion", values_to = "Response") |> 
    mutate(Subquestion = str_match(Subquestion, "Q\\d+_\\d+ (.*?)\\[Question:")[,2]) |> 
    group_by(Subquestion, Response) |> 
    summarise(n = n(), .groups = "drop") |> 
    group_by(Subquestion) |> 
    mutate(proportion = n / sum(n)) |> 
    ungroup() |> 
    filter(!is.na(Response)) |>  
    select(-Subquestion)
  out
}

# Helper: Crosstab summary string
crosstab_summary <- function(data, crosstab_variable) {
  counts <- table(data[[crosstab_variable]])
  paste(names(counts), counts, sep = " = ", collapse = "; ")
}

# Main function
generate_table_data <- function(input, survey_data) {

  likert_dictionary <- list(
    "Agreement" = c("Strongly disagree", "Disagree", "Neutral", "Agree", "Strongly agree"),
    "Agreement2" = c("Strongly disagree", "Disagree a little", "Neither agree nor disagree", "Agree a little", "Strongly Agree"),
    "Familiarity" = c("Very unfamiliar", "Somewhat unfamiliar", "Neutral", "Somewhat familiar", "Very familiar"),
    "Satisfaction" = c("Very dissatisfied", "Dissatisfied", "Neutral", "Satisfied", "Very satisfied"),
    "Importance" = c("Not important", "Slightly important", "Neutral", "Important", "Very important"),
    "Frequency" = c("Never", "Rarely", "Sometimes", "Often", "Always"),
    "Frequency2" = c("Daily", "Several times a week", "Once a week", "Less than once a week", "Rarely / Never"),
    "Frequency3" = c("All the time", "Often", "Sometimes", "Rarely", "Never"),
    "Likelihood" = c("Very unlikely", "Unlikely", "Neutral", "Likely", "Very likely"),
    "Confidence" = c("Much more confident", "Somewhat more confident", "About the same", "Somewhat less confident", "Much less confident"),
    "PID" = c("Very Conservative", "Lean Conservative", "Moderate / Middloe of the road", "Lean Liberal", "Very Liberal"),
    "Likeability" = c("Dislike very much", "Dislike somewhat", "Like Somewhat", "Like very much", "I do not know who this person is.")
  )

  eventReactive(input$generate_table, {
    req(input$question, survey_data()$data)
    data <- survey_data()$data
    date <- survey_data()$date
    respondent_count <- survey_data()$respondent_count

    selected_question <- input$question
    selected_crosstab <- if (input$crosstab == "None") NULL else input$crosstab

    if (str_detect(selected_question, "^Q\\d+")) {
      question_prefix <- str_extract(selected_question, "^Q\\d+")
      selected_data <- data |> 
        select(matches(paste0("^", question_prefix, "(_|\\b)"))) |> 
        select(!contains("Comments"))
    } else {
      selected_data <- data |> select(all_of(selected_question))
    }

    if (is.null(selected_crosstab)) {
      summarized_data <- if (ncol(selected_data) == 1) {
        summarize_single_question(selected_data, 1, likert_dictionary)
      } else {
        summarize_battery(selected_data, question_prefix, likert_dictionary)
      }
    } else {
      # Delegate to the existing logic from generate_table.R or build similar helpers
      summarized_data <- generate_table_with_crosstab(
        data = data,
        question_data = selected_data,
        question_prefix = question_prefix,
        crosstab_var = selected_crosstab,
        likert_dictionary = likert_dictionary,
        confidence_level = 1 - as.numeric(input$confidence_interval)
      )
    }

    if ("Subquestion" %in% names(summarized_data) && all(summarized_data$Subquestion == summarized_data$Response)) {
      summarized_data <- summarized_data %>% select(-Subquestion)
    }
    
    tab_n <- crosstab_summary(data, input$crosstab)

    list(
      summarized_data = summarized_data,
      date = date,
      respondent_count = respondent_count,
      tab_n = tab_n
    )
  })
}
