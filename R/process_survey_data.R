process_survey_data <- function(file_upload) {
  shiny::req(file_upload) # Ensure a file is uploaded
  file_path <- file_upload$datapath

  # Read in file to get Data and N size
  if (grepl("\\.csv$", file_upload$name)) {
    tmp <- readr::read_csv(file_path, n_max = 1, col_names = FALSE)
    date <- stringr::str_extract(tmp, "\\d{4}-\\d{2}-\\d{2}")
    respondent_count <- stringr::str_remove(
      trimws(sub(".*[:]", "", tmp)),
      "\\)"
    )
  } else {
    tmp <- readxl::read_excel(file_path, n_max = 1, col_names = FALSE)
    date <- stringr::str_extract(tmp, "\\d{4}-\\d{2}-\\d{2}")
    respondent_count <- stringr::str_remove(
      trimws(sub(".*[:]", "", tmp)),
      "\\)"
    )
  }

  # Read file based on extension
  if (grepl("\\.csv$", file_upload$name)) {
    data <- readr::read_csv(file_path, skip = 3)
  } else {
    data <- readxl::read_excel(file_path, skip = 3)
  }

  # Clean column names
  colnames(data) <- gsub("\\((.*?)\\)", "\\1", colnames(data))
  colnames(data) <- gsub("\\.+", " ", colnames(data)) # Replace dots with spaces
  colnames(data) <- stringr::str_trim(colnames(data)) # Trim spaces

  # Identify and remove sentiment columns
  sentiment_columns <- stringr::str_extract(
    colnames(data)[stringr::str_detect(colnames(data), "^Q\\d+ Sentiment")],
    "[^ ]+" # Extract the Q#
  )
  if (length(sentiment_columns) > 0) {
    data <- data %>%
      dplyr::select(
        -dplyr::matches(paste0(
          "^(",
          paste(sentiment_columns, collapse = "|"),
          ")"
        ))
      )
  }

  # Drop columns that are entirely NA
  data <- data %>% dplyr::select(where(~ any(!is.na(.))))

  # Add Age Cohorts and Generation
  if ("Age range" %in% colnames(data)) {
    data <- data |>
      dplyr::mutate(
        `Age range` = as.numeric(`Age range`),
        Cohort = dplyr::case_when(
          `Age range` <= 24 ~ "18-24",
          `Age range` > 24 & `Age range` <= 34 ~ "25-34",
          `Age range` > 34 & `Age range` <= 44 ~ "35-44",
          `Age range` > 44 & `Age range` <= 54 ~ "45-54",
          `Age range` > 54 & `Age range` <= 64 ~ "55-64",
          `Age range` > 64 ~ "65+"
        ),
        Cohort = factor(
          Cohort,
          levels = c("18-24", "25-34", "35-44", "45-54", "55-64", "65+")
        ),
        yob = lubridate::year(Sys.Date()) - `Age range`,
        Generation = dplyr::case_when(
          yob < 2013 & yob > 1996 ~ "Gen Z",
          yob < 1997 & yob > 1980 ~ "Millennials",
          yob < 1981 & yob > 1964 ~ "Gen X",
          yob < 1965 & yob > 1945 ~ "Boomers"
        ),
        Generation = factor(
          Generation,
          levels = c("Gen Z", "Millennials", "Gen X", "Boomers")
        ),
        `Age Decile` = dplyr::case_when(
          `Age range` <= 19 ~ "<20",
          `Age range` >= 20 & `Age range` <= 29 ~ "20-29",
          `Age range` >= 30 & `Age range` <= 39 ~ "30-39",
          `Age range` >= 40 & `Age range` <= 49 ~ "40-49",
          `Age range` >= 50 & `Age range` <= 59 ~ "50-59",
          `Age range` >= 59 ~ "60+",
        ),
        `Age Decile` = factor(
          `Age Decile`,
          levels = c("<20", "20-29", "30-39", "40-49", "50-59", "60+")
        )
      ) |>
      dplyr::select(-yob) |>
      dplyr::relocate(Cohort, .after = "Age range") |>
      dplyr::relocate(Generation, .after = "Cohort") |>
      dplyr::relocate(`Age Decile`, .after = "Generation")
  }

  if ("Parent" %in% colnames(data)) {
    data <- data |>
      dplyr::mutate(
        Parent = factor(Parent)
      )
  }

  if ("Home location" %in% colnames(data)) {
    data <- data |>
      dplyr::mutate(
        `Home location` = factor(
          `Home location`,
          levels = c("West", "Midwest", "Northeast", "South")
        )
      )
  }

  if ("Combined household income" %in% colnames(data)) {
    data <- data |>
      dplyr::mutate(
        HHI = dplyr::case_when(
          `Combined household income` %in%
            c("$0 - $24;999", "$25;000 - $49;999") ~
            "< $50k",
          `Combined household income` %in% c("$50;000 - $74;999") ~
            "$50k - $75k",
          `Combined household income` %in% c("$75;000 - $99;999") ~
            "$75k - $100k",
          `Combined household income` %in%
            c("$100;000 - $124;999", "$150;000 - $174;999") ~
            "$100k - $150k",
          `Combined household income` %in%
            c("$150;000 - $174;999", "$175;000 - $199;999") ~
            "$150k - $200k",
          TRUE ~ "$200k+"
        ),
        HHI = factor(
          HHI,
          levels = c(
            "< $50k",
            "$50k - $75k",
            "$75k - $100k",
            "$100k - $150k",
            "$150k - $200k",
            "$200k+"
          )
        )
      ) |>
      dplyr::relocate(HHI, .after = `Combined household income`)
  }

  if ("Ethnicity" %in% colnames(data)) {
    data <- data |>
      dplyr::mutate(
        Ethnicity = factor(
          Ethnicity,
          levels = c(
            "White",
            "Hispanic/Latino",
            "Black",
            "Asian",
            "American Indian/Alaskan Native",
            "Other"
          )
        )
      )
  }

  if ("Home geography" %in% colnames(data)) {
    data <- data |>
      dplyr::mutate(
        `Home geography` = factor(
          `Home geography`,
          levels = c("Urban", "Suburban", "Rural")
        )
      )
  }

  data <- list(data = data, date = date, respondent_count = respondent_count)
  return(data)
}
