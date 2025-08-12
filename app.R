pacman::p_load(tidyverse, readxl, shiny, gt, tidyr, glue, bslib)

# Helper functions in R folder
# proptest_dataframe()
# generate_table()
# process_survey_data()
# generate_table_data()

# UI ----------------------------------------------------------------------

# Define UI for the application
ui <- shiny::fluidPage(
  theme = bslib::bs_theme(version = 4, bootswatch = "flatly"), # Choose a modern Bootstrap theme
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
  ),

  shiny::titlePanel("OnePulse Survey Summary"),
  shiny::sidebarLayout(
    shiny::sidebarPanel(
      shiny::fileInput(
        "file_upload",
        "Upload Dataset:",
        accept = c(".xlsx", ".csv")
      ),
      shiny::uiOutput("question_selector"),
      shiny::uiOutput("crosstab_selector"),
      shiny::radioButtons(
        "confidence_interval",
        label = "Select Confidence Interval",
        choices = c("80%" = 0.8, "90%" = 0.9, "95%" = 0.95),
        selected = 0.9
      ),
      shiny::actionButton(
        "generate_table",
        "Generate Table",
        class = "btn-primary"
      ), # Styled button
      shiny::hr(),
      shiny::uiOutput("capture_button")
    ),
    shiny::mainPanel(
      shiny::div(
        gt_output("gt_table"),
        class = "no-scroll" # custom CSS hook
      ),
      shiny::div(style = "margin-top: 40px;")
    )
  )
)


# SERVER ------------------------------------------------------------------

# Define server logic
server <- function(input, output, session) {
  # Dynamically provide the UI for table capture
  output$capture_button <- shiny::renderUI({
    shiny::req(input$question)

    # Extract question ID for filename
    question_id <- if (stringr::str_detect(input$question, "^Q\\d+")) {
      stringr::str_extract(input$question, "^Q\\d+")
    } else {
      input$question
    }

    # Add crosstab if selected
    if (!is.null(input$crosstab) && input$crosstab != "None") {
      crosstab_id <- if (stringr::str_detect(input$crosstab, "^Q\\d+")) {
        stringr::str_extract(input$crosstab, "^Q\\d+")
      } else {
        input$crosstab
      }

      base_filename <- paste0(
        "table_",
        question_id,
        "_by_",
        crosstab_id,
        ".png"
      )
    } else {
      base_filename <- paste0("table_", question_id, ".png")
    }

    capture::capture(
      id = "capture-container",
      selector = "#gt_table",
      filename = base_filename,
      shiny::icon("download"),
      "Download Table"
    )
  })

  # Likert scales are located in `generate_table_data.R`

  # Reactive to load uploaded dataset
  survey_data <- shiny::reactive({
    process_survey_data(input$file_upload)
  })

  # Generate clean question labels for the dropdown
  clean_question_labels <- shiny::reactive({
    shiny::req(survey_data()$data)
    tmp <- colnames(survey_data()$data) |>
      sapply(function(col) {
        if (stringr::str_detect(col, "^Q\\d+ Comments")) {
          return(NA)
        }

        if (stringr::str_detect(col, "^Q\\d+_")) {
          question_number <- stringr::str_extract(col, "^Q\\d+")
          question_text <- stringr::str_extract(
            col,
            "(?<=\\[Question: )(.*?)(?=\\])"
          )
          paste(question_number, question_text)
        } else {
          col
        }
      }) |>
      na.omit() |>
      unname() |>
      unique()

    setdiff(tmp, c("User ID", "Created", "Respondent ID"))
  })

  # Render question selector dropdown
  output$question_selector <- shiny::renderUI({
    shiny::req(clean_question_labels())
    shiny::selectInput(
      "question",
      "Select a Question:",
      choices = clean_question_labels()
    )
  })

  # Render crosstab selector dropdown
  output$crosstab_selector <- shiny::renderUI({
    shiny::req(clean_question_labels())
    shiny::selectInput(
      "crosstab",
      "Crosstab By:",
      choices = c("None", clean_question_labels())
    ) # Add "None" for no crosstab
  })

  # Generate table data on button press
  reactive_table_data <- generate_table_data(input, survey_data)

  # Render the table using the generated data
  output$gt_table <- gt::render_gt({
    shiny::req(reactive_table_data()) # Ensure data exists

    data <- reactive_table_data()$summarized_data
    generate_table(
      data,
      input$question,
      input$crosstab,
      reactive_table_data()$date,
      reactive_table_data()$respondent_count,
      input$confidence_interval,
      reactive_table_data()$tab_n
    )
  })

  # Download handler
  output$download_plot <- shiny::downloadHandler(
    filename = function() {
      paste("survey_plot", Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      ggplot2::ggsave(
        file,
        plot = ggplot2::last_plot(),
        device = "png",
        width = 8,
        height = 6
      )
    }
  )
}

# Run the application
shiny::shinyApp(ui = ui, server = server)
