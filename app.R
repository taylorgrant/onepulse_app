pacman::p_load(tidyverse, readxl, shiny, gt, tidyr, glue, bslib)

# Helper functions in R folder
# proptest_dataframe()
# generate_table()
# process_survey_data()
# generate_table_data()


# UI ----------------------------------------------------------------------

# Define UI for the application
ui <- fluidPage(
  theme = bs_theme(version = 4, bootswatch = "flatly"), # Choose a modern Bootstrap theme
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
  ),
  
  titlePanel("OnePulse Survey Summary"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file_upload", "Upload Dataset:", accept = c(".xlsx", ".csv")),
      uiOutput("question_selector"),
      uiOutput("crosstab_selector"),
      radioButtons(
        "confidence_interval",
        label = "Select Confidence Interval",
        choices = c("80%" = 0.8, "90%" = 0.9, "95%" = 0.95),
        selected = 0.9
      ),
      actionButton("generate_table", "Generate Table", class = "btn-primary"),  # Styled button
      hr(),
      uiOutput("capture_button")
    ),
    mainPanel(
      div(
        gt_output("gt_table"),
        class = "no-scroll"  # custom CSS hook
      ),
      div(style = "margin-top: 40px;")
    )
  )
)



# SERVER ------------------------------------------------------------------

# Define server logic
server <- function(input, output, session) {
  
  # Dynamically provide the UI for table capture 
  output$capture_button <- renderUI({
    req(input$question)
    
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
      
      base_filename <- paste0("table_", question_id, "_by_", crosstab_id, ".png")
    } else {
      base_filename <- paste0("table_", question_id, ".png")
    }
    
    capture::capture(
      id = "capture-container",
      selector = "#gt_table",
      filename = base_filename,
      icon("download"),
      "Download Table"
    )
  })
  
  # Likert scales are located in `generate_table_data.R`
  
  # Reactive to load uploaded dataset
  survey_data <- reactive({
    process_survey_data(input$file_upload)
  })
  
  # Generate clean question labels for the dropdown
  clean_question_labels <- reactive({
    req(survey_data()$data)
    tmp <- colnames(survey_data()$data) |>  
      sapply(function(col) {
        if (str_detect(col, "^Q\\d+ Comments")) return(NA)
        
        if (str_detect(col, "^Q\\d+_")) {
          
          question_number <- str_extract(col, "^Q\\d+")
          question_text <- str_extract(col, "(?<=\\[Question: )(.*?)(?=\\])")
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
  output$question_selector <- renderUI({
    req(clean_question_labels())
    selectInput("question", "Select a Question:", choices = clean_question_labels())
  })
  
  # Render crosstab selector dropdown
  output$crosstab_selector <- renderUI({
    req(clean_question_labels())
    selectInput("crosstab", "Crosstab By:", choices = c("None", clean_question_labels()))  # Add "None" for no crosstab
  })
  
  # Generate table data on button press
  reactive_table_data <- generate_table_data(input, survey_data)
  
  # Render the table using the generated data
  output$gt_table <- render_gt({
    req(reactive_table_data())  # Ensure data exists
    
    data <- reactive_table_data()$summarized_data
    generate_table(data, input$question, input$crosstab, reactive_table_data()$date, 
                   reactive_table_data()$respondent_count, input$confidence_interval, 
                   reactive_table_data()$tab_n)
  })
  
  
  # Download handler 
  output$download_plot <- downloadHandler(
    filename = function() {
      paste("survey_plot", Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      ggsave(file, plot = last_plot(), device = "png", width = 8, height = 6)
    }
  )
}

# Run the application
shinyApp(ui = ui, server = server)
