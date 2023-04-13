library(shiny)
library(dplyr)
library(ggplot2)

# Define UI
ui <- fluidPage(
  titlePanel("Regression Analysis"),
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "x_var", label = "Select Predictor Variable:", choices = names(data)[-c(1, ncol(data))]),
      selectInput(inputId = "y_var", label = "Select Response Variable:", choices = names(data)[ncol(data)])
    ),
    mainPanel(
      plotOutput(outputId = "plot"),
      h4("Model Summary:"),
      verbatimTextOutput(outputId = "model_summary"),
      h4("Predictions:"),
      tableOutput(outputId = "predictions")
    )
  )
)

# Define server
server <- function(input, output) {
  
  # Reactive data
  data <- reactive({
    # Load your data here
    data <- read.csv("data.csv")
    # Remove any rows with missing values
    data <- na.omit(data)
    # Return the filtered data
    return(data)
  })
  
  # Perform linear regression
  model <- reactive({
    lm(as.formula(paste(input$y_var, "~", input$x_var)), data())
  })
  
  # Generate plot
  output$plot <- renderPlot({
    ggplot(data(), aes_string(x = input$x_var, y = input$y_var)) +
      geom_point() +
      geom_smooth(method = "lm", se = FALSE)
  })
  
  # Generate model summary
  output$model_summary <- renderPrint({
    summary(model())
  })
  
  # Generate predictions
  output$predictions <- renderTable({
    new_data <- data() %>% 
      select(-matches(input$y_var))
    new_data$predicted <- predict(model(), newdata = new_data)
    new_data
  })
  
}

# Run the app
shinyApp(ui = ui, server = server)