# Load required packages
library(shiny)
library(dplyr)
library(ggplot2)
library(corrplot)
library(Lahman)
library(data.table)

# Load batting data from Lahman Baseball Database
batting <- fread("Batting.csv", select = c("playerID", "yearID", "stint", "teamID", "lgID", "G", "AB", "R", "H", "2B", "3B", "HR"))
setnames(batting, old = c("playerID", "yearID", "stint", "teamID", "lgID", "G", "AB", "R", "H", "2B", "3B", "HR"), 
         new = c("Player_ID", "Year_ID", "Stint", "Team", "League", "Games_Played", "At_Bats", "Runs", "Hits", "Doubles", "Triples", "Home_Runs"))

people <- fread("People.csv", select = c("playerID", "birthYear", "birthMonth", "birthDay", "birthCountry", "birthState", "birthCity", "nameFirst", "nameLast", "nameGiven", "weight", "height", "bats", "throws", "debut", "finalGame"))
setnames(people, old = c("playerID", "birthYear", "birthMonth", "birthDay", "birthCountry", "birthState", "birthCity", "nameFirst", "nameLast", "nameGiven", "weight", "height", "bats", "throws", "debut", "finalGame"), 
         new = c("Player_ID", "Birth_Year", "Birth_Month", "Birth_Day", "Birth_Country", "Birth_State", "Birth_City", "First_Name", "Last_Name", "Given_Name", "Weight", "Height", "Batting_Hand", "Throwing_Hand", "Debut", "Final_Game"))

fielding <- fread("Fielding.csv", select = c("playerID", "POS"))
setnames(fielding, old = c("playerID", "POS"), new = c("Player_ID", "Position"))

salaries <- fread("Salaries.csv", select = c("playerID", "salary"))
setnames(salaries, old = c("playerID", "salary"), new = c("Player_ID", "Salary"))

collegeplaying <- fread("CollegePlaying.csv" , select = c("playerID","schoolID","yearID"))
setnames(collegeplaying,old = c("playerID","schoolID","yearID"), new = c("Player_ID","School_Playing","Year_ID"))

school <- fread("Schools.csv",select = c("schoolID","name_full"))
setnames(school,old = c("schoolID","name_full"),new = c("School_Playing","School_Name"))

awards <- fread("AwardsPlayers.csv", select = c("playerID","awardID"))
setnames(awards,old = c("playerID","awardID"), new = c("Player_ID","Awards"))


setDT(batting)
setDT(people)
setDT(fielding)
setDT(salaries)
setDT(collegeplaying)
setDT(school)
setDT(awards)

batting <- unique(batting, by = c("Player_ID", names(batting)[-1]))
people <- unique(people, by = c("Player_ID", names(people)[-1]))
fielding <- unique(fielding, by = c("Player_ID", names(fielding)[-1]))
salaries <- unique(salaries, by = c("Player_ID", names(salaries)[-1]))
collegeplaying <- unique(collegeplaying, by = c("Player_ID", names(collegeplaying)[-1]))
school <- unique(school, by = c("School_Playing", names(school)[-1]))
awards <- unique(awards, by = c("Player_ID", names(awards)[-1]))


#Merging all dataset to get one final dataset
final_data <- merge(batting, people, by = "Player_ID")
final_data <- merge(final_data, fielding, by = "Player_ID")
final_data <- merge(final_data, salaries , by = "Player_ID")
final_data <- merge(final_data, collegeplaying, by = "Player_ID")
final_data <- merge(final_data, school, by = "School_Playing")
final_data <- merge(final_data, awards, by = "Player_ID")


# Create new variable for batting average
final_data$Average <- ifelse(is.nan(final_data$Hits / final_data$At_Bats),0,final_data$Hits / final_data$At_Bats)

# Remove missing data
final_data <- na.omit(final_data)


#Create a new variable for age
final_data$Current_Age <- as.integer((Sys.Date() - as.Date(paste(final_data$Birth_Year, final_data$Birth_Month, final_data$Birth_Day, sep = "-"))) / 365.25)
final_data$Debut_Age <- as.integer((Sys.Date() - as.Date(final_data$Debut)) / 365.25)
final_data$FinalGame_Age <- as.integer((Sys.Date() - as.Date(final_data$Final_Game)) / 365.25)


# Define user interface
ui <- fluidPage(
  titlePanel("Influential Factors on Baseball Batting Average"),
  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput("options", "Select Options:",
                         choices = c("Games_Played", "Position", "School_Playing"),
                         selected = c("Games_Played")),
      tags$div(
        style = "font-size: 24px;",
        "Adjusted R-squared:",
        textOutput("adj_r_squared")
      )
    ),
    mainPanel(
      h4("Multiple Linear Regression Model:"),
      verbatimTextOutput("selectedoptions"),
      plotOutput("mlr"),
      plotOutput("rdp")
    )
  )
)

# Define server logic
server <- function(input, output) {
  filtered_data <- reactive({
    final_data %>% filter(At_Bats > 30)
  })
  
  independent_vars <- reactive({
    input$options
  })
  
  model <- reactive({
    lm(Average ~ ., data = filtered_data()[, c("Average", independent_vars())])
  })
  
  output$selectedoptions <- renderPrint({
    paste("You have selected the following options:", paste(input$options, collapse = "+ "))
  })
  
  output$mlr <- renderPlot({
    predicted <- predict(model(), newdata = filtered_data())
    ggplot(filtered_data(), aes(x = Average, y = predicted)) +
      geom_point() +
      geom_abline(intercept = 0, slope = 1, color = "red") +
      labs(x = "Actual Batting Average", y = "Predicted Batting Average")
  })
  
  output$rdp <- renderPlot({
    qqnorm(model()$residuals)
    qqline(model()$residuals)
  })
  
  output$adj_r_squared <- renderText({
    adj_r_sq <- round(summary(model())$adj.r.squared, 2) * 100
    adj_r_sq
  })
}


# Run the app
shinyApp(ui, server)

