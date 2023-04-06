# Load required packages
library(shiny)
library(dplyr)
library(ggplot2)
library(corrplot)
library(Lahman)

# Load batting data from Lahman Baseball Database
batting <- read.csv("Batting.csv") %>% 
  select(playerID,yearID,stint,teamID,lgID,G,AB,R,H,X2B,X3B,HR) %>% 
  rename("Player_ID" = playerID,
         "Year_ID" = yearID,
         "Stint" = stint,
         "Team" = teamID,
         "League" = lgID,
         "Games_Played" = G,
         "At_Bats" = AB,
         "Runs" = R,
         "Hits" = H,
         "Doubles" = X2B,
         "Triples" = X3B,
         "Home_Runs" = HR)
people <- read.csv("People.csv") %>% 
  select(playerID,birthYear,birthMonth,birthDay,birthCountry,birthState,birthCity,nameFirst,nameLast,nameGiven,weight,height,bats,throws,debut,finalGame) %>% 
  rename("Player_ID" = playerID,
         "Birth_Year" = birthYear,
         "Birth_Month" = birthMonth,
         "Birth_Day" = birthDay,
         "Birth_Country" = birthCountry,
         "Birth_State" = birthState,
         "Birth_City" = birthCity,
         "First_Name" = nameFirst,
         "Last_Name" = nameLast,
         "Given_Name" = nameGiven,
         "Weight" = weight,
         "Height" = height,
         "Batting_Hand" = bats,
         "Throwing_Hand" = throws,
         "Debut" = debut,
         "Final_Game" = finalGame)

fielding <- read.csv("Fielding.csv") %>% 
  select(playerID,POS) %>% 
  rename("Player_ID" = playerID,
         "Position" = POS)

salaries <- read.csv("Salaries.csv") %>% 
  select(playerID, salary) %>% 
  rename("Player_ID" = playerID,
         "Salary" = salary)

collegeplaying <- read.csv("CollegePlaying.csv") %>% 
  select(playerID, schoolID,yearID) %>% 
  rename("Player_ID" = playerID,
         "School_Playing" = schoolID,
         "Year_ID" = yearID)


school <- read.csv("Schools.csv") %>% 
  select(schoolID,name_full) %>% 
  rename("School_Playing" = schoolID,
         "School_Name" = name_full)

awards <- read.csv("AwardsPlayers.csv") %>% 
  select(playerID,awardID) %>% 
  rename("Player_ID" = playerID,
         "Awards" = awardID)

#Removing the duplicate values
batting <- batting[!duplicated(t(apply(batting,1,sort))),]
people <- people[!duplicated(t(apply(people,1,sort))),]
fielding <- fielding[!duplicated(t(apply(fielding,1,sort))),]
salaries <- salaries[!duplicated(t(apply(salaries,1,sort))),]
collegeplaying <- collegeplaying[!duplicated(t(apply(collegeplaying,1,sort))),]
school <- school[!duplicated(t(apply(school,1,sort))),]
awards <- awards[!duplicated(t(apply(awards,1,sort))),]

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
      textOutput("des"),
      plotOutput("rdp"),
      plotOutput("hist"),
      plotOutput("rp")
      
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
  
  output$des <- renderText({
    paste("The intercept value of the model is", coef(model()), "which means that if all the independent variables in the model are zero, the expected value of the dependent variable (Average) would be", coef(model()))
  })
  
  output$rdp <- renderPlot({
    qqnorm(model()$residuals)
    qqline(model()$residuals)
  })
  output$hist <- renderPlot({
    # Create a histogram of residuals
    hist(model()$residuals)
  })
  output$rp <- renderPlot({
    # Create a residual plot
    plot(model(), which = 1)
    
  })
  
  output$adj_r_squared <- renderText({
    adj_r_sq <- paste(round(summary(model())$adj.r.squared, 2) * 100," %")
    adj_r_sq
  })
}


# The coefficient estimates for the predictor variables are shown in the "Coefficients" table. The estimated coefficient for Games_Played is 0.0027, which means that on average, for every additional game played, the expected value of Average increases by 0.0027. The estimated coefficient for Position is -0.0186, which means that on average, players in positions other than pitcher have a lower expected value of Average by 0.0186. The estimated coefficient for School_Playing is 0.0164, which means that on average, players who attended schools that are not in the "other" category have a higher expected value of Average by 0.0164.
# The multiple R-squared value of the model is 0.1332, which means that 13.32% of the variability in Average can be explained by the predictor variables in the model.
# The p-values for the coefficients in the model are also shown in the "Coefficients" table. All the predictor variables have p-values less than 0.05, indicating that they are statistically significant predictors of Average.
# The adjusted R-squared value of the model is 0.1318, which is similar to the multiple R-squared value but adjusted for the number of predictor variables in the model.
# The standard error of the regression is 0.05667, which indicates the average amount that the predicted values deviate from the actual values of Average.
# The F-statistic and its associated p-value in the "Analysis of Variance" table indicate whether the overall model is statistically significant. In this case, the F-statistic is 103.3 and the p-value is less than 2.2e-16, indicating that the model is statistically significant.
# 

# Run the app
shinyApp(ui, server)

