# Load necessary packages
library(dplyr)
library(ggplot2)
library(corrplot)
library(Lahman)
library(purrr)
library(olsrr) # For the forward selection model

# Load batting data from Lahman Baseball Database
g_batting <- read.csv("~/Stat_631/Dataset/Batting.csv") %>% 
  select(playerID,yearID,stint,teamID,lgID,G,AB,R,H,X2B,X3B,HR) %>% 
  rename("Player_ID" = playerID,
         "Team" = teamID,
         "League" = lgID,
         "Games_Played" = G,
         "At_Bats" = AB,
         "Runs" = R,
         "Hits" = H,
         "Doubles" = X2B,
         "Triples" = X3B,
         "Home_Runs" = HR)


# Group by player_id and team
batting <- g_batting %>% 
  group_by(Player_ID,Team) %>% 
  summarise(Games_Played = sum(Games_Played),
            At_Bats = sum(At_Bats),
            Runs = sum(Runs),
            Hits = sum(Hits),
            Double = sum(Doubles),
            Triples = sum(Triples),
            Home_Runs = sum(Home_Runs))


people <- read.csv("~/Stat_631/Dataset/People.csv") %>% 
  select(playerID,birthCountry,weight,height,bats,throws) %>% 
  rename("Player_ID" = playerID,
         "Birth_Country" = birthCountry,
         "Weight" = weight,
         "Height" = height,
         "Batting_Hand" = bats,
         "Throwing_Hand" = throws)

fielding <- read.csv("~/Stat_631/Dataset/Fielding.csv") %>% 
  select(playerID,POS,teamID) %>% 
  rename("Player_ID" = playerID,
         "Position" = POS,
         "Team" = teamID)

g_salaries <- read.csv("~/Stat_631/Dataset/Salaries.csv") %>% 
  select(playerID, salary,teamID) %>% 
  rename("Player_ID" = playerID,
         "Salary" = salary,
         "Team" = teamID)

# Group by salary and team
salaries <- g_salaries %>% 
  group_by(Player_ID,Team) %>% 
  summarise(Salary = sum(Salary))


collegeplaying <- read.csv("~/Stat_631/Dataset/CollegePlaying.csv") %>% 
  select(playerID, schoolID) %>% 
  rename("Player_ID" = playerID,
         "School_Playing" = schoolID)


awards <- read.csv("~/Stat_631/Dataset/AwardsPlayers.csv") %>% 
  select(playerID,awardID) %>% 
  rename("Player_ID" = playerID,
         "Awards" = awardID)

#Removing the duplicate values
# batting <- batting[!duplicated(t(apply(batting,1,sort))),]
# people <- people[!duplicated(t(apply(people,1,sort))),]
# fielding <- fielding[!duplicated(t(apply(fielding,1,sort))),]
# salaries <- salaries[!duplicated(t(apply(salaries,1,sort))),]
# collegeplaying <- collegeplaying[!duplicated(t(apply(collegeplaying,1,sort))),]
# school <- school[!duplicated(t(apply(school,1,sort))),]
# awards <- awards[!duplicated(t(apply(awards,1,sort))),]

batting <- distinct(batting)
people <- distinct(people)
fielding <- distinct(fielding)
salaries <- distinct(salaries)
collegeplaying <- distinct(collegeplaying)
awards <- distinct(awards)

#Merging all dataset to get one final dataset
final_data <- merge(batting, people, by = "Player_ID",all = FALSE)
final_data <- merge(final_data, fielding, by = c("Player_ID","Team"),all = FALSE)
final_data <- merge(final_data, salaries , by = c("Player_ID","Team"),all = FALSE)
final_data <- merge(final_data, collegeplaying, by = "Player_ID",all = FALSE)
final_data <- merge(final_data, awards, by = "Player_ID",all = FALSE)

# final_data <- batting %>%
#   left_join(people, by = "Player_ID",relationship = "many-to-many") %>%
#   left_join(fielding, by = "Player_ID",relationship = "many-to-many") %>%
#   left_join(salaries, by = "Player_ID",relationship = "many-to-many") %>%
#   left_join(collegeplaying, by = "Player_ID",relationship = "many-to-many") %>%
#   left_join(school, by = "School_Playing",relationship = "many-to-many") %>%
#   left_join(awards, by = "Player_ID",relationship = "many-to-many")


# Create new variable for batting average
final_data$Average <- ifelse(is.nan(final_data$Hits / final_data$At_Bats),0,final_data$Hits / final_data$At_Bats)
final_data <- subset(final_data,select = -c(At_Bats,Hits,Player_ID))
# Remove missing data
final_data <- na.omit(final_data)

# Define user interface
ui <- fluidPage(
  titlePanel("Influential Factors on Baseball Batting Average"),
  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput("options", "Select Options:",
                         choices = c("Games_Played",
                                     "Team",
                                     "Runs",
                                     "Double",
                                     "Triples",
                                     "Home_Runs",
                                     "Birth_Country",
                                     "Weight",
                                     "Height",
                                     "Batting_Hand",
                                     "Throwing_Hand",
                                     "Position",
                                     "Salary",
                                     "School_Playing",
                                     "Awards"),
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
      plotOutput("correlationplot"),
      textOutput("des"),
      plotOutput("outliers"),
      textOutput("descoutliers"),
      textOutput("backwarddesc")
      
    )
  )
)

# Define server logic
server <- function(input, output) {
  
  independent_vars <- reactive({
    input$options
  })
  
  model <- reactive({
    lm(Average ~ ., data = final_data[, c("Average", independent_vars())])
  })
  
  t <- reactive({
    4/nrow(final_data)
  })
  
  cooks_distance <- reactive({
    cooks.distance(model())
  })
  
  clean_pd <- reactive({
    final_data[-c(which(cooks_distance()>t())),]
  })
  
  selected_model <- reactive({
    step(lm(Average~., data=clean_pd()), direction="backward")
  })
  
  output$selectedoptions <- renderPrint({
    paste("You have selected the following options:", paste(input$options, collapse = "+ "))
  })
  
  output$correlationplot <- renderPlot({

    numeric_data <- final_data %>% 
      select_if(is.numeric)
    
    # Plot the correlation matrix
    
    corrplot(cor(numeric_data), method = "circle")
  })
  
  
  output$des <- renderText({
    paste(round(summary(model())$adj.r.squared,2))
  })
  

  output$outliers <- renderPlot({
    plot(cooks_distance())
  })
  output$descoutliers <- renderText({
    paste(round(summary(lm(Average~., data=clean_pd()))$adj.r.squared,2))
  })
  
  output$backwarddesc <- renderText({
    # Create a residual plot
    paste(round(summary(selected_model())$adj.r.squared,2))
  })
  output$adj_r_squared <- renderText({
    paste(round(summary(selected_model())$adj.r.squared,0),"%")
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

