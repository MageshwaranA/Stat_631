# Load necessary packages
library(dplyr)
library(ggplot2)
library(corrplot)
library(Lahman)
library(purrr)
library(olsrr)# For the forward selection model
library(shiny)
library(shinycssloaders)

# Load batting data from Lahman Baseball Database
g_batting <- read.csv("Dataset/Batting.csv") %>% 
  select(playerID,yearID,stint,teamID,lgID,G,AB,R,H,X2B,X3B,HR,SB,BB,SO) %>% 
  rename("Player_ID" = playerID,
         "Team" = teamID,
         "League" = lgID,
         "Games_Played" = G,
         "At_Bats" = AB,
         "Runs" = R,
         "Hits" = H,
         "Doubles" = X2B,
         "Triples" = X3B,
         "Home_Runs" = HR,
         "Stolen_Base" = SB,
         "Walks" = BB,
         "Strike_Outs" = SO)


# Group by player_id and team
batting <- g_batting %>% 
  group_by(Player_ID,Team) %>% 
  summarise(Games_Played = sum(Games_Played),
            At_Bats = sum(At_Bats),
            Runs = sum(Runs),
            Hits = sum(Hits),
            Double = sum(Doubles),
            Triples = sum(Triples),
            Home_Runs = sum(Home_Runs),
            Walks = sum(Walks),
            Strike_Outs = sum(Strike_Outs),
            Stolen_Base = sum(Stolen_Base))


people <- read.csv("Dataset/People.csv") %>% 
  select(playerID,birthCountry,weight,height,bats,throws) %>% 
  rename("Player_ID" = playerID,
         "Birth_Country" = birthCountry,
         "Weight" = weight,
         "Height" = height,
         "Batting_Hand" = bats,
         "Throwing_Hand" = throws)

fielding <- read.csv("Dataset/Fielding.csv") %>% 
  select(playerID,POS,teamID) %>% 
  rename("Player_ID" = playerID,
         "Position" = POS,
         "Team" = teamID)

g_salaries <- read.csv("Dataset/Salaries.csv") %>% 
  select(playerID, salary,teamID) %>% 
  rename("Player_ID" = playerID,
         "Salary" = salary,
         "Team" = teamID)

# Group by salary and team
salaries <- g_salaries %>% 
  group_by(Player_ID,Team) %>% 
  summarise(Salary = sum(Salary))


collegeplaying <- read.csv("Dataset/CollegePlaying.csv") %>% 
  select(playerID, schoolID) %>% 
  rename("Player_ID" = playerID,
         "School_Playing" = schoolID)


awards <- read.csv("Dataset/AwardsPlayers.csv") %>% 
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
final_data1 <- final_data
final_data <- subset(final_data,select = -c(Height,Weight))
numeric_data <- final_data1 %>% 
  select_if(is.numeric)

################################################################################
#TYLER MODEL
mlr_model_full <- lm(Average ~ Team + Games_Played + Runs + Double + Walks + Strike_Outs + 
                       Stolen_Base + Birth_Country + Batting_Hand + Throwing_Hand + 
                       Position + Salary + School_Playing + Awards, data = final_data)


log_mlr_model_full <- lm(Average ~ Team + log(Games_Played) + Runs + Double + Walks + Strike_Outs + 
                           Stolen_Base + Birth_Country + Batting_Hand + Throwing_Hand + 
                           Position + log(Salary) + School_Playing + Awards, data = final_data)


mlr_model <- lm(Average ~ Team + log(Games_Played) + Birth_Country + Batting_Hand + Throwing_Hand + 
                  Position + log(Salary) + Awards + School_Playing, data = final_data)


team_position_model <- lm(Average ~ Team + Position, data = final_data)


games_model <- lm(Average ~ Games_Played, data = final_data)


position_model <- lm(Average ~ Position, data = final_data)


games_position_model <- lm(Average ~ Games_Played + Position, data = final_data)


# Use this instead of games_position_model
log_games_position_model <- lm(Average ~ log(Games_Played) + Position, data = final_data)
# summary(log_games_position_model)

salary_model <- lm(Average ~ Salary, data = final_data)
# summary(salary_model)


log_salary_model <- lm(Average ~ log(Salary), data = final_data)
# summary(log_salary_model)

games_position_salary_model <- lm(Average ~ log(Games_Played) + Salary + Position, data = final_data)
# summary(games_position_salary_model)

# Salary doesn't add enough to the model. Remove salary.
log_games_position_salary_model <- lm(Average ~ log(Games_Played) + log(Salary) + Position, data = final_data)
# summary(log_games_position_salary_model)

# With Team (Use this model)
games_position_team_model <- lm(Average ~ log(Games_Played) + Position + Team, data = final_data)
# summary(games_position_team_model)

# Batting_Hand Model
batting_hand_model <- lm(Average ~ Batting_Hand, data = final_data)
# summary(batting_hand_model)

# With Batting_Hand (not good enough)
games_position_team_hand_model <- lm(Average ~ log(Games_Played) + Position + Team + Batting_Hand, data = final_data)
# summary(games_position_team_hand_model)

# With School_Playing - won't work due to overfitting.
games_position_team_school_model <- lm(Average ~ log(Games_Played) + Position + Team + School_Playing, data = final_data)
# summary(games_position_team_school_model)

TypesOfModel <- c("MLR Full Model","Log MLR Full Model","MLR Model",
          "team_position_model","games_model","position_model","games_position_model",
          "log_games_position_model","salary_model","log_salary_model","games_position_salary_model",
          "log_games_position_salary_model","games_position_team_model","batting_hand_model",
          "games_position_team_hand_model","games_position_team_school_model")
Fstatistics <- c(summary(mlr_model_full)$fstatistic[1],
                 summary(log_mlr_model_full)$fstatistic[1],
                 summary(mlr_model)$fstatistic[1],
                 summary(team_position_model)$fstatistic[1],
                 summary(games_model)$fstatistic[1],
                 summary(position_model)$fstatistic[1],
                 summary(games_position_model)$fstatistic[1],
                 summary(log_games_position_model)$fstatistic[1],
                 summary(salary_model)$fstatistic[1],
                 summary(log_salary_model)$fstatistic[1],
                 summary(games_position_salary_model)$fstatistic[1],
                 summary(log_games_position_salary_model)$fstatistic[1],
                 summary(games_position_team_model)$fstatistic[1],
                 summary(batting_hand_model)$fstatistic[1],
                 summary(games_position_team_hand_model)$fstatistic[1],
                 summary(games_position_team_school_model)$fstatistic[1])

Rsquared <- c(summary(mlr_model_full)$r.squared,
              summary(log_mlr_model_full)$r.squared,
              summary(mlr_model)$r.squared,
              summary(team_position_model)$r.squared,
              summary(games_model)$r.squared,
              summary(position_model)$r.squared,
              summary(games_position_model)$r.squared,
              summary(log_games_position_model)$r.squared,
              summary(salary_model)$r.squared,
              summary(log_salary_model)$r.squared,
              summary(games_position_salary_model)$r.squared,
              summary(log_games_position_salary_model)$r.squared,
              summary(games_position_team_model)$r.squared,
              summary(batting_hand_model)$r.squared,
              summary(games_position_team_hand_model)$r.squared,
              summary(games_position_team_school_model)$r.squared)
table1 <- data.frame(TypesOfModel,Fstatistics,Rsquared)

################################################################################

# Define user interface
ui <- fluidPage(
  titlePanel("Influential Factors on Baseball Batting Average"),
  sidebarLayout(
    sidebarPanel(
      # checkboxGroupInput("options", "Select Options:",
      #                    choices = c("Games_Played",
      #                                "Team",
      #                                "Runs",
      #                                "Double",
      #                                "Triples",
      #                                "Home_Runs",
      #                                "Birth_Country",
      #                                "Weight",
      #                                "Height",
      #                                "Batting_Hand",
      #                                "Throwing_Hand",
      #                                "Position",
      #                                "Salary",
      #                                "School_Playing",
      #                                "Awards"),
      #                    selected = c("Games_Played")),
      tags$div(
        style = "font-size: 24px;",
        "The Predictor variables are as below:"
      ),
      tags$div(
        style = "font-size: 12px;",
        "Games Played, ","Team, ","Runs, ","Double, ",
        "Triples, ","Home Runs, ","Birth Country, ",
        "Weight, ","Height, ","Batting Hand, ","Throwing Hand, ",
        "Position, ","Salary, ","School Playing, ","Awards"),
      tags$div(
        style = "font-size: 24px;",
        "Adjusted R-squared:",
        textOutput("adj_r_squared")
      ),
      radioButtons("options", "Model Selection Type:",
                   c("Automatic" = "auto",
                     "Manual" = "Manual"))
    ),
    mainPanel(
      h4("Multiple Linear Regression Model:"),
      withSpinner(htmlOutput("selectedoptions")),
      withSpinner(plotOutput("correlationplot")),
      withSpinner(htmlOutput("des")),
      withSpinner(plotOutput("beforescatter")),
      withSpinner(htmlOutput("bsdesc")),
      withSpinner(plotOutput("outliers")),
      withSpinner(htmlOutput("outdesc")),
      withSpinner(plotOutput("afterscatter")),
      withSpinner(htmlOutput("descoutliers")),
      withSpinner(dataTableOutput("AIC")),
      withSpinner(htmlOutput("backwarddesc"))
      
    )
  )
)

# Define server logic
server <- function(input, output) {
  
  
    model <- reactive({
      # lm(Average ~ ., data = final_data[, c("Average", independent_vars())])
      lm(Average ~., data = final_data)
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
    
    output$selectedoptions <- renderText({
      if (input$options == "auto"){
        paste("In this report, we will be determining the influential factors for the batting average of baseball players.",br(),br()," First, let's load the <b>Lahman</b> package, <b>dplyr</b> package, <b>ggplot2</b> package, <b>corrplot</b> package, and the <b>olsrr</b> package which we'll use to manipulate the data.",br(),br()," Next, let's load the data and explore it.",br(),br()," The 'batting' dataset has 112,184 observations and 15 variables, the 'people' dataset has 20,676 observations and 6 variables, the 'fielding' dataset has 72,495 observations and 3 variables, the 'salary' dataset has 26,428 observations and 3 variables, the 'college playing' dataset has 7,550 observations and 2 variables, and finally, the 'awards' dataset has 3,122 observations and 2 variables.

",br(),br(),"The datasets we chose contain a record of each team and the years played for every player, so they are grouped together to reduce the overall amount of data. Then, all the datasets are merged to form a final dataset with all observations present. As for the last step, the dataset is checked for NA values and any rows that contain them are omitted.

The batting average for each baseball player is calculated based on the following formula:",br(),br(),
              
              "<b>Average = Hits / At_Bats</b>",br(),br(),
              
              "We'll be using <b>Average</b>, which is the batting average, as our response variable, along with any other factors that could influence the batting average. We'll also select a few variables that we think might be important for the analysis.

We have selected Games Played, Team, Runs, Double, Triples, Home Runs, Walks, Strike Outs, Stolen Base, Birth Country, Weight, Height, Batting Hand, Throwing Hand, Position, Salary, School Playing, Awards. We have left out the Player ID, At bats and hits from the model since they are directly related to the batting average. Also, we wanted to avoid overfitting. 

The numerical values are considered for the correlation matrix since the categorical variables are not supported.",br() 
        )
      }
      # else{
      #   paste(summary(mlr_model_full),br(),br(),
      #         summary(log_mlr_model_full),br(),br(),summary(mlr_model),br(),br(),summary(position_model),br(),br(),
      #         summary(games_position_model),br(),br()
      #         )
      # }
      
    })
    
    output$correlationplot <- renderPlot({
      if(input$options == "auto"){
        corrplot(cor(numeric_data), method = "circle")
      }
      else{
        final_data %>% 
          ggplot(aes(x=Games_Played)) + 
          geom_histogram(bins = 24) + 
          theme_bw()
      }
      
    })
    
    
    output$des <- renderText({
      if(input$options == "auto"){
        paste(br(),"From the correlation matrix, we can see that the variables Games Played, Runs, Doubles, Triples, Home Runs, Walks, Strike Outs, Stolen Bases, and Salary have the highest correlation with the batting average.")
      }
          })
    
    output$beforescatter <- renderPlot({
      if(input$options == "auto"){
        predicted <- predict(model(), newdata = final_data)
        ggplot(final_data, aes(x = Average, y = predicted)) +
          geom_point() +
          geom_abline(intercept = 0, slope = 1, color = "red") +
          labs(x = "Actual Batting Average", y = "Predicted Batting Average")
      }
      else{
        final_data %>% 
          ggplot(aes(x=log(Games_Played))) + 
          geom_histogram(bins = 24) + 
          theme_bw()
      }
      
    })
    
    output$bsdesc <- renderText({
      if(input$options == "auto"){
        paste(br(),"The scatterplot shows a weak positive relationship between the variables, with some scattered points but no clear pattern. There appear to be a few outliers at the upper end of the x-axis, with some extreme values that are far from the general trend of the data.",
              br(),"The adjusted R Square value for the model with outliers is<b>",round(summary(model())$adj.r.squared * 100,2),"</b>")
      }
       })
    
    output$outliers <- renderPlot({
      if(input$options == "auto"){
        plot(cooks_distance())
      }
      else{
        final_data %>% 
          ggplot(aes(x=log(Salary))) + 
          geom_histogram(bins = 24) + 
          theme_bw()
      }
    })
    
    
    output$outdesc <- renderText({
      if(input$options == "auto"){
        paste(br(),"The Cook's distance plot for the multiple linear regression model is a graphical representation of the influence of each observation on the model. Each point on the plot represents an observation and the Cook's distance value for that observation. The plot is useful in identifying outliers that may be having a significant impact on the model.")
      }
      
    # else{
    #   paste(summary(log_games_position_model),br(),br(),summary(salary_model),br(),br(),summary(log_salary_model),br(),br(),summary(games_position_salary_model),br(),br(),summary(log_games_position_salary_model),br(),br(),summary(games_position_team_model),br(),br(),summary(batting_hand_model),br(),br(),summary(games_position_team_hand_model),br(),br(),summary(games_position_team_school_model))
    # }
      })
    
    output$afterscatter <- renderPlot({
      if(input$options == "auto"){
        predicted <- predict(model(), newdata = clean_pd())
        ggplot(clean_pd(), aes(x = Average, y = predicted)) +
          geom_point() +
          geom_abline(intercept = 0, slope = 1, color = "red") +
          labs(x = "Actual Batting Average", y = "Predicted Batting Average")
      }
    })
    
    output$descoutliers <- renderText({
      if(input$options == "auto"){
        paste(br(),"The scatterplot shows a stronger positive relationship between the variables, with most points closely following the trend. The outliers at the upper end of the x-axis have been removed, and the remaining data points form a more tightly clustered pattern. The correlation coefficient has increased, indicating a stronger linear relationship between the variables."
              ,"The Adjusted R squared value of the modl after outliers being removed is:<b>",round(summary(lm(Average~., data=clean_pd()))$adj.r.squared * 100,2),",</b>",
              br(),br(),"<b> Backward Selection for determining the best model</b>",
              br(),br(),"The backward selected model is:",
              br(),"Team, Games_Played, Runs, Double, Walks, Strike_Outs, 
    Stolen_Base, Birth_Country, Batting_Hand, Throwing_Hand, 
    Position, Salary, School_Playing, Awards")
      }
    })
    
    output$AIC <- renderDataTable({
      if(input$options == "auto"){
        Model <- c("Model 1","Model 2","Model 3")
        AIC <- c(selected_model()$anova$AIC[[1]],selected_model()$anova$AIC[[2]],selected_model()$anova$AIC[[3]])
        table <- data.frame(Model,AIC)
        table
      }
      else{
        table1
      }
    })
    output$backwarddesc <- renderText({
      if(input$options == "auto"){
        # Create a residual plot
        paste("The adjusted R Square value of the Backward Selected Model is: <b>",round(summary(selected_model())$adj.r.squared * 100,2),"</b>",
              br(),"The F-statistic and its associated p-value indicate whether the overall model is statistically significant. In this case, the F-statistic is <b>",
              summary(model())$fstatistic[1],"</b> indicating that the model is statistically significant.")
      }
      })
    output$adj_r_squared <- renderText({
      if(input$options == "auto"){
        paste(round(summary(selected_model())$adj.r.squared * 100,0),"%")
      }
      else{
        paste(round(summary(log_mlr_model_full)$r.squared * 100,0),"%")
      }
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

