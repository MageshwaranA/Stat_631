# Load necessary packages
library(dplyr)
library(ggplot2)
library(corrplot)
library(Lahman)
library(purrr)
library(olsrr) # For the forward selection model

# Load batting data from Lahman Baseball Database
batting <- read.csv("~/Stat_631/Dataset/Batting.csv") %>% 
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
people <- read.csv("~/Stat_631/Dataset/People.csv") %>% 
  select(playerID,birthYear,birthMonth,birthDay,birthCountry,birthState,birthCity,nameFirst,nameLast,nameGiven,weight,height,bats,throws,debut,finalGame) %>% 
  rename("Player_ID" = playerID,
         "Birth_Year" = birthYear,
         "Birth_Month" = birthMonth,
         "Birth_Day" = birthDay,
         "Birth_Country" = birthCountry,
         "Birth_State" = birthState,
          "Birth_City" = birthCity,
         # "First_Name" = nameFirst,
         # "Last_Name" = nameLast,
         "Given_Name" = nameGiven,
         "Weight" = weight,
         "Height" = height,
         "Batting_Hand" = bats,
         "Throwing_Hand" = throws,
         "Debut" = debut,
         "Final_Game" = finalGame)

fielding <- read.csv("~/Stat_631/Dataset/Fielding.csv") %>% 
  select(playerID,POS) %>% 
  rename("Player_ID" = playerID,
         "Position" = POS)

salaries <- read.csv("~/Stat_631/Dataset/Salaries.csv") %>% 
  select(playerID, salary) %>% 
  rename("Player_ID" = playerID,
         "Salary" = salary)

collegeplaying <- read.csv("~/Stat_631/Dataset/CollegePlaying.csv") %>% 
  select(playerID, schoolID,yearID) %>% 
  rename("Player_ID" = playerID,
         "School_Playing" = schoolID,
         "Year_ID" = yearID)


school <- read.csv("~/Stat_631/Dataset/Schools.csv") %>% 
  select(schoolID,name_full) %>% 
  rename("School_Playing" = schoolID,
         "School_Name" = name_full)

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
school <- distinct(school)
awards <- distinct(awards)

#Merging all dataset to get one final dataset
# final_data <- merge(batting, people, by = "Player_ID")
# final_data <- merge(final_data, fielding, by = "Player_ID")
# final_data <- merge(final_data, salaries , by = "Player_ID")
# final_data <- merge(final_data, collegeplaying, by = "Player_ID")
# final_data <- merge(final_data, school, by = "School_Playing")
# final_data <- merge(final_data, awards, by = "Player_ID")

final_data <- batting %>%
  left_join(people, by = "Player_ID",relationship = "many-to-many") %>%
  left_join(fielding, by = "Player_ID",relationship = "many-to-many") %>%
  left_join(salaries, by = "Player_ID",relationship = "many-to-many") %>%
  left_join(collegeplaying, by = "Player_ID",relationship = "many-to-many") %>%
  left_join(school, by = "School_Playing",relationship = "many-to-many") %>%
  left_join(awards, by = "Player_ID",relationship = "many-to-many")


# Create new variable for batting average
final_data$Average <- ifelse(is.nan(final_data$Hits / final_data$At_Bats),0,final_data$Hits / final_data$At_Bats)

# Remove missing data
final_data <- na.omit(final_data)


#Create a new variable for age
#final_data$Current_Age <- as.integer((Sys.Date() - as.Date(paste(final_data$Birth_Year, final_data$Birth_Month, final_data$Birth_Day, sep = "-"))) / 365.25)
final_data$Debut_Age <- as.integer((Sys.Date() - as.Date(final_data$Debut)) / 365.25)
#final_data$FinalGame_Age <- as.integer((Sys.Date() - as.Date(final_data$Final_Game)) / 365.25)

final_data$Player_ID <- as.factor(final_data$Player_ID)
final_data$Team <- as.factor(final_data$Team)
final_data$League <- as.factor(final_data$League)
final_data$Birth_Country <- as.factor(final_data$Birth_Country)
final_data$Birth_State <- as.factor(final_data$Birth_State)
final_data$Birth_City <- as.factor(final_data$Birth_City)
final_data$Given_Name <- as.factor(final_data$Given_Name)
final_data$Batting_Hand <- as.factor(final_data$Batting_Hand)
final_data$Throwing_Hand <- as.factor(final_data$Throwing_Hand)
final_data$Debut <- as.factor(final_data$Debut)
final_data$Final_Game <- as.factor(final_data$Final_Game)
final_data$Position <- as.factor(final_data$Position)
final_data$School_Playing <- as.factor(final_data$School_Playing)
final_data$School_Name <- as.factor(final_data$School_Name)
final_data$Awards <- as.factor(final_data$Awards)

model <- lm(Average ~ Stint+Team+League+Birth_Year+Birth_Month+Birth_Day+Birth_Country+Birth_State+Birth_City+Given_Name+Weight+Height+Batting_Hand+Throwing_Hand+Debut+Position+Salary+School_Playing+School_Name+Awards+Debut_Age, data = final_data)
ols_step_best_subset(model)
deviance(model)
summary(model)$r.squared






























# #Create a model
# model <- lm(Average ~ Games_Played + Position + School_Playing, data = final_data %>% filter(At_Bats > 30))
# # Print summary of model results
# summary(model)
# 
# # Create scatterplot of predicted versus actual batting average
# predicted <- predict(model, newdata = final_data %>% filter(At_Bats > 30))
# ggplot(final_data %>% filter(At_Bats > 30), aes(x = Average, y = predicted)) +
#   geom_point() +
#   geom_abline(intercept = 0, slope = 1, color = "red") +
#   labs(x = "Actual Batting Average", y = "Predicted Batting Average")
# 
# # # Calculate cross-validated prediction error
# # library(caret)
# # set.seed(123)
# # cv <- trainControl(method = "cv", number = 10, verboseIter = TRUE)
# # model_cv <- train(Average ~ Games_Played + At_Bats + Hits, data = final_data %>% filter(At_Bats > 30), method = "lm", trControl = cv)
# # model_cv$results$RMSE
# 
# train_idx <- sample(nrow(final_data), round(0.5*nrow(final_data)))
# train_data <- final_data[train_idx, ]
# 
# 
# # Random Sampling
# 
# Sampled_data <- final_data[sample(nrow(final_data), 100), ]
# model <- lm(Average ~., data=Sampled_data)
# Sampled_data <- Sampled_data %>% 
#   select(Average,Stint,Team,League,Games_Played,Birth_Year,Birth_Month,Birth_Day,Birth_Country,Birth_State,Birth_City,Weight,Height,Batting_Hand,Throwing_Hand,Position,School_Playing,School_Name,Awards,Debut_Age)
# # Fit the initial linear regression model
# model <- lm(Average ~., data=Sampled_data)
# 
# # Perform backward selection using the step function
# selected_model <- step(model, direction="backward")
# 
# # Print the selected model summary
# summary(selected_model)
# 
# 
# # Define a function to perform backward selection
# backward_select <- function(model) {
#   while (TRUE) {
#     pvals <- summary(model)$coefficients[-1, "Pr(>|t|)"]
#     max_pval <- max(pvals)
#     if (max_pval > 0.05) {
#       var_to_remove <- names(which.max(pvals))
#       formula <- formula(paste("Average ~", paste(names(pvals)[-match(var_to_remove, names(pvals))], collapse="+")))
#       model <- lm(formula, data=Sampled_data)
#     } else {
#       break
#     }
#   }
#   return(model)
# }
# 
# # Perform backward selection
# selected_model <- backward_select(model)
# 
# # Print the selected model summary
# summary(selected_model)
# model <- lm(Average ~ Stint,final_data)
# 
# 
# library(caret)
# train_control <- trainControl(method = "cv", number = 10)
# lm_fit <- train(Average ~ Games_Played + Position + School_Playing, data = final_data, method = "lm", trControl = train_control)
# 
# 
# library(caret)
# set.seed(123)
# train_index <- createDataPartition(final_data$Average, p = 0.1, list = FALSE)
# train_data <- final_data[train_index, ]
# test_data <- final_data[-train_index, ]
# full_model <- train(Average ~ ., data = train_data, method = "lm")
# full_model_pred <- predict(full_model, newdata = test_data)
# full_model_mse <- mean((full_model_pred - test_data$target_variable)^2)
# 
