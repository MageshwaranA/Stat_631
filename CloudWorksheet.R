# Load necessary packages
library(dplyr)
library(ggplot2)
library(corrplot)
library(Lahman)
library(purrr)
library(olsrr) # For the forward selection model

# Load batting data from Lahman Baseball Database
g_batting <- read.csv("~/Masters Project/Stat_631/Dataset/Batting.csv") %>% 
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


people <- read.csv("~/Masters Project/Stat_631/Dataset/People.csv") %>% 
  select(playerID,birthCountry,weight,height,bats,throws) %>% 
  rename("Player_ID" = playerID,
         "Birth_Country" = birthCountry,
         "Weight" = weight,
         "Height" = height,
         "Batting_Hand" = bats,
         "Throwing_Hand" = throws)

fielding <- read.csv("~/Masters Project/Stat_631/Dataset/Fielding.csv") %>% 
  select(playerID,POS,teamID) %>% 
  rename("Player_ID" = playerID,
         "Position" = POS,
         "Team" = teamID)

g_salaries <- read.csv("~/Masters Project/Stat_631/Dataset/Salaries.csv") %>% 
  select(playerID, salary,teamID) %>% 
  rename("Player_ID" = playerID,
         "Salary" = salary,
         "Team" = teamID)

# Group by salary and team
salaries <- g_salaries %>% 
  group_by(Player_ID,Team) %>% 
  summarise(Salary = sum(Salary))


collegeplaying <- read.csv("~/Masters Project/Stat_631/Dataset/CollegePlaying.csv") %>% 
  select(playerID, schoolID) %>% 
  rename("Player_ID" = playerID,
         "School_Playing" = schoolID)


awards <- read.csv("~/Masters Project/Stat_631/Dataset/AwardsPlayers.csv") %>% 
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

# Remove missing data
final_data <- na.omit(final_data)

# Create a new variable for age
# final_data$Current_Age <- as.integer((Sys.Date() - as.Date(paste(final_data$Birth_Year, final_data$Birth_Month, final_data$Birth_Day, sep = "-"))) / 365.25)
# final_data$Debut_Age <- as.integer((Sys.Date() - as.Date(final_data$Debut)) / 365.25)
# final_data$FinalGame_Age <- as.integer((Sys.Date() - as.Date(final_data$Final_Game)) / 365.25)

# final_data$Player_ID <- as.factor(final_data$Player_ID)
# final_data$Team <- as.factor(final_data$Team)
# final_data$League <- as.factor(final_data$League)
# final_data$Birth_Country <- as.factor(final_data$Birth_Country)
# final_data$Batting_Hand <- as.factor(final_data$Batting_Hand)
# final_data$Throwing_Hand <- as.factor(final_data$Throwing_Hand)
# final_data$Debut <- as.factor(final_data$Debut)
# final_data$Position <- as.factor(final_data$Position)
# final_data$School_Playing <- as.factor(final_data$School_Playing)
# final_data$School_Name <- as.factor(final_data$School_Name)
# final_data$Awards <- as.factor(final_data$Awards)




library(corrplot)

# Select only the numeric variables
numeric_data <- final_data %>% 
  select_if(is.numeric)

# Calculate the correlation matrix
cor_matrix <- cor(numeric_data)

# Plot the correlation matrix
corrplot(cor_matrix, method = "circle")

model <- lm(Average ~., data = final_data)

t = 4/nrow(final_data)
cooks_distance <- cooks.distance(model)
plot(cooks_distance)

outliers <- which(cooks_distance>t)
clean_pd <- final_data[-c(outliers),]


model1 <- lm(Average~., data=clean_pd)
summary(model1)

library(MASS)
base_mdl <- lm(Average~Games_Played+Position, data=clean_pd)
full_mdl <- lm(Average~., data=clean_pd)

# Note k=2 uses AIC.  You can use k=log(n) if you want BIC
backward_mdl <- step(full_mdl, scope=list(lower=base_mdl, upper=full_mdl), direction="backward", k=2)

# Perform backward selection using the step function
selected_model <- step(model, direction="backward")

# Print the selected model summary
summary(selected_model)
































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
