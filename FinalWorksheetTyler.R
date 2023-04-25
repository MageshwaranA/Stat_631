rm(list=ls())
setwd("/home/torent/STA 631/Final_Project/STA_631_Project_New/Stat_631")


# Load necessary packages
library(dplyr)
library(ggplot2)
library(corrplot)
library(Lahman)
library(purrr)

# Load batting data from Lahman Baseball Database
batting <- read.csv("Dataset/Batting.csv") %>% 
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
people <- read.csv("Dataset/People.csv") %>% 
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

fielding <- read.csv("Dataset/Fielding.csv") %>% 
  select(playerID,POS) %>% 
  rename("Player_ID" = playerID,
         "Position" = POS)

salaries <- read.csv("Dataset/Salaries.csv") %>% 
  select(playerID, salary) %>% 
  rename("Player_ID" = playerID,
         "Salary" = salary)

collegeplaying <- read.csv("Dataset/CollegePlaying.csv") %>% 
  select(playerID, schoolID,yearID) %>% 
  rename("Player_ID" = playerID,
         "School_Playing" = schoolID,
         "Year_ID" = yearID)


school <- read.csv("Dataset/Schools.csv") %>% 
  select(schoolID,name_full) %>% 
  rename("School_Playing" = schoolID,
         "School_Name" = name_full)

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
final_data$Current_Age <- as.integer((Sys.Date() - as.Date(paste(final_data$Birth_Year, final_data$Birth_Month, final_data$Birth_Day, sep = "-"))) / 365.25)
final_data$Debut_Age <- as.integer((Sys.Date() - as.Date(final_data$Debut)) / 365.25)
final_data$FinalGame_Age <- as.integer((Sys.Date() - as.Date(final_data$Final_Game)) / 365.25)

Sampled_data <- final_data[sample(nrow(final_data), 100), ]
model <- lm(Average ~., data=Sampled_data)
Sampled_data <- Sampled_data %>% 
  select(Average,Stint,Team,League,Games_Played,Birth_Year,Birth_Month,Birth_Day,Birth_Country,Birth_State,Birth_City,Weight,Height,Batting_Hand,Throwing_Hand,Position,School_Playing,School_Name,Awards,Debut_Age)

model <- lm(Average ~., data=Sampled_data)

# Print the model summary
summary(model)

# Perform backward selection using the step function
selected_model <- step(model, direction="backward")

# Print the selected model summary
summary(selected_model)

Sampled_data <- Sampled_data %>% 
  select(Average,Stint,Team,League,Games_Played,Birth_Year,Birth_Month,Birth_Day,Birth_Country,Birth_State,Birth_City,Weight,Height,Batting_Hand,Throwing_Hand,Position,School_Playing,School_Name,Awards,Debut_Age)

# Selected Model
# Average ~ Team + Games_Played + Birth_City + Position + Awards

team_model <- lm(Average ~ Team, data=final_data)
summary(team_model)

games_model <- lm(Average ~ Games_Played, data=final_data)
summary(games_model)

birth_year_model <- lm(Average ~ Birth_Year, data=final_data)
summary(birth_year_model)

birth_city_model <- lm(Average ~ Birth_City, data=final_data)
summary(birth_city_model)

final_data$Position <- as.factor(final_data$Position)
position_model <- lm(Average ~ Position, data=final_data)
summary(position_model)



# Test (did not work)
# create a NULL vector called model so we have something to add our layers to
model=NULL

# create a vector of the dataframe column names used to build the formula
vars = names(final_data)
# remove variable names you don’t want to use (at least
# the response variable (if its in the first column)
vars = vars[-1]

# the combn function will run every different combination of variables and then run the glm
for(i in 1:length(vars)){
  xx = combn(vars,i)
  if(is.null(dim(xx))){
    fla = paste("Average ~", paste(xx, collapse="+"))
    model[[length(model)+1]]=glm(as.formula(fla),data=final_data)
  } else {
    for(j in 1:dim(xx)[2]){
      fla = paste("Average ~", paste(xx[1:dim(xx)[1],j], collapse="+"))
      model[[length(model)+1]]=glm(as.formula(fla),data=final_data) 
    }
  }
}

# see how many models were build using the loop above
length(model)

# create a vector to extract AIC and BIC values from the model variable
AICs = NULL
BICs = NULL
for(i in 1:length(model)){
  AICs[i] = AIC(model[[i]])
  BICs[i] = BIC(model[[i]])
}

#see which models were chosen as best by both methods
which(AICs==min(AICs))
which(BICs==min(BICs))
