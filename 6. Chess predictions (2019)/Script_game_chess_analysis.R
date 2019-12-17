rm(list=ls())
# the link dl has to be changed to 1
game_chess <- "https://www.dropbox.com/s/5vrubrqhhvp5kyu/games_chess.csv?dl=1"

getwd() # To check if you want to change the working directory

library(data.table)
library(bluegrafir)
library(tidyverse)
library(broom)
library(caret)
library(psych)
library(rms)
library(klaR)

data_chess <- fread(game_chess, quote = "", fill = T)

# Describe data to understand it
# names(data_chess)
# head(data_chess)
# str(data_chess)
# dim(data_chess)

#######################################################
# Describing the data to understand it better

# Describing probable outcome variable (winner) 
# Who wins
graficat(data_chess$winner)

# Ways of winning (can be outcome variable)
# Reasons for loosing are unbalanced
graficat(data_chess$victory_status)

# Understanding some variables
# NUmber of moves in the opening phase
graficat(data_chess$opening_ply)

# Check the openings types
graficat(data_chess$opening_name)

# Standardized code for types of openings
graficat(data_chess$opening_eco)

# Both variables are related to each other
#data_chess$opening_eco == data_chess$opening_name

######################################################
# Create a training set

set.seed(12345)
data_chess_samples <- data_chess$winner %>% 
  createDataPartition(p = 0.8, list = F)
data_chess_train <- data_chess[data_chess_samples,]
data_chess_test <- data_chess[-data_chess_samples,]

names(data_chess_train)
# Calculate variables for underdogs

data_chess_train <- data_chess_train %>% mutate(
  underdog = case_when(
    white_rating > black_rating ~ "black",
    white_rating < black_rating ~ "white",
    TRUE ~ "No"))

data_chess_train <- data_chess_train %>% 
  filter(winner != "draw")

data_chess_train$winner <- recode(data_chess_train$winner,
                               "white" = "1",
                                "black" = "0")


# Try with a logistic regression

data_chess_train$winner <- as.numeric(data_chess_train$winner)

# It is related to white winning.
m0 <- glm(winner ~ white_rating,
          data = data_chess_train,
          family = binomial)
tidy(m0) # tidy table with broom

# It is inversely related to black winning. 
m0.1 <- glm(winner ~ black_rating,
            data =data_chess_train,
            family = binomial)
tidy(m0.1) # tidy table with broom

# Check how the underdog does against the theoretical better player

data_chess_train$underdog <- factor(data_chess_train$underdog,
                                 level= c("white","No","black"))

# Probability of underdog winning
m0.2 <- glm(winner ~ underdog, 
            data = data_chess_train,
            family = binomial)
tidy(m0.2) # tidy table with broom
glance(m0.2) # tidy model fit with broom

########################################################

# La cantidad de jugadas se relaciona con el ganador?
describeBy(data_chess_train$turns,
           group = data_chess_train$winner)

ggplot(data_chess_train, aes(turns, group = winner))+
  geom_histogram(alpha=0.5, position="identity",
                 bins = 100)+
  theme(panel.background = element_blank())+
  labs(x = "Cantidad de turno",
       y = "Distribución")
  
# Hay un super outlier que podría estar jalando la línea
ggplot(data_chess_train, aes(x=turns, y=winner))+
  geom_point()+
  theme(panel.background = element_blank()) +
  labs(x = "Turns",
       y = "Winner")

describe(data_chess_train$turns)

data_chess_train %>% 
  filter(turns == 349)

# Aquí lo único que hice fue imputar el outlier con la mediana porque igual está asimétrico
data_chess_train[data_chess_train$id=="pN0ioHNr",
                 "turns"] <- 54
                                 
# Lineal
mt <- glm(winner ~ turns,
          data = data_chess_train)
tidy(mt)
glance(mt)
lrm(mt)
# Cuadrática
mt2 <- glm(winner ~ turns + I(turns^2),
          data = data_chess_train)
tidy(mt2)
glance(mt2)

# Más turnos menos posibilidad de que las blancas ganen (linelmente).
# El AIC no mejora mucho así que lo dejaría ahí. 


#########################################################
# Underdog and turns
m0.3 <- glm(winner ~ underdog + turns,
            data = data_chess_train,
            family = binomial)

tidy(m0.3)
glance(m0.3)
lrm(m0.3) # Pseudo R square

# Underdog, turns and elo (careful with elo and multicollinearity) 

# check visually elo and underdog

# The image gives ideas that they are related
ggplot(data_chess_train, aes(x = underdog,
                             y =white_rating))+
  geom_point()+
  theme(panel.background = element_blank())+
  labs(x ="Underdog",
       y = "White rating")


maov <- aov(white_rating ~ underdog,
            data =data_chess_train)
tidy(maov) # Results 
glance(maov) # The R square does not show relationship
TukeyHSD(maov) # They are difference between groups

m0.4 <- glm(winner ~ underdog + turns + white_rating,
            data = data_chess_train,
            family = binomial)

tidy(m0.4)
glance(m0.4)
lrm(m0.4)

#########################################################
cor.test(data_chess_train$black_rating,data_chess_train$white_rating) # Checking for multi colinearity

# Centering variables to have interpretable zero and be able to eliminate intercept
data_chess_train$black_rating <- scale(data_chess_train$black_rating, center = T, scale = F)
data_chess_train$white_rating <- scale(data_chess_train$white_rating, center = T, scale = F)
data_chess_train$turns <- scale(data_chess_train$turns, center = T, scale = F)
data_chess_train$opening_ply <- scale(data_chess_train$opening_ply, center = T, scale = F)

m0.5 <- glm(winner ~ underdog + turns + white_rating + black_rating,
            data = data_chess_train,
            family = binomial)
tidy(m0.5)
glance(m0.5)
lrm(m0.5)

#########################################################
#Up to now I could stick with this model. 

m0.6 <- glm(winner ~ turns + white_rating + black_rating,
            data = data_chess_train,
            family = binomial)
tidy(m0.6)
glance(m0.6)
lrm(m0.6)

# Try with a Naive bayes
data_chess_train$winner <- as.factor(data_chess_train$winner)

mn1 <- NaiveBayes(winner ~ turns + white_rating + black_rating,
                  data = data_chess_train)


########################################################
# Diagnostics

data_chess_test <- data_chess_test %>% 
  filter(winner != "draw")

data_chess_test$winner <- recode(data_chess_test$winner,
                                  "white" = "1",
                                  "black" = "0")

describeBy(data_chess_test$turns, data_chess_test$winner)

data_chess_test %>% 
  filter(turns == 349)

# Aquí lo único que hice fue imputar el outlier con la mediana porque igual está asimétrico
data_chess_train[data_chess_test$id=="pN0ioHNr",
                 "turns"] <- 54


data_chess_test$black_rating <- scale(data_chess_test$black_rating, center = T, scale = F)
data_chess_test$white_rating <- scale(data_chess_test$white_rating, center = T, scale = F)
data_chess_test$turns <- scale(data_chess_test$turns, center = T, scale = F)

predictions <- predict(m0.6, data_chess_test) # This should be the test set.
prediction.probabilities <- predictions$posterior[,2]

### Model predictions in Naive Bayes ###
predNB <- mn1 %>% predict(data_chess_test)
### Model accuracy ###
mean(predNB$class == data_chess_test$winner)
