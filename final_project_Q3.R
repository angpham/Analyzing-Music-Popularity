library('tidyverse')
library('here')
library('rsample')
library('ggplot2')
library('nnet')
library('yardstick')
library('ggridges')

options(scipen = 50)
set.seed(1818)

### Creating & Cleaning Data
artist_genres <- read.csv(here('final project', 'artist_genres.csv'))

artist_genres <- artist_genres %>% mutate(Genre = as.factor(Genre))
artist_genres$Genre <- relevel(artist_genres$Genre, ref = "other")

### Exploring Data
ggplot(artist_genres, aes(x = acousticness, y = Genre)) +
  geom_density_ridges(fill = '#5F4690') + theme_minimal()
ggplot(artist_genres, aes(x = danceability, y = Genre)) +
  geom_density_ridges(fill = '#1D6996') + theme_minimal()
ggplot(artist_genres, aes(x = energy, y = Genre)) +
  geom_density_ridges(fill = '#38A6A5') + theme_minimal()
ggplot(artist_genres, aes(x = instrumentalness, y = Genre)) +
  geom_density_ridges(fill = '#0F8554') + theme_minimal()
ggplot(artist_genres, aes(x = key, y = Genre)) +
  geom_density_ridges(fill = '#73AF48') + theme_minimal()
ggplot(artist_genres, aes(x = liveness, y = Genre)) +
  geom_density_ridges(fill = '#EDAD08') + theme_minimal()
ggplot(artist_genres, aes(x = loudness, y = Genre)) +
  geom_density_ridges(fill = '#E17C05') + theme_minimal()
ggplot(artist_genres, aes(x = speechiness, y = Genre)) +
  geom_density_ridges(fill = '#94346E') + theme_minimal()
ggplot(artist_genres, aes(x = tempo, y = Genre)) +
  geom_density_ridges(fill = '#CC503E') + theme_minimal()
ggplot(artist_genres, aes(x = time_signature, y = Genre)) +
  geom_density_ridges(fill = '#994E95') + theme_minimal()
ggplot(artist_genres, aes(x = valence, y = Genre)) +
  geom_density_ridges(fill = '#1DB954FF') + theme_minimal()

ggplot(artist_genres, aes(y = Genre)) +
  geom_bar() + theme_minimal()

glimpse(artist_genres)
summary(artist_genres)

### Fitting Model
genres_split <- initial_split(artist_genres, prop = 0.75)
genres_train <- training(genres_split)
genres_test <- testing(genres_split)

genre_model <- multinom(Genre ~ acousticness + danceability + energy + 
                          loudness + liveness + speechiness + tempo + 
                          valence + key + instrumentalness + time_signature, 
                        data = genres_train)
exp(coef(genre_model))

### Making Predictions & Validating Model
preds_train <- predict(genre_model, newdata = genres_train, type = "class")
preds_test <- predict(genre_model, newdata = genres_test, type = "class")

results_train <- data.frame(
  "truth" = as.factor(genres_train$Genre),
  "prediction" = as.factor(preds_train)
)
cm_train <- conf_mat(results_train, truth = truth, estimate = prediction)
autoplot(cm_train, type = "heatmap") 

precision_train <- 
  data.frame("Genre" = c("rock", "rap", "r&b",
                         "pop", "metal", "k-pop", 
                         "jazz", "hip hop", "electronic", 
                         "edm", "country", "classical", 
                         "blues", "other"),
             "Precision" = c(24/(51+1+1+1+5+2+24), 9/(10+10+4+1+9), 1/(2+3+1+1+1),
                             3/(3+1+3+1), 178/(197+1+1+178+7+31), 0,
                             1/(1), 237/(271+1+5+1+3+237+6+1+53+20+126), 0,
                             8/(18+8+1+1+2), 0, 360/(321+3601+1+10+1+3+1),
                             0, 14132/(212+737+448+405+323+1391+672+227+533+4009+417+730+1756)))


results_test <- data.frame(
  "truth" = genres_test$Genre,
  "prediction" = preds_test
)
cm_test <- conf_mat(results_test, truth = truth, estimate = prediction)
autoplot(cm_test, type = "heatmap") + theme_minimal()

precision_test <- 
  data.frame("Genre" = c("rock", "rap", "r&b",
                         "pop", "metal", "k-pop", 
                         "jazz", "hip hop", "electronic", 
                         "edm", "country", "classical", 
                         "blues", "other"),
             "Precision" = c(10/(25+1+1+4+10), 0/(6+3+1), 0/(2+1+2), 
                             0, 73/(62+73+3+12), 0,
                             0, 61/(90+3+2+61+22+5+44+1), 0,
                             1/(1+1), 0, 143/(110+143+4+3+1+1),
                             0, 4721/(4721+72+224+165+121+91+473+214+66+197+1319+164+246+558)))

### Conclusions
