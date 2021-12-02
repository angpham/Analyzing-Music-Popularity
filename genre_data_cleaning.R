library('here')
library('spotifyr')
library('dplyr')
library('stringi')
library('textclean')

Sys.setenv(SPOTIFY_CLIENT_ID = '6b41c962842f424ca0a420d1c9771d43')
Sys.setenv(SPOTIFY_CLIENT_SECRET = 'eaed3e2fb888427eacb9c3e3c841ea27')

spotify_artists <- read.csv(here('final project', 'spotify_artists.csv'))
spotify_tracks <- read.csv(here('final project', 'spotify_tracks.csv'))

spotify <- data.frame("Artist" = spotify_artists$name,
                      "ID" = spotify_artists$id,
                      "Genre" = 0,
                      "Artist_Popularity" = spotify_artists$artist_popularity)

for (i in 1:56129 ) {
  spotify$Genre[i] <- toString(get_artist(spotify$ID[i])$genres[1]) 
}

write_csv(spotify, "/Users/nikkipoentis/Desktop/mgsc310/final project/spotify.csv")

artist_genres <- drop_row(spotify, "Genre", "NULL")
artist_genres <- artist_genres %>% mutate(acousticness = NA, danceability = NA,
                                          energy = NA, loudness = NA,
                                          liveness = NA, speechiness = NA,
                                          tempo = NA, valence = NA,
                                          key = NA, instrumentalness = NA,
                                          time_signature = NA)


for (i in 1:39068) {
  if (length(str_locate_all(pattern ='classical', artist_genres$Genre[i])[[1]]) != 0) {
    artist_genres$Genre[i] = 'classical'
  } else if (length(str_locate_all(pattern ='country', artist_genres$Genre[i])[[1]]) != 0) {
    artist_genres$Genre[i] = 'country'
  } else if (length(str_locate_all(pattern ='electronic', artist_genres$Genre[i])[[1]]) != 0) {
    artist_genres$Genre[i] = 'electronic'
  } else if (length(str_locate_all(pattern ='edm', artist_genres$Genre[i])[[1]]) != 0) {
    artist_genres$Genre[i] = 'edm'
  } else if (length(str_locate_all(pattern ='jazz', artist_genres$Genre[i])[[1]]) != 0) {
    artist_genres$Genre[i] = 'jazz'
  } else if (length(str_locate_all(pattern ='k-pop', artist_genres$Genre[i])[[1]]) != 0) {
    artist_genres$Genre[i] = 'k-pop'
  } else if (length(str_locate_all(pattern ='metal', artist_genres$Genre[i])[[1]]) != 0) {
    artist_genres$Genre[i] = 'metal'
  } else if (length(str_locate_all(pattern ='pop', artist_genres$Genre[i])[[1]]) != 0) {
    artist_genres$Genre[i] = 'pop'
  } else if (length(str_locate_all(pattern ='hip hop', artist_genres$Genre[i])[[1]]) != 0) {
    artist_genres$Genre[i] = 'hip hop'
  } else if (length(str_locate_all(pattern ='rap', artist_genres$Genre[i])[[1]]) != 0) {
    artist_genres$Genre[i] = 'rap'
  } else if (length(str_locate_all(pattern ='r&b', artist_genres$Genre[i])[[1]]) != 0) {
    artist_genres$Genre[i] = 'r&b'
  } else if (length(str_locate_all(pattern ='rock', artist_genres$Genre[i])[[1]]) != 0) {
    artist_genres$Genre[i] = 'rock'
  } else if (length(str_locate_all(pattern ='blues', artist_genres$Genre[i])[[1]]) != 0) {
    artist_genres$Genre[i] = 'blues'
  } else {
    artist_genres$Genre[i] = 'other'
  }
}

which(strsplit(spotify_tracks$artists_id[9], "")[[1]] == "'") # 2, 25, 28, 51

for (ID in artist_genres$ID) {
  artist_genres$acousticness[artist_genres$ID == ID] <- mean(spotify_tracks$acousticness[stri_sub(spotify_tracks$artists_id, 3, 24) == ID | 
                                                                                             stri_sub(spotify_tracks$artists_id, 29, 50) == ID])
  artist_genres$danceability[artist_genres$ID == ID] <- mean(spotify_tracks$danceability[stri_sub(spotify_tracks$artists_id, 3, 24) == ID | 
                                                                                             stri_sub(spotify_tracks$artists_id, 29, 50) == ID])
  artist_genres$energy[artist_genres$ID == ID] <- mean(spotify_tracks$energy[stri_sub(spotify_tracks$artists_id, 3, 24) == ID | 
                                                                                 stri_sub(spotify_tracks$artists_id, 29, 50) == ID])
  artist_genres$loudness[artist_genres$ID == ID] <- mean(spotify_tracks$loudness[stri_sub(spotify_tracks$artists_id, 3, 24) == ID | 
                                                                                     stri_sub(spotify_tracks$artists_id, 29, 50) == ID])
  artist_genres$liveness[artist_genres$ID == ID] <- mean(spotify_tracks$liveness[stri_sub(spotify_tracks$artists_id, 3, 24) == ID | 
                                                                                     stri_sub(spotify_tracks$artists_id, 29, 50) == ID])
  artist_genres$speechiness[artist_genres$ID == ID] <- mean(spotify_tracks$speechiness[stri_sub(spotify_tracks$artists_id, 3, 24) == ID | 
                                                                                           stri_sub(spotify_tracks$artists_id, 29, 50) == ID])
  artist_genres$tempo[artist_genres$ID == ID] <- mean(spotify_tracks$tempo[stri_sub(spotify_tracks$artists_id, 3, 24) == ID | 
                                                                               stri_sub(spotify_tracks$artists_id, 29, 50) == ID])
  artist_genres$valence[artist_genres$ID == ID] <- mean(spotify_tracks$valence[stri_sub(spotify_tracks$artists_id, 3, 24) == ID | 
                                                                                   stri_sub(spotify_tracks$artists_id, 29, 50) == ID])
  artist_genres$key[artist_genres$ID == ID] <- mean(spotify_tracks$key[stri_sub(spotify_tracks$artists_id, 3, 24) == ID | 
                                                                                 stri_sub(spotify_tracks$artists_id, 29, 50) == ID])
  artist_genres$instrumentalness[artist_genres$ID == ID] <- mean(spotify_tracks$instrumentalness[stri_sub(spotify_tracks$artists_id, 3, 24) == ID | 
                                                                                 stri_sub(spotify_tracks$artists_id, 29, 50) == ID])
  artist_genres$time_signature[artist_genres$ID == ID] <- mean(spotify_tracks$time_signature[stri_sub(spotify_tracks$artists_id, 3, 24) == ID | 
                                                                                 stri_sub(spotify_tracks$artists_id, 29, 50) == ID])
}
artist_genres <- drop_row(artist_genres, "acousticness", "NaN")

write_csv(artist_genres, "/Users/nikkipoentis/Desktop/mgsc310/final project/artist_genres.csv")
