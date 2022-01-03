install.packages("spotifyr")
library(spotifyr)
library(ggplot2)

Sys.setenv(SPOTIFY_CLIENT_ID = "bbd3e8e97ce948ec8c1ea0e992b82899")
Sys.setenv(SPOTIFY_CLIENT_SECRET = "32f4416c74b5435d855abbcd73549b14")

access_token <- get_spotify_access_token()

pink_floyd <- get_artist_audio_features(artist = "Pink Floyd")
parcels <- get_artist_audio_features(artist = "Parcels")

# dance vibes of all artist songs
ggplot(data = parcels, aes(x = valence, y = danceability, colour = energy)) +
  geom_vline(xintercept = .5, colour = "white") +
  geom_hline(yintercept = .5, colour = "white") + 
  geom_point() +
  theme_minimal() +
  theme(
    legend.position = "none",
    
    panel.grid = element_blank(),
    panel.background = element_rect(fill = "black"),
    
    plot.background = element_rect(fill = "black"),
    
    axis.text = element_blank(),
    axis.title = element_text(colour = "white")
  )
