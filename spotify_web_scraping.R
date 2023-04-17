library(spotifyr)
library(tidyverse)
library(knitr)
library(lubridate)
library(rvest)
library(stringr)
library(ggplot2)
library(hrbrthemes)
library(plotly)
library(GGally)

Sys.setenv(SPOTIFY_CLIENT_ID = '49b25569386740b6908de85bb42dfadb')
Sys.setenv(SPOTIFY_CLIENT_SECRET = '34802de3b4ee4e19bc938afc1327640f')

# beatles <- get_artist_audio_features('the beatles')
# 
# beatles %>% 
#   count(key_mode, sort = TRUE) %>% 
#   head(5) %>% 
#   kable()
# 
# 
# 
# get_my_recently_played(limit = 10) %>% 
#   mutate(artist.name = map_chr(track.artists, function(x) x$name[1]),
#          played_at = as_datetime(played_at)) %>% 
#   select(track.name, artist.name, track.album.name, played_at) %>% 
#   kable()
# 
# 
# get_my_top_artists_or_tracks(type = 'artists', time_range = 'long_term', limit = 5) %>% 
#   select(name, genres) %>% 
#   rowwise %>% 
#   mutate(genres = paste(genres, collapse = ', ')) %>% 
#   ungroup %>% 
#   kable()
# 
# get_my_top_artists_or_tracks(type = 'tracks', time_range = 'long_term', limit = 10) %>% 
#   select(name) %>% 
#   rowwise %>% 
#   ungroup %>% 
#   kable()
# 
# get_my_top_artists_or_tracks(type = 'tracks', time_range = 'short_term', limit = 10) %>% 
#   mutate(artist.name = map_chr(artists, function(x) x$name[1])) %>% 
#   select(name, artist.name, album.name) %>% 
#   kable()

playl <- get_user_playlists(
  "22yrb3lso7eiw5bzw62eia5qy",
  limit = 50,
  offset = 0,
  authorization = get_spotify_authorization_code(),
  include_meta_info = FALSE
)

my_playl <- get_playlist(
  "0zOtY4XYCCayp4I541PlJm",
  authorization = get_spotify_authorization_code()
)

my_playl <- get_playlist_tracks(
  "0zOtY4XYCCayp4I541PlJm",
  authorization = get_spotify_access_token(),
)

my_playl2 <- get_playlist_tracks(
  "0zOtY4XYCCayp4I541PlJm",
  fields = c("track.id", "track.name", "track.album.release_date"),
  limit = 100,
  offset = 100,
  authorization = get_spotify_access_token()
)

pista <- get_track("5RsfvV4oSchEPH6t93C1lR", authorization = get_spotify_access_token())

# mother_goose <- get_track_audio_analysis("5RsfvV4oSchEPH6t93C1lR", authorization = get_spotify_access_token())

mother_goose <- get_track_audio_features("5RsfvV4oSchEPH6t93C1lR", authorization = get_spotify_access_token())

pista[['album']]
my_playl$track.name
decadas <- 10 * as.integer(str_sub(my_playl$track.album.release_date, 1, 4)) %/% 10

# LOOP PARA EXTRAER LOS DATOS SOBRE LAS PISTAS QUE ESTAN EN LA LISTA DE REPRODUCCION. CADA LISTA DE REPRODUCCION TIENE UN ID UNICO QUE SE DEBE CONOCER. LA API SOLO PROVEE DATOS DE UN MAXIMO DE 100 PISTAS.

bdmusica <- NULL
for(i in 0:13){
  aux <- get_playlist_tracks(
    "0zOtY4XYCCayp4I541PlJm",
    fields = c("track.artists", "track.id", "track.name", "track.album.release_date"),
    limit = 100,
    offset = i * 100,
    authorization = get_spotify_access_token()
  )
  bdmusica <- bdmusica %>% bind_rows(aux)
}

aux <- get_playlist_tracks(
  "37i9dQZEVXbNG2KDcFcKOF",
  fields = c("track.artists", "track.id", "track.name", "track.album.release_date"),
  limit = 100,
  offset = 0,
  authorization = get_spotify_access_token()
)

bdmusica <- aux

# MODIFICACIONES A LA BASE DE DATOS DE MUSICA

# EXTRAER EL AÑO A PARTIR DE LA FECHA DE PUBLICACION DEL ALBUM

bdmusica <- bdmusica %>% mutate(año = str_sub(bdmusica$track.album.release_date, 1, 4))

# CONVERTIR EL AÑO EN DECADA

bdmusica <- bdmusica %>% mutate(decada = 10 * as.integer(bdmusica$año) %/% 10)

# CONVERTIR DECADAS EN FACTORES

bdmusica <- bdmusica %>% mutate(decada_f = as.factor(bdmusica$decada))

# artists <- bdmusica$track.artists  %>% lapply('[[', 3)

# EXTRAER EL NOMBRE DEL ARTISTA Y AGREGAR UNA COLUMNA CON ESTE DATO A LA BASE DE DATOS DE MUSICA

bdmusica <- bdmusica %>% mutate(artista = do.call("rbind", lapply(bdmusica$track.artists %>% lapply('[[', 3), "[[", 1)))

#CAMBIAR EL NOMBRE DEL id DE LA PISTA PARA PODER HACER EL INNER JOIN POSTERIOR

bdmusica <- bdmusica %>% rename(id = track.id)

# CONSTRUCCION DE LA BASE DE DATOS CON EL ANALISIS DE CADA UNO DE LOS TEMAS EN EL PLAYLIST. SE BARRE CON UN for TODOS LOS ELEMENTOS DE bdmusica PARA EXTRAER EL id DE LA PISTA (NOMBRE DE COLUMNA CAMBIADO PREVIAMENTE) Y CON ESE DATO SE SOLICITA A SPOTIFY LAS CARACTERISTICAS DEL AUDIO DE LA PISTA. 
bd_analisis <- NULL
for(i in 1:50){
  aux <- get_track_audio_features(bdmusica[i,]$id, authorization = get_spotify_access_token())
  bd_analisis <- bd_analisis %>% bind_rows(aux)
}

# CONSTRUCCION DE LA BASE DE DATOS COMPLETA UNIENDO LAS DOS BASES DE DATOS EN BASE AL id DE CADA PISTA. EL ORDEN DE LAS PISTAS ES EL MISMO EN MABAS BASES DE DATOS

bd_completa <- NULL
bd_completa <- inner_join(bdmusica, bd_analisis, by = "id")

bd_completa1 <- bd_completa %>% mutate(pl = "PL1")

# FUNCION DE NORMALIZACION PARA CONVERTIR LOS PARAMETROS DE ANALISIS A UNA MISMA ESCALA

normalz <-function(x){
  z <- (x-mean(x))/sd(x)
  return(z)
}

# APLICACION DE LA FUNCION DE NORMALIZACION A PARAMETROS COMO LA "bailabilidad", "energía" O "vivacidad" DE CADA PISTA Y CREACION DE NUEVAS VARIABLES NORMALIZADAS EN LA BASE DE DATOS.

bd_completa <- bd_completa %>% mutate(dance_norm = (normalz(bd_completa$danceability)))

bd_completa <- bd_completa %>% mutate(energy_norm = (normalz(bd_completa$energy)))

bd_completa <- bd_completa %>% mutate(live_norm = (normalz(bd_completa$liveness)))

bd_completa <- bd_completa %>% mutate(val_norm = (normalz(bd_completa$valence)))

bd_completa2 <- bd_completa %>% mutate(pl = "PL2")

bd_completa <- bd_completa1 %>% rbind(bd_completa2)

saveRDS(bd_completa, "datos_spotify.rds")

graf_1 <- ggplot(bd_completa, aes(x = dance_norm, y = live_norm, color = decada_f)) + 
  geom_point(size = 1) +
  theme_dark()
  
graf_1

ggplotly(graf_1)

pairs(bd_completa[26:28])

ggpairs(bd_completa, columns = 26:28, aes(color = decada_f, alpha = 0.5))

coord_fixed(ratio = 1, xlim = c(-3.5, 3.5), ylim = c(-3.5, 3.5), expand = FALSE, clip = "on")

graf_2 <- ggplot(bd_completa, aes(x=as.factor(decada), fill=as.factor(decada) )) + 
  geom_bar( ) +
  geom_text(aes(label = after_stat(count)), stat = "count", vjust = -.5, colour = "black") + 
  scale_fill_hue(c = 40) +
  xlab("Canciones por década") +
  ylab("") +
  theme_bw() +
  theme(legend.position="none") 
graf_2

graf
ggplotly(graf_2, tooltip = "count")

text