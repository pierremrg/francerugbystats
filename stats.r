require(ggplot2)
require(plyr)
require(reshape2)

########################################################
# Pourcentage de victoire de l'équipe de France par an #
########################################################

matches = read.csv2("data/matches_list.csv", encoding="UTF-8", stringsAsFactors=FALSE, sep=',')

#df=as.data.frame(aggregate(Nombre ~ Année, matches, sum))
df=as.data.frame(matches)

head(df)

head(df[df$result == 'won', ])

won = matches[matches$result == 'won', ]
df_won = as.data.frame(aggregate(result ~ year, won, FUN=length), c("one", "two"))

lost = matches[matches$result == 'lost', ]
df_lost = as.data.frame(aggregate(result ~ year, lost, FUN=length))

head(df_won)
head(df_lost)

# Annees sans défaite
years_missing_lost = df_won[!df_won$year %in% df_lost$year, ]$year

# Annees sans victoire
years_missing_won = df_lost[!df_lost$year %in% df_won$year, ]$year


# Ajout des annees sans victoire
df_years_missing_won = data.frame(years_missing_won, rep(0, length(years_missing_won)))
colnames(df_years_missing_won) = c('year', 'result')

df_won = rbind(df_won, df_years_missing_won)

# Ajout des annees sans defaite
df_years_missing_lost = data.frame(years_missing_lost, rep(0, length(years_missing_lost)))
colnames(df_years_missing_lost) = c('year', 'result')

df_lost = rbind(df_lost, df_years_missing_lost)


# Tri des colonnes pour la fusion
df_won = df_won[order(df_won$year), ]
df_lost = df_lost[order(df_lost$year), ]

colnames(df_won) = c('year', 'won')
colnames(df_lost) = c('year', 'lost')

# Fusion des deux dataframes
df_result = data.frame(df_won, df_lost$lost)
colnames(df_result) = c('year', 'won', 'lost')


# Calcul du pourcentage de victoire
percent_won = df_result$won/(df_result$won + df_result$lost)

df_result = cbind(df_result, percent_won)
head(df_result, 10)


ggplot(df_result, aes(year, y=percent_won)) +
  geom_line()
  #xlab("Année") +
  #scale_x_discrete(limits=c(min(names$Année):max(names$Année))) +
  #ylab("Longueur") +
  #ggtitle("Longueur moyenne des noms")




###############################################
# Moyenne du nombre d'essais/pénalités par an #
###############################################

details = read.csv2("data/details_scores.csv", encoding="UTF-8", stringsAsFactors=FALSE, sep=',')

df=as.data.frame(details)
# On supprime l'annee en cours (donnees incompletes)
df = df[df$year < 2019,]

head(df)

df=ddply(df, .(year), summarize, mean_tries=mean(tries), mean_pens=mean(pens))

ggplot(df, aes(year, y=mean_pens)) +
  geom_line() +
  xlab("Année") +
  scale_x_discrete(limits=c(min(df$year):max(df$year)))
  #ylab("Longueur") +
  #ggtitle("Longueur moyenne des noms")

approxData = data.frame(
  with(df,
       approx(df$year, df$mean_pens, xout = seq(min(df$year), max(df$year), by=10), rule=1)
  )
)

head(df)
approxData


# Fonction qui renvoie une partie des lignes d'une dataframe autour de l'annee indiquee
df_windows_years = function(df, year, w = 5){
  #min_year = floor(year/w)*w
  #max_year = min_year + w
  min_year = year - w/2
  max_year = year + w/2
  df = df[df$year >= min_year & df$year <= max_year, ]
  return(df)
}

head(df)

df_smooth = ddply(df, .(year, mean_tries, mean_pens), summarize,
        mean_tries_smooth=mean(df_windows_years(df, year, 30)$mean_tries),
        mean_pens_smooth=mean(df_windows_years(df, year, 30)$mean_pens)
)

ggplot() +
  geom_line(data=df_smooth, aes(year, mean_tries), color='gray') +
  geom_line(data=df_smooth, aes(year, mean_tries_smooth), color='blue') +
  xlab("Année") +
  ylab("Nombre d'essais moyen") +
  ggtitle("Evolution du nombre d'essais par match en fonction des années")

ggplot() +
  geom_line(data=df_smooth, aes(year, mean_pens), color='black') +
  geom_line(data=df_smooth, aes(year, mean_pens_smooth), color='red') +
  xlab("Année") +
  ylab("Nombre de pénalités moyen") +
  ggtitle("Evolution du nombre de pénalités par match en fonction des années")


#####################
# Villes finalistes #
#####################


#require('jsonlite')
#json = fromJSON("https://nominatim.openstreetmap.org/search?format=json&q=Castres")
#df_city = as.data.frame(json)

#as.double(df_city$lat[[1]])
#as.double(df_city$lon[[1]])


#json = fromJSON("https://maps.googleapis.com/maps/api/geocode/json?key=AIzaSyCx5qXLm-eyr51hJvSPhsNfOnZZMjMvjAg&address=Toulouse")
#json


require(leaflet)
require(plyr)


format_cities_data = function(df, min_year, max_year){
  # On garde seulement la tranche d'annees concernees
  df = df[df$year >= min_year & df$year <= max_year, ]
  
  # On assemble les champions et les finalistes
  df_winners = df[, c('champion', 'champion_lat', 'champion_lon')]
  colnames(df_winners) = c('city', 'lat', 'lon')
  df_losers = df[, c('finalist', 'finalist_lat', 'finalist_lon')]
  colnames(df_losers) = c('city', 'lat', 'lon')
  
  df = rbind(df_winners, df_losers)
  
  # On compte le nombre de fois ou l'equipe etait en finale (vainqueur ou non)
  df$appearances = as.numeric(ave(df$city, df$city, FUN = length))
  
  # Lat/Lon en nombres
  df$lat = as.numeric(df$lat)
  df$lon = as.numeric(df$lon)
  
  # On enleve les lignes dupliquees
  df = df[!duplicated(df), ]
  
  return(df)
}

create_cities_map = function(df){
  # Map design
  df = ddply(df, .(city, lat, lon, appearances), summarize,
             radius=min(40, 15+5*appearances),
             opacity=min(0.8, 0.1+0.1*appearances)
  )
  
  # Creation de la map
  map = leaflet(padding = 0)
  map = addTiles(map)
  map = setView(map, lng=2, lat=47,zoom = 6)
  map = addProviderTiles(map,"CartoDB.Positron")
  map = addCircleMarkers(map,
                         lng = df$lon,
                         lat = df$lat,
                         radius = df$radius, weight = 1,
                         opacity = 0,
                         fill = T, fillColor = "#2A4293",
                         fillOpacity = 0.95,
                         color = "white")
  return(map)
}


finalists = read.csv2("data/finalists.csv", encoding="UTF-8", stringsAsFactors=FALSE, sep=',')

df=as.data.frame(finalists)
df = format_cities_data(df, 2000, 2020)
nrow(df)
map = create_cities_map(df)
map

df=as.data.frame(finalists)
df = format_cities_data(df, 1980, 1999)
nrow(df)
map = create_cities_map(df)
map

df=as.data.frame(finalists)
df = format_cities_data(df, 1960, 1979)
nrow(df)
map = create_cities_map(df)
map

df=as.data.frame(finalists)
df = format_cities_data(df, 1940, 1959)
nrow(df)
map = create_cities_map(df)
map

df=as.data.frame(finalists)
df = format_cities_data(df, 1920, 1939)
nrow(df)
map = create_cities_map(df)
map

df=as.data.frame(finalists)
df = format_cities_data(df, 1900, 1919)
nrow(df)
map = create_cities_map(df)
map


###################################
# tests




df=ddply(details, .(year), summarize, mean_tries=mean(tries), mean_pens=mean(pens))

df_2015 = df[df$year >= 2015, ]

df_2015

df_2015$champion_lat = as.numeric(df_2015$champion_lat)
df_2015$champion_lon = as.numeric(df_2015$champion_lon)
df_2015$finalist_lat = as.numeric(df_2015$finalist_lat)
df_2015$finalist_lon = as.numeric(df_2015$finalist_lon)


tail(df)
end_df = df[df$year <= 1908,]
end_df

test = end_df

test=ddply(test, .(champion, champion_lat, champion_lon, finalist_lat, finalist_lon),
           summarize, c=sum(test[which(test$champion == champion)])
)
test

test$c <- as.numeric(ave(test$champion, test$champion, FUN = length))


map = leaflet(padding = 0)
map = addTiles(map)
map = setView(map, lng=2, lat=47,zoom = 6)
map = addProviderTiles(map,"CartoDB.Positron")
map = addCircleMarkers(map, 
                       lng = df_2015$champion_lon, 
                       lat = df_2015$champion_lat, 
                       radius = 20, weight = 0.25, 
                       opacity = 0.3,
                       fill = T, fillColor = "red", 
                       fillOpacity = 0.3,
                       color = "white")

map




map = addCircleMarkers(map, 
                       lng = 2.19, 
                       lat = 48.52, 
                       radius = 20, weight = 0.25, 
                       opacity = 0.3,
                       fill = T, fillColor = "red", 
                       fillOpacity = 0.3,
                       color = "white")