require(ggplot2)
require(plyr)
require(reshape2)
require(gridExtra)
require(scales)
require(leaflet)

########################################################
# Pourcentage de victoire de l'équipe de France par an #
########################################################

matches = read.csv2("data/matches_list.csv", encoding="UTF-8", stringsAsFactors=FALSE, sep=',')

df=as.data.frame(matches)

head(df)

won = matches[matches$result == 'won', ]
df_won = as.data.frame(aggregate(result ~ year, won, FUN=length))

lost = matches[matches$result == 'lost', ]
df_lost = as.data.frame(aggregate(result ~ year, lost, FUN=length))

#head(df_won)
#head(df_lost)

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
#head(df_result, 10)


ggplot(df_result, aes(year, y=percent_won)) +
  geom_line() +
  xlab("Année") +
  ylab("Nombre de victoires") +
  ggtitle("Nombre moyen de victoires de l'équipe de France par an")



################################################
# Moyenne du nombre de points par match par an #
################################################

matches = read.csv2("data/matches_list.csv", encoding="UTF-8", stringsAsFactors=FALSE, sep=',')

df=as.data.frame(matches)

# On enleve les matchs dupliques (a domicile + a l'exterieur)
df = df[!duplicated(df), ]

#head(df)

# Calcul du nombre total de points
df$totalPoints = df$pointsFor + df$pointsAgainst

# Calcul de la moyenne des points a partir de 1950 (plus representatif)
df_points = ddply(df, .(year), summarize, mean_points = mean(totalPoints))
df_points = df_points[df_points$year >= 1950, ]

ggplot() +
  geom_line(data=df_points, aes(year, mean_points)) +
  geom_smooth(data=df_points, aes(x=year, y=mean_points), color='orange', level = 0) +
  xlab("Année") +
  ylab("Nombre de points moyen") +
  ggtitle("Evolution du nombre de points par match en fonction des années") +
  geom_vline(xintercept = 1995, linetype = "dotted", color="black", size=1.5) +
  annotate("text", x=1993, y=15, label="Pro", angle=90)


#########################################################
# Moyenne du nombre d'essais/pénalités par match par an #
#########################################################

details = read.csv2("data/details_scores.csv", encoding="UTF-8", stringsAsFactors=FALSE, sep=',')

df=as.data.frame(details)

# On supprime l'annee en cours (donnees incompletes)
df = df[df$year < 2019,]

df=ddply(df, .(year), summarize, mean_tries=mean(tries), mean_pens=mean(pens))

ggplot() +
  geom_point(data=df, aes(year, mean_tries)) +
  geom_smooth(data=df, aes(x=year, y=mean_tries), color='blue', level = 0) +
  xlab("Année") +
  ylab("Nombre d'essais moyen") +
  ggtitle("Evolution du nombre d'essais par match en fonction des années") +
  scale_y_discrete(limits=c(floor(min(df_smooth$mean_tries)):ceiling(max(df_smooth$mean_tries))))

ggplot() +
  geom_point(data=df, aes(year, mean_pens)) +
  geom_smooth(data=df, aes(x=year, y=mean_pens), color='red', level = 0) +
  xlab("Année") +
  ylab("Nombre de pénalités moyen par match") +
  ggtitle("Evolution du nombre de pénalités par match en fonction des années") +
  scale_y_discrete(limits=c(floor(min(df_smooth$mean_tries)):ceiling(max(df_smooth$mean_tries))))


# Fonction qui renvoie une partie des lignes d'une dataframe autour de l'annee indiquee
# Fonction non utilisee, finalement remplacee par geom_smooth() qui offre un rendu plus joli
df_windows_years = function(df, year, w = 5){
  #min_year = floor(year/w)*w
  #max_year = min_year + w
  min_year = year - w/2
  max_year = year + w/2
  df = df[df$year >= min_year & df$year <= max_year, ]
  return(df)
}

# Utilisation de la fonction de moyenne
df_smooth = ddply(df, .(year, mean_tries, mean_pens), summarize,
                  mean_tries_smooth=mean(df_windows_years(df, year, 30)$mean_tries),
                  mean_pens_smooth=mean(df_windows_years(df, year, 30)$mean_pens)
)

ggplot() +
  geom_line(data=df_smooth, aes(year, mean_tries), color='gray') +
  geom_line(data=df_smooth, aes(year, mean_tries_smooth), color='blue') +
  scale_y_discrete(limits=c(floor(min(df_smooth$mean_tries)):ceiling(max(df_smooth$mean_tries)))) +
  xlab("Année") +
  ylab("Nombre d'essais moyen") +
  ggtitle("Evolution du nombre d'essais par match en fonction des années")

# Plot penalites en utilisant la fonction de moyenne
ggplot() +
  geom_line(data=df_smooth, aes(year, mean_pens), color='black') +
  geom_line(data=df_smooth, aes(year, mean_pens_smooth), color='red') +
  xlab("Année") +
  ylab("Nombre de pénalités moyen") +
  ggtitle("Evolution du nombre de pénalités par match en fonction des années")


#####################
# Villes finalistes #
#####################

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
             radius=min(40, 15+5*appearances)
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
                         fillOpacity = 0.7,
                         color = "white")
  
  mean_lat = sum(df$lat * df$appearances)/sum(df$appearances)
  mean_lon = sum(df$lon * df$appearances)/sum(df$appearances)
  
  map = addPolylines(map, lat = c(mean_lat, mean_lat), lng = c(-180,180), color = "red")
  map = addPolylines(map, lat = c(-90, 90), lng = c(mean_lon, mean_lon), color = "red")
  
  
  return(map)
}



finalists = read.csv2("data/finalists.csv", encoding="UTF-8", stringsAsFactors=FALSE, sep=',')

df=as.data.frame(finalists)
df = format_cities_data(df, 1900, 1919)
nrow(df)
map = create_cities_map(df)
map

df=as.data.frame(finalists)
df = format_cities_data(df, 1920, 1939)
nrow(df)
map = create_cities_map(df)
map

df=as.data.frame(finalists)
df = format_cities_data(df, 1940, 1959)
nrow(df)
map = create_cities_map(df)
map

df=as.data.frame(finalists)
df = format_cities_data(df, 1960, 1979)
nrow(df)
map = create_cities_map(df)
map

df=as.data.frame(finalists)
df = format_cities_data(df, 1980, 1999)
nrow(df)
map = create_cities_map(df)
map

df=as.data.frame(finalists)
df = format_cities_data(df, 2000, 2019)
nrow(df)
map = create_cities_map(df)
map


##########################################################
# Taille et poids moyen des joueurs de l'ASM depuis 1900 #
##############4############################################

asm_players = read.csv2("data/asm_players.csv", encoding="UTF-8", stringsAsFactors=FALSE, sep=',')

head(asm_players)

df=ddply(asm_players, .(AnneeNaiss), summarize, mean_size=mean(Taille), mean_wgh=mean(Poids))

head(df)


ggplot(df, aes(AnneeNaiss, y=mean_size)) +
  geom_line() +
  xlab("AnnÃ©e") +
  ylab("Taille en cm") +
  ggtitle("Evolution de la taille moyenne des joueurs en fonction de leur annÃ©e de naissance")

ggplot(df, aes(AnneeNaiss, y=mean_wgh)) +
  geom_line() +
  xlab("AnnÃ©e") +
  ylab("Poids en kg") +
  ggtitle("Evolution du poids moyen des joueurs en fonction de leur annÃ©e de naissance")



dfRealWeight = data.frame(a=df$AnneeNaiss, w=df$mean_wgh)

plotWeight = ggplot() + 
  geom_line(data=dfRealWeight, aes(x=a, y=w), color='black') + 
  geom_smooth(data=dfRealWeight, aes(x=a, y=w), color='red', level = 0) + 
  #  scale_x_continuous(breaks=seq(min(dfRealWeight$a), max(dfRealWeight$a), 10)) +
  xlab("Année de naissance") +
  ylab("Poids en kg") +
  ggtitle("Poids moyen des joueurs en fonction de leur année de naissance")



dfRealSize = data.frame(a=df$AnneeNaiss, w=df$mean_size)

plotHeight = ggplot() + 
  geom_line(data=dfRealSize, aes(x=a, y=w), color='black') + 
  geom_smooth(data=dfRealSize, aes(x=a, y=w), color='red', level=0) + 
  xlab("Année de naissance") +
  ylab("Taille en cm") +
  ggtitle("Taille moyenne des joueurs en fonction de leur année de naissance")


grid.arrange(plotWeight, plotHeight, nrow=2)


####################################
# Evolution du nombre de licenciés #
##############4#####################

nbPlayers = read.csv2("data/nbPlayers.csv", encoding="UTF-8", stringsAsFactors=FALSE, sep=',')

head(nbPlayers)



ggplot() + 
  geom_bar(mapping = aes(x=nbPlayers$Annee, y=nbPlayers$NbLicencies), stat = "identity", color="blue", fill="blue") +
  geom_smooth(data=nbPlayers, aes(x=Annee, y=NbLicencies), color='red', level = 0.90, span = .94) + 
  xlab("Année") +
  xlim(1960, 2020) +
  ylab("Nombre de licenciés") +
  scale_y_continuous(labels = comma) +
  ggtitle("Evolution du nombre de licenciés") +
  geom_vline(xintercept = 1995, linetype="dotted",color = "black", size=1.5) +
  annotate("text", x = 1993, y = 50000, label = "Pro", angle=90)

