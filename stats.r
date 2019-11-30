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

head(df)

df=ddply(details, .(year), summarize, mean_tries=mean(tries), mean_pens=mean(pens))

ggplot(df, aes(year, y=pens)) +
  geom_line() +
  xlab("Année") +
  scale_x_discrete(limits=c(1950:max(names$Année)))
  #ylab("Longueur") +
  #ggtitle("Longueur moyenne des noms")

approxData = data.frame(
  with(df,
       approx(df$year, df$mean_pens, xout = seq(min(df$year), max(df$year), by=10), rule=1)
  )
)

ggplot(approxData, aes(x, y)) +
  geom_line(col = "blue")


approxData <- data.frame(
  with(df, 
       approx(year, mean_pens, xout = seq(1, 1000, by = 10), method = "linear")
  ),
  method = "approx()"
)

approxData

ggplot(approxData, aes(year, mean_pens)) + 
  #geom_point(df = df, aes(year, mean_pens), alpha = 0.2, col = "red") +
  geom_line(col = "blue") +
  facet_wrap(~method) +
  ggtitle("Interpolation and smoothing functions in R") +
  theme_bw(16)









