##########################################################
# Taille et poids moyen des joueurs de l'ASM depuis 1900 #
##########################################################

# source données : http://www.cybervulcans.net/site/
# http://www.cybervulcans.net/modules/bd/stats_taille-poids.php?poste=11&Submit=Envoyer

asm_players = read.csv2("data/asm_players.csv", encoding="UTF-8", stringsAsFactors=FALSE, sep=',')

head(asm_players)

df=ddply(asm_players, .(AnneeNaiss), summarize, mean_size=mean(Taille), mean_wgh=mean(Poids))

head(df)


ggplot(df, aes(AnneeNaiss, y=mean_size)) +
  geom_line() +
  xlab("Année") +
  ylab("Taille en cm") +
  ggtitle("Evolution de la taille moyenne des joueurs en fonction de leur année de naissance")

ggplot(df, aes(AnneeNaiss, y=mean_wgh)) +
  geom_line() +
  xlab("Année") +
  ylab("Poids en kg") +
  ggtitle("Evolution du poids moyen des joueurs en fonction de leur année de naissance")


approxDataWeight = data.frame(
  with(df,
       approx(df$AnneeNaiss, df$mean_wgh, xout = seq(min(df$AnneeNaiss), max(df$AnneeNaiss), by=10), rule=1)
  )
)

dfRealWeight = data.frame(a=df$AnneeNaiss, w=df$mean_wgh)

dfInterpWeight = data.frame(a=approxDataWeight$x, w=approxDataWeight$y)

ggplot() + 
  geom_line(data=dfRealWeight, aes(x=a, y=w), color='black') + 
  geom_line(data=dfInterpWeight, aes(x=a, y=w), color='red') +
  xlab("Année") +
  ylab("Poids en kg") +
  ggtitle("Evolution du poids moyen des joueurs en fonction de leur année de naissance")


approxDataSize = data.frame(
  with(df,
       approx(df$AnneeNaiss, df$mean_size, xout = seq(min(df$AnneeNaiss), max(df$AnneeNaiss), by=10), rule=1)
  )
)

dfRealSize = data.frame(a=df$AnneeNaiss, w=df$mean_size)

dfInterpSize = data.frame(a=approxDataSize$x, w=approxDataSize$y)

ggplot() + 
  geom_line(data=dfRealSize, aes(x=a, y=w), color='black') + 
  geom_line(data=dfInterpSize, aes(x=a, y=w), color='red') +
  xlab("Année") +
  ylab("Taille en cm") +
  ggtitle("Evolution de la taille moyenne des joueurs en fonction de leur année de naissance")

