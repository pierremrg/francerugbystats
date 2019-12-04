##########################################################
# Taille et poids moyen des joueurs de l'ASM depuis 1900 #
##############4############################################
require(ggplot2)
require(plyr)
require(reshape2)


# source donnÃ©es : http://www.cybervulcans.net/site/
# http://www.cybervulcans.net/modules/bd/stats_taille-poids.php?poste=11&Submit=Envoyer

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

###
approxDataWeight = data.frame(
  with(df,
       approx(df$AnneeNaiss, df$mean_wgh, xout = seq(min(df$AnneeNaiss), max(df$AnneeNaiss), by=10), rule=1)
  )
)
dfInterpWeight = data.frame(a=approxDataWeight$x, w=approxDataWeight$y)
###


dfRealWeight = data.frame(a=df$AnneeNaiss, w=df$mean_wgh)

ggplot() + 
  geom_line(data=dfRealWeight, aes(x=a, y=w), color='black') + 
  geom_smooth(data=dfRealWeight, aes(x=a, y=w), color='red', level = 0) + 
#  scale_x_continuous(breaks=seq(min(dfRealWeight$a), max(dfRealWeight$a), 10)) +
  xlab("Année de naissance") +
  ylab("Poids en kg") +
  ggtitle("Poids moyen des joueurs en fonction de leur année de naissance")

###
approxDataSize = data.frame(
  with(df,
       approx(df$AnneeNaiss, df$mean_size, xout = seq(min(df$AnneeNaiss), max(df$AnneeNaiss), by=10), rule=1)
  )
)

dfInterpSize = data.frame(a=approxDataSize$x, w=approxDataSize$y)
###

dfRealSize = data.frame(a=df$AnneeNaiss, w=df$mean_size)


ggplot() + 
  geom_line(data=dfRealSize, aes(x=a, y=w), color='black') + 
  geom_smooth(data=dfRealSize, aes(x=a, y=w), color='red', level=0) + 
  xlab("Année de naissance") +
  ylab("Taille en cm en kg") +
  ggtitle("Evolution de la taille moyenne des joueurs en fonction de leur année de naissance")


####################################
# Evolution du nombre de licenciés #
##############4#####################

nbPlayers = read.csv2("data/nbPlayers.csv", encoding="UTF-8", stringsAsFactors=FALSE, sep=',')

head(nbPlayers)

ggplot() + 
  geom_bar(mapping = aes(x=nbPlayers$Annee, y=nbPlayers$NbLicencies), stat = "identity", color="blue", fill="blue") +
  geom_smooth(data=nbPlayers, aes(x=Annee, y=NbLicencies), color='red', level = 0.90, span = .90) + 
  xlab("Année") +
  xlim(1960, 2020) +
  ylab("Nombre de licenciés") +
  ggtitle("Evolution du nombre de licenciés") +
  annotate("segment", x = 1995, xend = 1995, y = 0, yend = 500000, colour="red") +
  annotate("text", x = 1995, y = -10000, label = "Professionnalisation") +
  annotate("segment", x = 2013, xend = 2013, y = 0, yend = 500000, colour="red") +
  annotate("text", x = 2013, y = -10000, label = "Commotions")




