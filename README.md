# Projet R : francerugbystats

Le rapport PDF contient une analyse de l'évolution du rugby et de l'influence de la profesionnalisation sur celui-ci au fil des années. Ce repo contient le code nécessaire à la rédaction du rapport.

# Sources de données

Le répertoire `data/` contient les données utilisées dans notre script R.

Ces données proviennent de sources publiques différentes, et ont été "scrapées" (le script Python utilisé est disponible dans le même dossier) et réarrangées à posteriori.

**Données utilisées :**

- `asm_players.csv` : Informations sur les joueurs du club de Clermont-Ferrand (Source : http://www.cybervulcans.net/modules/bd/stats_taille-poids.php?poste=11&Submit=Envoyer)
- `details_scores.csv` : Informations sur les matchs avec notamment le nombre d'essais et de pénalités marqués (Source : http://en.espn.co.uk/statsguru/rugby/team/9.html?class=1;type=team)
- `finalists.csv` : Historique des clubs finalistes du Top 14 (Sources : LNR & Google Maps)
- `matches_list.csv` : Historique des matches de l'équipe de France de rugby (Source : http://en.espn.co.uk/statsguru/rugby/team/9.html?class=1;type=team)
- `nbPlayers.csv` : Informations sur le nombre de licenciés en France (Source : https://fr.wikipedia.org/wiki/F%C3%A9d%C3%A9ration_fran%C3%A7aise_de_rugby)
- `opponents_list.csv` : Informations sur les équipes affrontées par l'équipe de France de rugby (Source : http://en.espn.co.uk/statsguru/rugby/team/9.html?class=1;type=team)
