### initialisation ###
rm(list=ls())
library(ggplot2)

load("C:/Users/tvh/Documents/ULB/4- master stic/STIC B540 - Visualisation des données et de l'information/projet/data01.RData")
View(data)

#### Q1 - offre cinématographique au fil du temps ####

# évolution sortie de film
ggplot(data = data) +
  geom_bar(mapping = aes(x = release_year, y = ..count..))

#evolution sortie de film par mois ==> les mois avec le plus de sortie de film
ggplot(data = data) +
  geom_bar(mapping = aes(x = release_month, y = ..count..))


#genre par pays de production
ggplot(data = data) +
  geom_bar(mapping = aes(x = genres, y = ..count..))+
  facet_wrap(~production_countries)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



#evolution des genres à travers le temps
data_genres <- as.data.frame(table(data$release_year,data$genres))
colnames(data_genres) <- c("release_year","genres", "count_genre")

ggplot(data = data_genres)+
  geom_line(mapping = aes(x = release_year,y = count_genre,color = genres ,group = genres)) +
  scale_x_discrete(breaks = seq(1915,2020, by = 15)) +
  facet_wrap(~genres)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#boite à moustache du runtime
ggplot(data = data) +
  geom_boxplot(mapping = aes(x = release_year, y = runtime)) +
  scale_x_discrete(breaks = seq(1915,2020, by = 15)) +
  facet_wrap(~production_countries)+
  coord_flip()+
  scale_y_log10()


#sortie film chaque année par pays
#enleve col = white pour voir que en 1960, il y a 1 film russe sorti
ggplot(data = data) +
  geom_histogram(mapping = aes(x = release_year, fill = production_countries), col = "red", binwidth = 5) + 
  scale_x_continuous(breaks = seq(1900,2025, by = 5)) + 
  coord_flip()

ggplot(data = data) +
  geom_histogram(mapping = aes(x = release_year, y = ..count..), col = "red", binwidth = 10) + 
  scale_x_continuous(breaks = seq(1900,2025, by = 10)) + 
  facet_wrap(~production_countries)+
  coord_flip()


ggplot(data = data) +
  geom_jitter(mapping = aes(x = release_year,
                           y = production_countries,
                           size = runtime), alpha = 0.5 )


#carte thermique: genre pour chaque pays/langues
ggplot(data = data) +
  geom_bin2d(mapping = aes(x = production_countries,
                           y = genres,
                           fill = ..count..))

ggplot(data = data) +
  geom_bin2d(mapping = aes(x = original_language,
                           y = genres,
                           fill = ..count..))

#### Q2 - facteurs film apprécié par les internautes ####
##subset##
# hypothèse les votes des internautes date des années 2000, et à partir de là note des trucs proche de leur date de sortie
film_Y2K = subset(data, release_year >= 2000 & vote_count > 0)
film_Y2K_with_note = subset(film_Y2K, vote_count > 100)
#film_with_note = subset(data, vote_count > 100, vote_count)
film_with_note = subset(data, vote_count > 100)

###genre et vote_average, courbes###
ggplot(data = film_Y2K_with_note) +
  # geom_line(mapping = aes(x = release_year, y = vote_average),stat = "summary", fun = "mean") + 
  geom_line(mapping = aes(x = release_year, y = vote_average),stat = "summary", fun = "median") +
  geom_point(mapping = aes(x = release_year, y = vote_average),stat = "summary", fun = "median") +
  facet_wrap(~ genres)

###genre et vote_average, nuage de point###
ggplot(film_Y2K_with_note) + 
  geom_point(mapping = aes(x = release_year, y = vote_average, size = vote_count, color=genres), alpha = 0.5)

ggplot(film_Y2K_with_note) + 
  geom_point(mapping = aes(x = release_year, y = vote_average, size = vote_count, color=genres), alpha = 0.5) + facet_wrap(~production_countries)

###genre et vote_average, pays, carte thermique###
ggplot(data =film_Y2K) +
  geom_raster(mapping = aes(x = release_year, y = genres, fill = vote_average)) +
  scale_fill_gradient(low = "white", high = "red") + facet_wrap(~production_countries)

ggplot(data =film_Y2K_with_note) +
  geom_raster(mapping = aes(x = production_countries, y = genres, fill = vote_average)) +
  scale_fill_gradient(low = "white", high = "red")

###popularité et vote_average, nuage de point###
# les films ultra ultra populaire (extreme) ont une bonne note (au dessus de 7)
# les films pas populaire (en dessous de 10) ont quand meme des notes en majorité au dessus de la moyenne
#--------
ggplot(film_with_note) + 
  geom_point(mapping = aes(x = popularity, y = vote_average)) + scale_x_log10()

ggplot(film_with_note) + 
  geom_point(mapping = aes(x = popularity, y = vote_average, size = vote_count)) + scale_x_log10()

ggplot(film_with_note) + 
  geom_point(mapping = aes(x = popularity, y = vote_average)) + scale_x_log10() + facet_wrap(~ genres)

###durée du film et vote_average####
# runtime y'a des films d'un minute wtf
#--------
ggplot(film_with_note) + 
  geom_point(mapping = aes(x = runtime, y = vote_average))
#--------
#film long ont de bonne notes
# film très court aussi, et film longueur moyenne note cv, pas vraiment un critère (le montrer et dire que ce n'est pas un critère ? +/-)



#### Q3 - facteurs film est profitable / rentable ####
data$profitability <- data$revenue - data$budget

## Générer un graphique pour analyser la profitabilité
ggplot(data = data, aes(x = budget, y = profitability, color = genres)) +
  geom_point(alpha = 0.7) + scale_y_log10() + facet_wrap(~genres)
labs(
  title = "Analyse de la profitabilité des films en fonction du budget",
  x = "Budget (en dollars)",
  y = "Profitabilité (en dollars)",
  color = "Genres"
) +
  theme_minimal()
#### graphe à garder pour le projet !!!!

#Ajout de léchelle logarithmique
ggplot(data = data, aes(x = profitability, fill = genres)) +
  geom_histogram(binwidth = 5000000, alpha = 0.8, position = "dodge") +
  scale_x_log10() +  # Applique une échelle logarithmique sur l'axe X 
  labs(
    title = "Répartition de la profitabilité des films par genre",
    x = "Genres",
    y = "Nombre de films",
    fill = "Genres"
  ) +
  theme_minimal()

####poser la question : que mettre en axe des X ?
#ce graphe est intéressant : 
#Dominance de certains genres : On peut observer quels genres ont tendance à être plus souvent associés à une forte profitabilité.


#Créer un graphique classé + log10 pour l'axe Y + Les genres sont classés par ordre décroissant de profitabilité moyenne.
library(scales)
ggplot(data = data, aes(x = genres, y = profitability, fill = genres)) +
  geom_boxplot(alpha = 0.7) +  # Utilisation d'un boxplot pour visualiser les distributions
  coord_flip() +  # Inverser les axes pour une meilleure lisibilité
  scale_y_log10() +  # Échelle logarithmique pour l'axe Y 
  labs(
    title = "Profitabilité des films par genre",
    x = "Genres",
    y = "Profitabilité (en dollars)"
  ) +
  theme_minimal() +
  theme(legend.position = "none")  # Supprime la légende si elle est redondante

#########scale_x_continuous( labels = dollar)+ "poser la question au prof

#3. Relation entre la popularité et la profitabilité
#Objectif : Vérifier si la popularité d'un film influence directement sa rentabilité. En effet, un film populaire pourrait être plus susceptible de réaliser des revenus plus élevés.

#Graphique proposé : Nuage de points pour visualiser la popularité en fonction de la profitabilité.
# Calcul de la profitabilité si ce n'est pas encore fait
data$profitability <- data$revenue - data$budget

# Supprimer les valeurs manquantes ou problématiques
data <- data 
filter(!is.na(popularity), !is.na(profitability), profitability > 0, popularity > 0)

#J'ai beaucoup de points et je souhaite une densité visuelle plus claire, je vais utiliser un graphique "hexbin" pour regrouper les points   
library(ggplot2)
library(viridis)  # Palette de couleurs

ggplot(data = data, aes(x = popularity, y = profitability)) +
  geom_hex(bins = 30) +  # Crée un graphique hexbin
  scale_fill_gradient(low = "blue", high = "red") +  # Palette de couleurs simple
  scale_x_log10() +  # Échelle logarithmique pour la profitabilité
  scale_y_log10() + 
  labs(
    title = "Relation entre la popularité et la profitabilité des films",
    x = "Popularité",
    y = "Profitabilité (échelle logarithmique)",
    fill = "Densité"
  ) +
  theme_minimal()

#Sans utiliser "viridis"


#ce graphe est intéressant : Relation entre popularité et profitabilité :

#Si le rouge se situe dans la partie haute de l'axe Y, cela signifie que les films les plus profitables ont tendance à avoir une popularité élevée.

#Films très profitables et populaires :

#Ces films sont probablement des blockbusters (grandes productions) ou des films ayant bénéficié d'une large diffusion et d'une forte acceptation du public.
#Exemples : franchises célèbres, films avec un fort marketing ou acteurs connus.

#Films moins profitables et moins populaires (parties basses et bleues) :

#Les films ayant une faible densité dans cette région peuvent être des films à budget limité, à audience restreinte ou ayant rencontré des échecs critiques ou commerciaux.

#Interprétation sociologique et économique :

# Blocbusters dominent : Les films très populaires dominent également en termes de profitabilité, illustrant l'effet des grandes campagnes marketing et des sorties mondiales.

#Échelle logarithmique : Elle montre que, bien que certains films soient extrêmement profitables, la majorité se situe dans une gamme plus modeste.

#Zone rouge en haut : Cela peut être vu comme une corrélation entre la popularité (attractivité auprès du public) et le succès financier.



#4. Distribution des profits selon la durée (runtime) 

library(ggplot2)

# Graphique de dispersion avec une courbe de tendance
ggplot(data = data, aes(x = runtime, y = profitability)) +
  geom_point(alpha = 0.6, color = "blue") +  # Points pour chaque film
  geom_smooth(method = "loess", color = "red", se = TRUE) +  # Courbe de tendance lissée
  scale_y_log10() +  # Échelle logarithmique pour la profitabilité
  labs(
    title = "Relation entre la durée des films et leur profitabilité",
    x = "Durée du film (en minutes)",
    y = "Profitabilité (échelle logarithmique, en dollars)"
  ) +
  theme_minimal()

#se = FALSE : Supprime la bande d'erreur autour de la courbe pour garder le graphique lisible. Pour obtenir le contraire : se = TRUE
#la carte thermique qui suit est plus lisible que celle-ci

# Carte thermique pour visualiser la relation entre la durée des films (runtime) et leur profitabilité (profitability)

ggplot(data, aes(x = runtime, y = profitability)) +
  geom_bin2d(bins = 30) +  # Histogramme bidimensionnel avec 30 bins
  scale_fill_viridis_c(option = "plasma") +  # Palette de couleurs lisible
  scale_y_log10() +  # Échelle logarithmique pour la profitabilité
  labs(
    title = "Carte thermique de la durée des films et leur profitabilité",
    x = "Durée du film (en minutes)",
    y = "Profitabilité (échelle logarithmique, en dollars)",
    fill = "Fréquence"
  ) +
  theme_minimal()
# ce graphique en carte thermique est très intéressant : 
# 1. Observation des films rentables (axe Y élevé) :
#Les films d'une durée proche de 1h40 (100 minutes) semblent apparaître fréquemment parmi les films ayant une profitabilité élevée.Cela pourrait indiquer que cette durée est optimale pour maximiser les profits dans l'industrie cinématographique.

# 2. Potentiels facteurs explicatifs :
#Préférences des spectateurs : Une durée d'environ 1h30 à 1h45 pourrait correspondre au format préféré du grand public, offrant un équilibre entre immersion et attention.

#Coûts de production : Les films d'environ 1h40 pourraient être plus abordables à produire que des films très longs (réduction des coûts de tournage et post-production).

#Distribution et marketing : Les films de cette durée sont peut-être plus faciles à projeter plusieurs fois dans une journée dans les salles de cinéma, augmentant ainsi leur rentabilité.

#3. Points à approfondir :
#Genre des films : Les genres les plus populaires (comédies, thrillers, animations) pourraient correspondre à cette durée et contribuer au succès financier.

#Budget initial : Cette catégorie de films pourrait bénéficier d'un rapport coût/bénéfice optimal.

#Popularité et notes : Il serait intéressant de voir si les films de 1h40 sont également les mieux notés ou les plus populaires.

#5. Impact de la langue parlée sur la rentabilité :

library(ggplot2)

# Créer un boxplot de la profitabilité par langue parlée
ggplot(data, aes(x = spoken_languages, y = profitability, fill = spoken_languages)) +
  geom_boxplot(outlier.colour = "red", outlier.shape = 1, alpha = 0.7) +  # Boxplot avec points rouges pour les outliers
  coord_flip() +  # Affiche le graphique horizontalement
  scale_y_log10() +  # Échelle logarithmique pour réduire l'effet des valeurs extrêmes
  scale_fill_viridis_d(option = "turbo", guide = "none") +  # Palette de couleurs discrètes et esthétique
  labs(
    title = "Impact de la langue parlée sur la rentabilité des films",
    x = "Langue parlée",
    y = "Profitabilité (échelle logarithmique, en dollars)"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Ajuste les étiquettes pour une meilleure lisibilité




#filtrer les langues
library(ggplot2)

# Filtrer les données pour ne conserver que les langues spécifiées
data_filtered <- subset(data, spoken_languages %in% c("Arabic", "English", "Russian", "Italian", "German", "French"))

# Créer un boxplot de la profitabilité par langue parlée avec le sous-ensemble filtré
ggplot(data_filtered, aes(x = spoken_languages, y = profitability, fill = spoken_languages)) +
  geom_boxplot(outlier.colour = "red", outlier.shape = 1, alpha = 0.7) +  # Boxplot avec points rouges pour les outliers
  coord_flip() +  # Affiche le graphique horizontalement
  scale_y_log10() +  # Échelle logarithmique pour réduire l'effet des valeurs extrêmes
  scale_fill_viridis_d(option = "turbo", guide = "none") +  # Palette de couleurs discrètes et esthétique
  labs(
    title = "Impact de la langue parlée sur la rentabilité des films",
    x = "Langue parlée",
    y = "Profitabilité (échelle logarithmique, en dollars)"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Ajuste les étiquettes pour une meilleure lisibilité

#1. Comparaison de la médiane par langue :

#Chaque boxplot représente la distribution des valeurs de profitabilité pour une langue donnée.
#La médiane est indiquée par la ligne dans chaque boîte. Si certaines langues ont une médiane plus élevée que d'autres, cela signifie que, généralement, les films dans cette langue ont une profitabilité plus élevé.

#Exemple d'interprétation : Si la médiane de la profitabilité est élevée pour English et faible pour Arabic, cela pourrait indiquer que les films en anglais sont généralement plus rentables que ceux dans d'autres langues.

#2. Écarts-types et dispersion des valeurs :
#La taille de la boîte (interquartile range) indique la variabilité des valeurs de profitabilité autour de la médiane. Une boîte plus étendue signifie que la profitabilité des films dans cette langue est très variable.

#Les "outliers", représentés par des points situés au-dessus ou en dessous de la boîte, indiquent des films qui ont une profitabilité exceptionnellement élevée ou faible comparée à la plupart des autres films dans cette langue.

#Exemple d'interprétation : Si English a une boîte très étendue, cela signifie qu'il existe une grande diversité dans les profits des films en anglais : certains films sont très rentables, tandis que d'autres le sont moins.

#3. Présence des valeurs extrêmes :
#Les points rouges (les outliers) montrent les films ayant des valeurs de profitabilité exceptionnellement hautes ou basses dans leur catégorie respective.

#Un grand nombre d'outliers dans une certaine langue pourrait indiquer un phénomène particulier, comme des films avec des succès inattendus ou des échecs financiers.

#Exemple d'interprétation : Si un grand nombre de valeurs très élevées sont observées pour English, cela peut être le signe que certains films en anglais bénéficient d'une large audience mondiale ou d'une production importante avec un grand budget.

#4. Différences significatives entre langues :
#Si certaines langues montrent systématiquement des valeurs de profitabilité plus élev sur l'axe X, cela peut suggérer qu'elles sont plus susceptibles d'attirer un public mondial ou d'avoir une stratégie marketing plus efficace.
#Par exemple, l'anglais pourrait dominer en raison de sa capacité à toucher un public plus large avec des sous-titres ou des doublages dans divers pays.



