#Pour identifier les facteurs qui rendent un film plus ou moins profitable, plusieurs éléments peuvent être analysés, tels que le budget (coût de production), le revenu (box-office), la popularité, la note moyenne, et d’autres caractéristiques comme la durée ou le genre du film. L'objectif ici est de comprendre quelles variables semblent prédire le succès financier d’un film.




#1. Relation entre le budget et le revenu (box-office)
    #Objectif : Étudier si un budget plus élevé correspond à un revenu plus élevée, et si la rentabilité est influencée par la taille du budget.

    #Graphique proposé : Nuage de points pour visualiser la relation entre le budget et le revenu. Une ligne de régression peut être ajoutée pour identifier la tendance.
library(ggplot2)
View(data)

ggplot(data, aes(x = budget, y = revenue)) +
  geom_point(aes(color = popularity), alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE, color = "red") + scale_x_log10() +
  labs(title = "Relation entre le budget et le revenu (box-office)",
       x = "Budget du film",
       y = "Revenu du film",
       color = "Popularité") +
  theme_minimal()

    #Explication : Ce graphique permet d’observer si les films ayant un budget élevé génèrent proportionnellement plus de revenus. L’ajout de la couleur popularité permet de vérifier si la popularité d'un film influence également sa rentabilité.






#2. Profitabilité en fonction du genre du film
    #Objectif : Vérifier si certains genres de films sont plus profitables que d’autres. Cela pourrait aider à identifier les types de films qui génèrent plus de profit par rapport à leur budget.

    #Graphique proposé : Boxplot de la profitabilité (calculée comme revenu - budget) par genre. Ce graphique montre la distribution des profits pour chaque genre.

data$profitability <- data$revenue - data$budget
ggplot(data, aes(x = genres, y = profitability)) +
  geom_boxplot(aes(color = genres)) +
  labs(title = "Profitabilité des films par genre",
       x = "Genre",
       y = "Profitabilité (Revenu - Budget)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

    #Explication : Ce graphique permet de voir quels genres de films ont des marges de profit plus élevées ou plus faibles. Il permet également de comparer la distribution des profits dans les différents genres.

#Cette solution n'a pas fonctionné parce qu'on n'a pas la colonne de profitabilité.






#Pour calculer la profitabilité d’un film (revenu moins budget) et l’intégrer dans le code, je vais ajouter une colonne calculée à mon dataframe.

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

ggplot(data = data, aes(x = profitability, fill = genres)) +
  geom_histogram(binwidth = 5000000, alpha = 0.8, position = "dodge") +
  labs(
    title = "Répartition de la profitabilité des films par genre",
    x = "Profitabilité (en dollars)",
    y = "Nombre de films",
    fill = "Genres"
  ) +
  theme_minimal()

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

#########scale_x_continuous( labels = dollar)+ "poser la question au prof"


#3. Relation entre la popularité et la profitabilité
    #Objectif : Vérifier si la popularité d'un film influence directement sa rentabilité. En effet, un film populaire pourrait être plus susceptible de réaliser des revenus plus élevés.

    #Graphique proposé : Nuage de points pour visualiser la popularité en fonction de la profitabilité.

# Calcul de la profitabilité si ce n'est pas encore fait
data$profitability <- data$revenue - data$budget

# Supprimer les valeurs manquantes ou problématiques
data <- data 
  filter(!is.na(popularity), !is.na(profitability), profitability > 0, popularity > 0)
  
  library(ggplot2)
  View (data)
  ggplot(data = data, aes(x = popularity, y = profitability)) +
    geom_point(alpha = 0.6, color = "blue") +  # Nuage de points
    geom_smooth(method = "lm", color = "red", se = FALSE) +  # Régression linéaire
    scale_y_log10() +  # Échelle logarithmique pour l'axe Y (profitabilité)
    labs(
      title = "Relation entre la popularité et la profitabilité des films",
      x = "Popularité",
      y = "Profitabilité (échelle logarithmique)"
    ) +
    theme_minimal()
#pas très lisible 

  
 #J'ai beaucoup de points et je souhaite une densité visuelle plus claire, je vais utiliser un graphique "hexbin" pour regrouper les points   
  library(ggplot2)
  library(viridis)  # Palette de couleurs
  
  ggplot(data = data, aes(x = popularity, y = profitability)) +
    geom_hex(bins = 30) +  # Hexbin avec 30 hexagones
    scale_fill_viridis(option = "plasma") +  # Palette de couleurs
    scale_y_log10() +  # Échelle logarithmique pour l'axe Y
    labs(
      title = "Relation entre la popularité et la profitabilité des films",
      x = "Popularité",
      y = "Profitabilité (échelle logarithmique)",
      fill = "Densité"
    ) +
    theme_minimal()

#Sans utiliser "viridis"
  
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
  
  
  
  
  
# 6. Relation entre la popularité et les profits par genre
  #Objectif : Identifier si la popularité d’un film dans un genre spécifique a une influence sur sa rentabilité.
  
  #Graphique proposé : Barplot de la moyenne des profits par genre et en fonction de la popularité.
  
  ggplot(data, aes(x = genres, y = profitability, fill = popularity)) +
    geom_bar(stat = "summary", fun = "mean", position = "dodge") + scale_y_log10() +
    labs(title = "Profitabilité moyenne par genre et popularité",
         x = "Genre",
         y = "Profitabilité moyenne (Revenu - Budget)") +
    theme_minimal()
  
  
# Interprétation du Barplot : Relation entre la popularité et les profits par genre
  #Le barplot visualise la profitabilité moyenne (revenus - budget) par genre, tout en tenant compte de la popularité des films. 
  
#1. Relation Popularité - Rentabilité :
  #Les barres représentent la moyenne des profits pour chaque genre, regroupés ou colorés par popularité.
  
  #Si les barres les plus hautes dans un genre sont associées à des niveaux de popularité plus élevés (ex. : teinte ou nuance différente du remplissage de la barre), cela suggère que les films populaires dans ce genre sont généralement plus rentables.
  
  #Si aucune corrélation claire n'est visible entre popularité et profitabilité, cela pourrait indiquer que la popularité d’un film ne garantit pas forcément sa rentabilité dans ce genre.
  
#2. Différences par genre :
  #Certains genres pourraient avoir une moyenne de profitabilité plus élevée indépendamment de la popularité. Cela indique que ces genres, en général, attirent plus de revenus que d'autres.
  
  #Exemple d'interprétation : Si la catégorie "Action" a une moyenne de profitabilité plus élevée que "Drama", cela peut indiquer que les films d'action, même moins populaires, ont un potentiel de profitabilité supérieur.
  
  
#3. Impact des genres populaires :
  #Les genres avec des barres clairement plus hautes pour des films populaires montrent que la popularité joue un rôle important dans leur profitabilité.
  
  #À l’inverse, si la hauteur des barres ne change pas beaucoup entre différentes popularités dans un genre, cela pourrait signifier que d'autres facteurs (comme le budget ou la distribution) influencent davantage la profitabilité que la popularité elle-même.
  
  
#4. Popularité et rentabilité ne sont pas toujours liées :
  #Les films d'un genre spécifique (ex. : "Animation" ou "Family") peuvent être populaires sans pour autant générer des profits élevés. Cela pourrait être dû à des coûts élevés de production ou des marchés limités.
  
  #D'autres genres (ex. : "Horror") peuvent avoir des profits élevés même avec une popularité plus modérée, souvent grâce à des budgets de production relativement faibles.