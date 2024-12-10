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


#carte thermique: genre pour chaque pays
ggplot(data = data) +
  geom_bin2d(mapping = aes(x = production_countries,
                           y = genres,
                           fill = ..count..))



#### Q2 - facteurs film apprécié par les internautes ####
##subset##
# hypothèse les votes des internautes date des années 2000, et à partir de là note des trucs proche de leur date de sortie
film_Y2K = subset(data, release_year >= 2000 & vote_count > 0)
film_Y2K_with_note = subset(film_Y2K, vote_count > 100)
film_with_note = subset(data, vote_count > 100, vote_count)

###genre et vote_average, courbes###
ggplot(data = film_Y2K_with_note) +
  geom_line(mapping = aes(x = release_year, y = vote_average),stat = "summary", fun = "mean") + facet_wrap(~ genres)

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
