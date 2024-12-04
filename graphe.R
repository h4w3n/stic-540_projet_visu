### initialisation ###
rm(list=ls())
library(ggplot2)

load("C:/Users/tvh/Documents/ULB/4- master stic/STIC B540 - Visualisation des données et de l'information/projet/data01.RData")
View(data)

#### Q1 - offre cinématographique au fil du temps ####


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
