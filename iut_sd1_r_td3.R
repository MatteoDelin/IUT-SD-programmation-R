library(readxl)
pokemon=read_excel(path="pokemon.xlsx",sheet="pokemon")

dim(pokemon)

summary(pokemon)

pokemon$is_legendary = as.factor(pokemon$is_legendary)
pokemon$generation = as.factor(pokemon$generation)
pokemon$type = as.factor(pokemon$type)

summary(pokemon)

pokemon$attack_group = as.factor(ifelse(test=pokemon$attack >= median(pokemon$attack),yes="attack+",no="attack-"))

summary(pokemon$attack_group)

pokemon$water_fire = as.factor(ifelse(test=pokemon$type %in% c("water","fire"), yes="yes",no="no"))
summary(pokemon$water_fire)

quantile_attack = quantile(x=pokemon$attack,probs=0.75)
quantile_defense = quantile(x=pokemon$defense,probs=0.75)
quantile_speed = quantile(x=pokemon$speed,probs=0.75)
pokemon$best = as.factor(ifelse(test=pokemon$attack >= quantile_attack &
                                      pokemon$defense >= quantile_defense&
                                      pokemon$speed >= quantile_speed
                                      ,yes="yes",no="no"))

summary(pokemon$best)

requete = subset(pokemon, is.na(weight_kg))
View(requete)

requete = subset(pokemon, !is.na(weight_kg))
View(requete)

median_weight=median(pokemon$weight_kg,na.rm = TRUE)
median_height=median(pokemon$height_m,na.rm = TRUE)

pokemon$weight_kgNa = ifelse(test = is.na(pokemon$weight_kg),yes=median_weight,no=pokemon$weight_kg)
pokemon$weight_kgNa = ifelse(test = is.na(pokemon$weight_kg),yes=median_weight,no=pokemon$weight_kg)

pokemon$weight_group = cut(x=pokemon$weight_kg, breaks = 3, labels = c("léger","moyen","lourd"))

pokemon$height_m_group = cut(x=pokemon$height_m, breaks = c(0,1,2,3,max(pokemon$height_m,na.rm = TRUE)))

pokemon$defense_group = cut(pokemon$defense,breaks = quantile(pokemon$defense,na.rm = TRUE),include.lowest = TRUE)
summary(pokemon$defense_group)

aggregate(x = attack ~ type, data = pokemon, FUN = function(x) mean(x))
aggregate(x = attack ~ type+generation, data = pokemon, FUN = function(x) mean(x))

aggregate(x = pokedex_number ~ type, data = pokemon, FUN = function(x) length(x))

aggregate(speed ~ generation + type,data = pokemon, FUN = function(x) c(moy = mean(x),med = median(x),eff = length(x) ) )

