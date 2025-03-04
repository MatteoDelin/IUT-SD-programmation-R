#### Exercice 1
df = read.csv("fao.csv",sep=";",dec = ",")

nrow(df)

summary(df)

#### Exercice 2
mean(df$Dispo_alim, na.rm = TRUE)

sum(df$Population, na.rm = TRUE)

sd(df$Import_viande,na.rm = TRUE)
sd(df$Export_viande,na.rm = TRUE)

median(df$Prod_viande, na.rm=TRUE)

quantile(df$Dispo_alim, na.rm = TRUE)

quantile(df$Dispo_alim, na.rm = TRUE, probs = seq(0,1,0.01))

#### Exercice 3
minpop = sort(df$Population)[5]
pays_minpop = subset(df,Population <= minpop)
View(pays_minpop)

maxpop = sort(df$Population,decreasing = TRUE)[5]
pays_maxpop = subset(df,Population >= maxpop)
View(pays_maxpop)

maxprod = sort(df$Prod_viande,decreasing = TRUE)[5]
pays_maxprod = subset(df,Prod_viande >= maxprod)
View(pays_maxprod)

maximport = sort(df$Import_viande,decreasing = TRUE)[5]
pays_maximport = subset(df,Import_viande >= maximport)
View(pays_maximport)

pays_bondispo = subset(df,Dispo_alim >= 2300)
nrow(pays_bondispo)
View(pays_bondispo)

pays_bondispo_bonexport = subset(df,Dispo_alim >= 2300 & Export_viande>=1000)
nrow(pays_bondispo_bonexport)
View(pays_bondispo_bonexport)

resultat = subset(df, Nom %in% c("France","Belgique"))
View(resultat)

#### Exercice 4
df$part_export = df$Export_viande/df$Prod_viande

df$Dispo_alim_pays = df$Dispo_alim * df$Population

write.table(x=df,file="ExportTp2.csv")

dispo_alim_mondiale = sum(df$Dispo_alim_pays, na.rm=TRUE)

dispo_alim_mondiale/2300

#### Exercice 5
plot(x = df$Prod_viande,y = df$Export_viande,main = "Pays : Prod_viande / Export_viande")

corelation=cor(x = subset(df,!is.na(Prod_viande) & !is.na(Export_viande))$Prod_viande,y = subset(df,!is.na(Prod_viande) & !is.na(Export_viande))$Export_viande)

View(subset(df,!is.na(Prod_viande) & !is.na(Export_viande)))

matriceCor = cor(df[ , - 1] , use = "complete.obs")
matriceCor = round(matriceCor , 2)
View(matriceCor)

library(corrplot)
corrplot(matriceCor, method="circle")
