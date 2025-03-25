# Créer une toile de fond vide pour le graphique
par(mfrow = c(1,1))
plot(NA, xlim=c(-5, 5), ylim=c(0, 1), xlab="X", 
     ylab="Densité de probabilité", 
     main="Densités de probabilité de lois normales")

mu = c(0,0,0,-2)
sigma = c(0.45,1,2.25,0.7)
colors = c("red", "blue", "green", "orange")
legend_labels = c()

for (i in 1:length(mu)){
  serie = rnorm(n = 1000, 
                mean = mu[i], 
                sd = sigma[i])
  lines(density(serie), col = colors[i])
  legend_labels = c(legend_labels, paste("m =", mu[i], ",", "s =", sigma[i]))
}

# Ajouter une légende
legend("topright", legend=legend_labels, col=colors, lwd=2, cex=0.8)

serie2 =rnorm(n=10000,mean=0,sd=1)

hist(serie2, main = "loi normal centrée-réduite",
     probability = TRUE)
lines(density(serie2))

median(serie2)

quantile(serie2)

quantile(serie2,probs = seq(0,1,0.01))
quantile(serie2,prob=0.95)

qnorm(0.95,0,1)
pnorm(qnorm(0.95,0,1),0,1)

qnorm(0.975,0,1)

1-pnorm(1.96,0,1)

indices_lignes = seq(from = 0, to = 3.9, by = 0.1)
indices_colones = seq(0,0.09,0.01)
#on crée un vecteur vide pour ajouter les probas au fur et à mesure
table = NULL
#On parcourt les indices lignes
for (i in indices_colones){
  all_probas = c()
  for (j in indices_lignes){
    proba = pnorm(q = i+j, mean = 0, sd = 1)
    #on ajoute la nouvelle proba au vecteur existant
    all_probas = c(all_probas,proba)
    all_probas = round(all_probas,digits = 4)
  }
  table = cbind(table,all_probas)
}

class(table)
table = data.frame(table)
colnames(table) = indices_colones
rownames(table) = indices_lignes
View(table)

population = rnorm(10e7,171,9)

mean(population)
sd(population)

hist(population)

#observé
pop190 = population[population < 190]
length(pop190)
length(pop190) / length(population)

#en théorie
pnorm(q = 190, mean=171, sd=9)*10e7

#observé
pop200 = population[population >= 200]
length(pop200)
length(pop200) / length(population)

#en théorie
10e7-pnorm(q = 200, mean=171, sd=9)*10e7

echant = sample(x=population,size=100)
mean(echant)
sd(echant)

taille_int=qnorm(1-0.05/2)*sd(echant)/sqrt(100)
born_inf=mean(echant)-taille_int
born_sup=mean(echant)+taille_int


taille_ech<-100
nb_replicat<-1000
echantillons<-replicate(n = nb_replicat,
                        expr =  sample(population,
                                       taille_ech, 
                                       replace = TRUE))

moyennes<-apply(X = echantillons,
                MARGIN = 2,
                FUN = function(x) mean(x))
ecart_types<-apply(echantillons,
                   MARGIN = 2,
                   FUN = function(x) sd(x))

hist(moyennes)

mean(moyennes)
sd(moyennes)


#observé
moy172 = moyennes[moyennes > 172]
length(moy172)
length(moy172) / length(moyennes)

#en théorie
#proba de P( X < 172cm)
proba_inf_172 = pnorm(q = 172, 
                      mean=171, 
                      sd=9/sqrt(taille_ech))
#proba de P( X >= 172cm)
1 - proba_inf_172

largeur<-apply(X = echantillons,
               MARGIN = 2,
               FUN = function(x) pnorm(0.975)*sd(x)/taille_ech)

borne_inf_IC<-moyennes-largeur
borne_sup_IC<-moyennes+largeur

df = data.frame(largeur,borne_inf_IC,moyennes,borne_sup_IC)
View(df)

moyenne_echantillon <- function(V, n){
  return(mean(sample(V,n)))
}

moyennes_20<-replicate(n = nb_replicat,expr = moyenne_echantillon(V = population,n = 20))
moyennes_30<-replicate(n = nb_replicat,expr = moyenne_echantillon(V = population,n = 30))
moyennes_50<-replicate(n = nb_replicat,expr = moyenne_echantillon(V = population,n = 50))
moyennes_100<-replicate(n = nb_replicat,expr = moyenne_echantillon(V = population,n = 100))
moyennes_500<-replicate(n = nb_replicat,expr = moyenne_echantillon(V = population,n = 500))

par(mfrow=c(2,3))
hist(moyennes_20, xlim=c(161,181), main="20")
hist(moyennes_30, xlim=c(161,181), main="30")
hist(moyennes_50, xlim=c(161,181), main="50")
hist(moyennes_100, xlim=c(161,181), main="100")
hist(moyennes_500, xlim=c(161,181), main="500")


population<-runif(n = 1e7, min = 0, max = 1)
moyennes_20<-replicate(nb_replicat, moyenne_echantillon(population,20))
moyennes_30<-replicate(nb_replicat, moyenne_echantillon(population,30))
moyennes_50<-replicate(nb_replicat, moyenne_echantillon(population,50))
moyennes_100<-replicate(nb_replicat, moyenne_echantillon(population,100))
moyennes_500<-replicate(nb_replicat, moyenne_echantillon(population,500))
par(mfrow=c(2,3))
hist(moyennes_20, xlim=c(0,1), main="20")
hist(moyennes_30, xlim=c(0,1), main="30")
hist(moyennes_50, xlim=c(0,1), main="50")
hist(moyennes_100, xlim=c(0,1), main="100")
hist(moyennes_500, xlim=c(0,1), main="500")

