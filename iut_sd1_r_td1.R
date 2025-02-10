### Exercice 1
a=5
b=10
resultat=a*b
print(resultat)
A=7.2
B=10.1
resultat=A*B
rm(a,b,A,B,resultat)

### Exercice 2
vec = c(1,2,3,4,5)
class(vec)
vec[3]

v1 = 1:5
v2 = v1+3

v3=1:6
v4=v3**2
v5=v4/2

vecteur=c("Lundi","Mardi","Mercredi","Jeudi","Vendredi","Samedi","Dimanche")
class(vecteur)
vecteur[2]
vecteur[7]

vectbool = c(TRUE,TRUE,FALSE,TRUE)
class(vectbool)

vectdeci = c(7.2,2.6,7.9,14.5)
class(vectdeci)
vectdeci[-3]

vectmoi = c("Janvier","Février","Mardi","Avril","Mai","Juin","Juillet","Août","Septembre","Octobre","Novembre","Décembre")
class(vectmoi)
vectmoi[c(1,2,3)]

vectnega = c(-5:-1)
class(vectnega)
vectnega[c(5,1)]

vectfruit = c("Pomme", "Banane", "Orange", "Fraise", "Ananas")
class(vectfruit)
vectfruit[-c(1,2)]

vectmanq = c(1,2,5,NA,7,NA,8)
class(vectmanq)

sequ = seq(from = 1, to = 10)
length(sequ)

seqpair = 2*sequ
length(seqpair)

seqdecroi = seq(from=0, to=-5)
length(seqdecroi)

seq5 = seq(from=5,to=50,by=5)
length(seq5)

seq10to1 = seq(from=10,to=1)
length(seq10to1)

seq0to1 = seq(from = 0, to = 1, by=0.1)
length(seq0to1)

seq5tonega5 = seq(from=5,to=-5)
length(seq5tonega5)

seq10impair = seq(from=1,to=10,by=2)
length(seq10impair)

vect3 = c(rep(3,times=5))

vectABC = c(rep(c("A","B","C"),times=3))

vect123 = c(rep(c(1:3),times=3))

vectTrueFalse = c(rep(c(TRUE,FALSE),times=4))

rm(list=ls())

### Exercice 3
vectuniform = runif(5,min=0,max=1)
vectuniform
mean(vectuniform)
median(vectuniform)
min(vectuniform)
max(vectuniform)

vectuniform2 = runif(10,min=-5,max=5)
vectuniform2
mean(vectuniform2)
median(vectuniform2)
min(vectuniform2)
max(vectuniform2)

vectuniform3 = runif(100,min=10,max=20)
vectuniform3
mean(vectuniform3)
median(vectuniform3)
min(vectuniform3)
max(vectuniform3)

vectuniform4 = runif(15,min=50,max=100)
vectuniform4
mean(vectuniform4)
median(vectuniform4)
min(vectuniform4)
max(vectuniform4)

vectnorm = rnorm(20,-2,3)
mean(vectnorm)
sd(vectnorm)
hist(vectnorm)

vectnorm1 = rnorm(2000,-2,3)
mean(vectnorm1)
sd(vectnorm1)
hist(vectnorm1)

vectnorm2 = rnorm(2000,0,1)
mean(vectnorm2)
sd(vectnorm2)
hist(vectnorm2)

quantile(vectnorm2,0.25)
quantile(vectnorm2,0.50)
quantile(vectnorm2,0.75)

quantile(vectnorm2, probs = seq(0,1,0.1))

quantile(vectnorm2, probs = seq(0,1,0.01))

salaire=rnorm(3000,2400,300)
mean(salaire)
sd(salaire)

salaire=round(salaire,2)

sum(salaire)

median(salaire)

print(paste("Le 1% des salaires les plus haut sont supérieur à",quantile(salaire,0.99)))

print(paste("Le 20% des salaires les plus bas sont inférieur à",quantile(salaire,0.2)))

de = 1:6
lancer = sample(de,1)

lancer12 = sample(de,12,TRUE)

unique(lancer12)

tableau = sort(table(lancer12))
tableau1 = sort(prop.table(lancer12))

simulation <- sample(x = c(1,2,3,4,5,6), size = 100000, replace = TRUE)

simulation <- sample(x = c(1,2,3,4,5,6), size = 100000, replace = TRUE)
frequence <- prop.table( table(simulation) )
sort(frequence, decreasing = TRUE)
rm(list=ls())
