###### Exercice 1

getwd()

setwd(dir="C:/Users/sguedj/Downloads/dataset")

bodies_karts = read.csv(file = "bodies_karts.csv", header = TRUE, sep = ";", dec = ",")
tires = read.csv(file = "tires.csv", header = TRUE, sep = "\t", dec = ",")
gliders = read.csv(file = "gliders.csv", header = TRUE, sep = "|", dec = ".")
drivers = read.csv(file = "drivers.csv", header = TRUE, sep = ";", dec = ",")

dim(bodies_karts)
dim(tires)
dim(gliders)
dim(drivers)

###### Exercice 2

summary(bodies_karts)
summary(tires)
summary(gliders)
summary(drivers)

plot(x = drivers$Weight,
     y = drivers$Acceleration, 
     main = "Drivers : Weight / Acceleration")
#Il semble que les deux variables soient corrélées négativement
#Il y a autant de points mais ils sont superposés car certains drivers ont les mêmes statistiques

correlation = cor(x = drivers$Weight, y = drivers$Acceleration)
print(correlation)

correlation2 = cov(x = drivers$Weight,y = drivers$Acceleration) / (sd(drivers$Weight)* sd(drivers$Acceleration))
print(correlation2)

# OU
#covXY = cov(x = drivers$Weight,
#            y = drivers$Acceleration)
#sX = sd(drivers$Weight)
#sY = sd(drivers$Acceleration)
#print(covXY / (sX*sY))

coefDeter = correlation^2
print(coefDeter)

matriceCor = cor(drivers[ , - 1])
matriceCor = round(matriceCor , 2)
View(matriceCor)
#Toutes les variables semblent fortement corrélées entre elles.

#commande à executer qu'une seule fois
install.packages("corrplot")

library(corrplot)#je charge mon package pour pouvoir utiliser ses fonctionalités
corrplot(matriceCor, method="circle")

matriceCor = round(cor(tires[ , - 1]),1)
corrplot(matriceCor, method="color",  
         type="upper", order="hclust", 
         addCoef.col = "black", # Ajout du coefficient de corrélation
         tl.col="black", tl.srt=45, #Rotation des étiquettes de textes
         # Cacher les coefficients de corrélation sur la diagonale
         diag=FALSE 
)

matriceCor = round(cor(bodies_karts[ , - 1]),1)
corrplot(matriceCor, method="color",  
         type="upper", order="hclust", 
         addCoef.col = "black", # Ajout du coefficient de corrélation
         tl.col="black", tl.srt=45, #Rotation des étiquettes de textes
         # Cacher les coefficients de corrélation sur la diagonale
         diag=FALSE 
)

matriceCor = round(cor(gliders[ , c(- 1,-11)]),1)
corrplot(matriceCor, method="color",  
         type="upper", order="hclust", 
         addCoef.col = "black", # Ajout du coefficient de corrélation
         tl.col="black", tl.srt=45, #Rotation des étiquettes de textes
         # Cacher les coefficients de corrélation sur la diagonale
         diag=FALSE 
)

##### Exercice 3

resultat = drivers[ , c("Driver" , "Weight")]
View(resultat)

resultat = drivers[ 1:10 , c("Driver" , "Acceleration")]
View(resultat)

resultat = drivers[ , -c(5,7,9)]
View(resultat)

resultat = drivers[ , -c(2,3)]

resultat = drivers[ , c("Driver", "Acceleration", "Weight")]
View(resultat)
#Les colonnes sont dans l'ordre défini par le vecteur.

resultat = drivers[ c(3,12,32) , ]
View(resultat)

resultat = drivers[ c(32,3,12) , ]
View(resultat)
#Les lignes sont dans l'ordre défini par le vecteur.

rang = order(drivers$Weight)
resultat = drivers[ rang  , c("Driver", "Weight") ]
View(resultat)

rang = order(drivers$Acceleration, decreasing = TRUE)
resultat = drivers[ rang  , c("Driver", "Acceleration") ]
View(resultat)

rang = order(drivers$Acceleration, drivers$Weight, decreasing = c(TRUE,FALSE))
resultat = drivers[ rang  , c("Driver", "Acceleration","Weight") ]
View(resultat)

help(subset)
topDriver = subset(x = drivers,
                   subset = Acceleration == max(Acceleration), 
                   select = c("Driver","Acceleration"))

topGlider = subset(x = gliders,
                   subset = Acceleration == max(Acceleration), 
                   select = c("Glider","Acceleration"))

topTires = subset(x = tires,
                  subset = Acceleration == max(Acceleration), 
                  select = c("Tire","Acceleration"))

topBody = subset(x = bodies_karts,
                 subset = Acceleration == max(Acceleration), 
                 select = c("Body","Acceleration"))

