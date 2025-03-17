setwd("C:/Users/mdelin/Downloads")

df = read.csv("velov.csv",header = TRUE, sep = ";", dec = ",")

summary(df)
class(df$status)
class(df$CodePostal)

df$status = as.factor(df$status)
df$CodePostal = as.factor(df$CodePostal)

df$bornes = ifelse(df$capacity == df$bikes, yes="OK", no="KO")
table(df$bornes)

#### EXERCICE 2

hist(x = df$capacity, 
     main = "Distribution de \n la capacité des stations",
     col = "red",
     probability = TRUE,
     xlab = "Capacity",
     ylim = c(0,0.08))

abline(h = 100, col = "blue", lty = 2)

lines(density(df$capacity),
      lty = 2,
      col = "blue",
      lwd = 2)

#### EXERCICE 3

boxplot(x = df$capacity, 
        main = "Boxplot de \n la capacité des stations",
        horizontal = FALSE,
        outline = FALSE)

points(mean(df$capacity, na.rm = TRUE), col = "red", pch = 15, cex = 2)


par(mfrow=c(1,2))
#7ème
df7 = subset(df, CodePostal == "69007")
boxplot(x = df7$bikes, 
        main = "Boxplot nb vélos \n 69007",
        ylim = c(0,40))
#8ème
df8 = subset(df, CodePostal == "69008")
boxplot(x = df8$bikes, 
        main = "Boxplot nb vélos \n 69008",
        ylim = c(0,40))

par(mfrow=c(1,1))
boxplot(formula = bikes ~ bonus,
        data = df, 
        main = "Dispo vélos vs Stations Bonus")

means <- tapply(X = df$bikes, 
                INDEX = df$bonus, 
                FUN = function(X) mean(X))
print(means)
points(means, col = "red", pch = 19)

#### EXERCICE 4

effectif = table(df$bonus)
barplot(height = prop.table(effectif),
        main = "Répartition du nombre \n de station bonus",
        horiz = TRUE)

effectif = table(df$banking, df$bonus)
print(effectif)
barplot(height = effectif,
        main = "Bonus vs Banking",
        xlab = "Station Bonus ?")

frequence = prop.table(x = effectif,margin = 2)
barplot(height = frequence,
        main = "Bonus vs Banking",
        xlab = "Station Bonus ?",
        col = c("red","orange"),
        beside = TRUE)

legend_labels <- colnames(frequence)
legend(x = "topright", 
       legend = legend_labels, 
       fill  = c("red","orange"))

etiquette = paste(rownames(effectif),"\n",effectif)
pie(x = effectif,
    main = "Répartition du nombre \n de station bonus",
    col = c("orange","green"),
    labels = etiquette)

effectif = table(df$CodePostal)
top10 = sort(effectif, decreasing = TRUE)[1:10]
barplot(height = top10,
        main = "Top 10 sur le \n nombre de station",
        col = colors(),
        las = 2)  

dev.print(device = png, file = "export.png", width = 600)

#### EXERCICE 5

myColors <- c("blue","red", "green")  
plot(x = df$stands, y = df$capacity,
     main = "Place disponible vs Capacité",
     xlim = c(0, 60),
     ylim = c(0, 60),
     col = myColors[df$bornes],
     pch = 19)
legend("topright", legend = levels(df$bornes),
       col = myColors, pch = 19)

moy_stands = mean(df$stands)
moy_capacity = mean(df$capacity)
points(x = moy_stands,y = moy_capacity, 
       pch = 15,
       col = myColors[3],
       cex = 2)

#### EXERICE 6
library(leaflet)
library(dplyr)
library(ggplot2)
maCarte <- leaflet(df) %>% 
  addTiles() %>% 
  addMarkers(~position_longitude, 
             ~position_latitude, 
             popup = ~address)
maCarte
