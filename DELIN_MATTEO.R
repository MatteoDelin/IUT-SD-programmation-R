##### Exercice 1
### Question 1
dfEpisode = read.csv("TheOffice.csv",header = TRUE,sep = ";",dec = ",")
dfDialogue = read.csv("TheOfficeDialogue.csv",header = TRUE,sep = ";",dec = ",")

### Question 2
dfEpisode$Season = as.factor(dfEpisode$Season)
dfEpisode$Episode_Number = as.factor(dfEpisode$Episode_Number)
dfDialogue$Season = as.factor(dfDialogue$Season)
dfDialogue$Episode_Number = as.factor(dfDialogue$Episode_Number)
dfDialogue$Character = as.factor(dfDialogue$Character)

##### Exercice 2
### Question 1
topDialogue = sort(table(dfDialogue$Character),decreasing = TRUE)[1:10]
barplot(topDialogue,main = "Repartion des dialogues en fonction de la personne",xlab = "Personnage",ylab = "Nombre de dialogue")
# On voit que Micheal et le personnage avec le plus de dialogue avec plus de 10 000 lignes et le dixième est Ryan avec à peu près 1 000 lignes

### Question 2
dureeSerie = sum(dfEpisode$Duration)/60
# La série dure un total de 84.76 heures

### Question 3
deciNotes = quantile(dfEpisode$Ratings,probs = seq(0,1,0.1))
hist(dfEpisode$Ratings,probability = TRUE, main = "Repartition des notes des épisodes",xlab = "Note des épisodes")
lines(density(dfEpisode$Ratings))
# La courbe de densité suit l'histogramme

### Question 4
par(mfrow = c(1,1))
plot(NA, xlim=c(-5, 5), ylim=c(0, 10), xlab="X", 
     ylab="Densité de probabilité", 
     main="Densités de probabilité \n de lois normales")
note_saison = aggregate(x = Ratings ~ Season, data = dfEpisode, FUN = function(x) c(x))
for (i in length(note_saison$Season)){
  lines(density(note_saison[i,2]))
}





