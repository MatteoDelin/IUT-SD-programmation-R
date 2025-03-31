setwd("C:/Users/tmargerand/Downloads/IUT_SD1/dataset/nba")
getwd()

fichiers = list.files(path = getwd(),
                      pattern = ".csv$",
                      full.names = TRUE)
library("tools")

print(fichiers[1])
nom_fichier = basename(path = fichiers[1])
nom_fichier_sans_extension = file_path_sans_ext(x = nom_fichier)
print(nom_fichier_sans_extension)

# Lire le fichier CSV et l'affecter à une variable avec le nom du fichier
assign(x = nom_fichier_sans_extension, 
       value = read.csv(fichiers[1],
                        sep = ",",
                        dec = "."))
#un dataframe vient d'être créé avec comme nom d'objet le nom du fichier sans extension.

for (i in 1:length(fichiers)){
  nom_fichier = basename(path = fichiers[i])
  nom_fichier_sans_extension = file_path_sans_ext(x = nom_fichier)
  start_time <- Sys.time()
  assign(x = nom_fichier_sans_extension, 
         value = read.csv(fichiers[i],
                          sep = ",",
                          dec = "."))
  end_time <- Sys.time()
  # Calcul du temps écoulé
  execution_time <- end_time - start_time
  cat("Importation : ",nom_fichier_sans_extension, "=" , execution_time , "\n")
}

df_x = subset(team, city == "Los Angeles", select = c("id", "city"))
df_y = subset(game, select = c("game_id", "team_id_home"))
dfJoin = merge(x = df_x, y = df_y, 
               by.x = "id", 
               by.y = "team_id_home", 
               all.x = TRUE)
nrow(dfJoin)
View(dfJoin)

df_x = dfJoin
df_y = subset(game_info, select = c("game_id", "attendance"))
dfJoin = merge(x = df_x, y = df_y, 
               by = "game_id", 
               all.x = TRUE)
mean(dfJoin$attendance,na.rm = TRUE)
View(dfJoin)

df_x = subset(game_summary, season == 2020,
              select = c("game_id", "season"))
dfJoin = merge(x = df_x, y = officials, 
               by = "game_id",
               all.x = TRUE)
length(unique(dfJoin$official_id))
View(dfJoin)

df_x = subset(game_summary,
              select = c("game_id", "season"))
df_y = subset(officials, first_name == "Dick" & last_name == "Bavetta")
dfJoin = merge(x = df_x, y = df_y, 
               by = "game_id",
               all.y = TRUE)
View(dfJoin)
table(dfJoin$season)

install.packages(c("DBi","RSQLite"))
library("DBI")
library("RSQLite")
mydb <- dbConnect(SQLite(), "nbaDb.sqlite")


dbListTables(mydb)

dbGetQuery(mydb, 'SELECT * FROM team LIMIT 5')


dfJoin = dbGetQuery(mydb,'select s.game_id, o.first_name, o.last_name,s.season
           from game_summary s left outer join officials o
           on s.game_id = o.game_id
           where s.season = 2020')

dbWriteTable(mydb, "match_2020", dfJoin)
dbListTables(mydb)

dbDisconnect(mydb)
