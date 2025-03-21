salaire_net_cadre = function(salaire_brut = 2500, temps_travail = 1){
  if (!is.numeric(salaire_brut)){
    return("Erreur, le salaire brut doit être un numérique")
  }
  else if (!(is.numeric(temps_travail) & 0<=temps_travail & temps_travail<=1)){
    return("Erreur, le temps de travail doit être un numérique entre 0 et 1")
  }
  else{
    return(salaire_brut*temps_travail*0.75)
  }
}

salaire_net = function(salaire_brut = 2500, temps_travail = 1, cadre = TRUE){
  if (!is.numeric(salaire_brut)){
    return("Erreur, le salaire brut doit être un numérique")
  }
  else if (!(is.numeric(temps_travail) & 0<=temps_travail & temps_travail<=1)){
    return("Erreur, le temps de travail doit être un numérique entre 0 et 1")
  }
  else if (!is.logical(cadre)){
    return("Erreur, cadre doit être un booléen")
  }
  
  if (cadre){
    salaire_net_av_impot=salaire_brut*temps_travail*0.75
  }else{
    salaire_net_av_impot=salaire_brut*temps_travail*0.78
  }
  
  if (salaire_net_av_impot <= 1591) {
    salaire_net_ap_impot = salaire_net_av_impot
  
  } else if (salaire_net_av_impot <= 2006) {
    salaire_net_ap_impot = salaire_net_av_impot * (1 - 0.029)
    
  } else if (salaire_net_av_impot <= 3476) {
    salaire_net_ap_impot = salaire_net_av_impot * (1 - 0.099)
    
  } else if (salaire_net_av_impot <= 8557) {
    salaire_net_ap_impot = salaire_net_av_impot * (1 - 0.20)
    
  } else {
    salaire_net_ap_impot = salaire_net_av_impot * (1 - 0.43)
  }
  return(salaire_net_ap_impot)
}

salaire_net(salaire_brut = 2000, temps_travail = 1, cadre = TRUE)

shifumi = function(){
  signes = c("pierre", "feuille", "ciseaux")
  
  choix_personne=readline(prompt="pierre, feuille ou ciseaux :")
  choix_machine=sample(signes, 1)
  
  cat("Votre choix :", choix_personne, "\n")
  cat("Choix de l'ordinateur :", choix_machine, "\n")
  
  choix_personne_num=grep(choix_personne,signes)
  print(choix_personne_num)
  choix_machine_num=grep(choix_machine,signes)
  if (choix_personne_num %in% c(1,2,3)){
    return("Vous devez entrer soit pierre, feuille ou ciseaux")
  }
  else if (choix_machine_num == choix_personne_num){
    return("Égalité")
  }
  else if ((choix_machine_num < choix_personne_num)|(choix_machine_num == 3 & choix_personne_num == 1)){
    return("Vous avez gagné(e)")
  }else {
    return("Vous avez perdu(e)")
  }
}

shifumi()

somme_cummul = function(){
  v = 1:5
  som = 0
  for (i in 1:length(v)){
    som = som + v[i]
    print(som)
  }
}

somme_cummul2 = function(){
  som = 0
  i = 1
  while (som <= 50){
    som = som + i
    cat(i," : ",som,"\n")
    i = i + 1
  }
}

class_iris = function(){
  for (e in colnames(iris)){
    cat(e,"est de classe",class(iris[,e]),"\n")
  }
}

class_iris2 = function(){
  colon = colnames(iris)
  i = 1
  while (i<=length(colon)){
    cat(colon[i],"est de classe",class(iris[,colon[i]]),"\n")
    i = i + 1
    
  }
}

cinq_carre = function(){
  for (i in 1:5){
    choix=as.numeric(readline(prompt = "entrez un nombre :"))
    cat("le carre de ce nombre est :",choix*choix,"\n")
  }
}

cinq_carre()