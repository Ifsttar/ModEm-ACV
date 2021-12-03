#Chargement de données

#structure d'un parc vide jusqu'à 2021
parc_vide <- read_xlsx("data/structure_parc.xlsx")

#Chargement des paramètres de configuration
importer <- "non"  #"oui" ou  "non" (valeur par defaut = EGT 2010)
fichier_parametres <- "data/test_entreesEGT2010.xlsx"

if(importer == "oui") {  charger_para_xslx(fichier_parametres, ltrip = 6.2, conso_elec = 0.2,hybrid_elec = 0.5)  }
if(importer == "non") {  load("data/para_defaut.RData")  }
rm(importer)

#chargement des parcs prédéfinis
Parc_utilisateur <- list(EGT2010 = read.xlsx("data/Parc_EGT2010_arrondi.xlsx"), Lyon2015 = read.xlsx("data/Parc_Lyon2015.xlsx"), FR2020 = read.xlsx("data/Parc_FR2020.xlsx"))
#Chargement des donnees brutes pre-traitees
load("data/data_ini.RData")

#listes des gaz et carburants pour l'analyse des conso et emissions directes
liste_gaz <- c("CO2","NOx","CO","PM10","PM2.5","NMVOC") #choix parmi "CO2","NOx","CO","PM10","PM2.5","NMVOC"
liste_carburant <- c("Diesel","Essence","GPL","GNV","Elec") #Petrol = Essence ; GPL = LPG ; GNV = CNG ; choix parmi ("Diesel","Essence","GPL","GNV","Electricité")

#liste des indicateurs ACV disponibles
liste_impacts_ACV <- read.xlsx("data/liste_impacts.xlsx")
rownames(liste_impacts_ACV) <- liste_impacts_ACV$Abrev

#Distribution des vitesse entre 5 et 130 pour les quatres sous cycles WLTC3b (lent, moyen, rapide et très rapide)
distrib_vitesse <- read.xlsx("data/distrib_cycles.xlsx",rowNames = TRUE)
#Accélération moyenne pour chaque vitesse entre 5 et 130 m/s² calculé à partir du cycle WLTC3b, utilisé pour l'analyse de flux d'énergie sankey
accel_moy <- read.xlsx("data/acceleration.xlsx")
row.names(accel_moy) <- accel_moy$vit

#fichier contenant les codes couleurs utilisés pour les graphs
couleurs <- read.xlsx("data/couleurs.xlsx", sheet = "liste_finale")