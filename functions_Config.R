# Fonctions utiles dans l'appli ####

#realise le calcule de la formule de Copert5 (av?+bv+c+d/v)/(ev?+fv+g)
formule <- function(parametres,vitesse){
  result <- (parametres[,grec[1]]%*%t(vitesse^2)+parametres[,grec[2]]%*%t(vitesse)+parametres[,grec[3]]+parametres[,grec[4]]%*%t(vitesse^(-1)))/(parametres[,grec[5]]%*%t(vitesse^2)+parametres[,grec[6]]%*%t(vitesse)+parametres[,grec[7]])
  return(result)
}

#convetir l annee en norme euro
norme <- function(annee) {
  resultats <- as.character(annee)
  resultats[which(resultats > max(tab_norme$Year))] <- as.character(tab_norme$Norm[nrow(tab_norme)])
  resultats[which(resultats < min(tab_norme$Year))] <- as.character(tab_norme$Norm[1])
  resultats[which(resultats %in% rownames(tab_norme))] <- as.character(tab_norme[resultats[which(resultats %in% rownames(tab_norme))],"Norm"])
  return(resultats)
}

#fonction creant l id_vehicle correspndant 
ident_vehicle <- function(data_vehicle) {
  Category <- as.character(data_vehicle$Category)
  Fuel <- as.character(data_vehicle$Fuel)
  Segment <- as.character(data_vehicle$Segment)
  Euro.Standard <- as.character(data_vehicle$Euro.Standard)
  Technology <- as.character(data_vehicle$Technology)
  
  #pas de segment mini avant euro 4
  Segment[which(Segment == "Mini" & Euro.Standard %in% c("Euro 1", "Euro 2", "Euro 3"))] <- "Small"
  #pas de norme inferieur a euro 4 pour CNG et hybrid
  Technology[which(Fuel == "Petrol Hybrid" & Euro.Standard %in% c("Euro 1", "Euro 2", "Euro 3"))] <- "GDI"
  Euro.Standard[which(Fuel %in% c("CNG", "Petrol Hybrid") & Euro.Standard %in% c("Euro 1", "Euro 2", "Euro 3"))] <- "Euro 4"
  #Pas de difference en norme euro 6 pour CNG et LPG 
  Euro.Standard[which(Fuel %in% c("CNG", "LPG") & Euro.Standard %in% c("Euro 6b", "Euro 6c", "Euro 6d"))] <- "Euro 6"
  
  id_vehicle <- paste(Category,Fuel,Segment,Euro.Standard,Technology, sep = "_")
  return(id_vehicle)
}

#creation du tableau de parc a partir de la serie temporelle
table_parc <- function(annee_min, annee_max) {
  annees <- annee_min:annee_max
  parc <- ini_parc[which(ini_parc$Year %in% annees),]
  annees_sup <- annees[which(annees > 2021)]
  annees_inf <- annees[which(annees < 1993)]
  parc <- rbind(ini_parc[rep(which(ini_parc$Year == 1993),length(annees_inf)),],parc, ini_parc[rep(which(ini_parc$Year == 2021),length(annees_sup)),])
  parc$Year[which(parc$Year == 1993)] <- rep(c(annees_inf,1993),each = 20)
  parc$Year[which(parc$Year == 2021)] <- rep(c(2021,annees_sup),each = 44)
  Euro.Standard <- norme(parc$Year)
  parc <- cbind(parc,Euro.Standard)
  id_vehicle <- ident_vehicle(parc)
  parc <- cbind(id_vehicle,parc)
  return(parc)
}

#Calcule le parametre Beta en fonction d'une longueur de deplacement et de la temperature
function_Beta <- function(ltrip,temp) {
  Beta_ini <- max(c(0,0.6474-0.02545*ltrip-(0.00974-0.000385*ltrip)*temp))
  resultats <- Beta
  resultats[,Pollutant_cold] <- resultats[,Pollutant_cold]*Beta_ini
  levels(resultats$Segment)[levels(resultats$Segment) == "Large-SUV-Executive"] <- "Large"
  return(resultats)
}

#Calcule de Q : formule a*v + b*temp + c (calcul matrice)
Qcold_hot <- function(parametres,temp,vitesse) {
  return(parametres[,"Alpha"]%*%t(vitesse)+parametres[,"Beta"]*temp+parametres[,"Gamma"] - 1)
}

#suremission à froid pour un mois donné
Cold_month <- function(temp, ltrip=NA, ratio_annee=1) {
  
  for(p in c("CO","VOC")) {
    coef <- Cold[[p]]
    if(temp > 15){
      assign(p,cbind(coef[which(coef$Range == "RANGE 3" & coef$Fuel != "Diesel"),c("Fuel","Segment","Euro.Standard")],Qcold_hot(coef[which(coef$Range == "RANGE 3" & coef$Fuel != "Diesel"),],temp,5:45)))
      assign(p,rbind(get(p),cbind(coef[which(coef$Range == "RANGE 1" & coef$Fuel == "Diesel"),c("Fuel","Segment","Euro.Standard")],Qcold_hot(coef[which(coef$Range == "RANGE 1" & coef$Fuel == "Diesel"),],temp,5:45))))
    }
    else {
      assign(p,cbind(coef[which(coef$Range == "RANGE 1" & coef$Fuel != "Diesel"),c("Fuel","Segment","Euro.Standard")],Qcold_hot(coef[which(coef$Range == "RANGE 1" & coef$Fuel != "Diesel"),],temp,5:25),Qcold_hot(coef[which(coef$Range == "RANGE 2" & coef$Fuel != "Diesel"),],temp,26:45)))
      assign(p,rbind(get(p),cbind(coef[which(coef$Range == "RANGE 1" & coef$Fuel == "Diesel"),c("Fuel","Segment","Euro.Standard")],Qcold_hot(coef[which(coef$Range == "RANGE 1" & coef$Fuel == "Diesel"),],temp,5:25),Qcold_hot(coef[which(coef$Range == "RANGE 1" & coef$Fuel == "Diesel"),],temp,26:45))))
    }
    next
  }
  coef <- Cold[["NOx"]]
  NOx <- cbind(coef[which(coef$Range == "RANGE 1" & coef$Fuel != "Diesel"),c("Fuel","Segment","Euro.Standard")],Qcold_hot(coef[which(coef$Range == "RANGE 1" & coef$Fuel != "Diesel"),],temp,5:25),Qcold_hot(coef[which(coef$Range == "RANGE 2"),],temp,26:45))
  NOx <- rbind(NOx,cbind(coef[which(coef$Fuel == "Diesel"),c("Fuel","Segment","Euro.Standard")],Qcold_hot(coef[which(coef$Fuel == "Diesel"),],temp,5:25),Qcold_hot(coef[which(coef$Fuel == "Diesel"),],temp,26:45)))
  coef <- Cold[["PM Exhaust"]]
  assign("PM Exhaust",cbind(coef[which(coef$Range == "RANGE 1"),c("Fuel","Segment","Euro.Standard")],Qcold_hot(coef[which(coef$Range == "RANGE 1"),],temp,5:130)))
  coef <- Cold[["EC"]]
  EC <- cbind(coef[which(coef$Range == "RANGE 1"),c("Fuel","Segment","Euro.Standard")],Qcold_hot(coef[which(coef$Range == "RANGE 1"),],temp,5:130))
  
  for(p in Pollutant_cold) {
    if(ncol(get(p)) == 44) {
      assign(p, cbind(get(p),replicate(85,get(p)[,44])))
    }
    next
  }
  if(!is.na(ltrip)) {
    facteur_beta <- function_Beta(ltrip,temp)
  }
  resultats <- list(get(Pollutant_cold[1]),get(Pollutant_cold[2]),get(Pollutant_cold[3]),get(Pollutant_cold[4]),get(Pollutant_cold[5]))
  names(resultats) <- Pollutant_cold
  if(!is.na(ltrip)) {
    facteur_beta <- function_Beta(ltrip,temp)
    ind_match <- match(as.character(interaction(resultats[[1]][,1:3])),as.character(interaction(facteur_beta[,2:4])))
    resultats <- lapply(Pollutant_cold,function(x) cbind(resultats[[x]][,1:3],resultats[[x]][,-(1:3)] * facteur_beta[ind_match,x] / ratio_annee))
    names(resultats) <- Pollutant_cold
  }
  return(resultats)
}

#suremission à froid pour l'année
Cold_year <- function(ltrip,temp) {
  
  monthly_results <- lapply(1:12, function(i) Cold_month(Temp$Temperature[i],ltrip = ltrip,ratio_annee = 12))
  resultats <- lapply(1:5,function(x) Reduce('+',lapply(1:12,function(i) monthly_results[[i]][[x]][,-(1:3)])))
  resultats <- lapply(resultats,function(x) cbind(monthly_results[[1]][[1]][,1:3],x))
  names(resultats) <- Pollutant_cold
  return(resultats)
}

#émissions à chaud
Hot_vitesse <- function(){
  pollutants <- names(Hot_PC)
  id_vehicle <- rownames(Hot_PC[[1]])
  resultats <- array(0, dim = c(length(id_vehicle),126,length(pollutants)), dimnames = list(id_vehicle,5:130,pollutants))
  for(p in pollutants){
    resultats[,,p] <- formule(Hot_PC[[p]],5:130)
    next
  }
  return(resultats)
  
}

#conso NRJ (en MJ) par km du a la clim, valeur moyenne des trois type (urban, rural, autoroute) rapportee a la minute avec les valeurs de 20 km/h, 60km/h et 100 km/h
NRJ_clim <- function(temp) {
  conso_minute <- matrix(0, nrow = 12, ncol = 2)
  for(i in 1:2) {
    if(i == 1) {
      para50 <- clim_para[which(clim_para$Fuel == "Petrol" & clim_para$Humidity == 0.5),]
      para80 <- clim_para[which(clim_para$Fuel == "Petrol" & clim_para$Humidity == 0.8),]
      NRJ_CO2 <- 1000*44.011/(12.011+1.008*FC_CO2["Petrol","H.C.Ratio"])/FC_CO2["Petrol","Energy.Content"]
    }
    else {
      para50 <- clim_para[which(clim_para$Fuel == "Diesel" & clim_para$Humidity == 0.5),]
      para80 <- clim_para[which(clim_para$Fuel == "Diesel" & clim_para$Humidity == 0.8),]
      NRJ_CO2 <- 1000*44.011/(12.011+1.008*FC_CO2["Diesel","H.C.Ratio"])/FC_CO2["Diesel","Energy.Content"]
    }
    for( m in 1:12) {
      degre <- temp$Temperature[m]
      if(degre >= 5) {
        humid <- temp$RH.[m]
        
        urban <- (20/60)*max( para50$a[which(para50$Mode == "Urban Peak")]*degre + para50$b[which(para50$Mode == "Urban Peak")]+ (humid-50)*(para80$a[which(para80$Mode == "Urban Peak")]*degre + para80$b[which(para50$Mode == "Urban Peak")] - (para50$a[which(para50$Mode == "Urban Peak")]*degre + para50$b[which(para50$Mode == "Urban Peak")]))/30,
                              para50$c[which(para50$Mode == "Urban Peak")] + (humid-50)*(para80$c[which(para80$Mode == "Urban Peak")]-para50$c[which(para50$Mode == "Urban Peak")])/30)
        rural <- (60/60)*max( para50$a[which(para50$Mode == "Rural")]*degre + para50$b[which(para50$Mode == "Rural")]+ (humid-50)*(para80$a[which(para80$Mode == "Rural")]*degre + para80$b[which(para50$Mode == "Rural")] - (para50$a[which(para50$Mode == "Rural")]*degre + para50$b[which(para50$Mode == "Rural")]))/30,
                              para50$c[which(para50$Mode == "Rural")] + (humid-50)*(para80$c[which(para80$Mode == "Rural")]-para50$c[which(para50$Mode == "Rural")])/30)
        highway <- (100/60)*max( para50$a[which(para50$Mode == "Highway")]*degre + para50$b[which(para50$Mode == "Highway")]+ (humid-50)*(para80$a[which(para80$Mode == "Highway")]*degre + para80$b[which(para50$Mode == "Highway")] - (para50$a[which(para50$Mode == "Highway")]*degre + para50$b[which(para50$Mode == "Highway")]))/30,
                                 para50$c[which(para50$Mode == "Highway")] + (humid-50)*(para80$c[which(para80$Mode == "Highway")]-para50$c[which(para50$Mode == "Highway")])/30)
        
        conso_minute[m,i] <- mean(c(urban,rural,highway))/NRJ_CO2
      }
      next
    }
    next
  }
  
  return(data.frame(Petrol = 60*mean(conso_minute[,1])/5:130, Diesel = 60*mean(conso_minute[,2])/5:130, row.names = 5:130))
}

#Converti la conso en MJ en g en fonction des differents carburants et retourne les emissions de CO2 fossil a partir des mix de carburants 
convert_fuel <- function(Table_conso,BioRatio) {
  
  Fuel <- sapply(strsplit(row.names(Table_conso),"_"), "[",2)
  Fuel <- sapply(strsplit(Fuel," "), "[",1)
  
  list_fuel <- row.names(FC_CO2)[1:7]
  Table_fuel <- as.data.frame(matrix(0, nrow = 4, ncol = 9, dimnames = list(c("Petrol","Diesel","LPG","CNG"),c(list_fuel,"BioCNG","CO2"))))
  Table_fuel["Petrol","Petrol"] <- ((1-BioRatio["Bioethanol",]-BioRatio["ETBE",])*FC_CO2["Petrol","Density"]/((1-BioRatio["Bioethanol",]-BioRatio["ETBE",])*FC_CO2["Petrol","Density"]+BioRatio["Bioethanol",]*FC_CO2["Bioethanol","Density"]+BioRatio["ETBE",]*FC_CO2["ETBE","Density"]))*1000/FC_CO2["Petrol","Energy.Content"]
  Table_fuel["Petrol","Bioethanol"] <- (BioRatio["Bioethanol",]*FC_CO2["Bioethanol","Density"]/((1-BioRatio["Bioethanol",]-BioRatio["ETBE",])*FC_CO2["Petrol","Density"]+BioRatio["Bioethanol",]*FC_CO2["Bioethanol","Density"]+BioRatio["ETBE",]*FC_CO2["ETBE","Density"]))*1000/FC_CO2["Bioethanol","Energy.Content"]
  Table_fuel["Petrol","ETBE"] <- (BioRatio["ETBE",]*FC_CO2["ETBE","Density"]/((1-BioRatio["Bioethanol",]-BioRatio["ETBE",])*FC_CO2["Petrol","Density"]+BioRatio["Bioethanol",]*FC_CO2["Bioethanol","Density"]+BioRatio["ETBE",]*FC_CO2["ETBE","Density"]))*1000/FC_CO2["ETBE","Energy.Content"]
  Table_fuel["Diesel","Diesel"] <- ((1-BioRatio["Biodiesel",])*FC_CO2["Diesel","Density"]/((1-BioRatio["Biodiesel",])*FC_CO2["Diesel","Density"]+BioRatio["Biodiesel",]*FC_CO2["Biodiesel","Density"]))*1000/FC_CO2["Diesel","Energy.Content"]
  Table_fuel["Diesel","Biodiesel"] <- (BioRatio["Biodiesel",]*FC_CO2["Biodiesel","Density"]/((1-BioRatio["Biodiesel",])*FC_CO2["Diesel","Density"]+BioRatio["Biodiesel",]*FC_CO2["Biodiesel","Density"]))*1000/FC_CO2["Biodiesel","Energy.Content"]
  Table_fuel["LPG","LPG"] <- 1000/FC_CO2["LPG","Energy.Content"]
  Table_fuel["CNG","CNG"] <- (1-BioRatio["BioCNG",])*1000/FC_CO2["CNG","Energy.Content"]
  Table_fuel["CNG","BioCNG"] <- BioRatio["BioCNG",]*1000/FC_CO2["CNG","Energy.Content"]
  
  CO2_fuel <- 44.011/(12.011+1.008*FC_CO2[list_fuel,"H.C.Ratio"]+16*FC_CO2[list_fuel,"O.C.Ratio"])*(1-FC_CO2[list_fuel,"Cbio.Ratio"])
  Table_fuel$CO2 <- rowSums(t(CO2_fuel*t(Table_fuel[,list_fuel])))
  
  resultats <- Table_conso
  for(f in names(Table_fuel)) {
    resultats <- abind(resultats,Table_conso*Table_fuel[Fuel,f], along = 3)
    next
  }
  dimnames(resultats)[[3]] <- c("EC",names(Table_fuel))
  
  return(resultats[,,-1])
}

#Calcul les emissions et conso avec copert
Compil_Copert <- function(ltrip, temp, BioRatio,usageAC, steps = c("Hot","Cold","Clim","Oil")) {
  #Hot emission
  Hot <- Hot_vitesse()
  Resultats <- Hot
  id_veh <- dimnames(Resultats)[[1]]
  Fuel <- sapply(strsplit(row.names(Resultats),"_"), "[",2)
  Segment <- sapply(strsplit(row.names(Resultats),"_"), "[",3)
  Euro.Standard <- sapply(strsplit(row.names(Resultats),"_"), "[",4)
  #Cold emissions
  if("Cold" %in% steps) {
    Cold <- Cold_year(ltrip, temp)
    mat_cold <- array(0, dim = dim(Resultats), dimnames = dimnames(Resultats))
    for(p in Pollutant_cold) {
      mat_cold[,,p] <- as.matrix(merge(cbind(Fuel,Segment,Euro.Standard),Cold[[p]], by = c("Fuel","Segment","Euro.Standard"), all.x = TRUE)[,-1:-3])
      next
    }
    Resultats <- (1+mat_cold)*Resultats
  }
  #Clim emission
  if("Clim" %in% steps) {
    Clim <- NRJ_clim(temp)
    RatioAC <- usageAC$InstalledAC*usageAC$ACUsage
    usageAC <- cbind(usageAC,RatioAC)
    
    ini <- rep(1,length(Fuel))
    ini[which(Fuel == "Diesel")] <- 2
    Resultats[,,"EC"] <- Resultats[,,"EC"] + t(Clim[,ini])*left_join(data.frame(cbind(Fuel,Segment,Euro.Standard)),usageAC, by = c("Fuel","Segment","Euro.Standard"))[,"RatioAC"]
  }
  
  #Possibilite d enlever les emissions a chaud pour faire des analyse
  if(!("Hot" %in% steps)) {
    Resultats <- Resultats - Hot
  }
  #particulates
  PMTSP <- matrix(NonExhaustPM$TSP, ncol = 126, nrow = length(Fuel), byrow = TRUE)+Resultats[,,"PM Exhaust"]
  PM10 <- matrix(NonExhaustPM$PM10, ncol = 126, nrow = length(Fuel), byrow = TRUE)+Resultats[,,"PM Exhaust"]
  PM2.5 <- matrix(NonExhaustPM$PM2.5, ncol = 126, nrow = length(Fuel), byrow = TRUE)+Resultats[,,"PM Exhaust"]
  Resultats <- abind(Resultats, PMTSP, PM10, PM2.5, make.names = TRUE) 
  #Energy conversion
  Resultats <- abind(Resultats, convert_fuel(Resultats[,,"EC"],BioRatio), along = 3)
  #lubricant
  if("Oil" %in% steps) {
    Resultats[,,"CO2"] <- Resultats[,,"CO2"] + CO2_lubricant
  }
  
  NMVOC <- Resultats[,,"VOC"] - Resultats[,,"CH4"]
  NMVOC[NMVOC < 0] <- 0
  Resultats <- abind(Resultats,NMVOC, along = 3,make.names = TRUE)
  
  return(Resultats)
}

#AJoute les voitures electrique, une conso electrique ( possibilite de faire un tableau pour les conso) en kWh
Ajout_elec <- function(conso_elec, hybrid_UF, ltrip ,temp, BioRatio ,usageAC, steps = c("Hot","Cold","Clim","Oil")) {
  facteurs_veh <- Compil_Copert(ltrip,temp, BioRatio, usageAC, steps)
  RatioAC <- usageAC$InstalledAC*usageAC$ACUsage
  usageAC <- cbind(usageAC,RatioAC)
  
  electricity <- facteurs_veh[,,"EC"]
  electricity[,] <- 0
  facteurs_veh <- abind(facteurs_veh, electricity, make.names = TRUE) 
  
  id_veh <- c(paste(rep("Passenger Cars_Electric_Mini_",5),c("Euro 4","Euro 5","Euro 6b","Euro 6c","Euro 6d"),"_",sep =""),
              paste(rep("Passenger Cars_Electric_",3*8),rep(c("Small","Medium","Large"),each = 8),"_",c("Euro 1","Euro 2","Euro 3","Euro 4","Euro 5","Euro 6b","Euro 6c","Euro 6d"),"_",sep =""))
  
  facteurs_elec <- array(0, dim = c(length(id_veh),dim(facteurs_veh)[-1]), dimnames = list(id_veh,dimnames(facteurs_veh)[[2]],dimnames(facteurs_veh)[[3]]))
  facteurs_elec[,,"PMTSP"] <- matrix(NonExhaustPM$TSP, ncol = 126, nrow = length(id_veh), byrow = TRUE) + facteurs_elec[,,"PMTSP"]
  facteurs_elec[,,"PM10"] <- matrix(NonExhaustPM$PM10, ncol = 126, nrow = length(id_veh), byrow = TRUE) + facteurs_elec[,,"PM10"]
  facteurs_elec[,,"PM2.5"] <- matrix(NonExhaustPM$PM2.5, ncol = 126, nrow = length(id_veh), byrow = TRUE) + facteurs_elec[,,"PM2.5"]
  
  Fuel <- sapply(strsplit(id_veh,"_"), "[",2)
  Segment <- sapply(strsplit(id_veh,"_"), "[",3)
  Euro.Standard <- sapply(strsplit(id_veh,"_"), "[",4)
  
  facteurs_elec[,,"electricity"] <- t(NRJ_clim(temp)[,rep("Diesel",length(id_veh))])*left_join(data.frame(cbind(Fuel,Segment,Euro.Standard)),usageAC, by = c("Fuel","Segment","Euro.Standard"))[,"RatioAC"]/3.6 + conso_elec
  
  id_hybr <- dimnames(facteurs_veh)[[1]][which(sapply(strsplit(dimnames(facteurs_veh)[[1]],"_"), "[",2) == "Petrol Hybrid")]
  
  facteurs_hybr <- facteurs_veh[id_hybr,,]*(1-hybrid_UF)
  facteurs_hybr[,,"PMTSP"] <- hybrid_UF*matrix(NonExhaustPM$TSP, ncol = 126, nrow = length(id_hybr), byrow = TRUE) + facteurs_hybr[,,"PMTSP"]
  facteurs_hybr[,,"PM10"] <- hybrid_UF*matrix(NonExhaustPM$PM10, ncol = 126, nrow = length(id_hybr), byrow = TRUE) + facteurs_hybr[,,"PM10"]
  facteurs_hybr[,,"PM2.5"] <- hybrid_UF*matrix(NonExhaustPM$PM2.5, ncol = 126, nrow = length(id_hybr), byrow = TRUE) + facteurs_hybr[,,"PM2.5"]
  
  Fuel <- sapply(strsplit(dimnames(facteurs_hybr)[[1]],"_"), "[",2)
  Segment <- sapply(strsplit(dimnames(facteurs_hybr)[[1]],"_"), "[",3)
  Euro.Standard <- sapply(strsplit(dimnames(facteurs_hybr)[[1]],"_"), "[",4)
  
  facteurs_hybr[,,"electricity"] <- (t(NRJ_clim(temp)[,rep("Diesel",length(id_hybr))])*left_join(data.frame(cbind(Fuel,Segment,Euro.Standard)),usageAC, by = c("Fuel","Segment","Euro.Standard"))[,"RatioAC"]/3.6 + conso_elec)*hybrid_UF
  
  Resultats <- abind(facteurs_veh[-which(dimnames(facteurs_veh)[[1]] %in% id_hybr),,],facteurs_elec, facteurs_hybr, along = 1)
  
  return(Resultats)
}

#Impacts d un kWh electric
Impacts_elec <- function(para_elec,ACV_electricity) {
  ratio <- sum(para_elec["photovoltaique",]/(1+para_elec["Perte low",]),para_elec["incinerateur",1]/((1+para_elec["Perte low",])*(1+para_elec["Perte medium",])),para_elec[1:7,1]/((1+para_elec["Perte low",])*(1+para_elec["Perte medium",])*(1+para_elec["Perte high",])))
  medium <- (1+para_elec["Perte low",])-para_elec["photovoltaique",]/ratio
  high <- sum(para_elec[1:7,1])/(ratio*(1+para_elec["Perte high",]))
  
  impacts <- colSums((para_elec[1:9,1]/ratio)*slice(ACV_electricity,match(tolower(rownames(Mix_elec)[1:9]),rownames(ACV_electricity)))) + filter(ACV_electricity,rownames(ACV_electricity)=="distrib_low") + medium*filter(ACV_electricity,rownames(ACV_electricity)=="distrib_medium")+high*filter(ACV_electricity,rownames(ACV_electricity)=="distrib_high")
  return(impacts)
}

#Impacts des infrastructures par vkm
Impacts_infra <- function(para_infra, ACV_infrastructures, detail = FALSE) {
  R_tunnel <- sum(1000*para_infra$dont.tunnel*para_infra$Utilisation*para_infra$Allocation/(365*para_infra$flux.moyen.journalier))
  R_pont <- sum(1000*para_infra$dont.pont*para_infra$Utilisation*para_infra$Allocation/(365*para_infra$flux.moyen.journalier))
  
  impacts <- rbind((1000*para_infra$Utilisation*para_infra$nb.voies*para_infra$Allocation/(365*para_infra$flux.moyen.journalier))*dplyr::slice(ACV_infrastructures,match(rownames(para_infra),rownames(ACV_infrastructures))),
                   R_tunnel*filter(ACV_infrastructures,rownames(ACV_infrastructures)=="Tunnel"),R_pont*filter(ACV_infrastructures,rownames(ACV_infrastructures)=="Pont"))
  if(!detail) {return(colSums(impacts))}
  
  return(t(impacts))
}

#Impacts dus aux vehicules (constructions, maintenance, EoL)
Impacts_vehicules <- function(parc, ACV_vehicules, detail = FALSE) {
  id_maint <- rep("Maint_ICE",nrow(parc))
  id_maint[which(parc$Fuel == "Electric")] <- "Maint_Elec"
  id_dechet <- rep("Dechet_ICE",nrow(parc))
  id_dechet[which(parc$Fuel == "Electric")] <- "Dechet_Elec"
  
  maintenance <-  slice(ACV_vehicules,match(id_maint,rownames(ACV_vehicules)))
  dechets <- as.matrix((parc$M_carros+parc$M_moteur_Elec+parc$M_moteur_ICE)/parc$DV_veh)*slice(ACV_vehicules,match(id_dechet,rownames(ACV_vehicules)))
  moteur <- as.matrix(parc$M_moteur_ICE/parc$DV_veh)%*%as.matrix(filter(ACV_vehicules,rownames(ACV_vehicules)=="Moteur_ICE")) + as.matrix(parc$M_moteur_Elec/parc$DV_veh)%*%as.matrix(filter(ACV_vehicules,rownames(ACV_vehicules)=="Moteur_Elec"))
  carrosserie <- as.matrix(parc$M_carros/parc$DV_veh)%*%as.matrix(filter(ACV_vehicules,rownames(ACV_vehicules)=="Carrosserie"))
  batterie <-  as.matrix(parc$M_batterie/parc$DV_batterie)%*%as.matrix(filter(ACV_vehicules,rownames(ACV_vehicules)=="Batterie"))

  impacts <- abind(carrosserie,moteur,batterie,maintenance,dechets, along = 3, make.names = TRUE)
  
  if(!detail) {impacts <- apply(impacts,1:2,sum)}
  
  return(impacts)
}

#Associe emissions et consommation de copert à chaque ligne du parc utilisé
Copert_Parc <- function(parc,facteur_copert) {
  SegmentBis <- parc$Segment
  EuroBis <- parc$Euro.Standard
  TechnoBis <- parc$Technology
  
  SegmentBis[which(SegmentBis == "Mini" & EuroBis %in% c("Euro 1", "Euro 2", "Euro 3"))] <- "Small"
  TechnoBis[parc$Fuel == "Petrol Hybrid" & EuroBis %in% c("Euro 1", "Euro 2", "Euro 3")] <- "GDI"
  EuroBis[parc$Fuel %in% c("Petrol Hybrid","CNG","Electric") & EuroBis %in% c("Euro 1", "Euro 2", "Euro 3")] <- "Euro 4"
  
  id_mat <- paste(parc$Category,parc$Fuel,SegmentBis,EuroBis,TechnoBis, sep = "_")
  
  id_mat <- match(id_mat,dimnames(facteur_copert)[[1]])
  
  facteur_copert[id_mat,,"NOx"] <- facteur_copert[id_mat,,"NOx"]*(1-parc$RedNOxEuro6)
  resultats <- facteur_copert[id_mat,,]
  return(resultats)
}

#Calcul les impacts ACV pour l'ensemble des phases
Impacts_total <- function(parc, para_infra, para_elec, facteur_copert, list_impacts, detail = FALSE) {
  
  imp_infra <- Impacts_infra(para_infra,subset(ACV_infrastructures, select = as.character(list_impacts$Abrev)), detail)
  imp_veh <- Impacts_vehicules(parc, subset(ACV_vehicules, select = as.character(list_impacts$Abrev)), detail)
  
  emissions <- facteur_copert[,,c(rownames(ACV_gaz),"NMVOC")]/1000
  dimnames(facteur_copert)[[3]][which(dimnames(facteur_copert)[[3]] == "BioCNG")] <- "Biomethane"
  dimnames(facteur_copert)[[3]][which(dimnames(facteur_copert)[[3]] == "Petrol")] <- "Essence"
  dimnames(facteur_copert)[[3]][which(dimnames(facteur_copert)[[3]] == "LPG")] <- "GPL"
  dimnames(facteur_copert)[[3]][which(dimnames(facteur_copert)[[3]] == "CNG")] <- "Gaz naturel"
  dimnames(facteur_copert)[[3]][which(dimnames(facteur_copert)[[3]] == "Bioethanol")] <- "Ethanol"
  consommation <- facteur_copert[,,rownames(ACV_carburants)]/1000

  if(detail) {
    imp_infra <- array(rep(imp_infra,each = nrow(parc)*126),dim = c(nrow(parc),126,dim(imp_infra)))
    imp_veh <- aperm(replicate(126,imp_veh,simplify = "array"),c(1,4,2,3))
    dimnames(imp_veh) <- list(1:nrow(parc),5:130,dimnames(imp_veh)[[3]],dimnames(imp_veh)[[4]])
    dimnames(imp_infra) <- list(1:nrow(parc),5:130,dimnames(imp_veh)[[3]],c(para_infra$Type_infra,"Tunnel","Pont"))
  } else {
    imp_infra <- array(rep(imp_infra,each = nrow(parc)*126),dim = c(nrow(parc),126,length(imp_infra)))
    imp_veh <- aperm(replicate(126,imp_veh,simplify = "array"),c(1,3,2))
  }
  
  elec <-  facteur_copert[,,"electricity"]
  imp_elec <- Impacts_elec(para_elec,subset(ACV_electricity,select = as.character(list_impacts$Abrev)))
  
  if(detail) {
    imp_carb <- array(consommation, c(dim(consommation),length(list_impacts$Abrev)))
    imp_carb <- imp_carb*array(rep(unlist(subset(ACV_carburants,select = as.character(list_impacts$Abrev))),each = dim(consommation)[1]*dim(consommation)[2]), dim = dim(imp_carb))
    dimnames(imp_carb) <- list(1:nrow(parc),5:130,dimnames(consommation)[[3]],names(subset(ACV_carburants,select = as.character(list_impacts$Abrev))))
    imp_carb <- abind(imp_carb,Electricity = Calcul_matrice(elec,imp_elec), along = 3)
    imp_carb <- aperm(imp_carb,c(1,2,4,3))
  } else {
    imp_carb <- Calcul_matrice(consommation,subset(ACV_carburants,select = as.character(list_impacts$Abrev)))
    imp_carb <- imp_carb + Calcul_matrice(elec,imp_elec)
  }
  
  if(detail) {
    temp_gaz <- array(emissions[,,rownames(ACV_gaz)],c(dim(emissions[,,rownames(ACV_gaz)]),ncol(subset(ACV_gaz,select = as.character(list_impacts$Abrev)))))
    temp_gaz <- temp_gaz*array(rep(unlist(subset(ACV_gaz,select = as.character(list_impacts$Abrev))),each = dim(temp_gaz)[1]*dim(temp_gaz)[2]), dim = dim(temp_gaz))
    dimnames(temp_gaz) <- list(1:nrow(parc),5:130,rownames(ACV_gaz),names(subset(ACV_gaz,select = as.character(list_impacts$Abrev))))
    imp_gaz <- aperm(abind(CO2 = R.utils::extract(temp_gaz,"CO2",dims=3),Particules = R.utils::extract(temp_gaz,"PMTSP",dims=3)+R.utils::extract(temp_gaz,"PM10",dims=3)+R.utils::extract(temp_gaz,"PM2.5",dims=3), NOx =  R.utils::extract(temp_gaz,"NOx",dims=3),
                     NMVOV = R.utils::extract(temp_gaz,"VOC",dims=3), Autres = R.utils::extract(temp_gaz,"CO",dims=3)+R.utils::extract(temp_gaz,"CH4",dims=3)+R.utils::extract(temp_gaz,"N2O",dims=3)+R.utils::extract(temp_gaz,"NH3",dims=3),along = 3,make.names = TRUE),c(1,2,4,3))
    dimnames(imp_gaz)[[4]] <- c("CO2","Particules","NOx","NMVOV","Autres")
    imp_gaz[,,,"Particules"] <- R.utils::extract(imp_gaz,"Particules",dims=4) + aperm(replicate(1,replicate(nrow(parc),as.matrix(NonExhaustPM[,c("PM_tyre","PM_break")]/1000)%*%as.matrix(subset(ACV_particules,select = as.character(list_impacts$Abrev))),simplify = "array"),simplify = "array"),c(3,1,2,4))				
    imp_gaz[,,,"NMVOV"] <- R.utils::extract(imp_gaz,"NMVOV",dims=4) + replicate(1,multiply_matrice(emissions[,,"NMVOC"],slice(subset(ACV_NMVOC,select = as.character(list_impacts$Abrev)),match(parc$Fuel,rownames(ACV_NMVOC)))),simplify = "array")
    imp_gaz[,,,"Autres"] <- R.utils::extract(imp_gaz,"Autres",dims=4) + replicate(1,Calcul_matrice(consommation[,,rownames(ACV_fuel_metal)],subset(ACV_fuel_metal,select = as.character(list_impacts$Abrev)))
    + aperm(replicate(126,as.matrix(ACV_othersPollutants[match(paste(parc$Fuel,parc$Euro.Standard, sep = "_"),paste(ACV_othersPollutants[,1],ACV_othersPollutants[,2], sep = "_")),-c(1,2)][,as.character(list_impacts$Abrev)]),simplify = "array"),c(1,3,2)),simplify = "array")
  } else {
    #impacts gaz principaux
    imp_gaz <- Calcul_matrice(emissions[,,rownames(ACV_gaz)],subset(ACV_gaz,select = as.character(list_impacts$Abrev)))
    #impacts NMVOC
    imp_gaz <- imp_gaz + multiply_matrice(emissions[,,"NMVOC"],subset(ACV_NMVOC,select = as.character(list_impacts$Abrev))[parc$Fuel,])
    #metaux contenus dans les particues de pneus et de freins
    imp_gaz <- imp_gaz + aperm(replicate(nrow(parc),as.matrix(NonExhaustPM[,c("PM_tyre","PM_break")]/1000)%*%as.matrix(subset(ACV_particules,select = as.character(list_impacts$Abrev))),simplify = "array"),c(3,1,2))
    #metaux contenus dans les carburants fossils (essence et diesel)
    imp_gaz <- imp_gaz + Calcul_matrice(consommation[,,rownames(ACV_fuel_metal)],subset(ACV_fuel_metal,select = as.character(list_impacts$Abrev)))
    #autres polluants (dont issue d huile) cst par type de vehicule
    imp_gaz <- imp_gaz + aperm(replicate(126,as.matrix(slice(subset(ACV_othersPollutants,select = as.character(list_impacts$Abrev)),match(paste(parc$Fuel,parc$Euro.Standard, sep = "_"),paste(ACV_othersPollutants[,1],ACV_othersPollutants[,2], sep = "_")))),simplify = "array"),c(1,3,2))
      # ACV_othersPollutants[match(paste(parc$Fuel,parc$Euro.Standard, sep = "_"),paste(ACV_othersPollutants[,1],ACV_othersPollutants[,2], sep = "_")),-c(1,2)][,as.character(list_impacts$Abrev)]),simplify = "array"),c(1,3,2))
  }
  
  if("Biodiv_GWP" %in% list_impacts$Abrev) {
    if(detail) {
        imp_infra <- abind(imp_infra,Sante = fast_sum_apply(imp_infra[,,which(substr(dimnames(imp_infra)[[3]],1,5) == "Sante"),],3),Biodiv = fast_sum_apply(imp_infra[,,which(substr(dimnames(imp_infra)[[3]],1,6) == "Biodiv"),],3),along = 3)
        imp_veh <- abind(imp_veh,Sante = fast_sum_apply(imp_veh[,,which(substr(dimnames(imp_veh)[[3]],1,5) == "Sante"),],3),Biodiv = fast_sum_apply(imp_veh[,,which(substr(dimnames(imp_veh)[[3]],1,6) == "Biodiv"),],3) ,along = 3)
        imp_carb <- abind(imp_carb,Sante = fast_sum_apply(imp_carb[,,which(substr(dimnames(imp_carb)[[3]],1,5) == "Sante"),],3),Biodiv = fast_sum_apply(imp_carb[,,which(substr(dimnames(imp_carb)[[3]],1,6) == "Biodiv"),],3),along = 3)
        imp_gaz <- abind(imp_gaz,Sante = fast_sum_apply(imp_gaz[,,which(substr(dimnames(imp_gaz)[[3]],1,5) == "Sante"),],3),Biodiv = fast_sum_apply(imp_gaz[,,which(substr(dimnames(imp_gaz)[[3]],1,6) == "Biodiv"),],3),along = 3)	
    } else {
      dimnames(imp_infra)[[3]] <- names(subset(ACV_carburants,select = as.character(list_impacts$Abrev)))
      dimnames(imp_veh)[[3]] <- names(subset(ACV_carburants,select = as.character(list_impacts$Abrev)))
      dimnames(imp_carb)[[3]] <- names(subset(ACV_carburants,select = as.character(list_impacts$Abrev)))
      dimnames(imp_gaz)[[3]] <- names(subset(ACV_carburants,select = as.character(list_impacts$Abrev)))
      imp_infra <- abind(imp_infra,Sante = fast_sum_apply(imp_infra[,,which(substr(dimnames(imp_infra)[[3]],1,5) == "Sante")],3),Biodiv = fast_sum_apply(imp_infra[,,which(substr(dimnames(imp_infra)[[3]],1,6) == "Biodiv")],3),along = 3)
      imp_veh <- abind(imp_veh,Sante = fast_sum_apply(imp_veh[,,which(substr(dimnames(imp_veh)[[3]],1,5) == "Sante")],3),Biodiv = fast_sum_apply(imp_veh[,,which(substr(dimnames(imp_veh)[[3]],1,6) == "Biodiv")],3) ,along = 3)
      imp_carb <- abind(imp_carb,Sante = fast_sum_apply(imp_carb[,,which(substr(dimnames(imp_carb)[[3]],1,5) == "Sante")],3),Biodiv = fast_sum_apply(imp_carb[,,which(substr(dimnames(imp_carb)[[3]],1,6) == "Biodiv")],3),along = 3)
      imp_gaz <- abind(imp_gaz,Sante = fast_sum_apply(imp_gaz[,,which(substr(dimnames(imp_gaz)[[3]],1,5) == "Sante")],3),Biodiv = fast_sum_apply(imp_gaz[,,which(substr(dimnames(imp_gaz)[[3]],1,6) == "Biodiv")],3),along = 3)	
    }
  }
  
  Resultats <- list()
  if(detail) {
    Resultats <- list(Gaz = imp_gaz,Carburants = imp_carb, Vehicules = imp_veh, Infrastructures = imp_infra)
    imp_infra <- fast_sum_apply(imp_infra,4)
    imp_veh <- fast_sum_apply(imp_veh,4)
    imp_carb <- fast_sum_apply(imp_carb,4)
    imp_gaz <- fast_sum_apply(imp_gaz,4)
  }
  imp_total <- imp_gaz+imp_carb+imp_veh+imp_infra
  Resultats <- c(Total = list(abind(imp_total,imp_gaz,imp_carb,imp_veh,imp_infra, along = 4, new.names = list(1:nrow(parc),5:130,dimnames(imp_total)[[3]],c("Total","Gaz","Carburant","Vehicule","Infrastructures")))),Resultats)
  
  return(Resultats)
}

#Fonction plus rapide qu'un apply
fast_sum_apply <- function(A,dim) {
  if(length(dim(A)) == 2) {
    if(dim == 1) {return(rowSums(A))}
    if(dim == 2) {return(colSums(A))}
  }
  if(length(dim(A)) == 3) {
    if(dim == 1) {
      temp <- A[1,,]
      for(i in 2:dim(A)[1]) {
        temp <- temp + A[i,,]
        next
      }
    }
    if(dim == 2) {
      temp <- A[,1,]
      for(i in 2:dim(A)[2]) {
        temp <- temp + A[,i,]
        next
      }
    }
    if(dim == 3) {
      temp <- A[,,1]
      for(i in 2:dim(A)[3]) {
        temp <- temp + A[,,i]
        next
      }
    }
    if(length(dim(temp))<2) {
      temp <- replicate(1,temp,simplify = "array")
      dimnames(temp)[[2]] <- dimnames(A)[[2]]
    }
    return(temp)
  }
  if(length(dim(A)) == 4) {
    if(dim == 1) {
      temp <- A[1,,,]
      for(i in 2:dim(A)[1]) {
        temp <- temp + A[i,,,]
        next
      }
    }
    if(dim == 2) {
      temp <- A[,1,,]
      for(i in 2:dim(A)[2]) {
        temp <- temp + A[,i,,]
        next
      }
    }
    if(dim == 3) {
      temp <- A[,,1,]
      for(i in 2:dim(A)[3]) {
        temp <- temp + A[,,i,]
        next
      }
    }
    if(dim == 4) {
      temp <- A[,,,1]
      for(i in 2:dim(A)[4]) {
        temp <- temp + A[,,,i]
        next
      }
    }
    if(length(dim(temp))<3) {
      temp <- replicate(1,temp,simplify = "array")
      dimnames(temp)[[3]] <- dimnames(A)[[3]]
    }
    return(temp)	
  }
  if(length(dim(A)) == 5) {
    if(dim == 1) {
      temp <- A[1,,,,]
      for(i in 2:dim(A)[1]) {
        temp <- temp + A[i,,,,]
        next
      }
    }
    if(dim == 2) {
      temp <- A[,1,,,]
      for(i in 2:dim(A)[2]) {
        temp <- temp + A[,i,,,]
        next
      }
    }
    if(dim == 3) {
      temp <- A[,,1,,]
      for(i in 2:dim(A)[3]) {
        temp <- temp + A[,,i,,]
        next
      }
    }
    if(dim == 4) {
      temp <- A[,,,1,]
      for(i in 2:dim(A)[4]) {
        temp <- temp + A[,,,i,]
        next
      }
    }
    if(dim == 5) {
      temp <- A[,,,,1]
      for(i in 2:dim(A)[5]) {
        temp <- temp + A[,,,,i]
        next
      }
    }
    if(length(dim(temp))<4) {
      temp <- replicate(1,temp,simplify = "array")
      dimnames(temp)[[4]] <- dimnames(A)[[4]]
    }
    return(temp)		
  }
}

#multiplie la matrice A par la matrice B pour chaque vitesse 5:130
Calcul_matrice <- function(A,B) {
  if(length(dim(A)) == 3) {
    Resultats <- A[,1,]%*%as.matrix(B)
    for(i in 2:126) {
      Resultats <- abind(Resultats,A[,i,]%*%as.matrix(B), along = 3)
      next
    }
  }
  if(length(dim(A)) == 2) {
    Resultats <- A[,1]%*%as.matrix(B)
    for(i in 2:126) {
      Resultats <- abind(Resultats,A[,i]%*%as.matrix(B), along = 3)
      next
    }
  }
  return(aperm(Resultats,c(1,3,2)))
}

#multiplie une matrice pour chaque vitesse
multiply_matrice <- function(A,B) {
  Resultats <- A[,1]*as.matrix(B)
  for(i in 2:126) {
    Resultats <- abind(Resultats,A[,i]*as.matrix(B), along = 3)
    next
  }
  return(aperm(Resultats,c(1,3,2)))
}

#charger un excel de parametres
charger_para_xslx <- function(fichier,ltrip,conso_elec,hybrid_elec) {
  xslx_para <- loadWorkbook(file = fichier)
  BioFuel <- read.xlsx(xslx_para,sheet = "Fuel")
  BioFuel <- data.frame(ratio = BioFuel[,2], row.names = BioFuel[,1])
  assign("BioFuel",BioFuel,.GlobalEnv)
  
  assign("AC_usage",read.xlsx(xslx_para,sheet = "Clim"),.GlobalEnv)
  
  Temp <- read.xlsx(xslx_para,sheet = "Temp")
  rownames(Temp) <- Temp$Month
  assign("Temp",Temp,.GlobalEnv)
  
  Infra_usage <- read.xlsx(xslx_para,sheet = "Infra")
  rownames(Infra_usage) <- Infra_usage$Type_infra
  assign("Infra_usage",Infra_usage,.GlobalEnv)
  
  Mix_elec <- read.xlsx(xslx_para,sheet = "Elec")
  Mix_elec <- data.frame(ratio = Mix_elec[,2], row.names = Mix_elec[,1])
  assign("Mix_elec",Mix_elec,.GlobalEnv)
  
  Parc_utilisateur <- read.xlsx(xslx_para,sheet = "Parc")
  gc()
  levels(Parc_utilisateur$Technology) <- c(levels(Parc_utilisateur$Technology),"")
  Parc_utilisateur$Technology[is.na(Parc_utilisateur$Technology)] <- ""
  assign("Parc_utilisateur",Parc_utilisateur,.GlobalEnv)
  
  assign("ltrip",ltrip,.GlobalEnv)
  assign("conso_elec",conso_elec,.GlobalEnv)
  assign("hybrid_elec",hybrid_elec,.GlobalEnv)
}

#retourne uniquement les gaz et carburants souhait?s, emission par km,carburants en litres/100km ou kg/100km (GNV) ou kWh/100km (elec)
tri_gaz_carb <- function(facteur_copert,list_gaz, list_fuel) {
  Resultats <- facteur_copert[,,list_gaz]
  
  if("Diesel" %in% list_fuel) {
    Diesel <- 100*((facteur_copert[,,"Diesel"]/FC_CO2["Diesel","Density"]) + (facteur_copert[,,"Biodiesel"]/FC_CO2["Biodiesel","Density"]))
    Resultats <- abind(Resultats, Diesel, along = 3, make.names = TRUE)
  }
  if("Essence" %in% list_fuel) {
    Essence <- 100*((facteur_copert[,,"Petrol"]/FC_CO2["Petrol","Density"]) + (facteur_copert[,,"Bioethanol"]/FC_CO2["Bioethanol","Density"]) + (facteur_copert[,,"ETBE"]/FC_CO2["ETBE","Density"]))
    Resultats <- abind(Resultats, Essence, along = 3, make.names = TRUE)
  }
  if("GPL" %in% list_fuel) {
    GPL <- 100*facteur_copert[,,"LPG"]/FC_CO2["LPG","Density"]
    Resultats <- abind(Resultats, GPL, along = 3, make.names = TRUE)
  }
  if("GNV" %in% list_fuel) {
    GNV <- 0.1*(facteur_copert[,,"CNG"] + facteur_copert[,,"BioCNG"])
    Resultats <- abind(Resultats, GNV, along = 3, make.names = TRUE)
  }
  if("Elec" %in% list_fuel) {
    Electricite <- 100*facteur_copert[,,"electricity"]
    Resultats <- abind(Resultats, Electricite, along = 3, make.names = TRUE)
  }
  
  return(Resultats)
}

#permet de choisir les colonnes pour un objet de plus de deux dimensions, B un vecteurs de noms ou chiffres, '+' pour garder, '-' pour enlever, dim pour la dimension à regarder
multi_select <- function(A,B,signe = '+',dim) {
  noms <- dimnames(A)[[dim]]
  if(signe == '+') {C <- match(B,noms, nomatch = 0)}
  if(signe == '-') {C <- -match(B,noms, nomatch = 0)}
  
  if(all(C == 0)){return(A)}
  else {
    return(extract(A,C,dims=dim))
  }
}

#Calcul la vitesse moyenne du réseau en fonction de la repartition autoroute 106 km/h ,route prim 72 km/h, route second 51 km/h, route tertiaire 32 km/h (les vitesses des cycle WLTC3b)
vitesse_reseau <- function(config) {
  return(round(sum(config[["Infra_usage"]]$Utilisation * c(106,72,51,32)),0))
}

#fonction pour calculer les données du sankey
sankey_energie <- function(Data_Impacts_parc, Data_Parc_Utilisateur, Data_Emis_conso_parc,Data_Mix_elec,vitesse, coef_frott, surf_frontales) {
  if(length(vitesse) == 126) {
    vit <- as.character(5:130)
    coef_vit <- vitesse
  }
  else{vit <- as.character(vitesse)}
  
  carburants <- data.frame(
    english=c("Diesel","Biodiesel","Petrol","Bioethanol","ETBE","LPG","CNG","BioCNG","electricity"),
    french=c("Diesel","Biodiesel","Essence","Éthanol","ETBE","GPL","GNV","Biogaz","Électricité"))
  
  nodes <- data.frame(
    name = c("Énergie primaire", "Énergie carburants","Énergie véhicules","Énergie infrastructures",
             rep(c("Non-renouvelable","Nucléaire","Renouvelable"),3), as.character(carburants$french),
             "Perte production carburants","Réservoir","Perte véhicule","Énergie roues",
             "Frottement aérodynamique","Frottement de roulement","Accélérations"),
    color=c("rgba(65,156,238,0.9)","rgba(5,73,92,0.9)","rgba(239,207,47,0.9)","rgba(163,14,16,0.9)",rep(c("rgba(153,153,153,0.9)","rgba(82,214,214,0.9)","rgba(72,232,110,0.9)"),3),
            "rgba(213,123,37,0.9)","rgba(232,178,124,0.9)","rgba(0,88,63,0.9)","rgba(0,162,116,0.9)","rgba(41,255,194,0.9)","rgba(207,199,3,0.9)","rgba(237,46,44,0.9)","rgba(247,155,155,0.9)","rgba(138,207,238,0.9)",
            "rgba(128,128,128,0.9)","rgba(255,1,1,0.9)","rgba(135,137,119,0.9)","rgba(248,225,8,0.9)",
            "rgba(128,176,167,0.9)","rgba(198,106,154,0.9)","rgba(181,137,123,0.9)"),
    x=c(0.05,rep(0.2,3),rep(0.35,9),rep(0.5,9),rep(0.65,2),rep(0.8,2),rep(0.95,3)),
    y= c(0.5,0.1,0.85,1,
         0.1,0.3,0.35,0.8,0.8,0.8,0.85,0.85,0.85,
         0.05,0.05,0.15,0.15,0.15,0.15,0.15,0.15,0.15,
         0.05,0.40,0.15,0.5,0.5,0.5,0.5),
    ID=0:28)
  
  links <- data.frame(IDsource=c(rep(0:3,each=3),rep(4:6,each=9),rep(13:21,each=2),rep(23,2),rep(25,3)),
                      IDtarget=c(1:12,rep(13:21,3),rep(22:23,9),24:28),value=rep(0,62))
  
  assign("source",nodes$name[links$IDsource+1])
  target <- nodes$name[links$IDtarget+1]
  
  links <- cbind(get("source"),target,links)
  rm("source","target")
  names(links)[1] <- "source"						
  
  if(!("Infrastructures" %in% dimnames(Data_Impacts_parc)[[4]])) {
    nodes <- nodes[c(-4,-11:-13),]		
  }
  
  links <- links[which(links$source %in% nodes$name & links$target %in% nodes$name),]
  
  CED_carburants <- ACV_carburants[c(3,1,4,6,5,8,7,2),c("NRJf","NRJn","NRJr")]
  CED_carburants <- rbind(CED_carburants,1000*Impacts_elec(Data_Mix_elec,ACV_electricity)[,c("NRJf","NRJn","NRJr")])
  rownames(CED_carburants) <- 13:21
  colnames(CED_carburants) <- 4:6
  
  if(length(vit)==1) {conso_parc <- colSums(Data_Emis_conso_parc[,vit,as.character(carburants$english)])}
  else {conso_parc <- apply(Data_Emis_conso_parc[,,as.character(carburants$english)],3,function(x) sum(x%*%as.matrix(coef_vit)))}
  
  CED_carburants <- conso_parc*CED_carburants/1000
  
  reservoir_carburants <- c(FC_CO2[as.character(carburants$english[c(-8,-9)]),"Energy.Content"],FC_CO2["CNG","Energy.Content"],3600)
  reservoir_carburants <- reservoir_carburants*conso_parc/1000
  
  masse_flotte <- sum(rowSums(Data_Parc_Utilisateur[,c("M_carros","M_moteur_ICE","M_moteur_Elec","M_batterie")])*Data_Parc_Utilisateur$Part)
  part_segment <- aggregate(Data_Parc_Utilisateur$Part,by = list(Data_Parc_Utilisateur$Segment),sum)
  surface_moy <- sum(surf_frontales[which(c("Mini","Small","Medium","Large") %in% as.character(part_segment$Group.1))]*part_segment[match(c("Mini","Small","Medium","Large"),as.character(part_segment$Group.1),nomatch = 0),"x"])
  roulement <- coef_frott$roul * 9.81 * masse_flotte / 1000
  faero <- function(v) {return(0.5 * coef_frott$aero * coef_frott$Dens_air * surface_moy * ((as.numeric(v)/3.6)^3) * (3600/as.numeric(v)) /1000000)}
  facc <- function(v) {return(masse_flotte * (as.numeric(v)/3.6) * accel_moy[v,"acc"] * (3600/as.numeric(v)) / 1000000)}
  if(length(vit)==1) {
    aero <- faero(vit)
    acceleration <- facc(vit)
  }
  else {
    aero <- sum(faero(vit)*coef_vit)
    acceleration <- sum(facc(vit)*coef_vit)
  }
  
  for(i in 1:nrow(links)) {
    if(links$IDtarget[i] %in% 13:21) {
      links$value[i] <- CED_carburants[as.character(links$IDtarget[i]),as.character(links$IDsource[i])]
      next
    }
    else {next}
  }
  
  for(i in 1:nrow(links)) {
    if(links$IDtarget[i] == 23) {
      links$value[i] <- reservoir_carburants[links$IDsource[i]-12]
      links$value[i-1] <- sum(links$value[which(links$IDtarget == links$IDsource[i])]) - links$value[i]
    }
    next
  }
  links$value[which(links$target=="Frottement aérodynamique")] <- aero
  links$value[which(links$target=="Frottement de roulement")] <- roulement
  links$value[which(links$target=="Accélérations")] <- acceleration
  links$value[which(links$target=="Énergie roues")] <- sum(links$value[which(links$target %in% c("Frottement aérodynamique","Frottement de roulement","Accélérations"))])
  
  links$value[which(links$target=="Perte véhicule")] <- sum(links$value[which(links$IDtarget==23)]) - links$value[which(links$target=="Énergie roues")]
  
  links$value[which(links$IDtarget == 4)] <- sum(links$value[which(links$IDsource == 4)])
  links$value[which(links$IDtarget == 5)] <- sum(links$value[which(links$IDsource == 5)])
  links$value[which(links$IDtarget == 6)] <- sum(links$value[which(links$IDsource == 6)])
  
  links$value[which(links$IDtarget == 7)] <- sum(Data_Impacts_parc[,vit[1],"NRJf","Vehicule"])
  links$value[which(links$IDtarget == 8)] <- sum(Data_Impacts_parc[,vit[1],"NRJn","Vehicule"])
  links$value[which(links$IDtarget == 9)] <- sum(Data_Impacts_parc[,vit[1],"NRJr","Vehicule"])
  
  links$value[which(links$IDtarget == 10)] <- sum(Data_Impacts_parc[,vit[1],"NRJf","Infrastructures"])
  links$value[which(links$IDtarget == 11)] <- sum(Data_Impacts_parc[,vit[1],"NRJn","Infrastructures"])
  links$value[which(links$IDtarget == 12)] <- sum(Data_Impacts_parc[,vit[1],"NRJr","Infrastructures"])
  
  links$value[which(links$IDtarget == 1)] <- sum(links$value[which(links$IDsource == 1)])
  links$value[which(links$IDtarget == 2)] <- sum(links$value[which(links$IDsource == 2)])
  links$value[which(links$IDtarget == 3)] <- sum(links$value[which(links$IDsource == 3)])
  
  links <- links[which(links$value != 0),]
  nodes <- nodes[which(nodes$ID %in% c(0,links$IDtarget)),]
  nodes$IDbis <- 0:(nrow(nodes)-1)
  links$IDsource <- nodes$IDbis[match(links$IDsource,nodes$ID)]
  links$IDtarget <- nodes$IDbis[match(links$IDtarget,nodes$ID)]
  gc()
  return(list(links=links, nodes=nodes))
}

#aggregege les résultats et supprime les colonnes à zéro
Aggreg_veh <- function(matrice, liste_aggreg, func) {
  temp <- aggregate(matrice, by = liste_aggreg, func)
  if(length(liste_aggreg)==1) {
    rownames(temp) <- temp[,1]
    temp <- temp[,-1,drop=FALSE]
  } 
  if(length(liste_aggreg)==2) {
    rownames(temp) <- paste(temp[,1],temp[,2], sep = " ")
    temp <- temp[,-(1:2),drop=FALSE]
  } 
  if(length(liste_aggreg)==3) {
    rownames(temp) <- paste(temp[,1],temp[,2],temp[,3], sep = " ")
    temp <- temp[,-(1:3),drop=FALSE]
  }
  
  temp <- temp[rowSums(temp) > 0 ,,drop=FALSE]
  return(temp)
}

#rename various name into french understandable words
renames_all <- function(A) {
  B <- recode(A, 
              "Gaz" = "Usage du véhicule", "Vehicule" = "Prod. véhicules", "Carburant" = "Prod. carburants","Vehicules" = "Prod. véhicules", "Carburants" = "Prod. carburants",
              "CNG" = "Gaz Naturel", "Petrol" = "Essence", "LPG" = "GPL", "Petrol.Hybrid" = "Hybride", "Petrol Hybrid" = "Hybride","Petrol.Hybrid" = "Hybride", "Electric" = "Electrique",
              "Small" = "Petit",
              "Euro.1" = "Euro 1","Euro.2" = "Euro 2","Euro.3" = "Euro 3","Euro.4" = "Euro 4","Euro.5" = "Euro 5","Euro.6b" = "Euro 6b","Euro.6c" = "Euro 6c","Euro.6d" = "Euro 6d",
              
              "Diesel.Large" = "Diesel/Large","Diesel.Mini" = "Diesel/Mini","Diesel.Small" = "Diesel/Petit","Diesel.Medium" = "Diesel/Medium",
              "Petrol.Large" = "Essence/Large","Petrol.Mini" = "Essence/Mini","Petrol.Small" = "Essence/Petit","Petrol.Medium" = "Essence/Medium",
              "Petrol.Hybrid.Large" = "Hybride/Large","Petrol.Hybrid.Mini" = "Hybride/Mini","Petrol.Hybrid.Small" = "Hybride/Petit","Petrol.Hybrid.Medium" = "Hybride/Medium",
              "LPG.Large" = "GPL/Large","LPG.Mini" = "GPL/Mini","LPG.Small" = "GPL/Petit","LPG.Medium" = "GPL/Medium",
              "CNG.Large" = "Gaz Naturel/Large","CNG.Mini" = "Gaz Naturel/Mini","CNG.Small" = "Gaz Naturel/Petit","CNG.Medium" = "Gaz Naturel/Medium",
              "Electric.Large" = "Electrique/Large","Electric.Mini" = "Electrique/Mini","Electric.Small" = "Electrique/Petit","Electric.Medium" = "Electrique/Medium",
              
              "Diesel.Euro.1" = "Diesel/Euro 1","Diesel.Euro.2" = "Diesel/Euro 2","Diesel.Euro.3" = "Diesel/Euro 3","Diesel.Euro.4" = "Diesel/Euro 4","Diesel.Euro.5" = "Diesel/Euro 5","Diesel.Euro.6b" = "Diesel/Euro 6b","Diesel.Euro.6c" = "Diesel/Euro 6c","Diesel.Euro.6d" = "Diesel/Euro 6d",
              "Petrol.Euro.1" = "Essence/Euro 1","Petrol.Euro.2" = "Essence/Euro 2","Petrol.Euro.3" = "Essence/Euro 3","Petrol.Euro.4" = "Essence/Euro 4","Petrol.Euro.5" = "Essence/Euro 5","Petrol.Euro.6b" = "Essence/Euro 6b","Petrol.Euro.6c" = "Essence/Euro 6c","Petrol.Euro.6d" = "Essence/Euro 6d",
              "Petrol.Hybrid.Euro.1" = "Hybride/Euro 1","Petrol.Hybrid.Euro.2" = "Hybride/Euro 2","Petrol.Hybrid.Euro.3" = "Hybride/Euro 3","Petrol.Hybrid.Euro.4" = "Hybride/Euro 4","Petrol.Hybrid.Euro.5" = "Hybride/Euro 5","Petrol.Hybrid.Euro.6b" = "Hybride/Euro 6b","Petrol.Hybrid.Euro.6c" = "Hybride/Euro 6c","Petrol.Hybrid.Euro.6d" = "Hybride/Euro 6d",
              "LPG.Euro.1" = "GPL/Euro 1","LPG.Euro.2" = "GPL/Euro 2","LPG.Euro.3" = "GPL/Euro 3","LPG.Euro.4" = "GPL/Euro 4","LPG.Euro.5" = "GPL/Euro 5","LPG.Euro.6b" = "GPL/Euro 6b","LPG.Euro.6c" = "GPL/Euro 6c","LPG.Euro.6d" = "GPL/Euro 6d",
              "CNG.Euro.1" = "Gaz Naturel/Euro 1","CNG.Euro.2" = "Gaz Naturel/Euro 2","CNG.Euro.3" = "Gaz Naturel/Euro 3","CNG.Euro.4" = "Gaz Naturel/Euro 4","CNG.Euro.5" = "Gaz Naturel/Euro 5","CNG.Euro.6b" = "Gaz Naturel/Euro 6b","CNG.Euro.6c" = "Gaz Naturel/Euro 6c","CNG.Euro.6d" = "Gaz Naturel/Euro 6d",
              "Electric.Euro.1" = "Electrique/Euro 1","Electric.Euro.2" = "Electrique/Euro 2","Electric.Euro.3" = "Electrique/Euro 3","Electric.Euro.4" = "Electrique/Euro 4","Electric.Euro.5" = "Electrique/Euro 5","Electric.Euro.6b" = "Electrique/Euro 6b","Electric.Euro.6c" = "Electrique/Euro 6c","Electric.Euro.6d" = "Electrique/Euro 6d",
              
              "Diesel.Euro.1.Large" = "Diesel/Euro 1/Large","Diesel.Euro.2.Large" = "Diesel/Euro 2/Large","Diesel.Euro.3.Large" = "Diesel/Euro 3/Large","Diesel.Euro.4.Large" = "Diesel/Euro 4/Large","Diesel.Euro.5.Large" = "Diesel/Euro 5/Large","Diesel.Euro.6b.Large" = "Diesel/Euro 6b/Large","Diesel.Euro.6c.Large" = "Diesel/Euro 6c/Large","Diesel.Euro.6d.Large" = "Diesel/Euro 6d/Large",
              "Petrol.Euro.1.Large" = "Essence/Euro 1/Large","Petrol.Euro.2.Large" = "Essence/Euro 2/Large","Petrol.Euro.3.Large" = "Essence/Euro 3/Large","Petrol.Euro.4.Large" = "Essence/Euro 4/Large","Petrol.Euro.5.Large" = "Essence/Euro 5/Large","Petrol.Euro.6b.Large" = "Essence/Euro 6b/Large","Petrol.Euro.6c.Large" = "Essence/Euro 6c/Large","Petrol.Euro.6d.Large" = "Essence/Euro 6d/Large",
              "Petrol.Hybrid.Euro.1.Large" = "Hybride/Euro 1/Large","Petrol.Hybrid.Euro.2.Large" = "Hybride/Euro 2/Large","Petrol.Hybrid.Euro.3.Large" = "Hybride/Euro 3/Large","Petrol.Hybrid.Euro.4.Large" = "Hybride/Euro 4/Large","Petrol.Hybrid.Euro.5.Large" = "Hybride/Euro 5/Large","Petrol.Hybrid.Euro.6b.Large" = "Hybride/Euro 6b/Large","Petrol.Hybrid.Euro.6c.Large" = "Hybride/Euro 6c/Large","Petrol.Hybrid.Euro.6d.Large" = "Hybride/Euro 6d/Large",
              "LPG.Euro.1.Large" = "GPL/Euro 1/Large","LPG.Euro.2.Large" = "GPL/Euro 2/Large","LPG.Euro.3.Large" = "GPL/Euro 3/Large","LPG.Euro.4.Large" = "GPL/Euro 4/Large","LPG.Euro.5.Large" = "GPL/Euro 5/Large","LPG.Euro.6b.Large" = "GPL/Euro 6b/Large","LPG.Euro.6c.Large" = "GPL/Euro 6c/Large","LPG.Euro.6d.Large" = "GPL/Euro 6d/Large",
              "CNG.Euro.1.Large" = "Gaz Naturel/Euro 1/Large","CNG.Euro.2.Large" = "Gaz Naturel/Euro 2/Large","CNG.Euro.3.Large" = "Gaz Naturel/Euro 3/Large","CNG.Euro.4.Large" = "Gaz Naturel/Euro 4/Large","CNG.Euro.5.Large" = "Gaz Naturel/Euro 5/Large","CNG.Euro.6b.Large" = "Gaz Naturel/Euro 6b/Large","CNG.Euro.6c.Large" = "Gaz Naturel/Euro 6c/Large","CNG.Euro.6d.Large" = "Gaz Naturel/Euro 6d/Large",
              "Electric.Euro.1.Large" = "Electrique/Euro 1/Large","Electric.Euro.2.Large" = "Electrique/Euro 2/Large","Electric.Euro.3.Large" = "Electrique/Euro 3/Large","Electric.Euro.4.Large" = "Electrique/Euro 4/Large","Electric.Euro.5.Large" = "Electrique/Euro 5/Large","Electric.Euro.6b.Large" = "Electrique/Euro 6b/Large","Electric.Euro.6c.Large" = "Electrique/Euro 6c/Large","Electric.Euro.6d.Large" = "Electrique/Euro 6d/Large",
              
              "Diesel.Euro.1.Mini" = "Diesel/Euro 1/Mini","Diesel.Euro.2.Mini" = "Diesel/Euro 2/Mini","Diesel.Euro.3.Mini" = "Diesel/Euro 3/Mini","Diesel.Euro.4.Mini" = "Diesel/Euro 4/Mini","Diesel.Euro.5.Mini" = "Diesel/Euro 5/Mini","Diesel.Euro.6b.Mini" = "Diesel/Euro 6b/Mini","Diesel.Euro.6c.Mini" = "Diesel/Euro 6c/Mini","Diesel.Euro.6d.Mini" = "Diesel/Euro 6d/Mini",
              "Petrol.Euro.1.Mini" = "Essence/Euro 1/Mini","Petrol.Euro.2.Mini" = "Essence/Euro 2/Mini","Petrol.Euro.3.Mini" = "Essence/Euro 3/Mini","Petrol.Euro.4.Mini" = "Essence/Euro 4/Mini","Petrol.Euro.5.Mini" = "Essence/Euro 5/Mini","Petrol.Euro.6b.Mini" = "Essence/Euro 6b/Mini","Petrol.Euro.6c.Mini" = "Essence/Euro 6c/Mini","Petrol.Euro.6d.Mini" = "Essence/Euro 6d/Mini",
              "Petrol.Hybrid.Euro.1.Mini" = "Hybride/Euro 1/Mini","Petrol.Hybrid.Euro.2.Mini" = "Hybride/Euro 2/Mini","Petrol.Hybrid.Euro.3.Mini" = "Hybride/Euro 3/Mini","Petrol.Hybrid.Euro.4.Mini" = "Hybride/Euro 4/Mini","Petrol.Hybrid.Euro.5.Mini" = "Hybride/Euro 5/Mini","Petrol.Hybrid.Euro.6b.Mini" = "Hybride/Euro 6b/Mini","Petrol.Hybrid.Euro.6c.Mini" = "Hybride/Euro 6c/Mini","Petrol.Hybrid.Euro.6d.Mini" = "Hybride/Euro 6d/Mini",
              "LPG.Euro.1.Mini" = "GPL/Euro 1/Mini","LPG.Euro.2.Mini" = "GPL/Euro 2/Mini","LPG.Euro.3.Mini" = "GPL/Euro 3/Mini","LPG.Euro.4.Mini" = "GPL/Euro 4/Mini","LPG.Euro.5.Mini" = "GPL/Euro 5/Mini","LPG.Euro.6b.Mini" = "GPL/Euro 6b/Mini","LPG.Euro.6c.Mini" = "GPL/Euro 6c/Mini","LPG.Euro.6d.Mini" = "GPL/Euro 6d/Mini",
              "CNG.Euro.1.Mini" = "Gaz Naturel/Euro 1/Mini","CNG.Euro.2.Mini" = "Gaz Naturel/Euro 2/Mini","CNG.Euro.3.Mini" = "Gaz Naturel/Euro 3/Mini","CNG.Euro.4.Mini" = "Gaz Naturel/Euro 4/Mini","CNG.Euro.5.Mini" = "Gaz Naturel/Euro 5/Mini","CNG.Euro.6b.Mini" = "Gaz Naturel/Euro 6b/Mini","CNG.Euro.6c.Mini" = "Gaz Naturel/Euro 6c/Mini","CNG.Euro.6d.Mini" = "Gaz Naturel/Euro 6d/Mini",
              "Electric.Euro.1.Mini" = "Electrique/Euro 1/Mini","Electric.Euro.2.Mini" = "Electrique/Euro 2/Mini","Electric.Euro.3.Mini" = "Electrique/Euro 3/Mini","Electric.Euro.4.Mini" = "Electrique/Euro 4/Mini","Electric.Euro.5.Mini" = "Electrique/Euro 5/Mini","Electric.Euro.6b.Mini" = "Electrique/Euro 6b/Mini","Electric.Euro.6c.Mini" = "Electrique/Euro 6c/Mini","Electric.Euro.6d.Mini" = "Electrique/Euro 6d/Mini",
              
              "Diesel.Euro.1.Medium" = "Diesel/Euro 1/Medium","Diesel.Euro.2.Medium" = "Diesel/Euro 2/Medium","Diesel.Euro.3.Medium" = "Diesel/Euro 3/Medium","Diesel.Euro.4.Medium" = "Diesel/Euro 4/Medium","Diesel.Euro.5.Medium" = "Diesel/Euro 5/Medium","Diesel.Euro.6b.Medium" = "Diesel/Euro 6b/Medium","Diesel.Euro.6c.Medium" = "Diesel/Euro 6c/Medium","Diesel.Euro.6d.Medium" = "Diesel/Euro 6d/Medium",
              "Petrol.Euro.1.Medium" = "Essence/Euro 1/Medium","Petrol.Euro.2.Medium" = "Essence/Euro 2/Medium","Petrol.Euro.3.Medium" = "Essence/Euro 3/Medium","Petrol.Euro.4.Medium" = "Essence/Euro 4/Medium","Petrol.Euro.5.Medium" = "Essence/Euro 5/Medium","Petrol.Euro.6b.Medium" = "Essence/Euro 6b/Medium","Petrol.Euro.6c.Medium" = "Essence/Euro 6c/Medium","Petrol.Euro.6d.Medium" = "Essence/Euro 6d/Medium",
              "Petrol.Hybrid.Euro.1.Medium" = "Hybride/Euro 1/Medium","Petrol.Hybrid.Euro.2.Medium" = "Hybride/Euro 2/Medium","Petrol.Hybrid.Euro.3.Medium" = "Hybride/Euro 3/Medium","Petrol.Hybrid.Euro.4.Medium" = "Hybride/Euro 4/Medium","Petrol.Hybrid.Euro.5.Medium" = "Hybride/Euro 5/Medium","Petrol.Hybrid.Euro.6b.Medium" = "Hybride/Euro 6b/Medium","Petrol.Hybrid.Euro.6c.Medium" = "Hybride/Euro 6c/Medium","Petrol.Hybrid.Euro.6d.Medium" = "Hybride/Euro 6d/Medium",
              "LPG.Euro.1.Medium" = "GPL/Euro 1/Medium","LPG.Euro.2.Medium" = "GPL/Euro 2/Medium","LPG.Euro.3.Medium" = "GPL/Euro 3/Medium","LPG.Euro.4.Medium" = "GPL/Euro 4/Medium","LPG.Euro.5.Medium" = "GPL/Euro 5/Medium","LPG.Euro.6b.Medium" = "GPL/Euro 6b/Medium","LPG.Euro.6c.Medium" = "GPL/Euro 6c/Medium","LPG.Euro.6d.Medium" = "GPL/Euro 6d/Medium",
              "CNG.Euro.1.Medium" = "Gaz Naturel/Euro 1/Medium","CNG.Euro.2.Medium" = "Gaz Naturel/Euro 2/Medium","CNG.Euro.3.Medium" = "Gaz Naturel/Euro 3/Medium","CNG.Euro.4.Medium" = "Gaz Naturel/Euro 4/Medium","CNG.Euro.5.Medium" = "Gaz Naturel/Euro 5/Medium","CNG.Euro.6b.Medium" = "Gaz Naturel/Euro 6b/Medium","CNG.Euro.6c.Medium" = "Gaz Naturel/Euro 6c/Medium","CNG.Euro.6d.Medium" = "Gaz Naturel/Euro 6d/Medium",
              "Electric.Euro.1.Medium" = "Electrique/Euro 1/Medium","Electric.Euro.2.Medium" = "Electrique/Euro 2/Medium","Electric.Euro.3.Medium" = "Electrique/Euro 3/Medium","Electric.Euro.4.Medium" = "Electrique/Euro 4/Medium","Electric.Euro.5.Medium" = "Electrique/Euro 5/Medium","Electric.Euro.6b.Medium" = "Electrique/Euro 6b/Medium","Electric.Euro.6c.Medium" = "Electrique/Euro 6c/Medium","Electric.Euro.6d.Medium" = "Electrique/Euro 6d/Medium",
              
              "Diesel.Euro.1.Small" = "Diesel/Euro 1/Petit","Diesel.Euro.2.Small" = "Diesel/Euro 2/Petit","Diesel.Euro.3.Small" = "Diesel/Euro 3/Petit","Diesel.Euro.4.Small" = "Diesel/Euro 4/Petit","Diesel.Euro.5.Small" = "Diesel/Euro 5/Petit","Diesel.Euro.6b.Small" = "Diesel/Euro 6b/Petit","Diesel.Euro.6c.Small" = "Diesel/Euro 6c/Petit","Diesel.Euro.6d.Small" = "Diesel/Euro 6d/Petit",
              "Petrol.Euro.1.Small" = "Essence/Euro 1/Petit","Petrol.Euro.2.Small" = "Essence/Euro 2/Petit","Petrol.Euro.3.Small" = "Essence/Euro 3/Petit","Petrol.Euro.4.Small" = "Essence/Euro 4/Petit","Petrol.Euro.5.Small" = "Essence/Euro 5/Petit","Petrol.Euro.6b.Small" = "Essence/Euro 6b/Petit","Petrol.Euro.6c.Small" = "Essence/Euro 6c/Petit","Petrol.Euro.6d.Small" = "Essence/Euro 6d/Petit",
              "Petrol.Hybrid.Euro.1.Small" = "Hybride/Euro 1/Petit","Petrol.Hybrid.Euro.2.Small" = "Hybride/Euro 2/Petit","Petrol.Hybrid.Euro.3.Small" = "Hybride/Euro 3/Petit","Petrol.Hybrid.Euro.4.Small" = "Hybride/Euro 4/Petit","Petrol.Hybrid.Euro.5.Small" = "Hybride/Euro 5/Petit","Petrol.Hybrid.Euro.6b.Small" = "Hybride/Euro 6b/Petit","Petrol.Hybrid.Euro.6c.Small" = "Hybride/Euro 6c/Petit","Petrol.Hybrid.Euro.6d.Small" = "Hybride/Euro 6d/Petit",
              "LPG.Euro.1.Small" = "GPL/Euro 1/Petit","LPG.Euro.2.Small" = "GPL/Euro 2/Petit","LPG.Euro.3.Small" = "GPL/Euro 3/Petit","LPG.Euro.4.Small" = "GPL/Euro 4/Petit","LPG.Euro.5.Small" = "GPL/Euro 5/Petit","LPG.Euro.6b.Small" = "GPL/Euro 6b/Petit","LPG.Euro.6c.Small" = "GPL/Euro 6c/Petit","LPG.Euro.6d.Small" = "GPL/Euro 6d/Petit",
              "CNG.Euro.1.Small" = "Gaz Naturel/Euro 1/Petit","CNG.Euro.2.Small" = "Gaz Naturel/Euro 2/Petit","CNG.Euro.3.Small" = "Gaz Naturel/Euro 3/Petit","CNG.Euro.4.Small" = "Gaz Naturel/Euro 4/Petit","CNG.Euro.5.Small" = "Gaz Naturel/Euro 5/Petit","CNG.Euro.6b.Small" = "Gaz Naturel/Euro 6b/Petit","CNG.Euro.6c.Small" = "Gaz Naturel/Euro 6c/Petit","CNG.Euro.6d.Small" = "Gaz Naturel/Euro 6d/Petit",
              "Electric.Euro.1.Small" = "Electrique/Euro 1/Petit","Electric.Euro.2.Small" = "Electrique/Euro 2/Petit","Electric.Euro.3.Small" = "Electrique/Euro 3/Petit","Electric.Euro.4.Small" = "Electrique/Euro 4/Petit","Electric.Euro.5.Small" = "Electrique/Euro 5/Petit","Electric.Euro.6b.Small" = "Electrique/Euro 6b/Petit","Electric.Euro.6c.Small" = "Electrique/Euro 6c/Petit","Electric.Euro.6d.Small" = "Electrique/Euro 6d/Petit",
              
              "Electricity" = "Electricité",
              "carrosserie" = "Carrosserie", "moteur" = "Moteur", "batterie" = "Batterie", "maintenance" = "Maintenance", "dechets" = "Déchets",
              "Routeprim" = "Route Primaire", "Routesecond" = "Route Secondaire", "Routeterti" = "Route Tertiaire",
              
              "Euro.1.Mini" = "Euro 1/Mini","Euro.2.Mini" = "Euro 2/Mini","Euro.3.Mini" = "Euro 3/Mini","Euro.4.Mini" = "Euro 4/Mini","Euro.5.Mini" = "Euro 5/Mini","Euro.6b.Mini" = "Euro 6b/Mini","Euro.6c.Mini" = "Euro 6c/Mini","Euro.6d.Mini" = "Euro 6d/Mini",
              "Euro.1.Small" = "Euro 1/Petit","Euro.2.Small" = "Euro 2/Petit","Euro.3.Small" = "Euro 3/Petit","Euro.4.Small" = "Euro 4/Petit","Euro.5.Small" = "Euro 5/Petit","Euro.6b.Small" = "Euro 6b/Petit","Euro.6c.Small" = "Euro 6c/Petit","Euro.6d.Small" = "Euro 6d/Petit",
              "Euro.1.Medium" = "Euro 1/Medium","Euro.2.Medium" = "Euro 2/Medium","Euro.3.Medium" = "Euro 3/Medium","Euro.4.Medium" = "Euro 4/Medium","Euro.5.Medium" = "Euro 5/Medium","Euro.6b.Medium" = "Euro 6b/Medium","Euro.6c.Medium" = "Euro 6c/Medium","Euro.6d.Medium" = "Euro 6d/Medium",
              "Euro.1.Large" = "Euro 1/Large","Euro.2.Large" = "Euro 2/Large","Euro.3.Large" = "Euro 3/Large","Euro.4.Large" = "Euro 4/Large","Euro.5.Large" = "Euro 5/Large","Euro.6b.Large" = "Euro 6b/Large","Euro.6c.Large" = "Euro 6c/Large","Euro.6d.Large" = "Euro 6d/Large",
              
              "Nucleaire" = "Nucléaire", "Petrole" = "Pétrole", "eolien" = "Éolien", "biomasse" = "Biomasse", "incinerateur" = "Incinérateur", "photovoltaique" = "Photovoltaique", "Gaz.Naturel" = "Gaz Naturel", "Gaz naturel" = "Gaz Naturel"
              
  )
  if(Sys.getlocale(category = "LC_CTYPE")=="French_France.1252") {
    B <- iconv(B,"UTF-8","ISO_8859-2")
  }
  
  
  
  return(B)
}

#fonction de tri (order) pour des listes plus petites
tri_perso <- function(A,B) {
  C <- order(match(A,B))
  return(C)
}

#Met en rouge certaines valeurs dans les somme de tableaux de configuration de parcs (si la somme != 100%)
TableFooter <- function(ncol,red_mark = c()) {
  if(length(red_mark)!=0) {
    javascript <- JS(
      "function(tfoot, data, start, end, display ) {",
      "var api = this.api(), data;",
      "$(api.column(1).footer()).html('TOTAL');",
      paste0("$(api.column(",2:ncol,").footer()).html(api.column(",2:ncol,").data().reduce( function ( a, b ) {return Math.round((a+b)*100)/100;} )+'%');"),
      paste0("$(api.column(",red_mark,").footer()).css('color','red');"),
      ";}")
  } else {
    javascript <- JS(
      "function(tfoot, data, start, end, display ) {",
      "var api = this.api(), data;",
      "$(api.column(1).footer()).html('TOTAL');",
      paste0("$(api.column(",2:ncol,").footer()).html(api.column(",2:ncol,").data().reduce( function ( a, b ) {return Math.round((a+b)*100)/100;} )+'%');"),
      ";}")
  }
  return(javascript)
}