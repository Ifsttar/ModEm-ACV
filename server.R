library(shiny)

shinyServer(function(input, output, session) {

  # ---- ____Config - Parc ----

 Masse_DV <- Parc_utilisateur[[1]] %>%
    select(RedNOxEuro6,M_carros,M_moteur_ICE,M_moteur_Elec,M_batterie,DV_veh,DV_batterie)
  
 Parc_utilisateur <- Parc_utilisateur %>%
  lapply(function(i) select(i,-RedNOxEuro6,-M_carros,-M_moteur_ICE,-M_moteur_Elec,-M_batterie,-DV_veh,-DV_batterie))
 
 Nom_parc <- data.frame(Abrev = names(Parc_utilisateur), Noms = c("Ile de France 2010","Aire urbaine de Lyon 2015","France 2020"), row.names = names(Parc_utilisateur))

 REACT_Parc_utilisateur <- reactiveVal({
   Parc_utilisateur[[1]]
})
 
observeEvent(c(input$Input_Configurateur_defautimportperso,input$Input_Configurateur_defaut_choixParc,input$Input_Configurateur_perso_choixParc),{
  if(input$Input_Configurateur_defautimportperso == "defaut") {
    new_parc <- Parc_utilisateur[[input$Input_Configurateur_defaut_choixParc]]
    REACT_Parc_utilisateur(new_parc)
  } else if(input$Input_Configurateur_defautimportperso == "perso") {
    new_parc <- Parc_utilisateur[[input$Input_Configurateur_perso_choixParc]]
    REACT_Parc_utilisateur(new_parc)
  } else {
    new_parc <- Parc_utilisateur[[1]]
    REACT_Parc_utilisateur(new_parc)
  }
  
}) 
  
 # Import du parc ----

 Parc_utilisateur_Importe <- reactive({
   req(input$upload_Parc)
   inFile <- input$upload_Parc
   Parc_utilisateur <- as.data.frame(read_excel(inFile$datapath, 1))
   Parc_utilisateur
 })
 
  observeEvent(input$button_utiliser_Config_Parc, {

    if(nrow(Parc_utilisateur_Importe()) > 0) {
    newdf_Parc_initial <- Parc_utilisateur_Importe()
    REACT_Parc_utilisateur(newdf_Parc_initial)
    }

  })

  observeEvent(input$button_NEPASutiliser_Config_Parc, {

    newdf_Parc_initial <- Parc_utilisateur[[1]]
    REACT_Parc_utilisateur(newdf_Parc_initial)

  })
  
  REACT_TitreSynthese <- eventReactive(c(input$Input_Configurateur_defautimportperso,input$Input_Configurateur_defaut_choixParc),{
    temp <- "Parc utilisé : "
    if( input$Input_Configurateur_defautimportperso == "defaut"){
      temp <- paste("Parc utilisé : ", as.character(Nom_parc[input$Input_Configurateur_defaut_choixParc,"Noms"]),sep ="")
    }
    if(input$Input_Configurateur_defautimportperso == "import"){
      temp <- "Parc utilisé : importé"
    }
    if(input$Input_Configurateur_defautimportperso == "perso" ){
      temp <- "Parc utilisé : personnalisé"
    }
    temp
   })

  output$TITRE_SYNTHESE <- renderText({
    REACT_TitreSynthese()
    })
  
  # ---- _Configurateur Motorisation ----
  # -- Motorisation globale --

  REACT_Motorisation_globale <- reactiveValues(ini = data.frame(
    Parc_utilisateur[[1]] %>% group_by(Fuel) %>% summarise(Part = sum(Part)*100)
  ), config = data.frame(
    Parc_utilisateur[[1]] %>% group_by(Fuel) %>% summarise(Part = sum(Part)*100)
  ))
  
  observeEvent(input$Input_Configurateur_perso_choixParc,{
    REACT_Motorisation_globale[["ini"]] <- data.frame(
      Parc_utilisateur[[input$Input_Configurateur_perso_choixParc]] %>% group_by(Fuel) %>% summarise(Part = sum(Part)*100)  
    )
    REACT_Motorisation_globale[["ini"]] <- REACT_Motorisation_globale[["ini"]][tri_perso(REACT_Motorisation_globale[["ini"]][,"Fuel"],couleurs$Id),]
    REACT_Motorisation_globale[["config"]] <- REACT_Motorisation_globale[["ini"]]
  })

  output$download_structure_parc <- downloadHandler(
    
    filename = function() { "structure_parc.xlsx"},
    content = function(file) {write_xlsx(x = parc_vide, 
                                         path = file)} 
  )
  
  output$DT_Config_Motorisation_global <- renderDT({
    
    df <- REACT_Motorisation_globale[["config"]] %>% mutate(Fuel = as.character(Fuel), Part = round(Part,2)) %>%
                  mutate(Fuel = recode(Fuel, "CNG" = "Gaz Naturel", "Petrol" = "Essence", "LPG" = "GPL", "Petrol Hybrid" = "Hybride", "Electric" = "Electrique")) %>% 
      mutate(Part = as.numeric(Part))
    red_mark <- which(round(sum(df[,-1]),2)<99.99 | round(sum(df[,-1]),2)>100.01)+1
    javaFooter <- TableFooter(ncol(df),red_mark)
    datatable(df,
              editable = list(target = REACT_possibilite_editable_Motorisation_globale(),disable = list(columns = 1 )),
              class = "hover cell-border compact",
              caption = "Motorisation du parc",
              container = htmltools::tags$table(tableHeader(c('','Motorisation', 'Part (%)')),tableFooter(rep("", ncol(df)+1))),
              selection = "none",
              options = list(dom = 't',ordering=F,
                             columnDefs = list(list(targets=c(0), visible=F),
                                               list(targets=c(1), visible=TRUE, width='150')),
                             footerCallback = javaFooter)) %>%
      formatCurrency(2,"%",dec.mark = ",",before = FALSE) %>% 
      formatStyle(0,target = "row",fontWeight = styleEqual(7, "bold")) %>% 
      formatStyle("Part",
                  background = styleColorBar(c(0,100), 'lightblue'),
                  backgroundSize = '98% 88%',
                  backgroundRepeat = 'no-repeat',
                  backgroundPosition = 'center') %>%
      formatStyle(1, backgroundColor = styleEqual(couleurs$Nom, couleurs$HEX_aire), fontWeight = "bold")

  })

  observeEvent(input[["DT_Config_Motorisation_global_cell_edit"]], {
    cell <- input[["DT_Config_Motorisation_global_cell_edit"]]
    newdf <- REACT_Motorisation_globale[["config"]]
    newdf[cell$row, cell$col] <- as.numeric(cell$value)
    REACT_Motorisation_globale[["config"]] <- newdf
  })

  output$VerifSomme_Config_Motorisation_global <- renderText({
    if(sum(REACT_Motorisation_globale[["config"]]$Part) > 100.001 | sum(REACT_Motorisation_globale[["config"]]$Part) < 99.999) {paste0("Attention, somme différente de 100 % :"," ", sum(REACT_Motorisation_globale[["config"]]$Part), " %")}
  })
  

  # -- Motorisation par norme Euro --
  REACT_Motorisation_parnorme <- reactiveValues(
    ini = data.frame(
      Parc_utilisateur[[1]] %>% group_by(Euro.Standard,Fuel) %>% summarise(Part = sum(Part)) %>% 
        pivot_wider(names_from = Euro.Standard, values_from = Part, values_fill = list(value = 0)) %>%
        mutate(`Euro 1` = `Euro 1` / sum(`Euro 1`)*100,
               `Euro 2` = `Euro 2` / sum(`Euro 2`)*100,
               `Euro 3` = `Euro 3` / sum(`Euro 3`)*100,
               `Euro 4` = `Euro 4` / sum(`Euro 4`)*100,
               `Euro 5` = `Euro 5` / sum(`Euro 5`)*100,
               `Euro 6b` = `Euro 6b` / sum(`Euro 6b`)*100,
               `Euro 6c` = `Euro 6c` / sum(`Euro 6c`)*100,
               `Euro 6d` = `Euro 6d` / sum(`Euro 6d`)*100)
    ),
    config = data.frame(
      Parc_utilisateur[[1]] %>% group_by(Euro.Standard,Fuel) %>% summarise(Part = sum(Part)) %>% 
        pivot_wider(names_from = Euro.Standard, values_from = Part, values_fill = list(value = 0)) %>%
        mutate(`Euro 1` = `Euro 1` / sum(`Euro 1`)*100,
               `Euro 2` = `Euro 2` / sum(`Euro 2`)*100,
               `Euro 3` = `Euro 3` / sum(`Euro 3`)*100,
               `Euro 4` = `Euro 4` / sum(`Euro 4`)*100,
               `Euro 5` = `Euro 5` / sum(`Euro 5`)*100,
               `Euro 6b` = `Euro 6b` / sum(`Euro 6b`)*100,
               `Euro 6c` = `Euro 6c` / sum(`Euro 6c`)*100,
               `Euro 6d` = `Euro 6d` / sum(`Euro 6d`)*100)
    ))
  
  observeEvent(input$Input_Configurateur_perso_choixParc,{
    REACT_Motorisation_parnorme[["ini"]] <- data.frame(
      Parc_utilisateur[[input$Input_Configurateur_perso_choixParc]] %>% group_by(Euro.Standard,Fuel) %>% summarise(Part = sum(Part)) %>%
        pivot_wider(names_from = Euro.Standard, values_from = Part, values_fill = list(value = 0)) %>%
        mutate(`Euro 1` = `Euro 1` / sum(`Euro 1`)*100,
               `Euro 2` = `Euro 2` / sum(`Euro 2`)*100,
               `Euro 3` = `Euro 3` / sum(`Euro 3`)*100,
               `Euro 4` = `Euro 4` / sum(`Euro 4`)*100,
               `Euro 5` = `Euro 5` / sum(`Euro 5`)*100,
               `Euro 6b` = `Euro 6b` / sum(`Euro 6b`)*100,
               `Euro 6c` = `Euro 6c` / sum(`Euro 6c`)*100,
               `Euro 6d` = `Euro 6d` / sum(`Euro 6d`)*100)
    )
    REACT_Motorisation_parnorme[["ini"]][is.na(REACT_Motorisation_parnorme[["ini"]])] <- 0
    REACT_Motorisation_parnorme[["ini"]] <- REACT_Motorisation_parnorme[["ini"]][tri_perso(REACT_Motorisation_parnorme[["ini"]][,"Fuel"],couleurs$Id),]
    REACT_Motorisation_parnorme[["config"]] <- REACT_Motorisation_parnorme[["ini"]]
  })
  
  
  output$DT_Config_Motorisation_parnorme <- renderDT({
    
    df <- REACT_Motorisation_parnorme[["config"]] %>% 
                  mutate(Fuel = as.character(Fuel)) %>% mutate_at(vars(-c(Fuel)), round,2) %>%
                  mutate(Fuel = recode(Fuel, "CNG" = "Gaz Naturel", "Petrol" = "Essence", "LPG" = "GPL", "Petrol Hybrid" = "Hybride", "Electric" = "Electrique"))
    if(input$Input_Configurateur_perso_Euro == "global") {desactiv <- as.vector(c(1,which(REACT_Euro_globale[["config"]][,-1]==0)+1))}
    if(input$Input_Configurateur_perso_Euro == "parmotorisation") {desactiv <- as.vector(c(1,which(rowSums(REACT_Euro_parmotorisation[["config"]][,-1])==0)+1))}
    if(input$Input_Configurateur_perso_Euro == "parsegment") {desactiv <- as.vector(c(1,which(rowSums(REACT_Euro_parsegment[["config"]][,-1])==0)+1))}
    red_mark <- which(round(colSums(df[,-1]),2)<99.99 | round(colSums(df[,-1]),2)>100.01)+1
    javaFooter <- TableFooter(ncol(df),red_mark)
    datatable(df,
              editable = list(target = REACT_possibilite_editable_Motorisation_parnorme(),disable = list(columns = desactiv )),
              class = "hover cell-border compact",
              caption = "Motorisation par norme Euro",
              container = htmltools::tags$table(tableHeader(c('','Motorisation', renames_all(names(df)[-1]))),tableFooter(rep("", ncol(df)+1))),
              selection = "none",
              options = list(dom = 't',ordering=F,
                             columnDefs = list(list(className = 'dt-right', targets = 2:9),
                                               list(targets=c(0), visible=F),
                                               list(targets=c(1:ncol(df)), visible=TRUE, width='150')),
                             footerCallback = javaFooter)) %>% 
      formatCurrency(2:ncol(df),"%",dec.mark = ",",before = FALSE) %>%
      formatStyle(0,target = "row",fontWeight = styleEqual(7, "bold"))  %>% 
      formatStyle(names(df %>% select(-Fuel)),
                  background = styleColorBar(c(0,100), 'lightblue'),
                  backgroundSize = '98% 88%',
                  backgroundRepeat = 'no-repeat',
                  backgroundPosition = 'center')%>%
      formatStyle(1, backgroundColor = styleEqual(couleurs$Nom, couleurs$HEX_aire), fontWeight = "bold")
    
  })

  observeEvent(input[["DT_Config_Motorisation_parnorme_cell_edit"]], {
    cell <- input[["DT_Config_Motorisation_parnorme_cell_edit"]]
    newdf <- REACT_Motorisation_parnorme[["config"]]
    newdf[cell$row, cell$col] <- as.numeric(cell$value)
    REACT_Motorisation_parnorme[["config"]] <- newdf
  })
  
  # -- Motorisation par segment de puissance --
  REACT_Motorisation_parsegment <- reactiveValues(
    ini = data.frame(
      Parc_utilisateur[[1]] %>% group_by(Segment,Fuel) %>% summarise(Part = sum(Part)) %>% 
        pivot_wider(names_from = Segment, values_from = Part, values_fill = list(value = 0)) %>%
        mutate(Large = Large / sum(Large)*100,
               Medium = Medium / sum(Medium)*100,
               Mini = Mini / sum(Mini)*100,
               Small = Small / sum(Small)*100) %>% 
        select(Fuel,Mini,Small,Medium,Large)	 
    ),
    config = data.frame(
      Parc_utilisateur[[1]] %>% group_by(Segment,Fuel) %>% summarise(Part = sum(Part)) %>% 
        pivot_wider(names_from = Segment, values_from = Part, values_fill = list(value = 0)) %>%
        mutate(Large = Large / sum(Large)*100,
               Medium = Medium / sum(Medium)*100,
               Mini = Mini / sum(Mini)*100,
               Small = Small / sum(Small)*100) %>% 
        select(Fuel,Mini,Small,Medium,Large)	 
    ))
  
  observeEvent(input$Input_Configurateur_perso_choixParc,{
    REACT_Motorisation_parsegment[["ini"]] <- data.frame(
      Parc_utilisateur[[input$Input_Configurateur_perso_choixParc]] %>% group_by(Segment,Fuel) %>% summarise(Part = sum(Part)) %>%
        pivot_wider(names_from = Segment, values_from = Part, values_fill = list(value = 0)) %>%
        mutate(Large = Large / sum(Large)*100,
               Medium = Medium / sum(Medium)*100,
               Mini = Mini / sum(Mini)*100,
               Small = Small / sum(Small)*100) %>% 
        select(Fuel,Mini,Small,Medium,Large)	 
    )
    REACT_Motorisation_parsegment[["ini"]][is.na(REACT_Motorisation_parsegment[["ini"]])] <- 0
    REACT_Motorisation_parsegment[["ini"]] <- REACT_Motorisation_parsegment[["ini"]][tri_perso(REACT_Motorisation_parsegment[["ini"]][,"Fuel"],couleurs$Id),]
    REACT_Motorisation_parsegment[["config"]] <- REACT_Motorisation_parsegment[["ini"]]
  })
  
  
  output$DT_Config_Motorisation_parsegment <- renderDT({
    
    df <- REACT_Motorisation_parsegment[["config"]] %>% 
                      mutate(Fuel = as.character(Fuel)) %>% mutate_at(vars(-c(Fuel)), round,2) %>%
                      mutate(Fuel = recode(Fuel, "CNG" = "Gaz Naturel", "Petrol" = "Essence", "LPG" = "GPL", "Petrol Hybrid" = "Hybride", "Electric" = "Electrique"))
    if(input$Input_Configurateur_perso_Segment == "global") {desactiv <- as.vector(c(1,which(REACT_Segment_globale[["config"]][,-1]==0)+1))}
    if(input$Input_Configurateur_perso_Segment == "parmotorisation") {desactiv <- as.vector(c(1,which(rowSums(REACT_Segment_parmotorisation[["config"]][,-1])==0)+1))}
    if(input$Input_Configurateur_perso_Segment == "parnorme") {desactiv <- as.vector(c(1,which(rowSums(REACT_Segment_parnorme[["config"]][,-1])==0)+1))}
    red_mark <- which(round(colSums(df[,-1]),2)<99.99 | round(colSums(df[,-1]),2)>100.01)+1
    javaFooter <- TableFooter(ncol(df),red_mark)
    datatable(df,
              editable = list(target = REACT_possibilite_editable_Motorisation_parsegment(),disable = list(columns = desactiv )),
              class = "hover cell-border compact",
              caption = "Motorisation par segment de puissance",
              container = htmltools::tags$table(tableHeader(c('','Motorisation', renames_all(names(df)[-1]))),tableFooter(rep("", ncol(df)+1))),
              selection = "none",
              options = list(dom = 't',ordering=F,
                             columnDefs = list(list(className = 'dt-right', targets = 2:5),
                                               list(targets=c(0), visible=F),
                                               list(targets=c(1:ncol(df)), visible=TRUE, width='150')),
                             footerCallback = javaFooter)) %>% 
      formatCurrency(2:ncol(df),"%",dec.mark = ",",before = FALSE) %>%
      formatStyle(0,target = "row",fontWeight = styleEqual(7, "bold"))  %>% 
      formatStyle(names(df %>% select(-Fuel)),
                  background = styleColorBar(c(0,100), 'lightblue'),
                  backgroundSize = '98% 88%',
                  backgroundRepeat = 'no-repeat',
                  backgroundPosition = 'center') %>%
      formatStyle(1, backgroundColor = styleEqual(couleurs$Nom, couleurs$HEX_aire), fontWeight = "bold")
  })
  
  observeEvent(input[["DT_Config_Motorisation_parsegment_cell_edit"]], {
    cell <- input[["DT_Config_Motorisation_parsegment_cell_edit"]]
    newdf <- REACT_Motorisation_parsegment[["config"]]
    newdf[cell$row, cell$col] <- as.numeric(cell$value)
    REACT_Motorisation_parsegment[["config"]] <- newdf
  })
  
  # ----  Info modif Config Motorisation ----
  # Indicateurs de modification des tables
  REACT_Modifications_Config_Motorisation_global <- reactive({
    if( setequal(c(REACT_Motorisation_globale[["config"]]$Part),c(REACT_Motorisation_globale[["ini"]]$Part)) == FALSE) {1} else {0}
  })
  
  REACT_Modifications_Config_Motorisation_parnorme <- reactive({
    if( setequal(c(REACT_Motorisation_parnorme[["config"]]$Euro.1),c(REACT_Motorisation_parnorme[["ini"]]$Euro.1)) == FALSE | 
        setequal(c(REACT_Motorisation_parnorme[["config"]]$Euro.2),c(REACT_Motorisation_parnorme[["ini"]]$Euro.2)) == FALSE |
        setequal(c(REACT_Motorisation_parnorme[["config"]]$Euro.3),c(REACT_Motorisation_parnorme[["ini"]]$Euro.3)) == FALSE |
        setequal(c(REACT_Motorisation_parnorme[["config"]]$Euro.4),c(REACT_Motorisation_parnorme[["ini"]]$Euro.4)) == FALSE |
        setequal(c(REACT_Motorisation_parnorme[["config"]]$Euro.5),c(REACT_Motorisation_parnorme[["ini"]]$Euro.5)) == FALSE |
        setequal(c(REACT_Motorisation_parnorme[["config"]]$Euro.6b),c(REACT_Motorisation_parnorme[["ini"]]$Euro.6b)) == FALSE |
        setequal(c(REACT_Motorisation_parnorme[["config"]]$Euro.6c),c(REACT_Motorisation_parnorme[["ini"]]$Euro.6c)) == FALSE |
        setequal(c(REACT_Motorisation_parnorme[["config"]]$Euro.6d),c(REACT_Motorisation_parnorme[["ini"]]$Euro.6d)) == FALSE )
        {1} else {0}
  })
  
  REACT_Modifications_Config_Motorisation_parsegment <- reactive({
    if( setequal(c(REACT_Motorisation_parsegment[["config"]]$Mini),c(REACT_Motorisation_parsegment[["ini"]]$Mini)) == FALSE |
        setequal(c(REACT_Motorisation_parsegment[["config"]]$Small),c(REACT_Motorisation_parsegment[["ini"]]$Small)) == FALSE |
        setequal(c(REACT_Motorisation_parsegment[["config"]]$Medium),c(REACT_Motorisation_parsegment[["ini"]]$Medium)) == FALSE |
        setequal(c(REACT_Motorisation_parsegment[["config"]]$Large),c(REACT_Motorisation_parsegment[["ini"]]$Large)) == FALSE ) 
      {1} else {0}
  })
  
  # Indication de modification d'une des tables de motorisation
  output$Modifs_Config_Motorisation <- renderText({
    if(REACT_Modifications_Config_Motorisation_global() == 1) {
      "Modifications des paramètres - Motorisation globale : " } else if (REACT_Modifications_Config_Motorisation_parnorme() == 1) {
        "Modifications des paramètres - Motorisation par norme : " } else if (REACT_Modifications_Config_Motorisation_parsegment() == 1) {
          "Modifications des paramètres - Motorisation par segment de puissance : " } else {"Paramètres de motorisation non modifiés"}
  })
  output$Validation_Config_Motorisation <- renderText({
    if(REACT_Modifications_Config_Motorisation_global() == 1) {
      "Modifications des paramètres - Motorisation globale : " } else if (REACT_Modifications_Config_Motorisation_parnorme() == 1) {
        "Modifications des paramètres - Motorisation par norme : " } else if (REACT_Modifications_Config_Motorisation_parsegment() == 1) {
          "Modifications des paramètres - Motorisation par segment de puissance : " } else {"Paramètres de motorisation non modifiés"}
  })
  
  # Possibilité d'éditions des 3 tables de motorisation
  REACT_possibilite_editable_Motorisation_globale <- reactive({
    if(REACT_Modifications_Config_Motorisation_parnorme() == 0 &
       REACT_Modifications_Config_Motorisation_parsegment() == 0) {
      "cell"} else {FALSE}
  })
  REACT_possibilite_editable_Motorisation_parnorme <- reactive({
    if(REACT_Modifications_Config_Motorisation_global() == 0 &
       REACT_Modifications_Config_Motorisation_parsegment() == 0) {
      "cell"} else {FALSE}
  })
  REACT_possibilite_editable_Motorisation_parsegment <- reactive({
    if(REACT_Modifications_Config_Motorisation_global() == 0 &
       REACT_Modifications_Config_Motorisation_parnorme() == 0) {
      "cell"} else {FALSE}
  })
  
  # Reset des 3 tables de motorisation
  observeEvent(input$button_reset_Config_Motorisation, {
    newdf_moto <- REACT_Motorisation_globale[["ini"]]
    REACT_Motorisation_globale[["config"]] <- newdf_moto
    
    newdf_moto_age <- REACT_Motorisation_parnorme[["ini"]]
    REACT_Motorisation_parnorme[["config"]] <- newdf_moto_age
    
    newdf_moto_segment <- REACT_Motorisation_parsegment[["ini"]]
    REACT_Motorisation_parsegment[["config"]] <- newdf_moto_segment
  })
  

  # ---- _Configurateur Euro ----
  REACT_Euro_globale <- reactiveValues(
    ini = data.frame(Parc_utilisateur[[1]] %>% 
                       group_by(Euro.Standard) %>% summarise(Part = sum(Part)*100)),
    config = data.frame(Parc_utilisateur[[1]] %>% 
                          group_by(Euro.Standard) %>% summarise(Part = sum(Part)*100))
  )
  
  observeEvent(input$Input_Configurateur_perso_choixParc,{
    REACT_Euro_globale[["ini"]] <- data.frame(
      Parc_utilisateur[[input$Input_Configurateur_perso_choixParc]] %>% group_by(Euro.Standard) %>% summarise(Part = sum(Part)*100)
    )
  REACT_Euro_globale[["config"]] <- REACT_Euro_globale[["ini"]]
  })
  
  output$DT_Config_Euro_global <- renderDT({
    df <- REACT_Euro_globale[["config"]] %>% mutate(Euro.Standard = as.character(Euro.Standard)) %>% mutate_at(vars(-c(Euro.Standard)), round,2)
    red_mark <- which(round(sum(df[,-1]),2)<99.99 | round(sum(df[,-1]),2)>100.01)+1
    javaFooter <- TableFooter(ncol(df),red_mark)
    
    datatable(df,
              editable = list(target = REACT_possibilite_editable_Euro_globale(),disable = list(columns = 1 )),
              colnames = c('Norme Euro', 'Part (%)'),
              class = "hover cell-border compact",
              caption = "Normes Euro du parc",
              container = htmltools::tags$table(tableHeader(c('','Norme Euro', 'Part (%)')),tableFooter(rep("", ncol(df)+1))),
              selection = "none",
              options = list(dom = 't',ordering=F,
                             columnDefs = list(list(className = 'dt-right', targets = 2),
                                               list(targets=c(0), visible=F),
                                               list(targets=c(1), visible=TRUE, width='150')),
                             footerCallback = javaFooter)) %>%
      formatCurrency(2,"%",dec.mark = ",",before = FALSE) %>% 
      formatStyle(0,target = "row",fontWeight = styleEqual(9, "bold")) %>% 
      formatStyle("Part",
                  background = styleColorBar(c(0,100), 'lightblue'),
                  backgroundSize = '98% 88%',
                  backgroundRepeat = 'no-repeat',
                  backgroundPosition = 'center') %>%
      formatStyle(1, backgroundColor = styleEqual(couleurs$Nom, couleurs$HEX_aire), fontWeight = "bold")
  })
  
  observeEvent(input[["DT_Config_Euro_global_cell_edit"]], {
    cell <- input[["DT_Config_Euro_global_cell_edit"]]
    newdf <- REACT_Euro_globale[["config"]]
    newdf[cell$row, cell$col] <- as.numeric(cell$value)
    REACT_Euro_globale[["config"]] <- newdf
  })
  
  output$VerifSomme_Config_Euro_global <- renderText({
    if(round(sum(REACT_Euro_globale[["config"]]$Part),0) != 100) {paste0("Attention, somme différente de 100 % :"," ", round(sum(REACT_Euro_globale[["config"]]$Part),0), " %")}
  })
  
  # -- Norme Euro par motorisation --
  REACT_Euro_parmotorisation <- reactiveValues(
    ini = data.frame(    Parc_utilisateur[[1]] %>%
                           group_by(Fuel,Euro.Standard) %>% summarise(Part = sum(Part)) %>% 
                           pivot_wider(names_from = Fuel, values_from = Part, values_fill = list(value = 0)) %>%
                           mutate(CNG = CNG / sum(CNG)*100,
                                  Diesel = Diesel / sum(Diesel)*100,
                                  Electric = Electric / sum(Electric)*100,
                                  LPG = LPG / sum(LPG)*100,
                                  Petrol = Petrol / sum(Petrol)*100,
                                  `Petrol Hybrid` = `Petrol Hybrid` / sum(`Petrol Hybrid`)*100)
    ),
    config = data.frame(    Parc_utilisateur[[1]] %>%
                              group_by(Fuel,Euro.Standard) %>% summarise(Part = sum(Part)) %>% 
                              pivot_wider(names_from = Fuel, values_from = Part, values_fill = list(value = 0)) %>%
                              mutate(CNG = CNG / sum(CNG)*100,
                                     Diesel = Diesel / sum(Diesel)*100,
                                     Electric = Electric / sum(Electric)*100,
                                     LPG = LPG / sum(LPG)*100,
                                     Petrol = Petrol / sum(Petrol)*100,
                                     `Petrol Hybrid` = `Petrol Hybrid` / sum(`Petrol Hybrid`)*100)
    ))
  
  observeEvent(input$Input_Configurateur_perso_choixParc,{
    REACT_Euro_parmotorisation[["ini"]] <- data.frame(
      Parc_utilisateur[[input$Input_Configurateur_perso_choixParc]] %>% group_by(Fuel,Euro.Standard) %>% summarise(Part = sum(Part)) %>%
        pivot_wider(names_from = Fuel, values_from = Part, values_fill = list(value = 0)) %>%
        mutate(CNG = CNG / sum(CNG)*100,
               Diesel = Diesel / sum(Diesel)*100,
               Electric = Electric / sum(Electric)*100,
               LPG = LPG / sum(LPG)*100,
               Petrol = Petrol / sum(Petrol)*100,
               `Petrol Hybrid` = `Petrol Hybrid` / sum(`Petrol Hybrid`)*100)
    )
    REACT_Euro_parmotorisation[["ini"]][is.na(REACT_Euro_parmotorisation[["ini"]])] <- 0
    REACT_Euro_parmotorisation[["ini"]] <- REACT_Euro_parmotorisation[["ini"]][,c(1,tri_perso(names(REACT_Euro_parmotorisation[["ini"]])[-1],couleurs$Id)+1)]
    REACT_Euro_parmotorisation[["config"]] <- REACT_Euro_parmotorisation[["ini"]]
  })
  
  output$DT_Config_Euro_parmotorisation <- renderDT({
    
    if(REACT_Modifications_Config_Motorisation_parnorme() == 1){
      datatable(data.frame(Impossible = c("Combinaison déjà effectuée avec les paramètres fixés précédemment : motorisation par normes Euro")),
                class = "hover cell-border compact",
                caption = "Norme Euro par motorisation",
                options = list(dom = 't'),
                rownames = F
      )
    } else {
      
      df <- REACT_Euro_parmotorisation[["config"]] %>% 
        mutate(Euro.Standard = as.character(Euro.Standard)) %>% mutate_at(vars(-c(Euro.Standard)), round,2)
      if(input$Input_Configurateur_perso_Motorisation == "global") {desactiv <- as.vector(c(1,which(REACT_Motorisation_globale[["config"]][,-1]==0)+1))}
      if(input$Input_Configurateur_perso_Motorisation == "parnorme") {desactiv <- as.vector(c(1,which(rowSums(REACT_Motorisation_parnorme[["config"]][,-1])==0)+1))}
      if(input$Input_Configurateur_perso_Motorisation == "parsegment") {desactiv <- as.vector(c(1,which(rowSums(REACT_Motorisation_parsegment[["config"]][,-1])==0)+1))}
      red_mark <- which(round(colSums(df[,-1]),2)<99.99 | round(colSums(df[,-1]),2)>100.01)+1
      javaFooter <- TableFooter(ncol(df),red_mark)
      
      datatable(df,
                editable = list(target = REACT_possibilite_editable_Euro_parmotorisation(),disable = list(columns = desactiv )),
                class = "hover cell-border compact",
                caption = "Norme Euro par motorisation",
                container = htmltools::tags$table(tableHeader(c('','Norme Euro', renames_all(names(df)[-1]))),tableFooter(rep("", ncol(df)+1))),
                selection = "none",
                options = list(dom = 't',ordering=F,
                               columnDefs = list(list(className = 'dt-right', targets = 2:7),
                                                 list(targets=c(0), visible=F),
                                                 list(targets=c(1:ncol(df)), visible=TRUE, width='100')),
                               footerCallback = javaFooter)) %>% 
        formatCurrency(2:ncol(df),"%",dec.mark = ",",before = FALSE) %>% 
        formatStyle(0,target = "row",fontWeight = styleEqual(9, "bold"))  %>% 
        formatStyle(names(df %>% select(-Euro.Standard)),
                    background = styleColorBar(c(0,100), 'lightblue'),
                    backgroundSize = '98% 88%',
                    backgroundRepeat = 'no-repeat',
                    backgroundPosition = 'center') %>%
        formatStyle(1, backgroundColor = styleEqual(couleurs$Nom, couleurs$HEX_aire), fontWeight = "bold")
    }
  })
  
  observeEvent(input[["DT_Config_Euro_parmotorisation_cell_edit"]], {
    cell <- input[["DT_Config_Euro_parmotorisation_cell_edit"]]
    newdf <- REACT_Euro_parmotorisation[["config"]]
    newdf[cell$row, cell$col] <- as.numeric(cell$value)
    REACT_Euro_parmotorisation[["config"]] <- newdf
  })
  
  # -- Norme Euro par segment de puissance --
  REACT_Euro_parsegment <- reactiveValues(
    ini = data.frame(Parc_utilisateur[[1]] %>%
                       group_by(Segment,Euro.Standard) %>% summarise(Part = sum(Part)) %>% 
                       pivot_wider(names_from = Segment, values_from = Part, values_fill = list(value = 0)) %>%
                       mutate(Large = Large / sum(Large)*100,
                              Medium = Medium / sum(Medium)*100,
                              Mini = Mini / sum(Mini)*100,
                              Small = Small / sum(Small)*100) %>%
                       select(Euro.Standard,Mini,Small,Medium,Large)
    ),
    config = data.frame(Parc_utilisateur[[1]] %>%
                          group_by(Segment,Euro.Standard) %>% summarise(Part = sum(Part)) %>% 
                          pivot_wider(names_from = Segment, values_from = Part, values_fill = list(value = 0)) %>%
                          mutate(Large = Large / sum(Large)*100,
                                 Medium = Medium / sum(Medium)*100,
                                 Mini = Mini / sum(Mini)*100,
                                 Small = Small / sum(Small)*100) %>%
                          select(Euro.Standard,Mini,Small,Medium,Large)
    ))
  
  observeEvent(input$Input_Configurateur_perso_choixParc,{
    REACT_Euro_parsegment[["ini"]] <- data.frame(
      Parc_utilisateur[[input$Input_Configurateur_perso_choixParc]] %>% group_by(Segment,Euro.Standard) %>% summarise(Part = sum(Part)) %>% 
        pivot_wider(names_from = Segment, values_from = Part, values_fill = list(value = 0)) %>%
        mutate(Large = Large / sum(Large)*100,
               Medium = Medium / sum(Medium)*100,
               Mini = Mini / sum(Mini)*100,
               Small = Small / sum(Small)*100) %>%
        select(Euro.Standard,Mini,Small,Medium,Large)
    )
    REACT_Euro_parsegment[["ini"]][is.na(REACT_Euro_parsegment[["ini"]])] <- 0
    REACT_Euro_parsegment[["config"]] <- REACT_Euro_parsegment[["ini"]]
  })
  
  output$DT_Config_Euro_parsegment <- renderDT({
    df <- REACT_Euro_parsegment[["config"]] %>% 
                      mutate(Euro.Standard = as.character(Euro.Standard)) %>% mutate_at(vars(-c(Euro.Standard)), round,2)
    if(input$Input_Configurateur_perso_Segment == "global") {desactiv <- as.vector(c(1,which(REACT_Segment_globale[["config"]][,-1]==0)+1))}
    if(input$Input_Configurateur_perso_Segment == "parmotorisation") {desactiv <- as.vector(c(1,which(rowSums(REACT_Segment_parmotorisation[["config"]][,-1])==0)+1))}
    if(input$Input_Configurateur_perso_Segment == "parnorme") {desactiv <- as.vector(c(1,which(rowSums(REACT_Segment_parnorme[["config"]][,-1])==0)+1))}
    red_mark <- which(round(colSums(df[,-1]),2)<99.99 | round(colSums(df[,-1]),2)>100.01)+1
    javaFooter <- TableFooter(ncol(df),red_mark)
    datatable(df,
              editable = list(target = REACT_possibilite_editable_Euro_parsegment(),disable = list(columns = desactiv )),
              colnames = c('Norme Euro', 'Mini', 'Petit', 'Medium', 'Large'),
              class = "hover cell-border compact",
              caption = "Norme Euro par segment de puissance",
              container = htmltools::tags$table(tableHeader(c('','Norme Euro', renames_all(names(df)[-1]))),tableFooter(rep("", ncol(df)+1))),
              selection = "none",
    options = list(dom = 't',ordering=F,
                   columnDefs = list(list(className = 'dt-right', targets = 2:5),
                                     list(targets=c(0), visible=F),
                                     list(targets=c(1:ncol(df)), visible=TRUE, width='150')),
                   footerCallback = javaFooter)) %>% 
      formatCurrency(2:ncol(df),"%",dec.mark = ",",before = FALSE) %>% 
      formatStyle(0,target = "row",fontWeight = styleEqual(9, "bold"))  %>% 
      formatStyle(names(df %>% select(-Euro.Standard)),
                  background = styleColorBar(c(0,100), 'lightblue'),
                  backgroundSize = '98% 88%',
                  backgroundRepeat = 'no-repeat',
                  backgroundPosition = 'center') %>%
      formatStyle(1, backgroundColor = styleEqual(couleurs$Nom, couleurs$HEX_aire), fontWeight = "bold")
  })
  
  observeEvent(input[["DT_Config_Euro_parsegment_cell_edit"]], {
    cell <- input[["DT_Config_Euro_parsegment_cell_edit"]]
    newdf <- REACT_Euro_parsegment[["config"]]
    newdf[cell$row, cell$col] <- as.numeric(cell$value)
    REACT_Euro_parsegment[["config"]] <- newdf
  })
  
  # ----  Info modif Config Euro ----
  # Indicateurs de modification des tables
  REACT_Modifications_Config_Euro_global <- reactive({
    if( setequal(c(REACT_Euro_globale[["config"]]$Part),c(REACT_Euro_globale[["ini"]]$Part)) == FALSE ) {1} else {0}
  })
  REACT_Modifications_Config_Euro_parmotorisation <- reactive({
    if( setequal(c(REACT_Euro_parmotorisation[["config"]]$CNG),c(REACT_Euro_parmotorisation[["ini"]]$CNG)) == FALSE |
        setequal(c(REACT_Euro_parmotorisation[["config"]]$Diesel),c(REACT_Euro_parmotorisation[["ini"]]$Diesel)) == FALSE |
        setequal(c(REACT_Euro_parmotorisation[["config"]]$Electric),c(REACT_Euro_parmotorisation[["ini"]]$Electric)) == FALSE |
        setequal(c(REACT_Euro_parmotorisation[["config"]]$LPG),c(REACT_Euro_parmotorisation[["ini"]]$LPG)) == FALSE |
        setequal(c(REACT_Euro_parmotorisation[["config"]]$Petrol),c(REACT_Euro_parmotorisation[["ini"]]$Petrol)) == FALSE |
        setequal(c(REACT_Euro_parmotorisation[["config"]]$Petrol.Hybrid),c(REACT_Euro_parmotorisation[["ini"]]$Petrol.Hybrid)) == FALSE ) {1} else {0}
  })
  REACT_Modifications_Config_Euro_parsegment <- reactive({
    if( setequal(c(REACT_Euro_parsegment[["config"]]$Mini),c(REACT_Euro_parsegment[["ini"]]$Mini)) == FALSE | 
        setequal(c(REACT_Euro_parsegment[["config"]]$Small),c(REACT_Euro_parsegment[["ini"]]$Small)) == FALSE | 
        setequal(c(REACT_Euro_parsegment[["config"]]$Medium),c(REACT_Euro_parsegment[["ini"]]$Medium)) == FALSE | 
        setequal(c(REACT_Euro_parsegment[["config"]]$Large),c(REACT_Euro_parsegment[["ini"]]$Large)) == FALSE ) {1} else {0}
  })
  
  # Indication de modification d'une des tables de normes Euro
  output$Modifs_Config_Euro <- renderText({
    if(REACT_Modifications_Config_Euro_global() == 1) {
      "Modifications des paramètres - Euro globale :" } else if (REACT_Modifications_Config_Euro_parmotorisation() == 1) {
        "Modifications des paramètres - Euro par motorisation :" } else if (REACT_Modifications_Config_Euro_parsegment() == 1) {
          "Modifications des paramètres - Euro par segment de puissance :" } else {"Paramètres de Euro non modifiés"}
  })
  output$Validation_Config_Euro <- renderText({
    if(REACT_Modifications_Config_Euro_global() == 1) {
      "Modifications des paramètres - Euro globale :" } else if (REACT_Modifications_Config_Euro_parmotorisation() == 1) {
        "Modifications des paramètres - Euro par motorisation :" } else if (REACT_Modifications_Config_Euro_parsegment() == 1) {
          "Modifications des paramètres - Euro par segment de puissance :" } else {"Paramètres de Euro non modifiés"}
  })
  
  # Possibilité d'éditions des 3 tables de normes Euro
  REACT_possibilite_editable_Euro_globale <- reactive({
    if(REACT_Modifications_Config_Euro_parmotorisation() == 0 &
       REACT_Modifications_Config_Euro_parsegment() == 0) {
      "cell"} else {FALSE}
  })
  REACT_possibilite_editable_Euro_parmotorisation <- reactive({
    if(REACT_Modifications_Config_Euro_global() == 0 &
       REACT_Modifications_Config_Euro_parsegment() == 0) {
      "cell"} else {FALSE}
  })
  REACT_possibilite_editable_Euro_parsegment <- reactive({
    if(REACT_Modifications_Config_Euro_global() == 0 &
       REACT_Modifications_Config_Euro_parmotorisation() == 0) {
      "cell"} else {FALSE}
  })
  
  # Reset des 3 tables de normes Euro
  observeEvent(input$button_reset_Config_Euro, {
    newdf_euro <- REACT_Euro_globale[["ini"]]
    REACT_Euro_globale[["config"]] <- newdf_euro
    
    newdf_euro_moto <- REACT_Euro_parmotorisation[["ini"]]
    REACT_Euro_parmotorisation[["config"]] <- newdf_euro_moto
    
    newdf_euro_segment <- REACT_Euro_parsegment[["ini"]]
    REACT_Euro_parsegment[["config"]] <- newdf_euro_segment
  })
  
  # ---- _Configurateur Segment de puissance ----
  REACT_Segment_globale <- reactiveValues(
    ini = data.frame(
      Parc_utilisateur[[1]] %>% group_by(Segment) %>% summarise(Part = sum(Part)*100)
    ),
    config = data.frame(
      Parc_utilisateur[[1]] %>% group_by(Segment) %>% summarise(Part = sum(Part)*100)
    ))
  
  observeEvent(input$Input_Configurateur_perso_choixParc,{
    REACT_Segment_globale[["ini"]] <- data.frame(
      Parc_utilisateur[[input$Input_Configurateur_perso_choixParc]] %>% group_by(Segment) %>% summarise(Part = sum(Part)*100)
    )
    REACT_Segment_globale[["ini"]] <- REACT_Segment_globale[["ini"]][tri_perso(REACT_Segment_globale[["ini"]][,1],couleurs$Id),]
    REACT_Segment_globale[["config"]] <- REACT_Segment_globale[["ini"]]
  })
  
  output$DT_Config_Segment_global <- renderDT({
    df <- REACT_Segment_globale[["config"]] %>% mutate(Segment = as.character(Segment)) %>% mutate_at(vars(-c(Segment)), round,2) %>%
      mutate(Segment = recode(Segment, "Small" = "Petit"))
    red_mark <- which(round(sum(df[,-1]),2)<99.99 | round(sum(df[,-1]),2)>100.01)+1
    javaFooter <- TableFooter(ncol(df),red_mark)
    datatable(df,
              editable = list(target = REACT_possibilite_editable_Segment_globale(),disable = list(columns = 1 )),
              class = "hover cell-border compact",
              caption = "Segment de puissance du parc",
              container = htmltools::tags$table(tableHeader(c('','Segment de puissance', 'Part (%)')),tableFooter(rep("", ncol(df)+1))),
              selection = "none",
              options = list(dom = 't',ordering=F,
                             columnDefs = list(list(className = 'dt-right', targets = 2),
                                               list(targets=c(0), visible=F),
                                               list(targets=c(1), visible=TRUE, width='150')),
                             footerCallback = javaFooter)) %>% 
      formatCurrency(2,"%",dec.mark = ",",before = FALSE) %>% 
      formatStyle(0,target = "row",fontWeight = styleEqual(5, "bold")) %>% 
      formatStyle("Part",
                  background = styleColorBar(c(0,100), 'lightblue'),
                  backgroundSize = '98% 88%',
                  backgroundRepeat = 'no-repeat',
                  backgroundPosition = 'center') %>%
      formatStyle(1, backgroundColor = styleEqual(couleurs$Nom, couleurs$HEX_aire), fontWeight = "bold") 
  })
  
  observeEvent(input[["DT_Config_Segment_global_cell_edit"]], {
    cell <- input[["DT_Config_Segment_global_cell_edit"]]
    newdf <- REACT_Segment_globale[["config"]]
    newdf[cell$row, cell$col] <- as.numeric(cell$value)
    REACT_Segment_globale[["config"]] <- newdf
  })
  
  output$VerifSomme_Config_Segment_global <- renderText({
    if(sum(REACT_Segment_globale[["config"]]$Part) > 100.001 | sum(REACT_Segment_globale[["config"]]$Part) < 99.999) {paste0("Attention, somme différente de 100 % :"," ", sum(REACT_Segment_globale[["config"]]$Part), " %")}
  })
  
  # -- Segment de puissance par motorisation --
  REACT_Segment_parmotorisation <- reactiveValues(
    ini = data.frame(
      Parc_utilisateur[[1]] %>% group_by(Fuel,Segment) %>% summarise(Part = sum(Part)) %>% 
        pivot_wider(names_from = Fuel, values_from = Part, values_fill = list(value = 0)) %>%
        mutate(CNG = CNG / sum(CNG)*100,
               Diesel = Diesel / sum(Diesel)*100,
               Electric = Electric / sum(Electric)*100,
               LPG = LPG / sum(LPG)*100,
               Petrol = Petrol / sum(Petrol)*100,
               `Petrol Hybrid` = `Petrol Hybrid` / sum(`Petrol Hybrid`)*100)  %>%
        mutate(OrdreSegment = c("d","c","a","b")) %>% arrange(OrdreSegment) %>% select(-OrdreSegment)
    ),
    config = data.frame(
      Parc_utilisateur[[1]] %>% group_by(Fuel,Segment) %>% summarise(Part = sum(Part)) %>% 
        pivot_wider(names_from = Fuel, values_from = Part, values_fill = list(value = 0)) %>%
        mutate(CNG = CNG / sum(CNG)*100,
               Diesel = Diesel / sum(Diesel)*100,
               Electric = Electric / sum(Electric)*100,
               LPG = LPG / sum(LPG)*100,
               Petrol = Petrol / sum(Petrol)*100,
               `Petrol Hybrid` = `Petrol Hybrid` / sum(`Petrol Hybrid`)*100)  %>%
        mutate(OrdreSegment = c("d","c","a","b")) %>% arrange(OrdreSegment) %>% select(-OrdreSegment)
    ))
  
  observeEvent(input$Input_Configurateur_perso_choixParc,{
    REACT_Segment_parmotorisation[["ini"]] <- data.frame(
      Parc_utilisateur[[input$Input_Configurateur_perso_choixParc]] %>% group_by(Fuel,Segment) %>% summarise(Part = sum(Part)) %>%
        pivot_wider(names_from = Fuel, values_from = Part, values_fill = list(value = 0)) %>%
        mutate(CNG = CNG / sum(CNG)*100,
               Diesel = Diesel / sum(Diesel)*100,
               Electric = Electric / sum(Electric)*100,
               LPG = LPG / sum(LPG)*100,
               Petrol = Petrol / sum(Petrol)*100,
               `Petrol Hybrid` = `Petrol Hybrid` / sum(`Petrol Hybrid`)*100)  %>%
        mutate(OrdreSegment = c("d","c","a","b")) %>% arrange(OrdreSegment) %>% select(-OrdreSegment)
    )
    REACT_Segment_parmotorisation[["ini"]][is.na(REACT_Segment_parmotorisation[["ini"]])] <- 0
    REACT_Segment_parmotorisation[["ini"]] <- REACT_Segment_parmotorisation[["ini"]][,c(1,tri_perso(names(REACT_Segment_parmotorisation[["ini"]])[-1],couleurs$Id)+1)]
    REACT_Segment_parmotorisation[["config"]] <- REACT_Segment_parmotorisation[["ini"]]
  })
  
  output$DT_Config_Segment_parmotorisation <- renderDT({
    
    if(REACT_Modifications_Config_Motorisation_parsegment() == 1 | 
       (REACT_Modifications_Config_Motorisation_parnorme() == 1 & REACT_Modifications_Config_Euro_parsegment() == 1) ){
      datatable(data.frame(Impossible = c("Combinaison déjà effectuée avec les paramètres fixés précédemment : motorisation par segments de puissance")),
                class = "hover cell-border compact",
                caption = "Segment par motorisation",
                options = list(dom = 't')
      )
    } else {
      
      df <- REACT_Segment_parmotorisation[["config"]] %>% 
                        mutate(Segment = as.character(Segment)) %>% mutate_at(vars(-c(Segment)), round,2) %>%
                        mutate(Segment = recode(Segment, "Small" = "Petit"))
      if(input$Input_Configurateur_perso_Motorisation == "global") {desactiv <- as.vector(c(1,which(REACT_Motorisation_globale[["config"]][,-1]==0)+1))}
      if(input$Input_Configurateur_perso_Motorisation == "parnorme") {desactiv <- as.vector(c(1,which(rowSums(REACT_Motorisation_parnorme[["config"]][,-1])==0)+1))}
      if(input$Input_Configurateur_perso_Motorisation == "parsegment") {desactiv <- as.vector(c(1,which(rowSums(REACT_Motorisation_parsegment[["config"]][,-1])==0)+1))}
      red_mark <- which(round(colSums(df[,-1]),2)<99.99 | round(colSums(df[,-1]),2)>100.01)+1
      javaFooter <- TableFooter(ncol(df),red_mark)
      datatable(df,
                editable = list(target = REACT_possibilite_editable_Segment_parmotorisation(),disable = list(columns = desactiv )),
                class = "hover cell-border compact",
                caption = "Segment par motorisation",
                container = htmltools::tags$table(tableHeader(c('','Segment de puissance', renames_all(names(df)[-1]))),tableFooter(rep("", ncol(df)+1))),
                selection = "none",
                options = list(dom = 't',ordering=F,
                               columnDefs = list(list(className = 'dt-right', targets = 2:7),
                                                 list(targets=c(0), visible=F),
                                                 list(targets=c(1:ncol(df)), visible=TRUE, width='100')),
                               footerCallback = javaFooter)) %>% 
        formatCurrency(2:ncol(df),"%",dec.mark = ",",before = FALSE) %>%
        formatStyle(0,target = "row",fontWeight = styleEqual(5, "bold"))  %>% 
        formatStyle(names(df %>% select(-Segment)),
                    background = styleColorBar(c(0,100), 'lightblue'),
                    backgroundSize = '98% 88%',
                    backgroundRepeat = 'no-repeat',
                    backgroundPosition = 'center') %>%
        formatStyle(1, backgroundColor = styleEqual(couleurs$Nom, couleurs$HEX_aire), fontWeight = "bold")
    }
  })
  
  observeEvent(input[["DT_Config_Segment_parmotorisation_cell_edit"]], {
    cell <- input[["DT_Config_Segment_parmotorisation_cell_edit"]]
    newdf <- REACT_Segment_parmotorisation[["config"]]
    newdf[cell$row, cell$col] <- as.numeric(cell$value)
    REACT_Segment_parmotorisation[["config"]] <- newdf
  })
  
  # -- Segment de puissance par norme Euro --
  REACT_Segment_parnorme <- reactiveValues(
    ini = data.frame(Parc_utilisateur[[1]] %>% 
                       group_by(Euro.Standard,Segment) %>% summarise(Part = sum(Part)) %>% 
                       pivot_wider(names_from = Euro.Standard, values_from = Part, values_fill = list(value = 0)) %>%
                       mutate(`Euro 1` = `Euro 1` / sum(`Euro 1`)*100,
                              `Euro 2` = `Euro 2` / sum(`Euro 2`)*100,
                              `Euro 3` = `Euro 3` / sum(`Euro 3`)*100,
                              `Euro 4` = `Euro 4` / sum(`Euro 4`)*100,
                              `Euro 5` = `Euro 5` / sum(`Euro 5`)*100,
                              `Euro 6b` = `Euro 6b` / sum(`Euro 6b`)*100,
                              `Euro 6c` = `Euro 6c` / sum(`Euro 6c`)*100,
                              `Euro 6d` = `Euro 6d` / sum(`Euro 6d`)*100) %>%
                       mutate(OrdreSegment = c("d","c","a","b")) %>% arrange(OrdreSegment) %>% select(-OrdreSegment)
    ),
    config = data.frame(Parc_utilisateur[[1]] %>% 
                          group_by(Euro.Standard,Segment) %>% summarise(Part = sum(Part)) %>% 
                          pivot_wider(names_from = Euro.Standard, values_from = Part, values_fill = list(value = 0)) %>%
                          mutate(`Euro 1` = `Euro 1` / sum(`Euro 1`)*100,
                                 `Euro 2` = `Euro 2` / sum(`Euro 2`)*100,
                                 `Euro 3` = `Euro 3` / sum(`Euro 3`)*100,
                                 `Euro 4` = `Euro 4` / sum(`Euro 4`)*100,
                                 `Euro 5` = `Euro 5` / sum(`Euro 5`)*100,
                                 `Euro 6b` = `Euro 6b` / sum(`Euro 6b`)*100,
                                 `Euro 6c` = `Euro 6c` / sum(`Euro 6c`)*100,
                                 `Euro 6d` = `Euro 6d` / sum(`Euro 6d`)*100) %>%
                          mutate(OrdreSegment = c("d","c","a","b")) %>% arrange(OrdreSegment) %>% select(-OrdreSegment)
    ))
  
  observeEvent(input$Input_Configurateur_perso_choixParc,{
    REACT_Segment_parnorme[["ini"]] <- data.frame(
      Parc_utilisateur[[input$Input_Configurateur_perso_choixParc]] %>% group_by(Euro.Standard,Segment) %>% summarise(Part = sum(Part)) %>% 
        pivot_wider(names_from = Euro.Standard, values_from = Part, values_fill = list(value = 0)) %>%
        mutate(`Euro 1` = `Euro 1` / sum(`Euro 1`)*100,
               `Euro 2` = `Euro 2` / sum(`Euro 2`)*100,
               `Euro 3` = `Euro 3` / sum(`Euro 3`)*100,
               `Euro 4` = `Euro 4` / sum(`Euro 4`)*100,
               `Euro 5` = `Euro 5` / sum(`Euro 5`)*100,
               `Euro 6b` = `Euro 6b` / sum(`Euro 6b`)*100,
               `Euro 6c` = `Euro 6c` / sum(`Euro 6c`)*100,
               `Euro 6d` = `Euro 6d` / sum(`Euro 6d`)*100) %>%
        mutate(OrdreSegment = c("d","c","a","b")) %>% arrange(OrdreSegment) %>% select(-OrdreSegment)
    )
    REACT_Segment_parnorme[["ini"]][is.na(REACT_Segment_parnorme[["ini"]])] <- 0
    REACT_Segment_parnorme[["config"]] <- REACT_Segment_parnorme[["ini"]]
  })
  
  output$DT_Config_Segment_parnorme <- renderDT({
    
    if(REACT_Modifications_Config_Euro_parsegment() == 1 | 
       (REACT_Modifications_Config_Motorisation_parsegment() == 1 & REACT_Modifications_Config_Euro_parmotorisation() == 1) ){
      datatable(data.frame(Impossible = c("Combinaison déjà effectuée avec les paramètres fixés précédemment : normes Euro par segments de puissance")),
                class = "hover cell-border compact",
                caption = "Segment par motorisation",
                options = list(dom = 't')
      )
    } else {
      
      df <- REACT_Segment_parnorme[["config"]] %>% 
                        mutate(Segment = as.character(Segment)) %>% mutate_at(vars(-c(Segment)), round,2) %>%
                        mutate(Segment = recode(Segment, "Small" = "Petit"))
      if(input$Input_Configurateur_perso_Euro == "global") {desactiv <- as.vector(c(1,which(REACT_Euro_globale[["config"]][,-1]==0)+1))}
      if(input$Input_Configurateur_perso_Euro == "parmotorisation") {desactiv <- as.vector(c(1,which(rowSums(REACT_Euro_parmotorisation[["config"]][,-1])==0)+1))}
      if(input$Input_Configurateur_perso_Euro == "parsegment") {desactiv <- as.vector(c(1,which(rowSums(REACT_Euro_parsegment[["config"]][,-1])==0)+1))}
      red_mark <- which(round(colSums(df[,-1]),2)<99.99 | round(colSums(df[,-1]),2)>100.01)+1
      javaFooter <- TableFooter(ncol(df),red_mark)
      datatable(df,
              editable = list(target = REACT_possibilite_editable_Segment_parnorme(),disable = list(columns = desactiv )),
              class = "hover cell-border compact",
              caption = "Segment de puissance par norme Euro",
              container = htmltools::tags$table(tableHeader(c('','Segment de puissance', renames_all(names(df)[-1]))),tableFooter(rep("", ncol(df)+1))),
              selection = "none",
              options = list(dom = 't',ordering=F,
                             columnDefs = list(list(className = 'dt-right', targets = 2:9),
                                               list(targets=c(0), visible=F),
                                               list(targets=c(1:ncol(df)), visible=TRUE, width='100')),
                             footerCallback = javaFooter)) %>% 
        formatCurrency(2:ncol(df),"%",dec.mark = ",",before = FALSE) %>% 
        formatStyle(0,target = "row",fontWeight = styleEqual(5, "bold"))  %>% 
        formatStyle(names(df %>% select(-Segment)),
                    background = styleColorBar(c(0,100), 'lightblue'),
                    backgroundSize = '98% 88%',
                    backgroundRepeat = 'no-repeat',
                    backgroundPosition = 'center') %>%
        formatStyle(1, backgroundColor = styleEqual(couleurs$Nom, couleurs$HEX_aire), fontWeight = "bold")
    }
  })
  
  observeEvent(input[["DT_Config_Segment_parnorme_cell_edit"]], {
    cell <- input[["DT_Config_Segment_parnorme_cell_edit"]]
    newdf <- REACT_Segment_parnorme[["config"]]
    newdf[cell$row, cell$col] <- as.numeric(cell$value)
    REACT_Segment_parnorme[["config"]] <- newdf
  })
  
  # ----  Info modif Config Segment ----
  # Indicateurs de modification des tables de segment
  REACT_Modifications_Config_Segment_global <- reactive({
    if( setequal(c(REACT_Segment_globale[["config"]]$Part),c(REACT_Segment_globale[["ini"]]$Part)) == FALSE) {1} else {0}
  })
  REACT_Modifications_Config_Segment_parmotorisation <- reactive({
    if( setequal(c(REACT_Segment_parmotorisation[["config"]]$CNG),c(REACT_Segment_parmotorisation[["ini"]]$CNG)) == FALSE |
        setequal(c(REACT_Segment_parmotorisation[["config"]]$Diesel),c(REACT_Segment_parmotorisation[["ini"]]$Diesel)) == FALSE |
        setequal(c(REACT_Segment_parmotorisation[["config"]]$Electric),c(REACT_Segment_parmotorisation[["ini"]]$Electric)) == FALSE |
        setequal(c(REACT_Segment_parmotorisation[["config"]]$LPG),c(REACT_Segment_parmotorisation[["ini"]]$LPG)) == FALSE |
        setequal(c(REACT_Segment_parmotorisation[["config"]]$Petrol),c(REACT_Segment_parmotorisation[["ini"]]$Petrol)) == FALSE |
        setequal(c(REACT_Segment_parmotorisation[["config"]]$Petrol.Hybrid),c(REACT_Segment_parmotorisation[["ini"]]$Petrol.Hybrid)) == FALSE ) {1} else {0}
  })
  REACT_Modifications_Config_Segment_parnorme <- reactive({
    if( setequal(c(REACT_Segment_parnorme[["config"]]$Euro.1),c(REACT_Segment_parnorme[["ini"]]$Euro.1)) == FALSE | 
        setequal(c(REACT_Segment_parnorme[["config"]]$Euro.2),c(REACT_Segment_parnorme[["ini"]]$Euro.2)) == FALSE |
        setequal(c(REACT_Segment_parnorme[["config"]]$Euro.3),c(REACT_Segment_parnorme[["ini"]]$Euro.3)) == FALSE |
        setequal(c(REACT_Segment_parnorme[["config"]]$Euro.4),c(REACT_Segment_parnorme[["ini"]]$Euro.4)) == FALSE |
        setequal(c(REACT_Segment_parnorme[["config"]]$Euro.5),c(REACT_Segment_parnorme[["ini"]]$Euro.5)) == FALSE |
        setequal(c(REACT_Segment_parnorme[["config"]]$Euro.6b),c(REACT_Segment_parnorme[["ini"]]$Euro.6b)) == FALSE |
        setequal(c(REACT_Segment_parnorme[["config"]]$Euro.6c),c(REACT_Segment_parnorme[["ini"]]$Euro.6c)) == FALSE |
        setequal(c(REACT_Segment_parnorme[["config"]]$Euro.6d),c(REACT_Segment_parnorme[["ini"]]$Euro.6d)) == FALSE )
    {1} else {0}
  })
  
  # Indication de modification d'une des tables de segment
  output$Modifs_Config_Segment <- renderText({
    if(REACT_Modifications_Config_Segment_global() == 1) {
      "Modifications des paramètres - Segment globale :" } else if (REACT_Modifications_Config_Segment_parmotorisation() == 1) {
        "Modifications des paramètres - Segment par motorisation :" } else if (REACT_Modifications_Config_Segment_parnorme() == 1) {
          "Modifications des paramètres - Segment par norme Euro :" } else {"Paramètres de Segment non modifiés"}
  })
  output$Validation_Config_Segment <- renderText({
    if(REACT_Modifications_Config_Segment_global() == 1) {
      "Modifications des paramètres - Segment globale :" } else if (REACT_Modifications_Config_Segment_parmotorisation() == 1) {
        "Modifications des paramètres - Segment par motorisation :" } else if (REACT_Modifications_Config_Segment_parnorme() == 1) {
          "Modifications des paramètres - Segment par norme Euro :" } else {"Paramètres de Segment non modifiés"}
  })
  
  # Possibilité d'éditions des 3 tables de segment
  REACT_possibilite_editable_Segment_globale <- reactive({
    if(REACT_Modifications_Config_Segment_parmotorisation() == 0 &
       REACT_Modifications_Config_Segment_parnorme() == 0) {
      "cell"} else {FALSE}
  })
  REACT_possibilite_editable_Segment_parmotorisation <- reactive({
    if(REACT_Modifications_Config_Segment_global() == 0 &
       REACT_Modifications_Config_Segment_parnorme() == 0) {
      "cell"} else {FALSE}
  })
  REACT_possibilite_editable_Segment_parnorme <- reactive({
    if(REACT_Modifications_Config_Segment_global() == 0 &
       REACT_Modifications_Config_Segment_parmotorisation() == 0) {
      "cell"} else {FALSE}
  })
  
  # Reset des 3 tables de segment
  observeEvent(input$button_reset_Config_Segment, {
    newdf_Segment <- REACT_Segment_globale[["ini"]]
    REACT_Segment_globale[["config"]] <- newdf_Segment
    
    newdf_segment_moto <- REACT_Segment_parmotorisation[["ini"]]
    REACT_Segment_parmotorisation[["config"]] <- newdf_segment_moto
    
    newdf_segment_euro <- REACT_Segment_parnorme[["ini"]]
    REACT_Segment_parnorme[["config"]] <- newdf_segment_euro
  })

  
  # ---- _Nouveau fichier Parc_utilisateur ----
  observeEvent(input$button_reset_Config, {

    # reset de la motorisation
    newdf_moto <- REACT_Motorisation_globale[["ini"]]
    REACT_Motorisation_globale[["config"]] <- newdf_moto
    
    newdf_moto_age <- REACT_Motorisation_parnorme[["ini"]]
    REACT_Motorisation_parnorme[["config"]] <- newdf_moto_age
    
    newdf_moto_segment <- REACT_Motorisation_parsegment[["ini"]]
    REACT_Motorisation_parsegment[["config"]] <- newdf_moto_segment
    
    # reset des normes
    newdf_euro <- REACT_Euro_globale[["ini"]]
    REACT_Euro_globale[["config"]] <- newdf_euro
    
    newdf_euro_moto <- REACT_Euro_parmotorisation[["ini"]]
    REACT_Euro_parmotorisation[["config"]] <- newdf_euro_moto
    
    newdf_euro_segment <- REACT_Euro_parsegment[["ini"]]
    REACT_Euro_parsegment[["config"]] <- newdf_euro_segment
    
    # reset des segments
    newdf_Segment <- REACT_Segment_globale[["ini"]]
    REACT_Segment_globale[["config"]] <- newdf_Segment
    
    newdf_segment_moto <- REACT_Segment_parmotorisation[["ini"]]
    REACT_Segment_parmotorisation[["config"]] <- newdf_segment_moto
    
    newdf_segment_euro <- REACT_Segment_parnorme[["ini"]]
    REACT_Segment_parnorme[["config"]] <- newdf_segment_euro
    
  })
  
  observeEvent(input$button_maj_Config, {
    temp_parc <- REACT_Parc_utilisateur() %>% select(Category, Fuel, Segment, Annee, Euro.Standard, Technology, Part) %>%
      mutate(Concat = paste(Fuel,Euro.Standard,Segment, sep = "_"), Compte = 1)
    Dataframe_Div <- aggregate( Compte ~ Concat, temp_parc, sum) %>% rename(Div = Compte)
    temp_parc <- temp_parc %>% left_join(Dataframe_Div, by = "Concat") %>% select(-Compte, -Concat)
    REACT_Parc_utilisateur(temp_parc)

    if(
      (REACT_Modifications_Config_Motorisation_global() + REACT_Modifications_Config_Motorisation_parnorme() + REACT_Modifications_Config_Motorisation_parsegment()) == 0 &
      (REACT_Modifications_Config_Euro_global() + REACT_Modifications_Config_Euro_parmotorisation() + REACT_Modifications_Config_Euro_parsegment()) == 0 &
      (REACT_Modifications_Config_Segment_global() + REACT_Modifications_Config_Segment_parmotorisation() + REACT_Modifications_Config_Segment_parnorme()) == 0
    ) {
      newdf_Parc <- REACT_Parc_utilisateur()
    }
    else if( # M1/rien - E1/rien - P1/rien
      (REACT_Modifications_Config_Motorisation_parnorme() + REACT_Modifications_Config_Motorisation_parsegment()) == 0 &
      (REACT_Modifications_Config_Euro_parmotorisation() + REACT_Modifications_Config_Euro_parsegment()) == 0 &
      (REACT_Modifications_Config_Segment_parmotorisation() + REACT_Modifications_Config_Segment_parnorme()) == 0
    ) {
      newdf_Parc <- REACT_Parc_utilisateur() %>% select(-Part) %>%
        left_join(REACT_Motorisation_globale[["config"]], by = "Fuel") %>% rename(PartMotorisation = Part) %>%
        left_join(REACT_Euro_globale[["config"]], by = "Euro.Standard") %>% rename(PartEuro = Part) %>%
        left_join(REACT_Segment_globale[["config"]], by = "Segment") %>% rename(PartSegment = Part) %>%
        mutate(Part = PartMotorisation * PartEuro * PartSegment / Div / 1000000) %>% select(-Div,-PartMotorisation,-PartEuro,-PartSegment)
    } 
    else if( # M1/rien - E1/rien - P2
      (REACT_Modifications_Config_Motorisation_parnorme() + REACT_Modifications_Config_Motorisation_parsegment()) == 0 &
      (REACT_Modifications_Config_Euro_parmotorisation() + REACT_Modifications_Config_Euro_parsegment()) == 0 &
      REACT_Modifications_Config_Segment_parmotorisation() == 1
    ) {
      newdf_Parc <- REACT_Parc_utilisateur() %>% select(-Part) %>%
        left_join(REACT_Motorisation_globale[["config"]], by = "Fuel") %>% rename(PartMotorisation = Part) %>%
        left_join(REACT_Euro_globale[["config"]], by = "Euro.Standard") %>% rename(PartEuro = Part) %>%
        left_join(REACT_Segment_parmotorisation[["config"]] %>%
                    rename(`Petrol Hybrid` = Petrol.Hybrid) %>% 
                    pivot_longer(cols = !Segment, names_to = "Fuel", values_to = "PartSegmentMotorisation")
                  , by = c("Segment","Fuel")) %>%
        mutate(Part = PartMotorisation * PartEuro * PartSegmentMotorisation / Div / 1000000)
    }
    else if( # M1/rien - E1/rien - P3
      (REACT_Modifications_Config_Motorisation_parnorme() + REACT_Modifications_Config_Motorisation_parsegment()) == 0 &
      (REACT_Modifications_Config_Euro_parmotorisation() + REACT_Modifications_Config_Euro_parsegment()) == 0 &
      REACT_Modifications_Config_Segment_parnorme() == 1
    ) {
      newdf_Parc <- REACT_Parc_utilisateur() %>% select(-Part) %>%
        left_join(REACT_Motorisation_globale[["config"]], by = "Fuel") %>% rename(PartMotorisation = Part) %>%
        left_join(REACT_Euro_globale[["config"]], by = "Euro.Standard") %>% rename(PartEuro = Part) %>%
        left_join(REACT_Segment_parnorme[["config"]] %>%
                    rename(`Euro 1` = Euro.1,`Euro 2` = Euro.2,`Euro 3` = Euro.3,`Euro 4` = Euro.4,`Euro 5` = Euro.5,`Euro 6b` = Euro.6b,`Euro 6c` = Euro.6c,`Euro 6d` = Euro.6d) %>% 
                    pivot_longer(cols = starts_with("Euro"), names_to = "Euro.Standard", values_to = "PartSegmentNorme")
                  , by = c("Segment","Euro.Standard")) %>%
        mutate(Part = PartMotorisation * PartEuro * PartSegmentNorme / Div / 1000000)
    } 
    else if( # M1/rien - E2 - P1/rien
      (REACT_Modifications_Config_Motorisation_parnorme() + REACT_Modifications_Config_Motorisation_parsegment()) == 0 &
      REACT_Modifications_Config_Euro_parmotorisation() == 1 &
      (REACT_Modifications_Config_Segment_parmotorisation() + REACT_Modifications_Config_Segment_parnorme()) == 0
    ) {
      print(REACT_Motorisation_globale[["config"]])
      print(REACT_Euro_parmotorisation[["config"]])
      print(REACT_Segment_globale[["config"]])
      REACT_Euro_parmotorisation[["config"]][is.na(REACT_Euro_parmotorisation[["config"]])] <- 0
      print(REACT_Euro_parmotorisation[["config"]])
      print(head(REACT_Parc_utilisateur()))
      newdf_Parc <- REACT_Parc_utilisateur() %>% select(-Part) %>%
        left_join(REACT_Motorisation_globale[["config"]], by = "Fuel") %>% rename(PartMotorisation = Part) %>%
        left_join(REACT_Euro_parmotorisation[["config"]] %>%
                    rename(`Petrol Hybrid` = Petrol.Hybrid) %>%
                    pivot_longer(cols = !Euro.Standard, names_to = "Fuel", values_to = "PartNormeMotorisation") 
                    , by = c("Euro.Standard","Fuel")) %>%
        left_join(REACT_Segment_globale[["config"]], by = "Segment") %>% rename(PartSegment = Part) %>%
        mutate(Part = PartMotorisation * PartNormeMotorisation * PartSegment / Div / 1000000)
    }
    else if( # M1/rien - E2 - P2
      (REACT_Modifications_Config_Motorisation_parnorme() + REACT_Modifications_Config_Motorisation_parsegment()) == 0 &
      REACT_Modifications_Config_Euro_parmotorisation() == 1 &
      REACT_Modifications_Config_Segment_parmotorisation() == 1
    ) {
      newdf_Parc <- REACT_Parc_utilisateur() %>% select(-Part) %>%
        left_join(REACT_Motorisation_globale[["config"]], by = "Fuel") %>% rename(PartMotorisation = Part) %>%
        left_join(REACT_Euro_parmotorisation[["config"]] %>%
                    rename(`Petrol Hybrid` = Petrol.Hybrid) %>%
                    pivot_longer(cols = !Euro.Standard, names_to = "Fuel", values_to = "PartNormeMotorisation"), by = c("Euro.Standard","Fuel")) %>%
        left_join(REACT_Segment_parmotorisation[["config"]] %>%
                    rename(`Petrol Hybrid` = Petrol.Hybrid) %>% 
                    pivot_longer(cols = !Segment, names_to = "Fuel", values_to = "PartSegmentMotorisation"), by = c("Segment","Fuel")) %>%
        mutate(Part = PartMotorisation * PartNormeMotorisation * PartSegmentMotorisation / Div / 1000000)
    }
    else if( # M2 - E1/rien - P1/rien
      REACT_Modifications_Config_Motorisation_parnorme() == 1 &
      (REACT_Modifications_Config_Euro_parmotorisation() + REACT_Modifications_Config_Euro_parsegment()) == 0 &
      (REACT_Modifications_Config_Segment_parmotorisation() + REACT_Modifications_Config_Segment_parnorme()) == 0
    ) {
      newdf_Parc <- REACT_Parc_utilisateur() %>% select(-Part) %>%
        left_join(REACT_Motorisation_parnorme[["config"]] %>%
                    rename(`Euro 1` = Euro.1,`Euro 2` = Euro.2,`Euro 3` = Euro.3,`Euro 4` = Euro.4,`Euro 5` = Euro.5,`Euro 6b` = Euro.6b,`Euro 6c` = Euro.6c,`Euro 6d` = Euro.6d) %>% 
                    pivot_longer(cols = starts_with("Euro"), names_to = "Euro.Standard", values_to = "PartMotorisationNorme"), by = c("Fuel","Euro.Standard")) %>%
        left_join(REACT_Euro_globale[["config"]], by = "Euro.Standard") %>% rename(PartEuro = Part) %>%
        left_join(REACT_Segment_globale[["config"]], by = "Segment") %>% rename(PartSegment = Part) %>%
        mutate(Part = PartMotorisationNorme * PartEuro * PartSegment / Div / 1000000)
    } 
    else if( # M2 - E1/rien - P3
      REACT_Modifications_Config_Motorisation_parnorme() == 1 &
      (REACT_Modifications_Config_Euro_parmotorisation() + REACT_Modifications_Config_Euro_parsegment()) == 0 &
      REACT_Modifications_Config_Segment_parnorme() == 1
    ) {
      newdf_Parc <- REACT_Parc_utilisateur() %>% select(-Part) %>%
        left_join(REACT_Motorisation_parnorme[["config"]] %>%
                    rename(`Euro 1` = Euro.1,`Euro 2` = Euro.2,`Euro 3` = Euro.3,`Euro 4` = Euro.4,`Euro 5` = Euro.5,`Euro 6b` = Euro.6b,`Euro 6c` = Euro.6c,`Euro 6d` = Euro.6d) %>% 
                    pivot_longer(cols = starts_with("Euro"), names_to = "Euro.Standard", values_to = "PartMotorisationNorme"), by = c("Fuel","Euro.Standard")) %>%
        left_join(REACT_Euro_globale[["config"]], by = "Euro.Standard") %>% rename(PartEuro = Part) %>%
        left_join(REACT_Segment_parnorme[["config"]] %>%
                    pivot_longer(cols = !Segment, names_to = "Euro.Standard", values_to = "PartSegmentNorme"), by = c("Segment","Euro.Standard")) %>%
        mutate(Part = PartMotorisationNorme * PartEuro * PartSegmentNorme / Div / 1000000)
    }
    else if( # M2 - E3 - P1/rien
      REACT_Modifications_Config_Motorisation_parnorme() == 1 &
      REACT_Modifications_Config_Euro_parsegment() == 1 &
      (REACT_Modifications_Config_Segment_parmotorisation() + REACT_Modifications_Config_Segment_parnorme()) == 0
    ) {
      newdf_Parc <- REACT_Parc_utilisateur() %>% select(-Part) %>%
        left_join(REACT_Motorisation_parnorme[["config"]] %>%
                    rename(`Euro 1` = Euro.1,`Euro 2` = Euro.2,`Euro 3` = Euro.3,`Euro 4` = Euro.4,`Euro 5` = Euro.5,`Euro 6b` = Euro.6b,`Euro 6c` = Euro.6c,`Euro 6d` = Euro.6d) %>% 
                    pivot_longer(cols = starts_with("Euro"), names_to = "Euro.Standard", values_to = "PartMotorisationNorme"), by = c("Fuel","Euro.Standard")) %>%
        left_join(REACT_Euro_parsegment[["config"]] %>%
                    pivot_longer(cols = !Euro.Standard, names_to = "Segment", values_to = "PartNormeSegment"), by = c("Euro.Standard","Segment")) %>%
        left_join(REACT_Segment_globale[["config"]], by = "Segment") %>% rename(PartSegment = Part) %>%
        mutate(Part = PartMotorisationNorme * PartNormeSegment * PartSegment / Div / 1000000)
    }
    else {newdf_Parc <- REACT_Parc_utilisateur()}
    
    REACT_Parc_utilisateur(newdf_Parc)
    
  })

  output$Visu_REACT_Parc_utilisateur <- renderDT({
      datatable(REACT_Parc_utilisateur() %>% select(-Category,-Technology,-RedNOxEuro6,-M_carros,-M_moteur_ICE,-M_moteur_Elec,-M_batterie,-DV_veh,-DV_batterie),
                class = "hover cell-border compact")
  })
  output$Verif_REACT_Parc_utilisateur <- renderText({
    sum(REACT_Parc_utilisateur()$Part)
  })

  # ---- Synthèse du configurateur du parc ----
    output$GRAPH_Synthese_Motorisation_ly <- renderPlotly({
    
    DataGraph <- REACT_Parc_utilisateur() %>% group_by(Fuel) %>% summarise(Part = round(sum(Part)*100, 2))
    DataGraph$Fuel <- fct_recode(DataGraph$Fuel,
                                 "Electrique" = "Electric",
                                 "Essence" = "Petrol",
                                 "Hybride" = "Petrol Hybrid",
                                 "GPL" = "LPG",
                                 "Gaz Naturel" = "CNG")
    DataGraph$couleurs <- couleurs$HEX_trait[match(DataGraph$Fuel,couleurs$Nom)]
    DataGraph <- DataGraph[DataGraph$Part !=0,]
    DataGraph <- DataGraph[tri_perso(DataGraph$Fuel,couleurs$Nom),]
    plot_ly(DataGraph, labels = ~Fuel, values = ~Part, type = "pie",
            marker = list(colors = ~couleurs),
            textposition = "inside", 
            textinfo = 'label+percent',
            insidetextfont = list(color = '#FFFFFF'),
            hoverinfo = 'text',
            text = ~paste("<b>",Fuel,":</b> <br>",Part, "% du parc"), 
            sort = FALSE,direction = "clockwise"
    ) %>%
      layout(title = "Motorisation",
             xaxis = list(title = "Motorisation"),
             yaxis = list(title = 'Part (%)',
                          range = c(0,100))) %>% 
      layout(showlegend = FALSE)

  })
    output$GRAPH_Synthese_Age_ly <- renderPlotly({

      DataGraph <- REACT_Parc_utilisateur() %>% group_by(Euro.Standard) %>% summarise(Part = round(sum(Part)*100, 2))  %>% filter(Part != 0)
      DataGraph <- DataGraph[apply(DataGraph[,-1], 1, function(x) !all(x==0)),]
      DataGraph <- droplevels(DataGraph)
      
      plot_ly(DataGraph, x = ~Euro.Standard, y = ~Part, type = "bar",
              textposition = "inside",
              text = ~paste(round(Part,1), "%"),
              insidetextfont = list(color = '#FFFFFF'),
              hoverinfo = 'text',
              hovertext = ~paste("<b>",Euro.Standard,":</b> <br>",round(Part,1), "% du parc"),
              marker = list(color = couleurs$HEX_trait[match(DataGraph$Euro.Standard, couleurs$Nom)])
      ) %>%
        layout(title = "Normes Euro",
               xaxis = list(title = ""),
               yaxis = list(title = 'Part (%)'
                            )
               ) %>% 
        layout(showlegend = FALSE)
    })
    
    
    output$Visu_REACT_AgeMoyen_Parc_utilisateur <- renderText({

      DataGraph <- REACT_Parc_utilisateur() %>% group_by(Euro.Standard) %>% summarise(Part = round(sum(Part)*100, 2))

      AgeMoyen <- round((DataGraph[DataGraph$Euro.Standard == "Euro 1", "Part"] * 1994.5 +
                           DataGraph[DataGraph$Euro.Standard == "Euro 2", "Part"] * 1998.5 +
                           DataGraph[DataGraph$Euro.Standard == "Euro 3", "Part"] * 2003 +
                           DataGraph[DataGraph$Euro.Standard == "Euro 4", "Part"] * 2008 +
                           DataGraph[DataGraph$Euro.Standard == "Euro 5", "Part"] * 2012.5 +
                           DataGraph[DataGraph$Euro.Standard == "Euro 6b", "Part"] * 2016 +
                           DataGraph[DataGraph$Euro.Standard == "Euro 6c", "Part"] * 2019 +
                           DataGraph[DataGraph$Euro.Standard == "Euro 6d", "Part"] * 2021) / 100,1)
      
      paste0("Année moyenne d'immatriculation : ", AgeMoyen)
      
    })
     
    output$GRAPH_Synthese_Puissance_ly <- renderPlotly({

      DataGraph <- REACT_Parc_utilisateur() %>% group_by(Segment) %>% summarise(Part = round(sum(Part)*100, 2)) %>%
        mutate(Segment = as.factor(Segment))
      DataGraph$Segment <- fct_recode(DataGraph$Segment,"Petit" = "Small")
      DataGraph$Segment <- fct_relevel(DataGraph$Segment,"Mini","Petit","Medium","Large")
      DataGraph <- DataGraph[tri_perso(DataGraph$Segment,couleurs$Nom),]
      DataGraph$couleurs <- couleurs$HEX_trait[match(DataGraph$Segment,couleurs$Nom)]
      plot_ly(DataGraph, labels = ~Segment, values = ~Part, type = "pie",
              marker = list(colors = ~couleurs),
              textposition = "inside", 
              textinfo = 'label+percent',
              insidetextfont = list(color = '#000000'),
              hoverinfo = 'text',
              text = ~paste("<b>",Segment,"</b>: <br>",Part, "% du parc"),
              sort = FALSE,direction = "clockwise"
      ) %>%
        layout(title = "Segments de puissance",
               xaxis = list(title = ""),
               yaxis = list(title = 'Part (%)'
               )
        ) %>% 
        layout(showlegend = FALSE)
    })
    
    output$Visu_REACT_PuissanceMoyenne_Parc_utilisateur <- renderText({
      
      DataGraph <- REACT_Parc_utilisateur() %>% group_by(Segment) %>% summarise(Part = round(sum(Part), 2)) %>%
        mutate(Segment = as.factor(Segment))
      DataGraph$Segment <- fct_recode(DataGraph$Segment,"Petit" = "Small")
      DataGraph$Segment <- fct_relevel(DataGraph$Segment,"Mini","Petit","Medium","Large")
      
       
      PuissanceMoyenne <- round((DataGraph[DataGraph$Segment == "Large", "Part"] * 10.5 +
        DataGraph[DataGraph$Segment == "Medium", "Part"] * 8 +
        DataGraph[DataGraph$Segment == "Petit", "Part"] * 5 +
        DataGraph[DataGraph$Segment == "Mini", "Part"] * 2), 1)
      

      paste0("Puissance moyenne : ", PuissanceMoyenne, " CV")
      
    })
    
    output$GRAPH_Synthese_DeuxVariables <- renderPlotly({

      if(input$Input_Config_Synthese_Abs == "Fuel" & input$Input_Config_Synthese_Fill_Fuel == "Euro.Standard"){
        titre <- "Motorisation en fonction des normes Euro"
        x_axis <- "Norme Euro"
        
        Data <- REACT_Parc_utilisateur() %>% group_by(Fuel,Euro.Standard) %>%
        rename(Categories = Fuel, X = Euro.Standard) %>% summarise(Part = sum(Part))
        Data$Categories <- renames_all(Data$Categories)
        Data$X <- renames_all(Data$X)
        
      }
      else if(input$Input_Config_Synthese_Abs == "Fuel" & input$Input_Config_Synthese_Fill_Fuel == "Segment"){
        titre <- "Motorisation en fonction des segments de puissance"
        x_axis <- "Segment de puissance"
        
        Data <- REACT_Parc_utilisateur() %>% group_by(Fuel,Segment) %>%
          rename(Categories = Fuel, X = Segment) %>% summarise(Part = sum(Part))
        Data$Categories <- renames_all(Data$Categories)
        Data$X <- renames_all(Data$X)
        
      }
      else if(input$Input_Config_Synthese_Abs == "Euro.Standard" & input$Input_Config_Synthese_Fill_Euro == "Fuel"){
        titre <- "Normes Euro en fonction des motorisations"
        x_axis <- "Motorisation"
        
        Data <- REACT_Parc_utilisateur() %>% group_by(Euro.Standard,Fuel) %>%
          rename(Categories = Euro.Standard, X = Fuel) %>% summarise(Part = sum(Part))
        Data$Categories <- renames_all(Data$Categories)
        Data$X <- renames_all(Data$X)
        
      }
      else if(input$Input_Config_Synthese_Abs == "Euro.Standard" & input$Input_Config_Synthese_Fill_Euro == "Segment"){
        titre <- "Normes Euro en fonction des segments de puissance"
        x_axis <- "Segment de puissance"
        
        Data <- REACT_Parc_utilisateur() %>% group_by(Euro.Standard,Segment) %>%
          rename(Categories = Euro.Standard, X = Segment) %>% summarise(Part = sum(Part))
        Data$Categories <- renames_all(Data$Categories)
        Data$X <- renames_all(Data$X)
       
      }
      else if(input$Input_Config_Synthese_Abs == "Segment" & input$Input_Config_Synthese_Fill_Segment == "Fuel"){
        titre <- "Segments de puissance en fonction des motorisations"
        x_axis <- "Motorisation"
        
        Data <- REACT_Parc_utilisateur() %>% group_by(Segment,Fuel) %>%
          rename(Categories = Segment, X = Fuel) %>% summarise(Part = sum(Part))
        Data$Categories <- renames_all(Data$Categories)
        Data$X <- renames_all(Data$X)
        
      }
      else if(input$Input_Config_Synthese_Abs == "Segment" & input$Input_Config_Synthese_Fill_Segment == "Euro.Standard"){
        titre <- "Segments de puissance en fonction des normes Euro"
        x_axis <- "Norme Euro"
        
        Data <- REACT_Parc_utilisateur() %>% group_by(Segment,Euro.Standard) %>%
          rename(Categories = Segment, X = Euro.Standard) %>% summarise(Part = sum(Part))
        Data$Categories <- renames_all(Data$Categories)
        Data$X <- renames_all(Data$X)
       
      }
      
      DataGraph <- data.frame(matrix(Data$Part, ncol = length(table(Data$Categories)), byrow = FALSE))
      names(DataGraph) <- Data$Categories[!duplicated(Data$Categories)]
      rownames(DataGraph) <- Data$X[!duplicated(Data$X)]
      DataGraph <- DataGraph[which(rowSums(DataGraph)!=0),which(colSums(DataGraph)!=0)]
      DataGraph <- DataGraph[tri_perso(rownames(DataGraph), couleurs$Nom),tri_perso(names(DataGraph), couleurs$Nom)]
      DataGraph <- 100*DataGraph/rowSums(DataGraph)
      couleurs <- couleurs$HEX_trait[match(names(DataGraph),couleurs$Nom)]
      
      fig <- plot_ly(x = rownames(DataGraph), y = DataGraph[,1], type = "bar",
                     name = names(DataGraph)[1], marker = list(color = couleurs[1]),
                     hoverinfo = 'text',
                     hovertext = ~paste("<b>",x_axis,":</b>",rownames(DataGraph),"<br>",round(DataGraph[,1],1), "% du parc<br><i><b>",names(DataGraph)[1],"</i></b>")
      )
      for(i in 2:ncol(DataGraph)) {
        Y <- DataGraph[,i]
        nom <- names(DataGraph)[i]
        hov <- paste("<b>",x_axis,":</b>",rownames(DataGraph),"<br>",round(Y,1), "% du parc<br><i><b>",nom,"</i></b>")
        fig <- fig %>% add_trace(y = Y,
                                 name = nom, marker = list(color = couleurs[i]),
                                 hoverinfo = 'text',
                                 hovertext = hov
                                 )
        next
      }
      
      
      fig <- fig %>% layout(
        barmode = 'stack',
        title = titre,
        xaxis = list(title = x_axis,categoryorder = "array",categoryarray = rownames(DataGraph)),
        yaxis = list(title = "Part du parc",ticksuffix = "%",  range = c(0, 100))
      )
    }) 
    
    
  # ---- Téléchargement du Parc ----
    output$download_Parc <- downloadHandler(
     filename = function() {
        "Parc.xlsx" },
      content = function(file) {
        
        write_xlsx(
          
          if("Div" %in% names(REACT_Parc_utilisateur())) {
            REACT_Parc_utilisateur() %>% select(-Div)
          } else {REACT_Parc_utilisateur()}
          
          ,
          
          path = file)
        }
    )

    # ---- _Carburants ----
    # Biocarburants ----
    Carburants_initiale <- data.frame(
      BioFuel %>% mutate(
        BioFuel = rownames(BioFuel)) %>% select(BioFuel,ratio) %>% mutate(ratio = ratio * 100)
    )

    REACT_Config_Carburants <- reactiveVal(Carburants_initiale)
    
    output$DT_Carburants <- renderDT({
      
      temp <- REACT_Config_Carburants() 
      
      temp$BioFuel <- fct_recode(temp$BioFuel, "Bio Ethanol" = "Bioethanol",
                                               "ETBE" = "ETBE",
                                               "Bio Diesel" = "Biodiesel",
                                               "Bio Gaz" = "BioCNG")
        datatable(temp, colnames = c('Agro-carburants',"Pourcentage d'enrichissement (%volumique)"),
                editable = list(target = "cell",disable = list(columns = 1 )),
                class = "hover cell-border compact",
                caption = "Composition du Bio Carburant",
                selection = "none",
                options = list(dom = 't',ordering=F,
                               columnDefs = list(list(targets=c(0), visible=F),
                                                 list(targets=c(1), visible=TRUE, width='150')))) %>%
          formatCurrency(2,"%",dec.mark = ",",before = FALSE) %>%
        formatStyle(1, backgroundColor = styleEqual(couleurs$Nom, couleurs$HEX_aire), fontWeight = "bold")
    })
    
    observeEvent(input[["DT_Carburants_cell_edit"]], {
      cell <- input[["DT_Carburants_cell_edit"]]
      newdf <- REACT_Config_Carburants()
      newdf[cell$row, cell$col] <- as.numeric(cell$value)
      REACT_Config_Carburants(newdf)
    })
    
    REACT_Modifications_Config_Carburants <- reactive({
      if( setequal(c(REACT_Config_Carburants()$ratio),c(Carburants_initiale$ratio)) == FALSE)
      {1} else {0}
    })

    observeEvent(input$button_reset_Config_Carburants, {
      
      newdf_carburant <- Carburants_initiale
      REACT_Config_Carburants(newdf_carburant)
      REACT_Config_Carburants_2(newdf_carburant)                  
      
    })
    
    REACT_Config_Carburants_2 <- reactiveVal(Carburants_initiale)
    
    observeEvent(input$button_maj_Config_Carburants, {
      
      newdf_carburants <- Carburants_initiale

      if( REACT_Modifications_Config_Carburants() != 0 ) {
        newdf_carburants <- REACT_Config_Carburants()
      }
      
      REACT_Config_Carburants_2(newdf_carburants)

    })
    
    output$GRAPH_Synthese_Carburants <- renderPlotly({
      
      DataGraph <- data.frame(Essence = rep(0,5),Diesel = rep(0,5), Gaz.Naturel = rep(0,5), row.names = c("Fossile","Bio Ethanol","ETBE","Bio Diesel","Bio Gaz"))
      names(DataGraph) <- c("Essence","Diesel","Gaz Naturel")
      DataGraph["Bio Ethanol","Essence"] <- REACT_Config_Carburants_2()$ratio[which(REACT_Config_Carburants_2()$BioFuel == "Bioethanol")]
      DataGraph["ETBE","Essence"] <- REACT_Config_Carburants_2()$ratio[which(REACT_Config_Carburants_2()$BioFuel == "ETBE")]
      DataGraph["Bio Diesel","Diesel"] <- REACT_Config_Carburants_2()$ratio[which(REACT_Config_Carburants_2()$BioFuel == "Biodiesel")]
      DataGraph["Bio Gaz","Gaz Naturel"] <- REACT_Config_Carburants_2()$ratio[which(REACT_Config_Carburants_2()$BioFuel == "BioCNG")]
      DataGraph["Fossile",] <- 100-colSums(DataGraph[-1,])
      
      DataGraph <- t(DataGraph)
      fig <- plot_ly(x=rownames(DataGraph),y=DataGraph[,1],type ="bar",
                     name = colnames(DataGraph)[1],marker = list(color = "#595959"),
                     hoverinfo = 'text',
                     hovertext = paste0("<b>",rownames(DataGraph)," - ",colnames(DataGraph)[1],"</b><br> Part :",DataGraph[,1]," %")
                     )
        fig <- fig %>% add_trace(y=DataGraph[,2],
                                 name = colnames(DataGraph)[2],marker = list(color = "#00a274"),
                                 hoverinfo = 'text',
                                 hovertext = paste0("<b>",rownames(DataGraph)," - ",colnames(DataGraph)[2],"</b><br> Part :",DataGraph[,2]," %")
                                 )
        fig <- fig %>% add_trace(y=DataGraph[,3],
                                 name = colnames(DataGraph)[3],marker = list(color = "#29ffc2"),
                                 hoverinfo = 'text',
                                 hovertext = paste0("<b>",rownames(DataGraph)," - ",colnames(DataGraph)[3],"</b><br> Part :",DataGraph[,3]," %")
        )
        fig <- fig %>% add_trace(y=DataGraph[,4],
                                 name = colnames(DataGraph)[4],marker = list(color = "#e8b27c"),
                                 hoverinfo = 'text',
                                 hovertext = paste0("<b>",rownames(DataGraph)," - ",colnames(DataGraph)[4],"</b><br> Part :",DataGraph[,4]," %")
        )
        fig <- fig %>% add_trace(y=DataGraph[,5],
                                 name = colnames(DataGraph)[5],marker = list(color = "#f79b9b"),
                                 hoverinfo = 'text',
                                 hovertext = paste0("<b>",rownames(DataGraph)," - ",colnames(DataGraph)[5],"</b><br> Part :",DataGraph[,5]," %")
        )
        fig <- fig %>% layout(
                              barmode = 'stack',
                              title = "Incorporation des agro-carburants",
                              xaxis = list(title = 'Carburants à la pompe',categoryorder = "array",categoryarray = rownames(DataGraph)),
                              yaxis = list(title = "Ratio (% vol)",ticksuffix = "%",  range = c(0, 100))
                              )
      
      fig
      
    })
    
    output$Visu_SurconsoDiesel <- renderText({
      
      SurconsoDiesel <- 35.8638 / (REACT_Config_Carburants_2()[REACT_Config_Carburants_2()$BioFuel == "Biodiesel",2]/100 * 33.197 + 
                                     (1 - REACT_Config_Carburants_2()[REACT_Config_Carburants_2()$BioFuel == "Biodiesel",2]/100) * 35.8638) -1
        
      paste0("Surconsommation diesel (%vol) : ", abs(round(SurconsoDiesel*100,1)) , " %")
      
    })
    output$Visu_SurconsoEssence <- renderText({
      
      SurconsoEssence <- 32.8305 / (REACT_Config_Carburants_2()[REACT_Config_Carburants_2()$BioFuel == "Bioethanol",2]/100 * 22.8672 + 
                                     REACT_Config_Carburants_2()[REACT_Config_Carburants_2()$BioFuel == "ETBE",2]/100 * 26.6432 + 
                                     (1 - REACT_Config_Carburants_2()[REACT_Config_Carburants_2()$BioFuel == "ETBE",2]/100 - REACT_Config_Carburants_2()[REACT_Config_Carburants_2()$BioFuel == "Bioethanol",2]/100) * 32.8305) - 1
      
      paste0("Surconsommation essence (%vol) : ", abs(round(SurconsoEssence*100,1)) , " %")
      
    })
    output$Visu_TeneurOxygene <- renderText({
      
      TeneurOxygene <- (REACT_Config_Carburants_2()[REACT_Config_Carburants_2()$BioFuel == "Bioethanol",2]/100 * 276.1739 + 
                          REACT_Config_Carburants_2()[REACT_Config_Carburants_2()$BioFuel == "ETBE",2]/100 * 115.6683) / 
        ( (1 - REACT_Config_Carburants_2()[REACT_Config_Carburants_2()$BioFuel == "Bioethanol",2]/100 - REACT_Config_Carburants_2()[REACT_Config_Carburants_2()$BioFuel == "ETBE",2]/100) * 750)

      paste0("Teneur en oxygène %(m/m) : ", abs(round(TeneurOxygene*100,1)) , " %")
      
    })
    output$Visu_EthanolTotal <- renderText({
      
      EthanolTotal <- REACT_Config_Carburants_2()[REACT_Config_Carburants_2()$BioFuel == "Bioethanol",2]/100 + 
        REACT_Config_Carburants_2()[REACT_Config_Carburants_2()$BioFuel == "ETBE",2]/100 * 0.47
      
      paste0("% Ethanol total (%vol) : ", abs(round(EthanolTotal*100,1)) , " %")
      
    })

    # Mix Electrique ----
    MixElect_initiale <- data.frame(
      Mix_elec %>% mutate(
        Energie = rownames(Mix_elec),
        Part = ratio*100) %>% select(-ratio)
    )
    MixElect_initiale <- MixElect_initiale[c(0:9),]
    
    REACT_Config_MixElect <- reactiveVal(MixElect_initiale)

    output$DT_MixElect <- renderDT({
      df <- REACT_Config_MixElect() %>% mutate(Part = round(Part,2))
      df[,1] <- renames_all(df[,1])
      red_mark <- which(round(sum(df[,-1]),2)<99.99 | round(sum(df[,-1]),2)>100.01)+1
      javaFooter <- TableFooter(ncol(df),red_mark)
      datatable(df,
                
                editable = list(target = "cell",disable = list(columns = 1 )),
                class = "hover cell-border compact",
                caption = "Mix électrique",
                selection = "none",
                container = htmltools::tags$table(tableHeader(c('','Energies', "Part (%)")),tableFooter(rep("", 3))),
                options = list(dom = 't',ordering=F,
                               columnDefs = list(list(className = 'dt-right', targets = 2),
                                                 list(targets=c(0), visible=F),
                                                 list(targets=c(1:2), visible=TRUE, width='150')),
                               footerCallback = javaFooter)) %>%
        formatCurrency(2,"%",dec.mark = ",",before = FALSE) %>% 
        formatStyle(0, target = "row", fontWeight = styleEqual(10,"bold"))  %>% 
        formatStyle("Part",
                    background = styleColorBar(c(0,100), 'lightblue'),
                    backgroundSize = '98% 88%',
                    backgroundRepeat = 'no-repeat',
                    backgroundPosition = 'center') %>%
        formatStyle(1, backgroundColor = styleEqual(couleurs$Nom, couleurs$HEX_aire), fontWeight = "bold")
      
    })
    
    observeEvent(input[["DT_MixElect_cell_edit"]], {
      cell <- input[["DT_MixElect_cell_edit"]]
      newdf <- REACT_Config_MixElect()
      newdf[cell$row, cell$col] <- as.numeric(cell$value)
      REACT_Config_MixElect(newdf)
    })
    
    output$Verif_Somme_MixElect <- renderText({
      if(sum(REACT_Config_MixElect()$Part) > 100.01 | sum(REACT_Config_MixElect()$Part) < 99.99) {paste0("Attention, somme différente de 100 % :"," ", round(sum(REACT_Config_MixElect()$Part),2), " %")}
    })
    
    REACT_Modifications_Config_MixElect <- reactive({
      if( setequal(c(REACT_Config_MixElect()$Part),c(MixElect_initiale$Part)) == FALSE)
      {1} else {0}
    })
    
    # Perte énergie ----
    PerteEnergie_initiale <- data.frame(
      Mix_elec %>% mutate(
        Tension = rownames(Mix_elec),
        Perte = ratio*100) %>% select(-ratio)
    )
    PerteEnergie_initiale <- PerteEnergie_initiale[c(10:12),]
    
    REACT_Config_PerteEnergie <- reactiveVal(PerteEnergie_initiale)
    
    output$DT_PerteEnergie <- renderDT({
      temp <- REACT_Config_PerteEnergie()
      temp$Tension <- fct_recode(temp$Tension, "Basse tension" = "Perte low",
                                                "Moyenne tension" = "Perte medium",
                                                "Haute tension" = "Perte high")
      datatable(temp %>% mutate(Perte = round(Perte,2)),
                colnames = c('Tension Electrique','Perte (%)'),
                editable = list(target = "cell",disable = list(columns = 1 )),
                class = "hover cell-border compact",
                caption = "Perte énergétique",
                selection = "none",
                options = list(dom = 't',ordering=F,
                               columnDefs = list(list(targets=c(0), visible=F),
                                                 list(targets=c(1), visible=TRUE, width='150')))) %>%
        formatCurrency(2,"%",dec.mark = ",",before = FALSE)
    })
    
    observeEvent(input[["DT_PerteEnergie_cell_edit"]], {
      cell <- input[["DT_PerteEnergie_cell_edit"]]
      newdf <- REACT_Config_PerteEnergie()
      newdf[cell$row, cell$col] <- as.numeric(cell$value)
      REACT_Config_PerteEnergie(newdf)
    })

    REACT_Modifications_Config_PerteEnergie <- reactive({
      if( setequal(c(REACT_Config_PerteEnergie()$Perte),c(PerteEnergie_initiale$Perte)) == FALSE)
      {1} else {0}
    })
    
    observeEvent(input$button_reset_Config_Elect, {
      
      newdf_mixelect <- MixElect_initiale
      REACT_Config_MixElect(newdf_mixelect)
      REACT_Config_MixElect_2(newdf_mixelect)
      
      newdf_perteenergie <- PerteEnergie_initiale
      REACT_Config_PerteEnergie(newdf_perteenergie)
      REACT_Config_PerteEnergie_2(newdf_perteenergie)
      
    })
    
    REACT_Config_MixElect_2 <- reactiveVal(MixElect_initiale)
    REACT_Config_PerteEnergie_2 <- reactiveVal(PerteEnergie_initiale)
    
    observeEvent(input$button_maj_Config_Elect, {
      
      newdf_mixelect <- MixElect_initiale
      newdf_perteenergie <- PerteEnergie_initiale
      
      if( REACT_Modifications_Config_MixElect() + REACT_Modifications_Config_PerteEnergie() != 0  ) {
        newdf_mixelect <- REACT_Config_MixElect()
        newdf_perteenergie <- REACT_Config_PerteEnergie()
      }
      
      REACT_Config_MixElect_2(newdf_mixelect)
      REACT_Config_PerteEnergie_2(newdf_perteenergie)
      
    })
    
    output$GRAPH_Synthese_MixElect_tout_ly <- renderPlotly({
      
      DataGraph <- REACT_Config_MixElect_2() %>% mutate(Part = round(Part,1)) %>%
        mutate(Energie = fct_collapse(Energie, "Renouvelables" = c("Hydraulique","eolien","incinerateur","photovoltaique","biomasse"))) %>% 
        group_by(Energie) %>% summarise(Part = sum(Part)) %>%
        mutate(Energie = fct_relevel(Energie, c("Renouvelables","Petrole","Nucleaire","Gaz Naturel","Charbon"))) 
      DataGraph$Energie <- renames_all(DataGraph$Energie)
      DataGraph <- DataGraph[tri_perso(DataGraph$Energie, couleurs$Nom[-7]),]
      plot_ly(DataGraph, labels = ~Energie, values = ~Part, type = "pie",
              marker = list(colors = couleurs$HEX_trait[match(DataGraph$Energie,couleurs$Nom[-7])+1]),
              rotation = 90+(3.60*DataGraph$Part[1])/2,
              textposition = "top right", 
              textinfo = 'label+percent',
              insidetextfont = list(color = '#000000', front = "bold"),
              hoverinfo = 'text', 
              text = ~paste(Energie,"\n" ,Part,"%")
      ) %>%
        layout(title = "Mix électrique",
               xaxis = list(title = "Motorisation"),
               yaxis = list(title = 'Part (%)',
                            range = c(0,100))) %>% 
        layout(showlegend = FALSE)
      
    })
    output$GRAPH_Synthese_MixElect_renew <- renderPlotly({
      
      DataGraph <- REACT_Config_MixElect_2() %>% mutate(Part = round(Part,1)) %>%
        filter(Energie %in% c("Hydraulique","eolien","incinerateur","photovoltaique","biomasse")) %>%
        group_by(Energie) %>% summarise(Part = sum(Part))
      DataGraph$Energie <- renames_all(DataGraph$Energie)
      DataGraph <- DataGraph[tri_perso(DataGraph$Energie, couleurs$Nom),]
      fig <- plot_ly(DataGraph,labels = ~Energie, values = ~Part, type = "pie",
                     marker = list(colors = couleurs$HEX_trait[match(DataGraph$Energie,couleurs$Nom)]),
                     showlegend = FALSE,
                     textposition = "top right", 
                     textinfo = 'label',
                     hoverinfo = 'text',
                     text = ~paste(Energie," " ,Part,"%")
                      )
      fig
      
    })
    
    output$Visu_PerteMoyenne <- renderText({
      
      PerteMoyenne <- REACT_Config_PerteEnergie_2()[1,2] +
        REACT_Config_PerteEnergie_2()[2,2] * (1 - REACT_Config_MixElect_2()[REACT_Config_MixElect_2()$Energie == "photovoltaique",2]/100) +
        REACT_Config_PerteEnergie_2()[3,2] * (1 - REACT_Config_MixElect_2()[REACT_Config_MixElect_2()$Energie == "photovoltaique",2]/100 - REACT_Config_MixElect_2()[REACT_Config_MixElect_2()$Energie == "incinerateur",2]/100)

      paste0("Perte moyenne (réseau et transformateurs) : ", abs(round(PerteMoyenne,2)) , " %")
      
    })
    
    # Conso et %hybride ----
    ConsoHybride_initiale <- data.frame("conso_elec" = conso_elec, "hybrid_elec" = hybrid_elec)
    
    REACT_Config_ConsoHybride <- reactiveVal(ConsoHybride_initiale)
    
    output$DT_ConsoHybride <- renderDT({
      
      datatable(REACT_Config_ConsoHybride()*100,
                colnames = c("Consommation électrique moyenne (kWh/100km)", "Usage des hybrides en mode électrique (%)"),
                editable = "cell",
                class = "hover cell-border compact",
                caption = "Hypothèses de consommation électrique",
                selection = "none",
                options = list(dom = 't',ordering=F,
                               columnDefs = list(list(targets=c(0), visible=F),
                                                 list(targets=c(1), visible=TRUE, width='400')))) %>% 
        formatCurrency(1," kWh/100km",dec.mark = ",",before = FALSE, digits = 1) %>% 
        formatCurrency(2,"%",dec.mark = ",",before = FALSE, digits = 1)
    })
    
    observeEvent(input[["DT_ConsoHybride_cell_edit"]], {
      cell <- input[["DT_ConsoHybride_cell_edit"]]
      newdf <- REACT_Config_ConsoHybride()
      newdf[cell$row, cell$col] <- as.numeric(cell$value)/100
      REACT_Config_ConsoHybride(newdf)
    })
    
    REACT_Modifications_Config_ConsoHybride <- reactive({
      if( setequal(c(REACT_Config_ConsoHybride()$conso_elec),c(ConsoHybride_initiale$ltrip)) == FALSE | 
          setequal(c(REACT_Config_ConsoHybride()$hybrid_elec),c(ConsoHybride_initiale$hybrid_elec)) == FALSE )
      {1} else {0}
    })
    
    observeEvent(input$button_reset_Config_ConsoHybride, {
      
      newdf_ConsoHybride <- ConsoHybride_initiale
      REACT_Config_ConsoHybride(newdf_ConsoHybride)
      REACT_Config_ConsoHybride_2(newdf_ConsoHybride)   
      
    })
    
    REACT_Config_ConsoHybride_2 <- reactiveVal(ConsoHybride_initiale)
    
    observeEvent(input$button_maj_Config_ConsoHybride, {
      
      newdf_ConsoHybride <- ConsoHybride_initiale
      
      if( REACT_Modifications_Config_ConsoHybride() != 0 ) {
        newdf_ConsoHybride <- REACT_Config_ConsoHybride()
      }
      
      REACT_Config_ConsoHybride_2(newdf_ConsoHybride)
      
    })

    # ---- _Poids et équipements ----
    
    Masse_vehicule <- aggregate(Masse_DV[,c(-1,-6,-7)],list(Parc_utilisateur[[1]]$Fuel,Parc_utilisateur[[1]]$Segment),mean)
    Masse_vehicule <- Masse_vehicule %>% rename(Segment = Group.2, Fuel = Group.1)
    Masse_vehicule$Segment <- fct_relevel(Masse_vehicule$Segment,c("Mini","Small","Medium","Large"))
    Masse_vehicule <- arrange(Masse_vehicule,Segment)
    
    # Masse ----
    # essence
    MasseEssence_initiale <- Masse_vehicule %>% filter(Fuel == "Petrol")
    MasseEssence_initiale <- MasseEssence_initiale[,-1]
    REACT_Config_MasseEssence <- reactiveVal(MasseEssence_initiale)
    output$DT_MasseEssence <- renderDT({

      datatable(REACT_Config_MasseEssence() %>%
                  mutate(Segment = recode(Segment, "Small" = "Petit")) %>% mutate("Masse Totale" = M_carros+M_moteur_ICE+M_moteur_Elec+M_batterie),
                colnames = c('Segment de puissance','Masse carrosserie',"Masse du moteur thermique","Masse du moteur électrique","Masse de la batterie (veh. électrique)","TOTAL Masse"),
                editable = list(target = "cell",disable = list(columns = c(1,6) )),
                class = "hover cell-border compact",
                caption = "Masse des véhicules Essence (kg)",
                selection = "none",
                options = list(dom = 't',ordering=F,
                               columnDefs = list(list(targets=c(0), visible=F),
                                                 list(targets=c(1:6), visible = T, width='100')))) %>% 
        formatCurrency(2:6," kg",dec.mark = ",",mark = " ",before = FALSE,digits = 0) %>%
        formatStyle(6,fontWeight = "bold") %>%
        formatStyle(1, backgroundColor = styleEqual(couleurs$Nom, couleurs$HEX_aire), fontWeight = "bold")
    })
    observeEvent(input[["DT_MasseEssence_cell_edit"]], {
      cell <- input[["DT_MasseEssence_cell_edit"]]
      newdf <- REACT_Config_MasseEssence()
      newdf[cell$row, cell$col] <- as.numeric(cell$value)
      REACT_Config_MasseEssence(newdf)
    })
    REACT_Modifications_Config_MasseEssence <- reactive({
      if( setequal(c(REACT_Config_MasseEssence()$M_carros),c(MasseEssence_initiale$M_carros)) == FALSE |
          setequal(c(REACT_Config_MasseEssence()$M_moteur_ICE),c(MasseEssence_initiale$M_moteur_ICE)) == FALSE |
          setequal(c(REACT_Config_MasseEssence()$M_moteur_Elec),c(MasseEssence_initiale$M_moteur_Elec)) == FALSE |
          setequal(c(REACT_Config_MasseEssence()$M_batterie),c(MasseEssence_initiale$M_batterie)) == FALSE )
      {1} else {0}
    })
    
    # diesel
    MasseDiesel_initiale <- Masse_vehicule %>% filter(Fuel == "Diesel")
    MasseDiesel_initiale <- MasseDiesel_initiale[,-1]
    REACT_Config_MasseDiesel <- reactiveVal(MasseDiesel_initiale)
    output$DT_MasseDiesel <- renderDT({
      
      datatable(REACT_Config_MasseDiesel() %>%
                  mutate(Segment = recode(Segment, "Small" = "Petit")) %>% mutate("Masse Totale" = M_carros+M_moteur_ICE+M_moteur_Elec+M_batterie),
                colnames = c('Segment de puissance','Masse carrosserie',"Masse du moteur thermique","Masse du moteur électrique","Masse de la batterie (veh. électrique)","TOTAL Masse"),
                editable = list(target = "cell",disable = list(columns = c(1,6) )),
                class = "hover cell-border compact",
                caption = "Masse des véhicules Diesel (kg)",
                selection = "none",
                options = list(dom = 't',ordering=F,
                               columnDefs = list(list(targets=c(0), visible=F),
                                                 list(targets=c(1:6), visible = T, width='100')))) %>% 
        formatCurrency(2:6," kg",dec.mark = ",",mark = " ",before = FALSE,digits = 0) %>%
        formatStyle(6,fontWeight = "bold") %>%
        formatStyle(1, backgroundColor = styleEqual(couleurs$Nom, couleurs$HEX_aire), fontWeight = "bold")
    })
    observeEvent(input[["DT_MasseDiesel_cell_edit"]], {
      cell <- input[["DT_MasseDiesel_cell_edit"]]
      newdf <- REACT_Config_MasseDiesel()
      newdf[cell$row, cell$col] <- as.numeric(cell$value)
      REACT_Config_MasseDiesel(newdf)
    })
    REACT_Modifications_Config_MasseDiesel <- reactive({
      if( setequal(c(REACT_Config_MasseDiesel()$M_carros),c(MasseDiesel_initiale$M_carros)) == FALSE |
          setequal(c(REACT_Config_MasseDiesel()$M_moteur_ICE),c(MasseDiesel_initiale$M_moteur_ICE)) == FALSE |
          setequal(c(REACT_Config_MasseDiesel()$M_moteur_Elec),c(MasseDiesel_initiale$M_moteur_Elec)) == FALSE |
          setequal(c(REACT_Config_MasseDiesel()$M_batterie),c(MasseDiesel_initiale$M_batterie)) == FALSE )
      {1} else {0}
    })
    
    # electrique
    MasseElectrique_initiale <- Masse_vehicule %>% filter(Fuel == "Electric")
    MasseElectrique_initiale <- MasseElectrique_initiale[,-1]
    REACT_Config_MasseElectrique <- reactiveVal(MasseElectrique_initiale)
    output$DT_MasseElectrique <- renderDT({
      
      datatable(REACT_Config_MasseElectrique() %>%
                  mutate(Segment = recode(Segment, "Small" = "Petit")) %>% mutate("Masse Totale" = M_carros+M_moteur_ICE+M_moteur_Elec+M_batterie),
                colnames = c('Segment de puissance','Masse carrosserie',"Masse du moteur thermique","Masse du moteur électrique","Masse de la batterie (veh. électrique)","TOTAL Masse"),
                editable = list(target = "cell",disable = list(columns = c(1,6) )),
                class = "hover cell-border compact",
                caption = "Masse des véhicules Electrique (kg)",
                selection = "none",
                options = list(dom = 't',ordering=F,
                               columnDefs = list(list(targets=c(0), visible=F),
                                                 list(targets=c(1:6), visible = T, width='100')))) %>% 
        formatCurrency(2:6," kg",dec.mark = ",",mark = " ",before = FALSE,digits = 0) %>%
        formatStyle(6,fontWeight = "bold") %>%
        formatStyle(1, backgroundColor = styleEqual(couleurs$Nom, couleurs$HEX_aire), fontWeight = "bold")
    })
    observeEvent(input[["DT_MasseElectrique_cell_edit"]], {
      cell <- input[["DT_MasseElectrique_cell_edit"]]
      newdf <- REACT_Config_MasseElectrique()
      newdf[cell$row, cell$col] <- as.numeric(cell$value)
      REACT_Config_MasseElectrique(newdf)
    })
    REACT_Modifications_Config_MasseElectrique <- reactive({
      if( setequal(c(REACT_Config_MasseElectrique()$M_carros),c(MasseElectrique_initiale$M_carros)) == FALSE |
          setequal(c(REACT_Config_MasseElectrique()$M_moteur_ICE),c(MasseElectrique_initiale$M_moteur_ICE)) == FALSE |
          setequal(c(REACT_Config_MasseElectrique()$M_moteur_Elec),c(MasseElectrique_initiale$M_moteur_Elec)) == FALSE |
          setequal(c(REACT_Config_MasseElectrique()$M_batterie),c(MasseElectrique_initiale$M_batterie)) == FALSE )
      {1} else {0}
    })
    
    # hybride
    MasseHybride_initiale <- Masse_vehicule %>% filter(Fuel == "Petrol Hybrid")
    MasseHybride_initiale <- MasseHybride_initiale[,-1]
    REACT_Config_MasseHybride <- reactiveVal(MasseHybride_initiale)
    output$DT_MasseHybride <- renderDT({
      
      datatable(REACT_Config_MasseHybride() %>%
                  mutate(Segment = recode(Segment, "Small" = "Petit")) %>% mutate("Masse Totale" = M_carros+M_moteur_ICE+M_moteur_Elec+M_batterie),
                colnames = c('Segment de puissance','Masse carrosserie',"Masse du moteur thermique","Masse du moteur électrique","Masse de la batterie (veh. électrique)","TOTAL Masse"),
                editable = list(target = "cell",disable = list(columns = c(1,6) )),
                class = "hover cell-border compact",
                caption = "Masse des véhicules Hybride (kg)",
                selection = "none",
                options = list(dom = 't',ordering=F,
                               columnDefs = list(list(targets=c(0), visible=F),
                                                 list(targets=c(1:6), visible = T, width='100')))) %>% 
        formatCurrency(2:6," kg",dec.mark = ",",mark = " ",before = FALSE,digits = 0) %>%
        formatStyle(6,fontWeight = "bold") %>%
        formatStyle(1, backgroundColor = styleEqual(couleurs$Nom, couleurs$HEX_aire), fontWeight = "bold")
    })
    observeEvent(input[["DT_MasseHybride_cell_edit"]], {
      cell <- input[["DT_MasseHybride_cell_edit"]]
      newdf <- REACT_Config_MasseHybride()
      newdf[cell$row, cell$col] <- as.numeric(cell$value)
      REACT_Config_MasseHybride(newdf)
    })
    REACT_Modifications_Config_MasseHybride <- reactive({
      if( setequal(c(REACT_Config_MasseHybride()$M_carros),c(MasseHybride_initiale$M_carros)) == FALSE |
          setequal(c(REACT_Config_MasseHybride()$M_moteur_ICE),c(MasseHybride_initiale$M_moteur_ICE)) == FALSE |
          setequal(c(REACT_Config_MasseHybride()$M_moteur_Elec),c(MasseHybride_initiale$M_moteur_Elec)) == FALSE |
          setequal(c(REACT_Config_MasseHybride()$M_batterie),c(MasseHybride_initiale$M_batterie)) == FALSE )
      {1} else {0}
    })
    
    # Gaz naturel
    MasseGazNaturel_initiale <- Masse_vehicule %>% filter(Fuel == "CNG")
    MasseGazNaturel_initiale <- MasseGazNaturel_initiale[,-1]
    REACT_Config_MasseGazNaturel <- reactiveVal(MasseGazNaturel_initiale)
    output$DT_MasseGazNaturel <- renderDT({
      
      datatable(REACT_Config_MasseGazNaturel() %>%
                  mutate(Segment = recode(Segment, "Small" = "Petit")) %>% mutate("Masse Totale" = M_carros+M_moteur_ICE+M_moteur_Elec+M_batterie),
                colnames = c('Segment de puissance','Masse carrosserie',"Masse du moteur thermique","Masse du moteur électrique","Masse de la batterie (veh. électrique)","TOTAL Masse"),
                editable = list(target = "cell",disable = list(columns = c(1,6) )),
                class = "hover cell-border compact",
                caption = "Masse des véhicules GazNaturel (kg)",
                selection = "none",
                options = list(dom = 't',ordering=F,
                               columnDefs = list(list(targets=c(0), visible=F),
                                                 list(targets=c(1:6), visible = T, width='100')))) %>% 
        formatCurrency(2:6," kg",dec.mark = ",",mark = " ",before = FALSE,digits = 0) %>%
        formatStyle(6,fontWeight = "bold") %>%
        formatStyle(1, backgroundColor = styleEqual(couleurs$Nom, couleurs$HEX_aire), fontWeight = "bold")
    })
    observeEvent(input[["DT_MasseGazNaturel_cell_edit"]], {
      cell <- input[["DT_MasseGazNaturel_cell_edit"]]
      newdf <- REACT_Config_MasseGazNaturel()
      newdf[cell$row, cell$col] <- as.numeric(cell$value)
      REACT_Config_MasseGazNaturel(newdf)
    })
    REACT_Modifications_Config_MasseGazNaturel <- reactive({
      if( setequal(c(REACT_Config_MasseGazNaturel()$M_carros),c(MasseGazNaturel_initiale$M_carros)) == FALSE |
          setequal(c(REACT_Config_MasseGazNaturel()$M_moteur_ICE),c(MasseGazNaturel_initiale$M_moteur_ICE)) == FALSE |
          setequal(c(REACT_Config_MasseGazNaturel()$M_moteur_Elec),c(MasseGazNaturel_initiale$M_moteur_Elec)) == FALSE |
          setequal(c(REACT_Config_MasseGazNaturel()$M_batterie),c(MasseGazNaturel_initiale$M_batterie)) == FALSE )
      {1} else {0}
    })
    
    # GPL
    MasseGPL_initiale <- Masse_vehicule %>% filter(Fuel == "LPG")
    MasseGPL_initiale <- MasseGPL_initiale[,-1]
    REACT_Config_MasseGPL <- reactiveVal(MasseGPL_initiale)
    output$DT_MasseGPL <- renderDT({
      
      datatable(REACT_Config_MasseGPL() %>%
                  mutate(Segment = recode(Segment, "Small" = "Petit")) %>% mutate("Masse Totale" = M_carros+M_moteur_ICE+M_moteur_Elec+M_batterie),
                colnames = c('Segment de puissance','Masse carrosserie',"Masse du moteur thermique","Masse du moteur électrique","Masse de la batterie (veh. électrique)","TOTAL Masse"),
                editable = list(target = "cell",disable = list(columns = c(1,6) )),
                class = "hover cell-border compact",
                caption = "Masse des véhicules GPL (kg)",
                selection = "none",
                options = list(dom = 't',ordering=F,
                               columnDefs = list(list(targets=c(0), visible=F),
                                                 list(targets=c(1:6), visible = T, width='100')))) %>% 
        formatCurrency(2:6," kg",dec.mark = ",",mark = " ",before = FALSE,digits = 0) %>%
        formatStyle(6,fontWeight = "bold") %>%
        formatStyle(1, backgroundColor = styleEqual(couleurs$Nom, couleurs$HEX_aire), fontWeight = "bold")
    })
    observeEvent(input[["DT_MasseGPL_cell_edit"]], {
      cell <- input[["DT_MasseGPL_cell_edit"]]
      newdf <- REACT_Config_MasseGPL()
      newdf[cell$row, cell$col] <- as.numeric(cell$value)
      REACT_Config_MasseGPL(newdf)
    })
    REACT_Modifications_Config_MasseGPL <- reactive({
      if( setequal(c(REACT_Config_MasseGPL()$M_carros),c(MasseGPL_initiale$M_carros)) == FALSE |
          setequal(c(REACT_Config_MasseGPL()$M_moteur_ICE),c(MasseGPL_initiale$M_moteur_ICE)) == FALSE |
          setequal(c(REACT_Config_MasseGPL()$M_moteur_Elec),c(MasseGPL_initiale$M_moteur_Elec)) == FALSE |
          setequal(c(REACT_Config_MasseGPL()$M_batterie),c(MasseGPL_initiale$M_batterie)) == FALSE )
      {1} else {0}
    })
    
    MasseComplet_initiale <- rbind(
        MasseEssence_initiale %>% mutate(Fuel = "Petrol"),
        MasseDiesel_initiale %>% mutate(Fuel = "Diesel"),
        MasseElectrique_initiale %>% mutate(Fuel = "Electric"),
        MasseHybride_initiale %>% mutate(Fuel = "Petrol Hybrid"),
        MasseGazNaturel_initiale %>% mutate(Fuel = "CNG"),
        MasseGPL_initiale %>% mutate(Fuel = "LPG")
      )
    REACT_Config_MasseComplet <- reactive({
      rbind(
        REACT_Config_MasseEssence() %>% mutate(Fuel = "Petrol"),
        REACT_Config_MasseDiesel() %>% mutate(Fuel = "Diesel"),
        REACT_Config_MasseElectrique() %>% mutate(Fuel = "Electric"),
        REACT_Config_MasseHybride() %>% mutate(Fuel = "Petrol Hybrid"),
        REACT_Config_MasseGazNaturel() %>% mutate(Fuel = "CNG"),
        REACT_Config_MasseGPL() %>% mutate(Fuel = "LPG")
      )
    })
    REACT_Config_MasseComplet_2 <- reactiveVal({
      rbind(
        MasseEssence_initiale %>% mutate(Fuel = "Petrol"),
        MasseDiesel_initiale %>% mutate(Fuel = "Diesel"),
        MasseElectrique_initiale %>% mutate(Fuel = "Electric"),
        MasseHybride_initiale %>% mutate(Fuel = "Petrol Hybrid"),
        MasseGazNaturel_initiale %>% mutate(Fuel = "CNG"),
        MasseGPL_initiale %>% mutate(Fuel = "LPG")
      )
    })
    
    REACT_Config_MasseEssence_2 <- reactiveVal(MasseEssence_initiale)
    REACT_Config_MasseDiesel_2 <- reactiveVal(MasseDiesel_initiale)
    REACT_Config_MasseElectrique_2 <- reactiveVal(MasseElectrique_initiale)
    REACT_Config_MasseHybride_2 <- reactiveVal(MasseHybride_initiale)
    REACT_Config_MasseGazNaturel_2 <- reactiveVal(MasseGazNaturel_initiale)
    REACT_Config_MasseGPL_2 <- reactiveVal(MasseGPL_initiale)
    
    observeEvent(input$button_reset_Config_Masse, {

      newdf_MasseEssence <- MasseEssence_initiale
      REACT_Config_MasseEssence(newdf_MasseEssence)
      REACT_Config_MasseEssence_2(newdf_MasseEssence)

      newdf_MasseDiesel <- MasseDiesel_initiale
      REACT_Config_MasseDiesel(newdf_MasseDiesel)
      REACT_Config_MasseDiesel_2(newdf_MasseDiesel)
      
      newdf_MasseElectrique <- MasseElectrique_initiale
      REACT_Config_MasseElectrique(newdf_MasseElectrique)
      REACT_Config_MasseElectrique_2(newdf_MasseElectrique)
      
      newdf_MasseHybride <- MasseHybride_initiale
      REACT_Config_MasseHybride(newdf_MasseHybride)
      REACT_Config_MasseHybride_2(newdf_MasseHybride)
      
      newdf_MasseGazNaturel <- MasseGazNaturel_initiale
      REACT_Config_MasseGazNaturel(newdf_MasseGazNaturel)
      REACT_Config_MasseGazNaturel_2(newdf_MasseGazNaturel)
      
      newdf_MasseGPL <- MasseGPL_initiale
      REACT_Config_MasseGPL(newdf_MasseGPL)
      REACT_Config_MasseGPL_2(newdf_MasseGPL)
      
      newdf_MasseComplet <- MasseComplet_initiale
      REACT_Config_MasseComplet_2(newdf_MasseComplet)

    })

    observeEvent(input$button_maj_Config_Masse, {

      newdf_MasseEssence <- MasseEssence_initiale
      newdf_MasseDiesel <- MasseDiesel_initiale
      newdf_MasseElectrique <- MasseElectrique_initiale
      newdf_MasseHybride <- MasseHybride_initiale
      newdf_MasseGazNaturel <- MasseGazNaturel_initiale
      newdf_MasseGPL <- MasseGPL_initiale
      newdf_MasseComplet <- MasseComplet_initiale

      if( REACT_Modifications_Config_MasseEssence() + REACT_Modifications_Config_MasseDiesel() + REACT_Modifications_Config_MasseElectrique() +
          REACT_Modifications_Config_MasseHybride() + REACT_Modifications_Config_MasseGazNaturel() + REACT_Modifications_Config_MasseGPL() != 0  ) {
        newdf_MasseEssence <- REACT_Config_MasseEssence()
        newdf_MasseDiesel <- REACT_Config_MasseDiesel()
        newdf_MasseElectrique <- REACT_Config_MasseElectrique()
        newdf_MasseHybride<- REACT_Config_MasseHybride()
        newdf_MasseGazNaturel<- REACT_Config_MasseGazNaturel()
        newdf_MasseGPL <- REACT_Config_MasseGPL()
        newdf_MasseComplet <- REACT_Config_MasseComplet()
      }

      REACT_Config_MasseEssence_2(newdf_MasseEssence)
      REACT_Config_MasseDiesel_2(newdf_MasseDiesel)
      REACT_Config_MasseElectrique_2(newdf_MasseElectrique)
      REACT_Config_MasseHybride_2(newdf_MasseHybride)
      REACT_Config_MasseGazNaturel_2(newdf_MasseGazNaturel)
      REACT_Config_MasseGPL_2(newdf_MasseGPL)
      REACT_Config_MasseComplet_2(newdf_MasseComplet)

    })
    
    # Durée de vie ----
    # véhicules
    DureeVieVehicule_initiale <- data.frame(
      "Fuel" = c("Petrol", "Diesel", "CNG", "Electric", "LPG", "Petrol Hybrid"),
      "DV_veh" = c(150000,150000,150000,150000,150000,150000)
    )
    
    REACT_Config_DureeVieVehicule <- reactiveVal(DureeVieVehicule_initiale)
    
    output$DT_DureeVieVehicule <- renderDT({
      
      datatable(REACT_Config_DureeVieVehicule() %>%
                  mutate(Fuel = recode(Fuel, "CNG" = "Gaz Naturel", "Petrol" = "Essence", "LPG" = "GPL", "Petrol Hybrid" = "Hybride", "Electric" = "Electrique")),
                colnames = c('Motorisation','Durée de vie des véhicule (km)'),
                editable = list(target = "cell",disable = list(columns = 1 )),
                class = "hover cell-border compact",
                caption = "Durée de vie des véhicules",
                selection = "none",
                options = list(dom = 't',ordering=F,
                               columnDefs = list(list(targets=c(0), visible=F),
                                                 list(targets=c(1), visible=TRUE, width='150')))) %>%
        formatCurrency(2," km",dec.mark = ",", mark = " ",before = FALSE,digits = 0) %>%
        formatStyle(1, backgroundColor = styleEqual(couleurs$Nom, couleurs$HEX_aire), fontWeight = "bold")
    })
    
    observeEvent(input[["DT_DureeVieVehicule_cell_edit"]], {
      cell <- input[["DT_DureeVieVehicule_cell_edit"]]
      newdf <- REACT_Config_DureeVieVehicule()
      newdf[cell$row, cell$col] <- as.numeric(cell$value)
      REACT_Config_DureeVieVehicule(newdf)
    })
    
    REACT_Modifications_Config_DureeVieVehicule <- reactive({
      if( setequal(c(REACT_Config_DureeVieVehicule()$DV_veh),c(DureeVieVehicule_initiale$DV_veh)) == FALSE)
      {1} else {0}
    })
    
    
    # batterie
    DureeVieBatterie_initiale <- data.frame(
      "DV_batterie" = c(100000)
    )
    
    REACT_Config_DureeVieBatterie <- reactiveVal(DureeVieBatterie_initiale)
    
    output$DT_DureeVieBatterie <- renderDT({
      
      datatable(REACT_Config_DureeVieBatterie(),
                colnames = c('Durée de vie de la batterie (km)'),
                editable = "cell",
                class = "hover cell-border compact",
                caption = "Durée de vie de la batterie",
                selection = "none",
                options = list(dom = 't',ordering=F,
                               columnDefs = list(list(targets=c(0), visible=F)))) %>%
        formatCurrency(1," km",dec.mark = ",", mark = " ",before = FALSE,digits = 0)
    })
    
    observeEvent(input[["DT_DureeVieBatterie_cell_edit"]], {
      cell <- input[["DT_DureeVieBatterie_cell_edit"]]
      newdf <- REACT_Config_DureeVieBatterie()
      newdf[cell$row, cell$col] <- as.numeric(cell$value)
      REACT_Config_DureeVieBatterie(newdf)
    })
    
    REACT_Modifications_Config_DureeVieBatterie <- reactive({
      if( setequal(c(REACT_Config_DureeVieBatterie()$DV_batterie),c(DureeVieBatterie_initiale$DV_batterie)) == FALSE)
      {1} else {0}
    })
    
    
    REACT_Config_DureeVieBatterie_2 <- reactiveVal(DureeVieBatterie_initiale)
    REACT_Config_DureeVieVehicule_2 <- reactiveVal(DureeVieVehicule_initiale)
    
    observeEvent(input$button_reset_Config_DureeVie, {
      
      newdf_DureeVieBatterie <- DureeVieBatterie_initiale
      REACT_Config_DureeVieBatterie(newdf_DureeVieBatterie)
      REACT_Config_DureeVieBatterie_2(newdf_DureeVieBatterie)
      
      newdf_DureeVieVehicule <- DureeVieVehicule_initiale
      REACT_Config_DureeVieVehicule(newdf_DureeVieVehicule)
      REACT_Config_DureeVieVehicule_2(newdf_DureeVieVehicule)
      
    })
    
    observeEvent(input$button_maj_Config_DureeVie, {
      
      newdf_DureeVieBatterie <- DureeVieBatterie_initiale
      newdf_DureeVieVehicule <- DureeVieVehicule_initiale

      if( REACT_Modifications_Config_DureeVieBatterie() + REACT_Modifications_Config_DureeVieVehicule() != 0  ) {
        newdf_DureeVieBatterie <- REACT_Config_DureeVieBatterie()
        newdf_DureeVieVehicule <- REACT_Config_DureeVieVehicule()
      }
      
      REACT_Config_DureeVieBatterie_2(newdf_DureeVieBatterie)
      REACT_Config_DureeVieVehicule_2(newdf_DureeVieVehicule)
      
    })
    
    # Climatisation ----
    AC_initiale <- data.frame(
      "Euro.Standard" = c("Euro 1","Euro 2","Euro 3","Euro 4","Euro 5","Euro 6b","Euro 6c","Euro 6d"), 
      "InstalledAC" = c(0.20,0.60,0.85,0.95,0.95,0.95,0.95,0.95)*100,
      "ACUsage" = c(0.4,0.4,0.4,0.4,0.4,0.4,0.4,0.4)*100
    )
    
    REACT_Config_AC <- reactiveVal(AC_initiale)
    
    output$DT_AC <- renderDT({
      
      datatable(REACT_Config_AC(),
                colnames = c('Norme Euro',"Taux d'installation de climatisation (%)","Taux d'utilisation (%)"),
                editable = list(target = "cell",disable = list(columns = 1 )),
                class = "hover cell-border compact",
                caption = "Installation de la climatisation",
                selection = "none",
                options = list(dom = 't',ordering=F,
                               columnDefs = list(list(targets=c(0), visible=F),
                                                 list(targets=c(1,3), visible=TRUE, width='150'),
                                                 list(targets=c(2), visible=TRUE, width='250')))) %>% 
        formatCurrency(2:3,"%",dec.mark = ",",before = FALSE, digits = 0) %>% 
        formatStyle(1, backgroundColor = styleEqual(couleurs$Nom, couleurs$HEX_aire), fontWeight = "bold")

    })
    
    observeEvent(input[["DT_AC_cell_edit"]], {
      cell <- input[["DT_AC_cell_edit"]]
      newdf <- REACT_Config_AC()
      newdf[cell$row, cell$col] <- as.numeric(cell$value)
      REACT_Config_AC(newdf)
    })
    
    REACT_Modifications_Config_AC <- reactive({
      if( setequal(c(REACT_Config_AC()$InstalledAC),c(AC_initiale$InstalledAC)) == FALSE |
          setequal(c(REACT_Config_AC()$ACUsage),c(AC_initiale$ACUsage)) == FALSE )
      {1} else {0}
    })
    
    
    REACT_Config_AC_2 <- reactiveVal(AC_initiale)
    
    observeEvent(input$button_reset_Config_AC, {
      
      newdf_AC <- AC_initiale
      REACT_Config_AC(newdf_AC)
      REACT_Config_AC_2(newdf_AC)
      
    })
    
    observeEvent(input$button_maj_Config_AC, {
      
      newdf_AC <- AC_initiale
      
      if( REACT_Modifications_Config_AC() != 0  ) {
        newdf_AC <- REACT_Config_AC()
      }
      
      REACT_Config_AC_2(newdf_AC)
      
    })

    # Technologies de dépollution ----
    # Essence
    TechnoDepolEssence_initiale <- data.frame(
      "Essence" = c("Injection directe", "Injection séquentielle", "Inj. directe + filtres à particules"),
      "Abrev" = c("GDI","PFI","GDI+GPF"),
      "Euro 1" = c(NA,NA,NA),"Euro 2" = c(NA,NA,NA),"Euro 3" = c(50,50,NA),"Euro 4" = c(50,50,NA),
      "Euro 5" = c(50,50,NA),"Euro 6b" = c(40,40,20),"Euro 6c" = c(40,40,20),"Euro 6d" = c(40,40,20)
    )
    
    REACT_Config_TechnoDepolEssence <- reactiveVal(TechnoDepolEssence_initiale)
    
    output$DT_TechnoDepolEssence <- renderDT({
      
      datatable(REACT_Config_TechnoDepolEssence(),
                editable = "cell",
                class = "hover cell-border compact",
                caption = "Dépollution - véhicules essence",
                options = list(dom = 't',ordering=F)
      )
    })
    
    observeEvent(input[["DT_TechnoDepolEssence_cell_edit"]], {
      cell <- input[["DT_TechnoDepolEssence_cell_edit"]]
      newdf <- REACT_Config_TechnoDepolEssence()
      newdf[cell$row, cell$col] <- as.numeric(cell$value)
      REACT_Config_TechnoDepolEssence(newdf)
    })
    
    REACT_Modifications_Config_TechnoDepolEssence <- reactive({
      if( setequal(c(REACT_Config_TechnoDepolEssence()$Euro.1),c(TechnoDepolEssence_initiale$Euro.1)) == FALSE | 
          setequal(c(REACT_Config_TechnoDepolEssence()$Euro.2),c(TechnoDepolEssence_initiale$Euro.2)) == FALSE |
          setequal(c(REACT_Config_TechnoDepolEssence()$Euro.3),c(TechnoDepolEssence_initiale$Euro.3)) == FALSE |
          setequal(c(REACT_Config_TechnoDepolEssence()$Euro.4),c(TechnoDepolEssence_initiale$Euro.4)) == FALSE |
          setequal(c(REACT_Config_TechnoDepolEssence()$Euro.5),c(TechnoDepolEssence_initiale$Euro.5)) == FALSE |
          setequal(c(REACT_Config_TechnoDepolEssence()$Euro.6b),c(TechnoDepolEssence_initiale$Euro.6b)) == FALSE |
          setequal(c(REACT_Config_TechnoDepolEssence()$Euro.6c),c(TechnoDepolEssence_initiale$Euro.6c)) == FALSE |
          setequal(c(REACT_Config_TechnoDepolEssence()$Euro.6d),c(TechnoDepolEssence_initiale$Euro.6d)) == FALSE)
      {1} else {0}
    })
    
    # Diesel
    TechnoDepolDiesel_initiale <- data.frame(
      "Diesel" = c("Filtres à particules", "Filtres + Red.catalytique selective", "Filtre + Piège à NOx"),
      "Abrev" = c("DPF","DPF+SCR","DPF+LNT"),
      "Euro 1" = c(NA,NA,NA),"Euro 2" = c(NA,NA,NA),"Euro 3" = c(100,NA,NA),"Euro 4" = c(100,NA,NA),
      "Euro 5" = c(100,NA,NA),"Euro 6b" = c(40,40,20),"Euro 6c" = c(40,40,20),"Euro 6d" = c(40,40,20)
    )
    
    REACT_Config_TechnoDepolDiesel <- reactiveVal(TechnoDepolDiesel_initiale)
    
    output$DT_TechnoDepolDiesel <- renderDT({
      
      datatable(REACT_Config_TechnoDepolDiesel(),
                editable = "cell",
                class = "hover cell-border compact",
                caption = "Dépollution - véhicules diesel",
                options = list(dom = 't',ordering=F)
      )
    })
    
    observeEvent(input[["DT_TechnoDepolDiesel_cell_edit"]], {
      cell <- input[["DT_TechnoDepolDiesel_cell_edit"]]
      newdf <- REACT_Config_TechnoDepolDiesel()
      newdf[cell$row, cell$col] <- as.numeric(cell$value)
      REACT_Config_TechnoDepolDiesel(newdf)
    })
    
    REACT_Modifications_Config_TechnoDepolDiesel <- reactive({
      if( setequal(c(REACT_Config_TechnoDepolDiesel()$DV_batterie),c(TechnoDepolDiesel_initiale$DV_batterie)) == FALSE)
      {1} else {0}
    })
    
    
    REACT_Config_TechnoDepolDiesel_2 <- reactiveVal(TechnoDepolDiesel_initiale)
    REACT_Config_TechnoDepolEssence_2 <- reactiveVal(TechnoDepolEssence_initiale)
    
    observeEvent(input$button_reset_Config_TechnoDepol, {
      
      newdf_TechnoDepolDiesel <- TechnoDepolDiesel_initiale
      REACT_Config_TechnoDepolDiesel(newdf_TechnoDepolDiesel)
      REACT_Config_TechnoDepolDiesel_2(newdf_TechnoDepolDiesel)
      
      newdf_TechnoDepolEssence <- TechnoDepolEssence_initiale
      REACT_Config_TechnoDepolEssence(newdf_TechnoDepolEssence)
      REACT_Config_TechnoDepolEssence_2(newdf_TechnoDepolEssence)
      
    })
    
    observeEvent(input$button_maj_Config_TechnoDepol, {
      
      newdf_TechnoDepolDiesel <- DureeVieBatterie_initiale
      newdf_TechnoDepolEssence <- TechnoDepolEssence_initiale
      
      if( REACT_Modifications_Config_TechnoDepolDiesel() + REACT_Modifications_Config_TechnoDepolEssence() != 0  ) {
        newdf_TechnoDepolDiesel <- REACT_Config_TechnoDepolDiesel()
        newdf_TechnoDepolEssence <- REACT_Config_TechnoDepolEssence()
      }
      
      REACT_Config_TechnoDepolDiesel_2(newdf_TechnoDepolDiesel)
      REACT_Config_TechnoDepolEssence_2(newdf_TechnoDepolEssence)
      
    })
    
    # ---- _Infrastructures ----
    # Utilisation ----
    InfraUtilisation_initiale <- Infra_usage %>% select(Type_infra,Utilisation) %>% mutate(Utilisation = Utilisation * 100)
    
    REACT_Config_InfraUtilisation <- reactiveVal(InfraUtilisation_initiale)
    
    output$DT_InfraUtilisation <- renderDT({
      
      datatable(REACT_Config_InfraUtilisation() %>% 
                  mutate(Type_infra = recode(Type_infra, "Routeprim" = "Route Primaire","Routesecond" = "Route Secondaire","Routeterti" = "Route Tertiaire")),
                colnames = c("Type de voie", "Part (%)"),
                editable = list(target = "cell",disable = list(columns = 1 )),
                class = "hover cell-border compact",
                caption = "Hypothèses d'utilisation de l'infrastructure",
                selection = "none",
                options = list(dom = 't',ordering=F,
                               columnDefs = list(list(targets=c(0), visible=F),
                                                 list(targets=c(1:2), visible=TRUE, width='100')))) %>% 
        formatCurrency(2,"%",dec.mark = ",",before = FALSE, digits = 1) %>%
        formatStyle(1, backgroundColor = styleEqual(couleurs$Nom, couleurs$HEX_aire), fontWeight = "bold")
    })
    
    observeEvent(input[["DT_InfraUtilisation_cell_edit"]], {
      cell <- input[["DT_InfraUtilisation_cell_edit"]]
      newdf <- REACT_Config_InfraUtilisation()
      newdf[cell$row, cell$col] <- as.numeric(cell$value)
      REACT_Config_InfraUtilisation(newdf)
    })
    
    output$Verif_Somme_InfraUtilisation <- renderText({
      if(sum(REACT_Config_InfraUtilisation()$Utilisation) > 100.01 | sum(REACT_Config_InfraUtilisation()$Utilisation) < 99.99) {paste0("Attention, somme différente de 100 % :"," ", sum(REACT_Config_InfraUtilisation()$Utilisation), " %")}
    })
    
    REACT_Modifications_Config_InfraUtilisation <- reactive({
      if( setequal(c(REACT_Config_Infra()$Utilisation),c(Infra_initiale$Utilisation)) == FALSE)
      {1} else {0}
    })
    
    output$GRAPH_Synthese_InfraUtilisation_ly <- renderPlotly({
      
      DataGraph <- REACT_Config_InfraUtilisation() %>% 
        mutate(Type_infra = recode(Type_infra, "Routeprim" = "Route Primaire","Routesecond" = "Route Secondaire","Routeterti" = "Route Tertiaire"))
      DataGraph <- DataGraph[tri_perso(DataGraph$Type_infra, couleurs$Nom),]
      plot_ly(DataGraph, labels = ~Type_infra, values = ~Utilisation, type = "pie",
              textposition = "inside", 
              textinfo = 'label+percent',
              insidetextfont = list(color = '#FFFFFF'),
              hoverinfo = 'text',
              text = ~paste0("<b> ",Type_infra," :</b><br>",Utilisation, "% de l'infrastructure"),
              marker = list(colors = couleurs$HEX_trait[match(DataGraph$Type_infra,couleurs$Nom)]),
              sort = FALSE,direction = "clockwise"
              ) %>%
        layout(title = "Utilisation de l'infrastructure",
               xaxis = list(title = "Type de route"),
               yaxis = list(title = 'Part (%)',
                            range = c(0,100)),
               showlegend = FALSE)
      
    })

    # Caractéristiques Infra ----
    InfraCaract_initiale <- Infra_usage %>% select(-Utilisation) %>% mutate(dont.tunnel = dont.tunnel * 100, dont.pont = dont.pont * 100)
    
    REACT_Config_InfraCaract <- reactiveVal(InfraCaract_initiale)
    
    output$DT_InfraCaract <- renderDT({
      
      datatable(REACT_Config_InfraCaract() %>% mutate(Type_infra = recode(Type_infra, "Routeprim" = "Route Primaire","Routesecond" = "Route Secondaire","Routeterti" = "Route Tertiaire")),
                colnames = c("Type de voie","Allocation (% du flux annuel)","Flux moyen journalier (véh./jour)","Nombre de voies","Proportion de tunnel (%)","Proportion de pont (%)"),
                editable = list(target = "cell",disable = list(columns = 1 )),
                class = "hover cell-border compact",
                caption = "Hypothèses de caractéristiques de l'infrastructure",
                selection = "none",
                options = list(dom = 't',
                               columnDefs = list(list(targets=c(0), visible=F),
                                                 list(targets=1:6, visible = T, width = 150)))) %>%
        formatPercentage(2, digits = 1) %>%
        formatCurrency(3," veh/jour",dec.mark = ",",mark = " ",before = FALSE, digits = 0) %>%
        formatCurrency(5:6,"%",dec.mark = ",",before = FALSE, digits = 2) %>%
        formatStyle(1, backgroundColor = styleEqual(couleurs$Nom, couleurs$HEX_aire), fontWeight = "bold")        
    })
    
    observeEvent(input[["DT_InfraCaract_cell_edit"]], {
      cell <- input[["DT_InfraCaract_cell_edit"]]
      newdf <- REACT_Config_InfraCaract()
      newdf[cell$row, cell$col] <- as.numeric(cell$value)
      REACT_Config_InfraCaract(newdf)
    })
    
    REACT_Modifications_Config_InfraCaract <- reactive({
      if( setequal(c(REACT_Config_InfraCaract()$Allocation),c(InfraCaract_initiale$Allocation)) == FALSE | 
          setequal(c(REACT_Config_InfraCaract()$flux.moyen.journalier),c(InfraCaract_initiale$flux.moyen.journalier)) == FALSE | 
          setequal(c(REACT_Config_InfraCaract()$nb.voies),c(InfraCaract_initiale$nb.voies)) == FALSE | 
          setequal(c(REACT_Config_InfraCaract()$dont.tunel),c(InfraCaract_initiale$dont.tunel)) == FALSE | 
          setequal(c(REACT_Config_InfraCaract()$dont.pont),c(InfraCaract_initiale$dont.pont)) == FALSE )
      {1} else {0}
    })
    
    observeEvent(input$button_reset_Config_InfraCaract, {
      
      newdf_InfraCaract <- InfraCaract_initiale
      REACT_Config_InfraCaract(newdf_InfraCaract)
      REACT_Config_InfraCaract_2(newdf_InfraCaract)                  
      
    })
    
    REACT_Config_InfraCaract_2 <- reactiveVal(InfraCaract_initiale)
    
    observeEvent(input$button_maj_Config_InfraCaract, {
      
      newdf_InfraCaract <- InfraCaract_initiale
      
      if( REACT_Modifications_Config_InfraCaract() != 0 ) {
        newdf_InfraCaract <- REACT_Config_InfraCaract()
      }
      
      REACT_Config_InfraCaract_2(newdf_InfraCaract)
      
    })

    # ---- _Météo ----
    # Temp ----
    Temp_initiale <- Temp 
    
    REACT_Config_Temp <- reactiveVal(Temp_initiale)
    
    output$DT_Temp <- renderDT({
      datatable(REACT_Config_Temp() %>% 
                  mutate(Month = recode(Month, "January"= "Janvier","February"="Février","March"="Mars","April"="Avril","May"="Mai","June"="Juin",
                                        "July"="Juillet","August"="Aout","September"="Septembre","October"="Octobre","November"="Novembre","December"="Décembre")),
                colnames = c('Mois','Températures °C','Humidités relatives'),
                editable = list(target = "cell",disable = list(columns = 1 )),
                class = "hover cell-border compact",
                caption = "Températures et humidités relatives",
                selection = "none",
                options = list(dom = 't', ordering=F,
                               columnDefs = list(list(targets=c(0), visible=F),
                                                 list(targets=c(1), visible=TRUE, width='150'),
                                                 list(targets=c(1), visible=TRUE, width='150')),
                               pageLength  = 12)) %>%
        formatCurrency(2," °C",dec.mark = ",",before = FALSE, digits = 1) %>%
        formatCurrency(3,"%",dec.mark = ",",before = FALSE, digits = 1) %>%
        formatStyle(1, fontWeight = "bold")         
    })
    
    observeEvent(input[["DT_Temp_cell_edit"]], {
      cell <- input[["DT_Temp_cell_edit"]]
      newdf <- REACT_Config_Temp()
      newdf[cell$row, cell$col] <- as.numeric(cell$value)
      REACT_Config_Temp(newdf)
    })
    
    REACT_Modifications_Config_Temp <- reactive({
      if( setequal(c(REACT_Config_Temp()$Temperature),c(Temp_initiale$Temperature)) == FALSE | 
          setequal(c(REACT_Config_Temp()$RH.),c(Temp_initiale$RH.)) == FALSE )
      {1} else {0}
    })
    
    
    observeEvent(input$button_reset_Config_Temp, {
      
      newdf_temp <- Temp_initiale
      REACT_Config_Temp(newdf_temp)
      REACT_Config_Temp_2(newdf_temp)                  
      
    })
    
    REACT_Config_Temp_2 <- reactiveVal(Temp_initiale)
    
    observeEvent(input$button_maj_Config_Temp, {
      
      newdf_temp <- Temp_initiale
      
      if( REACT_Modifications_Config_Temp() != 0 ) {
        newdf_temp <- REACT_Config_Temp()
      }
      
      REACT_Config_Temp_2(newdf_temp)
      
    })
    
    output$GRAPH_Synthese_Temp_ly <- renderPlotly({
      
      DataGraph <- REACT_Config_Temp_2()  %>%
        mutate(Mois = recode(Month, "January"= "Janvier","February"="Février","March"="Mars","April"="Avril","May"="Mai","June"="Juin",
                             "July"="Juillet","August"="Aout","September"="Septembre","October"="Octobre","November"="Novembre","December"="Décembre")) %>%
        mutate(Mois = fct_relevel(Mois, c("Janvier","Février","Mars","Avril","Mai","Juin","Juillet","Aout","Septembre","Octobre","Novembre","Décembre")))
      
      plot_ly(DataGraph, x = ~Mois, y = ~Temperature, type = "bar", name = "Température",
              marker = list(color = couleurs$HEX_trait[match(round(DataGraph$Temperature,0),couleurs$Id)]),
              hoverinfo = 'text',
              hovertext = ~paste0("<b>",Mois," :</b> <br>Temp. :",Temperature, "°C<br> Humidité : ",DataGraph$RH.,"%")) %>%
        add_trace(x = ~Mois, y = ~RH., type = "scatter",mode = "lines+markers", yaxis = "y2", name = "Humidité relative",
                  line = list(color = "black"), marker = list(color = "black"),
                  hoverinfo = "none") %>%
        layout(yaxis = list(range = c(min(c(min(DataGraph$Temperature),0)),max(c(max(DataGraph$Temperature),40))), ticksuffix = "°C",dtick = 10),
               yaxis2 = list(overlaying = "y", side = "right", range = c(0,100),dtick = 25, title = "Humidité relative",ticksuffix = "%"),
               showlegend = FALSE,
               margin = list(r = 50))
      
      
    })
    
    # ltrip ----
    Mobilite_initiale <- data.frame("ltrip" = ltrip)
    
    REACT_Config_Mobilite <- reactiveVal(Mobilite_initiale)
    
    output$DT_Mobilite <- renderDT({
      
      datatable(REACT_Config_Mobilite(),
                colnames = c("Longueur moyenne d'un déplacement (km)"),
                editable = "cell",
                class = "hover cell-border compact",
                caption = "",
                selection = "none",
                options = list(dom = 't',ordering=F,
                               columnDefs = list(list(targets=c(0), visible=F)))) %>%
        formatCurrency(1," km",dec.mark = ",",before = FALSE)
    })
    
    observeEvent(input[["DT_Mobilite_cell_edit"]], {
      cell <- input[["DT_Mobilite_cell_edit"]]
      newdf <- REACT_Config_Mobilite()
      newdf[cell$row, cell$col] <- as.numeric(cell$value)
      REACT_Config_Mobilite(newdf)
    })
    
    REACT_Modifications_Config_Mobilite <- reactive({
      if( setequal(c(REACT_Config_Mobilite()$hybrid_elec),c(Mobilite_initiale$hybrid_elec)) == FALSE )
      {1} else {0}
    })
    
    output$Visu_MoteurFroid <- renderText({

      DataGraph <- REACT_Config_Temp() %>%
        mutate(Month = recode(Month, "January"= "Janvier","February"="Février","March"="Mars","April"="Avril","May"="Mai","June"="Juin",
                              "July"="Juillet","August"="Aout","September"="Septembre","October"="Octobre","November"="Novembre","December"="Décembre"))

      MoteurFroid <- 0.6474 - 0.02545*REACT_Config_Mobilite()[1,1] - (0.00974 - 0.000385 * REACT_Config_Mobilite()[1,1]) * mean(REACT_Config_Temp_2()$Temperature)
        
      paste0("Pourcentage du trajet avec un moteur froid : \n ", round(max(c(0,MoteurFroid))*100,1), " %")

    })

    # ---- ____Calculs ACV ----
    # Dataframes intermédiaires ----
    
    # Parc_Utilisateur
    REACT_SuperParcUtilisateur <- reactive({
      
      if("Div" %in% names(REACT_Parc_utilisateur())){ # on a modifié le Parc_utilisation
        SuperParcUtilisateur <- REACT_Parc_utilisateur() %>% select(-Div) %>% 
          left_join(REACT_Config_MasseComplet_2(), by = c("Fuel","Segment")) %>%
          left_join(REACT_Config_DureeVieVehicule_2(), by = "Fuel") %>%
          mutate(DV_batterie = REACT_Config_DureeVieBatterie_2()[1,1]) %>%
          mutate(RedNOxEuro6 = 0) %>%
          select(Category,Fuel,Segment,Annee,Euro.Standard,Technology,Part,RedNOxEuro6,M_carros,M_moteur_ICE,M_moteur_Elec,M_batterie,DV_veh,DV_batterie)
        } 
      else{
        SuperParcUtilisateur <- REACT_Parc_utilisateur() %>% 
          left_join(REACT_Config_MasseComplet_2(), by = c("Fuel","Segment")) %>%
          left_join(REACT_Config_DureeVieVehicule_2(), by = "Fuel") %>%
          mutate(DV_batterie = REACT_Config_DureeVieBatterie_2()[1,1]) %>%
          mutate(RedNOxEuro6 = 0) %>%
          select(Category,Fuel,Segment,Annee,Euro.Standard,Technology,Part,RedNOxEuro6,M_carros,M_moteur_ICE,M_moteur_Elec,M_batterie,DV_veh,DV_batterie)
      }
      
      levels(SuperParcUtilisateur$Technology) <- c(levels(SuperParcUtilisateur$Technology),"")
      SuperParcUtilisateur$Technology[is.na(SuperParcUtilisateur$Technology)] <- ""
      
      SuperParcUtilisateur
      
    })
    
    output$VISU_ParcUtilisateur <- renderDT({
      
      datatable(REACT_SuperParcUtilisateur(),
                class = "hover cell-border compact"
      )
    })
    
    output$Verif_NouveauParc_utilisateur <- renderText({
      sum(REACT_SuperParcUtilisateur()$Part)
    })
    
    
    output$VISU_ParcUtilisateur <- renderDT({
      
      datatable(REACT_SuperParcUtilisateur(),
                class = "hover cell-border compact"
      )
    })
    
    # AC_usage
    REACT_SuperACUsage <- reactive({
      AC_usage %>% 
        select(-InstalledAC,-ACUsage) %>%
        left_join(REACT_Config_AC_2(), by = "Euro.Standard") %>%
        mutate(InstalledAC = InstalledAC / 100,
               ACUsage = ACUsage / 100,)
    })
    output$VISU_ACUsage <- renderDT({
      datatable(REACT_SuperACUsage(),
                class = "hover cell-border compact"
      )
    })
    
    
    # BioFuel
    REACT_SuperBioFuel <- reactive({
      SuperBioFuel <- REACT_Config_Carburants_2() %>% mutate(ratio = ratio / 100)
      row.names(SuperBioFuel) <- SuperBioFuel$BioFuel
      SuperBioFuel %>% select(-BioFuel)
    })
    output$VISU_BioFuel <- renderDT({
      datatable(REACT_SuperBioFuel(),
                class = "hover cell-border compact",
                options = list(dom = 't',
                               pageLength = 30)
      )
    })
    
    # Mix_elec
    REACT_SuperMix_elec <- reactive({
      a <- REACT_Config_MixElect_2() %>% 
        rename(Source = Energie,
               ratio = Part)

      b <- REACT_Config_PerteEnergie_2() %>%
        rename(Source = Tension,
               ratio = Perte)
      
      SuperMix_elec <- rbind(a,b) %>% mutate(ratio = ratio / 100)
      row.names(SuperMix_elec) <- SuperMix_elec$Source
      SuperMix_elec %>% select(-Source)
      
    })
    output$VISU_Mix_elec <- renderDT({
      datatable(REACT_SuperMix_elec(),
                class = "hover cell-border compact",
                options = list(dom = 't',
                               pageLength = 30)
      )
    })

    # Infra_usage
    REACT_SuperInfra_usage <- reactive({
      a <- REACT_Config_InfraUtilisation() %>% mutate(Utilisation = Utilisation / 100)
      
      b <- REACT_Config_InfraCaract_2() %>% mutate(dont.tunnel = dont.tunnel / 100, dont.pont = dont.pont / 100)
      
      SuperInfra_usage <- left_join(a,b, by = "Type_infra") 
      row.names(SuperInfra_usage) <- SuperInfra_usage$Type_infra
      
      SuperInfra_usage
      
    })
    output$VISU_Infra_usage <- renderDT({
      datatable(REACT_SuperInfra_usage(),
                class = "hover cell-border compact",
                options = list(dom = 't',
                               pageLength = 30)
      )
    })
    
    # Temp
    REACT_SuperTemp <- reactive({
      REACT_Config_Temp_2()
    })
    output$VISU_Temp <- renderDT({
      datatable(REACT_SuperTemp(),
                class = "hover cell-border compact",
                options = list(dom = 't',
                               pageLength = 30)
      )
    })
    
    # ltrip = 6.2, conso_elec = 0.2,hybrid_elec = 0.5
    REACT_Super_ltrip <- reactive({
      REACT_Config_Mobilite()[1,1]
    })
    output$VISU_ltrip <- renderText({
      REACT_Super_ltrip()
    })
    
    REACT_Super_conso_elec <- reactive({
      REACT_Config_ConsoHybride_2()[1,1]
    })
    output$VISU_conso_elec <- renderText({
      REACT_Super_conso_elec()
    })
    
    REACT_Super_hybrid_elec <- reactive({
      REACT_Config_ConsoHybride_2()[1,2]
    })
    output$VISU_hybrid_elec <- renderText({
      REACT_Super_hybrid_elec()
    })

    # Download config complète ----
    output$download_Config <- downloadHandler(

      filename = function() { "ModEm_MaConfiguration.xlsx"},
      content = function(file) {write_xlsx(list("Parc" = REACT_SuperParcUtilisateur(), 
                                                "Infra" = REACT_SuperInfra_usage(),
                                                "Elec" = REACT_SuperMix_elec() %>% mutate(Part = rownames(REACT_SuperMix_elec())) %>% select(Part,ratio),
                                                "BioFuel" = REACT_SuperBioFuel() %>% mutate(Fuel = rownames(REACT_SuperBioFuel())) %>% select(Fuel,ratio),
                                                "Clim" = REACT_SuperACUsage(),
                                                "Temp" = REACT_SuperTemp()), 
                                           path = file)} 
    )
    
    # Upload Config complète ----
    REACT_Upload_Config_Parc <- reactive({
      req(input$upload_Config)
      inFile <- input$upload_Config
      Parc_utilisateur <- as.data.frame(read_excel(inFile$datapath, 1))
      levels(Parc_utilisateur$Technology) <- c(levels(Parc_utilisateur$Technology),"")
      Parc_utilisateur$Technology[is.na(Parc_utilisateur$Technology)] <- ""
      Parc_utilisateur
    })
    
    REACT_Upload_Config_Infra <- reactive({
      req(input$upload_Config)
      inFile <- input$upload_Config
      Infra_usage <- as.data.frame(read_excel(inFile$datapath, 2))
      rownames(Infra_usage) <- Infra_usage$Type_infra
      Infra_usage
    })
    
    REACT_Upload_Config_Elec <- reactive({
      req(input$upload_Config)
      inFile <- input$upload_Config
      Mix_elec <- as.data.frame(read_excel(inFile$datapath, 3))
      Mix_elec <- data.frame(ratio = Mix_elec[,2], row.names = Mix_elec[,1])
      Mix_elec
    })
    
    REACT_Upload_Config_BioFuel <- reactive({
      req(input$upload_Config)
      inFile <- input$upload_Config
      BioFuel <- as.data.frame(read_excel(inFile$datapath, 4))
      BioFuel <- data.frame(ratio = BioFuel[,2], row.names = BioFuel[,1])
      BioFuel
    })
    
    REACT_Upload_Config_Clim <- reactive({
      req(input$upload_Config)
      inFile <- as.data.frame(input$upload_Config)
      ACUsage <- read_excel(inFile$datapath, 5)
      names(ACUsage) <- c("Category","Fuel","Segment","Euro.Standard","InstalledAC","ACUsage")
      ACUsage
    })
    
    REACT_Upload_Config_Temp <- reactive({
      req(input$upload_Config)
      inFile <- as.data.frame(input$upload_Config)
      Temp <- read_excel(inFile$datapath, 6)
      rownames(Temp) <- Temp$Month
      Temp
    })
    
    output$VISU_ParcUtilisateur_upload <- renderDT({
      datatable(REACT_Upload_Config_Parc(),
                class = "hover cell-border compact",
                options = list(pageLength = 10)
      )
    })
    output$VISU_ACUsage_upload <- renderDT({
      datatable(REACT_Upload_Config_Clim(),
                class = "hover cell-border compact",
                options = list(pageLength = 10)
      )
    })
    output$VISU_BioFuel_upload <- renderDT({
      datatable(REACT_Upload_Config_BioFuel(),
                class = "hover cell-border compact",
                options = list(dom = 't',
                               pageLength = 10)
      )
    })
    output$VISU_Mix_elec_upload <- renderDT({
      datatable(REACT_Upload_Config_Elec(),
                class = "hover cell-border compact",
                options = list(dom = 't',
                               pageLength = 30)
      )
    })
    output$VISU_Infra_usage_upload <- renderDT({
      datatable(REACT_Upload_Config_Infra(),
                class = "hover cell-border compact",
                options = list(dom = 't',
                               pageLength = 10)
      )
    })
    output$VISU_Temp_upload <- renderDT({
      datatable(REACT_Upload_Config_Temp(),
                class = "hover cell-border compact",
                options = list(dom = 't',
                               pageLength = 30)
      )
    })

    # Lancement calcul ----
    REACT_liste_impacts_ACV <- reactive({
      if("Dommages pour la santé humaine et la biodiversité" %in% input$Input_LancementCalcul_Indicateurs) {
        liste_impacts_ACV  %>%
          filter(Indicators %in% input$Input_LancementCalcul_Indicateurs[-length(input$Input_LancementCalcul_Indicateurs)]) %>%
          rbind(liste_impacts_ACV[which(liste_impacts_ACV$Type %in% c("Santé humaine","Atteinte à la biodiversité")),])
      } else {
        liste_impacts_ACV  %>%
          filter(Indicators %in% input$Input_LancementCalcul_Indicateurs)
      }
    })
    
    REACT_VISU_liste_impacts_ACV <- reactive({
      
      if("Dommages pour la santé humaine et la biodiversité" %in% input$Input_LancementCalcul_Indicateurs) {
        liste_impacts_ACV  %>% 
          filter(Indicators %in% input$Input_LancementCalcul_Indicateurs[-length(input$Input_LancementCalcul_Indicateurs)]) %>%
          rbind(liste_impacts_ACV[match(c("Sante","Biodiv"),liste_impacts_ACV$Abrev),])
      } else {
        liste_impacts_ACV  %>% 
          filter(Indicators %in% input$Input_LancementCalcul_Indicateurs)
      }
      
    })    
    
    output$DT_IndicateursSelectionnes <- renderDT({
      datatable(REACT_VISU_liste_impacts_ACV() %>% 
                  select(Type,Indicators,Units,Methods),
                class = "hover cell-border compact",
                caption = "Indicateurs sélectionnés :",
                selection = "none",
                colnames = c("Groupe d'indicateurs","Indicateurs","Unité","Méthode"),
                options = list(dom = 't',ordering=F,
                               pageLength = 30),
                rownames = F
      )
    })
    
    REACT_list_calculs <- reactiveValues(nom = vector(mode = "character"))
    REACT_list_config <- reactiveValues()
    REACT_list_Impacts_parc <- reactiveValues()
    REACT_list_direct <- reactiveValues()
    REACT_Emis_conso_parc <- reactiveValues()
    
    observeEvent(input$button_lancementCalcul,{
      
      showModal(modalDialog('Calcul en cours ...', footer = NULL))
      if(input$Input_Configurateur_UploadConfig == "ConfigDefaut") {
        Config <- list(Parc_utilisateur = REACT_SuperParcUtilisateur() %>% filter(Part != 0),
                       Temp = REACT_SuperTemp(),
                       BioFuel = REACT_SuperBioFuel(),
                       AC_usage = REACT_SuperACUsage(),
                       Infra_usage = REACT_SuperInfra_usage(),
                       Mix_elec = REACT_SuperMix_elec(),
                       conso_elec = REACT_Super_conso_elec(),
                       hybrid_elec = REACT_Super_hybrid_elec(),
                       ltrip = REACT_Super_ltrip(),
                       liste_impacts_ACV = REACT_VISU_liste_impacts_ACV()) 
      } else if(input$Input_Configurateur_UploadConfig == "ConfigUpload") {
        Config <- list(Parc_utilisateur = REACT_Upload_Config_Parc() %>% filter(Part != 0),
                       Temp = REACT_Upload_Config_Temp(),
                       BioFuel = REACT_Upload_Config_BioFuel(),
                       AC_usage = REACT_Upload_Config_Clim(),
                       Infra_usage = REACT_Upload_Config_Infra(),
                       Mix_elec = REACT_Upload_Config_Elec(),
                       conso_elec = REACT_Super_conso_elec(),
                       hybrid_elec = REACT_Super_hybrid_elec(),
                       ltrip = REACT_Super_ltrip(),
                       liste_impacts_ACV = REACT_VISU_liste_impacts_ACV()) 
      }
      
      Emis_conso <- Ajout_elec(Config[["conso_elec"]],Config[["hybrid_elec"]],Config[["ltrip"]],Config[["Temp"]],Config[["BioFuel"]],Config[["AC_usage"]])													
      Emis_conso_unitaire <- Copert_Parc(Config[["Parc_utilisateur"]], Emis_conso)
      Impacts_unitaire <- Impacts_total(Config[["Parc_utilisateur"]],Config[["Infra_usage"]],Config[["Mix_elec"]],Emis_conso_unitaire,REACT_liste_impacts_ACV(),input$Input_detail_ACV)
      Emis_conso_parc <- Config[["Parc_utilisateur"]]$Part*Emis_conso_unitaire
      Impacts_parc <- lapply(Impacts_unitaire,function(i) Config[["Parc_utilisateur"]]$Part*i)
      
      if("carburants" %in% input$Input_LancementCalcul_GazCarburants){
        tri_direct_parc <- tri_gaz_carb(Emis_conso_parc,
                                        list_gaz = input$Input_LancementCalcul_GazCarburants[input$Input_LancementCalcul_GazCarburants != "carburants"],
                                        list_fuel = liste_carburant)
      } else{
        tri_direct_parc <- tri_gaz_carb(Emis_conso_parc,
                                        list_gaz = input$Input_LancementCalcul_GazCarburants[input$Input_LancementCalcul_GazCarburants != "carburants"],
                                        list_fuel = c())
      }
      REACT_list_calculs$nom <- c(REACT_list_calculs$nom,input$nom_calcul)
      REACT_list_config[[input$nom_calcul]] <- Config
      REACT_list_Impacts_parc[[input$nom_calcul]] <- Impacts_parc
      REACT_list_direct[[input$nom_calcul]] <- tri_direct_parc
      REACT_Emis_conso_parc[[input$nom_calcul]] <- Emis_conso_parc
      removeModal()
    })
    
    observeEvent(input$button_supprimer_calcul,{
      REACT_list_calculs$nom <- REACT_list_calculs$nom[REACT_list_calculs$nom != input$calcul_suppr]
      REACT_list_config[[input$calcul_suppr]] <- NULL
      REACT_list_Impacts_parc[[input$calcul_suppr]] <- NULL
      REACT_list_direct[[input$calcul_suppr]] <- NULL
      REACT_Emis_conso_parc[[input$nom_calcul]] <- NULL
    })
    
    output$DT_liste_calculs <- renderDT({ datatable(data.frame(Calculs = REACT_list_calculs$nom),
                                                    selection = "none",
                                                    options = list(dom = 't',ordering=F,pageLength = 30,
                                                                   language = list(emptyTable = 'Aucun calcul réalisé')),
                                                    rownames = F) })
    
    # ---- ____Analyses ---- 
    
    # Vérifs ####
    output$VISU_Emis_conso <- renderDT({
      datatable(REACT_Emis_conso()[,,1],
                class = "hover cell-border compact"
      )
    })
    
    output$VISU_Emis_conso_unitaire <- renderDT({
      datatable(REACT_Emis_conso_unitaire()[1,,],
                class = "hover cell-border compact"
      )
    })
    
    output$VISU_Impacts_unitaire <- renderDT({
      datatable(REACT_Impacts_unitaire()[1,1,,],
                class = "hover cell-border compact"
      )
    })
    
    output$VISU_Emis_conso_parc <- renderDT({
      datatable(REACT_Emis_conso_parc()[1,,],
                class = "hover cell-border compact"
      )
    })
    
    output$VISU_Impacts_parc <- renderDT({
      datatable(REACT_Impacts_parc()[1,1,,],
                class = "hover cell-border compact"
      )
    })
    
    # tri_direct
    output$VISU_tri_direct_unitaire <- renderDT({
      datatable(REACT_tri_direct_unitaire()[,1,],
                class = "hover cell-border compact"
      )
    })
    output$VISU_tri_direct_parc <- renderDT({
      datatable(REACT_tri_direct_parc()[,1,],
                class = "hover cell-border compact"
      )
    })

    # Synthese ####
    output$Synthese_choix_calcul = renderUI({
      pickerInput(
        inputId = "Input_Analyses_Synthese_ChoixConfig",
        label = "Sélection du calcul :",
        width = 400,
        choices = REACT_list_calculs$nom,
        options = list(
          style = "btn-danger",
          pickerOptions(noneSelectedText = "Aucun calcul"))
      )
      
    })
    output$Synthese_vitessereseau = renderText({
      if(is.null(input$Input_Analyses_Synthese_ChoixConfig)) {""} else {
        paste("Estimation de la vitesse du réseau : ",
              vitesse_reseau(REACT_list_config[[input$Input_Analyses_Synthese_ChoixConfig]]),
              " km/h")
      }
      
    })
    Synthese_Export <- reactiveValues(Export = 0,table = 0)
    output$Synthese_Tableau = renderDT({
      
      df <- Synthese_Export$Export
      if(is.null(dim(df))) {datatable(matrix("pas de calcul réalisé",nrow = 1,ncol = 1))} 
      else {
        colnames(df) <- df[1,]
        df <- df[-1,]
        Indicateurs <- liste_impacts_ACV[rownames(df),"Indicators"]
        Unités <- liste_impacts_ACV[rownames(df),"Units"]
        data <- matrix(gsub("[.]",",",gsub("e","E",signif(as.numeric(as.matrix(df)),5))),nrow = nrow(df),ncol = ncol(df),dimnames = list(Indicateurs,names(df)))
        Synthese_Export$table <- as.data.frame(cbind(Indicateurs,Unités,data))
        for(i in 1:nrow(df)) {
          df[i,] <- paste(gsub("e","E",signif(as.numeric(df[i,]),3)),liste_impacts_ACV[rownames(df)[i],"Units"])
          next
        }
        rownames(df) <- liste_impacts_ACV[rownames(df),"Indicators"]
        datatable(df,
                  class = "hover cell-border compact",
                  selection = "none",
                  colnames = names(df),
                  options = list(dom = 't',ordering=F,pageLength = 30,
                                 columnDefs = list(
                                                   list(targets=c(0), width='175'),
                                                   list(targets=c(1:ncol(df)), visible=TRUE, width='100'))))
      }
      
    })
    output$GRAPH_Synthese <- renderPlotly({
      aggreg <- input$Input_Analyses_Synthese_decomposition
      choix_vitesse <- input$Input_Analyses_Synthese_ChoixVitesses
      vitesse <-input$Input_Analyses_synthese_vitesse
      config <- input$Input_Analyses_Synthese_ChoixConfig  
      
      if(is.null(config)) {
        fig <- plotly_empty(type = "pie")
        fig <- fig %>% layout(title = "Aucun calcul réalisé")
        fig
      } else {
        Data_Impacts_parc <- REACT_list_Impacts_parc[[config]][["Total"]] %>% 
          multi_select(REACT_list_config[[config]][["liste_impacts_ACV"]]$Abrev,'+',3)
        Data_Parc_Utilisateur <- REACT_list_config[[config]][["Parc_utilisateur"]]
        if(choix_vitesse == "perso") {
          data_vit <- Data_Impacts_parc[,as.character(vitesse),,-1]
        } else if(choix_vitesse == "reseau") {
          usage_reseau <- REACT_list_config[[config]][["Infra_usage"]]$Utilisation
          vitesses_reseau <- apply(distrib_vitesse,1,function(i) sum(i*usage_reseau))
          data_vit <- apply(Data_Impacts_parc[,,,-1], MARGIN = c(1,3,4), function(i) sum(i*vitesses_reseau))
        } else {
          data_vit <- apply(Data_Impacts_parc[,,,-1], MARGIN = c(1,3,4), function(i) sum(i*distrib_vitesse[,choix_vitesse]))
        }
        
        # Formatage de la données pour graph plotly
        if(aggreg == "ACV") {
          data <- t(apply(data_vit,2:3,sum))
        } else {
          if(aggreg == "Moto") {
            data <- Aggreg_veh(apply(data_vit,1:2,sum),list(Data_Parc_Utilisateur$Fuel),sum)
          }
          if(aggreg == "Euro") {
            data <- Aggreg_veh(apply(data_vit,1:2,sum),list(Data_Parc_Utilisateur$Euro.Standard),sum)
          }
          if(aggreg == "Segment") {
            data <- Aggreg_veh(apply(data_vit,1:2,sum),list(Data_Parc_Utilisateur$Segment),sum)
          }
        }
        data <- data.frame(Categories = rownames(data),data)
        data <- data %>% mutate(Categories = renames_all(data$Categories))
        data <- data[tri_perso(data$Categories,couleurs$Nom),]
        temp_color <- couleurs$HEX_trait[match(data$Categories, couleurs$Nom)]
        all_buttons <- list()
        for(i in 2:ncol(data)) {
          all_buttons[[i-1]] <- list(method = "restyle",
                                     args = list(list(values = list(data[,i]), text = list(paste(gsub("e","E",signif(data[,i],3)),liste_impacts_ACV[as.character(names(data)[i]),"Units"])))),
                                     label = liste_impacts_ACV[as.character(names(data)[i]),"Indicators"])
          next
        }
        # Graphique plotly
        fig <- plot_ly(data, labels = data$Categories, values = data[,2], text = paste(gsub("e","E",signif(data[,2],3)),liste_impacts_ACV[as.character(names(data)[2]),"Units"]),
                       type = 'pie',marker = list(colors = temp_color), 
                       sort = FALSE,direction = "clockwise",
                       textposition = "top right", 
                       textinfo = 'label+percent',
                       hovertemplate = "<b>%{label} :</b><br> <i>%{text}</i><br> %{percent} <extra></extra>")
        fig <- fig %>% layout(
          xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
          yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
          updatemenus = list(list(buttons=all_buttons, xanchor = "center",x=0.5,y=1.1, yanchor = "top",bgcolor = "white",font=list(size=16))),
          paper_bgcolor = "#ecf0f5",
          showlegend=F)
        Synthese_Export$Export = rbind(c(data[,1],"Total"),data.frame(t(data[,-1]), Total = colSums(data[,-1])))
        fig
      }
    })
    output$download_Synthese <- downloadHandler(
      filename = "resultats_modemacv.xlsx",
      content = function(file) {write_xlsx(list("Résumé" = data.frame(`Export Modem_ACV` = c("Date","Type d'analyse","Nom du calcul"),
                                                                      Info = c(as.character(Sys.time()),"Synthèse",input$Input_Analyses_Synthese_ChoixConfig )),
                                                "Résultats" = Synthese_Export$table),path = file)} 
    )
    # MonoVitesse ####
    output$MonoVitesse_choix_calcul = renderUI({
      pickerInput(
        inputId = "Input_Analyses_MonoVitesse_ChoixConfig",
        label = "Sélection du calcul :",
        width = 400,
        choices = REACT_list_calculs$nom,
        options = list(
          style = "btn-danger",
          pickerOptions(noneSelectedText = "Aucun calcul"))
      )
      
    })
    output$MonoVitesse_Indicateurs = renderUI({
      
      pickerInput(
        inputId = "Input_Analyses_MonoVitesse_Indicateurs",
        label = "Indicateur observé :",
        width = 400,
        choices = as.character(liste_impacts_ACV[dimnames(REACT_list_Impacts_parc[[input$Input_Analyses_MonoVitesse_ChoixConfig]][["Total"]] %>%
                                                            multi_select(c("Biodiv_GWP","Biodiv_Ion","Biodiv_Ecotox","Biodiv_Eutro","Biodiv_Acid","Biodiv_Sol","Biodiv_Eau",
                                                                           "Sante_GWP","Sante_Ion","Sante_PMF","Sante_POF","Sante_Ozon","Sante_Htox","Sante_Eau"),'-',3))[[3]],"Indicators"]) ,
        options = list(
          style = "btn-primary",
          pickerOptions(noneSelectedText = "Aucun calcul"))
      )
      
    })
    
    MonoVitesse_Export <- reactiveValues(Export = 0)
    output$GRAPH_MonoVitesse <- renderPlotly({
      
      # input
      config <- input$Input_Analyses_MonoVitesse_ChoixConfig  
      phaseACV_TRUE <- input$Input_Analyses_MonoVitesse_phaseACV_TRUE
      MotoEuroSegment <- input$Input_Analyses_MonoVitesse_MotoEuroSegment
      
      impact <- as.character(liste_impacts_ACV[which(liste_impacts_ACV$Indicators == input$Input_Analyses_MonoVitesse_Indicateurs ), "Abrev"])
      impacts_unite <- as.character(liste_impacts_ACV[which(liste_impacts_ACV$Indicators == input$Input_Analyses_MonoVitesse_Indicateurs ), "Unit_D3"])
      
      vitesses <- 5:130
      
      # Preparation data
      if(is.null(config)) {
        fig <- plotly_empty(type = "bar")
        fig <- fig %>% layout(title = "Aucun calcul réalisé")
        fig
      } else {
        Data_Impacts_parc <- REACT_list_Impacts_parc[[config]][["Total"]] 
        Data_Parc_Utilisateur <- REACT_list_config[[config]][["Parc_utilisateur"]]
        
        data_vit <- Data_Impacts_parc[,,impact,-1]
        
        if(phaseACV_TRUE) {
          data <- data.frame(vitesses,apply(data_vit,2:3,sum))
        } else {
          list_aggreg <- list()
          if("Moto" %in% MotoEuroSegment) {
            list_aggreg <- c(list_aggreg,list(Data_Parc_Utilisateur$Fuel))
          }
          if("Euro" %in% MotoEuroSegment) {
            list_aggreg <- c(list_aggreg,list(Data_Parc_Utilisateur$Euro.Standard))
          }
          if("Segment" %in% MotoEuroSegment) {
            list_aggreg <- c(list_aggreg,list(Data_Parc_Utilisateur$Segment))
          }
          data <- data.frame(vitesses,t(Aggreg_veh(apply(data_vit,1:2,sum),list_aggreg, sum)))
        }
        colnames(data) <- renames_all(colnames(data))
        data <- data[,c(1,1+tri_perso(colnames(data)[-1],couleurs$Nom))]
        temp_color <- couleurs$HEX_aire[match(colnames(data)[-1], couleurs$Nom)]
        temp_unit <- liste_impacts_ACV$Units[impact]
        # Graphique (absolu et relatif)
        fig <- plot_ly(data, x = ~vitesses, y = signif(data[,2],3), name = names(data)[2], 
                       fillcolor = temp_color[1], text = paste(gsub("e","E",signif(data[,2],3)),impacts_unite),customdata = rep(as.character(colnames(data)[2]),126),
                       type = 'scatter', mode = 'none', stackgroup = 'one',
                       hovertemplate = paste('<b>%{customdata}</b>',
                                             '<br><i>Vit </i>: %{x} km/h',
                                             '<br>%{text} <extra></extra>'))
        for(i in 3:ncol(data)) {
          fig <- fig %>% add_trace(y = signif(data[,i],3), name = names(data)[i],text = paste(gsub("e","E",signif(data[,i],3)),impacts_unite),
                                   fillcolor = temp_color[i-1],customdata = rep(as.character(colnames(data)[i]),126),
                                   hovertemplate = paste('<b>%{customdata}</b>',
                                                         '<br><i>Vit </i>: %{x} km/h',
                                                         '<br>%{text} <extra></extra>'))
          next
        }
        fig <- fig %>% layout(
          title = paste('Impacts en fonction de la vitesse','<br><i>',as.character(input$Input_Analyses_MonoVitesse_Indicateurs),'</i>'),
          plot_bgcolor = "#d9d9d9",
          margin = list(t = 65),
          xaxis = list(title = 'vitesses (km/h)',rangemode = 'tozero',gridcolor = "#b4b4b4", dtick = 10),
          yaxis = list(title = impacts_unite,gridcolor = "b4b4b4",exponentformat = "E",minexponent = 3),
          hoverlabel = list(font = list(color = "black")),
          updatemenus = list(
            list(
              buttons = list(
                list(method = 'restyle',
                     args = list('groupnorm',''),
                     label = 'Absolu'),
                
                list(method = 'restyle',
                     args = list('groupnorm','percent'),
                     label = 'Relatif')			
              )
            )
          )
        )
        MonoVitesse_Export$Export = data.frame(data, Total = rowSums(data[,-1]))
        fig
      }
    })
    output$download_MonoVitesse <- downloadHandler(
      filename = "resultats_modemacv.xlsx",
      content = function(file) {write_xlsx(list("Résumé" = data.frame(`Export Modem_ACV` = c("Date","Type d'analyse","Nom du calcul","Indicateur","Unité"),
                                                                      Info = c(as.character(Sys.time()),"Impacts en fonction de la vitesse",input$Input_Analyses_MonoVitesse_ChoixConfig,
                                                                               as.character(input$Input_Analyses_MonoVitesse_Indicateurs),as.character(liste_impacts_ACV[which(liste_impacts_ACV$Indicators == input$Input_Analyses_MonoVitesse_Indicateurs ), "Units"]))),
                                                "Résultats" = MonoVitesse_Export$Export),path = file)} 
    )
    # MonoCompaVehicules ####
    output$MonoCompaVehicules_choix_calcul = renderUI({
      pickerInput(
        inputId = "Input_Analyses_MonoCompaVehicules_ChoixConfig",
        label = "Sélection du calcul :",
        width = 400,
        choices = REACT_list_calculs$nom,
        options = list(
          style = "btn-danger",
          pickerOptions(noneSelectedText = "Aucun calcul"))
      )
      
    })
    # MonoCompaVehicules - Vitesse ####
    output$MonoCompaVehicules_Vitesse_Indicateurs = renderUI({
      
      pickerInput(
        inputId = "Input_Analyses_MonoCompaVehicules_Vitesse_Indicateurs",
        label = "Indicateur observé :",
        width = 400,
        choices = as.character(liste_impacts_ACV[dimnames(REACT_list_Impacts_parc[[input$Input_Analyses_MonoCompaVehicules_ChoixConfig]][["Total"]] %>%
                                                            multi_select(c("Biodiv_GWP","Biodiv_Ion","Biodiv_Ecotox","Biodiv_Eutro","Biodiv_Acid","Biodiv_Sol","Biodiv_Eau",
                                                                           "Sante_GWP","Sante_Ion","Sante_PMF","Sante_POF","Sante_Ozon","Sante_Htox","Sante_Eau"),'-',3))[[3]],"Indicators"]) ,
        options = list(
          style = "btn-primary",
          pickerOptions(noneSelectedText = "Aucun calcul"))
      )
      
    })
    
    MonoCompaVehicules_Vitesse_Export <- reactiveValues(Export = 0)
    output$GRAPH_MonoCompaVehicules_Vitesse <- renderPlotly({
      
      # input
      config <- input$Input_Analyses_MonoCompaVehicules_ChoixConfig  
      impact <- as.character(liste_impacts_ACV[which(liste_impacts_ACV$Indicators == input$Input_Analyses_MonoCompaVehicules_Vitesse_Indicateurs ), "Abrev"])
      impacts_unite <- as.character(liste_impacts_ACV[which(liste_impacts_ACV$Indicators == input$Input_Analyses_MonoCompaVehicules_Vitesse_Indicateurs ), "Unit_D3"])
      aggreg <- as.character(input$Input_Analyses_MonoCompaVehicules_Vitesse_MotoEuroSegment)
      vitesses <- 5:130
      
      # Preparation data
      if(is.null(config)) {
        fig <- plotly_empty(type = "bar")
        fig <- fig %>% layout(title = "Aucun calcul réalisé")
        fig
      } else {
        Data_Impacts_parc <- REACT_list_Impacts_parc[[config]][["Total"]] %>% 
          multi_select(REACT_list_config[[config]][["liste_impacts_ACV"]]$Abrev,'+',3)
        Data_Parc_Utilisateur <- REACT_list_config[[config]][["Parc_utilisateur"]]
        
        data_vit <- Data_Impacts_parc[,,impact,-1]
        
        list_aggreg <- list()
        if("Moto" %in% aggreg) {
          list_aggreg <- c(list_aggreg,list(Data_Parc_Utilisateur$Fuel))
        }
        if("Euro" %in% aggreg) {
          list_aggreg <- c(list_aggreg,list(Data_Parc_Utilisateur$Euro.Standard))
        }
        if("Segment" %in% aggreg) {
          list_aggreg <- c(list_aggreg,list(Data_Parc_Utilisateur$Segment))
        }
        
        data <- data.frame(vitesses,t(Aggreg_veh(apply(data_vit,1:2,sum),list_aggreg, sum)))
        part_parc <- aggregate(Data_Parc_Utilisateur$Part, list_aggreg, sum)[which(aggregate(Data_Parc_Utilisateur$Part, list_aggreg, sum)[,length(list_aggreg)+1] != 0),length(list_aggreg)+1]
        data <- data.frame(vitesses,t(t(data[,-1])/part_parc))
        colnames(data) <- renames_all(colnames(data))
        data <- data[,c(1,1+tri_perso(colnames(data)[-1],couleurs$Nom))]
        temp_color <- couleurs$HEX_trait[match(colnames(data)[-1], couleurs$Nom)]
        temp_dash <- rep("solid", ncol(data)-1)
        if(length(aggreg) >= 2 & "Segment" %in% aggreg) {
          dash_pattern <- c("dot","dash","solid","longdashdot")
          names(dash_pattern) <- c("Mini","Petit","Medium","Large")
          if(length(aggreg) == 3){
            data <- data[,c(1,1+tri_perso(paste(sapply(strsplit(colnames(data)[-1],split = "/"),"[",1),sapply(strsplit(colnames(data)[-1],split = "/"),"[",2),sep="/"),couleurs$Nom))]
            temp_color <- couleurs$HEX_trait[match(paste(sapply(strsplit(colnames(data)[-1],split = "/"),"[",1),sapply(strsplit(colnames(data)[-1],split = "/"),"[",2),sep="/"),couleurs$Nom)]
            temp_dash <- dash_pattern[sapply(strsplit(colnames(data)[-1],split = "/"),"[",3)]
          } else {
            data <- data[,c(1,1+tri_perso(sapply(strsplit(colnames(data)[-1],split = "/"),"[",1),couleurs$Nom))]
            temp_color <- couleurs$HEX_trait[match(sapply(strsplit(colnames(data)[-1],split = "/"),"[",1),couleurs$Nom)]
            temp_dash <- dash_pattern[sapply(strsplit(colnames(data)[-1],split = "/"),"[",2)]
          }
          
        }
        # Graphique
        fig <- plot_ly(data, x = ~vitesses, y = data[,2], name = names(data)[2],customdata = rep(as.character(colnames(data)[2]),126),
                       line = list(color = temp_color[1], width = 3, dash = temp_dash[1]),
                       text = paste(gsub("e","E",signif(data[,2],3)),impacts_unite),
                       type = 'scatter', mode = 'lines',
                       hovertemplate = paste('<b>%{customdata}</b>','<br><i>Vit </i>: %{x} km/h',
                                             '<br>%{text} <extra></extra>'),
                       hoverlabel = list(bgcolor=paste(temp_color[1],"80",sep="")))
        for(i in 3:ncol(data)) {
          fig <- fig %>% add_trace(y = data[,i], name = names(data)[i],
                                   customdata = rep(as.character(colnames(data)[i]),126),text = paste(gsub("e","E",signif(data[,i],3)),impacts_unite),
                                   line = list(color = temp_color[i-1], width = 3, dash = temp_dash[i-1]),
                                   hovertemplate = paste('<b>%{customdata}</b>',
                                                                                 '<br><i>Vit </i>: %{x} km/h',
                                                                                 '<br>%{text} <extra></extra>'),
                                   hoverlabel = list(bgcolor=paste(temp_color[i-1],"80",sep="")))
          
          next
        }
        data <- data.frame(data, Parc = apply(data_vit,2,sum))
        fig <- fig %>% add_trace(y = data$Parc, name = "Moyenne parc", line = list(color = "black", width =5, dash = "solid"),
                                 customdata = rep("Moyenne parc",126),text = paste(gsub("e","E",signif(data[,"Parc"],3)),impacts_unite),
                                 hovertemplate = paste('<b>%{customdata}</b>',
                                                       '<br><i>Vit </i>: %{x} km/h',
                                                       '<br>%{text} <extra></extra>'),
                                 hoverlabel = list(bgcolor="white"))
        fig <- fig %>% layout(
          title = paste('Impacts en fonction de la vitesse','<br><i>',as.character(input$Input_Analyses_MonoCompaVehicules_Vitesse_Indicateurs),'</i>'),
          xaxis = list(title = 'vitesses (km/h)',rangemode = 'tozero',gridcolor = "#b4b4b4",dtick = 10),
          yaxis = list(rangemode = 'tozero',title = impacts_unite,gridcolor = "b4b4b4",exponentformat = "E",minexponent = 3),
          hoverlabel = list(font = list(color = "black")),
          margin = list(t = 65),
          plot_bgcolor = "#d9d9d9"
        )
        
        MonoCompaVehicules_Vitesse_Export$Export = data.frame(data)
        fig
      }
      
    })
    output$download_MonoCompaVehicules_Vitesse <- downloadHandler(
      filename = "resultats_modemacv.xlsx",
      content = function(file) {write_xlsx(list("Résumé" = data.frame(`Export Modem_ACV` = c("Date","Type d'analyse","Nom du calcul","Indicateur","Unité"),
                                                                      Info = c(as.character(Sys.time()),"Analyse par véhicule en fonction de la vitesse",input$Input_Analyses_MonoCompaVehicules_ChoixConfig,
                                                                               as.character(input$Input_Analyses_MonoCompaVehicules_Vitesse_Indicateurs),as.character(liste_impacts_ACV[which(liste_impacts_ACV$Indicators == input$Input_Analyses_MonoCompaVehicules_Vitesse_Indicateurs ), "Units"]))),
                                                "Résultats" = MonoCompaVehicules_Vitesse_Export$Export),path = file)} 
    )
    # MonoCompaVehicules - phases ACV ####
    output$MonoCompaVehicules_PhasesACV_Indicateurs = renderUI({
      
      pickerInput(
        inputId = "Input_Analyses_MonoCompaVehicules_PhasesACV_Indicateurs",
        label = "Indicateur observé :",
        width = 400,
        choices = as.character(liste_impacts_ACV[dimnames(REACT_list_Impacts_parc[[input$Input_Analyses_MonoCompaVehicules_ChoixConfig]][["Total"]] %>%
                                                            multi_select(c("Biodiv_GWP","Biodiv_Ion","Biodiv_Ecotox","Biodiv_Eutro","Biodiv_Acid","Biodiv_Sol","Biodiv_Eau",
                                                                           "Sante_GWP","Sante_Ion","Sante_PMF","Sante_POF","Sante_Ozon","Sante_Htox","Sante_Eau"),'-',3))[[3]],"Indicators"]) ,
        options = list(
          style = "btn-primary",
          pickerOptions(noneSelectedText = "Aucun calcul"))
      )
      
    })
    output$MonoCompaVehicules_PhasesACV_vitessereseau = renderText({
      if(is.null(input$Input_Analyses_MonoCompaVehicules_ChoixConfig)) {""} else {
        paste("Estimation de la vitesse du réseau : ",
              vitesse_reseau(REACT_list_config[[input$Input_Analyses_MonoCompaVehicules_ChoixConfig]]),
              " km/h")
      }
      
    })
    
    MonoCompaVehicules_PhasesACV_Export <- reactiveValues(Export = 0, vitesse = 0)
    output$GRAPH_MonoCompaVehicules_PhasesACV <- renderPlotly({
      
      # input
      config <- input$Input_Analyses_MonoCompaVehicules_ChoixConfig 
      impact <- as.character(liste_impacts_ACV[which(liste_impacts_ACV$Indicators == input$Input_Analyses_MonoCompaVehicules_PhasesACV_Indicateurs ), "Abrev"])
      impacts_unite <- as.character(liste_impacts_ACV[which(liste_impacts_ACV$Indicators == input$Input_Analyses_MonoCompaVehicules_PhasesACV_Indicateurs ), "Unit_D3"])
      choix_vitesse <- input$Input_Analyses_MonoCompaVehicules_PhasesACV_ChoixVitesses
      vitesse <-input$Input_Analyses_MonoCompaVehicules_PhasesACV_vitesse
      type_veh <- input$Input_Analyses_MonoCompaVehicules_PhasesACV_MotoEuroSegment
      
      # Preparation data
      if(is.null(config)) {
        fig <- plotly_empty(type = "bar")
        fig <- fig %>% layout(title = "Aucun calcul réalisé")
        fig
      } else {
        Data_Impacts_parc <- REACT_list_Impacts_parc[[config]][["Total"]] %>% 
          multi_select(REACT_list_config[[config]][["liste_impacts_ACV"]]$Abrev,'+',3)
        Data_Parc_Utilisateur <- REACT_list_config[[config]][["Parc_utilisateur"]]
        
        if(choix_vitesse == "perso") {
          data_ini <- Data_Impacts_parc[,as.character(vitesse),impact,-1]
          MonoCompaVehicules_PhasesACV_Export$vitesse <- paste0("Vitesse :",as.character(vitesse)," km/h")
          
        } else if(choix_vitesse == "reseau") {
          usage_reseau <- REACT_list_config[[config]][["Infra_usage"]]$Utilisation
          vitesses_reseau <- apply(distrib_vitesse,1,function(i) sum(i*usage_reseau))
          data_ini <- apply(Data_Impacts_parc[,,impact,-1], MARGIN = c(1,3), function(i) sum(i*vitesses_reseau))
          MonoCompaVehicules_PhasesACV_Export$vitesse <- paste0("Vitesse du réseau :",vitesse_reseau(REACT_list_config[[input$Input_Analyses_MonoCompaVehicules_ChoixConfig]])," km/h")
        } else {
          data_ini <- apply(Data_Impacts_parc[,,impact,-1], MARGIN = c(1,3), function(i) sum(i*distrib_vitesse[,choix_vitesse]))
          if(choix_vitesse == "lent") {MonoCompaVehicules_PhasesACV_Export$vitesse <- "Vitesse lente : 32 km/h"}
          if(choix_vitesse == "moyen") {MonoCompaVehicules_PhasesACV_Export$vitesse <- "Vitesse moyenne : 51 km/h"}
          if(choix_vitesse == "rapide") {MonoCompaVehicules_PhasesACV_Export$vitesse <- "Vitesse rapide : 72 km/h"}
          if(choix_vitesse == "très.rapide") {MonoCompaVehicules_PhasesACV_Export$vitesse <- "Vitesse très rapide : 106 km/h"}
        }
        
        list_aggreg <- list()
        if("Moto" %in% type_veh) {
          list_aggreg <- c(list_aggreg,list(Data_Parc_Utilisateur$Fuel))
        }
        if("Euro" %in% type_veh) {
          list_aggreg <- c(list_aggreg,list(Data_Parc_Utilisateur$Euro.Standard))
        }
        if("Segment" %in% type_veh) {
          list_aggreg <- c(list_aggreg,list(Data_Parc_Utilisateur$Segment))
        }
        
        data <- data.frame(Aggreg_veh(data_ini,list_aggreg, sum))
        part_parc <- aggregate(Data_Parc_Utilisateur$Part, list_aggreg, sum)[which(aggregate(Data_Parc_Utilisateur$Part, list_aggreg, sum)[,length(list_aggreg)+1] != 0),length(list_aggreg)+1]
        rownames(data) <- renames_all(gsub(" ",".",rownames(data)))
        data <- data.frame(type_veh = c(rownames(data),"Moyenne parc"),rbind(data/part_parc,colSums(data_ini)))
        rownames(data) <- data$type_veh
        data <- data[tri_perso(data$type_veh,couleurs$Nom),]
        #annotations
        annotations <- list()
        for(i in 1:nrow(data)) {
          annotations[[i]] <- list(x = data$type_veh[i],
                                   y = signif(rowSums(data[,-1])[i],4),
                                   text = paste(gsub("e","E",signif(rowSums(data[,-1])[i],4)),impacts_unite),
                                   yanchor = "bottom",
                                   showarrow = FALSE)
          next
        }
        
        # Graphique
        fig <- plot_ly(x = as.character(data$type_veh), y = data$Gaz, type = 'bar', 
                       name = "Usage du véhicule", marker = list(color = couleurs$HEX_trait[which(couleurs$Id == "Gaz")]),
                       text = paste(gsub("e","E",signif(data$Gaz,3)),impacts_unite),
                       hovertemplate = paste('<b>%{x}</b>',
                                             '<br>%{text}'),
                       hoverlabel = list(bgcolor=couleurs$HEX_aire[which(couleurs$Id == "Gaz")]))
        fig <- fig %>% add_trace(y = data$Carburant, 
                                 name = "Prod. carburants", marker = list(color = couleurs$HEX_trait[which(couleurs$Id == "Carburants")]),
                                 text = paste(gsub("e","E",signif(data$Carburant,3)),impacts_unite),
                                 hovertemplate = paste('<b>%{x}</b>',
                                                       '<br>%{text}'),
                                 hoverlabel = list(bgcolor=couleurs$HEX_aire[which(couleurs$Id == "Carburants")]))
        fig <- fig %>% add_trace(y = data$Vehicule, 
                                 name = "Prod. véhicules", marker = list(color = couleurs$HEX_trait[which(couleurs$Id == "Vehicules")]),
                                 text = paste(gsub("e","E",signif(data$Vehicule,3)),impacts_unite),
                                 hovertemplate = paste('<b>%{x}</b>',
                                                       '<br>%{text}'),
                                 hoverlabel = list(bgcolor=couleurs$HEX_aire[which(couleurs$Id == "Vehicules")]))
        fig <- fig %>% add_trace(y = data$Infrastructures, 
                                 name = "Infrastructures", marker = list(color = couleurs$HEX_trait[which(couleurs$Id == "Infrastructures")]),
                                 text = paste(gsub("e","E",signif(data$Infrastructures,3)),impacts_unite),
                                 hovertemplate = paste('<b>%{x}</b>',
                                                       '<br>%{text}'),
                                 hoverlabel = list(bgcolor=couleurs$HEX_aire[which(couleurs$Id == "Infrastructures")]))
        fig <- fig %>% layout( 
          barmode = 'stack',
          title = paste("Contribution des phases d'ACV",'<br><i>',as.character(input$Input_Analyses_MonoCompaVehicules_PhasesACV_Indicateurs),'</i>'),
          xaxis = list(title = 'Catégories de véhicules',categoryorder = "array",categoryarray = as.character(data$type_veh)),
          yaxis = list(title = impacts_unite,exponentformat = "E",minexponent = 3),
          hoverlabel = list(font = list(color = "black")),
          margin = list(t = 65),
          annotations = annotations,
          shapes = list(type = "line",xref = "paper", x0 = 0, x1 = 1, y0 = sum(data["Moyenne parc",2:5]), y1 = sum(data["Moyenne parc",2:5]),
                        line = list(color = "black", width = 1.5, dash = "dash"))
        )
        MonoCompaVehicules_PhasesACV_Export$Export = data
        fig
      }
      
    })
    output$download_MonoCompaVehicules_PhasesACV <- downloadHandler(
      filename = "resultats_modemacv.xlsx",
      content = function(file) {write_xlsx(list("Résumé" = data.frame(`Export Modem_ACV` = c("Date","Type d'analyse","Nom du calcul","Indicateur","Unité","Vitesse"),
                                                                      Info = c(as.character(Sys.time()),"Analyse par véhicule des contributions",input$Input_Analyses_MonoCompaVehicules_ChoixConfig,
                                                                               as.character(input$Input_Analyses_MonoCompaVehicules_PhasesACV_Indicateurs),as.character(liste_impacts_ACV[which(liste_impacts_ACV$Indicators == input$Input_Analyses_MonoCompaVehicules_PhasesACV_Indicateurs ), "Units"]),
                                                                               MonoCompaVehicules_PhasesACV_Export$vitesse)),
                                                "Résultats" = MonoCompaVehicules_PhasesACV_Export$Export),path = file)} 
    )
    # MonoCompaVehicules - vehicules ####  
    output$MonoCompaVehicules_Vehicules_Indicateurs = renderUI({
      
      pickerInput(
        inputId = "Input_Analyses_MonoCompaVehicules_Vehicules_Indicateurs",
        label = "Indicateur observé :",
        width = 400,
        choices = as.character(liste_impacts_ACV[dimnames(REACT_list_Impacts_parc[[input$Input_Analyses_MonoCompaVehicules_ChoixConfig]][["Total"]] %>%
                                                            multi_select(c("Biodiv_GWP","Biodiv_Ion","Biodiv_Ecotox","Biodiv_Eutro","Biodiv_Acid","Biodiv_Sol","Biodiv_Eau",
                                                                           "Sante_GWP","Sante_Ion","Sante_PMF","Sante_POF","Sante_Ozon","Sante_Htox","Sante_Eau"),'-',3))[[3]],"Indicators"]) ,
        options = list(
          style = "btn-primary",
          pickerOptions(noneSelectedText = "Aucun calcul"))
      )
      
    })
    output$MonoCompaVehicules_Vehicules_vitessereseau = renderText({
      if(is.null(input$Input_Analyses_MonoCompaVehicules_ChoixConfig)) {""} else {
        paste("Estimation de la vitesse du réseau : ",
              vitesse_reseau(REACT_list_config[[input$Input_Analyses_MonoCompaVehicules_ChoixConfig]]),
              " km/h")
      }
      
    })
    
    MonoCompaVehicules_Vehicules_Export <- reactiveValues(Export = 0,vitesse =0)
    output$GRAPH_MonoCompaVehicules_Vehicules <- renderPlotly({
      
      # input
      config <- input$Input_Analyses_MonoCompaVehicules_ChoixConfig 
      impact <- as.character(liste_impacts_ACV[which(liste_impacts_ACV$Indicators == input$Input_Analyses_MonoCompaVehicules_Vehicules_Indicateurs ), "Abrev"])
      impacts_unite <- as.character(liste_impacts_ACV[which(liste_impacts_ACV$Indicators == input$Input_Analyses_MonoCompaVehicules_Vehicules_Indicateurs ), "Unit_D3"])
      choix_vitesse <- input$Input_Analyses_MonoCompaVehicules_Vehicules_ChoixVitesses
      vitesse <-input$Input_Analyses_MonoCompaVehicules_Vehicules_vitesse
      type_veh <- input$Input_Analyses_MonoCompaVehicules_Vehicules_MotoEuroSegment
      
      # Preparation data
      if(is.null(config)) {
        fig <- plotly_empty(type = "bar")
        fig <- fig %>% layout(title = "Aucun calcul réalisé")
        fig
      } else {
        Data_Impacts_parc <- REACT_list_Impacts_parc[[config]][["Total"]] %>% 
          multi_select(REACT_list_config[[config]][["liste_impacts_ACV"]]$Abrev,'+',3)
        Data_Parc_Utilisateur <- REACT_list_config[[config]][["Parc_utilisateur"]]
        if(choix_vitesse == "perso") {
          data <- Data_Impacts_parc[,as.character(vitesse),impact,1]
          MonoCompaVehicules_Vehicules_Export$vitesse <- paste0("Vitesse :",as.character(vitesse)," km/h")
        } else if(choix_vitesse == "reseau") {
          usage_reseau <- REACT_list_config[[config]][["Infra_usage"]]$Utilisation
          vitesses_reseau <- apply(distrib_vitesse,1,function(i) sum(i*usage_reseau))
          data <- apply(Data_Impacts_parc[,,impact,1], MARGIN = c(1), function(i) sum(i*vitesses_reseau))
          MonoCompaVehicules_Vehicules_Export$vitesse <- paste0("Vitesse du réseau :",vitesse_reseau(REACT_list_config[[input$Input_Analyses_MonoCompaVehicules_ChoixConfig]])," km/h")
        } else {
          data <- apply(Data_Impacts_parc[,,impact,1], MARGIN = c(1), function(i) sum(i*distrib_vitesse[,choix_vitesse]))
          if(choix_vitesse == "lent") {MonoCompaVehicules_Vehicules_Export$vitesse <- "Vitesse lente : 32 km/h"}
          if(choix_vitesse == "moyen") {MonoCompaVehicules_Vehicules_Export$vitesse <- "Vitesse moyenne : 51 km/h"}
          if(choix_vitesse == "rapide") {MonoCompaVehicules_Vehicules_Export$vitesse <- "Vitesse rapide : 72 km/h"}
          if(choix_vitesse == "très.rapide") {MonoCompaVehicules_Vehicules_Export$vitesse <- "Vitesse très rapide : 106 km/h"}
        }
        
        list_aggreg <- list()
        if("Moto" %in% type_veh) {
          list_aggreg <- c(list_aggreg,list(Data_Parc_Utilisateur$Fuel))
        }
        if("Euro" %in% type_veh) {
          list_aggreg <- c(list_aggreg,list(Data_Parc_Utilisateur$Euro.Standard))
        }
        if("Segment" %in% type_veh) {
          list_aggreg <- c(list_aggreg,list(Data_Parc_Utilisateur$Segment))
        }
        
        # Graphique
        data1 <- aggregate(data, list_aggreg,sum)
        data2 <- aggregate(Data_Parc_Utilisateur$Part, list_aggreg,sum)
        data1 <- data1[which(data2$x != 0),]
        data1$x <- data1$x/data2$x[which(data2$x != 0)]
        data2 <- data2[which(data2$x != 0),]
        if(length(type_veh) == 1) {noms <- gsub(" ",".",data1[order(data1$x, decreasing = TRUE),1])}
        if(length(type_veh) == 2) {noms <- gsub(" ",".",paste(data1[order(data1$x, decreasing = TRUE),1],data1[order(data1$x, decreasing = TRUE),2], sep ="."))}
        if(length(type_veh) == 3) {noms <- gsub(" ",".",paste(data1[order(data1$x, decreasing = TRUE),1],data1[order(data1$x, decreasing = TRUE),2],data1[order(data1$x, decreasing = TRUE),3], sep ="."))}
        noms <- renames_all(noms)
        width= data2$x[order(data1$x, decreasing = TRUE)]*100
        y= data1$x[order(data1$x, decreasing = TRUE)]
        x= cumsum(width)-(width/2)
        if(NA %in% match(noms, couleurs$Nom)) {
          temp_colors <- distinctColorPalette(length(x))
        }
        else {
          temp_colors <- couleurs$HEX_trait[match(noms, couleurs$Nom)]
        }
        data <- data.frame(x, y, width, temp_colors, noms)
        
        
        
        fig <- plot_ly(data)
        fig <- fig %>% add_bars(
          x= ~x,
          y= ~y,
          width = ~width,
          marker = list(color = ~temp_colors),
          hoverinfo = "text",
          hovertext = paste("<b>",data$noms,"</b>",
                            "<br>",paste(gsub("e","E",signif(data$y,3)),impacts_unite),
                            "<br><i> Poids :", data$width, "%</i>"),
          hoverlabel = list(bgcolor=paste(data$couleurs,"80",sep=""))
        )
        fig <- fig %>% add_trace(type = 'scatter',mode = 'text',x=90,y=1.03*sum(data$y * data$width)/100,text = "Moyenne du parc", showlegend = F)
        fig <- fig %>%layout(
          bargap = 0, 
          showlegend = FALSE,
          title = paste("Poids et niveaux d'impact des catégories",'<br><i>',as.character(input$Input_Analyses_MonoCompaVehicules_Vehicules_Indicateurs),'</i>'),
          xaxis = list(title = "Poids dans le parc",range(0,100),dtick = 10,ticksuffix = "%",
                       autorange = FALSE, range = c(0,100)),
          yaxis = list(rangemode = 'tozero',title = impacts_unite,exponentformat = "E",minexponent = 3),
          hoverlabel = list(font = list(color = "black")),
          margin = list(t = 65),
          shapes = list(type = "line",xref = "paper", x0 = 0, x1 = 1, y0 = sum(data$y * data$width)/100, y1 = sum(data$y * data$width)/100,
                        line = list(color = "black", width = 2,dash = "dash"), name = "Moyenne parc")
          )
        
        MonoCompaVehicules_Vehicules_Export$Export = data.frame(type_veh = data$noms, impact = data$y, poids = data$width)
        fig
        
      }
    })
    output$download_MonoCompaVehicules_Vehicules <- downloadHandler(
      filename = "resultats_modemacv.xlsx",
      content = function(file) {write_xlsx(list("Résumé" = data.frame(`Export Modem_ACV` = c("Date","Type d'analyse","Nom du calcul","Indicateur","Unité","Vitesse"),
                                                                      Info = c(as.character(Sys.time()),"Contributions des véhicules",input$Input_Analyses_MonoCompaVehicules_ChoixConfig,
                                                                               as.character(input$Input_Analyses_MonoCompaVehicules_Vehicules_Indicateurs),as.character(liste_impacts_ACV[which(liste_impacts_ACV$Indicators == input$Input_Analyses_MonoCompaVehicules_Vehicules_Indicateurs ), "Units"]),
                                                                               MonoCompaVehicules_Vehicules_Export$vitesse)),
                                                "Résultats" = MonoCompaVehicules_Vehicules_Export$Export),path = file)} 
    )
    # MonoCompaParcs ####
    output$MonoCompaParcs_choix_calcul = renderUI({
      pickerInput(
        inputId = "Input_Analyses_MonoCompaParcs_ChoixConfig",
        label = "Sélection du calcul :",
        width = 400,
        choices = REACT_list_calculs$nom,
        options = list(
          style = "btn-danger",
          pickerOptions(noneSelectedText = "Aucun calcul")),
        multiple = TRUE,
        selected = REACT_list_calculs$nom
      )
      
    })
    # MonoCompaParcs - Vitesse ####
    output$MonoCompaParcs_Vitesse_Indicateurs = renderUI({
      list_impact <- list()
      for(conf in input$Input_Analyses_MonoCompaParcs_ChoixConfig) {
        list_impact[[conf]] <- REACT_list_config[[conf]][["liste_impacts_ACV"]]$Abrev
        next
      }
      list_impact <- c(list_impact,list(liste_impacts_ACV$Abrev))
      list_impact <- Reduce(intersect,list_impact)
      pickerInput(
        inputId = "Input_Analyses_MonoCompaParcs_Vitesse_Indicateurs",
        label = "Indicateur observé :",
        width = 400,
        
        choices = as.character(liste_impacts_ACV[list_impact,"Indicators"]) ,
        options = list(
          style = "btn-primary",
          pickerOptions(noneSelectedText = "Aucun calcul"))
      )
      
    })
    
    MonoCompaParcs_Vitesse_Export <- reactiveValues(Export = 0)
    output$GRAPH_MonoCompaParcs_Vitesse <- renderPlotly({
      
      # input
      config <- input$Input_Analyses_MonoCompaParcs_ChoixConfig  
      impact <- as.character(liste_impacts_ACV[which(liste_impacts_ACV$Indicators == input$Input_Analyses_MonoCompaParcs_Vitesse_Indicateurs ), "Abrev"])
      impacts_unite <- as.character(liste_impacts_ACV[which(liste_impacts_ACV$Indicators == input$Input_Analyses_MonoCompaParcs_Vitesse_Indicateurs ), "Unit_D3"])
      
      # Preparation data
      if(is.null(config) ) {
        fig <- plotly_empty(type = "bar")
        fig <- fig %>% layout(title = "Aucun calcul réalisé")
        fig
      } else {
        Data_Impacts_parc <- list()
        for(i in 1:length(config)) {
          
          Data_Impacts_parc <- c(
            Data_Impacts_parc,
            list(REACT_list_Impacts_parc[[config[i]]][["Total"]][,,impact,]) 
                 
          ) 
          next
        }
        names(Data_Impacts_parc) <- config
        
        data <- data.frame(vitesses = 5:130)
        
        for(i in 1:length(Data_Impacts_parc)) {
          data <- cbind(data,colSums(Data_Impacts_parc[[i]][,,1]))
          next
        }
        names(data) <- c("vitesse",names(Data_Impacts_parc))
        # Graphique
        fig <- plot_ly(x = data[,"vitesse"], y = data[,2], name = names(data)[2], type = 'scatter', mode = 'lines',
                       text = paste(gsub("e","E",signif(data[,2],4)),impacts_unite),
                       hovertemplate = paste('<b>%{text}</b>',
                                             '<br><i>Vit : %{x} km/h</i>'))
        if (ncol(data) >= 3) {
          for(i in 3:ncol(data)) {
            fig <- fig %>% add_trace(y = data[,i], name = names(data)[i],
                                     text = paste(gsub("e","E",signif(data[,i],4)),impacts_unite),
                                     hovertemplate = paste('<b>%{text}</b>',
                                                           '<br><i>Vit : %{x} km/h</i>'))
            next
          }
        }
        fig <- fig %>% layout(
          title = paste('Impacts en fonction de la vitesse','<br><i>',as.character(input$Input_Analyses_MonoCompaParcs_Vitesse_Indicateurs),'</i>'),
          xaxis = list(title = 'vitesses (km/h)',rangemode = 'tozero'),
          yaxis = list(rangemode = 'tozero',title = impacts_unite,exponentformat = "E",minexponent = 3)
        )
        
        MonoCompaParcs_Vitesse_Export$Export = data.frame(data)
        fig
        
      }
    })
    output$download_MonoCompaParcs_Vitesse <- downloadHandler(
      filename = "resultats_modemacv.xlsx",
      content = function(file) {write_xlsx(list("Résumé" = data.frame(`Export Modem_ACV` = c("Date","Type d'analyse","Nom des calculs","Indicateur","Unité"),
                                                                      Info = c(as.character(Sys.time()),"Comparaison de parcs_vitesses",paste(input$Input_Analyses_MonoCompaParcs_ChoixConfig,collapse = " ; ") ,
                                                                               as.character(input$Input_Analyses_MonoCompaParcs_Vitesse_Indicateurs),as.character(liste_impacts_ACV[which(liste_impacts_ACV$Indicators == input$Input_Analyses_MonoCompaParcs_Vitesse_Indicateurs ), "Units"]))),
                                                "Résultats" = MonoCompaParcs_Vitesse_Export$Export),path = file)} 
    )
    # MonoCompaParcs - contributions ####
    output$MonoCompaParcs_Contributions_Indicateurs = renderUI({
      list_impact <- list()
      for(conf in input$Input_Analyses_MonoCompaParcs_ChoixConfig) {
        list_impact[[conf]] <- REACT_list_config[[conf]][["liste_impacts_ACV"]]$Abrev
        next
      }
      list_impact <- c(list_impact,list(liste_impacts_ACV$Abrev))
      list_impact <- Reduce(intersect,list_impact)
      pickerInput(
        inputId = "Input_Analyses_MonoCompaParcs_Contributions_Indicateurs",
        label = "Indicateur observé :",
        width = 400,
        choices = as.character(liste_impacts_ACV[list_impact,"Indicators"]) ,
        options = list(
          style = "btn-primary",
          pickerOptions(noneSelectedText = "Aucun calcul"))
      )
      
    })
    
    MonoCompaParcs_Contributions_Export <- reactiveValues(Export = 0, vitesse = 0)
    output$GRAPH_MonoCompaParcs_Contributions <- renderPlotly({
      
      # input
      config <- input$Input_Analyses_MonoCompaParcs_ChoixConfig  
      impact <- as.character(liste_impacts_ACV[which(liste_impacts_ACV$Indicators == input$Input_Analyses_MonoCompaParcs_Contributions_Indicateurs ), "Abrev"])
      impacts_unite <- as.character(liste_impacts_ACV[which(liste_impacts_ACV$Indicators == input$Input_Analyses_MonoCompaParcs_Contributions_Indicateurs ), "Unit_D3"])
      
      choix_vitesse <- input$Input_Analyses_MonoCompaParcs_Contributions_ChoixVitesses
      vitesse <- input$Input_Analyses_MonoCompaParcs_Contributions_vitesse
      
      # Preparation data
      if(is.null(config) ) {
        fig <- plotly_empty(type = "bar")
        fig <- fig %>% layout(title = "Aucun calcul réalisé")
        fig
      } else if(length(config) == 1) {
        fig <- plotly_empty(type = "bar")
        fig <- fig %>% layout(title = "Ajouter un autre calcul")
        fig
      } else {
        
        Data_Impacts_parc <- list()
        for(i in 1:length(config)) {
          
          Data_Impacts_parc <- c(
            Data_Impacts_parc,
            list(REACT_list_Impacts_parc[[config[i]]][["Total"]][,,impact,]) 
            # list(REACT_list_Impacts_parc[[config[i]]][["Total"]] %>% 
            #        multi_select(REACT_list_config[[config]][["liste_impacts_ACV"]]$Abrev,'+',3))
          ) 
          next
        }
        names(Data_Impacts_parc) <- config
        
        Data_Impacts_parc <- lapply(Data_Impacts_parc,function(i) apply(i,MARGIN = c(2,3),sum))
        
        data <- Data_Impacts_parc[[1]][,-1]
        for(i in 2:length(config)) {
          # data <- abind(data,Data_Impacts_parc[[i]][,impact,-1], along = 3)
          data <- abind(data,Data_Impacts_parc[[i]][,-1], along = 3)
          next
        }
        if(choix_vitesse == "perso") {
          data <- data[as.character(vitesse),,]
          MonoCompaParcs_Contributions_Export$vitesse <- paste0("Vitesse :",as.character(vitesse)," km/h")
        } else {
          data <- apply(data, MARGIN = c(2,3), function(i) sum(i*distrib_vitesse[,choix_vitesse]))
          if(choix_vitesse == "lent") {MonoCompaParcs_Contributions_Export$vitesse <- "Vitesse lente : 32 km/h"}
          if(choix_vitesse == "moyen") {MonoCompaParcs_Contributions_Export$vitesse <- "Vitesse moyenne : 51 km/h"}
          if(choix_vitesse == "rapide") {MonoCompaParcs_Contributions_Export$vitesse <- "Vitesse rapide : 72 km/h"}
          if(choix_vitesse == "très.rapide") {MonoCompaParcs_Contributions_Export$vitesse <- "Vitesse très rapide : 106 km/h"}
        }
        
        names(data) <- config
        data <- data.frame(Options = config,t(data))
        #annotations
        annotations <- list()
        for(i in 1:nrow(data)) {
          annotations[[i]] <- list(x = data$Options[i],
                                   y = signif(rowSums(data[,-1])[i],4),
                                   text = paste(signif(rowSums(data[,-1])[i],4),impacts_unite),
                                   yanchor = "bottom",
                                   showarrow = FALSE)
          next
        }
        
        # Graphique
        fig <- plot_ly(data,x = ~Options, y = data$Gaz, type = 'bar', 
                       name = "Usage du véhicule", marker = list(color = couleurs$HEX_trait[which(couleurs$Id == "Gaz")]),
                       text = paste(gsub("e","E",signif(data$Gaz,3)),impacts_unite),
                       hovertemplate = paste('<b>%{x}</b>',
                                             '<br>%{text}'),
                       hoverlabel = list(bgcolor=couleurs$HEX_aire[which(couleurs$Id == "Gaz")]))
        fig <- fig %>% add_trace(y = data$Carburant, 
                                 name = "Prod. carburants", marker = list(color = couleurs$HEX_trait[which(couleurs$Id == "Carburants")]),
                                 text = paste(gsub("e","E",signif(data$Carburant,3)),impacts_unite),
                                 hovertemplate = paste('<b>%{x}</b>',
                                                       '<br>%{text}'),
                                 hoverlabel = list(bgcolor=couleurs$HEX_aire[which(couleurs$Id == "Carburants")]))
        fig <- fig %>% add_trace(y = data$Vehicule, 
                                 name = "Prod. véhicules", marker = list(color = couleurs$HEX_trait[which(couleurs$Id == "Vehicules")]),
                                 text = paste(gsub("e","E",signif(data$Vehicule,3)),impacts_unite),
                                 hovertemplate = paste('<b>%{x}</b>',
                                                       '<br>%{text}'),
                                 hoverlabel = list(bgcolor=couleurs$HEX_aire[which(couleurs$Id == "Vehicules")]))
        fig <- fig %>% add_trace(y = data$Infrastructures, 
                                 name = "Infrastructures", marker = list(color = couleurs$HEX_trait[which(couleurs$Id == "Infrastructures")]),
                                 text = paste(gsub("e","E",signif(data$Infrastructures,3)),impacts_unite),
                                 hovertemplate = paste('<b>%{x}</b>',
                                                       '<br>%{text}'),
                                 hoverlabel = list(bgcolor=couleurs$HEX_aire[which(couleurs$Id == "Infrastructures")]))
        fig <- fig %>% layout(
          title = paste("Contribution des phases d'ACV",'<br><i>',as.character(input$Input_Analyses_MonoCompaParcs_Contributions_Indicateurs),'</i>'),
          barmode = 'stack',
          xaxis = list(title = 'Parcs simulés'),
          yaxis = list(title = impacts_unite,exponentformat = "E",minexponent = 3),
          hoverlabel = list(font = list(color = "black")),
          margin = list(t = 65),
          annotations = annotations
        )	
        MonoCompaParcs_Contributions_Export$Export = data.frame(data)[1:length(config),]
        fig
      }
    })
    output$download_MonoCompaParcs_Contributions <- downloadHandler(
      filename = "resultats_modemacv.xlsx",
      content = function(file) {write_xlsx(list("Résumé" = data.frame(`Export Modem_ACV` = c("Date","Type d'analyse","Nom des calculs","Indicateur","Unité","Vitesse"),
                                                                      Info = c(as.character(Sys.time()),"Comparaison de parcs_contribution",paste(input$Input_Analyses_MonoCompaParcs_ChoixConfig,collapse = " ; ") ,
                                                                               as.character(input$Input_Analyses_MonoCompaParcs_Contributions_Indicateurs),as.character(liste_impacts_ACV[which(liste_impacts_ACV$Indicators == input$Input_Analyses_MonoCompaParcs_Contributions_Indicateurs ), "Units"]),
                                                                               MonoCompaParcs_Contributions_Export$vitesse)),
                                                "Résultats" = MonoCompaParcs_Contributions_Export$Export),path = file)} 
    )
    # MonoFluxEnergie - Sankey ####
    output$MonoFluxEnergie_choix_calcul = renderUI({
      pickerInput(
        inputId = "Input_Analyses_MonoFluxEnergie_ChoixConfig",
        label = "Sélection du calcul :",
        width = 400,
        choices = REACT_list_calculs$nom,
        options = list(
          style = "btn-danger",
          pickerOptions(noneSelectedText = "Aucun calcul"))
      )
      
    })
    output$MonoFluxEnergie_vitessereseau = renderText({
      if(is.null(input$Input_Analyses_MonoFluxEnergie_ChoixConfig)) {""} else {
        paste("Estimation de la vitesse du réseau : ",
              vitesse_reseau(REACT_list_config[[input$Input_Analyses_MonoFluxEnergie_ChoixConfig]]),
              " km/h")
      }
      
    })
    
    Sankey <- reactiveValues(rendWtT = 0, rendTtW = 0, rendWtW = 0, rendTOTAL = 0)
    SankeyEnergie_Export <- reactiveValues(Export = 0, vitesse = 0)
    output$GRAPH_SankeyEnergie <- renderPlotly({
      
      # input
      config <- input$Input_Analyses_MonoFluxEnergie_ChoixConfig  
      choix_vitesse <- input$Input_Analyses_MonoFluxEnergie_ChoixVitesses
      vitesse <- input$Input_Analyses_MonoFluxEnergie_vitesse
      
      surf_frontales = c(1.75,1.9,2.1,2.4)
      coef_frott = data.frame(aero=0.32,Dens_air=1.2,roul=0.012)
      
      # Prepration data
      if(is.null(config) ) {
        fig <- plotly_empty(type = "bar")
        fig <- fig %>% layout(title = "Aucun calcul réalisé")
        fig
      } else if( !all(c("NRJf","NRJn","NRJr") %in% REACT_list_config[[config]][["liste_impacts_ACV"]]$Abrev ) ) {
        fig <- plotly_empty(type = "bar")
        fig <- fig %>% layout(title = "Indicateurs d'énergies primaires non-selectionnés")
        fig
      } else {
        Data_Mix_elec <- REACT_list_config[[config]][["Mix_elec"]]
        Data_Impacts_parc <- REACT_list_Impacts_parc[[config]][["Total"]] %>% 
          multi_select(REACT_list_config[[config]][["liste_impacts_ACV"]]$Abrev,'+',3)
        Data_Parc_Utilisateur <- REACT_list_config[[config]][["Parc_utilisateur"]]
        Data_Emis_conso_parc <- REACT_Emis_conso_parc[[config]]
        
        if(choix_vitesse == "perso") {
          sankey <- sankey_energie(Data_Impacts_parc, Data_Parc_Utilisateur, Data_Emis_conso_parc,Data_Mix_elec,vitesse, coef_frott, surf_frontales)
          links <- sankey[["links"]]
          nodes <- sankey[["nodes"]]
          SankeyEnergie_Export$vitesse <- paste0("Vitesse :",as.character(vitesse)," km/h")
        } else if(choix_vitesse == "reseau") {
          usage_reseau <- REACT_list_config[[config]][["Infra_usage"]]$Utilisation
          vitesses_reseau <- apply(distrib_vitesse,1,function(i) sum(i*usage_reseau))
          SankeyEnergie_Export$vitesse <- paste0("Vitesse du réseau :",vitesse_reseau(REACT_list_config[[input$Input_Analyses_MonoFluxEnergie_ChoixConfig]])," km/h")
          vitesses <- 5:130
          sankey <- sankey_energie(Data_Impacts_parc, Data_Parc_Utilisateur, Data_Emis_conso_parc,Data_Mix_elec,vitesses[1], coef_frott, surf_frontales)
          links <- sankey[["links"]]
          nodes <- sankey[["nodes"]]
          links$value <- links$value*vitesses_reseau[1]
          
          for (i in 2:126) {
            sankey <- sankey_energie(Data_Impacts_parc, Data_Parc_Utilisateur, Data_Emis_conso_parc,Data_Mix_elec,vitesses[i], coef_frott, surf_frontales)
            links$value <- links$value+sankey[["links"]][,"value"]*vitesses_reseau[i]
            next
          }
          
        } else {
          vitesses <- 5:130
          sankey <- sankey_energie(Data_Impacts_parc, Data_Parc_Utilisateur, Data_Emis_conso_parc,Data_Mix_elec,vitesses[1], coef_frott, surf_frontales)
          links <- sankey[["links"]]
          nodes <- sankey[["nodes"]]
          links$value <- links$value*distrib_vitesse[1,as.character(choix_vitesse)]
          if(choix_vitesse == "lent") {SankeyEnergie_Export$vitesse <- "Vitesse lente : 32 km/h"}
          if(choix_vitesse == "moyen") {SankeyEnergie_Export$vitesse <- "Vitesse moyenne : 51 km/h"}
          if(choix_vitesse == "rapide") {SankeyEnergie_Export$vitesse <- "Vitesse rapide : 72 km/h"}
          if(choix_vitesse == "très.rapide") {SankeyEnergie_Export$vitesse <- "Vitesse très rapide : 106 km/h"}
          for (i in 2:126) {
            sankey <- sankey_energie(Data_Impacts_parc, Data_Parc_Utilisateur, Data_Emis_conso_parc,Data_Mix_elec,vitesses[i], coef_frott, surf_frontales)
            links$value <- links$value+sankey[["links"]][,"value"]*distrib_vitesse[i,choix_vitesse]
            next
          }
        }
        if(Sys.getlocale(category = "LC_CTYPE")=="French_France.1252") {
          nodes$name <- iconv(nodes$name,from = "UTF-8", to = "CP1252")
          links$source <- iconv(links$source,from = "UTF-8", to = "CP1252")
          links$target <- iconv(links$target,from = "UTF-8", to = "CP1252")
        }
        # Graphique
        fig <- plot_ly(
          type = "sankey",
          arrangement = "snap",
          domain = list(
            x =  c(0,1),
            y =  c(0,1)
          ),
          orientation = "h",
          valueformat = ".2f",
          valuesuffix = "MJ",
          textfont = list(size = 16),
          
          node = list(
            label = nodes$name,
            color = nodes$color,
            x = nodes$x,
            y = nodes$y,
            pad = 10,
            thickness = 15,
            line = list(
              color = "black",
              width = 0.5
            ),
            hovertemplate = '<b>%{label}</b>'
          ),
          
          link = list(
            source = links$IDsource,
            target = links$IDtarget,
            value =  links$value,
            label = paste0("<b>Source : ",links$source,"<br>Cible : ",links$target,"</b>"),
            hovertemplate = '%{label}'
          )
        )
        
        Sankey$rendWtT <- round(sum(links$value[which(links$target == "Réservoir")])/sum(links$value[which(links$target == "Énergie carburants")]),3)*100
        Sankey$rendTtW <- round(sum(links$value[which(links$target == "Énergie roues")])/sum(links$value[which(links$target == "Réservoir")]),3)*100
        Sankey$rendWtW <- round(sum(links$value[which(links$target == "Énergie roues")])/sum(links$value[which(links$target == "Énergie carburants")]),3)*100
        Sankey$rendTOTAL <- round(sum(links$value[which(links$target == "Énergie roues")])/sum(links$value[which(links$source == "Énergie primaire")]),3)*100
        
        fig <- fig %>% layout(
          title = list(text = "Diagramme de flux d'énergie",font = list(font = 20)),
          font = list(
            size = 10
          ),
          annotations = list(text = paste0("Rendement du puit au réservoir : ",Sankey$rendWtT, "%<br>",
                                          "Rendement du réservoir à la roue : ",Sankey$rendTtW, "%<br>",
                                          "Rendement du puit à la roue : ",Sankey$rendWtW, "%<br>",
                                          "Rendement total (avec les énergies grises) : ",Sankey$rendTOTAL, "%"),
                             x = 1, y = 0,showarrow=FALSE,font = list(size=15),bordercolor = "grey",align = "left" )
        )
        
        
        SankeyEnergie_Export$Export = links[,c("source","target","value")]
        
        fig
      }
      
    })
    
    output$download_SankeyEnergie <- downloadHandler(
      filename = "resultats_modemacv.xlsx",
      content = function(file) {write_xlsx(list("Résumé" = data.frame(`Export Modem_ACV` = c("Date","Type d'analyse","Nom du calcul","Unité","Vitesse"),
                                                                      Info = c(as.character(Sys.time()),"Analyse des flux d'énergies",input$Input_Analyses_MonoFluxEnergie_ChoixConfig,"MJ",
                                                                               SankeyEnergie_Export$vitesse)),
                                                "Résultats" = SankeyEnergie_Export$Export),path = file)} 
    )
    # MonoACVdetail - contributions ####
    output$MonoACVdetail_choix_calcul = renderUI({
      pickerInput(
        inputId = "Input_Analyses_MonoACVdetail_ChoixConfig",
        label = "Sélection du calcul :",
        width = '100%',
        choices = REACT_list_calculs$nom,
        options = list(
          style = "btn-danger",
          pickerOptions(noneSelectedText = "Aucun calcul"))
      )
      
    })	
    output$MonoACVdetail_vitessereseau = renderText({
      if(is.null(input$Input_Analyses_MonoACVdetail_ChoixConfig)) {""} else {
        paste("Estimation de la vitesse du réseau : ",
              vitesse_reseau(REACT_list_config[[input$Input_Analyses_MonoACVdetail_ChoixConfig]]),
              " km/h")
      }
      
    })
    output$MonoACVdetail_vitessereseau2 = renderText({
      if(is.null(input$Input_Analyses_MonoACVdetail_ChoixConfig)) {""} else {
        paste("Estimation de la vitesse du réseau : ",
              vitesse_reseau(REACT_list_config[[input$Input_Analyses_MonoACVdetail_ChoixConfig]]),
              " km/h")
      }
      
    })
    MonoACVdetail_Export1 <- reactiveValues(Export = 0, vitesse = 0)
    MonoACVdetail_Export2 <- reactiveValues(Export = 0)
    MonoACVdetail_Export3 <- reactiveValues(Export = 0, vitesse = 0)
    output$GRAPH_MonoACVdetail_synthese <- renderPlotly({
      choix_vitesse <- input$Input_Analyses_MonoACVdetail_ChoixVitesses
      vitesse <-input$Input_Analyses_MonoACVdetail_vitesse
      config <- input$Input_Analyses_MonoACVdetail_ChoixConfig
      
      if(is.null(config)) {
        fig <- plotly_empty(type = "pie")
        fig <- fig %>% layout(title = "Aucun calcul réalisé")
        fig
      } else {
        Data_Impacts_parc <- REACT_list_Impacts_parc[[config]]
        if(choix_vitesse == "perso") {
          data_vit <- lapply(Data_Impacts_parc,function(i) apply(i[,as.character(vitesse),,], MARGIN= c(2,3), sum))
          MonoACVdetail_Export1$vitesse <- paste0("Vitesse :",as.character(vitesse)," km/h")
        } else if(choix_vitesse == "reseau") {
          usage_reseau <- REACT_list_config[[config]][["Infra_usage"]]$Utilisation
          vitesses_reseau <- apply(distrib_vitesse,1,function(i) sum(i*usage_reseau))
          MonoACVdetail_Export1$vitesse <- paste0("Vitesse du réseau :",vitesse_reseau(REACT_list_config[[input$Input_Analyses_MonoACVdetail_ChoixConfig]])," km/h")
          data_vit <- lapply(Data_Impacts_parc,function(x) apply(x[,,,], MARGIN = c(3,4), function(i) sum(colSums(i)*vitesses_reseau)))
        } else {
          data_vit <- lapply(Data_Impacts_parc,function(x) apply(x[,,,], MARGIN = c(3,4), function(i) sum(colSums(i)*distrib_vitesse[,choix_vitesse])))
          if(choix_vitesse == "lent") {MonoACVdetail_Export1$vitesse <- "Vitesse lente : 32 km/h"}
          if(choix_vitesse == "moyen") {MonoACVdetail_Export1$vitesse <- "Vitesse moyenne : 51 km/h"}
          if(choix_vitesse == "rapide") {MonoACVdetail_Export1$vitesse <- "Vitesse rapide : 72 km/h"}
          if(choix_vitesse == "très.rapide") {MonoACVdetail_Export1$vitesse <- "Vitesse très rapide : 106 km/h"}
        }
        data <- data.frame(ids = c("Gaz","Carburants","Vehicules","Infrastructures"),
                         labels=c("Gaz","Carburants","Vehicules","Infrastructures"),
                         parents=c("","","",""),
                         t(data_vit[["Total"]][REACT_list_config[[config]][["liste_impacts_ACV"]]$Abrev,-1]))
        for(i in c("Gaz","Carburants","Vehicules","Infrastructures")) {
          Impacts <- data_vit[[i]]
          temp <- data.frame(ids = dimnames(Impacts)[[2]],
                             labels=dimnames(Impacts)[[2]],
                             parents=rep(i,dim(Impacts)[2]),
                             t(Impacts[REACT_list_config[[config]][["liste_impacts_ACV"]]$Abrev,])
          )
          data <- rbind(data,temp)			
          next
        }
        data$labels <- renames_all(data$labels)
        all_buttons <- list()
        for(i in 1:(ncol(data)-3)) {
          all_buttons[[i]] <- list(method = "restyle",
                                   args = list(list(values = list(data[,i+3]), text = list(paste(gsub("e","E",signif(data[,i+3],3)),liste_impacts_ACV[as.character(names(data)[i+3]),"Units"])))),
                                   label = liste_impacts_ACV[as.character(names(data)[i+3]),"Indicators"])
          next
        }
        temp_export <- data[,-c(1,3)]
        names(temp_export) <- c("Elements",paste0(as.character(liste_impacts_ACV$Indicators[names(temp_export)[-1]])," (",as.character(liste_impacts_ACV$Units[names(temp_export)[-1]]),")"))
        MonoACVdetail_Export1$Export <- temp_export
        fig <- plot_ly(ids=data$ids,labels = data$labels,parents = data$parents,values = data[,4],
          type = 'sunburst',domain=list(column=1),maxdepth=2,
          marker = list(colors = c("#d200d2","#05495c","#efcf2f","#a30e10",couleurs$HEX_aire[match(data$labels[-1:-4],couleurs$Nom)])),
          text = paste(gsub("e","E",signif(data[,4],3)),liste_impacts_ACV[as.character(names(data)[4]),"Units"]),
          insidetextorientation='radial',branchvalues = 'total',
          textinfo = "label+percent entry",
          hovertemplate = '<b>%{label} :</b><br>%{text}<br>%{percentEntry:0%} <extra></extra>')
        fig <- fig %>% layout(
          paper_bgcolor = "#ecf0f5",
          autosize = TRUE,
          margin = list(t =10, b = 5),
          updatemenus = list(list(buttons=all_buttons,xanchor = "right",x = 0, bgcolor = "white"))
        )
        fig
      }
      
    })
    
    output$download_MonoACVdetail1 <- downloadHandler(
      filename = "resultats_modemacv.xlsx",
      content = function(file) {write_xlsx(list("Résumé" = data.frame(`Export Modem_ACV` = c("Date","Type d'analyse","Nom du calcul","Vitesse"),
                                                                      Info = c(as.character(Sys.time()),"Analyse détaillée des contributions",input$Input_Analyses_MonoACVdetail_ChoixConfig,
                                                                               MonoACVdetail_Export1$vitesse)),
                                                "Résultats" = MonoACVdetail_Export1$Export),path = file)} 
    )
    
    output$MonoACVdetail_Indicateurs1 = renderUI({
      
      pickerInput(
        inputId = "Input_Analyses_MonoACVdetail_Indicateurs1",
        label = "Indicateur observé :",
        width = 400,
        choices = as.character(liste_impacts_ACV[dimnames(REACT_list_Impacts_parc[[input$Input_Analyses_MonoACVdetail_ChoixConfig]][["Total"]] %>%
                                                            multi_select(c("Biodiv_GWP","Biodiv_Ion","Biodiv_Ecotox","Biodiv_Eutro","Biodiv_Acid","Biodiv_Sol","Biodiv_Eau",
                                                                           "Sante_GWP","Sante_Ion","Sante_PMF","Sante_POF","Sante_Ozon","Sante_Htox","Sante_Eau"),'-',3))[[3]],"Indicators"]) ,
        options = list(
          style = "btn-primary",
          pickerOptions(noneSelectedText = "Aucun calcul"))                                           
      )
      
    })
    
    output$GRAPH_MonoACVdetail_vitesse <- renderPlotly({
      
      config <- input$Input_Analyses_MonoACVdetail_ChoixConfig
      
      impact <- as.character(liste_impacts_ACV[which(liste_impacts_ACV$Indicators == input$Input_Analyses_MonoACVdetail_Indicateurs1 ), "Abrev"])
      impacts_unite <- as.character(liste_impacts_ACV[which(liste_impacts_ACV$Indicators == input$Input_Analyses_MonoACVdetail_Indicateurs1 ), "Unit_D3"])
      
      details <- input$MonoACVdetail_Phases1
      
      if(is.null(config)) {
        fig <- plotly_empty(type = "bar")
        fig <- fig %>% layout(title = "Aucun calcul réalisé")
        fig
      } else {
        Data_Impacts_parc <- REACT_list_Impacts_parc[[config]]
        
        data <- lapply(Data_Impacts_parc,function(i) apply(i[,,impact,],2:3,sum))
        data[["Total"]] <- data[["Total"]][,-1]
        data <- data.frame(abind(data[["Total"]],data[["Gaz"]],data[["Carburants"]],data[["Vehicules"]],data[["Infrastructures"]],along = 2))
        data_graph <- data.frame(vitesse = 5:130)
        if("Gaz" %in% details) {data_graph <- cbind(data_graph,data[,5:9])} else {data_graph <- data.frame(data_graph,Gaz = data$Gaz)}
        if("Carburants" %in% details) {data_graph <- cbind(data_graph,data[,10:18])} else {data_graph <- data.frame(data_graph,Carburants = data$Carburant)}
        if("Vehicules" %in% details) {data_graph <- cbind(data_graph,data[,19:23])} else {data_graph <- data.frame(data_graph,Vehicules = data$Vehicule)}
        if("Infra" %in% details) {data_graph <- cbind(data_graph,data[,24:29])} else {data_graph <- data.frame(data_graph,Infrastructures = data$Infrastructures)}
        data_graph <- data_graph[,which(colSums(data_graph)!=0)]
        names(data_graph) <- renames_all(names(data_graph))
        temp_colors <- couleurs$HEX_aire[match(names(data_graph),couleurs$Nom)]
        MonoACVdetail_Export2$Export <- data_graph
        #construction du graph (options absolu et relatif)
        fig <- plot_ly(data_graph, x = ~vitesse, y = data_graph[,2], name = names(data_graph)[2], text = paste(gsub("e","E",signif(data_graph[,2],3)),impacts_unite),
                       fillcolor = temp_colors[2],type = 'scatter', mode = 'none', stackgroup = 'one',customdata = rep(as.character(names(data_graph)[2]),126),
                       hovertemplate = paste('<b>%{customdata}</b>',
                                             '<br><i>Vit </i>: %{x} km/h',
                                             '<br>%{text} <extra></extra>'))
        for(i in 3:ncol(data_graph)) {
          fig <- fig %>% add_trace(y = data_graph[,i], name = names(data_graph)[i],text = paste(gsub("e","E",signif(data_graph[,i],3)),impacts_unite),
                                   fillcolor = temp_colors[i],customdata = rep(as.character(names(data_graph)[i]),126),
                                   hovertemplate = paste('<b>%{customdata}</b>',
                                                         '<br><i>Vit </i>: %{x} km/h',
                                                         '<br>%{text} <extra></extra>'))
          next
        }
        fig <- fig %>% layout(
          title = paste('Impacts en fonction de la vitesse','<br><i>',as.character(input$Input_Analyses_MonoACVdetail_Indicateurs1),'</i>'),
          plot_bgcolor = "#d9d9d9",
          margin = list(t = 65),
          xaxis = list(title = 'vitesses (km/h)',rangemode = 'tozero',gridcolor = "#b4b4b4", dtick = 10),
          yaxis = list(title = impacts_unite,gridcolor = "b4b4b4",exponentformat = "E",minexponent = 3),
          updatemenus = list(
            list(
              buttons = list(
                list(method = 'restyle',
                     args = list('groupnorm',''),
                     label = 'Absolu'),
                
                list(method = 'restyle',
                     args = list('groupnorm','percent'),
                     label = 'Relatif')			
              )
            )
          )
        )
        fig
      }
    })
    
    output$download_MonoACVdetail2 <- downloadHandler(
      filename = "resultats_modemacv.xlsx",
      content = function(file) {write_xlsx(list("Résumé" = data.frame(`Export Modem_ACV` = c("Date","Type d'analyse","Nom du calcul","Indicateur","Unité"),
                                                                      Info = c(as.character(Sys.time()),"Analyse détaillée des contributions - fonction de la vitesse",input$Input_Analyses_MonoACVdetail_ChoixConfig,
                                                                               as.character(input$Input_Analyses_MonoACVdetail_Indicateurs1),as.character(liste_impacts_ACV[which(liste_impacts_ACV$Indicators == input$Input_Analyses_MonoACVdetail_Indicateurs1 ), "Units"]))),
                                                "Résultats" = MonoACVdetail_Export2$Export),path = file)} 
    )
    
    output$MonoACVdetail_Indicateurs2 = renderUI({
      
      pickerInput(
        inputId = "Input_Analyses_MonoACVdetail_Indicateurs2",
        label = "Indicateur observé :",
        width = 400,
        choices = as.character(liste_impacts_ACV[dimnames(REACT_list_Impacts_parc[[input$Input_Analyses_MonoACVdetail_ChoixConfig]][["Total"]] %>%
                                                            multi_select(c("Biodiv_GWP","Biodiv_Ion","Biodiv_Ecotox","Biodiv_Eutro","Biodiv_Acid","Biodiv_Sol","Biodiv_Eau",
                                                                           "Sante_GWP","Sante_Ion","Sante_PMF","Sante_POF","Sante_Ozon","Sante_Htox","Sante_Eau"),'-',3))[[3]],"Indicators"]) ,
        options = list(
          style = "btn-primary",
          pickerOptions(noneSelectedText = "Aucun calcul"))
        
      )
      
    })
    
    output$GRAPH_MonoACVdetail_typeveh <- renderPlotly({
      
      config <- input$Input_Analyses_MonoACVdetail_ChoixConfig
      
      impact <- as.character(liste_impacts_ACV[which(liste_impacts_ACV$Indicators == input$Input_Analyses_MonoACVdetail_Indicateurs2 ), "Abrev"])
      impacts_unite <- as.character(liste_impacts_ACV[which(liste_impacts_ACV$Indicators == input$Input_Analyses_MonoACVdetail_Indicateurs2 ), "Unit_D3"])
      choix_vitesse <- input$Input_Analyses_MonoACVdetail_ChoixVitesses
      vitesse <-input$Input_Analyses_MonoACVdetail_vitesse
      type_veh <- input$Input_Analyses_MonoACVdetail_Vehicules_MotoEuroSegment
      details <- input$MonoACVdetail_Phases2
      
      if(is.null(config)) {
        fig <- plotly_empty(type = "bar")
        fig <- fig %>% layout(title = "Aucun calcul réalisé")
        fig
      } else {
        Data_Impacts_parc <- REACT_list_Impacts_parc[[config]]
        Data_Config_Utilisateur <- REACT_list_config[[config]]
        
        
        if(choix_vitesse == "perso") {
          data_ini <- lapply(Data_Impacts_parc,function(i) i[,as.character(vitesse),impact,])
          MonoACVdetail_Export3$vitesse <- paste0("Vitesse :",as.character(vitesse)," km/h")
        } else if(choix_vitesse == "reseau") {
          usage_reseau <- Data_Config_Utilisateur[["Infra_usage"]]$Utilisation
          vitesses_reseau <- apply(distrib_vitesse,1,function(i) sum(i*usage_reseau))
          MonoACVdetail_Export3$vitesse <- paste0("Vitesse du réseau :",vitesse_reseau(REACT_list_config[[input$Input_Analyses_MonoACVdetail_ChoixConfig]])," km/h")
          data_ini <- lapply(Data_Impacts_parc,function(i) apply(i[,,impact,], MARGIN = c(1,3), function(i) sum(i*vitesses_reseau)))
        } else {
          data_ini <- lapply(Data_Impacts_parc,function(i) apply(i[,,impact,], MARGIN = c(1,3), function(i) sum(i*distrib_vitesse[,choix_vitesse])))
          if(choix_vitesse == "lent") {MonoACVdetail_Export3$vitesse <- "Vitesse lente : 32 km/h"}
          if(choix_vitesse == "moyen") {MonoACVdetail_Export3$vitesse <- "Vitesse moyenne : 51 km/h"}
          if(choix_vitesse == "rapide") {MonoACVdetail_Export3$vitesse <- "Vitesse rapide : 72 km/h"}
          if(choix_vitesse == "très.rapide") {MonoACVdetail_Export3$vitesse <- "Vitesse très rapide : 106 km/h"}
        }
        
        list_aggreg <- list()
        if("Moto" %in% type_veh) {
          list_aggreg <- c(list_aggreg,list(Data_Config_Utilisateur[["Parc_utilisateur"]]$Fuel))
        }
        if("Euro" %in% type_veh) {
          list_aggreg <- c(list_aggreg,list(Data_Config_Utilisateur[["Parc_utilisateur"]]$Euro.Standard))
        }
        if("Segment" %in% type_veh) {
          list_aggreg <- c(list_aggreg,list(Data_Config_Utilisateur[["Parc_utilisateur"]]$Segment))
        }
        
        data_ini[["Total"]] <- data_ini[["Total"]][,-1]
        data_ini <- abind(data_ini[["Total"]],data_ini[["Gaz"]],data_ini[["Carburants"]],data_ini[["Vehicules"]],data_ini[["Infrastructures"]],along = 2)
        data <- data.frame(Aggreg_veh(data_ini,list_aggreg, sum))
        part_parc <- aggregate(Data_Config_Utilisateur[["Parc_utilisateur"]]$Part, list_aggreg, sum)[which(aggregate(Data_Config_Utilisateur[["Parc_utilisateur"]]$Part, list_aggreg, sum)[,length(list_aggreg)+1] != 0),length(list_aggreg)+1]
        data <- data.frame(type_veh = c(rownames(data),"Moyenne parc"),rbind(data/part_parc,colSums(data_ini)))
        names(data) <- names(data)
        data_graph <- data.frame(type_veh = data[,1])
        if("Gaz" %in% details) {data_graph <- cbind(data_graph,data[,6:10])} else {data_graph <- data.frame(data_graph,Gaz = data$Gaz)}
        if("Carburants" %in% details) {data_graph <- cbind(data_graph,data[,11:19])} else {data_graph <- data.frame(data_graph,Carburants = data$Carburant)}
        if("Vehicules" %in% details) {data_graph <- cbind(data_graph,data[,20:24])} else {data_graph <- data.frame(data_graph,Vehicules = data$Vehicule)}
        if("Infra" %in% details) {data_graph <- cbind(data_graph,data[,25:30])} else {data_graph <- data.frame(data_graph,Infrastructures = data$Infrastructures)}
        data_graph <- data_graph[,c(1,1+which(colSums(data_graph[,-1])!=0))]
        data_graph$type_veh <- renames_all(gsub(" ",".",data_graph$type_veh))
        rownames(data_graph) <- data_graph$type_veh
        names(data_graph) <- renames_all(names(data_graph))
        temp_colors <- couleurs$HEX_trait[match(names(data_graph),couleurs$Nom)]
        data_graph <- data_graph[tri_perso(data_graph$type_veh,couleurs$Nom),]
        #annotations
        annotations <- list()
        for(i in 1:nrow(data_graph)) {
          annotations[[i]] <- list(x = data_graph$type_veh[i],
                                   y = signif(rowSums(data_graph[,-1])[i],4),
                                   text = paste(gsub("e","E",signif(rowSums(data_graph[,-1])[i],4)),impacts_unite),
                                   yanchor = "bottom",
                                   showarrow = FALSE)
          next
        }
        MonoACVdetail_Export3$Export <- data_graph
        #construction du graph 
        fig <- plot_ly(x = as.character(data_graph[,"type_veh"]), y = data_graph[,2], name = names(data_graph)[2],type = 'bar',
                       marker = list(color = temp_colors[2]),
                       text = paste(gsub("e","E",signif(data_graph[,2],3)),impacts_unite),customdata = rep(names(data_graph)[2],nrow(data_graph)),
                       hovertemplate = paste('<b>%{x}</b>',
                                             '<br>%{customdata}',
                                             '<br>%{text}<extra></extra>'))
        for(i in 3:ncol(data_graph)){
          fig <- fig %>% add_trace(y = data_graph[,i], name = as.character(names(data_graph)[i]),
                                   marker = list(color = temp_colors[i]),
                                   text = paste(gsub("e","E",signif(data_graph[,i],3)),impacts_unite),customdata = rep(names(data_graph)[i],nrow(data_graph)),
                                   hovertemplate = paste('<b>%{x}</b>',
                                                         '<br>%{customdata}',
                                                         '<br>%{text}<extra></extra>'))
          next
        }
        fig <- fig %>% layout( 
          barmode = 'stack',
          title = paste("Contribution des éléments",'<br><i>',as.character(input$Input_Analyses_MonoACVdetail_Indicateurs2),'</i>'),
          xaxis = list(title = 'Catégories de véhicules',categoryorder = "array",categoryarray = as.character(data_graph[,"type_veh"])),
          yaxis = list(title = impacts_unite,exponentformat = "E",minexponent = 3),
          margin = list(t = 65),
          annotations = annotations,
          shapes = list(type = "line",xref = "paper", x0 = 0, x1 = 1, y0 = sum(data_graph["Moyenne parc",-1]), y1 = sum(data_graph["Moyenne parc",-1]),
                        line = list(color = "black", width = 1.5,dash = "dash"))
          
        )	
        fig
      }
    })
    
    output$download_MonoACVdetail3 <- downloadHandler(
      filename = "resultats_modemacv.xlsx",
      content = function(file) {write_xlsx(list("Résumé" = data.frame(`Export Modem_ACV` = c("Date","Type d'analyse","Nom du calcul","Indicateur","Unité","Vitesse"),
                                                                      Info = c(as.character(Sys.time()),"Analyse détaillée des contributions - type de véhicule",input$Input_Analyses_MonoACVdetail_ChoixConfig,
                                                                               as.character(input$Input_Analyses_MonoACVdetail_Indicateurs2),as.character(liste_impacts_ACV[which(liste_impacts_ACV$Indicators == input$Input_Analyses_MonoACVdetail_Indicateurs2 ), "Units"]),
                                                                               MonoACVdetail_Export3$vitesse)),
                                                "Résultats" = MonoACVdetail_Export3$Export),path = file)} 
    )
    
    # MonoDommage  ####
    output$MonoDommage_choix_calcul = renderUI({
      pickerInput(
        inputId = "Input_Analyses_MonoDommage_ChoixConfig",
        label = "Sélection du calcul :",
        width = 400,
        choices = REACT_list_calculs$nom,
        options = list(
          style = "btn-danger",
          pickerOptions(noneSelectedText = "Aucun calcul"))
      )
      
    })	
    output$MonoDommage_vitessereseau = renderText({
      if(is.null(input$Input_Analyses_MonoDommage_ChoixConfig)) {""} else {
        paste("Estimation de la vitesse du réseau : ",
              vitesse_reseau(REACT_list_config[[input$Input_Analyses_MonoDommage_ChoixConfig]]),
              " km/h")
      }
      
    })
    MonoDommage_Export <- reactiveValues(Sante = 0,Biodiv = 0, Export_Sante = 0, Export_Biodiv = 0,Export_Sante2 = 0, Export_Biodiv2 = 0,Total_Sante = 0, Total_Biodiv = 0,vitesse = 0)
    
    output$GRAPH_MonoDommage <- renderPlotly({
      
      config <- input$Input_Analyses_MonoDommage_ChoixConfig
      
      choix_vitesse <- input$Input_Analyses_MonoDommage_ChoixVitesses
      vitesse <- input$Input_Analyses_MonoDommage_vitesse
      dommage <- input$Input_Analyses_MonoDommage_SanteBiodiv1
      
      if(is.null(config)) {
        fig <- plotly_empty(type = "bar")
        fig <- fig %>% layout(title = "Aucun calcul réalisé")
      } else {
        Data_Impacts_parc <- REACT_list_Impacts_parc[[config]][["Total"]]
        if(!("Sante" %in% dimnames(Data_Impacts_parc)[[3]])) {
          fig <- plotly_empty(type = "bar")
          fig <- fig %>% layout(title = "Indicateurs de dommages non-calculés")
        } else {
        
          data_sante <- Data_Impacts_parc[,,,1] %>%
                            multi_select(liste_impacts_ACV$Abrev[which(liste_impacts_ACV$Type == "Santé humaine")],'+',3) %>%
                            apply(MARGIN = 2:3,sum)
          MonoDommage_Export$Sante <- data.frame(data_sante)
          data_biodiv <- Data_Impacts_parc[,,,1] %>%
                            multi_select(liste_impacts_ACV$Abrev[which(liste_impacts_ACV$Type == "Atteinte à la biodiversité")],'+',3) %>%
                            apply(MARGIN = 2:3,sum)
          MonoDommage_Export$Biodiv <- data.frame(data_biodiv)
          
          if(choix_vitesse == "perso") {
            data_sante <- MonoDommage_Export$Sante[as.character(vitesse),]
            data_biodiv <- MonoDommage_Export$Biodiv[as.character(vitesse),]
            MonoDommage_Export$vitesse <- paste0("Vitesse :",as.character(vitesse)," km/h")
          } else if(choix_vitesse == "reseau") {
            usage_reseau <- REACT_list_config[[config]][["Infra_usage"]]$Utilisation
            vitesses_reseau <- apply(distrib_vitesse,1,function(i) sum(i*usage_reseau))
            data_sante <- apply(MonoDommage_Export$Sante,2,function(i) sum(i*vitesses_reseau))
            data_biodiv <- apply(MonoDommage_Export$Biodiv,2,function(i) sum(i*vitesses_reseau))
            MonoDommage_Export$vitesse <- paste0("Vitesse du réseau :",vitesse_reseau(REACT_list_config[[input$Input_Analyses_MonoDommage_ChoixConfig]])," km/h")
          } else {
            data_sante <- apply(MonoDommage_Export$Sante,2,function(i) sum(i*distrib_vitesse[,choix_vitesse]))
            data_biodiv <- apply(MonoDommage_Export$Biodiv,2,function(i) sum(i*distrib_vitesse[,choix_vitesse]))
            if(choix_vitesse == "lent") {MonoDommage_Export$vitesse <- "Vitesse lente : 32 km/h"}
            if(choix_vitesse == "moyen") {MonoDommage_Export$vitesse <- "Vitesse moyenne : 51 km/h"}
            if(choix_vitesse == "rapide") {MonoDommage_Export$vitesse <- "Vitesse rapide : 72 km/h"}
            if(choix_vitesse == "très.rapide") {MonoDommage_Export$vitesse <- "Vitesse très rapide : 106 km/h"}
          }
          MonoDommage_Export$Total_Sante <- sum(data_sante)
          MonoDommage_Export$Total_Biodiv <- sum(data_biodiv)
          names(data_sante) <- substring(liste_impacts_ACV$Indicators[20:26],16)
          names(data_biodiv) <- substring(liste_impacts_ACV$Indicators[27:33],15)
          MonoDommage_Export$Export_Sante <- data.frame(t(data_sante))
          MonoDommage_Export$Export_Biodiv <- data.frame(t(data_biodiv))
          tempSante <- data.frame(Vitesse = 5:130,MonoDommage_Export$Sante)
          tempBiodiv <- data.frame(Vitesse = 5:130,MonoDommage_Export$Biodiv)
          names(tempSante) <- c("Vitesse",names(data_sante)) 
          names(tempBiodiv) <- c("Vitesse",names(data_biodiv)) 
          MonoDommage_Export$Export_Sante2 <- tempSante
          MonoDommage_Export$Export_Biodiv2 <- tempBiodiv
          if(dommage == "Sante") {
            Categories <- substring(liste_impacts_ACV$Indicators[20:26],16)
            fig <- plot_ly(labels = Categories, values = as.numeric(data_sante), type = 'pie',sort=F,direction = "clockwise",
                           textposition = "top right", textinfo = 'percent',
                           text = paste(gsub("e","E",signif(as.numeric(data_sante),4)),"DALY"),
                           hovertemplate = paste0("<b>%{label}</b>",
                                                  "<br>%{text}",
                                                  "<br>%{percent}<extra></extra>"))
          } 
          if(dommage == "Biodiv") {
            Categories <- substring(liste_impacts_ACV$Indicators[27:33],15)
            fig <- plot_ly(labels = Categories, values = as.numeric(data_biodiv), type = 'pie',sort=F,direction = "clockwise",
                           textposition = "top right",textinfo = 'percent',
                           text = paste(gsub("e","E",signif(as.numeric(data_biodiv),4)),"PDF.m².an"),
                           hovertemplate = paste0("<b>%{label}</b>",
                                                  "<br>%{text}",
                                                  "<br>%{percent}<extra></extra>"))
          }
          fig <- fig %>% layout(
            paper_bgcolor = "#ecf0f5",
            legend = list(font=list(size=16)),
            xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
            yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)
          )
        }
      }
      fig
    })
    
    output$download_MonoDommage1 <- downloadHandler(
      filename = "resultats_modemacv.xlsx",
      content = function(file) {write_xlsx(list("Résumé" = data.frame(`Export Modem_ACV` = c("Date","Type d'analyse","Nom du calcul","Vitesse", "Unité pour la santé","Unité pour la biodiversité"),
                                                                      Info = c(as.character(Sys.time()),"Analyse des dommages environnementaux",input$Input_Analyses_MonoDommage_ChoixConfig,
                                                                               MonoDommage_Export$vitesse,"DALY","PDF.m².an")),
                                                "Résultats santé humaine" = MonoDommage_Export$Export_Sante,
                                                "Résultats biodiversité" = MonoDommage_Export$Export_Biodiv),path = file)} 
    )
    
    output$DT_MonoDommages <- renderDT({
      table_dommage <- matrix(nrow = 2,ncol = 3)
      table_dommage[1,1] <- "Valeurs de normalisation"
      table_dommage[1,2] <- "0.03 DALY /personne/an"
      table_dommage[1,3] <- "90 000 PDF.m².an /personne/an"
      table_dommage[2,1] <- "Nb de kilomètres avant dépassement"
      table_dommage[2,2] <- paste0(format(round(0.03/MonoDommage_Export$Total_Sante,0),big.mark = " ")," km/personne/an")
      table_dommage[2,3] <- paste0(format(round(90000/MonoDommage_Export$Total_Biodiv,0),big.mark = " ")," km/personne/an")
      
      datatable(table_dommage,
                class = "hover cell-border compact",
                caption = "Normalisation :",
                selection = "none",
                colnames = c("","Santé humaine","Atteinte aux écosystèmes"),
                options = list(dom = 't',ordering=F,
                               pageLength = 30),
                rownames = F
      )
    })  
    
    output$GRAPH_MonoDommage_vitesse <- renderPlotly({
      
      config <- input$Input_Analyses_MonoDommage_ChoixConfig
      
      dommage <- input$Input_Analyses_MonoDommage_SanteBiodiv2
      
      if(is.null(config)) {
        fig <- plotly_empty(type = "scatter")
        fig <- fig %>% layout(title = "Aucun calcul réalisé")
      } else {
        Data_Impacts_parc <- REACT_list_Impacts_parc[[config]][["Total"]]
        if(!("Sante" %in% dimnames(Data_Impacts_parc)[[3]])) {
          fig <- plotly_empty(type = "scatter")
          fig <- fig %>% layout(title = "Indicateurs de dommages non-calculés")
        } else {
          
          data_Sante <- MonoDommage_Export$Sante
          data_Biodiv <- MonoDommage_Export$Biodiv
          Vitesse <- 5:130
          
          # Graphique (absolu et relatif)
          if(dommage == "Sante") {
            fig <- plot_ly(x = Vitesse, y = data_Sante[,1], name = substring(liste_impacts_ACV$Indicators[20],16), type = 'scatter', mode = 'none', stackgroup = 'one',
                           text =paste(gsub("e","E",signif(data_Sante[,1],4)),"DALY"),customdata = rep(substring(liste_impacts_ACV$Indicators[20],16),126),
                           hovertemplate = paste0("<b>%{customdata}</b>",
                                                  "<br>Vit : %{x} km/h",
                                                  "<br>%{text}<extra></extra>"))
            for(i in 2:ncol(data_Sante)) {
              fig <- fig %>% add_trace(y = data_Sante[,i], name = substring(liste_impacts_ACV$Indicators[19+i],16),
                                       text =paste(gsub("e","E",signif(data_Sante[,i],4)),"DALY"),customdata = rep(substring(liste_impacts_ACV$Indicators[19+i],16),126),
                                       hovertemplate = paste0("<b>%{customdata}</b>",
                                                              "<br>Vit : %{x} km/h",
                                                              "<br>%{text}<extra></extra>")) %>%
                layout(yaxis = list(title = "DALY",gridcolor = "b4b4b4",exponentformat = "E",minexponent = 3))
              next
            }
          } 
          if(dommage == "Biodiv") {
            fig <- plot_ly(x = Vitesse, y = data_Biodiv[,1], name = substring(liste_impacts_ACV$Indicators[27],15), type = 'scatter', mode = 'none', stackgroup = 'one',
                           text =paste(gsub("e","E",signif(data_Biodiv[,1],4)),"PDF.m².an"),customdata = rep(substring(liste_impacts_ACV$Indicators[27],15),126),
                           hovertemplate = paste0("<b>%{customdata}</b>",
                                                  "<br>Vit : %{x} km/h",
                                                  "<br>%{text}<extra></extra>"))
            
            for(i in 2:ncol(data_Biodiv)) {
              fig <- fig %>% add_trace(y = data_Biodiv[,i], name = substring(liste_impacts_ACV$Indicators[26+i],15),
                                       text =paste(gsub("e","E",signif(data_Biodiv[,i],4)),"PDF.m².an"),customdata = rep(substring(liste_impacts_ACV$Indicators[26+i],15),126),
                                       hovertemplate = paste0("<b>%{customdata}</b>",
                                                              "<br>Vit : %{x} km/h",
                                                              "<br>%{text}<extra></extra>")) %>%
                layout(yaxis = list(title = "PDF.m².an",gridcolor = "b4b4b4",exponentformat = "E",minexponent = 3))
              next
            }
          }
          fig <- fig %>% layout(
            title = 'Impacts en fonction de la vitesse',
            plot_bgcolor = "#d9d9d9",
            xaxis = list(title = 'vitesses (km/h)',rangemode = 'tozero',gridcolor = "#b4b4b4", dtick = 10),
            updatemenus = list(
              list(
                buttons = list(
                  list(method = 'restyle',
                       args = list('groupnorm',''),
                       label = 'Absolu'),
                  
                  list(method = 'restyle',
                       args = list('groupnorm','percent'),
                       label = 'Relatif')			
                )
              )
            )
          )
        }
      }
      fig
    })
    
    output$download_MonoDommage2 <- downloadHandler(
      filename = "resultats_modemacv.xlsx",
      content = function(file) {write_xlsx(list("Résumé" = data.frame(`Export Modem_ACV` = c("Date","Type d'analyse","Nom du calcul", "Unité pour la santé","Unité pour la biodiversité"),
                                                                      Info = c(as.character(Sys.time()),"Analyse des dommages environnementaux",input$Input_Analyses_MonoDommage_ChoixConfig,"DALY","PDF.m².an")),
                                                "Résultats santé humaine" = MonoDommage_Export$Export_Sante2,
                                                "Résultats biodiversité" = MonoDommage_Export$Export_Biodiv2),path = file)} 
    )
    
    # MultiContributions  ####
    output$MultiContributions_choix_calcul = renderUI({
      pickerInput(
        inputId = "Input_Analyses_MultiContributions_ChoixConfig",
        label = "Sélection du calcul :",
        width = 400,
        choices = REACT_list_calculs$nom,
        options = list(
          style = "btn-danger",
          pickerOptions(noneSelectedText = "Aucun calcul"))
      )
      
    })
    output$MultiContributions_Indicateurs = renderUI({
      
      pickerInput(
        inputId = "Input_Analyses_MultiContributions_Indicateurs",
        label = "Indicateur observé :",
        width = 400,
        choices = as.character(liste_impacts_ACV[dimnames(REACT_list_Impacts_parc[[input$Input_Analyses_MultiContributions_ChoixConfig]][["Total"]] %>%
                                                            multi_select(c("Biodiv_GWP","Biodiv_Ion","Biodiv_Ecotox","Biodiv_Eutro","Biodiv_Acid","Biodiv_Sol","Biodiv_Eau",
                                                                           "Sante_GWP","Sante_Ion","Sante_PMF","Sante_POF","Sante_Ozon","Sante_Htox","Sante_Eau"),'-',3))[[3]],"Indicators"]) ,
        options = list(
          style = "btn-primary",
          pickerOptions(noneSelectedText = "Aucun calcul")),
        multiple = TRUE,
        selected = as.character(liste_impacts_ACV[dimnames(REACT_list_Impacts_parc[[input$Input_Analyses_MultiContributions_ChoixConfig]][["Total"]] %>%
                                                             multi_select(c("Biodiv_GWP","Biodiv_Ion","Biodiv_Ecotox","Biodiv_Eutro","Biodiv_Acid","Biodiv_Sol","Biodiv_Eau",
                                                                            "Sante_GWP","Sante_Ion","Sante_PMF","Sante_POF","Sante_Ozon","Sante_Htox","Sante_Eau"),'-',3))[[3]],"Indicators"])
      )
      
    })
    output$MultiContributions_vitessereseau = renderText({
      if(is.null(input$Input_Analyses_MultiContributions_ChoixConfig)) {""} else {
        paste("Estimation de la vitesse du réseau : ",
              vitesse_reseau(REACT_list_config[[input$Input_Analyses_MultiContributions_ChoixConfig]]),
              " km/h")
      }
      
    })
    
    MultiContributions_Export <- reactiveValues(Export = 0,vitesse = 0)
    output$GRAPH_MultiContributions <- renderPlotly({
      
      config <- input$Input_Analyses_MultiContributions_ChoixConfig 
      impacts <- as.character(liste_impacts_ACV[which(liste_impacts_ACV$Indicators %in% input$Input_Analyses_MultiContributions_Indicateurs ), "Abrev"])
      impacts_unite <- as.character(liste_impacts_ACV[which(liste_impacts_ACV$Indicators %in% input$Input_Analyses_MultiContributions_Indicateurs ), "Unit_D3"])
      phase_ACV <- input$Input_Analyses_MultiContributions_phaseACV_TRUE
      aggreg <- input$Input_Analyses_MultiContributions_MotoEuroSegment
      
      choix_vitesse <- input$Input_Analyses_MultiContributions_ChoixVitesses
      vitesse <-input$Input_Analyses_MultiContributions_vitesse  
      
      if(is.null(config) ) {
        fig <- plotly_empty(type = "bar")
        fig <- fig %>% layout(title = "Aucun calcul réalisé")
        fig
      } else {
        
        Data_Impacts_parc <- REACT_list_Impacts_parc[[config]][["Total"]] 
        Data_Parc_Utilisateur <- REACT_list_config[[config]][["Parc_utilisateur"]]
        
        if(choix_vitesse == "perso") {
          data_vit <- Data_Impacts_parc[,as.character(vitesse),impacts,-1]
          MultiContributions_Export$vitesse <- paste0("Vitesse :",as.character(vitesse)," km/h")
        } else if(choix_vitesse == "reseau") {
          usage_reseau <- REACT_list_config[[config]][["Infra_usage"]]$Utilisation
          vitesses_reseau <- apply(distrib_vitesse,1,function(i) sum(i*usage_reseau))
          data_vit <- apply(Data_Impacts_parc[,,impacts,-1], MARGIN = c(1,3,4), function(i) sum(i*vitesses_reseau))
          MultiContributions_Export$vitesse <- paste0("Vitesse du réseau :",vitesse_reseau(REACT_list_config[[input$Input_Analyses_MultiContributions_ChoixConfig]])," km/h")
        } else {
          data_vit <- apply(Data_Impacts_parc[,,impacts,-1], MARGIN = c(1,3,4), function(i) sum(i*distrib_vitesse[,choix_vitesse]))
          if(choix_vitesse == "lent") {MultiContributions_Export$vitesse <- "Vitesse lente : 32 km/h"}
          if(choix_vitesse == "moyen") {MultiContributions_Export$vitesse <- "Vitesse moyenne : 51 km/h"}
          if(choix_vitesse == "rapide") {MultiContributions_Export$vitesse <- "Vitesse rapide : 72 km/h"}
          if(choix_vitesse == "très.rapide") {MultiContributions_Export$vitesse <- "Vitesse très rapide : 106 km/h"}
        }
        
        if(phase_ACV) {
          data <- apply(data_vit,2:3,sum)
        } else {
          list_aggreg <- list()
          if("Moto" %in% aggreg) {
            list_aggreg <- c(list_aggreg,list(Data_Parc_Utilisateur$Fuel))
          }
          if("Euro" %in% aggreg) {
            list_aggreg <- c(list_aggreg,list(Data_Parc_Utilisateur$Euro.Standard))
          }
          if("Segment" %in% aggreg) {
            list_aggreg <- c(list_aggreg,list(Data_Parc_Utilisateur$Segment))
          }
          data <- t(Aggreg_veh(apply(data_vit,1:2,sum),list_aggreg, sum))
        }
        rownames(data) <- liste_impacts_ACV[as.character(rownames(data)),"Indicators"]
        data_relatif <- data/rowSums(data)
        data_relatif <- data.frame(Impacts = rownames(data), data_relatif)
        names(data_relatif) <- renames_all(names(data_relatif))
        temp_colors <- couleurs$HEX_trait[match(names(data_relatif)[-1],couleurs$Nom)]
        annotations <- list()
        for(i in 1:nrow(data)) {
          annotations[[i]] <- list(x = as.character(rownames(data)[i]),
                                   y = rep(1.5,nrow(data)),
                                   text = paste(signif(rowSums(data)[i],3),"<br>",liste_impacts_ACV[match(as.character(rownames(data)[i]),liste_impacts_ACV[,"Indicators"]),'Unit_D3'], sep = " "),
                                   yanchor = 'bottom',
                                   showarrow = FALSE)
          next
        }
        # Graph
        fig <- plot_ly(data_relatif,x = ~Impacts, y = data_relatif[,2], type = 'bar', 
                       name = names(data_relatif)[2],marker = list(color = temp_colors[2-1])
                       ,
                       text = paste(gsub("e","E",signif(data[,2-1],3)),impacts_unite),
                       hovertemplate = paste0("<b>%{x}</b>",
                                              "<br>%{text}",
                                              "<br>%{y}")
                       )
        for(i in 3:ncol(data_relatif)) {
          fig <- fig %>% add_trace(y = data_relatif[,i], 
                                   name = names(data_relatif)[i],marker = list(color = temp_colors[i-1])
                                   ,
                                   text = paste(gsub("e","E",signif(data[,i-1],3)),impacts_unite),
                                   hovertemplate = paste0("<b>%{x}</b>",
                                                          "<br>%{text}",
                                                          "<br>%{y}")
                                   )
          next
        }
        fig <- fig %>% layout( 
          barmode = 'stack',
          xaxis = list(title = "Catégories d'impacts environnementaux",categoryorder = "array",categoryarray = as.character(data_relatif[,"Impacts"])),
          yaxis = list(tickformat = "%"),
          annotations = annotations,
          legend = list(x = 100, y = 0.5)
        )
        
        MultiContributions_Export$Export = data.frame(data)
        fig
      }
      
    })
    output$download_MultiContributions <- downloadHandler(
      filename = "resultats_modemacv.xlsx",
      content = function(file) {write_xlsx(list("Résumé" = data.frame(`Export Modem_ACV` = c("Date","Type d'analyse","Nom du calcul","Vitesse", "Liste des impacts évalués",rep(" ",length(input$Input_Analyses_MultiContributions_Indicateurs)-1)),
                                                                      Info = c(as.character(Sys.time()),"Analyse multicritère des contributions",input$Input_Analyses_MultiContributions_ChoixConfig,MultiContributions_Export$vitesse,
                                                                               paste0(input$Input_Analyses_MultiContributions_Indicateurs," (",as.character(liste_impacts_ACV[match(input$Input_Analyses_MultiContributions_Indicateurs,as.character(liste_impacts_ACV$Indicators)),"Units"]),")"))),
                                                "Résultats" = MultiContributions_Export$Export),path = file)} 
    )
    # MultiCompaVehicules ####
    output$MultiCompaVehicules_choix_calcul = renderUI({
      pickerInput(
        inputId = "Input_Analyses_MultiCompaVehicules_ChoixConfig",
        label = "Sélection du calcul :",
        width = 400,
        choices = REACT_list_calculs$nom,
        options = list(
          style = "btn-danger",
          pickerOptions(noneSelectedText = "Aucun calcul"))
      )
      
    })
    output$MultiCompaVehicules_Indicateurs = renderUI({
      
      pickerInput(
        inputId = "Input_Analyses_MultiCompaVehicules_Indicateurs",
        label = "Indicateur observé :",
        width = 400,
        choices = as.character(liste_impacts_ACV[dimnames(REACT_list_Impacts_parc[[input$Input_Analyses_MultiCompaVehicules_ChoixConfig]][["Total"]] %>%
                                                            multi_select(c("Biodiv_GWP","Biodiv_Ion","Biodiv_Ecotox","Biodiv_Eutro","Biodiv_Acid","Biodiv_Sol","Biodiv_Eau",
                                                                           "Sante_GWP","Sante_Ion","Sante_PMF","Sante_POF","Sante_Ozon","Sante_Htox","Sante_Eau"),'-',3))[[3]],"Indicators"]) ,
        options = list(
          `actions-box` = TRUE,
          `deselect-all-text` = "Désélectionner tous les indicateurs",
          `select-all-text` = "Sélectionner tous les indicateurs",
          `none-selected-text` = "Aucun indicateurs sélectionnés",
          style = "btn-primary"
        ),
        multiple = TRUE,
        selected = as.character(liste_impacts_ACV[dimnames(REACT_list_Impacts_parc[[input$Input_Analyses_MultiCompaVehicules_ChoixConfig]][["Total"]] %>%
                                                             multi_select(c("Biodiv_GWP","Biodiv_Ion","Biodiv_Ecotox","Biodiv_Eutro","Biodiv_Acid","Biodiv_Sol","Biodiv_Eau",
                                                                            "Sante_GWP","Sante_Ion","Sante_PMF","Sante_POF","Sante_Ozon","Sante_Htox","Sante_Eau"),'-',3))[[3]],"Indicators"])
      )
      
    })
    output$MultiCompaVehicules_vitessereseau = renderText({
      if(is.null(input$Input_Analyses_MultiCompaVehicules_ChoixConfig)) {""} else {
        paste("Estimation de la vitesse du réseau : ",
              vitesse_reseau(REACT_list_config[[input$Input_Analyses_MultiCompaVehicules_ChoixConfig]]),
              " km/h")
      }
      
    })
    
    MultiCompaVehicules_Export <- reactiveValues(Export = 0,vitesse = 0)
    output$GRAPH_MultiCompaVehicules_bar <- renderPlotly({
      
      # input
      config <- input$Input_Analyses_MultiCompaVehicules_ChoixConfig 
      impacts <- as.character(liste_impacts_ACV[which(liste_impacts_ACV$Indicators %in% input$Input_Analyses_MultiCompaVehicules_Indicateurs ), "Abrev"])
      impacts_unite <- as.character(liste_impacts_ACV[which(liste_impacts_ACV$Indicators %in% input$Input_Analyses_MultiCompaVehicules_Indicateurs ), "Units"])
      
      choix_vitesse <- input$Input_Analyses_MultiCompaVehicules_ChoixVitesses
      vitesse <-input$Input_Analyses_MultiCompaVehicules_vitesse
      type_veh <- input$Input_Analyses_MultiCompaVehicules_MotoEuroSegment
      
      if(is.null(config) ) {
        fig <- plotly_empty(type = "bar")
        fig <- fig %>% layout(title = "Aucun calcul réalisé")
        fig
      } else {
        
        Data_Impacts_parc <- REACT_list_Impacts_parc[[config]][["Total"]] 
        Data_Parc_Utilisateur <- REACT_list_config[[config]][["Parc_utilisateur"]]
        
        if(choix_vitesse == "perso") {
          data <- Data_Impacts_parc[,as.character(vitesse),impacts,1]
          MultiCompaVehicules_Export$vitesse <- paste0("Vitesse :",as.character(vitesse)," km/h")
        } else if(choix_vitesse == "reseau") {
          usage_reseau <- REACT_list_config[[config]][["Infra_usage"]]$Utilisation
          vitesses_reseau <- apply(distrib_vitesse,1,function(i) sum(i*usage_reseau))
          MultiCompaVehicules_Export$vitesse <- paste0("Vitesse du réseau :",vitesse_reseau(REACT_list_config[[input$Input_Analyses_MultiCompaVehicules_ChoixConfig]])," km/h")
          data <- apply(Data_Impacts_parc[,,impacts,1], MARGIN = c(1,3), function(i) sum(i*vitesses_reseau))
        } else {
          data <- apply(Data_Impacts_parc[,,impacts,1], MARGIN = c(1,3), function(i) sum(i*distrib_vitesse[,choix_vitesse]))
          if(choix_vitesse == "lent") {MultiCompaVehicules_Export$vitesse <- "Vitesse lente : 32 km/h"}
          if(choix_vitesse == "moyen") {MultiCompaVehicules_Export$vitesse <- "Vitesse moyenne : 51 km/h"}
          if(choix_vitesse == "rapide") {MultiCompaVehicules_Export$vitesse <- "Vitesse rapide : 72 km/h"}
          if(choix_vitesse == "très.rapide") {MultiCompaVehicules_Export$vitesse <- "Vitesse très rapide : 106 km/h"}
        }
        
        list_aggreg <- list()
        if("Moto" %in% type_veh) {
          list_aggreg <- c(list_aggreg,list(Data_Parc_Utilisateur$Fuel))
        }
        if("Euro" %in% type_veh) {
          list_aggreg <- c(list_aggreg,list(Data_Parc_Utilisateur$Euro.Standard))
        }
        if("Segment" %in% type_veh) {
          list_aggreg <- c(list_aggreg,list(Data_Parc_Utilisateur$Segment))
        }
        
        data <- data.frame(Aggreg_veh(data,list_aggreg,sum))
        part_parc <- aggregate(Data_Parc_Utilisateur$Part, list_aggreg, sum)[which(aggregate(Data_Parc_Utilisateur$Part, list_aggreg, sum)[,"x"] != 0),"x"]
        data <- data.frame(type_veh = c(rownames(data),"Moyenne parc"),rbind(data/part_parc,colSums(data)))
        rownames(data) <- data$type_veh
        data_relatif <- t(data[,-1])/as.numeric(data["Moyenne parc",-1])
        data_relatif <- data.frame(Impacts = liste_impacts_ACV[as.character(rownames(data_relatif)),"Indicators"], data_relatif)
        
        names(data_relatif) <- renames_all(names(data_relatif))
        names(data_relatif)[ncol(data_relatif)] <- "Moyenne parc"
        data_relatif <- data_relatif[,c(1,1+tri_perso(names(data_relatif)[-1],couleurs$Nom))]
        
        temp_color <- couleurs$HEX_trait[match(names(data_relatif)[-1],couleurs$Nom)]
        temp_color[length(temp_color)] <- "#b4b4b4"
        
        # Graphique
        fig <- plot_ly(data_relatif, type = 'bar',
                       hovertemplate = paste0("<b>%{x}</b>",
                                              "<br>%{fullData.name}",
                                              "<br>%{y} : %{text} <extra></extra>"))
        for(i in 2:ncol(data_relatif)) {
          fig <- fig %>% add_trace(x = ~Impacts,y = data_relatif[,i], name = names(data_relatif)[i],
                                   marker = list(color = temp_color[i-1]),
                                   text = paste(gsub("e","E",signif(data[i-1,-1],3)),impacts_unite))
          next
        }
        fig <- fig %>% layout(
          title = "Analyse multicritère des différents véhicules",
          margin = list(t=50),
          xaxis = list(title = "Impacts environnementaux",categoryorder = "array",categoryarray = as.character(data_relatif[,"Impacts"])),
          yaxis = list(tickformat = "%", range = c(0,2),linecolor = "#d9d9d9")
        )
        
        MultiCompaVehicules_Export$Export = data.frame(Impacts = data_relatif$Impacts[1:length(impacts)], t(data[,-1]))
        fig
      }
      
    })
    output$GRAPH_MultiCompaVehicules_scatterpolar <- renderPlotly({
      
      # input
      config <- input$Input_Analyses_MultiCompaVehicules_ChoixConfig 
      impacts <- as.character(liste_impacts_ACV[which(liste_impacts_ACV$Indicators %in% input$Input_Analyses_MultiCompaVehicules_Indicateurs ), "Abrev"])
      impacts_unite <- as.character(liste_impacts_ACV[which(liste_impacts_ACV$Indicators %in% input$Input_Analyses_MultiCompaVehicules_Indicateurs ), "Unit_D3"])
      
      choix_vitesse <- input$Input_Analyses_MultiCompaVehicules_ChoixVitesses
      vitesse <-input$Input_Analyses_MultiCompaVehicules_vitesse
      type_veh <- input$Input_Analyses_MultiCompaVehicules_MotoEuroSegment
      
      if(is.null(config) ) {
        fig <- plotly_empty(type = "bar")
        fig <- fig %>% layout(title = "Aucun calcul réalisé")
        fig
      } else {
        
        Data_Impacts_parc <- REACT_list_Impacts_parc[[config]][["Total"]] 
        Data_Parc_Utilisateur <- REACT_list_config[[config]][["Parc_utilisateur"]]
        
        if(choix_vitesse == "perso") {
          data <- Data_Impacts_parc[,as.character(vitesse),impacts,1]
          MultiCompaVehicules_Export$vitesse <- paste0("Vitesse :",as.character(vitesse)," km/h")
        } else if(choix_vitesse == "reseau") {
          usage_reseau <- REACT_list_config[[config]][["Infra_usage"]]$Utilisation
          vitesses_reseau <- apply(distrib_vitesse,1,function(i) sum(i*usage_reseau))
          MultiCompaVehicules_Export$vitesse <- paste0("Vitesse du réseau :",vitesse_reseau(REACT_list_config[[input$Input_Analyses_MultiCompaVehicules_ChoixConfig]])," km/h")
          data <- apply(Data_Impacts_parc[,,impacts,1], MARGIN = c(1,3), function(i) sum(i*vitesses_reseau))
        } else {
          data <- apply(Data_Impacts_parc[,,impacts,1], MARGIN = c(1,3), function(i) sum(i*distrib_vitesse[,choix_vitesse]))
          if(choix_vitesse == "lent") {MultiCompaVehicules_Export$vitesse <- "Vitesse lente : 32 km/h"}
          if(choix_vitesse == "moyen") {MultiCompaVehicules_Export$vitesse <- "Vitesse moyenne : 51 km/h"}
          if(choix_vitesse == "rapide") {MultiCompaVehicules_Export$vitesse <- "Vitesse rapide : 72 km/h"}
          if(choix_vitesse == "très.rapide") {MultiCompaVehicules_Export$vitesse <- "Vitesse très rapide : 106 km/h"}
        }
        
        list_aggreg <- list()
        if("Moto" %in% type_veh) {
          list_aggreg <- c(list_aggreg,list(Data_Parc_Utilisateur$Fuel))
        }
        if("Euro" %in% type_veh) {
          list_aggreg <- c(list_aggreg,list(Data_Parc_Utilisateur$Euro.Standard))
        }
        if("Segment" %in% type_veh) {
          list_aggreg <- c(list_aggreg,list(Data_Parc_Utilisateur$Segment))
        }
        
        data <- data.frame(Aggreg_veh(data,list_aggreg,sum))
        part_parc <- aggregate(Data_Parc_Utilisateur$Part, list_aggreg, sum)[which(aggregate(Data_Parc_Utilisateur$Part, list_aggreg, sum)[,"x"] != 0),"x"]
        # data <- data.frame(type_veh = c(rownames(data),"Moyenne parc"),rbind(data/part_parc,colSums(data)))
        # rownames(data) <- renames_all(data$type_veh)
        # data <- data[tri_perso(rownames(data),couleurs$Nom),]
        # data_relatif <- t(data[,-1])/as.numeric(data["Moyenne parc",-1])
        # data_relatif <- data.frame(Impacts = liste_impacts_ACV[as.character(rownames(data_relatif)),"Indicators"], data_relatif)
        data <- data.frame(type_veh = c(rownames(data),"Moyenne parc"),rbind(data/part_parc,colSums(data)))
        rownames(data) <- data$type_veh
        data_relatif <- t(data[,-1])/as.numeric(data["Moyenne parc",-1])
        data_relatif <- data.frame(Impacts = liste_impacts_ACV[as.character(rownames(data_relatif)),"Indicators"], data_relatif)
        
        names(data_relatif) <- renames_all(names(data_relatif))
        names(data_relatif)[ncol(data_relatif)] <- "Moyenne parc"
        data_relatif <- data_relatif[,c(1,1+tri_perso(names(data_relatif)[-1],couleurs$Nom))]
        
        # Graphique
        data_radar <- rbind(data_relatif,data_relatif[1,])
        temp_color <- couleurs$HEX_trait[match(names(data_radar)[-1],couleurs$Nom)]
        temp_color[length(temp_color)] <- "#000000"
        temp_dash <- c(rep("solid",length(temp_color)-1),"dot")
        fig <- plot_ly(data_radar, type = 'scatterpolar', mode = "lines",hovertemplate = paste0("<b>%{theta}</b>",
                                                                                                "<br>%{fullData.name}",
                                                                                                "<br>%{r} : %{text} <extra></extra>"))
        for(i in 2:ncol(data_radar)) {
          fig <- fig %>% add_trace(theta = ~Impacts, r = data_radar[,i], 
                                   name = names(data_radar)[i],text = paste(gsub("e","E",signif(data[i-1,c(2:ncol(data),2)],3)),impacts_unite),
                                   line = list(color = temp_color[i-1] ,width = 3,dash = temp_dash[i-1]))
          next
        }
        fig <- fig %>% layout( 
          title = "Analyse multicritère des différents véhicules",
          margin = list(t=65),
          polar = list(
            radialaxis = list(
              visible = TRUE,linecolor="#d0d0d0",
              angle = 90,
              tickangle = 90,
              tickformat = "%"
            ),
            angularaxis = list(
              rotation = 90,linecolor="#d0d0d0",
              direction = 'clockwise')
          )
        )
        MultiCompaVehicules_Export$Export = data.frame(Impacts = data_relatif$Impacts[1:length(impacts)], t(data[,-1]))
        fig
      }
      
    })
    
    output$download_MultiCompaVehicules <- downloadHandler(
      filename = "resultats_modemacv.xlsx",
      content = function(file) {write_xlsx(list("Résumé" = data.frame(`Export Modem_ACV` = c("Date","Type d'analyse","Nom du calcul", "Vitesse","Liste des impacts évalués",rep(" ",length(input$Input_Analyses_MultiCompaVehicules_Indicateurs)-1)),
                                                                      Info = c(as.character(Sys.time()),"Analyse multicritère des véhicules",input$Input_Analyses_MultiCompaVehicules_ChoixConfig,
                                                                               MultiCompaVehicules_Export$vitesse,
                                                                               paste0(input$Input_Analyses_MultiCompaVehicules_Indicateurs," (",as.character(liste_impacts_ACV[match(input$Input_Analyses_MultiCompaVehicules_Indicateurs,as.character(liste_impacts_ACV$Indicators)),"Units"]),")"))),
                                                "Résultats" = MultiCompaVehicules_Export$Export),path = file)} 
    )
    # MultiCompaConfigs  ####
    output$MultiCompaConfigs_choix_calcul = renderUI({
      pickerInput(
        inputId = "Input_Analyses_MultiCompaConfigs_ChoixConfig",
        label = "Sélection du calcul :",
        width = 400,
        choices = REACT_list_calculs$nom,
        options = list(
          style = "btn-danger",
          pickerOptions(noneSelectedText = "Aucun calcul")),
        multiple = TRUE,
        selected = REACT_list_calculs$nom
      )
      
    })
    output$MultiCompaConfigs_Indicateurs = renderUI({
      list_impact <- list()
      for(conf in input$Input_Analyses_MonoCompaParcs_ChoixConfig) {
        list_impact[[conf]] <- REACT_list_config[[conf]][["liste_impacts_ACV"]]$Abrev
        next
      }
      list_impact <- c(list_impact,list(liste_impacts_ACV$Abrev))
      list_impact <- Reduce(intersect,list_impact)
      
      pickerInput(
        inputId = "Input_Analyses_MultiCompaConfigs_Indicateurs",
        label = "Indicateur observé :",
        width = 400,
        choices = as.character(liste_impacts_ACV[list_impact,"Indicators"]) ,
        options = list(
          `actions-box` = TRUE,
          `deselect-all-text` = "Désélectionner tous les indicateurs",
          `select-all-text` = "Sélectionner tous les indicateurs",
          `none-selected-text` = "Aucun indicateurs sélectionnés",
          style = "btn-primary"
        ),
        multiple = TRUE,
        selected = as.character(liste_impacts_ACV[list_impact,"Indicators"])
      )
      
    })
    
    MultiCompaConfigs_Export <- reactiveValues(Export = 0,vitesse = 0)
    output$GRAPH_MultiCompaConfigs_bar <- renderPlotly({
      
      # input
      config <- input$Input_Analyses_MultiCompaConfigs_ChoixConfig 
      impacts <- as.character(liste_impacts_ACV[which(liste_impacts_ACV$Indicators %in% input$Input_Analyses_MultiCompaConfigs_Indicateurs ), "Abrev"])
      impacts_unite <- as.character(liste_impacts_ACV[which(liste_impacts_ACV$Indicators %in% input$Input_Analyses_MultiCompaConfigs_Indicateurs ), "Unit_D3"])
      choix_vitesse <- input$Input_Analyses_MultiCompaConfigs_ChoixVitesses
      vitesse <- input$Input_Analyses_MultiCompaConfigs_vitesse
      
      if(is.null(config) ) {
        fig <- plotly_empty(type = "bar")
        fig <- fig %>% layout(title = "Aucun calcul réalisé")
        fig
      } else if(length(config) == 1)  {
        fig <- plotly_empty(type = "bar")
        fig <- fig %>% layout(title = "Ajouter un autre calcul")
        fig
      } else if(length(impacts) == 1)  {
        fig <- plotly_empty(type = "bar")
        fig <- fig %>% layout(title = "Ajouter des indicateurs")
        fig
      } else {
        
        Data_Impacts_parc <- list()
        for(i in 1:length(config)) {
          
          Data_Impacts_parc <- c(
            Data_Impacts_parc,
            list(REACT_list_Impacts_parc[[config[i]]][["Total"]][,,impacts,])
          ) 
          next
        }
        names(Data_Impacts_parc) <- config
        
        # Preparation data
        data_vit <- apply(Data_Impacts_parc[[1]][,,,1],MARGIN = 2:3,sum)
        for(i in 2:length(Data_Impacts_parc)) {
          aa <- Data_Impacts_parc[[i]]
          data_vit <- abind(data_vit, apply(aa[,,,1],MARGIN = 2:3,sum), along = 3)
          next
        }
        
        if(choix_vitesse == "perso") {
          data_vit <- data_vit[as.character(vitesse),,]
          MultiCompaConfigs_Export$vitesse <- paste0("Vitesse :",as.character(vitesse)," km/h")
        } else {
          data_vit <- apply(data_vit, MARGIN = c(2,3), function(i) sum(i*distrib_vitesse[,choix_vitesse]))
          if(choix_vitesse == "lent") {MultiCompaConfigs_Export$vitesse <- "Vitesse lente : 32 km/h"}
          if(choix_vitesse == "moyen") {MultiCompaConfigs_Export$vitesse <- "Vitesse moyenne : 51 km/h"}
          if(choix_vitesse == "rapide") {MultiCompaConfigs_Export$vitesse <- "Vitesse rapide : 72 km/h"}
          if(choix_vitesse == "très.rapide") {MultiCompaConfigs_Export$vitesse <- "Vitesse très rapide : 106 km/h"}
        }
        
        data_relatif <- data.frame(liste_impacts_ACV[as.character(rownames(data_vit)),"Indicators"], data_vit/as.numeric(apply(data_vit,1,max)))
        
        data_vit <- data.frame(liste_impacts_ACV[as.character(rownames(data_vit)),"Indicators"],data_vit)
        names(data_relatif) <- c("Impacts", as.character(config))
        
        names(data_vit) <- names(data_relatif)
        
        # Graphique
        fig <- plot_ly(data_relatif, type = 'bar',
                       hovertemplate = paste0("<b>%{x}</b>",
                                              "<br>Parc : %{fullData.name}",
                                              "<br>%{y} : %{text} <extra></extra>"))
        for(i in 2:ncol(data_relatif)) {
          fig <- fig %>% add_trace(x = ~Impacts,y = data_relatif[,i], name = names(data_relatif)[i],
                                   text = paste(gsub("e","E",signif(data_vit[,i],3)),impacts_unite))
          next
        }
        fig <- fig %>% layout( 
          title = paste0("Analyse multicritère de différents parcs",
                         '<br><i>(100% pour le maximum)</i>'),
          margin = list(t=50),
          xaxis = list(title = "Impacts environnementaux",categoryorder = "array",categoryarray = as.character(data_relatif[,"Impacts"])),
          yaxis = list(tickformat = "%",linecolor = "#d9d9d9")
        )
        MultiCompaConfigs_Export$Export = data.frame(data_vit)
        fig
      }
      
    })
    output$GRAPH_MultiCompaConfigs_scatterpolar <- renderPlotly({
      
      # input
      config <- input$Input_Analyses_MultiCompaConfigs_ChoixConfig 
      impacts <- as.character(liste_impacts_ACV[which(liste_impacts_ACV$Indicators %in% input$Input_Analyses_MultiCompaConfigs_Indicateurs ), "Abrev"])
      impacts_unite <- as.character(liste_impacts_ACV[which(liste_impacts_ACV$Indicators %in% input$Input_Analyses_MultiCompaConfigs_Indicateurs ), "Units"])
      choix_vitesse <- input$Input_Analyses_MultiCompaConfigs_ChoixVitesses
      vitesse <- input$Input_Analyses_MultiCompaConfigs_vitesse
      
      if(is.null(config) ) {
        fig <- plotly_empty(type = "bar")
        fig <- fig %>% layout(title = "Aucun calcul réalisé")
        fig
      } else if(length(config) == 1)  {
        fig <- plotly_empty(type = "bar")
        fig <- fig %>% layout(title = "Ajouter un autre calcul")
        fig
      } else if(length(impacts) == 1)  {
        fig <- plotly_empty(type = "bar")
        fig <- fig %>% layout(title = "Ajouter des indicateurs")
        fig
      } else {
        
        Data_Impacts_parc <- list()
        for(i in 1:length(config)) {
          
          Data_Impacts_parc <- c(
            Data_Impacts_parc,
            list(REACT_list_Impacts_parc[[config[i]]][["Total"]][,,impacts,])
          ) 
          next
        }
        names(Data_Impacts_parc) <- config
        
        # Preparation data
        data_vit <- apply(Data_Impacts_parc[[1]][,,,1],MARGIN = 2:3,sum)
        for(i in 2:length(Data_Impacts_parc)) {
          aa <- Data_Impacts_parc[[i]]
          data_vit <- abind(data_vit, apply(aa[,,,1],MARGIN = 2:3,sum), along = 3)
          next
        }
        
        if(choix_vitesse == "perso") {
          data_vit <- data_vit[as.character(vitesse),,]
          MultiCompaConfigs_Export$vitesse <- paste0("Vitesse :",as.character(vitesse)," km/h")
        } else {
          data_vit <- apply(data_vit, MARGIN = c(2,3), function(i) sum(i*distrib_vitesse[,choix_vitesse]))
          if(choix_vitesse == "lent") {MultiCompaConfigs_Export$vitesse <- "Vitesse lente : 32 km/h"}
          if(choix_vitesse == "moyen") {MultiCompaConfigs_Export$vitesse <- "Vitesse moyenne : 51 km/h"}
          if(choix_vitesse == "rapide") {MultiCompaConfigs_Export$vitesse <- "Vitesse rapide : 72 km/h"}
          if(choix_vitesse == "très.rapide") {MultiCompaConfigs_Export$vitesse <- "Vitesse très rapide : 106 km/h"}
        }
        
        data_relatif <- data.frame(liste_impacts_ACV[as.character(rownames(data_vit)),"Indicators"], data_vit/as.numeric(apply(data_vit,1,max)))
        
        data_vit <- data.frame(liste_impacts_ACV[as.character(rownames(data_vit)),"Indicators"],data_vit)
        names(data_relatif) <- c("Impacts", as.character(config))
        
        names(data_vit) <- names(data_relatif)
        data_radar <- rbind(data_relatif,data_relatif[1,])
        
        # Graphique
        fig <- plot_ly(data_radar, type = 'scatterpolar', mode = "lines",
                       hovertemplate = paste0("<b>%{theta}</b>",
                                              "<br>Parc : %{fullData.name}",
                                              "<br>%{r} : %{text} <extra></extra>"))
        for(i in 2:ncol(data_radar)) {
          fig <- fig %>% add_trace(theta = ~Impacts, r = data_radar[,i], 
                                   name = names(data_radar)[i],line = list( width = 3),
                                   text = paste(gsub("e","E",signif(data_vit[c(1:nrow(data_vit),1),i],3)),impacts_unite))
          next
        }
        fig <- fig %>% layout( 
          title = paste0("Analyse multicritère de différents parcs",
                         '<br><i>(100% pour le maximum)</i>'),
          margin = list(t=100),
          polar = list(
            radialaxis = list(
              visible = TRUE,linecolor="#d0d0d0",
              angle = 90,
              tickangle = 90,
              tickformat = "%"
            ),
            angularaxis = list(
              rotation = 90,linecolor="#d0d0d0",
              direction = 'clockwise')
          )
        )
        
        MultiCompaConfigs_Export$Export = data.frame(data_vit)
        fig
      }
      
    })
    output$download_MultiCompaConfigs <- downloadHandler(
      filename = "resultats_modemacv.xlsx",
      content = function(file) {write_xlsx(list("Résumé" = data.frame(`Export Modem_ACV` = c("Date","Type d'analyse","Nom des calculs", "Vitesse","Liste des impacts évalués",rep(" ",length(input$Input_Analyses_MultiCompaConfigs_Indicateurs)-1)),
                                                                      Info = c(as.character(Sys.time()),"Analyse multicritère de parcs",paste0(input$Input_Analyses_MultiCompaConfigs_ChoixConfig,collapse = " ; "),
                                                                               MultiCompaConfigs_Export$vitesse,
                                                                               paste0(input$Input_Analyses_MultiCompaConfigs_Indicateurs," (",as.character(liste_impacts_ACV[match(input$Input_Analyses_MultiCompaConfigs_Indicateurs,as.character(liste_impacts_ACV$Indicators)),"Units"]),")"))),
                                                "Résultats" = MultiCompaConfigs_Export$Export),path = file)} 
    )  
    
    # MultiACVdetail -   ####
    
    output$MultiACVdetail_choix_calcul = renderUI({
      pickerInput(
        inputId = "Input_Analyses_MultiACVdetail_ChoixConfig",
        label = "Sélection du calcul :",
        width = 400,
        choices = REACT_list_calculs$nom,
        options = list(
          style = "btn-danger",
          pickerOptions(noneSelectedText = "Aucun calcul"))
      )
      
    })
    output$MultiACVdetail_Indicateurs = renderUI({
      
      pickerInput(
        inputId = "Input_Analyses_MultiACVdetail_Indicateurs",
        label = "Indicateur observé :",
        width = 400,
        choices = as.character(liste_impacts_ACV[dimnames(REACT_list_Impacts_parc[[input$Input_Analyses_MultiACVdetail_ChoixConfig[1]]][["Total"]] %>%
                                                            multi_select(c("Biodiv_GWP","Biodiv_Ion","Biodiv_Ecotox","Biodiv_Eutro","Biodiv_Acid","Biodiv_Sol","Biodiv_Eau",
                                                                           "Sante_GWP","Sante_Ion","Sante_PMF","Sante_POF","Sante_Ozon","Sante_Htox","Sante_Eau"),'-',3))[[3]],"Indicators"]) ,
        options = list(
          `actions-box` = TRUE,
          `deselect-all-text` = "Désélectionner tous les indicateurs",
          `select-all-text` = "Sélectionner tous les indicateurs",
          `none-selected-text` = "Aucun indicateurs sélectionnés",
          style = "btn-primary"
        ),
        multiple = TRUE,
        selected = as.character(liste_impacts_ACV[dimnames(REACT_list_Impacts_parc[[input$Input_Analyses_MultiACVdetail_ChoixConfig[1]]][["Total"]] %>%
                                                             multi_select(c("Biodiv_GWP","Biodiv_Ion","Biodiv_Ecotox","Biodiv_Eutro","Biodiv_Acid","Biodiv_Sol","Biodiv_Eau",
                                                                            "Sante_GWP","Sante_Ion","Sante_PMF","Sante_POF","Sante_Ozon","Sante_Htox","Sante_Eau"),'-',3))[[3]],"Indicators"])
      )
      
    })
    output$MultiACVdetail_vitessereseau = renderText({
      if(is.null(input$Input_Analyses_MultiACVdetail_ChoixConfig)) {""} else {
        paste("Estimation de la vitesse du réseau : ",
              vitesse_reseau(REACT_list_config[[input$Input_Analyses_MultiACVdetail_ChoixConfig]]),
              " km/h")
      }
      
    })
    
    MultiACVdetail_Export <- reactiveValues(Export = 0, vitesse = 0)
    output$GRAPH_MultiACVdetail <- renderPlotly({
      
      config <- input$Input_Analyses_MultiACVdetail_ChoixConfig
      
      impacts <- as.character(liste_impacts_ACV[which(liste_impacts_ACV$Indicators %in% input$Input_Analyses_MultiACVdetail_Indicateurs ), "Abrev"])
      impacts_unite <- as.character(liste_impacts_ACV[which(liste_impacts_ACV$Indicators %in% input$Input_Analyses_MultiACVdetail_Indicateurs ), "Unit_D3"])
      choix_vitesse <- input$Input_Analyses_MultiACVdetail_ChoixVitesses
      vitesse <-input$Input_Analyses_MultiACVdetail_vitesse
      details <- input$MultiACVdetail_Phases
      
      if(is.null(config)) {
        fig <- plotly_empty(type = "bar")
        fig <- fig %>% layout(title = "Aucun calcul réalisé")
        fig
      } else {
        Data_Impacts_parc <- REACT_list_Impacts_parc[[config]]
        Data_Config_Utilisateur <- REACT_list_config[[config]]
        
        if(choix_vitesse == "perso") {
          data <- lapply(Data_Impacts_parc,function(i) apply(i[,as.character(vitesse),impacts,],2:3,sum))
          MultiACVdetail_Export$vitesse <- paste0("Vitesse :",as.character(vitesse)," km/h")
        } else if(choix_vitesse == "reseau") {
          usage_reseau <- Data_Config_Utilisateur[["Infra_usage"]]$Utilisation
          vitesses_reseau <- apply(distrib_vitesse,1,function(i) sum(i*usage_reseau))
          data <- lapply(Data_Impacts_parc,function(i) apply(apply(i[,,impacts,],2:4,sum), MARGIN = c(2,3), function(i) sum(i*vitesses_reseau)))
          MultiACVdetail_Export$vitesse <- paste0("Vitesse du réseau :",vitesse_reseau(REACT_list_config[[input$Input_Analyses_MultiACVdetail_ChoixConfig]])," km/h")
        } else {
          data <- lapply(Data_Impacts_parc,function(i) apply(apply(i[,,impacts,],2:4,sum), MARGIN = c(2,3), function(i) sum(i*distrib_vitesse[,choix_vitesse])))
          if(choix_vitesse == "lent") {MultiACVdetail_Export$vitesse <- "Vitesse lente : 32 km/h"}
          if(choix_vitesse == "moyen") {MultiACVdetail_Export$vitesse <- "Vitesse moyenne : 51 km/h"}
          if(choix_vitesse == "rapide") {MultiACVdetail_Export$vitesse <- "Vitesse rapide : 72 km/h"}
          if(choix_vitesse == "très.rapide") {MultiACVdetail_Export$vitesse <- "Vitesse très rapide : 106 km/h"}
        }
        
        data[["Total"]] <- data[["Total"]][,-1]
        data <- data.frame(abind(data[["Total"]],data[["Gaz"]],data[["Carburants"]],data[["Vehicules"]],data[["Infrastructures"]],along = 2))
        data_graph <- data.frame(Impacts = liste_impacts_ACV[as.character(impacts),"Indicators"])
        if("Gaz" %in% details) {data_graph <- cbind(data_graph,data[,5:9])} else {data_graph <- data.frame(data_graph,Gaz = data$Gaz)}
        if("Carburants" %in% details) {data_graph <- cbind(data_graph,data[,10:18])} else {data_graph <- data.frame(data_graph,Carburants = data$Carburant)}
        if("Vehicules" %in% details) {data_graph <- cbind(data_graph,data[,19:23])} else {data_graph <- data.frame(data_graph,Vehicules = data$Vehicule)}
        if("Infra" %in% details) {data_graph <- cbind(data_graph,data[,24:29])} else {data_graph <- data.frame(data_graph,Infrastructures = data$Infrastructures)}
        
        data_graph <- data_graph[,c(1,1+which(colSums(data_graph[,-1])!=0))]
        names(data_graph) <- renames_all(names(data_graph))
        #annotations
        annotations <- list()
        for(i in 1:nrow(data_graph)) {
          annotations[[i]] <- list(x = data_graph$Impacts[i],
                                   y = rep(1.5,nrow(data_graph)),
                                   text = paste(signif(rowSums(data_graph[,-1])[i],3),"<br>",liste_impacts_ACV[match(as.character(data_graph$Impacts[i]),as.character(liste_impacts_ACV$Indicators)),'Units'], sep = " "),
                                   yanchor = 'bottom',
                                   showarrow = FALSE)
          next
        }
        temp_colors <- couleurs$HEX_trait[match(names(data_graph)[-1],couleurs$Nom)]
        MultiACVdetail_Export$Export = data.frame(data_graph)
        data_relatif <- data_graph[,-1]/rowSums(data_graph[,-1])
        #construction du graph 
        fig <- plot_ly(data_graph, type = 'bar',
                       hovertemplate = paste0("<b>%{x}</b>",
                                              "<br>%{fullData.name}",
                                              "<br>%{text} (%{y})<extra></extra>"))
        for(i in 2:ncol(data_graph)) {
          fig <- fig %>% add_trace(x = ~Impacts,y = data_relatif[,i-1], name = names(data_graph)[i],
                                   marker = list(color = temp_colors[i-1]),
                                   text = paste(gsub("e","E",signif(data_graph[,i],3)),impacts_unite))
          next
        }
        fig <- fig %>% layout(
          xaxis = list(title = 'Impacts environnementaux',rangemode = 'tozero',
                       categoryorder = "array",categoryarray = as.character(data_graph$Impacts)),
          barmode = 'stack',
          yaxis = list(tickformat = "%"),
          annotations = annotations,
          legend = list(x = 100, y = 0.5)
        )
        MultiACVdetail_Export$Export <- data.frame(data_graph)
        fig
      }
    })
    
    
    output$download_MultiACVdetail <- downloadHandler(
      filename = "resultats_modemacv.xlsx",
      content = function(file) {write_xlsx(list("Résumé" = data.frame(`Export Modem_ACV` = c("Date","Type d'analyse","Nom du calcul","Vitesse", "Liste des impacts évalués",rep(" ",length(input$Input_Analyses_MultiACVdetail_Indicateurs)-1)),
                                                                      Info = c(as.character(Sys.time()),"Analyse détaillée des contributions",input$Input_Analyses_MultiACVdetail_ChoixConfig,MultiACVdetail_Export$vitesse,
                                                                               paste0(input$Input_Analyses_MultiACVdetail_Indicateurs," (",as.character(liste_impacts_ACV[match(input$Input_Analyses_MultiACVdetail_Indicateurs,as.character(liste_impacts_ACV$Indicators)),"Units"]),")"))),
                                                "Résultats" = MultiACVdetail_Export$Export),path = file)} 
    )
    # # EmissionsDirectes -   ####
    output$EmissionsDirectes_choix_calcul = renderUI({
      pickerInput(
        inputId = "Input_Analyses_EmissionsDirectes_ChoixConfig",
        label = "Sélection du calcul :",
        width = 400,
        choices = REACT_list_calculs$nom,
        options = list(
          style = "btn-danger",
          pickerOptions(noneSelectedText = "Aucun calcul"))
      )

    })
    
    EmissionsDirectes_Vitesse_Export <- reactiveValues(Export = 0)
    
    output$GRAPH_EmissionsDirectes_vitesse <- renderPlotly({

      config <- input$Input_Analyses_EmissionsDirectes_ChoixConfig
      choix_sup <- input$Input_EmissionsDiectes
      
      if(is.null(config)) {
        fig <- plotly_empty(type = "scatter")
        fig <- fig %>% layout(title = "Aucun calcul réalisé")
      } else {
        Data_Emissions <- REACT_list_direct[[config]]
        Data_Parc_Utilisateur <- REACT_list_config[[config]][["Parc_utilisateur"]]
        if(dim(Data_Emissions)[3] == 0) {
          fig <- plotly_empty(type = "scatter")
          fig <- fig %>% layout(title = "OPtion de calcul non-choisie")
        } else {

          data <- array(apply(Data_Emissions,2:3,sum),dim = c(1,dim(Data_Emissions)[-1]),
                        dimnames = list("Moyenne Parc",dimnames(Data_Emissions)[[2]],dimnames(Data_Emissions)[[3]]))
          
          if(length(choix_sup)!=0) {
            if(paste(choix_sup,collapse = " ")=="Moto") {temp_aggreg <- Data_Parc_Utilisateur$Fuel}
            if(paste(choix_sup,collapse = " ")=="Euro") {temp_aggreg <- Data_Parc_Utilisateur$Euro.Standard}
            if(paste(choix_sup,collapse = " ")=="Segment") {temp_aggreg <- Data_Parc_Utilisateur$Segment}
            if(paste(choix_sup,collapse = " ")=="Moto Euro") {temp_aggreg <- paste(Data_Parc_Utilisateur$Fuel,Data_Parc_Utilisateur$Euro.Standard)}
            if(paste(choix_sup,collapse = " ")=="Moto Segment") {temp_aggreg <- paste(Data_Parc_Utilisateur$Fuel,Data_Parc_Utilisateur$Segment)}
            if(paste(choix_sup,collapse = " ")=="Euro Segment") {temp_aggreg <- paste(Data_Parc_Utilisateur$Euro.Standard,Data_Parc_Utilisateur$Segment)}
            if(paste(choix_sup,collapse = " ")=="Moto Euro Segment") {temp_aggreg <- paste(Data_Parc_Utilisateur$Fuel,Data_Parc_Utilisateur$Euro.Standard,Data_Parc_Utilisateur$Segment)}
            
            part_parc <- 1
            
            temp_data <- abind(apply(Data_Emissions,MARGIN = 3, function(x) aggregate(x,list(temp_aggreg),sum)),along=3)
            dimnames(temp_data)[[1]] <- temp_data[,1,1]
            temp_data <- temp_data[apply(temp_data[,-1,],1,function(x) sum(as.numeric(x))) != 0,-1,]
            data <- abind(data,temp_data,along = 1)
            part_parc <- c(part_parc,aggregate(Data_Parc_Utilisateur$Part, list(temp_aggreg), sum)[which(aggregate(Data_Parc_Utilisateur$Part, list(temp_aggreg), sum)[,-1]!=0),-1])

            storage.mode(data) <- "numeric"
            data <- apply(data,MARGIN = 2:3,function(x) x/part_parc)
          }
          dimnames(data)[[1]] <- renames_all(dimnames(data)[[1]])  
          all_buttons <- list()
          unites <- c()
          for(i in 1:dim(data)[3]) {
            
            if(dimnames(data)[[3]][i] %in% c("Diesel","Essence","GPL")) {unites <- c(unites,"litres/100km")}
            else if(dimnames(data)[[3]][i] == "GNV") {unites <- c(unites,"kg/100km")}
            else if(dimnames(data)[[3]][i] == "Electricite") {unites <- c(unites,"kWh/100km")}
            else {unites <- c(unites,"g/km")}
            
            data_list <- list()
            text_list <- list()
            for(j in 1:dim(data)[1]) {
              data_list[[j]] <- data[j,,i]
              text_list[[j]] <- paste(gsub("e","E",signif(data[j,,i],4)),unites[i])
              next
            }
            all_buttons[[i]] <- list(method = "restyle",
                                     args = list(list(y = data_list, 
                                                      text = text_list)),
                                     label = dimnames(data)[[3]][i] )
            next
          }
          
          temp_color <- c("#000000",couleurs$HEX_trait[match(dimnames(data)[[1]][-1],couleurs$Nom)])
          temp_dash <- rep("solid", dim(data)[1])
          if(length(choix_sup) >= 2 & "Segment" %in% choix_sup) {
            dash_pattern <- c("dot","dash","solid","longdashdot")
            names(dash_pattern) <- c("Mini","Petit","Medium","Large")
            if(length(choix_sup) == 3){
              
              temp_color[-1] <- couleurs$HEX_trait[match(paste(sapply(strsplit(dimnames(data)[[1]][-1],split = "/"),"[",1),sapply(strsplit(dimnames(data)[[1]][-1],split = "/"),"[",2),sep="/"),couleurs$Nom)]
              temp_dash[-1] <- dash_pattern[sapply(strsplit(dimnames(data)[[1]][-1],split = "/"),"[",3)]
            } else {
              
              temp_color[-1] <- couleurs$HEX_trait[match(sapply(strsplit(dimnames(data)[[1]][-1],split = "/"),"[",1),couleurs$Nom)]
              temp_dash[-1] <- dash_pattern[sapply(strsplit(dimnames(data)[[1]][-1],split = "/"),"[",2)]
            }
            
          }
          
          
          fig <- plot_ly(type = 'scatter', mode = 'lines',showlegend = T,
                         x = 5:130, y = data[1,,1],name = dimnames(data)[[1]][1],
                         line = list(color = temp_color[1],dash = temp_dash[1]), text = paste(gsub("e","E",signif(data[1,,1],4)),unites[1]),
                         hovertemplate = paste0("<b>%{fullData.name}</b>",
                                                "<br><i>Vit : %{x} km/h</i>",
                                                "<br>%{text}<extra></extra>"))
          
          if(dim(data)[1]>1) {
            for(i in 2:dim(data)[1]) {
              fig <- fig %>% add_trace(x = 5:130, y = data[i,,1], 
                                       name = dimnames(data)[[1]][i],
                                       line = list(color = temp_color[i],dash = temp_dash[i]), text = paste(gsub("e","E",signif(data[i,,1],4)),unites[1]),
                                       hovertemplate = paste0("<b>%{fullData.name}</b>",
                                                              "<br><i>Vit : %{x} km/h</i>",
                                                              "<br>%{text}<extra></extra>"))
              next
            }
          }
          fig <- fig %>% layout(
            title = 'Impacts en fonction de la vitesse',
            plot_bgcolor = "#d9d9d9",
            margin = list(t = 65),
            xaxis = list(title = 'vitesses (km/h)',rangemode = 'tozero',gridcolor = "#b4b4b4", dtick = 10),
            yaxis = list(gridcolor = "b4b4b4",exponentformat = "E",minexponent = 3,rangemode = 'tozero')
            ,
            updatemenus = list(list(buttons=all_buttons))

          )
        }


      }
      
      temp_Export <- list()
      
      temp_Export[[1]] <- data.frame(`Export Modem_ACV` = c("Date","Type d'analyse","Nom du calcul","Unité"),
                                                                            Info = c(as.character(Sys.time()),"Émissions et consommations directes",input$Input_Analyses_EmissionsDirectes_ChoixConfig,
                                                                                     "g/km (polluants) ; litres/100km ; kg/100km (GNV) ; kWh/100km"))
      temp_Export <- c(temp_Export,lapply(seq(dim(data)[3]), function(x) data.frame(Vitesse = 5:130,t(data[ , , x]))))
      names(temp_Export) <- c("Résumé",dimnames(data)[[3]])
      EmissionsDirectes_Vitesse_Export$Export <- temp_Export
      
      fig

    })
    output$EmissionsDirectes_vitessereseau = renderText({
      if(is.null(input$Input_Analyses_EmissionsDirectes_ChoixConfig)) {""} else {
        paste("Estimation de la vitesse du réseau : ",
              vitesse_reseau(REACT_list_config[[input$Input_Analyses_EmissionsDirectes_ChoixConfig]]),
              " km/h")
      }

    })
    output$EmissionsDirectes_vitessereseau2 = renderText({
      if(is.null(input$Input_Analyses_EmissionsDirectes_ChoixConfig)) {""} else {
        paste("Estimation de la vitesse du réseau : ",
              vitesse_reseau(REACT_list_config[[input$Input_Analyses_EmissionsDirectes_ChoixConfig]]),
              " km/h")
      }

    })

    output$DT_EmissionsDirectes_gaz <- renderDT({
      config <- input$Input_Analyses_EmissionsDirectes_ChoixConfig
      choix_vitesse <- input$Input_Analyses_EmissionsDirectes_ChoixVitesses
      vitesse <- input$Input_Analyses_EmissionsDirectes_vitesse
      if(is.null(config)) {
        temp_table <- matrix(c("Emission de gaz (g/km)",""),nrow = 1, ncol = 2)
        datatable(temp_table,
                  class = "hover cell-border compact",
                  caption = "Émissions directes de gaz :",
                  colnames = c("","Aucun calcul réalisé"),
                  selection = "none",
                  options = list(dom = 't',ordering=F,
                                 pageLength = 30),
                  rownames = F
        )
      } else {
        Data_Emissions <- REACT_list_direct[[config]]
        if(!any(c("CO2","NOx","CO","PM10","PM2.5","NMVOC") %in% as.character(dimnames(Data_Emissions)[[3]]))) {
          temp_table <- matrix(c("Emission de gaz (g/km)",""),nrow = 1, ncol = 2)
          datatable(temp_table,
                    class = "hover cell-border compact",
                    caption = "Émissions directes de gaz :",
                    colnames = c("","Aucunes émissions de gaz calculées"),
                    selection = "none",
                    options = list(dom = 't',ordering=F,
                                   pageLength = 30),
                    rownames = F
          )
        } else {
          Data_Emissions <- Data_Emissions %>% multi_select(c("CO2","NOx","CO","PM10","PM2.5","NMVOC"),'+',3)
          if(choix_vitesse == "perso") {
            data <- apply(Data_Emissions[,as.character(vitesse),],2,sum)
          } else if(choix_vitesse == "reseau") {
            usage_reseau <- REACT_list_config[[config]][["Infra_usage"]]$Utilisation
            vitesses_reseau <- apply(distrib_vitesse,1,function(i) sum(i*usage_reseau))
            data <- apply(apply(Data_Emissions,c(1,3),function(x) sum(x*vitesses_reseau)),2,sum)
          } else {
            data <- apply(apply(Data_Emissions,c(1,3),function(x) sum(x*distrib_vitesse[,choix_vitesse])),2,sum)
          }
          temp_table <- matrix(c("Emission de gaz (g/km)",signif(data,3)),nrow = 1, ncol = length(data)+1)
          datatable(temp_table,
                    class = "hover cell-border compact",
                    caption = "Émissions directes de gaz :",
                    colnames = c("",names(data)),
                    selection = "none",
                    options = list(dom = 't',ordering=F,
                                   pageLength = 30),
                    rownames = F
          )

        }

      }

    })

    output$DT_EmissionsDirectes_carburants <- renderDT({
      config <- input$Input_Analyses_EmissionsDirectes_ChoixConfig
      choix_vitesse <- input$Input_Analyses_EmissionsDirectes_ChoixVitesses
      vitesse <- input$Input_Analyses_EmissionsDirectes_vitesse
      if(is.null(config)) {
        temp_table <- matrix(c("Consommation de carburants par 100km",""),nrow = 1, ncol = 2)
        datatable(temp_table,
                  class = "hover cell-border compact",
                  caption = "Consommation de carburants :",
                  colnames = c("","Aucun calcul réalisé"),
                  selection = "none",
                  options = list(dom = 't',ordering=F,
                                 pageLength = 30),
                  rownames = F
        )
      } else {
        Data_Conso <- REACT_list_direct[[config]]
        if(!any(c("Diesel","Essence","GPL","GNV","Electricite") %in% dimnames(Data_Conso)[[3]])) {
          temp_table <- matrix(c("Consommation de carburants par 100km",""),nrow = 1, ncol = 2)
          datatable(temp_table,
                    class = "hover cell-border compact",
                    caption = "Consommation de carburants :",
                    selection = "none",
                    colnames = c("","Aucunes consommations calculées"),
                    options = list(dom = 't',ordering=F,
                                   pageLength = 30),
                    rownames = F
          )
        } else {
          Data_Conso <- Data_Conso %>% multi_select(c("Diesel","Essence","GPL","GNV","Electricite"),'+',3)
          if(choix_vitesse == "perso") {
            data <- apply(Data_Conso[,as.character(vitesse),],2,sum)
          } else if(choix_vitesse == "reseau") {
            usage_reseau <- REACT_list_config[[config]][["Infra_usage"]]$Utilisation
            vitesses_reseau <- apply(distrib_vitesse,1,function(i) sum(i*usage_reseau))
            data <- apply(apply(Data_Conso,c(1,3),function(x) sum(x*vitesses_reseau)),2,sum)
          } else {
            data <- apply(apply(Data_Conso,c(1,3),function(x) sum(x*distrib_vitesse[,choix_vitesse])),2,sum)
          }
          temp_table <- matrix(c("Consommation de carburants par 100km",paste(signif(data,3),c("litres", "litres", "litres","kg","kWh"), sep =" ")),nrow = 1, ncol = length(data)+1)
          datatable(temp_table,
                    class = "hover cell-border compact",
                    caption = "Consommation de carburants :",
                    selection = "none",
                    colnames = c("",names(data)),
                    options = list(dom = 't',ordering=F,
                                   pageLength = 30),
                    rownames = F
          )

        }

      }





    })
    output$DT_EmissionsDirectes_veh <- renderDT({

      config <- input$Input_Analyses_EmissionsDirectes_ChoixConfig
      Gaz_Carb <- input$Input_Analyses_EmissionsDirectes_gazcarb
      choix_vitesse <- input$Input_Analyses_EmissionsDirectes_ChoixVitesses2
      vitesse <- input$Input_Analyses_EmissionsDirectes_vitesse2
      type_veh <- input$Input_Analyses_EmissionsDirectes_MotoEuroSegment
      if(is.null(config)) {
        temp_table <- matrix("",nrow = 1, ncol = 1)
        DT <- datatable(temp_table,
                  class = "hover cell-border compact",
                  caption = "Consommation de carburants :",
                  colnames = c("Aucun calcul réalisé"),
                  selection = "none",
                  options = list(dom = 't',ordering=F,
                                 pageLength = 30),
                  rownames = F
        )
      } else {
        Data_Parc_Utilisateur <- REACT_list_config[[config]][["Parc_utilisateur"]]
        Data_EmisConso <- REACT_list_direct[[config]]
        if(Gaz_Carb == "Gaz") {
          if(!any(c("CO2","NOx","CO","PM10","PM2.5","NMVOC") %in% dimnames(Data_EmisConso)[[3]])) {
            temp_table <- matrix("",nrow = 1, ncol = 1)
            DT <- datatable(temp_table,
                      class = "hover cell-border compact",
                      caption = "Émissions directes de gaz :",
                      selection = "none",
                      colnames = c("Aucunes émissions de gaz calculées"),
                      options = list(dom = 't',ordering=F,
                                     pageLength = 30),
                      rownames = F
            )
          } else {
            Data_Gaz <- Data_EmisConso %>% multi_select(c("CO2","NOx","CO","PM10","PM2.5","NMVOC"),'+',3)
            if(choix_vitesse == "perso") {
              data <- Data_Gaz[,as.character(vitesse),]
            } else if(choix_vitesse == "reseau") {
              usage_reseau <- REACT_list_config[[config]][["Infra_usage"]]$Utilisation
              vitesses_reseau <- apply(distrib_vitesse,1,function(i) sum(i*usage_reseau))
              data <- apply(Data_Gaz,c(1,3),function(x) sum(x*vitesses_reseau))
            } else {
              data <- apply(Data_Gaz,c(1,3),function(x) sum(x*distrib_vitesse[,choix_vitesse]))
            }

            list_aggreg <- list()
            if("Moto" %in% type_veh) {
              list_aggreg <- c(list_aggreg,list(Data_Parc_Utilisateur$Fuel))
            }
            if("Euro" %in% type_veh) {
              list_aggreg <- c(list_aggreg,list(Data_Parc_Utilisateur$Euro.Standard))
            }
            if("Segment" %in% type_veh) {
              list_aggreg <- c(list_aggreg,list(Data_Parc_Utilisateur$Segment))
            }
            data <- data.frame(Aggreg_veh(data,list_aggreg, sum))
            part_parc <- aggregate(Data_Parc_Utilisateur$Part, list_aggreg, sum)[which(aggregate(Data_Parc_Utilisateur$Part, list_aggreg, sum)[,length(list_aggreg)+1] != 0),length(list_aggreg)+1]
            data <- data/part_parc
            temp_table <- cbind(rownames(data),signif(data,4))
            DT <- datatable(temp_table,
                      class = "hover cell-border compact",
                      selection = "none",
                      caption = "Émissions directes de gaz :",
                      colnames = c("Emission de gaz (g/km)",colnames(data)),
                      options = list(dom = 't',ordering=F,
                                     pageLength = 30),
                      rownames = F
            )
          }
        }
        if(Gaz_Carb == "Carburant") {
          if(!any(c("Diesel","Essence","GPL","GNV","Electricite") %in% dimnames(Data_EmisConso)[[3]])) {
            temp_table <- matrix("",nrow = 1, ncol = 1)
            DT <- datatable(temp_table,
                      class = "hover cell-border compact",
                      selection = "none",
                      caption = "Consommation de carburants :",
                      colnames = c("Aucunes consommations calculées"),
                      options = list(dom = 't',ordering=F,
                                     pageLength = 30),
                      rownames = F
            )
          } else {
            Data_Conso <- Data_EmisConso %>% multi_select(c("Diesel","Essence","GPL","GNV","Electricite"),'+',3)
            if(choix_vitesse == "perso") {
              data <- Data_Conso[,as.character(vitesse),]
            } else if(choix_vitesse == "reseau") {
              usage_reseau <- REACT_list_config[[config]][["Infra_usage"]]$Utilisation
              vitesses_reseau <- apply(distrib_vitesse,1,function(i) sum(i*usage_reseau))
              data <- apply(Data_Conso,c(1,3),function(x) sum(x*vitesses_reseau))
            } else {
              data <- apply(Data_Conso,c(1,3),function(x) sum(x*distrib_vitesse[,choix_vitesse]))
            }

            list_aggreg <- list()
            if("Moto" %in% type_veh) {
              list_aggreg <- c(list_aggreg,list(Data_Parc_Utilisateur$Fuel))
            }
            if("Euro" %in% type_veh) {
              list_aggreg <- c(list_aggreg,list(Data_Parc_Utilisateur$Euro.Standard))
            }
            if("Segment" %in% type_veh) {
              list_aggreg <- c(list_aggreg,list(Data_Parc_Utilisateur$Segment))
            }
            data <- data.frame(Aggreg_veh(data,list_aggreg, sum))
            part_parc <- aggregate(Data_Parc_Utilisateur$Part, list_aggreg, sum)[which(aggregate(Data_Parc_Utilisateur$Part, list_aggreg, sum)[,length(list_aggreg)+1] != 0),length(list_aggreg)+1]
            data <- data/part_parc

            temp_table <- matrix(paste(unlist(signif(data,4)),rep(c("litres", "litres", "litres","kg","kWh"),each = nrow(data)), sep =" "),nrow = nrow(data), ncol = ncol(data))
            temp_table <- cbind(rownames(data),temp_table)
            DT <- datatable(temp_table,
                      class = "hover cell-border compact",
                      caption = "Consommation de carburants :",
                      selection = "none",
                      colnames = c("Consommation de carburants par 100km","Diesel","Essence","GPL","GNV","Electricité"),
                      options = list(dom = 't',ordering=F,
                                     pageLength = 30),
                      rownames = F
            )

          }
        }
      }
      DT

    })

    output$download_EmissionsDirectes_Vitesse <- downloadHandler(
      filename = "resultats_modemacv.xlsx",
      content = function(file) {openxlsx::write.xlsx(EmissionsDirectes_Vitesse_Export$Export,file = file)} 
    )

}) # fin du server
