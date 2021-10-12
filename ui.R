library(shinydashboard)
library(shinycssloaders)
library(shinyWidgets)
library(shinydashboardPlus)
library(shiny)
source("libraries.R")
source("functions_Config.R")
# source("functions_analyses.R")
source("Chargement_data.R")

# Définition de l'UI : User Interface ####
ui <- 
  navbarPage(title = tagList(div(tags$img(src="logo_modemACV.png", width=350), style="padding-top:15px;margin-top:-40px;margin-left:0px;"),
                             tags$link(rel = "stylesheet", type = "text/css", href = "custom_styles.css")),
             windowTitle ="ModEm - ACV",
             id="tabactive",
             position = "fixed-top",
             theme = "www/custom_styles.css",
             
             # --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
             # --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
             # --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
             
             tabPanel("Configurateur", # TabPanel "Configurateur" ####
                      style = "padding-top: 75px;",
                      icon = icon("sliders-h"),
                      # dashboardPagePlus(sidebar_fullCollapse = T,
                      dashboardPage(
                                        #sidebar_fullCollapse = T,
                                        #sidebar_background = "light",
                                        skin = "red",
                                        # dashboardHeaderPlus(title = "ModEM-ACV", 
                                        dashboardHeader(title = "ModEM-ACV",
                                                            disable = T,
                                                            # enable_rightsidebar = TRUE,
                                                            controlbarIcon = shiny::icon("gears")
                                                            # rightSidebarIcon = "gears"
                                                            ),
                                        
                                        dashboardSidebar(
                                          sidebarMenu(
                                            style = "position: fixed;",
                                            menuItem("Composition du parc", tabName = "parc", selected = TRUE, icon = icon("car")),
                                            menuItem("Paramètres carburants", tabName = "carburants", icon = icon("gas-pump")),
                                            menuItem("Poids et équipements", tabName = "poidsequipement", icon = icon("weight")),
                                            menuItem("Usage des infrastructures", tabName = "infra", icon = icon("road")),
                                            menuItem("Conditions météorologiques", tabName = "meteo", icon = icon("sun")),
                                            menuItem("Exporter ma configuration", tabName = "export", icon = icon("file-export")
                                                     # ,
                                                     # downloadButton("download_Config", "Télécharger ma configuration", class = "butt1")
                                                     ),
                                            menuItem("Charger ma configuration", tabName = "import", icon = icon("file-upload"))
                                                     # ,
                                                     # downloadButton("upload_Configuration", "Importer mon propre parc", class = "butt1"))
                                          )),
                                        
                                        dashboardBody(

                                          tabItems(
                                            
                                            # --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
                                            
                                            tabItem(tabName = "parc", ## _Config -  Composition du parc automobile ----
                                                    
                                                    fluidRow( # fluidRow du tabItem Parc
                                                      column(width = 12,
                                                             
                                                             h3("Composition du parc"),
                                                             
                                                             fluidRow(
                                                               column(width = 3,
                                                                      radioGroupButtons(
                                                               inputId =  "Input_Configurateur_defautimportperso",
                                                               label = "",
                                                               choices = c("Utiliser un parc existant" = "defaut",
                                                                           "Personnaliser un parc" = "perso",
                                                                           "Importer un parc" = "import"),
                                                               individual = TRUE,
                                                               checkIcon = list(
                                                                 yes = tags$i(class = "fa fa-circle", 
                                                                              style = "color: steelblue"),
                                                                 no = tags$i(class = "fa fa-circle-o", 
                                                                             style = "color: steelblue"))
                                                             )
                                                               )
                                                             ,

                                                             conditionalPanel(
                                                               condition = "input.Input_Configurateur_defautimportperso == 'defaut'",
                                                               
                                                               column(width = 4,
                                                                      
                                                               selectInput(
                                                                 inputId =  "Input_Configurateur_defaut_choixParc",
                                                                 label = "",
                                                                 choices = c(
                                                                   "France 2020" = "FR2020",
                                                                   "Ile de France 2010" = "EGT2010",
                                                                   "Aire urbaine de Lyon 2015" = "Lyon2015")
                                                               )
                                                             )
                                                             )
                                                             
                                                             ,
                                                             
                                                             conditionalPanel(
                                                               condition = "input.Input_Configurateur_defautimportperso == 'perso'",
                                                               
                                                               column(width = 4,
                                                                      br(),br(),
                                                                      selectInput(
                                                                        inputId =  "Input_Configurateur_perso_choixParc",
                                                                        label = "",
                                                                        choices = c(
                                                                          "France 2020" = "FR2020",
                                                                          "Ile de France 2010" = "EGT2010",
                                                                          "Aire urbaine de Lyon 2015" = "Lyon2015")
                                                                      )
                                                               )
                                                             )
                                                             
                                                               )
                                                             
                                                             ,br(),
                                                             
                                                             conditionalPanel( # conditional Panel import
                                                               condition = "input.Input_Configurateur_defautimportperso == 'import'",
                                                               
                                                               
                                                               fluidRow(
                                                                 column(width = 5,
                                                                        fileInput('upload_Parc', 'Choisir un fichier Excel',
                                                                         accept = c(".xlsx",".xls"))),
                                                                 column(width = 5,downloadButton("download_structure_parc", "Télécharger la structure excel", class = "butt2"))
                                                               )
                                                               
                                                               # fileInput("fileParc", "Télécharger un parc en .csv")
                                                               ,
                                                               
                                                               fluidRow(
                                                                 column(width = 4, offset = 1,
                                                                        
                                                               actionBttn(
                                                                 "button_utiliser_Config_Parc",
                                                                 "Utiliser le parc téléchargé",
                                                                 icon = NULL,
                                                                 style = "fill",
                                                                 color = "success",
                                                                 size = "md",
                                                                 block = FALSE,
                                                                 no_outline = TRUE
                                                               )
                                                               ),
                                                               column(width = 4,
                                                               actionBttn(
                                                                 "button_NEPASutiliser_Config_Parc",
                                                                 "Revenir au parc par défaut",
                                                                 icon = NULL,
                                                                 style = "fill",
                                                                 color = "danger",
                                                                 size = "md",
                                                                 block = FALSE,
                                                                 no_outline = TRUE
                                                               )
                                                             )
                                                             
                                                               )
                                                             ,br(),br()
                                                             
                                                             ) # fin du conditional Panel import
                                                             
                                                             ,
                                                       
                                                             
                                                             
                                                            conditionalPanel( # ___Personnalisation du parc - ConditionalPanel perso ----
                                                                               condition = "input.Input_Configurateur_defautimportperso == 'perso'",
                                                                               
                                                                               # boxPlus( # début de la boxPlus Personnalisation du Parc
                                                                              box( # début de la boxPlus Personnalisation du Parc
                                                                                 title = "Personnalisation du parc", 
                                                                                 width = NULL,
                                                                                 closable = FALSE,
                                                                                 collapsible = TRUE,
                                                                                 collapsed = FALSE,
                                                                                 enable_label = TRUE,
                                                                                 solidHeader = FALSE, 
                                                                                 
                                                                                 # Personnalisation de la Motorisation --
                                                                                 
                                                                                 fluidRow(
                                                                                   column(width = 5,
                                                                                          prettyRadioButtons(
                                                                                            inputId =  "Input_Configurateur_perso_Motorisation",
                                                                                            label = "Personnalisation de la motorisation",
                                                                                            choices = c("Taux des carburants, global" = "global",
                                                                                                        "Taux des carburants, par norme Euro" = "parnorme",
                                                                                                        "Taux des carburants, par segment de puissance" = "parsegment"),
                                                                                            width = NULL
                                                                                          ))
                                                                                   ,
                                                                                   column(width = 5,
                                                                                          br(),
                                                                                          span(textOutput("Modifs_Config_Motorisation"), style="color:red"),
                                                                                          actionBttn(
                                                                                            "button_reset_Config_Motorisation",
                                                                                            "Revenir aux taux de motorisation par défaut",
                                                                                            icon = NULL,
                                                                                            style = "minimal",
                                                                                            color = "success",
                                                                                            size = "sm",
                                                                                            block = FALSE,
                                                                                            no_outline = TRUE
                                                                                          ))
                                                                                 )
                                                                                 
                                                                                 ,
                                                                                 conditionalPanel( # conditionalPanel perso Motorisation - global
                                                                                   condition = "input.Input_Configurateur_perso_Motorisation == 'global'",
                                                                                   
                                                                                   DTOutput("DT_Config_Motorisation_global", width = "300px"),
                                                                                   span(textOutput("VerifSomme_Config_Motorisation_global"), style="color:red")
                                                                                   
                                                                                 ) # fin du conditionalPanel perso Motorisation - global
                                                                                 ,
                                                                                 conditionalPanel( # conditionalPanel perso Motorisation - parnorme
                                                                                   condition = "input.Input_Configurateur_perso_Motorisation == 'parnorme'",
                                                                                   
                                                                                   DTOutput("DT_Config_Motorisation_parnorme")
                                                                                   
                                                                                 ) # fin du conditionalPanel perso Motorisation - parnorme
                                                                                 ,
                                                                                 conditionalPanel( # conditionalPanel perso Motorisation - parsegment
                                                                                   condition = "input.Input_Configurateur_perso_Motorisation == 'parsegment'",
                                                                                   
                                                                                   DTOutput("DT_Config_Motorisation_parsegment")
                                                                                   
                                                                                 ) # fin du conditionalPanel perso Motorisation - parsegment
                                                                                 
                                                                                 ,br(),br(),
                                                                                 
                                                                                 # Personnalisation des normes Euro --
                                                                                 
                                                                                 fluidRow(
                                                                                   column(width = 5,
                                                                                          prettyRadioButtons(
                                                                                            inputId =  "Input_Configurateur_perso_Euro",
                                                                                            label = "Personnalisation des normes Euro",
                                                                                            choices = c("Taux de normes Euro, global" = "global",
                                                                                                        "Taux de normes Euro, par motorisation" = "parmotorisation",
                                                                                                        "Taux de normes Euro, par segment de puissance" = "parsegment"),
                                                                                            width = NULL
                                                                                          ))
                                                                                   ,
                                                                                   column(width = 5,
                                                                                          br(),
                                                                                          span(textOutput("Modifs_Config_Euro"), style="color:red"),
                                                                                          actionBttn(
                                                                                            "button_reset_Config_Euro",
                                                                                            "Revenir aux taux Euro par défaut",
                                                                                            icon = NULL,
                                                                                            style = "minimal",
                                                                                            color = "success",
                                                                                            size = "sm",
                                                                                            block = FALSE,
                                                                                            no_outline = TRUE
                                                                                          ))
                                                                                 )
                                                                                 
                                                                                 ,
                                                                                 conditionalPanel( # conditionalPanel perso Euro - global
                                                                                   condition = "input.Input_Configurateur_perso_Euro == 'global'",
                                                                                   
                                                                                   DTOutput("DT_Config_Euro_global", width = "300px"),
                                                                                   span(textOutput("VerifSomme_Config_Euro_global"), style="color:red")
                                                                                   
                                                                                 ) # fin du conditionalPanel perso Euro - global
                                                                                 ,
                                                                                 conditionalPanel( # conditionalPanel perso Euro - parmotorisation
                                                                                   condition = "input.Input_Configurateur_perso_Euro == 'parmotorisation'",
                                                                                   
                                                                                   DTOutput("DT_Config_Euro_parmotorisation"),
                                                                                   span(textOutput("VerifSomme_Config_Euro_parmotorisation"), style="color:red")
                                                                                   
                                                                                 ) # fin du conditionalPanel perso Euro - parmotorisation
                                                                                 ,
                                                                                 conditionalPanel( # conditionalPanel perso Euro - parsegment
                                                                                   condition = "input.Input_Configurateur_perso_Euro == 'parsegment'",
                                                                                   
                                                                                   DTOutput("DT_Config_Euro_parsegment"),
                                                                                   span(textOutput("VerifSomme_Config_Euro_parsegment"), style="color:red")
                                                                                   
                                                                                 ) # fin du conditionalPanel perso Euro - parsegment
                                                                                 
                                                                                 
                                                                                 ,br(),br(),
                                                                                 
                                                                                 # Personnalisation des segments de puissance --
                                                                                 
                                                                                 fluidRow(
                                                                                   column(width = 5,
                                                                                          prettyRadioButtons(
                                                                                            inputId =  "Input_Configurateur_perso_Segment",
                                                                                            label = "Personnalisation des segments de puissance",
                                                                                            choices = c("Taux de segment de puissance, global" = "global",
                                                                                                        "Taux de segment de puissance, par motorisation" = "parmotorisation",
                                                                                                        "Taux de segment de puissance, par norme Euro" = "parnorme"),
                                                                                            width = NULL
                                                                                          ))
                                                                                   ,
                                                                                   column(width = 5,
                                                                                          br(),
                                                                                          span(textOutput("Modifs_Config_Segment"), style="color:red"),
                                                                                          actionBttn(
                                                                                            "button_reset_Config_Segment",
                                                                                            "Revenir aux taux de segment par défaut",
                                                                                            icon = NULL,
                                                                                            style = "minimal",
                                                                                            color = "success",
                                                                                            size = "sm",
                                                                                            block = FALSE,
                                                                                            no_outline = TRUE
                                                                                          ))
                                                                                 )
                                                                                 
                                                                                 ,
                                                                                 conditionalPanel( # conditionalPanel perso Segment - global
                                                                                   condition = "input.Input_Configurateur_perso_Segment == 'global'",
                                                                                   
                                                                                   DTOutput("DT_Config_Segment_global", width = "300px"),
                                                                                   span(textOutput("VerifSomme_Config_Segment_global"), style="color:red")
                                                                                   
                                                                                 ) # fin du conditionalPanel perso Segment - global
                                                                                 ,
                                                                                 conditionalPanel( # conditionalPanel perso Segment - parmotorisation
                                                                                   condition = "input.Input_Configurateur_perso_Segment == 'parmotorisation'",
                                                                                   
                                                                                   DTOutput("DT_Config_Segment_parmotorisation"),
                                                                                   span(textOutput("VerifSomme_Config_Segment_parmotorisation"), style="color:red")
                                                                                   
                                                                                 ) # fin du conditionalPanel perso Segment - parmotorisation
                                                                                 ,
                                                                                 conditionalPanel( # conditionalPanel perso Segment - parnorme
                                                                                   condition = "input.Input_Configurateur_perso_Segment == 'parnorme'",
                                                                                   
                                                                                   DTOutput("DT_Config_Segment_parnorme"),
                                                                                   span(textOutput("VerifSomme_Config_Segment_parnorme"), style="color:red")
                                                                                   
                                                                                 ) # fin du conditionalPanel perso Segment - parnorme
                                                                                 
                                                                                 ,br(),br(),
                                                                                 
                                                                                 h4("Validation des modifications"),
                                                                                 fluidRow( # fluidRow validation des modifications
                                                                                   column(width = 3,
                                                                                          span(textOutput("Validation_Config_Motorisation"), style="color:red"),
                                                                                          span(textOutput("Validation_Config_Euro"), style="color:red"),
                                                                                          span(textOutput("Validation_Config_Segment"), style="color:red")
                                                                                   ),
                                                                                   
                                                                                   column(width = 4,
                                                                                          br(),
                                                                                          actionBttn(
                                                                                            "button_maj_Config",
                                                                                            "Valider les modifications du parc",
                                                                                            icon = NULL,
                                                                                            style = "fill",
                                                                                            color = "success",
                                                                                            size = "md",
                                                                                            block = FALSE,
                                                                                            no_outline = TRUE
                                                                                          )
                                                                                   ),
                                                                                   column(width = 4,
                                                                                          br(),
                                                                                          actionBttn(
                                                                                            "button_reset_Config",
                                                                                            "Annuler les modifications du parc",
                                                                                            icon = NULL,
                                                                                            style = "fill",
                                                                                            color = "danger",
                                                                                            size = "md",
                                                                                            block = FALSE,
                                                                                            no_outline = TRUE
                                                                                          )
                                                                                   )
                                                                                 ) # fin fluidRow validation des modifications
                                                                                 
                                                                                 
                                                                               ) # fin de boxPlus Personnalisation du Parc
                                                                               
                                                             ),
                                                             # fin du ConditionalPanel Personnalisation du parc
                                                             
                                                             
                                                            # ___Synthèse Composition du parc ----
                                                             conditionalPanel( # conditionalPanel Synthese
                                                               condition = "input.Input_Configurateur_defautimportperso == 'defaut' | 
                                                               input.Input_Configurateur_defautimportperso == 'perso' | 
                                                               input.Input_Configurateur_defautimportperso == 'import'",
                                                               
                                                               # boxPlus( # début de la boxPlus Synthese Composition du Parc
                                                               box( # début de la boxPlus Synthese Composition du Parc
                                                                 title = "Synthèse", 
                                                                 width = NULL,
                                                                 closable = FALSE,
                                                                 collapsible = TRUE,
                                                                 collapsed = FALSE,
                                                                 enable_label = TRUE,
                                                                 solidHeader = FALSE, 
                                                                 
                                                                 span(textOutput("TITRE_SYNTHESE"),style="color:green;font-size:1.8em"),
                                                                 br(),br(),

                                                               fluidRow(
                                                                 column(width = 4,
                                                                        withSpinner(plotlyOutput("GRAPH_Synthese_Motorisation_ly"))
                                                                 )
                                                                 ,
                                                                 column(width = 4,align="center",
                                                                        withSpinner(plotlyOutput("GRAPH_Synthese_Age_ly")),br(),
                                                                        span(textOutput("Visu_REACT_AgeMoyen_Parc_utilisateur"), style="color:black")
                                                                 )
                                                                 ,
                                                                 column(width = 4,align="center",
                                                                        withSpinner(plotlyOutput("GRAPH_Synthese_Puissance_ly")),
                                                                        span(textOutput("Visu_REACT_PuissanceMoyenne_Parc_utilisateur"), style="color:black")
                                                                 )
                                                               )
                                                               ,
                                                               br(),
                                                               h3("Explorer le parc"),
                                                               
                                                               fluidRow( # fluidRow Synthese 1
                                                                 
                                                                 column(width = 3,
                                                               pickerInput(
                                                                 inputId = "Input_Config_Synthese_Abs",
                                                                 label = "",
                                                                 choices = c("Motorisation" = "Fuel",
                                                                             "Normes Euro" = "Euro.Standard", 
                                                                             "Segments de puissance" = "Segment")
                                                               )
                                                               ),
                                                               column(width = 1,
                                                               br(),h5("en fonction")
                                                               ),
                                                               
                                                               conditionalPanel(  # conditionalPanel Fuel
                                                                 condition = "input.Input_Config_Synthese_Abs == 'Fuel'",
                                                               
                                                               column(width = 3,
                                                               pickerInput(
                                                                 inputId = "Input_Config_Synthese_Fill_Fuel",
                                                                 label = "",
                                                                 choices = c("des normes Euro" = "Euro.Standard", 
                                                                             "des segments de puissance" = "Segment")
                                                               )
                                                               )
                                                               ) # fin conditionalPanel Fuel
                                                               ,
                                                               
                                                               conditionalPanel(  # conditionalPanel Euro.Standard
                                                                 condition = "input.Input_Config_Synthese_Abs == 'Euro.Standard'",
                                                                 
                                                                 column(width = 3,
                                                                        pickerInput(
                                                                          inputId = "Input_Config_Synthese_Fill_Euro",
                                                                          label = "",
                                                                          choices = c("de la motorisation" = "Fuel",
                                                                                      "des segments de puissance" = "Segment")
                                                                        )
                                                                 )
                                                               ) # fin conditionalPanel Euro.Standard
                                                               ,
                                                               
                                                               conditionalPanel(  # conditionalPanel Segment
                                                                 condition = "input.Input_Config_Synthese_Abs == 'Segment'",
                                                                 
                                                                 column(width = 3,
                                                                        pickerInput(
                                                                          inputId = "Input_Config_Synthese_Fill_Segment",
                                                                          label = "",
                                                                          choices = c("de la motorisation" = "Fuel",
                                                                                      "des normes Euro" = "Euro.Standard")
                                                                        )
                                                                 )
                                                               ) # fin conditionalPanel Segment

                                                               ), # fin fluidRow Synthese 1
                                                               
                                                               br(),
                                                               
                                                               fluidRow(
                                                                 withSpinner(plotlyOutput("GRAPH_Synthese_DeuxVariables"))
                                                               ),
                                                               
                                                               
                                                               conditionalPanel(  # conditionalPanel perso - Tléchargement du parc
                                                                 condition = "input.Input_Configurateur_defautimportperso == 'perso'",
                                                               
                                                               fluidRow(
                                                                 column(width = 10, offset = 0.1,
                                                                        h4("Export du parc personnalisé")
                                                                        ,
                                                                        downloadButton("download_Parc", "Télécharger mon parc (.xlsx)")
                                                                 )
                                                               )
                                                               
                                                               ) # fin du conditionalPanel perso - Téléchargement du parc 
                                                               
                                                               
                                                               ) # fin du boxPlus Composition du Parc
                                                             ) # fin du conditionalPanel Synthese
                                                             
                                                             
                                                             
                                                              
                                                             
                                                      )) # fin du column et du fluidRow du tabItem 'parc'
                                                    
                                            ) # fin du tabItem 'parc'
                                            
                                            ,
                                            
                                            # --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
                                            
                                            tabItem(tabName = "carburants", ## _Config -  Carburants ----
                                                    
                                                    fluidRow( # fluidRow du tabItem 'Carburants'
                                                      column(width = 12, offset = 0,
                                                             
                                                             h3("Carburants"),
                                                             
                                                             # boxPlus( # début de la boxPlus Synthese Carburants
                                                             box( # début de la boxPlus Synthese Carburants
                                                               title = "Synthèse", 
                                                               width = NULL,
                                                               closable = FALSE,
                                                               collapsible = TRUE,
                                                               collapsed = FALSE,
                                                               enable_label = TRUE,
                                                               solidHeader = FALSE, 
                                                               
                                                               fluidRow(
                                                                 column(width = 4,
                                                                        plotlyOutput("GRAPH_Synthese_MixElect_tout_ly")
                                                                 )
                                                                 ,
                                                                 column(width = 3,
                                                                        plotlyOutput("GRAPH_Synthese_MixElect_renew")
                                                                 )
                                                                 ,
                                                                 column(width = 5,
                                                                        plotlyOutput("GRAPH_Synthese_Carburants")
                                                                 )
                                                               ), 
                                                               fluidRow(
                                                                 column(width = 7,align="center",
                                                                 span(textOutput("Visu_PerteMoyenne"), style="color:black")
                                                               )
                                                               )  
                                                               
                                                             ), # fin boxPlus Synthese Carburants
                                                             

                                                               # boxPlus( # début de la boxPlus BioFuel
                                                             box( # début de la boxPlus BioFuel
                                                                 title = "Incorporation d'agrocarburants", 
                                                                 width = NULL,
                                                                 closable = FALSE,
                                                                 collapsible = TRUE,
                                                                 collapsed = TRUE,
                                                                 enable_label = TRUE,
                                                                 solidHeader = FALSE, 
                                                                 
                                                                 
                                                                 fluidRow(
                                                                   column(width = 7,
                                                                   DTOutput("DT_Carburants", width = "500px"),
                                                                   br(),
                                                                   span(textOutput("Verif_Somme_Carburants"), style="color:red")
                                                                   )
                                                                   ,
                                                                 column(width = 4, offset = 1,
                                                                        br(),br(),br(),br(),
                                                                        span(textOutput("Visu_SurconsoDiesel"), style="color:black"),
                                                                        span(textOutput("Visu_SurconsoEssence"), style="color:black"),
                                                                        span(textOutput("Visu_TeneurOxygene"), style="color:black"),
                                                                        span(textOutput("Visu_EthanolTotal"), style="color:black")
                                                                 )
                                                               )
                                                                 
                                                                 ,
                                                                 
                                                                 br(),
                                                                 h4("Validation des modifications"),
                                                                 fluidRow(
                                                                   column(width = 4,
                                                                          actionBttn(
                                                                            "button_maj_Config_Carburants",
                                                                            "Valider les modifications",
                                                                            icon = NULL,
                                                                            style = "fill",
                                                                            color = "success",
                                                                            size = "md",
                                                                            block = FALSE,
                                                                            no_outline = TRUE
                                                                          )
                                                                   ),
                                                                   column(width = 4,
                                                                          actionBttn(
                                                                            "button_reset_Config_Carburants",
                                                                            "Annuler les modifications",
                                                                            icon = NULL,
                                                                            style = "fill",
                                                                            color = "danger",
                                                                            size = "md",
                                                                            block = FALSE,
                                                                            no_outline = TRUE
                                                                          )
                                                                   )
                                                                 )

                                                               ) # fin boxPlus BioFuel
                                                                 ,
                                                                 
                                                             # boxPlus( # début de la boxPlus MixElec et PerteEnergie
                                                             box( # début de la boxPlus MixElec et PerteEnergie
                                                               title = "Productions électrique", 
                                                               width = NULL,
                                                               closable = FALSE,
                                                               collapsible = TRUE,
                                                               collapsed = TRUE,
                                                               enable_label = TRUE,
                                                               solidHeader = FALSE, 
                                                                   
                                                                   DTOutput("DT_MixElect", width = "350px"),
                                                                   br(),
                                                                   span(textOutput("Verif_Somme_MixElect"), style="color:red"),
                                                                   br(),
                                                                   DTOutput("DT_PerteEnergie", width = "350px")
                                                               
                                                               ,
                                                               
                                                               br(),
                                                               h4("Validation des modifications"),
                                                               fluidRow(
                                                                 column(width = 4,
                                                                        actionBttn(
                                                                          "button_maj_Config_Elect",
                                                                          "Valider les modifications",
                                                                          icon = NULL,
                                                                          style = "fill",
                                                                          color = "success",
                                                                          size = "md",
                                                                          block = FALSE,
                                                                          no_outline = TRUE
                                                                        )
                                                                 ),
                                                                 column(width = 4,
                                                                        actionBttn(
                                                                          "button_reset_Config_Elect",
                                                                          "Annuler les modifications",
                                                                          icon = NULL,
                                                                          style = "fill",
                                                                          color = "danger",
                                                                          size = "md",
                                                                          block = FALSE,
                                                                          no_outline = TRUE
                                                                        )
                                                                 )
                                                               )
                                                                   
                                                             ) # fin boxPlus MixElec et PerteEnergie

                                                                 ,
                                                             
                                                             # boxPlus( # début de la boxPlus ConsoHybride
                                                             box( # début de la boxPlus ConsoHybride
                                                               title = "Consommation des véhicules électriques", 
                                                               width = NULL,
                                                               closable = FALSE,
                                                               collapsible = TRUE,
                                                               collapsed = TRUE,
                                                               enable_label = TRUE,
                                                               solidHeader = FALSE, 
                                                               
                                                               DTOutput("DT_ConsoHybride", width = "800px")
                                                               
                                                               ,
                                                               
                                                               br(),
                                                               h4("Validation des modifications"),
                                                               fluidRow(
                                                                 column(width = 4,
                                                                        actionBttn(
                                                                          "button_maj_Config_ConsoHybride",
                                                                          "Valider les modifications",
                                                                          icon = NULL,
                                                                          style = "fill",
                                                                          color = "success",
                                                                          size = "md",
                                                                          block = FALSE,
                                                                          no_outline = TRUE
                                                                        )
                                                                 ),
                                                                 column(width = 4,
                                                                        actionBttn(
                                                                          "button_reset_Config_ConsoHybride",
                                                                          "Annuler les modifications",
                                                                          icon = NULL,
                                                                          style = "fill",
                                                                          color = "danger",
                                                                          size = "md",
                                                                          block = FALSE,
                                                                          no_outline = TRUE
                                                                        )
                                                                 )
                                                               )

                                                             ) # fin boxPlus ConsoHybride
                                                             
                                                      )) # fin du column et du fluidRow du tabItem 'Carburants'
                                                    
                                            ) # fin du tabItem 'Carburants'
                                            
                                            ,

                                            # --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
                                            
                                            tabItem(tabName = "poidsequipement", ## _Config - PoidsEquipement ----
                                                    
                                                    fluidRow( # fluidRow du tabItem 'PoidsEquipement'
                                                      column(width = 12, offset = 0,
                                                             
                                                             h3("Poids et équipements des véhicules"),

                                                             # boxPlus( # début de la boxPlus Masse
                                                             box( # début de la boxPlus Masse
                                                               title = "Masses des composants",
                                                               width = NULL,
                                                               closable = FALSE,
                                                               collapsible = TRUE,
                                                               collapsed = TRUE,
                                                               enable_label = TRUE,
                                                               solidHeader = FALSE,
                                                               
                                                               
                                                               
                                                               radioGroupButtons(
                                                                 inputId = "Input_Config_Masse_Fuel",
                                                                 label = "Motorisation :", 
                                                                 choices = c("Essence","Diesel","Electrique","Hybride","Gaz Naturel","GPL"),
                                                                 selected = "Essence",
                                                                 checkIcon = list(
                                                                   yes = icon("ok",lib = "glyphicon"))
                                                               ),
                                                               tags$script(paste0("$(\"input:radio[name='Input_Config_Masse_Fuel'][value='Essence']\").parent().css('background-color','", couleurs$HEX_aire[match("Essence",couleurs$Nom)],"');")),
                                                               tags$script(paste0("$(\"input:radio[name='Input_Config_Masse_Fuel'][value='Diesel']\").parent().css('background-color', '", couleurs$HEX_aire[match("Diesel",couleurs$Nom)],"');")),
                                                               tags$script(paste0("$(\"input:radio[name='Input_Config_Masse_Fuel'][value='Electrique']\").parent().css('background-color', '", couleurs$HEX_aire[match("Electrique",couleurs$Nom)],"');")),
                                                               tags$script(paste0("$(\"input:radio[name='Input_Config_Masse_Fuel'][value='Hybride']\").parent().css('background-color', '", couleurs$HEX_aire[match("Hybride",couleurs$Nom)],"');")),
                                                               tags$script(paste0("$(\"input:radio[name='Input_Config_Masse_Fuel'][value='Gaz Naturel']\").parent().css('background-color', '", couleurs$HEX_aire[match("Gaz Naturel",couleurs$Nom)],"');")),
                                                               tags$script(paste0("$(\"input:radio[name='Input_Config_Masse_Fuel'][value='GPL']\").parent().css('background-color', '", couleurs$HEX_aire[match("GPL",couleurs$Nom)],"');")),
                                                               
                                                               
                                                               
                                                               
                                                               conditionalPanel(
                                                                 condition = "input.Input_Config_Masse_Fuel == 'Essence'",
                                                               DTOutput("DT_MasseEssence")
                                                               )
                                                               ,
                                                               conditionalPanel(
                                                                 condition = "input.Input_Config_Masse_Fuel == 'Diesel'",
                                                                 DTOutput("DT_MasseDiesel")
                                                               )
                                                               ,
                                                               conditionalPanel(
                                                                 condition = "input.Input_Config_Masse_Fuel == 'Electrique'",
                                                                 DTOutput("DT_MasseElectrique")
                                                               )
                                                               ,
                                                               conditionalPanel(
                                                                 condition = "input.Input_Config_Masse_Fuel == 'Hybride'",
                                                                 DTOutput("DT_MasseHybride")
                                                               )
                                                               ,
                                                               conditionalPanel(
                                                                 condition = "input.Input_Config_Masse_Fuel == 'Gaz Naturel'",
                                                                 DTOutput("DT_MasseGazNaturel")
                                                               )
                                                               ,
                                                               conditionalPanel(
                                                                 condition = "input.Input_Config_Masse_Fuel == 'GPL'",
                                                                 DTOutput("DT_MasseGPL")
                                                               )
                                                               ,
                                                               
                                                               br(),
                                                               h4("Validation des modifications"),
                                                               fluidRow(
                                                                 column(width = 4,
                                                                        actionBttn(
                                                                          "button_maj_Config_Masse",
                                                                          "Valider les modifications",
                                                                          icon = NULL,
                                                                          style = "fill",
                                                                          color = "success",
                                                                          size = "md",
                                                                          block = FALSE,
                                                                          no_outline = TRUE
                                                                        )
                                                                 ),
                                                                 column(width = 4,
                                                                        actionBttn(
                                                                          "button_reset_Config_Masse",
                                                                          "Annuler les modifications",
                                                                          icon = NULL,
                                                                          style = "fill",
                                                                          color = "danger",
                                                                          size = "md",
                                                                          block = FALSE,
                                                                          no_outline = TRUE
                                                                        )
                                                                 )
                                                               )

                                                             ) # fin boxPlus Masse
                                                             
                                                             ,
                                                             
                                                             # boxPlus( # début de la boxPlus Durée de vie
                                                             box( # début de la boxPlus Durée de vie
                                                               title = "Durée de vie des véhicules",
                                                               width = NULL,
                                                               closable = FALSE,
                                                               collapsible = TRUE,
                                                               collapsed = TRUE,
                                                               enable_label = TRUE,
                                                               solidHeader = FALSE,
                                                               
                                                               DTOutput("DT_DureeVieVehicule", width = "400px"),
                                                               br(),
                                                               DTOutput("DT_DureeVieBatterie", width = "400px")
                                                               
                                                               ,
                                                               
                                                               br(),
                                                               h4("Validation des modifications"),
                                                               fluidRow(
                                                                 column(width = 4,
                                                                        actionBttn(
                                                                          "button_maj_Config_DureeVie",
                                                                          "Valider les modifications",
                                                                          icon = NULL,
                                                                          style = "fill",
                                                                          color = "success",
                                                                          size = "md",
                                                                          block = FALSE,
                                                                          no_outline = TRUE
                                                                        )
                                                                 ),
                                                                 column(width = 4,
                                                                        actionBttn(
                                                                          "button_reset_Config_DureeVie",
                                                                          "Annuler les modifications",
                                                                          icon = NULL,
                                                                          style = "fill",
                                                                          color = "danger",
                                                                          size = "md",
                                                                          block = FALSE,
                                                                          no_outline = TRUE
                                                                        )
                                                                 )
                                                               )
                                                               
                                                             ) # fin boxPlus Durée de vie
                                                            
                                                             ,
                                                             
                                                             # boxPlus( # début de la boxPlus Climatisation
                                                             box( # début de la boxPlus Climatisation
                                                               title = "Climatisation",
                                                               width = NULL,
                                                               closable = FALSE,
                                                               collapsible = TRUE,
                                                               collapsed = TRUE,
                                                               enable_label = TRUE,
                                                               solidHeader = FALSE,
                                                               
                                                               DTOutput("DT_AC", width = "650px")
                                                               ,
                                                               
                                                               br(),
                                                               h4("Validation des modifications"),
                                                               fluidRow(
                                                                 column(width = 4,
                                                                        actionBttn(
                                                                          "button_maj_Config_AC",
                                                                          "Valider les modifications",
                                                                          icon = NULL,
                                                                          style = "fill",
                                                                          color = "success",
                                                                          size = "md",
                                                                          block = FALSE,
                                                                          no_outline = TRUE
                                                                        )
                                                                 ),
                                                                 column(width = 4,
                                                                        actionBttn(
                                                                          "button_reset_Config_AC",
                                                                          "Annuler les modifications",
                                                                          icon = NULL,
                                                                          style = "fill",
                                                                          color = "danger",
                                                                          size = "md",
                                                                          block = FALSE,
                                                                          no_outline = TRUE
                                                                        )
                                                                 )
                                                               )

                                                             ) # fin boxPlus Climatisation
                                                             
                                                             
                                                      )) # fin du column et du fluidRow du tabItem "PoidsEquipement"
                                                    
                                            ) # fin du tabItem "PoidsEquipement"
                                            
                                            ,
                                            
                                            # --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
                                            
                                            tabItem(tabName = "infra", ## _Config - Infrastructures ----

                                                    fluidRow( # fluidRow du tabItem 'Infra'
                                                      column(width = 12, offset = 0,

                                                             h3("Usage des infrastructures"),


                                                             fluidRow(
                                                              column(width = 4,
                                                                     br(),
                                                                     DTOutput("DT_InfraUtilisation"),
                                                                     br(),
                                                                     span(textOutput("Verif_Somme_InfraUtilisation"), style="color:red")
                                                              )
                                                              ,
                                                              column(width = 8,
                                                                     plotlyOutput("GRAPH_Synthese_InfraUtilisation_ly")
                                                              )
                                                             )

                                                             ,br(),

                                                             # boxPlus( # début de la boxPlus
                                                             box( # début de la boxPlus
                                                               title = "Caractéristiques des types de route",
                                                               width = NULL,
                                                               closable = FALSE,
                                                               collapsible = TRUE,
                                                               collapsed = TRUE,
                                                               enable_label = TRUE,
                                                               solidHeader = FALSE,

                                                               DTOutput("DT_InfraCaract")
                                                               
                                                               ,
                                                               
                                                               br(),
                                                               h4("Validation des modifications"),
                                                               fluidRow(
                                                                 column(width = 4,
                                                                        actionBttn(
                                                                          "button_maj_Config_InfraCaract",
                                                                          "Valider les modifications",
                                                                          icon = NULL,
                                                                          style = "fill",
                                                                          color = "success",
                                                                          size = "md",
                                                                          block = FALSE,
                                                                          no_outline = TRUE
                                                                        )
                                                                 ),
                                                                 column(width = 4,
                                                                        actionBttn(
                                                                          "button_reset_Config_InfraCaract",
                                                                          "Annuler les modifications",
                                                                          icon = NULL,
                                                                          style = "fill",
                                                                          color = "danger",
                                                                          size = "md",
                                                                          block = FALSE,
                                                                          no_outline = TRUE
                                                                        )
                                                                 )
                                                               )
                                                               
                                                             ) # fin boxPlus

                                                      )) # fin du column et du fluidRow du tabItem 'Infra'

                                            ) # fin du tabItem "infra"

                                            ,

                                            # --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
                                            
                                            tabItem(tabName = "meteo", ## _Config - Météo ----

                                                    fluidRow(
                                                      column(width = 12, offset = 0,

                                                             h3("Conditions météo"),


                                                             fluidRow(
                                                               column(width = 10,
                                                                      plotlyOutput("GRAPH_Synthese_Temp_ly")
                                                               )
                                                               ,
                                                               column(width = 2,
                                                                      DTOutput("DT_Mobilite"),
                                                                      br(),
                                                                      span(textOutput("Visu_MoteurFroid"), style="color:black")
                                                               )
                                                             )

                                                             ,br(),

                                                             # boxPlus( # début de la boxPlus
                                                             box( # début de la boxPlus
                                                               title = "Températures et humidités relatives",
                                                               width = NULL,
                                                               closable = FALSE,
                                                               collapsible = TRUE,
                                                               collapsed = TRUE,
                                                               enable_label = TRUE,
                                                               solidHeader = FALSE,

                                                               DTOutput("DT_Temp", width = "500px")
                                                               ,
                                                               
                                                               br(),
                                                               h4("Validation des modifications"),
                                                                                  fluidRow(
                                                                                    column(width = 4,
                                                                                           actionBttn(
                                                                                             "button_maj_Config_Temp",
                                                                                             "Valider les modifications",
                                                                                             icon = NULL,
                                                                                             style = "fill",
                                                                                             color = "success",
                                                                                             size = "md",
                                                                                             block = FALSE,
                                                                                             no_outline = TRUE
                                                                                           )
                                                                                    ),
                                                                                    column(width = 4,
                                                                                           actionBttn(
                                                                                             "button_reset_Config_Temp",
                                                                                             "Annuler les modifications",
                                                                                             icon = NULL,
                                                                                             style = "fill",
                                                                                             color = "danger",
                                                                                             size = "md",
                                                                                             block = FALSE,
                                                                                             no_outline = TRUE
                                                                                           )
                                                                                    )
                                                                                  )
                                                               
                                                               
                                                             ) # fin boxPlus

                                                      )) # fin du column et du fluidRow

                                            ) # fin du tabItem "météo"

                                            ,
                                            
                                            # --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---

                                            tabItem(tabName = "export", ## _Config - Export ----
                                                    
                                                    fluidRow(
                                                      column(width = 12, offset = 0,
                                                             
                                                             h3("Sauvegarder la configuration :"), 
                                                             
                                                             # boxPlus( 
                                                             box( 
                                                               title = "Exporter ma configuration", 
                                                               width = NULL,
                                                               closable = FALSE,
                                                               collapsible = TRUE,
                                                               collapsed = FALSE,
                                                               enable_label = TRUE,
                                                               solidHeader = FALSE,
                                                             # span(h3("Exporter ma configuration"), style="color:green"),
                                                              downloadButton("download_Config", "Télécharger ma configuration", class = "butt1")),
                                                             br(),br(),
                                                             # boxPlus( 
                                                             box(
                                                               title = "Visualisation de la configuration exportée", 
                                                               width = NULL,
                                                               closable = FALSE,
                                                               collapsible = TRUE,
                                                               collapsed = TRUE,
                                                               enable_label = TRUE,
                                                               solidHeader = FALSE,
                                                             
                                                               h4("Composition du parc"),
                                                               DTOutput("VISU_TEST_Parc"),
                                                               DTOutput("VISU_ParcUtilisateur"), 
                                                               span(textOutput("Verif_NouveauParc_utilisateur"), style="color:red"),
                                                               br(),br(),
                                                               h4("Climatisation"),
                                                               DTOutput("VISU_ACUsage"),
                                                               br(),br(),
                                                               h4("Incorporation d'agrocarburants"),
                                                               DTOutput("VISU_BioFuel"),
                                                               br(),br(),
                                                               h4("Production électrique"),
                                                               DTOutput("VISU_Mix_elec"),
                                                               br(),br(),
                                                               h4("Usage des infrastructures"),
                                                               DTOutput("VISU_Infra_usage"),
                                                               br(),br(),
                                                               h4("Conditions météorologiques"),
                                                               DTOutput("VISU_Temp")
                                                             )
                                                             
                                                             
                                                      )) # fin du column et du fluidRow
                                                    
                                            ) # fin du tabItem "export"
                                            
                                            , 
                                            
                                            # --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
                                            
                                            tabItem(tabName = "import", ## _Config - Import ----
                                                    
                                                    fluidRow(
                                                      column(width = 12, offset = 0,
                                            
                                                             h3("Importer une sauvegarde :"),
                                                                              
                                            fileInput('upload_Config', 'Choisir un fichier Excel',
                                                      accept = c(".xlsx",".xls"))
                                            ,
                                            
                                            fluidRow(
                                              column(width = 4, offset = 0,
                                                     
                                                     radioGroupButtons(
                                                       inputId =  "Input_Configurateur_UploadConfig",
                                                       label = "Utilisation ou non de la configuration importée",
                                                       choices = c("Ne pas utiliser la configuration importée" = "ConfigDefaut",
                                                                   "Utiliser la configuration importée" = "ConfigUpload"),
                                                       individual = TRUE,
                                                       checkIcon = list(
                                                         yes = tags$i(class = "fa fa-circle", 
                                                                      style = "color: steelblue"),
                                                         no = tags$i(class = "fa fa-circle-o", 
                                                                     style = "color: steelblue"))
                                                     )
                                              )
                                              
                                            )
                                            
                                            ,br(),br(),
                                            
                                            conditionalPanel( # conditionalPanel
                                              condition = "input.Input_Configurateur_UploadConfig == 'ConfigUpload'",
                                              
                                            h4("Visulation de la configuration importée --> sera utilisée pour le calcul"),
                                            br(),
                                            h4("Composition du parc"),
                                            DTOutput("VISU_ParcUtilisateur_upload"),
                                            br(),br(),
                                            h4("Climatisation"),
                                            DTOutput("VISU_ACUsage_upload"),
                                            br(),br(),
                                            h4("Incorporation d'agrocarburants"),
                                            DTOutput("VISU_BioFuel_upload"),
                                            br(),br(),
                                            h4("Production électrique"),
                                            DTOutput("VISU_Mix_elec_upload"),
                                            br(),br(),
                                            h4("Usage des infrastructures"),
                                            DTOutput("VISU_Infra_usage_upload"),
                                            br(),br(),
                                            h4("Conditions météorologiques"),
                                            DTOutput("VISU_Temp_upload")
                                            
                                            ) # fin du conditionalPanel
                                            
                                                      )) # fin du column et du fluidRow
                                            ) # fin du tabItem "import" 
                                            
                                          ) # fin du tabItems
                                        ) # fin du dashboardBody
                      ) # fin du dashboardPagePlus
             ) # fin du TabPanel "Configurateur" --- --- --- --- --- 
             
             ,
             
             # --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
             # --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
             # --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
             
             tabPanel("Lancer un calcul", # TabPanel "Calcul" ####
                      style = "padding-top: 90px;",
                      icon = icon("calculator"),
                      
                      fluidRow(
                        column(width = 5, offset=1,
                               h3("Lancement du calcul"))),
                      br(),
                      
                      fluidRow( # fluidRow du tabPanel "Calcul"
                        column(width = 6, offset = 1,
                               
                               pickerInput(
                                 inputId = "Input_LancementCalcul_Indicateurs",
                                 label = "Sélection des indicateurs :",
                                 width = 450,
                                 choices = list(
                                   `Changement climatique` = c("Changement climatique"),
                                   `Atteinte à la santé` = c("Formation de particules fines",
                                                             "Formation d'oxydants photochimiques",
                                                             "Appauvrissement de la couche d'ozone",
                                                             "Toxicité humaine",
                                                             "Radiations ionisantes"),
                                   `Déterioration des écosystèmes` = c("Ecotoxicité",
                                                                       "Eutrophisation",
                                                                       "Acidification"),
                                   Ressources = c("Ressources minérales",
                                                  "Occupation du sol",
                                                  "Raréfaction en eau"),
                                   Déchets = c("Déchets",
                                               "Déchets radioactifs"),
                                   `Energies primaires` = c("Energies primaires fossiles (hors nucléaire)",
                                                            "Energie primaire nucléaire",
                                                            "Energies primaires renouvelables"),
                                   `Dommages pour la santé humaine et la biodiversité` = c("Dommages pour la santé humaine et la biodiversité")),
                                 
                                 selected = c("Changement climatique","Formation de particules fines","Formation d'oxydants photochimiques","Appauvrissement de la couche d'ozone",
                                              "Formation d'oxydants photochimiques","Toxicité humaine","Radiations ionisantes",
                                              "Ecotoxicité","Eutrophisation","Acidification",
                                              "Ressources minérales","Occupation du sol","Raréfaction en eau",
                                              "Déchets","Déchets radioactifs",
                                              "Energies primaires fossiles (hors nucléaire)",
                                              "Energie primaire nucléaire","Energies primaires renouvelables",
                                              "Dommages pour la santé humaine et la biodiversité"),
                                 
                                 options = list(
                                   `actions-box` = TRUE,
                                   `deselect-all-text` = "Désélectionner tous les indicateurs",
                                   `select-all-text` = "Sélectionner tous les indicateurs",
                                   `none-selected-text` = "Aucun indicateurs sélectionnés"),
                                 multiple = TRUE
                               )
                               ,
                               DTOutput("DT_IndicateursSelectionnes")
                        )
                        ,
                        column(width = 4,
                               
                               pickerInput(
                                 inputId = "Input_LancementCalcul_GazCarburants",
                                 label = "Analyser les émissions directes et consommations :",
                                 width = 350,
                                 choices = list(
                                   Gaz = liste_gaz,
                                   Carburants = c("carburants")),
                                 selected = c(liste_gaz,"carburants"),
                                 options = list(
                                   `actions-box` = TRUE,
                                   `deselect-all-text` = "Tout desélectionner",
                                   `select-all-text` = "Tout sélectionner",
                                   `none-selected-text` = "Aucun indicateurs sélectionnés"),
                                 multiple = TRUE
                               )
                               ,
                               br(),br(),
                               h3("ACV détaillée"),
                               checkboxInput(
                                 inputId = "Input_detail_ACV", label = "Détails supplémentaires pour l'ACV",
                                 value = TRUE),
                               br(),
                               textInput("nom_calcul", label = h3("Nom du calcul"), value = "Entrer le nom..."),
                               
                               
                               
                               actionBttn(
                                 "button_lancementCalcul",
                                 "Calcul des impacts environnementaux",
                                 icon = NULL,
                                 style = "material-flat",
                                 color = "success",
                                 size = "lg",
                                 block = FALSE,
                                 no_outline = TRUE
                               ),
                               br(),
                               
                               DTOutput("DT_liste_calculs"),
                               br(),
                               h3("Supprimer un calcul"),
                               fluidRow(column(6,textInput("calcul_suppr", label = NULL, value = "Entrer un nom ...")),
                                        column(3,actionBttn("button_supprimer_calcul",
                                                            "Supprimer ce calcul",
                                                            icon = NULL,
                                                            style = "material-flat",
                                                            color = "danger",
                                                            size = "sm",
                                                            block = FALSE,
                                                            no_outline = TRUE))),
                               br(),br(),
                               br(),br(),br(),br(),br(),br(),br()
                        )
                        
                      ) # fin du fluidRow du tabPanel "Calcul"
                      
                      , br()
                      
             ) # fin du tabPanel "Lancer un calcul" 

             ,
             
             # --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
             # --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
             # --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
             
             tabPanel("Analyses", # TabPanel "Analyses" ####
                      style = "padding-top: 75px;",
                      icon = icon("chart-bar"),
                      # dashboardPagePlus(sidebar_fullCollapse = T,
                      dashboardPage(
                                        # sidebar_fullCollapse = T,
                                        skin = "red",
                                        # dashboardHeaderPlus(title = "ModEM-ACV", 
                                        dashboardHeader(title = "ModEM-ACV", 
                                                        disable = T,
                                                            # enable_rightsidebar = TRUE,
                                                        controlbarIcon = shiny::icon("gears")
                                                            # rightSidebarIcon = "gears"
                                                        ),
                                        
                                        dashboardSidebar(                      
                                          sidebarMenu(
                                            style = "position: fixed;",
                                            menuItem("Synthèse", tabName = "synthese", selected = TRUE, icon = icon("chart-pie")),
                                            menuItem("Monocritère", icon = icon("chart-area"), startExpanded = TRUE,
                                                     menuSubItem("En fonction de la vitesse", tabName = "MonoVitesse"),
                                                     menuSubItem("Comparaison de véhicules", tabName = "MonoCompaVehicules"),
                                                     menuSubItem("Comparer différents parcs", tabName = "MonoCompaParcs"),
                                                     menuSubItem("Flux d'énergies", tabName = "MonoFluxEnergie"),
                                                     menuSubItem("Analyse ACV détaillée", tabName = "MonoACVdetail"),
                                                     menuSubItem(text = div("Dommages sur",tags$br(),"l'environnement",
                                                                            style = "display: inline-block; vertical-align: middle;"), tabName = "MonoDommage")
                                            ),
                                            menuItem("Multicritère", icon = icon("chart-bar"), startExpanded = TRUE,
                                                     menuSubItem("Contributions", tabName = "MultiContributions"),
                                                     menuSubItem(text = div("Comparaison de types",tags$br(),"de véhicules",
                                                                            style = "display: inline-block; vertical-align: middle;"), tabName = "MultiCompaVehicules"),
                                                     menuSubItem(text = div("Comparer différentes",tags$br(),"configurations",
                                                                            style = "display: inline-block; vertical-align: middle;"), tabName = "MultiCompaConfigs"),
                                                     
                                                     menuSubItem("Analyse ACV détaillée", tabName = "MultiACVdetail")
                                            ),
                                            menuItem(text = div("Emissions directes et",tags$br(),"consommation",
                                                                style = "display: inline-block; vertical-align: middle;"), tabName = "EmissionsDirectes", icon = icon("chart-line"))
                                            
                                          )),
                                        dashboardBody(
                                          
                                          tabItems(
                                            tabItem(tabName = "synthese", ## Onglet Synthèse ####
                                                    
                                                    h2("Synthèse du calcul"),
                                                    
                                                    uiOutput("Synthese_choix_calcul"),
                                                    
                                                    fluidRow(
                                                      column(width = 6, radioGroupButtons(inputId = "Input_Analyses_Synthese_ChoixVitesses",
                                                                                          label = "Vitesses de circulation",
                                                                                          choices = c("Réseau" = "reseau","Lente" = "lent", "Moyenne" = "moyen",
                                                                                                      "Rapide" = "rapide", "Très rapide" = "très.rapide",
                                                                                                      "Personnalisée" = "perso"),
                                                                                          selected = "moyen",
                                                                                          status = "info"
                                                      ),
                                                      conditionalPanel(
                                                        condition = "input.Input_Analyses_Synthese_ChoixVitesses == 'reseau'",
                                                        span(textOutput("Synthese_vitessereseau"),style="font-size:1.4em")
                                                      ),
                                                      conditionalPanel(
                                                        condition = "input.Input_Analyses_Synthese_ChoixVitesses == 'lent'",
                                                        h4("vitesse moyenne : 32 km/h")
                                                      ),
                                                      conditionalPanel(
                                                        condition = "input.Input_Analyses_Synthese_ChoixVitesses == 'moyen'",
                                                        h4("vitesse moyenne : 51 km/h")
                                                      ),
                                                      conditionalPanel(
                                                        condition = "input.Input_Analyses_Synthese_ChoixVitesses == 'rapide'",
                                                        h4("vitesse moyenne : 72 km/h")
                                                      ),
                                                      conditionalPanel(
                                                        condition = "input.Input_Analyses_Synthese_ChoixVitesses == 'très.rapide'",
                                                        h4("vitesse moyenne : 106 km/h")
                                                      ),
                                                      conditionalPanel(
                                                        condition = "input.Input_Analyses_Synthese_ChoixVitesses == 'perso'",
                                                        numericInput("Input_Analyses_synthese_vitesse", label =h4("vitesse en km/h"), value = 50, min = 5, max = 130)
                                                      )
                                                      ),
                                                      
                                                      column(width = 4,
                                                             pickerInput(
                                                               inputId = "Input_Analyses_Synthese_decomposition",
                                                               label = "Décomposition des  résultats:",
                                                               selected = "ACV",
                                                               choices = c("Phases d'ACV" = "ACV",
                                                                           "Motorisation" = "Moto",
                                                                           "Normes Euro" = "Euro",
                                                                           "Segments de puissance" = "Segment"),
                                                               options = list(
                                                                 style = "btn-success")
                                                             )
                                                      )
                                                      
                                                    ),
                                                    
                                                    conditionalPanel(
                                                      condition = "Input_Analyses_Synthese_ChoixVitesses == 'perso'",
                                                      numericInput("Input_Analyses_synthese_vitesse", label = h4("vitesse en km/h"), value = 50, min = 5, max = 130)
                                                    ),
                                                    fluidRow(
                                                      column(width = 6,
                                                             withSpinner(DTOutput("Synthese_Tableau")),
                                                             downloadButton("download_Synthese", "Télécharger les données", class = "butt1")),
                                                      column(width = 6,
                                                           withSpinner(plotlyOutput("GRAPH_Synthese",height = "600px")))
                                                    )
                                                    
                                            ) # fin du tabItem 'Synthese'
                                            ,
                                            
                                            tabItem(tabName = "MonoVitesse", ## Onglet MonoVitesse ####
                                                    
                                                    h2("Evolution avec la vitesse de circulation"),
                                                    
                                                    uiOutput("MonoVitesse_choix_calcul"),
                                                    
                                                    
                                                    fluidRow(
                                                      
                                                      column(width = 5,
                                                             uiOutput("MonoVitesse_Indicateurs")
                                                      ),
                                                      
                                                      column(width = 3,
                                                             pickerInput(
                                                               inputId = "Input_Analyses_MonoVitesse_phaseACV_TRUE",
                                                               label = "Décomposition cycles ACV :",
                                                               choices = c("Oui" = TRUE,
                                                                           "Non" = FALSE),
                                                               options = list(
                                                                 style = "btn-success")
                                                             )
                                                      ),
                                                      conditionalPanel(
                                                        condition = "input.Input_Analyses_MonoVitesse_phaseACV_TRUE == 'FALSE'",
                                                        column(width = 4,
                                                               pickerInput(
                                                                 inputId = "Input_Analyses_MonoVitesse_MotoEuroSegment",
                                                                 label = "Choix :",
                                                                 choices = c("Motorisation" = "Moto",
                                                                             "Normes Euro" = "Euro",
                                                                             "Segments de puissance" = "Segment"),
                                                                 multiple = TRUE,
                                                                 options = list(
                                                                   style = "btn-success"),
                                                                 selected = "Moto"
                                                               )
                                                        )
                                                      )
                                                      
                                                    )
                                                    ,
                                                    withSpinner(plotlyOutput("GRAPH_MonoVitesse",height = "600px")),
                                                    downloadButton("download_MonoVitesse", "Télécharger les données", class = "butt1")
                                                    
                                            ) # fin du tabItem 'MonoVitesse'
                                            ,
                                            
                                            tabItem(tabName = "MonoCompaVehicules", ## Onglet MonoCompaVehicules ####
                                                    
                                                    h2("Comparaison des véhicules du parc"),
                                                    
                                                    uiOutput("MonoCompaVehicules_choix_calcul"),
                                                    
                                                    # boxPlus( # début boxPlus MonoCompaVehicules / Vitesse 
                                                    box( # début boxPlus MonoCompaVehicules / Vitesse 
                                                      title = "Evolution avec la vitesse de circulation", 
                                                      width = NULL,
                                                      closable = FALSE,
                                                      collapsible = TRUE,
                                                      collapsed = FALSE,
                                                      enable_label = TRUE,
                                                      solidHeader = FALSE, 
                                                      
                                                      uiOutput("MonoCompaVehicules_Vitesse_Indicateurs"),
                                                      
                                                      pickerInput(
                                                        inputId = "Input_Analyses_MonoCompaVehicules_Vitesse_MotoEuroSegment",
                                                        label = "Type de véhicules :",
                                                        width = 400,
                                                        choices = c("Motorisation" = "Moto",
                                                                    "Normes Euro" = "Euro",
                                                                    "Segments de puissance" = "Segment"),
                                                        selected = c("Moto"),
                                                        multiple = TRUE,
                                                        options = list(
                                                          style = "btn-success")
                                                      )
                                                      
                                                      ,withSpinner(plotlyOutput("GRAPH_MonoCompaVehicules_Vitesse",height = "600px")),
                                                      
                                                      downloadButton("download_MonoCompaVehicules_Vitesse", "Télécharger les données", class = "butt1")
                                                      
                                                      
                                                    ) # fin boxPlus MonoCompaVehicules / Vitesse 
                                                    ,
                                                    
                                                    # boxPlus( # début boxPlus MonoCompaVehicules / phases ACV 
                                                    box( # début boxPlus MonoCompaVehicules / phases ACV 
                                                      title = "Contribution des phases ACV", 
                                                      width = NULL,
                                                      closable = FALSE,
                                                      collapsible = TRUE,
                                                      collapsed = FALSE,
                                                      enable_label = TRUE,
                                                      solidHeader = FALSE, 
                                                      
                                                      uiOutput("MonoCompaVehicules_PhasesACV_Indicateurs")
                                                      
                                                      ,
                                                      
                                                      fluidRow(
                                                        column(width = 5,
                                                               pickerInput(
                                                                 inputId = "Input_Analyses_MonoCompaVehicules_PhasesACV_MotoEuroSegment",
                                                                 label = "Type de véhicules :",
                                                                 width = 400,
                                                                 choices = c("Motorisation" = "Moto",
                                                                             "Normes Euro" = "Euro",
                                                                             "Segments de puissance" = "Segment"),
                                                                 selected = c("Moto"),
                                                                 multiple = TRUE,
                                                                 options = list(
                                                                   style = "btn-success")
                                                               )
                                                        )
                                                        
                                                        , 
                                                        
                                                        fluidRow(
                                                          column(width = 6, radioGroupButtons(inputId = "Input_Analyses_MonoCompaVehicules_PhasesACV_ChoixVitesses",
                                                                                              label = "Vitesses de circulation",
                                                                                              choices = c("Réseau" = "reseau","Lente" = "lent", "Moyenne" = "moyen",
                                                                                                          "Rapide" = "rapide", "Très rapide" = "très.rapide",
                                                                                                          "Personnalisée" = "perso"),
                                                                                              selected = "moyen",
                                                                                              status = "info"
                                                          ),
                                                          conditionalPanel(
                                                            condition = "input.Input_Analyses_MonoCompaVehicules_PhasesACV_ChoixVitesses == 'reseau'",
                                                            span(textOutput("MonoCompaVehicules_PhasesACV_vitessereseau"),style="font-size:1.4em")
                                                          ),
                                                          conditionalPanel(
                                                            condition = "input.Input_Analyses_MonoCompaVehicules_PhasesACV_ChoixVitesses == 'lent'",
                                                            h4("vitesse moyenne : 32 km/h")
                                                          ),
                                                          conditionalPanel(
                                                            condition = "input.Input_Analyses_MonoCompaVehicules_PhasesACV_ChoixVitesses == 'moyen'",
                                                            h4("vitesse moyenne : 51 km/h")
                                                          ),
                                                          conditionalPanel(
                                                            condition = "input.Input_Analyses_MonoCompaVehicules_PhasesACV_ChoixVitesses == 'rapide'",
                                                            h4("vitesse moyenne : 72 km/h")
                                                          ),
                                                          conditionalPanel(
                                                            condition = "input.Input_Analyses_MonoCompaVehicules_PhasesACV_ChoixVitesses == 'très.rapide'",
                                                            h4("vitesse moyenne : 106 km/h")
                                                          ),
                                                          conditionalPanel(
                                                            condition = "input.Input_Analyses_MonoCompaVehicules_PhasesACV_ChoixVitesses == 'perso'",
                                                            numericInput("Input_Analyses_MonoCompaVehicules_PhasesACV_vitesse", label =h4("vitesse en km/h"), value = 50, min = 5, max = 130)
                                                          )
                                                          )
                                                        )
                                                      )
                                                      ,
                                                      
                                                      withSpinner(plotlyOutput("GRAPH_MonoCompaVehicules_PhasesACV",height = "600px")),
                                                      downloadButton("download_MonoCompaVehicules_PhasesACV", "Télécharger les données", class = "butt1")                                                     
                                                      
                                                    ) # fin boxPlus MonoCompaVehicules / phases ACV
                                                    ,
                                                    
                                                    # boxPlus( # début boxPlus MonoCompaVehicules / véhicules 
                                                    box( # début boxPlus MonoCompaVehicules / véhicules 
                                                      title = "Contribution des différents véhicules", 
                                                      width = NULL,
                                                      closable = FALSE,
                                                      collapsible = TRUE,
                                                      collapsed = FALSE,
                                                      enable_label = TRUE,
                                                      solidHeader = FALSE, 
                                                      
                                                      uiOutput("MonoCompaVehicules_Vehicules_Indicateurs")
                                                      
                                                      ,
                                                      
                                                      fluidRow(
                                                        column(width = 5,
                                                               pickerInput(
                                                                 inputId = "Input_Analyses_MonoCompaVehicules_Vehicules_MotoEuroSegment",
                                                                 label = "Type de véhicules :",
                                                                 width = 400,
                                                                 choices = c("Motorisation" = "Moto",
                                                                             "Normes Euro" = "Euro",
                                                                             "Segments de puissance" = "Segment"),
                                                                 selected = c("Moto"),
                                                                 multiple = TRUE,
                                                                 options = list(
                                                                   style = "btn-success")
                                                               )
                                                               
                                                        )
                                                        ,
                                                        
                                                        fluidRow(
                                                          column(width = 6, radioGroupButtons(inputId = "Input_Analyses_MonoCompaVehicules_Vehicules_ChoixVitesses",
                                                                                              label = "Vitesses de circulation",
                                                                                              choices = c("Réseau" = "reseau","Lente" = "lent", "Moyenne" = "moyen",
                                                                                                          "Rapide" = "rapide", "Très rapide" = "très.rapide",
                                                                                                          "Personnalisée" = "perso"),
                                                                                              selected = "moyen",
                                                                                              status = "info"
                                                          ),
                                                          conditionalPanel(
                                                            condition = "input.Input_Analyses_MonoCompaVehicules_Vehicules_ChoixVitesses == 'reseau'",
                                                            span(textOutput("MonoCompaVehicules_Vehicules_vitessereseau"),style="font-size:1.4em")
                                                          ),
                                                          conditionalPanel(
                                                            condition = "input.Input_Analyses_MonoCompaVehicules_Vehicules_ChoixVitesses == 'lent'",
                                                            h4("vitesse moyenne : 32 km/h")
                                                          ),
                                                          conditionalPanel(
                                                            condition = "input.Input_Analyses_MonoCompaVehicules_Vehicules_ChoixVitesses == 'moyen'",
                                                            h4("vitesse moyenne : 51 km/h")
                                                          ),
                                                          conditionalPanel(
                                                            condition = "input.Input_Analyses_MonoCompaVehicules_Vehicules_ChoixVitesses == 'rapide'",
                                                            h4("vitesse moyenne : 72 km/h")
                                                          ),
                                                          conditionalPanel(
                                                            condition = "input.Input_Analyses_MonoCompaVehicules_Vehicules_ChoixVitesses == 'très.rapide'",
                                                            h4("vitesse moyenne : 106 km/h")
                                                          ),
                                                          conditionalPanel(
                                                            condition = "input.Input_Analyses_MonoCompaVehicules_Vehicules_ChoixVitesses == 'perso'",
                                                            numericInput("Input_Analyses_MonoCompaVehicules_Vehicules_vitesse", label =h4("vitesse en km/h"), value = 50, min = 5, max = 130)
                                                          )
                                                          )
                                                        )
                                                      )
                                                      
                                                      ,withSpinner(plotlyOutput("GRAPH_MonoCompaVehicules_Vehicules",height = "600px")),
                                                      downloadButton("download_MonoCompaVehicules_Vehicules", "Télécharger les données", class = "butt1")                                                       
                                                      
                                                    ) # fin boxPlus MonoCompaVehicules / véhicules 
                                                    
                                            ) # fin du tabItem 'MonoCompaVehicules'
                                            ,
                                            
                                            
                                            tabItem(tabName = "MonoCompaParcs", ## Onglet MonoCompaParcs ####
                                                    
                                                    h2("Comparaison des différents parcs"),
                                                    
                                                    uiOutput("MonoCompaParcs_choix_calcul"),
                                                    
                                                    # boxPlus( # début boxPlus MonoCompaParcs / Vitesse 
                                                    box( # début boxPlus MonoCompaParcs / Vitesse 
                                                      title = "Evolution avec la vitesse de circulation", 
                                                      width = NULL,
                                                      closable = FALSE,
                                                      collapsible = TRUE,
                                                      collapsed = FALSE,
                                                      enable_label = TRUE,
                                                      solidHeader = FALSE, 
                                                      
                                                      uiOutput("MonoCompaParcs_Vitesse_Indicateurs"),
                                                      
                                                      withSpinner(plotlyOutput("GRAPH_MonoCompaParcs_Vitesse",height = "600px")),
                                                      downloadButton("download_MonoCompaParcs_Vitesse", "Télécharger les données", class = "butt1")
                                                      
                                                      
                                                    ) # fin boxPlus MonoCompaParcs / Vitesse 
                                                    ,
                                                    
                                                    # boxPlus( # début boxPlus MonoCompaParcs / contributions 
                                                    box( # début boxPlus MonoCompaParcs / contributions 
                                                      title = "Comparer les contributions ACV", 
                                                      width = NULL,
                                                      closable = FALSE,
                                                      collapsible = TRUE,
                                                      collapsed = FALSE,
                                                      enable_label = TRUE,
                                                      solidHeader = FALSE, 
                                                      
                                                      uiOutput("MonoCompaParcs_Contributions_Indicateurs")
                                                      
                                                      ,
                                                      
                                                      fluidRow(
                                                        column(width = 6, radioGroupButtons(inputId = "Input_Analyses_MonoCompaParcs_Contributions_ChoixVitesses",
                                                                                            label = "Vitesses de circulation",
                                                                                            choices = c("Lente" = "lent", "Moyenne" = "moyen",
                                                                                                        "Rapide" = "rapide", "Très rapide" = "très.rapide",
                                                                                                        "Personnalisée" = "perso"),
                                                                                            selected = "moyen",
                                                                                            status = "info"
                                                        ),
                                                        conditionalPanel(
                                                          condition = "input.Input_Analyses_MonoCompaParcs_Contributions_ChoixVitesses == 'lent'",
                                                          h4("vitesse moyenne : 32 km/h")
                                                        ),
                                                        conditionalPanel(
                                                          condition = "input.Input_Analyses_MonoCompaParcs_Contributions_ChoixVitesses == 'moyen'",
                                                          h4("vitesse moyenne : 51 km/h")
                                                        ),
                                                        conditionalPanel(
                                                          condition = "input.Input_Analyses_MonoCompaParcs_Contributions_ChoixVitesses == 'rapide'",
                                                          h4("vitesse moyenne : 72 km/h")
                                                        ),
                                                        conditionalPanel(
                                                          condition = "input.Input_Analyses_MonoCompaParcs_Contributions_ChoixVitesses == 'très.rapide'",
                                                          h4("vitesse moyenne : 106 km/h")
                                                        ),
                                                        conditionalPanel(
                                                          condition = "input.Input_Analyses_MonoCompaParcs_Contributions_ChoixVitesses == 'perso'",
                                                          numericInput("Input_Analyses_MonoCompaParcs_Contributions_vitesse", label =h4("vitesse en km/h"), value = 50, min = 5, max = 130)
                                                        )
                                                        )
                                                      )
                                                      
                                                      ,
                                                      withSpinner(plotlyOutput("GRAPH_MonoCompaParcs_Contributions",height = "600px")),
                                                      downloadButton("download_MonoCompaParcs_Contributions", "Télécharger les données", class = "butt1")
                                                      
                                                    ) # fin boxPlus MonoCompaParcs / contributions
                                                    
                                            ) # fin du tabItem 'MonoCompaParcs'
                                            ,
                                            
                                            
                                            tabItem(tabName = "MonoFluxEnergie", ## Onglet MonoFluxEnergie ####
                                                    
                                                    h2("Flux d'énergies"),
                                                    
                                                    uiOutput("MonoFluxEnergie_choix_calcul")
                                                    
                                                    ,
                                                    
                                                    fluidRow(
                                                      column(width = 6, radioGroupButtons(inputId = "Input_Analyses_MonoFluxEnergie_ChoixVitesses",
                                                                                          label = "Vitesses de circulation",
                                                                                          choices = c("Réseau" = "reseau","Lente" = "lent", "Moyenne" = "moyen",
                                                                                                      "Rapide" = "rapide", "Très rapide" = "très.rapide",
                                                                                                      "Personnalisée" = "perso"),
                                                                                          selected = "moyen",
                                                                                          status = "info"
                                                      ),
                                                      conditionalPanel(
                                                        condition = "input.Input_Analyses_MonoFluxEnergie_ChoixVitesses == 'reseau'",
                                                        span(textOutput("MonoFluxEnergie_vitessereseau"),style="font-size:1.4em")
                                                      ),
                                                      conditionalPanel(
                                                        condition = "input.Input_Analyses_MonoFluxEnergie_ChoixVitesses == 'lent'",
                                                        h4("vitesse moyenne : 32 km/h")
                                                      ),
                                                      conditionalPanel(
                                                        condition = "input.Input_Analyses_MonoFluxEnergie_ChoixVitesses == 'moyen'",
                                                        h4("vitesse moyenne : 51 km/h")
                                                      ),
                                                      conditionalPanel(
                                                        condition = "input.Input_Analyses_MonoFluxEnergie_ChoixVitesses == 'rapide'",
                                                        h4("vitesse moyenne : 72 km/h")
                                                      ),
                                                      conditionalPanel(
                                                        condition = "input.Input_Analyses_MonoFluxEnergie_ChoixVitesses == 'très.rapide'",
                                                        h4("vitesse moyenne : 106 km/h")
                                                      ),
                                                      conditionalPanel(
                                                        condition = "input.Input_Analyses_MonoFluxEnergie_ChoixVitesses == 'perso'",
                                                        numericInput("Input_Analyses_MonoFluxEnergie_vitesse", label =h4("vitesse en km/h"), value = 50, min = 5, max = 130)
                                                      )
                                                      )
                                                    )
                                                    
                                                    ,
                                                    
                                                    withSpinner(plotlyOutput("GRAPH_SankeyEnergie", height = 600))
                                                    ,
                                                    downloadButton("download_SankeyEnergie", "Télécharger les données", class = "butt1")
                                                    
                                            ) # fin du tabItem 'MonoFluxEnergie'
                                            ,
                                            
                                            
                                            tabItem(tabName = "MonoACVdetail", ## Onglet MonoACVdetail ####
                                                    
                                                    h2("Analyse détaillée des contributions"),
                                                    fluidRow(
                                                      column(width = 2,
                                                        uiOutput("MonoACVdetail_choix_calcul")
                                                      )
                                                      ,
                                                      column(width = 2,
                                                             radioGroupButtons(inputId = "Input_Analyses_MonoACVdetail_ChoixVitesses",
                                                                        label = "Vitesse de circulation",
                                                                        choices = c("Réseau" = "reseau","Lente" = "lent", "Moyenne" = "moyen",
                                                                                    "Rapide" = "rapide", "Très rapide" = "très.rapide",
                                                                                    "Personnalisée" = "perso"),
                                                                        selected = "moyen",
                                                                        width = '100%',
                                                                        status = "info",
                                                                        direction = "vertical"
                                                              )
                                                              ,
                                                              conditionalPanel(
                                                                condition = "input.Input_Analyses_MonoACVdetail_ChoixVitesses == 'reseau'",
                                                                span(textOutput("MonoACVdetail_vitessereseau"),style="font-size:1.4em")
                                                              ),
                                                              conditionalPanel(
                                                                condition = "input.Input_Analyses_MonoACVdetail_ChoixVitesses == 'lent'",
                                                                h4("vitesse moyenne : 32 km/h")
                                                              ),
                                                              conditionalPanel(
                                                                condition = "input.Input_Analyses_MonoACVdetail_ChoixVitesses == 'moyen'",
                                                                h4("vitesse moyenne : 51 km/h")
                                                              ),
                                                              conditionalPanel(
                                                                condition = "input.Input_Analyses_MonoACVdetail_ChoixVitesses == 'rapide'",
                                                                h4("vitesse moyenne : 72 km/h")
                                                              ),
                                                              conditionalPanel(
                                                                condition = "input.Input_Analyses_MonoACVdetail_ChoixVitesses == 'très.rapide'",
                                                                h4("vitesse moyenne : 106 km/h")
                                                              ),
                                                              conditionalPanel(
                                                                condition = "input.Input_Analyses_MonoACVdetail_ChoixVitesses == 'perso'",
                                                                numericInput("Input_Analyses_MonoACVdetail_vitesse", label =h4("vitesse en km/h"), value = 50, min = 5, max = 130)
                                                              )
                                                      ),
                                                      column(width = 8,
                                                             withSpinner(plotlyOutput("GRAPH_MonoACVdetail_synthese", width = "auto",height = "600px")),
                                                             downloadButton("download_MonoACVdetail1", "Télécharger les données", class = "butt1")
                                                      )
                                                    )
                                                    ,
                                                    br(),
                                                    # boxPlus( # début boxPlus MonoACVdetail / Vitesse 
                                                    box( # début boxPlus MonoACVdetail / Vitesse 
                                                      title = "Evolution avec la vitesse de circulation", 
                                                      width = NULL,
                                                      closable = FALSE,
                                                      collapsible = TRUE,
                                                      collapsed = FALSE,
                                                      enable_label = TRUE,
                                                      solidHeader = FALSE,
                                                      fluidRow(
                                                        
                                                        column(width = 5,
                                                               uiOutput("MonoACVdetail_Indicateurs1")
                                                        )
                                                      ),
                                                      br(),
                                                      fluidRow(
                                                        column(width = 10, offset = 1,
                                                        checkboxGroupButtons(
                                                          inputId = "MonoACVdetail_Phases1",
                                                          label = "Ajouter du détail pour :",
                                                          choices = c("Emissions directes" = "Gaz", 
                                                                      "Types de carburants" = "Carburants", 
                                                                      "Parties du véhicule" = "Vehicules", 
                                                                      "Types d'infrastructures" = "Infra"),
                                                          justified = TRUE,
                                                          checkIcon = list(
                                                            yes = icon("ok", 
                                                                       lib = "glyphicon"))
                                                        )
                                                        
                                                      )
                                                      )
                                                      , 
                                                      
                                                      withSpinner(plotlyOutput("GRAPH_MonoACVdetail_vitesse",height = "600px")),
                                                      downloadButton("download_MonoACVdetail2", "Télécharger les données", class = "butt1")
                                                    ),
                                                    # boxPlus( # début boxPlus MonoACVdetail / type veh 
                                                    box( # début boxPlus MonoACVdetail / type veh 
                                                      title = "Analyse par type de véhicule", 
                                                      width = NULL,
                                                      closable = FALSE,
                                                      collapsible = TRUE,
                                                      collapsed = FALSE,
                                                      enable_label = TRUE,
                                                      solidHeader = FALSE,
                                                      fluidRow(
                                                        
                                                        column(width = 5,
                                                               uiOutput("MonoACVdetail_Indicateurs2")
                                                        )
                                                      ),
                                                      br(),
                                                      fluidRow(
                                                        column(width = 6, 
                                                               radioGroupButtons(inputId = "Input_Analyses_MonoACVdetail_ChoixVitesses2",
                                                                                 label = "Vitesses de circulation",
                                                                                 choices = c("Réseau" = "reseau","Lente" = "lent", "Moyenne" = "moyen",
                                                                                             "Rapide" = "rapide", "Très rapide" = "très.rapide",
                                                                                             "Personnalisée" = "perso"),
                                                                                 selected = "moyen",
                                                                                 status = "info"
                                                               ),
                                                               conditionalPanel(
                                                                 condition = "input.Input_Analyses_MonoACVdetail_ChoixVitesses2 == 'reseau'",
                                                                 span(textOutput("MonoACVdetail_vitessereseau2"),style="font-size:1.4em")
                                                               ),
                                                               conditionalPanel(
                                                                 condition = "input.Input_Analyses_MonoACVdetail_ChoixVitesses2 == 'lent'",
                                                                 h4("vitesse moyenne : 32 km/h")
                                                               ),
                                                               conditionalPanel(
                                                                 condition = "input.Input_Analyses_MonoACVdetail_ChoixVitesses2 == 'moyen'",
                                                                 h4("vitesse moyenne : 51 km/h")
                                                               ),
                                                               conditionalPanel(
                                                                 condition = "input.Input_Analyses_MonoACVdetail_ChoixVitesses2 == 'rapide'",
                                                                 h4("vitesse moyenne : 72 km/h")
                                                               ),
                                                               conditionalPanel(
                                                                 condition = "input.Input_Analyses_MonoACVdetail_ChoixVitesses2 == 'très.rapide'",
                                                                 h4("vitesse moyenne : 106 km/h")
                                                               ),
                                                               conditionalPanel(
                                                                 condition = "input.Input_Analyses_MonoACVdetail_ChoixVitesses2 == 'perso'",
                                                                 numericInput("Input_Analyses_MonoACVdetail_vitesse2", label =h4("vitesse en km/h"), value = 50, min = 5, max = 130)
                                                               )
                                                        ),
                                                        column(width = 4,
                                                               pickerInput(
                                                                 inputId = "Input_Analyses_MonoACVdetail_Vehicules_MotoEuroSegment",
                                                                 label = "Type de véhicules :",
                                                                 width = 400,
                                                                 choices = c("Motorisation" = "Moto",
                                                                             "Normes Euro" = "Euro",
                                                                             "Segments de puissance" = "Segment"),
                                                                 selected = c("Moto"),
                                                                 multiple = TRUE,
                                                                 options = list(
                                                                   style = "btn-success")
                                                               ))
                                                        
                                                      ),
                                                      fluidRow(
                                                        column(width = 10, offset = 1,
                                                        checkboxGroupButtons(
                                                          inputId = "MonoACVdetail_Phases2",
                                                          label = "Ajouter du détail pour :",
                                                          choices = c("Emissions directes" = "Gaz", 
                                                                      "Types de carburants" = "Carburants", 
                                                                      "Parties du véhicule" = "Vehicules", 
                                                                      "Types d'infrastructures" = "Infra"),
                                                          justified = TRUE,
                                                          checkIcon = list(
                                                            yes = icon("ok", 
                                                                       lib = "glyphicon"))
                                                        )
                                                        
                                                      )
                                                      )
                                                      , 
                                                      
                                                      withSpinner(plotlyOutput("GRAPH_MonoACVdetail_typeveh",height = "600px")),
                                                      downloadButton("download_MonoACVdetail3", "Télécharger les données", class = "butt1")
                                                    )
                                            ) # fin du tabItem 'MonoACVdetail'
                                            ,
                                            
                                            
                                            tabItem(tabName = "MonoDommage", ## Onglet MonoDommage ####
                                                    
                                                    h2("Analyse des indicateurs de dommages (endpoint)"),
                                                    
                                                    uiOutput("MonoDommage_choix_calcul")

                                                    ,

                                                    fluidRow(
                                                      column(width = 3,
                                                             radioGroupButtons(inputId = "Input_Analyses_MonoDommage_ChoixVitesses",
                                                                               label = "Vitesses de circulation",
                                                                               choices = c("Réseau" = "reseau","Lente" = "lent", "Moyenne" = "moyen",
                                                                                           "Rapide" = "rapide", "Très rapide" = "très.rapide",
                                                                                           "Personnalisée" = "perso"),
                                                                               selected = "moyen",
                                                                               status = "info",
                                                                               direction = "vertical"
                                                             ),
                                                             conditionalPanel(
                                                               condition = "input.Input_Analyses_MonoDommage_ChoixVitesses == 'reseau'",
                                                               span(textOutput("MonoDommage_vitessereseau"),style="font-size:1.4em")
                                                             ),
                                                             conditionalPanel(
                                                               condition = "input.Input_Analyses_MonoDommage_ChoixVitesses == 'lent'",
                                                               h4("vitesse moyenne : 32 km/h")
                                                             ),
                                                             conditionalPanel(
                                                               condition = "input.Input_Analyses_MonoDommage_ChoixVitesses == 'moyen'",
                                                               h4("vitesse moyenne : 51 km/h")
                                                             ),
                                                             conditionalPanel(
                                                               condition = "input.Input_Analyses_MonoDommage_ChoixVitesses == 'rapide'",
                                                               h4("vitesse moyenne : 72 km/h")
                                                             ),
                                                             conditionalPanel(
                                                               condition = "input.Input_Analyses_MonoDommage_ChoixVitesses == 'très.rapide'",
                                                               h4("vitesse moyenne : 106 km/h")
                                                             ),
                                                             conditionalPanel(
                                                               condition = "input.Input_Analyses_MonoDommage_ChoixVitesses == 'perso'",
                                                               numericInput("Input_Analyses_MonoDommage_vitesse", label =h4("vitesse en km/h"), value = 50, min = 5, max = 130)
                                                             )
                                                      )
                                                    ,
                                                      column(width = 8,

                                                             radioGroupButtons(inputId = "Input_Analyses_MonoDommage_SanteBiodiv1",
                                                                               label = NULL,
                                                                               choices = c("Atteinte à la santé humaine" = "Sante",
                                                                                           "Atteinte aux écosystèmes" = "Biodiv"),
                                                                               selected = "Sante",
                                                                               status = "info"
                                                             ),
                                                             withSpinner(plotlyOutput("GRAPH_MonoDommage")),
                                                             downloadButton("download_MonoDommage1", "Télécharger les données", class = "butt1")

                                                      )


                                                    )
                                                   ,

                                                     fluidRow(
                                                      column(width = 10, offset = 1,
                                                          DTOutput("DT_MonoDommages")


                                                      )

                                                     )
                                                    ,
                                                    br(),br(),

                                                    # boxPlus(title = "Evolution avec la vitesse de circulation", 
                                                   box(title = "Evolution avec la vitesse de circulation", 
                                                            width = NULL,
                                                            closable = FALSE,
                                                            collapsible = TRUE,
                                                            collapsed = FALSE,
                                                            enable_label = TRUE,
                                                            solidHeader = FALSE,
                                                            radioGroupButtons(inputId = "Input_Analyses_MonoDommage_SanteBiodiv2",
                                                                              label = NULL,
                                                                              choices = c("Atteinte à la santé humaine" = "Sante",
                                                                                          "Atteinte aux écosystèmes" = "Biodiv"),
                                                                              selected = "Sante",
                                                                              status = "info"
                                                            ),
                                                            withSpinner(plotlyOutput("GRAPH_MonoDommage_vitesse",height = "600px")),
                                                            downloadButton("download_MonoDommage2", "Télécharger les données", class = "butt1")
                                                    )
                                                    
                                                    
                                                    
                                            ) # fin du tabItem 'MonoDommage'
                                            ,
                                            tabItem(tabName = "MultiContributions", ## Onglet MultiContributions ####
                                                    
                                                    h2("Analyse de contributions multicritère"),
                                                    
                                                    fluidRow(
                                                      
                                                      column(width = 5,
                                                             uiOutput("MultiContributions_choix_calcul"),
                                                             br(),
                                                             uiOutput("MultiContributions_Indicateurs")
                                                      ),
                                                      column(width = 7,radioGroupButtons(inputId = "Input_Analyses_MultiContributions_ChoixVitesses",
                                                                                         label = "Vitesses de circulation",
                                                                                         choices = c("Réseau" = "reseau","Lente" = "lent", "Moyenne" = "moyen",
                                                                                                     "Rapide" = "rapide", "Très rapide" = "très.rapide",
                                                                                                     "Personnalisée" = "perso"),
                                                                                         selected = "moyen",
                                                                                         status = "info"
                                                      ),
                                                      conditionalPanel(
                                                        condition = "input.Input_Analyses_MultiContributions_ChoixVitesses == 'reseau'",
                                                        span(textOutput("MultiContributions_vitessereseau"),style="font-size:1.4em")
                                                      ),
                                                      conditionalPanel(
                                                        condition = "input.Input_Analyses_MultiContributions_ChoixVitesses == 'lent'",
                                                        h4("vitesse moyenne : 32 km/h")
                                                      ),
                                                      conditionalPanel(
                                                        condition = "input.Input_Analyses_MultiContributions_ChoixVitesses == 'moyen'",
                                                        h4("vitesse moyenne : 51 km/h")
                                                      ),
                                                      conditionalPanel(
                                                        condition = "input.Input_Analyses_MultiContributions_ChoixVitesses == 'rapide'",
                                                        h4("vitesse moyenne : 72 km/h")
                                                      ),
                                                      conditionalPanel(
                                                        condition = "input.Input_Analyses_MultiContributions_ChoixVitesses == 'très.rapide'",
                                                        h4("vitesse moyenne : 106 km/h")
                                                      ),
                                                      conditionalPanel(
                                                        condition = "input.Input_Analyses_MultiContributions_ChoixVitesses == 'perso'",
                                                        numericInput("Input_Analyses_MultiContributions_vitesse", label =h4("vitesse en km/h"), value = 50, min = 5, max = 130)
                                                      )
                                                      ,
                                                        column(width = 3,
                                                               pickerInput(
                                                                 inputId = "Input_Analyses_MultiContributions_phaseACV_TRUE",
                                                                 label = "Décomposition cycles ACV :",
                                                                 choices = c("Oui" = TRUE,
                                                                             "Non" = FALSE),
                                                                 options = list(
                                                                   style = "btn-success")
                                                               )
                                                        ),
                                                        conditionalPanel(
                                                          condition = "input.Input_Analyses_MultiContributions_phaseACV_TRUE == 'FALSE'",
                                                          column(width = 4,
                                                                 pickerInput(
                                                                   inputId = "Input_Analyses_MultiContributions_MotoEuroSegment",
                                                                   label = "Choix :",
                                                                   choices = c("Motorisation" = "Moto",
                                                                               "Normes Euro" = "Euro",
                                                                               "Segments de puissance" = "Segment"),
                                                                   multiple = TRUE,
                                                                   selected = "Moto",
                                                                   options = list(
                                                                     style = "btn-success")
                                                                 )
                                                          )
                                                        )
                                                      )
                                                    )
                                                    
                                                    , 
                                                    
                                                    withSpinner(plotlyOutput("GRAPH_MultiContributions",height = "800px")),
                                                    downloadButton("download_MultiContributions", "Télécharger les données", class = "butt1")
                                                    
                                            ) # fin du tabItem 'MultiContributions'
                                            ,
                                            
                                            
                                            tabItem(tabName = "MultiCompaVehicules", ## Onglet MultiCompaVehicules ####
                                                    
                                                    h2("Comparaison multicritère de différents véhicules"),
                                                    
                                                    fluidRow(
                                                      
                                                      column(width = 5,
                                                             uiOutput("MultiCompaVehicules_choix_calcul"),
                                                             br(),
                                                             uiOutput("MultiCompaVehicules_Indicateurs")
                                                      ),
                                                      column(width = 7,radioGroupButtons(inputId = "Input_Analyses_MultiCompaVehicules_ChoixVitesses",
                                                                                         label = "Vitesses de circulation",
                                                                                         choices = c("Réseau" = "reseau","Lente" = "lent", "Moyenne" = "moyen",
                                                                                                     "Rapide" = "rapide", "Très rapide" = "très.rapide",
                                                                                                     "Personnalisée" = "perso"),
                                                                                         selected = "moyen",
                                                                                         status = "info"
                                                      ),
                                                      conditionalPanel(
                                                        condition = "input.Input_Analyses_MultiCompaVehicules_ChoixVitesses == 'reseau'",
                                                        span(textOutput("MultiCompaVehicules_vitessereseau"),style="font-size:1.4em")
                                                      ),
                                                      conditionalPanel(
                                                        condition = "input.Input_Analyses_MultiCompaVehicules_ChoixVitesses == 'lent'",
                                                        h4("vitesse moyenne : 32 km/h")
                                                      ),
                                                      conditionalPanel(
                                                        condition = "input.Input_Analyses_MultiCompaVehicules_ChoixVitesses == 'moyen'",
                                                        h4("vitesse moyenne : 51 km/h")
                                                      ),
                                                      conditionalPanel(
                                                        condition = "input.Input_Analyses_MultiCompaVehicules_ChoixVitesses == 'rapide'",
                                                        h4("vitesse moyenne : 72 km/h")
                                                      ),
                                                      conditionalPanel(
                                                        condition = "input.Input_Analyses_MultiCompaVehicules_ChoixVitesses == 'très.rapide'",
                                                        h4("vitesse moyenne : 106 km/h")
                                                      ),
                                                      conditionalPanel(
                                                        condition = "input.Input_Analyses_MultiCompaVehicules_ChoixVitesses == 'perso'",
                                                        numericInput("Input_Analyses_MultiCompaVehicules_vitesse", label =h4("vitesse en km/h"), value = 50, min = 5, max = 130)
                                                      )
                                                      )
                                                      
                                                    )
                                                    
                                                    , 
                                                    fluidRow(
                                                      column(width = 5,
                                                      pickerInput(
                                                        inputId = "Input_Analyses_MultiCompaVehicules_TypeGraph",
                                                        label = "Type de graphique :",
                                                        width = 400,
                                                        choices = c("Polaire" = "scatterpolar",
                                                                    "Barre" = "bar")
                                                      )
                                                      )
                                                      ,
                                                      column(width = 5,pickerInput(
                                                        inputId = "Input_Analyses_MultiCompaVehicules_MotoEuroSegment",
                                                        label = "Choix :",
                                                        choices = c("Motorisation" = "Moto",
                                                                    "Normes Euro" = "Euro",
                                                                    "Segments de puissance" = "Segment"),
                                                        multiple = TRUE,
                                                        selected = "Moto",
                                                        options = list(
                                                          style = "btn-success")
                                                      )
                                                      )
                                                    ),
                                                    conditionalPanel(
                                                      condition = "input.Input_Analyses_MultiCompaVehicules_TypeGraph == 'scatterpolar'",
                                                      withSpinner(plotlyOutput("GRAPH_MultiCompaVehicules_scatterpolar",height = "600px"))
                                                    ),
                                                    conditionalPanel(
                                                      condition = "input.Input_Analyses_MultiCompaVehicules_TypeGraph == 'bar'",
                                                      withSpinner(plotlyOutput("GRAPH_MultiCompaVehicules_bar",height = "600px"))
                                                    ),
                                                    downloadButton("download_MultiCompaVehicules", "Télécharger les données", class = "butt1")
                                                    
                                                    
                                                    
                                                    
                                            ) # fin du tabItem 'MultiCompaVehicules'
                                            ,
                                            
                                            
                                            tabItem(tabName = "MultiCompaConfigs", ## Onglet MultiCompaConfigs ####
                                                    
                                                    h2("Comparaison multicritère de différents parcs")
                                                    
                                                    ,
                                                    
                                                    uiOutput("MultiCompaConfigs_choix_calcul"),
                                                    
                                                    uiOutput("MultiCompaConfigs_Indicateurs")
                                                    
                                                    ,
                                                    
                                                    fluidRow(
                                                      column(width = 6,
                                                             pickerInput(
                                                                inputId = "Input_Analyses_MultiCompaConfigs_TypeGraph",
                                                                label = "Type de graphique :",
                                                                width = 400,
                                                                choices = c("Polaire" = "scatterpolar",
                                                                            "Barre" = "bar")
                                                      ))
                                                      ,
                                                      column(width = 6, radioGroupButtons(inputId = "Input_Analyses_MultiCompaConfigs_ChoixVitesses",
                                                                                          label = "Vitesses de circulation",
                                                                                          choices = c("Lente" = "lent", "Moyenne" = "moyen",
                                                                                                      "Rapide" = "rapide", "Très rapide" = "très.rapide",
                                                                                                      "Personnalisée" = "perso"),
                                                                                          selected = "moyen",
                                                                                          status = "info"
                                                      ),
                                                      conditionalPanel(
                                                        condition = "input.Input_Analyses_MultiCompaConfigs_ChoixVitesses == 'lent'",
                                                        h4("vitesse moyenne : 32 km/h")
                                                      ),
                                                      conditionalPanel(
                                                        condition = "input.Input_Analyses_MultiCompaConfigs_ChoixVitesses == 'moyen'",
                                                        h4("vitesse moyenne : 51 km/h")
                                                      ),
                                                      conditionalPanel(
                                                        condition = "input.Input_Analyses_MultiCompaConfigs_ChoixVitesses == 'rapide'",
                                                        h4("vitesse moyenne : 72 km/h")
                                                      ),
                                                      conditionalPanel(
                                                        condition = "input.Input_Analyses_MultiCompaConfigs_ChoixVitesses == 'très.rapide'",
                                                        h4("vitesse moyenne : 106 km/h")
                                                      ),
                                                      conditionalPanel(
                                                        condition = "input.Input_Analyses_MultiCompaConfigs_ChoixVitesses == 'perso'",
                                                        numericInput("Input_Analyses_MultiCompaConfigs_vitesse", label =h4("vitesse en km/h"), value = 50, min = 5, max = 130)
                                                      )
                                                      )
                                                    )
                                                    
                                                    
                                                    ,
                                                    
                                                    conditionalPanel(
                                                      condition = "input.Input_Analyses_MultiCompaConfigs_TypeGraph == 'scatterpolar'",
                                                      withSpinner(plotlyOutput("GRAPH_MultiCompaConfigs_scatterpolar",height = "600px"))
                                                    ),
                                                    conditionalPanel(
                                                      condition = "input.Input_Analyses_MultiCompaConfigs_TypeGraph == 'bar'",
                                                      withSpinner(plotlyOutput("GRAPH_MultiCompaConfigs_bar",height = "600px"))
                                                    ),
                                                    downloadButton("download_MultiCompaConfigs", "Télécharger les données", class = "butt1")
                                                    
                                                    # ) # fin du conditionalPanel
                                                    
                                            ) # fin du tabItem 'MultiCompaConfigs'
                                            ,
                                            
                                            tabItem(tabName = "MultiACVdetail", ## Onglet MultiACVdetail ####
                                                    
                                                    h2("Analyse détaillée des contributions")
                                                    
                                                    ,
                                                    fluidRow(
                                                      column(width = 5,
                                                        uiOutput("MultiACVdetail_choix_calcul"),
                                                        br(),
                                                        uiOutput("MultiACVdetail_Indicateurs")
                                                      )
                                                      ,
                                                      column(width = 7, radioGroupButtons(inputId = "Input_Analyses_MultiACVdetail_ChoixVitesses",
                                                                                          label = "Vitesses de circulation",
                                                                                          choices = c("Réseau" = "reseau","Lente" = "lent", "Moyenne" = "moyen",
                                                                                                      "Rapide" = "rapide", "Très rapide" = "très.rapide",
                                                                                                      "Personnalisée" = "perso"),
                                                                                          selected = "moyen",
                                                                                          status = "info"
                                                      ),
                                                      conditionalPanel(
                                                        condition = "input.Input_Analyses_MultiACVdetail_ChoixVitesses == 'reseau'",
                                                        span(textOutput("MultiACVdetail_vitessereseau"),style="font-size:1.4em")
                                                      ),
                                                      conditionalPanel(
                                                        condition = "input.Input_Analyses_MultiACVdetail_ChoixVitesses == 'lent'",
                                                        h4("vitesse moyenne : 32 km/h")
                                                      ),
                                                      conditionalPanel(
                                                        condition = "input.Input_Analyses_MultiACVdetail_ChoixVitesses == 'moyen'",
                                                        h4("vitesse moyenne : 51 km/h")
                                                      ),
                                                      conditionalPanel(
                                                        condition = "input.Input_Analyses_MultiACVdetail_ChoixVitesses == 'rapide'",
                                                        h4("vitesse moyenne : 72 km/h")
                                                      ),
                                                      conditionalPanel(
                                                        condition = "input.Input_Analyses_MultiACVdetail_ChoixVitesses == 'très.rapide'",
                                                        h4("vitesse moyenne : 106 km/h")
                                                      ),
                                                      conditionalPanel(
                                                        condition = "input.Input_Analyses_MultiACVdetail_ChoixVitesses == 'perso'",
                                                        numericInput("Input_Analyses_MultiACVdetail_vitesse", label =h4("vitesse en km/h"), value = 50, min = 5, max = 130)
                                                      )
                                                      )
                                                    )
                                                    ,
                                                    fluidRow(
                                                      column(width = 10, offset = 1,
                                                             checkboxGroupButtons(
                                                               inputId = "MultiACVdetail_Phases",
                                                               label = "Ajouter du détail pour :",
                                                               choices = c("Emissions directes" = "Gaz", 
                                                                           "Types de carburants" = "Carburants", 
                                                                           "Parties du véhicule" = "Vehicules", 
                                                                           "Types d'infrastructures" = "Infra"),
                                                               justified = TRUE,
                                                               checkIcon = list(
                                                                 yes = icon("ok", 
                                                                            lib = "glyphicon"))
                                                             )
                                                             
                                                      )
                                                    )
                                                    ,
                                                    withSpinner(plotlyOutput("GRAPH_MultiACVdetail",height = "600px")),
                                                    downloadButton("download_MultiACVdetail", "Télécharger les données", class = "butt1")
                                                    
                                                    
                                            ) # fin du tabItem 'MultiACVdetail'
                                            
                                            ,
                                            
                                            tabItem(tabName = "EmissionsDirectes", ## Onglet EmissionsDirectes ####
                                                    
                                                    h2("Analyse des émissions directes et des consommations")
                                                    
                                                    ,
                                                    
                                                    uiOutput("EmissionsDirectes_choix_calcul"),
                                                    
                                                    # boxPlus(title = "Evolution avec la vitesse de circulation", 
                                                    box(title = "Evolution avec la vitesse de circulation", 
                                                            width = NULL,
                                                            closable = FALSE,
                                                            collapsible = TRUE,
                                                            collapsed = FALSE,
                                                            enable_label = TRUE,
                                                            solidHeader = FALSE,
                                                            withSpinner(plotlyOutput("GRAPH_EmissionsDirectes_vitesse",height = "600px")),
                                                            fluidRow(
                                                              column(width = 4,
                                                                     downloadButton("download_EmissionsDirectes_Vitesse", "Télécharger les données", class = "butt1")
                                                              ),
                                                              column(width = 8,
                                                                     checkboxGroupButtons(
                                                                       inputId = "Input_EmissionsDiectes",
                                                                       label = "Ajouter des types de véhicules :", 
                                                                       choices = c("Motorisations" = "Moto","Normes Euro" = "Euro","Segments de puissance" = "Segment"),
                                                                       justified = TRUE,
                                                                       checkIcon = list(
                                                                         yes = icon("ok",lib = "glyphicon"))
                                                                     )
                                                              )
                                                            )
                                                    
                                                    )
                                                    ,

                                                    # boxPlus(title = "Tableau d'émissions et de consommations",
                                                    box(title = "Tableau d'émissions et de consommations",
                                                            width = NULL,
                                                            closable = FALSE,
                                                            collapsible = TRUE,
                                                            collapsed = FALSE,
                                                            enable_label = TRUE,
                                                            solidHeader = FALSE,
                                                            column(width = 3,
                                                                   radioGroupButtons(inputId = "Input_Analyses_EmissionsDirectes_ChoixVitesses",
                                                                                     label = "Vitesses de circulation",
                                                                                     choices = c("Réseau" = "reseau","Lente" = "lent", "Moyenne" = "moyen",
                                                                                                 "Rapide" = "rapide", "Très rapide" = "très.rapide",
                                                                                                 "Personnalisée" = "perso"),
                                                                                     selected = "moyen",
                                                                                     status = "info",
                                                                                     direction = "vertical"
                                                                   ),
                                                                   conditionalPanel(
                                                                     condition = "input.Input_Analyses_EmissionsDirectes_ChoixVitesses == 'reseau'",
                                                                     span(textOutput("EmissionsDirectes_vitessereseau"),style="font-size:1.4em")
                                                                   ),
                                                                   conditionalPanel(
                                                                     condition = "input.Input_Analyses_EmissionsDirectes_ChoixVitesses == 'lent'",
                                                                     h4("vitesse moyenne : 32 km/h")
                                                                   ),
                                                                   conditionalPanel(
                                                                     condition = "input.Input_Analyses_EmissionsDirectes_ChoixVitesses == 'moyen'",
                                                                     h4("vitesse moyenne : 51 km/h")
                                                                   ),
                                                                   conditionalPanel(
                                                                     condition = "input.Input_Analyses_EmissionsDirectes_ChoixVitesses == 'rapide'",
                                                                     h4("vitesse moyenne : 72 km/h")
                                                                   ),
                                                                   conditionalPanel(
                                                                     condition = "input.Input_Analyses_EmissionsDirectes_ChoixVitesses == 'très.rapide'",
                                                                     h4("vitesse moyenne : 106 km/h")
                                                                   ),
                                                                   conditionalPanel(
                                                                     condition = "input.Input_Analyses_EmissionsDirectes_ChoixVitesses == 'perso'",
                                                                     numericInput("Input_Analyses_EmissionsDirectes_vitesse", label =h4("vitesse en km/h"), value = 50, min = 5, max = 130)
                                                                   ) 
                                                            ),
                                                          column(width = 8,   
                                                                 DTOutput("DT_EmissionsDirectes_gaz"),
                                                                 DTOutput("DT_EmissionsDirectes_carburants")
                                                          )

                                                    )

                                                    ,

                                                    # boxPlus(title = "Comparaison de véhicules",
                                                    box(title = "Comparaison de véhicules",
                                                            width = NULL,
                                                            closable = FALSE,
                                                            collapsible = TRUE,
                                                            collapsed = FALSE,
                                                            enable_label = TRUE,
                                                            solidHeader = FALSE,
                                                            fluidRow(
                                                              column(width = 3,
                                                                   pickerInput(
                                                                     inputId = "Input_Analyses_EmissionsDirectes_MotoEuroSegment",
                                                                     label = "Discrétiser par :",
                                                                     width = 200,
                                                                     choices = c("Motorisations" = "Moto",
                                                                                 "Normes Euro" = "Euro",
                                                                                 "Segments de puissance" = "Segment"),
                                                                     selected = "Moto",
                                                                     multiple = TRUE,
                                                                     options = list(
                                                                       style = "btn-success")
                                                                   )
                                                                   ,
                                                                   radioGroupButtons(inputId = "Input_Analyses_EmissionsDirectes_ChoixVitesses2",
                                                                                     label = "Vitesses de circulation",
                                                                                     choices = c("Réseau" = "reseau","Lente" = "lent", "Moyenne" = "moyen",
                                                                                                 "Rapide" = "rapide", "Très rapide" = "très.rapide",
                                                                                                 "Personnalisée" = "perso"),
                                                                                     selected = "moyen",
                                                                                     status = "info",
                                                                                     direction = "vertical"
                                                                   )
                                                                   ,
                                                                   conditionalPanel(
                                                                     condition = "input.Input_Analyses_EmissionsDirectes_ChoixVitesses2 == 'reseau'",
                                                                     span(textOutput("EmissionsDirectes_vitessereseau2"),style="font-size:1.4em")
                                                                   )
                                                                   ,
                                                                   conditionalPanel(
                                                                     condition = "input.Input_Analyses_EmissionsDirectes_ChoixVitesses2 == 'lent'",
                                                                     h4("vitesse moyenne : 32 km/h")
                                                                   )
                                                                   ,
                                                                   conditionalPanel(
                                                                     condition = "input.Input_Analyses_EmissionsDirectes_ChoixVitesses2 == 'moyen'",
                                                                     h4("vitesse moyenne : 51 km/h")
                                                                   )
                                                                   ,
                                                                   conditionalPanel(
                                                                     condition = "input.Input_Analyses_EmissionsDirectes_ChoixVitesses2 == 'rapide'",
                                                                     h4("vitesse moyenne : 72 km/h")
                                                                   ),
                                                                   conditionalPanel(
                                                                     condition = "input.Input_Analyses_EmissionsDirectes_ChoixVitesses2 == 'très.rapide'",
                                                                     h4("vitesse moyenne : 106 km/h")
                                                                   ),
                                                                   conditionalPanel(
                                                                     condition = "input.Input_Analyses_EmissionsDirectes_ChoixVitesses2 == 'perso'",
                                                                     numericInput("Input_Analyses_EmissionsDirectes_vitesse2", label =h4("vitesse en km/h"), value = 50, min = 5, max = 130)
                                                                   )
                                                            )
                                                            ,
                                                            column(width = 8,
                                                                   radioGroupButtons(inputId = "Input_Analyses_EmissionsDirectes_gazcarb",
                                                                                    label = NULL,
                                                                                    choices = c("Emissions de gaz" = "Gaz",
                                                                                                "Consommations de carburant" = "Carburant"),
                                                                                    selected = "Gaz",
                                                                                    status = "info"
                                                                   )
                                                                   ,
                                                                   DTOutput("DT_EmissionsDirectes_veh")
                                                            )
                                                          )

                                                    )
                                                     
                                                    
                                                    
                                            ) # fin du tabItem 'EmissionsDirectes'
                                            
                                            
                                          ) # fin du tabItems
                                        ) # fin du dashboardBody
                      ) # fin du dashboardPagePlus
             ) # fin du tabPanel "Analyses" 

             ,
             
             # --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
             # --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
             # --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
             
             tabPanel("A propos (en construction)", # TabPanel "A propos" ####
                      style = "padding-top: 100px;",
                      icon = icon("envelope"),
                      
                      fluidRow(
                        column(width = 10, offset=1,
                               
                               # boxPlus(
                               box(
                                 title = div(icon("question-circle"),"Manuel"), 
                                 sidebar_icon = icon("envelope"),
                                 closable = FALSE, 
                                 width = NULL,
                                 enable_label = TRUE,
                                 solidHeader = FALSE, 
                                 collapsible = TRUE,
                                 collapsed = TRUE,
                                 background = "blue",
                                 includeMarkdown("www/Manuel.md")
                               )
                        ))
                      
                      ,
                      
                      fluidRow(
                        column(width = 10, offset=1,
                               # boxPlus(
                               box(
                                 title = div(icon("book"),"Méthodologie"), 
                                 sidebar_icon = icon("envelope"),
                                 closable = FALSE, 
                                 width = NULL,
                                 enable_label = TRUE,
                                 solidHeader = FALSE, 
                                 collapsible = TRUE,
                                 collapsed = TRUE,
                                 background = "blue",
                                 includeMarkdown("www/Method.md")
                               )
                        ))
                      
                      ,
                      
                      fluidRow(
                        column(width = 10, offset=1,
                               # boxPlus(
                               box(
                                 title = div(icon("envelope"),"Mentions légales"), 
                                 sidebar_icon = icon("envelope"),
                                 closable = FALSE, 
                                 width = NULL,
                                 enable_label = TRUE,
                                 solidHeader = FALSE, 
                                 collapsible = TRUE,
                                 collapsed = TRUE,
                                 background = "blue",
                                 includeMarkdown("www/mentions_legales.md")
                               )
                        ))
                      
                      ,
                      
                      fluidRow(
                        column(width = 10, offset=1,
                               
                               # widgetUserBox(
                               userBox(
                                 title = userDescription(
                                                          title = "Contact",
                                                          image = "Cyrille Francois-portrait.png",
                                                          type = 2
                                 )
                                 ,
                                 width = 12,
                                 # type = 2,
                                 boxToolSize = "sm"
                                 ,
                                 # src = "Cyrille Francois-portrait.png",
                                 # image = "Cyrille Francois-portrait.png",
                                 # color = "yellow",
                                 status = "orange",
                                 includeMarkdown("www/Cyrille.md")
                               )
                        ))         
                      
             ) # fin du tabPanel "A propos" 
             
             
             # Footer  ---------------------------
             ,
             footer = div("Copyright 2020 © - UGE    ",
                          style="
                         text-align: center;
                         padding-top: 20px;
                         box-shadow: 0px -5px 10px #d2d2d2;
                         height: 50px;
                         margin-top: 20px;
                         ")

            
  )
