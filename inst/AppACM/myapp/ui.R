options(encoding = "UTF-8")
library(shinythemes)
library(scatterD3)
library(DT)
library(shinyjs)

# --------------------------- TITRE -------------------------------

shinyUI(  
  navbarPage("Analyse de Correspondances Multiples",
             
             
#--------------------------- ONGLET DONNEES / selection -------------------------
         
             tabPanel(
               "1. Chargement",
               
                      fluidPage(
                        
                        
                        
                        # 1ère colonne :
                        
                        
                        column(5,
                               h6(strong('/!\\ Avant toute opération, cliquer que le bouton "Open in browser" 
                                   en haut à gauche afin de pouvoir télécharger les résultats') ),
                               
                               h4("1.1. Fichier à charger (.txt ou .csv)"),
                               h6("La table brute est une base de données dont les lignes correspondent aux individus statistiques.
                                  Elle doit être en format texte (.txt ou .csv) ; Le délimitateur, 
                                  l'extension du fichier et l'encodage des caractères sont précisés.
                                  Elle est importée en passant par le bouton \"browse\"."),
                               wellPanel(
                                 uiOutput("donnees.fichier.ui")),
                               h4("1.2. Vérifier les données d'entrée"),
                               h6("Vérifier que le tableau a correctement été 
                                  importé à l'aide du résumé du tableau :"),
                               textOutput("Dimensions"),
                               tableOutput("Resume")),
                        
                        # 2ème colonne :
                        
                        
                        column(7,
                               fluidRow(column(6, 
                                               h4("2. Choix de l'identifiant (Obligatoire)"),
                                               uiOutput("SelectID"),
                                               uiOutput("ErreurID"),
                                               h6("Si la table ne contient pas d'identifiant
                                                  unique, le bouton ci-dessous permet de 
                                                  télécharger la table à laquelle sera ajouté
                                                  une variable \"ID\"."),
                                               h6(strong("/!\\ Si le tableau comporte déjà une variable\"ID\", elle sera remplacée.")),
                                               downloadButton("PasDID",'Télécharger avec ajout d\'un identifiant ("ID")')
                                                                                             ),
                                        
                                        column(6,
                                               uiOutput("SelectLabelGraphInd"),
                                               uiOutput("ErreurLabelGraphInd")
                                               )),
                               
                               
                               h4("3. Sélection des individus (Optionnel)"),
                               h6("Ces filtres peuvent réduire et préciser la population étudiée. Ce sont des filtres logiques.
                                  Si on ne souhaite aucune sélection, penser à sélectionner un choix vide sur chaque champ."),
                               h6("Les filtres intermédiaires entre les critères portent sur les individus :"),
                               h6("ET : Une même ligne ne sera conservée que si elle satisfait aux deux critères ;"),
                               h6("OU : Une ligne sera conservée si elle répond au 1er critère ou bien si elle répond au 2ème critère."),
                               wellPanel(
                                 p(strong("Critère 1 :"), align="left"),
                                 fluidRow( column(5,
                                                  uiOutput("SelectVar1")),
                                           column(2,
                                                  selectInput("Operateur1", "Opérateur :",
                                                              choices=as.list(c(" ","=","diff. de",">",">=","<","<=")))),
                                           column(5,
                                                  
                                                  uiOutput("Select")))
                                 
                                 
                               ),
                               selectInput("OperateurMid","",
                                           choices=as.list(c(" ","OU","ET")), selected = NULL),
                               wellPanel(
                                 p(strong("Critère 2 :"), align="left"),
                                 fluidRow( column(5,
                                                  uiOutput("SelectVar2")),
                                           column(2,
                                                  selectInput("Operateur2", "Opérateur :",
                                                              choices=as.list(c(" ","=","diff. de",">",">=","<","<=")))),
                                           column(5,
                                                  
                                                  uiOutput("Select2")))),
                               
                               selectInput("OperateurMid2","",
                                           choices=as.list(c(" ","OU","ET")), selected = NULL),
                               wellPanel(
                                 p(strong("Critère 3 :"), align="left"),
                                 fluidRow( column(5,
                                                  uiOutput("SelectVar3")),
                                           column(2,
                                                  selectInput("Operateur3", "Opérateur :",
                                                              choices=as.list(c(" ","=","diff. de",">",">=","<","<=")))),
                                           column(5,
                                                  
                                                  uiOutput("Select3")))
                               ),
                               
                               selectInput("OperateurMid3","",
                                           choices=as.list(c(" ","OU","ET")), selected = NULL),
                               wellPanel(
                                 p(strong("Critère 4 :"), align="left"),
                                 fluidRow( column(5,
                                                  uiOutput("SelectVar4")),
                                           column(2,
                                                  selectInput("Operateur4", "Opérateur :",
                                                              choices=as.list(c(" ","=","diff. de",">",">=","<","<=")))),
                                           column(5,
                                                  
                                                  uiOutput("Select4")))))
                        )),
             
#--------------------------- ONGLET ACM / Inertie -------------------------
             
             tabPanel("2. Modèle et valeurs propres",
                      column(6,
                             
                             h4("1.1. Vérifier : Tri à plat des variables"),
                             h6("Le tri à plat des variables permet de vérifier 
                               si les filtres ont produit la sélection de population attendue."),
                             uiOutput("SelectVarTri"),
                             textOutput("TypeVarTri"),
                             h4("1.2. Facultatif et uniquement pour les tris à plat"),
                             selectInput("ChangementVarTri","Changer le type de la variable",
                                         choices=as.list(c("Pas de modification",
                                                           "Qualitative",
                                                           "Quantitative",
                                                           "Logique")), selected = "Pas de modification"),
                             h4(""),
                             tableOutput("TableVarTri"),
                             h4("2.1. Sélection des variables pour l'ACM - actives et supplémentaires"),
                             h6("Sélection de l'ensemble 
                                des variables intégrées à l'ACM, actives ET supplémentaires."),
                             h6("Pour l'ACM, les variables actives sont automatiquement converties en 
                                variables qualitatives (peu importe leur nature originale)."),
                             wellPanel(
                               checkboxInput("SelectAll", "Tout sélectionner", value=FALSE),
                               uiOutput("SelectACM")
                             ),
                           #  h4("ACM SPECIALE"),
                          #   verbatimTextOutput("TEST"),
                             
                          #   checkboxInput("ACMSpe", "ACM Spéciale", value= FALSE),
                           #  uiOutput("Choose_ModaSpe"),
                             h4("2.2. Choix des variables illustratives"),
                             h6("Parmi les variables conservées, sélection de celles 
                                qui seront considérées comme supplémentaires (projetées 
                                sur les plans factoriels mais ne participant pas à leur construction)."),
                             h6("On distingue les variables supplémentaires qualitatives et quantitatives :"),
                             h5("2.2.1. Qualitatives"),
                             wellPanel(uiOutput("SelectIllus")),
                             h5("2.2.2. Quantitatives"),
                             wellPanel(uiOutput("SelectIllusQuanti")),
                             h4("2.3. Variables conservées"),
                             h6("Rappel des variables conservées et de leurs types :"),
                             tableOutput("Type")),
                      column(6,
                             uiOutput("SelectNbreAxes"),
                             h4("3.1. Valeurs propres"),
                             tableOutput("ValeursPropres"),
                             h4("3.2. Graphiques des VP"),
                             plotOutput("Variance"),
                             plotOutput("VarianceCum"))),
             
             
#--------------------------- ONGLET Graphes des modalités ----------------------
             
             tabPanel("3. Variables : Graphiques",
                      fluidPage(
                        h4("Graphique des axes :"),
                        fluidRow(column(2,numericInput("VarAxeA", "", 1)),
                                 column(2,  numericInput("VarAxeB", "et", 2))),
                        
                        numericInput("MinContribVar1",
                                     "Afficher les étiquettes des modalités dont la contribution est
                                     supérieure à : ",2),
                        fluidRow(column(1,scatterD3Output("GraphVarAxeB", width = "75px", height = "600px") ),
                                 column(11, scatterD3Output("GraphVar"))),
                        fluidRow(column(1),
                                 column(11,scatterD3Output("GraphVarAxeA", width = "615px", height = "75px"))),
                        
                        
                        
                        h4("Graphique des axes :"),
                        fluidRow(column(2,numericInput("VarAxeC", "", 3)),
                                 column(2,  numericInput("VarAxeD", "et", 4))),
                        numericInput("MinContribVar2",
                                     "Afficher les étiquettes des modalités dont la contribution est
                                     supérieure à : ",2),
                        fluidRow(column(1,scatterD3Output("GraphVarAxeD", width = "75px", height = "600px") ),
                                 column(11, scatterD3Output("GraphVar2"))),
                        fluidRow(column(1),
                                 column(11,scatterD3Output("GraphVarAxeC", width = "615px", height = "75px")))
                        )),
             
#--------------------------- ONGLET Tableaux des modalités -------------------------
             
             tabPanel("4. Variables : Tables",
                      fluidPage(column(6,h4("Variables"), wellPanel(fluidRow(
                        column(6,
                               h4("Axe n°"),
                               numericInput("AxeVar1", NA,1)),
                        column(6,
                               h4("Contribution supérieure à"),
                               numericInput("ContribVar1", NA,2)))),
                        dataTableOutput("TableVar"),
                        downloadButton("DlTableVar1","Télécharger")),
                        column(6,h4("Variables"),wellPanel(fluidRow(
                          column(6,
                                 h4("Axe n°"),
                                 numericInput("AxeVar2", NA,2)),
                          column(6,
                                 h4("Contribution supérieure à"),
                                 numericInput("ContribVar2", NA,2)))),
                          dataTableOutput("TableVar2"),
                          downloadButton("DlTableVar2","Télécharger")))),
             
#--------------------------- ONGLET Classification -------------------------
             
             
             tabPanel("5. Classification",
                      fluidRow(column(2, h4("1.1. Type de classification"),
                                      h6("/!\\ Pour l'instant, que hiérarchique ascendante et l'agrégation par les diamètres ne fonctionne pas"),
                                      selectInput("Type.Cl", "",
                                                  choices=as.list(c("Hiérarchique","Non hiérarchique", "Pas de classification")),
                                                  selected="Hiérarchique")),
                               column(2,h4("Options de la classification :"),
                                      uiOutput("NbAxes.Cl.Choix")),
                               column(2,
                                      uiOutput("Metric.Cl.Choix"),
                                      uiOutput("Agreg.Cl.Choix")),
                               column(2,
                                      uiOutput("Part.H.Cl.Choix"),
                                      uiOutput("NbreCl.Cl.Choix"))),
                      fluidRow(
                        column(6,h4("2.1. Dendrodramme"),
                               plotOutput("plot")),
                        column(6,h4 ("2.2. Graphe de l'inertie"),
                               plotOutput("plot3"))
                      ),
                      fluidRow(
                        column(3,h4("3. Effectifs par classe"),
                             tableOutput("TableEffClasses")),
                        column(9,
                             h4("4. Croisement des classes avec d'autres variables (% en colonnes pour var quali)"),
                             wellPanel(uiOutput("SelectVarClasses")),
                             tableOutput("TableVarClasses"),
                             downloadButton("DlCroisCl","Télécharger"))),
                      fluidRow(column(8,h4("5. Classes des individus"),
                                      dataTableOutput("IndEtClasses")),
                               column(4,h4("6. Téléchargements"),
                                      downloadButton('TableIndividusClasses', 'Télécharger les noms et \n classes uniquement'),
                                      hr(),
                                      downloadButton('TableDepClasses', 'Télécharger la table de départ 
                                                     avec les classes') ))),
             
             
#--------------------------- ONGLET Graphes des individus -------------------------
             
             
             tabPanel("6. Individus : Graphiques",
                      fluidPage(  
                                   h4("Graphique des axes :"),
                                   
                                   fluidRow(column(2,numericInput("IndAxe1", "", 1)),
                                            column(2,  numericInput("IndAxe2", "et", 2))),
                                   fluidRow(column(4,h4("Catégories et étiquettes :"),
                                                   uiOutput("SelectVarClassesGraphe")
                                                   ),
                                            column(4,numericInput("MinContrib1","Afficher les étiquettes des individus dont la contribution est
                                             supérieure à : ",.5))),
                                checkboxInput("Ellipse", "Représenter les ellipses", value=FALSE),
                                scatterD3Output("GraphInd"),
                                
                                h4("Graphique des axes :"),
                                fluidRow(column(2,numericInput("IndAxe3", "", 3)),
                                         column(2,  numericInput("IndAxe4", "et", 4))),
                                numericInput("MinContrib2","Afficher les étiquettes des individus dont la contribution est
                                             supérieure à : ",.5),
                                checkboxInput("Ellipse2", "Représenter les ellipses", value=FALSE),
                                scatterD3Output("GraphInd2"))),
             
#--------------------------- ONGLET Tableaux des individus -------------------------
             
             
             tabPanel("7. Individus : Tables",
                      fluidPage(column(6, h4("Individus"), wellPanel(fluidRow(
                        column(6,
                               h4("Axe n°"),
                               numericInput("AxeInd1", NA,1)),
                        column(6,
                               h4("Contribution supérieure à"),
                               numericInput("ContribInd1", NA,.5)))),
                        dataTableOutput("TableInd"),
                        downloadButton("DlTableInd1","Télécharger")),
                        column(6,h4("Individus"),wellPanel(fluidRow(
                          column(6,
                                 h4("Axe n°"),
                                 numericInput("AxeInd2", NA,2)),
                          column(6,
                                 h4("Contribution supérieure à"),
                                 numericInput("ContribInd2", NA,.5)))),
                          dataTableOutput("TableInd2"),
                          downloadButton("DlTableInd2","Télécharger")))),
             
      
#--------------------------- ONGLET Métadonnées sur la session ACM---------------------
             
             tabPanel("8. Données sur la session en cours",
                      fluidPage(
                        h6("Table des métadonnées de la session en cours, issue des sélections et choix effectués."),
                        tableOutput("TableMeta"),
                        downloadButton("DLMetaDonnees", "Télécharger la table")
                      ))
#
# 
#--------------------------- Mises en forme -------------------------
# 
# tags$style(type="text/css",
#            ".shiny-output-error { visibility: hidden; }",
#            ".shiny-output-error:before { visibility: hidden; }")


, tags$head(tags$style(HTML(
  "h6{ background-color: #FFF3BE ; font-size:16px;
  font-family: calibri, Arial, sans-serif ; font-size:16px;}")
  #,type="text/css",
  #          ".shiny-output-error { visibility: hidden; }",
  #           ".shiny-output-error:before { visibility: hidden; }"
)
),theme=shinytheme("united")



 )
)
