#\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
# Packages -----
#\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

require(shiny)
require(FactoMineR)
require(explor)
require(scatterD3)
require(DT)
require(cluster)
require(JLutils)
require(RColorBrewer)
require(questionr)
require(tidyverse)
# require(tidyr)
# require(dplyr)
require(stringr)
# require(stringi)
# require(GDAtools)

options(shiny.maxRequestSize=30*1024^2)

#\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
# Fonctions pour les graphiques -----------
#\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

# ACM Spéciales :
# A FAIRE ?


# MCA Var plot sans fixe = T
MCA_var_data <- function(res, xax = 1, yax = 2, var_sup = TRUE, var_lab_min_contrib = 0) {
  tmp_x <- res$vars %>%
    arrange(Axis, Type, Variable) %>%
    filter(Axis == xax) %>%
    select_("Variable", "Level", "Type", "Class", "Coord", "Contrib", "Cos2", "Count")
  tmp_y <- res$vars %>% 
    filter(Axis == yax) %>%
    select_("Variable", "Level", "Type", "Class", "Coord", "Contrib", "Cos2", "Count")
  if (!(var_sup)) {
    tmp_x <- tmp_x %>% filter(Type == 'Active')
    tmp_y <- tmp_y %>% filter(Type == 'Active')
  }
  tmp <- tmp_x %>%
    left_join(tmp_y, by = c("Variable", "Level", "Type", "Class", "Count")) %>%
    mutate(Contrib = Contrib.x + Contrib.y,
           Cos2 = Cos2.x + Cos2.y,
           tooltip = paste(paste0("<strong>", Level, "</strong><br />"),
                           paste0("<strong>",
                                  gettext("Variable", domain = "R-explor"),
                                  ":</strong> ", Variable, "<br />"),
                           paste0("<strong>Axis ",xax," :</strong> ", Coord.x, "<br />"),
                           paste0("<strong>Axis ", yax," :</strong> ", Coord.y, "<br />"),
                           ifelse(is.na(Cos2), "",
                                  paste0("<strong>",
                                         gettext("Squared cosinus", domain = "R-explor"),
                                         ":</strong> ", Cos2, "<br />")),
                           ifelse(is.na(Contrib), "",
                                  paste0("<strong>",
                                         gettext("Contribution:", domain = "R-explor"),
                                         "</strong> ", Contrib, "<br />")),
                           ifelse(is.na(Count), "",
                                  paste0("<strong>",
                                         gettext("Count:", domain = "R-explor"),
                                         "</strong> ", Count))),
           Lab = ifelse(Contrib >= as.numeric(var_lab_min_contrib) | 
                          is.na(Contrib) & as.numeric(var_lab_min_contrib) == 0, Level, ""))
  data.frame(tmp)
}


MCA_var_plot2 <- function(res, xax = 1, yax = 2, var_sup = TRUE, var_lab_min_contrib = 0,
                         point_size = 64,
                         col_var = NULL,
                         symbol_var = NULL,
                         size_var = NULL,
                         size_range = c(10,300),
                         zoom_callback = NULL,
                         in_explor = FALSE, ...) {
  
  ## Settings changed if not run in explor
  html_id <- if(in_explor) "explor_var" else  NULL
  dom_id_svg_export <- if(in_explor) "explor-var-svg-export" else NULL
  dom_id_lasso_toggle <- if(in_explor) "explor-var-lasso-toggle" else NULL
  lasso <- if(in_explor) TRUE else FALSE 
  lasso_callback <- if(in_explor) explor_multi_lasso_callback() else NULL
  zoom_callback <- if(in_explor) explor_multi_zoom_callback(type = "var") else NULL
  
  var_data <- MCA_var_data(res, xax, yax, var_sup, var_lab_min_contrib)
  
  scatterD3::scatterD3(
    x = var_data[, "Coord.x"],
    y = var_data[, "Coord.y"],
    xlab = names(res$axes)[res$axes == xax],
    ylab = names(res$axes)[res$axes == yax],
    lab = var_data[, "Lab"],
    point_size = point_size,
    point_opacity = 1,
    col_var = if (is.null(col_var)) NULL else var_data[,col_var],
    col_lab = col_var,
    symbol_var = if (is.null(symbol_var)) NULL else var_data[,symbol_var],
    symbol_lab = symbol_var,
    size_var = if (is.null(size_var)) NULL else var_data[,size_var],
    size_lab = size_var,
    size_range = if (is.null(size_var)) c(10,300) else c(30,400) * point_size / 32,
    tooltip_text = var_data[, "tooltip"],
    type_var = ifelse(var_data[,"Class"] == "Quantitative", "arrow", "point"),
    unit_circle = var_sup && "Quantitative" %in% var_data[,"Class"],
    key_var = paste(var_data[, "Variable"], var_data[, "Level"], sep = "-"),
    fixed = FALSE,
    html_id = html_id,
    dom_id_svg_export = dom_id_svg_export,
    dom_id_lasso_toggle = dom_id_lasso_toggle,
    lasso = lasso,
    lasso_callback = lasso_callback,
    zoom_callback = zoom_callback,
    ...
  )  
}
# Adaptation des graphiques des individus d'explor (Julien Barnier) :
MCA_ind_data <- function(res, xax = 1, yax = 2, ind_sup, col_var = NULL, 
                         ind_lab_min_contrib = 0,opacity_var = NULL) {
  tmp_x <- res$ind %>% 
    filter(Axis == xax) %>%
    select(Name, Type, Coord, Contrib, Cos2)
  if (!ind_sup)
    tmp_x <- tmp_x %>% filter(Type == "Active")
  tmp_y <- res$ind %>% 
    filter(Axis == yax) %>%
    select(Name, Type, Coord, Contrib, Cos2)
  if (!ind_sup)
    tmp_y <- tmp_y %>% filter(Type == "Active")
  tmp <- tmp_x %>%
    left_join(tmp_y, by = c("Name", "Type")) %>%
    mutate(Contrib = Contrib.x + Contrib.y,
           Cos2 = Cos2.x + Cos2.y,
           tooltip = paste(paste0("<strong>", Name, "</strong><br />"),
                           paste0("<strong>Axis ", xax," :</strong> ", Coord.x, "<br />"),
                           paste0("<strong>Axis ", yax," :</strong> ", Coord.y, "<br />"),
                           ifelse(is.na(Cos2), "",
                                  paste0("<strong>",
                                         gettext("Squared cosinus", domain = "R-explor"),
                                         ":</strong> ", Cos2, "<br />")),
                           ifelse(is.na(Contrib), "",
                                  paste0("<strong>",
                                         gettext("Contribution:", domain = "R-explor"),
                                         "</strong> ", Contrib, "<br />"))),
           Lab = ifelse(Contrib >= as.numeric(ind_lab_min_contrib) | 
                          (is.na(Contrib) & as.numeric(ind_lab_min_contrib) == 0), Name, ""))
  if (!(is.null(col_var) || col_var %in% c("None", "Type"))) {
    tmp_data <- res$quali_data %>% select_("Name", col_var)
    tmp <- tmp %>%
      left_join(tmp_data, by = "Name")
  }
  data.frame(tmp)
}

MCA_ind_plot <- function(res, xax = 1, yax = 2, ind_sup = TRUE,
                         col_var = NULL,
                         symbol_var = NULL,
                         opacity_var = NULL,
                         size_var = NULL,
                         size_range = c(10,300),
                         lab_var = NULL,
                         zoom_callback = NULL,
                         in_explor = FALSE,
                         ind_lab_min_contrib = 0,
                         ...) {
  
  html_id <- if(in_explor) "explor_ind" else  NULL
  dom_id_svg_export <- if(in_explor) "explor-ind-svg-export" else NULL
  dom_id_lasso_toggle <- if(in_explor) "explor-ind-lasso-toggle" else NULL
  lasso <- if(in_explor) TRUE else FALSE 
  lasso_callback <- if(in_explor) explor_multi_lasso_callback() else NULL
  zoom_callback <- if(in_explor) explor_multi_zoom_callback(type = "ind") else NULL
  
  ind_data <- MCA_ind_data(res, xax, yax, ind_sup, col_var,ind_lab_min_contrib)
  
  scatterD3::scatterD3(
    x = ind_data[, "Coord.x"],
    y = ind_data[, "Coord.y"],
    xlab = names(res$axes)[res$axes == xax],
    ylab = names(res$axes)[res$axes == yax],
    lab=ind_data[,"Lab"],
    col_var = if (is.null(col_var)) NULL else ind_data[,col_var],
    col_lab = col_var,
    opacity_var = if (is.null(opacity_var)) NULL else ind_data[,opacity_var],
    tooltip_text = ind_data[, "tooltip"],
    key_var = ind_data[, "Name"],
    fixed = TRUE,
    html_id = html_id,
    dom_id_svg_export = dom_id_svg_export,
    dom_id_lasso_toggle = dom_id_lasso_toggle,
    lasso = lasso,
    lasso_callback = lasso_callback,
    zoom_callback = zoom_callback,
    ...)
  
}

#\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
# Fichier server ------------
#\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

shinyServer(function(input, output, session) { 
  
  # A/ Mise en forme et modifications de la table brute importée --------------
  #\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
  
  # Données importées (adaptation d'explore-data, Paris Descartes) :
  output$donnees.fichier.ui <- renderUI({
    list(
      fileInput("donnees.fichier.input", "Choisir le fichier :"),
      radioButtons("donnees.fichier.header", 
                   "Noms de variables en 1ère ligne :",
                   c("oui", "non")),
      radioButtons("donnees.fichier.sep", 
                   "Séparateur de champs :", 
                   c("point-virgule" = ";", 
                     "virgule" = ",", 
                     "espace" = " ", 
                     "tabulation" = "\t")),
      radioButtons("donnees.fichier.dec", 
                   "Séparateur de décimales :",
                   c("point" = ".", "virgule" = ",")),
      radioButtons("donnees.fichier.enc",
                   "Encodage des caractères :",
                   c("UTF-8 (par défaut sur Linux/Mac)" = "UTF-8",
                     "Windows-1252 (par défaut sur Windows)" = "WINDOWS-1252")),
      uiOutput("donnees.fichier.ok")
    )
    
  })
  file_name <- reactive({
    inFile <- input$donnees.fichier.input
    if (is.null(inFile))
      return("NULL")
    return (stringi::stri_extract_first(str = inFile$name, regex = ".*(?=\\.)"))
  })
  donnees_entree <-reactive({
    if (is.null(input$donnees.fichier.input)) return (NULL)
    don <- NULL
    try({
      don <- read.table(
        input$donnees.fichier.input$datapath, 
        header = input$donnees.fichier.header == "oui", 
        sep = input$donnees.fichier.sep,
        dec = input$donnees.fichier.dec, 
        fileEncoding = input$donnees.fichier.enc,
        stringsAsFactors = FALSE)
    }, silent = TRUE)
    don <- unique(don)
    for (i in 1:ncol(don)){
      if (class(don[,i])!="numeric" &&class(don[,i])!="integer" )
   { don[,i][is.na(don[,i])]<-""}}
    
    don
  })
  
  
  donnees_entree2 <- reactive ({
    LabelGraphInd <- input$LabelGraphInd
    don <- donnees_entree()
    if (is.null(LabelGraphInd)) don 
    row.names(don)<- don[,LabelGraphInd]
    
    don
  })
  
  # setwd("C:/Users/Cecile Rodriguez/Documents/Recherche/TMI2")
  #  donnees_entree <- read.csv2("EtatCivil/BiosFinal.csv") 
  #  donnees_entree$X <- NULL
  #  donnees_entree <- unique (donnees_entree)
  #  names(donnees_entree)[5]<-"Prenom"
  #  row.names (donnees_entree)<- paste0(donnees_entree$Noms, ".",donnees_entree$Prénom)
  # donnees_entree
  
  # taille et str du tableau de départ :  
  
  output$Dimensions <- renderText(
    if (is.null(input$donnees.fichier.input)) return ("")
    else {
      paste("Tableau constitué de", ncol(donnees_entree()),
            "colonnes et de", nrow(donnees_entree()),"lignes.
            Détail des variables :")
      
    })
  
  output$Resume <- renderTable({
    
    if (is.null(input$donnees.fichier.input)) return (NULL)
    tmp<-donnees_entree()
    donnees_entree<- data.frame( Variable = names(tmp[1]),
                                 Type = class(tmp[,1]),
                                 NbreValeursDiff = nrow(unique(tmp[1])))
    for (i in (2:ncol(tmp))) {
      donnees_entree<-rbind(donnees_entree, data.frame( Variable = names(tmp[i]),
                                                        Type = class(tmp[,i]),
                                                        NbreValeursDiff = nrow(unique(tmp[i]))))
    }
    donnees_entree
  })
  
  
  # Listes déroulantes dynamiques :
  
  ## Modalités du 1er critère :
  Choose_Field <- reactive({
    Var1 <- input$Variable1
    donnees_entree <- donnees_entree()
    if (is.null(input$Variable1)) return (NULL)
    Choose_Field <- as.list(c("",unique(as.character(donnees_entree[,Var1]))))
  })
  
  ## Modalités du 2ème critère :
  Choose_Field2 <- reactive({
    if (is.null(input$Variable2)) return (NULL)
    donnees_entree <- donnees_entree()
    Var2 <- input$Variable2
    Choose_Field <- as.list(c("",unique(as.character(donnees_entree[,Var2]))))
  })
  
  ## Modalités du 3ème critère :
  Choose_Field3 <- reactive({
    donnees_entree <- donnees_entree()
    Var3 <- input$Variable3
    if (is.null(input$Variable3)) return (NULL)
    Choose_Field <- as.list(c("",unique(as.character(donnees_entree[,Var3]))))
  })
  
  ## Modalités du 4ème critère :
  Choose_Field4 <- reactive({
    donnees_entree <- donnees_entree()
    Var4 <- input$Variable4
    if (is.null(input$Variable4)) return (NULL)
    Choose_Field <- as.list(c("",unique(as.character(donnees_entree[,Var4]))))
  })
  
  ## Variables conservées dans l'ACM (pour var illustratives) :
  Choose_Illus <- reactive ({
    Vars <- input$VarPourACM
    donnees_entree <- test()
    validate(need(length(input$VarPourACM)>1, " "))
    if (is.null(input$VarPourACM)) return (NULL)
    Choose_Illus <- data.frame(donnees_entree[,Vars])
  })
  
  ## ACM spéciale : Index pour choix des modalités à exclure :
  Choose_Spe <- reactive ({
    TableACM <- TableACM()
    
    if (is.null(TableACM)) return (NULL)
    Choose_Spe <- GDAtools::getindexcat(TableACM)
  })
  
  output$Choose_ModaSpe <- renderUI({
    
    ACMSpe <- input$ACMSpe
    
  #  if (ACMSpe == TRUE ) {
  #  selectizeInput("ModaSpe", "Choix des modalités à exclure :",
 #               choices=c(" ",Choose_Spe()) , selected = NULL, multiple = TRUE)  
  #  }
  
  })
  
  ListeModaSpe <- reactive({
    ModaSpe <- input$ModaSpe
    Choose_Spe <- Choose_Spe()
    c<-which(as.list(Choose_Spe)== ModaSpe[1])
    if (length(ModaSpe)<2){
      c<-which(as.list(Choose_Spe)== ModaSpe[1])
    }else{
      c<-which(as.list(Choose_Spe)== ModaSpe[1])
      #if (length(VarsIllus>1))
      for (i in 2:length(ModaSpe)) {
        c <- c(c, which(as.list(Choose_Spe)== ModaSpe[i]))
      }
    }
    c
    
  })
  
  
  # Elements de sélection / choix / input -----
  #\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
  
  # Sélection des individus :
  
  ## Choix de l'identifiant (pour jointures des différents tableaux "critères"):
  output$SelectID <- renderUI({
    selectInput("ID", "Choix de l'identifiant (doit être unique) :",
                choices=c(" ",names(donnees_entree())) , selected = NULL)  
  }) 
  
  ## Message d'erreur identifiant :
  
  
  output$ErreurID <- renderUI({
    
    ID <- input$ID
    Donnees <- donnees_entree()
    
      validate(
       need(is.null(Donnees)==F , "Charger une table")
       )
      
    if (ID == " ") {return ("Sélectionner une variable")} else{
    if (length(unique(Donnees[,ID]))==nrow(Donnees)){"Identifiant OK"}else{
      p("Identifiant pas OK : en choisir un autre ou \n télécharger la table avec un ID :", style = "color:red")
  }
    }
    })
  
  # Choix de la variable donnant les noms des étiquettes "individus" :
  
  output$SelectLabelGraphInd <- renderUI({
    selectInput("LabelGraphInd", "Noms des individus (Même variable que l'identifiant, par défaut) :",
                choices=c(" ",names(donnees_entree())) , selected = input$ID)  
  }) 
  
  
  ## Message d'erreur étiquette graphe individu :
  
  
  output$ErreurLabelGraphInd <- renderUI({
    
    LabelGraphInd <- input$LabelGraphInd
    Donnees <- donnees_entree()
    
    validate(
      need(is.null(Donnees)==F , "Charger une table")
    )
    
    if (LabelGraphInd == " ") {return ("Sélectionner une variable")}else{
      if (length(unique(Donnees[,LabelGraphInd]))==nrow(Donnees)){"Label OK"}else{
        p("Label pas OK : en choisir un autre ou laisser l'identifant pas défaut", style = "color:red")
        }
    }
  })
  
  
  
  # Sélection des variables :
  
  ## Choix des variables pour l'ACM :    
  output$SelectACM <- renderUI({
    if (input$SelectAll == TRUE){
      selectizeInput("VarPourACM", "Variables pour l'ACM :", 
                     choices = names(donnees_entree()), 
                     selected = names(donnees_entree()), multiple = TRUE,
                     options = NULL)
    }
    else if (input$SelectAll == FALSE) {
      selectizeInput("VarPourACM", "Variables pour l'ACM :", 
                     choices = names(donnees_entree()), 
                     selected = NULL, multiple = TRUE,
                     options = NULL)
    }
  })
  
  ## Choix des variables illustratives de l'ACM :
  output$SelectIllus <- renderUI({
    
    ## Validations / erreurs
    validate(
      need(nrow(donnees_entree())!=0, "Charger une table (onglet 1)")
    )
    
    
    validate(
      need(input$ID!= " ", "Sélectionner un identifiant (onglet 1)")
    )
    
    
    
    ## Contenu
    
    selectizeInput("VarIllusPourACM", "Variables illustratives quali (parmi celles conservées pour l'ACM) :", 
                   choices = names(Choose_Illus()), 
                   selected = NULL, multiple = TRUE,
                   options = NULL)
  })
  
  
  output$SelectIllusQuanti <- renderUI({
    ## Validations / erreurs
    validate(
      need(nrow(donnees_entree())!=0, "Charger une table (onglet 1)")
    )
    
    
    validate(
      need(input$ID!= " ", "Sélectionner un identifiant (onglet 1)")
    )
    
    
    ## Contenu
    selectizeInput("VarIllusQuantiPourACM", "Variables illustratives quanti (parmi celles conservées pour l'ACM) :", 
                   choices = names(Choose_Illus()), 
                   selected = NULL, multiple = TRUE,
                   options = NULL)
  })
  
  
  ## Choix des variables illustratives QUANTI de l'ACM :
  

  
  # Sélection des variables, opérateurs et modalités de chaque critère :
  
  output$SelectVar1 <- renderUI ({
    selectInput("Variable1", "Variable :",
                choices=as.list(c(" ",names(donnees_entree()))),selected=" ")
  })

    output$Select <- renderUI({
    
    Variable1 <- input$Variable1
    Donnees <- donnees_entree()
    validate(
      need(input$Variable1 !=" "  , "Choisir une variable")
    )
    
    if (class(Donnees[,Variable1])=="character" |
        class(Donnees[,Variable1])=="logical"){
      selectInput("Modalite1", "Modalité :",
                  choices=Choose_Field() , selected = NULL)  
    }
    else if (class(Donnees[,Variable1])=="integer" |
             class(Donnees[,Variable1])=="numeric"){
      sliderInput("Modalite1", "Modalité :",
                  min=min(Donnees[,Variable1], na.rm=T),
                  max=max(Donnees[,Variable1], na.rm=T), round=1, step=.5, value=0)
    }
  })
  
  output$SelectVar2 <- renderUI ({
    
    selectInput("Variable2", "Variable :",
                choices=as.list(c(" ",names(donnees_entree()))),selected=" ")
  })
  
  output$Select2 <- renderUI({
    Variable2 <- input$Variable2
    Donnees <- donnees_entree()
    
    
    validate(
      need(input$Variable2 !=" "  , "Choisir une variable")
    )
    if (class(Donnees[,Variable2])=="character" |
        class(Donnees[,Variable2])=="logical"){
      selectInput("Modalite2", "Modalité :",
                  choices=Choose_Field2() , selected = NULL)  
    }
    else if (class(Donnees[,Variable2])=="integer" |
             class(Donnees[,Variable2])=="numeric"){
      sliderInput("Modalite2", "Modalité :",
                  min=min(Donnees[,Variable2], na.rm=T),
                  max=max(Donnees[,Variable2], na.rm=T), round=1, step=.5, value=0)
    }
  })
  
  output$SelectVar3 <- renderUI ({
    selectInput("Variable3", "Variable :",
                choices=as.list(c(" ",names(donnees_entree()))),selected=" ")
  })
  
  output$Select3 <- renderUI({
    
    validate(
      need(input$Variable3 !=" "  , "Choisir une variable")
    )
    Variable3 <- input$Variable3
    Donnees <- donnees_entree()
    if(is.null(input$Variable3)) return(NULL)
    if (class(Donnees[,Variable3])=="character" |
        class(Donnees[,Variable3])=="logical"){
      selectInput("Modalite3", "Modalité :",
                  choices=Choose_Field3() , selected = NULL)  
    }
    else if (class(Donnees[,Variable3])=="integer" |
             class(Donnees[,Variable3])=="numeric"){
      sliderInput("Modalite3", "Modalité :",
                  min=min(Donnees[,Variable3], na.rm=T),
                  max=max(Donnees[,Variable3], na.rm=T), round=1, step=.5, value=0)
    }
  })
  
  output$SelectVar4<- renderUI ({
    selectInput("Variable4", "Variable :",
                choices=as.list(c(" ",names(donnees_entree()))),selected=" ")
  })
  
  output$Select4 <- renderUI({
    
    Variable4 <- input$Variable4
    Donnees <- donnees_entree()
    
    validate(
      need(input$Variable4 !=" "  , "Choisir une variable")
    )
    
    
    if (class(Donnees[,Variable4])=="character" |
        class(Donnees[,Variable4])=="logical"){
      selectInput("Modalite4", "Modalité :",
                  choices=Choose_Field4() , selected = NULL)  
    }
    else if (class(Donnees[,Variable4])=="integer" |
             class(Donnees[,Variable4])=="numeric"){
      sliderInput("Modalite4", "Modalité :",
                  min=min(Donnees[,Variable4], na.rm=T),
                  max=max(Donnees[,Variable4], na.rm=T), round=1, step=.5, value=0)
    }
  })
  output$SelectNbreAxes <- renderUI({
    
    ACM <- ACM()
    Max <- nrow(ACM$eig)
    
    sliderInput("NbreAxes", "Nombre d'axes à afficher :", min=1, max = Max, value=ifelse(Max < 11, Max, 10), step=1)
  })
  
  # B/ Création de la table selon les sous-ensembles définis ----------------
  #\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
  
  
  Critere1 <- reactive({
    Var1 <- input$Variable1
    Moda1 <- input$Modalite1
    BiosFinal <- donnees_entree2()
    BiosFinal<-  switch(input$Operateur1,
                        " " = BiosFinal,
                        "=" =  BiosFinal[BiosFinal[,Var1] == Moda1,], 
                        "diff. de" =  BiosFinal[BiosFinal[,Var1] != Moda1,],
                        ">" = BiosFinal[BiosFinal[,Var1] > Moda1,],
                        ">=" = BiosFinal[BiosFinal[,Var1] >= Moda1,],
                        "<" = BiosFinal[BiosFinal[,Var1] < Moda1,],
                        "<=" = BiosFinal[BiosFinal[,Var1] <= Moda1,])
    BiosFinal <- BiosFinal[!(str_detect(row.names(BiosFinal),"NA")),] 
  })
  
  Critere2 <- reactive({
    Var2 <- input$Variable2
    Moda2 <- input$Modalite2
    BiosFinal <- donnees_entree2()
    BiosFinal <- switch(input$Operateur2,
                        " " = BiosFinal,
                        "=" =  BiosFinal[BiosFinal[,Var2] == Moda2,], 
                        "diff. de" =  BiosFinal[BiosFinal[,Var2] != Moda2,],
                        ">" = BiosFinal[BiosFinal[,Var2] > Moda2,],
                        ">=" = BiosFinal[BiosFinal[,Var2] >= Moda2,],
                        "<" = BiosFinal[BiosFinal[,Var2] < Moda2,],
                        "<=" = BiosFinal[BiosFinal[,Var2] <= Moda2,])
    BiosFinal <- BiosFinal[!(str_detect(row.names(BiosFinal),"NA")),] 
  })
  
  Critere3 <- reactive({
    Var3 <- input$Variable3
    Moda3 <- input$Modalite3
    BiosFinal <- donnees_entree2()
    BiosFinal<- switch(input$Operateur3,
                       " " = BiosFinal,
                       "=" =  BiosFinal[BiosFinal[,Var3] == Moda3,], 
                       "diff. de" =  BiosFinal[BiosFinal[,Var3] != Moda3,],
                       ">" = BiosFinal[BiosFinal[,Var3] > Moda3,],
                       ">=" = BiosFinal[BiosFinal[,Var3] >= Moda3,],
                       "<" = BiosFinal[BiosFinal[,Var3] < Moda3,],
                       "<=" = BiosFinal[BiosFinal[,Var3] <= Moda3,])
    BiosFinal <- BiosFinal[!(str_detect(row.names(BiosFinal),"NA")),] 
  })
  
  Critere4 <- reactive({
    Var4 <- input$Variable4
    Moda4 <- input$Modalite4
    BiosFinal <- donnees_entree2()
    BiosFinal<- switch(input$Operateur4,
                       " " = BiosFinal,
                       "=" =  BiosFinal[BiosFinal[,Var4] == Moda4,], 
                       "diff. de" =  BiosFinal[BiosFinal[,Var4] != Moda4,],
                       ">" = BiosFinal[BiosFinal[,Var4] > Moda4,],
                       ">=" = BiosFinal[BiosFinal[,Var4] >= Moda4,],
                       "<" = BiosFinal[BiosFinal[,Var4] < Moda4,],
                       "<=" = BiosFinal[BiosFinal[,Var4] <= Moda4,])
    BiosFinal <- BiosFinal[!(str_detect(row.names(BiosFinal),"NA")),] 
  })
  
  
  test <- reactive({
    Critere1 <- Critere1()
    Critere2 <- Critere2()
    ID <- input$ID
    Crit <- data.frame(Critere2[,ID]) 

    names(Crit)[1]<- ID
    don<-switch(input$OperateurMid,
                " " = Critere1,
                "OU" =  unique(rbind(Critere1, Critere2)), 
                "ET" =  {
                Critere1$tempProdACMVariableImprobable <- rownames(Critere1)
                testC <- merge(Critere1, Crit, by=ID)
                rownames(testC) <- testC$tempProdACMVariableImprobable
                testC$tempProdACMVariableImprobable <- NULL
                testC
                }
                  # unique(merge(Critere1, Crit, by=ID))
                )

  })
  test2 <- reactive({
    Critere3 <- Critere3()
    Critere2 <- test()
    ID <- input$ID
    Crit <- data.frame(Critere3[,ID])
    names(Crit)[1]<- ID
    switch(input$OperateurMid2,
           " " = Critere2,
           "OU" =  unique(rbind(Critere3, Critere2)), 
           "ET" =  {
             Critere2$tempProdACMVariableImprobable <- rownames(Critere2)
             testC <- merge(Critere2, Crit, by=ID)
             rownames(testC) <- testC$tempProdACMVariableImprobable
             testC$tempProdACMVariableImprobable <- NULL
             testC
           }
             #unique(merge(Critere2, Crit, by=ID))
           )
  })
  
  
  test3 <- reactive({
    Critere4 <- Critere4()
    Critere3 <- test2()
    ID <- input$ID
    
    Crit <- data.frame(Critere4[,ID])

    names(Crit)[1]<- ID
    switch(input$OperateurMid3,
           " " = Critere3,
           "OU" =  unique(rbind(Critere4, Critere3)), 
           "ET" =  {
             Critere3$tempProdACMVariableImprobable <- rownames(Critere3)
             testC <- merge(Critere3, Crit, by=ID)
             rownames(testC) <- testC$tempProdACMVariableImprobable
             testC$tempProdACMVariableImprobable <- NULL
             testC
           } 
           #  unique(merge(Critere3, Crit, by=ID))
           )


  })

  test4 <- reactive({
    test3 <- test3()
    VariableTri <- input$VariableTri
    
    test4 <- test3
    test4[,VariableTri] <- switch(input$ChangementVarTri,
                                  "Pas de modification" = test4[,VariableTri] ,
                                  "Qualitative" = as.character ( test4[,VariableTri]),
                                  "Quantitative" =  as.numeric (test4[,VariableTri]),
                                  "Logique" = as.logical (test4[,VariableTri]))
    test4 <- test4[!(str_detect(row.names(test4),"NA")),]
    
    
  })
  

# C/ Création de la table pour l'ACM selon les variables conservées dans le modèle ----
#\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
  
  
  # Définition des variables illustratives :
  
  VarsIllus <- reactive({
    VarsIllus <- input$VarIllusPourACM
    TableACM <- TableACM()
    c<-which(as.list(names(TableACM))== VarsIllus[1])
    if (length(VarsIllus)<2){
      c<-which(as.list(names(TableACM))== VarsIllus[1])
    }else{
      c<-which(as.list(names(TableACM))== VarsIllus[1])
      #if (length(VarsIllus>1))
      for (i in 2:length(VarsIllus)) {
        c <- c(c, which(as.list(names(TableACM))== VarsIllus[i]))
      }
    }
    c
  })
  
  
  VarsIllusQuanti <- reactive({
    VarsIllusQuanti <- input$VarIllusQuantiPourACM
    TableACM <- TableACM()
    c<-which(as.list(names(TableACM))== VarsIllusQuanti[1])
    if (length(VarsIllusQuanti)<2){
      c<-which(as.list(names(TableACM))== VarsIllusQuanti[1])
    }else{
      c<-which(as.list(names(TableACM))== VarsIllusQuanti[1])
      #if (length(VarsIllusQuanti>1))
      for (i in 2:length(VarsIllusQuanti)) {
        c <- c(c, which(as.list(names(TableACM))== VarsIllusQuanti[i]))
      }
    }
    c
  })
  
  
  
  Illus <- reactive({
    VarsIllus <- VarsIllus()
    if (length(VarsIllus)<1) {
      test <- ""
    }else{
      d <- paste("c(",VarsIllus[1], sep="")
      if (length(VarsIllus)<2){
        test <- paste(d, ")", sep="")
      }else{
        for (i in 2:length(VarsIllus)) {
          d <- paste(d,",",VarsIllus[i], sep="")
        }
        test <- paste(d, ")", sep="")
      }
    }
    Illus<-  test
    Illus
  })
  IllusQuanti <- reactive({
    VarsIllusQuanti <- VarsIllusQuanti()
    if (length(VarsIllusQuanti)<1) {
      test <- ""
    }else{
      d <- paste("c(",VarsIllusQuanti[1], sep="")
      if (length(VarsIllusQuanti)<2){
        test <- paste(d, ")", sep="")
        
      }else{
        for (i in 2:length(VarsIllusQuanti)) {
          d <- paste(d,",",VarsIllusQuanti[i], sep="")
        }
        test <- paste(d, ")", sep="")
      }
    }
    IllusQuanti<-  test
    IllusQuanti
  })
 
  
  
  # Selection de la variable pour tri à plat :
  
  output$SelectVarTri <- renderUI ({
    ID <- input$ID
    if (ID == " ") {return ("")} else {
    
    selectInput("VariableTri", "Variable :",
                choices=as.list(c(" ",names(test3()))),selected=" ")
    }
  })
  

  TypeVarTri <- reactive({
 
    Donnees <- test4()
    VariableTri <-  input$VariableTri
    class(Donnees[,VariableTri])
  })
  
  output$TypeVarTri <- renderText({
    
    
    ## Validations / erreurs 
    validate(
      need(nrow(donnees_entree())!=0, "")
    )
    validate(
      need(input$ID!= " ", "")
    )
    validate(
      need(input$VariableTri!= " ", "")
    )
    
    ## Contenu
    
      TypeVarTri()
    })
  
  
  output$TableVarTri <- renderTable({
    
   ## Validations / erreurs 
    validate(
      need(nrow(donnees_entree())!=0, "Charger une table (onglet 1)")
    )
    
    
    validate(
      need(input$ID!= " ", "Sélectionner un identifiant (onglet 1)")
    )
    
    
    validate(
      need(input$VariableTri!= " ", "Sélectionner une variable")
    )
    
    ## Données
    Donnees <- test4()
    VariableTri <-  input$VariableTri
    
    ## Contenu
    if (class(Donnees[,VariableTri])=="character" |
        class(Donnees[,VariableTri])=="logical"){
      t<- freq(Donnees[,VariableTri], sort="dec",total = T) 
      t$Modalites <- row.names(t)
      t <- t[,c(4,1:3)]
      t
    }
    else if (class(Donnees[,VariableTri])=="integer" |
             class(Donnees[,VariableTri])=="numeric"){

      t <- Donnees %>% 
        dplyr::summarise (Min = min(eval(parse(text=VariableTri)), na.rm= T), 
                          Quartile1 = quantile(eval(parse(text=VariableTri)), .25, na.rm = T),
                          Mediane = median(eval(parse(text=VariableTri)), na.rm = T),
                          Moyenne = round(mean(eval(parse(text=VariableTri)), na.rm = T),1), 
                          Quartile3 = quantile(eval(parse(text=VariableTri)), .75, na.rm = T),
                          Max = max(eval(parse(text=VariableTri)), na.rm = T), 
                          NbreNA = sum(is.na(eval(parse(text=VariableTri)))))
      t
    }
    
  })
  
  
  # Table pour l'ACM :
  
  TableACM <- reactive({
    validate(
      need(length(input$VarPourACM)>1, " ")
    )
    Vars <- input$VarPourACM
    VarQuanti <- input$VarIllusQuantiPourACM
    BiosFinal <- test3()
    TableACM <- data.frame(BiosFinal[,Vars])
    for (i in 1:ncol(TableACM)) {
      TableACM[,i] <- factor(TableACM[,i])
    }
    if (length(VarQuanti)==0) {
      TableACM <- TableACM
    } else{
      for (i in 1:length(VarQuanti)){
        TableACM[,unlist(VarQuanti)[i]] <- as.numeric(TableACM[,unlist(VarQuanti)[i]])
      }
    }
    TableACM
    
  })
  
  
  # D/ Résultat de l'ACM  ------------
  #\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
  
  ACM <- reactive({
    tmp<-TableACM()
    Illus <- Illus()
    IllusQuanti <- IllusQuanti()
    ListeModaSpe <- ListeModaSpe()
  #  ACMSpe <- input$ACMSpe
  #  if (ACMSpe == TRUE) {
  #    ACM <- GDAtools::speMCA(tmp)
  #  } else {
    
    ACM <- MCA(tmp, quali.sup =  eval(parse(text=Illus)), ncp=100,
               quanti.sup=eval(parse(text=IllusQuanti)), graph = F)
#    }
    ACM
    
  })
  
  
  # Préparation des données pour les représentations :
  res <- reactive({
    ACMSpe <- input$ACMSpe
    ACM <- ACM()
    
  #  if (ACMSpe == TRUE) {
  #   res <- explor::prepare_results.speMCA(ACM)
   # } else {
    res <- explor::prepare_results(ACM)
   # }
  })
  
  # Choix pour la classification ---------------
  
  output$NbAxes.Cl.Choix <- renderUI({
    Type.Cl <- input$Type.Cl
    ACM <- ACM()
    Max <- nrow(ACM$eig)
    if (Type.Cl == "Hiérarchique"){
    sliderInput("NbAxes.Cl", label = "1. Nombre d'axes à inclure (max = nbre d'axes total, limité à 100)", min = 1, 
                max = Max, value = ifelse(Max < 11, Max, 10), step=1)}
    
  })
  
  output$Metric.Cl.Choix <- renderUI({
    Type.Cl <- input$Type.Cl
    if (Type.Cl == "Hiérarchique"){
    selectInput("Metric.Cl", "2. Choix de la métrique",
                choices=as.list(c("euclidienne","manhattan")),
                selected="euclidienne")
    }
  })
  
  output$Agreg.Cl.Choix <- renderUI({
    Type.Cl <- input$Type.Cl
    if (Type.Cl == "Hiérarchique"){
    selectInput("Agreg.Cl", "3. Critère d'agrégation",
                choices=as.list(c("saut minimum","diamètre","moyennes","ward")),
                selected="ward")
    }
  })
  
  output$Part.H.Cl.Choix <- renderUI({
    Type.Cl <- input$Type.Cl
    if (Type.Cl == "Hiérarchique"){
      selectInput("Part.H.Cl", "4. Type de partition",
                  choices=as.list(c("ascendante","descendante")),
                  selected="ascendante")
    }
  })
  
  
  output$NbreCl.Cl.Choix <- renderUI({
    Type.Cl <- input$Type.Cl
    if (Type.Cl == "Hiérarchique"){
      
      numericInput("NbreClasses","5. Nombre classes résultantes",5)
    }    
    else if (Type.Cl == "Pas de classification"){
      
      numericInput("NbreClasses","5. Nombre classes résultantes",1)
    }
  })
  
 


  # Résultation de la classification ---------------

  
  # Arbre selon le nbre de classes :
  cahTree <- reactive({
    ACM <- ACM()
    NbAxes.Cl <- input$NbAxes.Cl
    NbreClasses <- input$NbreClasses
    Metric.Cl <- switch(input$Metric.Cl,
                        "euclidienne"="euclidean",
                        "manhattan"= "manhattan")
    Agreg.Cl <- switch(input$Agreg.Cl,
                       "saut minimum" = "single",
                       "diamètre"="complete",
                       "moyennes"="average",
                       "ward"="ward")
    agnes(ACM$ind$coord[,1:NbAxes.Cl], method = Agreg.Cl, metric=Metric.Cl) 
  })
  
  DataClust <- reactive({
    # On veut : Noms + Classes
    Donnees <- test3()
    cahTree <- cahTree()
    NbreClasses <- input$NbreClasses
    Agnes.Cl <- cutree(cahTree, k=NbreClasses)
   # Agnes.Cl.vect <- factor(Agnes.Cl,labels=paste('classe',1:NbreClasses, sep=' '))
    
    DataClust <- data.frame(Name=rownames(cahTree$data), clust= cutree(cahTree, k=NbreClasses), stringsAsFactors = F)
   # DataClust <- data.frame(Name=rownames(cahTree$data), clust=Agnes.Cl.vect, stringsAsFactors = F)
   
     DataClust$clust <- as.character(DataClust$clust)
    
    # DataClust <-data.frame(Agnes.Cl.vect)
    # rownames (DataClust) <- rownames(Donnees)
    # colnames(DataClust) <- "clust"
    # DataClust$Name <- rownames(DataClust)
    # 
    
    DataClust
    
    
  })
  
  # E/ EN SORTIE : Tables et graphiques -----------
  #\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
  
  
  # Table des types de variables conservées pour l'ACM :
  
  output$Type <- renderTable({
    tmp <- TableACM()
    
    VarQuanti <- input$VarIllusQuantiPourACM
    
    Var <- input$VarIllusPourACM
    
    validate(
      need(length(input$VarPourACM)>0 , "Choisir au moins 2 variables pour l'ACM")
    )
    
    
    Type <-  data.frame( Variable = names(tmp[1]),
                         Type = class(tmp[,1]))
    for (i in (2:ncol(tmp))) {
      Type<-rbind(Type, data.frame( Variable = names(tmp[i]),
                                    Type = class(tmp[,i])))
    }
    
    Type$StatutACM <- "Active"
    if (length(Var)==0) {Type <- Type}else{
      for (i in (1:nrow(Type))){
        for (j in (1:length(Var))) {
          if (Type$Variable[i]==unlist(Var)[j]){
            Type$StatutACM[i] <- "Supplémentaire"
          }}}
    }
    
    if (length(VarQuanti)==0) {Type <- Type }else{
      for (i in (1:nrow(Type))){
        for (j in (1:length(VarQuanti))) {
          if (Type$Variable[i]==unlist(VarQuanti)[j]){
            Type$StatutACM[i] <- "Supplémentaire"
          }}}
    }
    
    
    Type
  })

  #  Valeurs propres :
  
  output$ValeursPropres<-renderTable({
    NbreAxes <- input$NbreAxes
    ACM <- ACM()
    ACMSpe <- input$ACMSpe
    
    validate(
      need(length(input$VarPourACM)>1, "Choisir au moins 2 variables pour l'ACM")
    )

   # if (ACMSpe == TRUE) {
      
    #  ValeursPropres <- data.frame (var=round(ACM$eig[,"rate"],digits=1),
    #                                VarCumulee=round(ACM$eig[,"cum.rate"],digits=1))
    #  ValeursPropres$Axe <- row.names(ValeursPropres)
    #  ValeursPropres <- ValeursPropres[,c(3,1,2)]
    #  ValeursPropres <- ValeursPropres[1:NbreAxes,]
    #  ValeursPropres[,1] <- paste("dim",ValeursPropres[,1], " ")
    #  row.names(ValeursPropres) <- ValeursPropres[,1]
      
      
   # } else {
    ValeursPropres <- data.frame (Axe= row.names(ACM$eig),
                                  var=round(ACM$eig[,"percentage of variance"],digits=1),
                                  VarCumulee=round(ACM$eig[,"cumulative percentage of variance"],digits=1))
    ValeursPropres <- ValeursPropres[1:NbreAxes,]
  #  }
  })
  
  # Graphes des valeurs propres :
  
  output$Variance <- renderPlot({
    ACM <- ACM()
    
    NbreAxes <- input$NbreAxes
    validate(
      need(length(input$VarPourACM)>1, "Choisir au moins 2 variables pour l'ACM")
    )
    
    Val10 <- data.frame (Axe= row.names(ACM$eig),
                                  var=round(ACM$eig[,"percentage of variance"],digits=1),
                                  VarCumulee=round(ACM$eig[,"cumulative percentage of variance"],digits=1))
    Val10<- Val10[1:NbreAxes,]
    
    Val10 <- arrange (Val10,  desc(var))
    Val10$Axe <- factor(Val10$Axe,
                        levels = Val10$Axe[order(Val10$VarCumulee)])
    ggplot(Val10, aes(x=Axe, y=var))+geom_bar(stat="identity")+
      ggtitle("Variance")+labs(y="Variance (%)")
  })
  
  output$VarianceCum <- renderPlot({
    
    validate(
      need(length(input$VarPourACM)>1, "Choisir au moins 2 variables pour l'ACM")
    )
    
    ACM <- ACM()
    
    NbreAxes <- input$NbreAxes
    Val10 <- data.frame (Axe= row.names(ACM$eig),
                         var=round(ACM$eig[,"percentage of variance"],digits=1),
                         VarCumulee=round(ACM$eig[,"cumulative percentage of variance"],digits=1))
    Val10<- Val10[1:NbreAxes,]
    
    Val10 <- arrange (Val10,  desc(var))
    Val10$Axe <- factor(Val10$Axe,
                        levels = Val10$Axe[order(Val10$VarCumulee)])
    ggplot(Val10, aes(x=Axe, y=var))+geom_bar(stat="identity")+
      ggtitle("Variance")+labs(y="Variance (%)")
    ggplot(Val10, aes(x=Axe, y=VarCumulee))+geom_bar(stat="identity")+
      ggtitle("Variance cumulée")+labs(y="Variance cumulée (%)")
  })
  
  # Graphiques des variables : 
  
  output$GraphVarAxeA <- renderScatterD3({
    
    validate(
      need(length(input$VarPourACM)>1, "Choisir au moins 2 variables pour l'ACM")
    )
    res <-res()
    ACM <- ACM()
    VarAxeA <- input$VarAxeA
    Graphiques1 <- explor::prepare_results(ACM)
    MaxCoord1 <- round(max(ACM$var$coord[,VarAxeA]), digits=2)
    MinCoord1 <- round(min(ACM$var$coord[,VarAxeA]), digits=2)
    vars <-Graphiques1$vars[Graphiques1$vars[,"Axis"]==VarAxeA,] 
    ind <-Graphiques1$ind[Graphiques1$ind[,"Axis"]==VarAxeA,] 
    eig <- Graphiques1$eig[Graphiques1$eig[,"dim"]==VarAxeA,]
    vareta2<-Graphiques1$vareta2
    quali_data <- Graphiques1$quali_data
    
    
  Graphiques1 <- list("vars"=vars, "ind"=ind, "eig"=eig,"axes"=as.integer(1), 
                      "vareta2"=vareta2,"quali_data"=quali_data)
  
  Graphiques1$vars <- Graphiques1$vars[Graphiques1$vars$Type == "Active",]
  
  Graphiques1$vars <- dplyr::arrange (Graphiques1$vars, Variable)
  
  scatterD3::scatterD3(data = Graphiques1$vars, x = Coord, y = Axis, fixed = F,
                       ylim = c(VarAxeA-.1,VarAxeA+.1) ,
                       xlim = c(MinCoord1-.5, MaxCoord1+.5),
                       col_var =  Variable, legend_width = F)
    })
 
  
  
  output$GraphVarAxeB <- renderScatterD3({
    
    validate(
      need(length(input$VarPourACM)>1, "Choisir au moins 2 variables pour l'ACM")
    )
    res <-res()
    ACM <- ACM()
    Graphiques1 <- explor::prepare_results(ACM)
    VarAxeB <- input$VarAxeB
    MaxCoord1 <- round(max(ACM$var$coord[,VarAxeB]), digits=2)
    MinCoord1 <- round(min(ACM$var$coord[,VarAxeB]), digits=2)
    vars <-Graphiques1$vars[Graphiques1$vars[,"Axis"]==VarAxeB,] 
    ind <-Graphiques1$ind[Graphiques1$ind[,"Axis"]==VarAxeB,] 
    eig <- Graphiques1$eig[Graphiques1$eig[,"dim"]==VarAxeB,]
    vareta2<-Graphiques1$vareta2
    quali_data <- Graphiques1$quali_data
    
    
    Graphiques1 <- list("vars"=vars, "ind"=ind, "eig"=eig,"axes"=as.integer(1), 
                        "vareta2"=vareta2,"quali_data"=quali_data)
    
    Graphiques1$vars <- Graphiques1$vars[Graphiques1$vars$Type == "Active",]
    Graphiques1$vars <- dplyr::arrange (Graphiques1$vars, Variable)
    
    scatterD3::scatterD3(data = Graphiques1$vars, x = Axis, y = Coord, fixed = F,
                         ylim = c(MinCoord1-.5, MaxCoord1+.5),
                         xlim = c(VarAxeB-.1, VarAxeB+.1) ,
                         col_var =  Variable, legend_width = F)
  })
  
  
  output$GraphVar <- renderScatterD3({
    
    validate(
      need(length(input$VarPourACM)>1, "Choisir au moins 2 variables pour l'ACM")
    )
    
    res <-res()
    ACM <- ACM()
    MinContribVar1 <- input$MinContribVar1
    VarAxeA <- input$VarAxeA
    VarAxeB <- input$VarAxeB
    MaxCoord1 <- round(max(ACM$var$coord[,VarAxeA]), digits=2)
    MinCoord1 <- round(min(ACM$var$coord[,VarAxeA]), digits=2)
    MaxCoord2 <- round(max(ACM$var$coord[,VarAxeB]), digits=2)
    MinCoord2 <- round(min(ACM$var$coord[,VarAxeB]), digits=2)
    GraphVar<- explor::MCA_var_plot(res, xax = VarAxeA, yax = VarAxeB,
                                    var_sup = TRUE, var_lab_min_contrib = MinContribVar1,
                                    col_var = "Variable", symbol_var = "Type",
                                    size_var = "Contrib", size_range = c(52.5, 700),
                                    labels_size = 12, point_size = 56,
                                    transitions = TRUE, labels_positions = NULL,
                                    xlim = c(MinCoord1-.5, MaxCoord1+.5),
                                    ylim = c(MinCoord2-.5, MaxCoord2+.5)) 
  })
  
  
  
  output$GraphVarAxeC <- renderScatterD3({
    
    validate(
      need(length(input$VarPourACM)>1, "Choisir au moins 2 variables pour l'ACM")
    )
    res <-res()
    ACM <- ACM()
    VarAxeC <- input$VarAxeC
    Graphiques1 <- explor::prepare_results(ACM)
    MaxCoord1 <- round(max(ACM$var$coord[,VarAxeC]), digits=2)
    MinCoord1 <- round(min(ACM$var$coord[,VarAxeC]), digits=2)
    vars <-Graphiques1$vars[Graphiques1$vars[,"Axis"]==VarAxeC,] 
    ind <-Graphiques1$ind[Graphiques1$ind[,"Axis"]==VarAxeC,] 
    eig <- Graphiques1$eig[Graphiques1$eig[,"dim"]==VarAxeC,]
    vareta2<-Graphiques1$vareta2
    quali_data <- Graphiques1$quali_data
    
    
    Graphiques1 <- list("vars"=vars, "ind"=ind, "eig"=eig,"axes"=as.integer(1), 
                        "vareta2"=vareta2,"quali_data"=quali_data)
    
    Graphiques1$vars <- Graphiques1$vars[Graphiques1$vars$Type == "Active",]
    
    Graphiques1$vars <- dplyr::arrange (Graphiques1$vars, Variable)
    
    scatterD3::scatterD3(data = Graphiques1$vars, x = Coord, y = Axis, fixed = F,
                         ylim = c(VarAxeC-.1,VarAxeC+.1) ,
                         xlim = c(MinCoord1-.5, MaxCoord1+.5),
                         col_var =  Variable, legend_width = F)
  })
  
  
  
  output$GraphVarAxeD <- renderScatterD3({
    
    validate(
      need(length(input$VarPourACM)>1, "Choisir au moins 2 variables pour l'ACM")
    )
    res <-res()
    ACM <- ACM()
    Graphiques1 <- explor::prepare_results(ACM)
    VarAxeD <- input$VarAxeD
    MaxCoord1 <- round(max(ACM$var$coord[,VarAxeD]), digits=2)
    MinCoord1 <- round(min(ACM$var$coord[,VarAxeD]), digits=2)
    vars <-Graphiques1$vars[Graphiques1$vars[,"Axis"]==VarAxeD,] 
    ind <-Graphiques1$ind[Graphiques1$ind[,"Axis"]==VarAxeD,] 
    eig <- Graphiques1$eig[Graphiques1$eig[,"dim"]==VarAxeD,]
    vareta2<-Graphiques1$vareta2
    quali_data <- Graphiques1$quali_data
    
    
    Graphiques1 <- list("vars"=vars, "ind"=ind, "eig"=eig,"axes"=as.integer(1), 
                        "vareta2"=vareta2,"quali_data"=quali_data)
    
    Graphiques1$vars <- Graphiques1$vars[Graphiques1$vars$Type == "Active",]
    Graphiques1$vars <- dplyr::arrange (Graphiques1$vars, Variable)
    
    scatterD3::scatterD3(data = Graphiques1$vars, x = Axis, y = Coord, fixed = F,
                         ylim = c(MinCoord1-.5, MaxCoord1+.5),
                         xlim = c(VarAxeD-.1, VarAxeD+.1) ,
                         col_var =  Variable, legend_width = F)
  })
  
  
  
  output$GraphVar2 <- renderScatterD3({
    
    validate(
      need(length(input$VarPourACM)>1, "Choisir au moins 2 variables pour l'ACM")
    )
    
    res <-res()
    ACM <- ACM()
    MinContribVar2 <- input$MinContribVar2
    VarAxeC <- input$VarAxeC
    VarAxeD <- input$VarAxeD
    MaxCoord3 <- round(max(ACM$var$coord[,VarAxeC]), digits=2)
    MinCoord3 <- round(min(ACM$var$coord[,VarAxeC]), digits=2)
    MaxCoord4 <- round(max(ACM$var$coord[,VarAxeD]), digits=2)
    MinCoord4 <- round(min(ACM$var$coord[,VarAxeD]), digits=2)
    GraphVar<- explor::MCA_var_plot(res, xax = VarAxeC, yax = VarAxeD,
                                    var_sup = TRUE, var_lab_min_contrib = MinContribVar2,
                                    col_var = "Variable", symbol_var = "Type",
                                    size_var = "Contrib", size_range = c(52.5, 700),
                                    labels_size = 12, point_size = 56,
                                    transitions = TRUE, labels_positions = NULL,
                                    xlim = c(MinCoord3-.5, MaxCoord3+.5),
                                    ylim = c(MinCoord4-.5, MaxCoord4+.5)) 
    GraphVar
  })
  
  # Tables des variables :
  
  TableVar <- reactive({
    
    validate(
      need(length(input$VarPourACM)>1, "Choisir au moins 2 variables pour l'ACM")
    )
    
    ACM <- ACM()
    AxeVar1 <- input$AxeVar1
    ContribVar1 <- input$ContribVar1
    Contrib <- data.frame ( Var=row.names(ACM$var$contrib),
                            Contrib=round(ACM$var$contrib[,AxeVar1],digits=1),
                            Coord=round(ACM$var$coord[,AxeVar1],digits=1),
                            Cos2=round(ACM$var$cos2[,AxeVar1],digits=1))
    Contrib <- arrange(Contrib, desc(Contrib))
    Contrib[(Contrib$Contrib>ContribVar1)  ,]
  }
  
  )
  output$TableVar <- renderDataTable({
    
    validate(
      need(length(input$VarPourACM)>1, "")
    )
    
    TableVar <- TableVar()
    datatable(TableVar,options = list(pageLength = 20))  
  })
  TableVar2 <- reactive({
    
    validate(
      need(length(input$VarPourACM)>1, "Choisir au moins 2 variables pour l'ACM")
    )
    
    ACM <- ACM()
    AxeVar2 <- input$AxeVar2
    ContribVar2 <- input$ContribVar2
    Contrib <- data.frame ( Var=row.names(ACM$var$contrib),
                            Contrib=round(ACM$var$contrib[,AxeVar2],digits=1),
                            Coord=round(ACM$var$coord[,AxeVar2],digits=1),
                            Cos2=round(ACM$var$cos2[,AxeVar2],digits=1))
    Contrib <- arrange(Contrib, desc(Contrib))
    Contrib[(Contrib$Contrib>ContribVar2)  ,]
  })
  output$TableVar2 <- renderDataTable({
    
    validate(
      need(length(input$VarPourACM)>1, "")
    )
    
    TableVar2 <- TableVar2()
    datatable(TableVar2,options = list(pageLength = 20))
    
  })  
  
  # Graphiques des individus
  
  output$GraphInd <- renderScatterD3({
    
    validate(
      need(length(input$VarPourACM)>1, "Choisir au moins 2 variables pour l'ACM")
    )
    
    res <-res()
 #   cah <- cah()
    ACM <- ACM()
    IndAxe1 <- input$IndAxe1
    IndAxe2 <- input$IndAxe2
    MaxCoord1 <- max(ACM$ind$coord[,IndAxe1])
    MinCoord1 <- min(ACM$ind$coord[,IndAxe1])
    MaxCoord2 <- max(ACM$ind$coord[,IndAxe2])
    MinCoord2 <- min(ACM$ind$coord[,IndAxe2])
    MinContrib1 <- input$MinContrib1
    NbreClasses <- input$NbreClasses
    Ellipse <- input$Ellipse
    VarClassesGraphe <- input$VarClassesGraphe
    ID <- input$ID
    TableACM <- test3()
    data.clust <- DataClust()
    
    if (VarClassesGraphe!=" "){
      
      TableACM$Name <- rownames(TableACM)
      TableACM$Name <- as.character(TableACM$Name)
      res$quali_data <- merge( res$quali_data, TableACM[,c("Name",VarClassesGraphe)], by="Name", all.x=T)
      names(res$quali_data) <- gsub("\\.y", "", names(res$quali_data))
      GraphInd <- MCA_ind_plot(res, xax = IndAxe1, yax = IndAxe2,ind_sup = FALSE, ind_lab_min_contrib = MinContrib1,
                               col_var = VarClassesGraphe, lab_var = "Name", labels_size = 12,
                               point_opacity = 0.5, opacity_var = "Contrib", point_size = 64,
                               ellipses = Ellipse, transitions = TRUE, labels_positions = NULL,
                               xlim = c(MinCoord1-.25, MaxCoord1+.25),
                               ylim = c(MinCoord2-.25, MaxCoord2+.25))
      
    } else {
      if (NbreClasses==1){
        GraphInd <- MCA_ind_plot(res, xax = IndAxe1, yax = IndAxe2,ind_sup = FALSE, ind_lab_min_contrib = MinContrib1,
                                 lab_var = "Name", labels_size = 12,
                                 point_opacity = 0.5, opacity_var = "Contrib", point_size = 64,
                                 ellipses = Ellipse, transitions = TRUE, labels_positions = NULL,
                                 xlim = c(MinCoord1-.25, MaxCoord1+.25),
                                 ylim = c(MinCoord2-.25, MaxCoord2+.25))
      } else {
        res$quali_data <- merge(res$quali_data, data.clust[,c("Name","clust")], by="Name", all.x=T)
        
        GraphInd <- MCA_ind_plot(res, xax = IndAxe1, yax = IndAxe2,ind_sup = FALSE, ind_lab_min_contrib = MinContrib1,
                                 col_var = "clust", lab_var = "Name", labels_size = 12,
                                 point_opacity = 0.5, opacity_var = "Contrib", point_size = 64,
                                 ellipses = Ellipse, transitions = TRUE, labels_positions = NULL,
                                 xlim = c(MinCoord1-.25, MaxCoord1+.25),
                                 ylim = c(MinCoord2-.25, MaxCoord2+.25))
      }
    }
        
    
    GraphInd
  })
  
  
  output$GraphInd2 <- renderScatterD3({
    
    validate(
      need(length(input$VarPourACM)>1, "Choisir au moins 2 variables pour l'ACM")
    )
    
    res <-res()
  #  cah <- cah()
    MinContrib2 <- input$MinContrib2
    
    ACM <- ACM()
    IndAxe3 <- input$IndAxe3
    IndAxe4 <- input$IndAxe4
    MaxCoord3 <- max(ACM$ind$coord[,IndAxe3])
    MinCoord3 <- min(ACM$ind$coord[,IndAxe3])
    MaxCoord4 <- max(ACM$ind$coord[,IndAxe4])
    MinCoord4 <- min(ACM$ind$coord[,IndAxe4])
    NbreClasses <- input$NbreClasses
    Ellipse2 <- input$Ellipse2
    VarClassesGraphe <- input$VarClassesGraphe
    ID <- input$ID
    TableACM <- test3()
    data.clust <- DataClust()
    
    if (VarClassesGraphe!=" "){
      
      TableACM$Name <- rownames(TableACM)
      TableACM$Name <- as.character(TableACM$Name)
      res$quali_data <- merge( res$quali_data, TableACM[,c("Name",VarClassesGraphe)], by="Name", all.x=T)
      names(res$quali_data) <- gsub("\\.y", "", names(res$quali_data))
      GraphInd2 <- MCA_ind_plot(res, xax = IndAxe3, yax = IndAxe4,ind_sup = FALSE, ind_lab_min_contrib = MinContrib2,
                               col_var = VarClassesGraphe, lab_var = "Name", labels_size = 12,
                               point_opacity = 0.5, opacity_var = "Contrib", point_size = 64,
                               ellipses = Ellipse2, transitions = TRUE, labels_positions = NULL,
                               xlim = c(MinCoord3-.25, MaxCoord3+.25),
                               ylim = c(MinCoord4-.25, MaxCoord4+.25))
      
    } else {
      if (NbreClasses==1){
        GraphInd2 <- MCA_ind_plot(res, xax = IndAxe3, yax = IndAxe4,ind_sup = FALSE, ind_lab_min_contrib = MinContrib2,
                                 lab_var = "Name", labels_size = 12,
                                 point_opacity = 0.5, opacity_var = "Contrib", point_size = 64,
                                 ellipses = Ellipse2, transitions = TRUE, labels_positions = NULL,
                                 xlim = c(MinCoord3-.25, MaxCoord3+.25),
                                 ylim = c(MinCoord4-.25, MaxCoord4+.25))
      } else {
        res$quali_data <- merge(res$quali_data, data.clust[,c("Name","clust")], by="Name", all.x=T)
        
        GraphInd2 <- MCA_ind_plot(res, xax = IndAxe3, yax = IndAxe4,ind_sup = FALSE, ind_lab_min_contrib = MinContrib2,
                                 col_var = "clust", lab_var = "Name", labels_size = 12,
                                 point_opacity = 0.5, opacity_var = "Contrib", point_size = 64,
                                 ellipses = Ellipse2, transitions = TRUE, labels_positions = NULL,
                                 xlim = c(MinCoord3-.25, MaxCoord3+.25),
                                 ylim = c(MinCoord4-.25, MaxCoord4+.25))
      }
    }
    
    
    GraphInd2
  })
  
  
  # Tables des individus :
  
  TableInd <- reactive ({
    ACM <- ACM()
    AxeInd1 <- input$AxeInd1
    ContribInd1 <- input$ContribInd1
    
    Contrib <- data.frame ( Ind=row.names(ACM$ind$contrib),
                            Contrib=round(ACM$ind$contrib[,AxeInd1],digits=1),
                            Coord=round(ACM$ind$coord[,AxeInd1],digits=1),
                            Cos2=round(ACM$ind$cos2[,AxeInd1],digits=1))
    Contrib <- arrange(Contrib, desc(Contrib))
    Contrib[(Contrib$Contrib>ContribInd1)  ,]
  })
  output$TableInd <- renderDataTable({
    TableInd <- TableInd()
    datatable(TableInd,options = list(pageLength = 20))
    
  })
  
  TableInd2 <- reactive ({
    ACM <- ACM()
    AxeInd2 <- input$AxeInd2
    ContribInd2 <- input$ContribInd2
    Contrib <- data.frame ( Ind=row.names(ACM$ind$contrib),
                            Contrib=round(ACM$ind$contrib[,AxeInd2],digits=1),
                            Coord=round(ACM$ind$coord[,AxeInd2],digits=1),
                            Cos2=round(ACM$ind$cos2[,AxeInd2],digits=1))
    Contrib <- arrange(Contrib, desc(Contrib))
    Contrib[(Contrib$Contrib>ContribInd2)  ,]
  })
  output$TableInd2 <- renderDataTable({
    TableInd2 <- TableInd2()
    datatable(TableInd2,options = list(pageLength = 20))
    
  })  
  
  #  Classification
  
  output$plot <- renderPlot({
    cah <- cahTree()
    NbreClasses <- input$NbreClasses
    A2Rplot(cah,k=NbreClasses, col.up = "gray50", 
            col.down = brewer.pal(NbreClasses, "Dark2"), show.labels = FALSE,boxes = FALSE)
  })
  
  output$plot3 <- renderPlot({
    cah <- cahTree()
    NbreClasses <- input$NbreClasses
    Max <-  ifelse(NbreClasses >5, NbreClasses*2, 10)
    tri <- data.frame(var=sort(cah$height[1:Max], decreasing = TRUE),
                      axe=seq(1,Max,1))
    tri$col <- ifelse(tri$axe <= NbreClasses, "1","2")
    tri$axe<-factor(tri$axe)
    
    ggplot(data=tri, aes(x=axe, y=var, fill=col))+geom_bar(stat="identity",
                                                           colour="black")+
      labs(x="Nombre de classes", y="Inertie") +
      ggtitle("")+
      scale_fill_manual(values=c('brown4','grey'),
                        guide=FALSE)+
      theme_light()+theme(axis.text.x = element_text(angle=45, hjust=1, vjust=1,size=10),
                          axis.title=element_text(size=12,face="bold"),
                          axis.title.y=element_blank(),
                          axis.text.y=element_blank(),
                          legend.text=element_text(size=10),
                          title = element_text(size=12, face="bold"),
                          plot.title=element_text(hjust = 0.5))+theme(legend.position="none")
    
    
    
  })
  
  # Table des individus selon les groupes générés par la classifications

  IndEtClasses <- reactive({
   # cah <- cah()
   # cah$data.clust$Name <- row.names(cah$data.clust)
    data.clust <- DataClust()
    IndEtClasses <- data.frame (Noms = data.clust$Name ,
                                Classes = data.clust$clust)
  })
  output$IndEtClasses <- renderDataTable({
    
    validate(
      need(nrow(donnees_entree())!=0, "")
    )
    validate(
      need(input$ID!= " ", "")
    )
    IndEtClasses <- IndEtClasses()
    
  })
  output$TableEffClasses <- renderTable({
    
    validate(
      need(nrow(donnees_entree())!=0, "")
    )
    validate(
      need(input$ID!= " ", "")
    )
    
    IndEtClasses <- IndEtClasses()
    t<- freq(IndEtClasses$Classes, total= T, cum = F)
    t$Classes <- row.names(t)
    t <- t[,c(4,1:2)]
    names(t) <- c("Classes","Effectifs","%")
    t
  })
  
  # Selection de la variable pour croisé avec les classes :
  
  output$SelectVarClasses <- renderUI ({
    
    ## Validations / erreurs :
    validate(
      need(nrow(donnees_entree())!=0, "")
    )
    
    
    validate(
      need(input$ID!= " ", "")
    )
    
    
    
    ## Contenu :
    
    selectInput("VariableClasses", "Variable :",
                choices=as.list(c(" ",names(test3()))),selected=" ")
  })
  
  
  
  # Croisement classe et autre variable :

  IndEtTableDep <-  reactive ({
    
    ## Validations / erreurs :
    validate(
      need(nrow(donnees_entree())!=0, "")
    )
    
    
    validate(
      need(input$ID!= " ", "")
    )
    
    ## Contenu
    TableDep <- test3()
    IndEtClasses <- IndEtClasses()
    ID <- input$LabelGraphInd
    
    IndEtTableDep <- merge (TableDep, IndEtClasses,
                            by.x = ID, by.y = "Noms",
                            all.x= T)
  })  
  
  TableVarClasses <- reactive({
    
    
    IndEtTableDep <- IndEtTableDep()
    
    VariableClasses <-  input$VariableClasses
    validate(
      need(input$VariableClasses!=" " , "Choisir une variable")
    )
    
    if (class(IndEtTableDep[,VariableClasses])=="character" |
        class(IndEtTableDep[,VariableClasses])=="logical"){
      t<-data.frame(cprop(table(IndEtTableDep[,VariableClasses], 
                                IndEtTableDep [,"Classes"])))
      t$Freq <- round(t$Freq, 2)
      t <- spread(t, "Var2","Freq")
      names(t)[1] <- VariableClasses
      names(t)[2:(ncol(t)-1)] <- paste0 ("Classe ",names(t)[2:(ncol(t)-1)])
      t
    }
    else if (class(IndEtTableDep[,VariableClasses])=="integer" |
             class(IndEtTableDep[,VariableClasses])=="numeric"){
      
      
      t <- IndEtTableDep %>% 
        group_by(Classes) %>% 
        dplyr::summarise (Min = min(eval(parse(text=VariableClasses)), na.rm= T), 
                          Quartile1 = quantile(eval(parse(text=VariableClasses)), .25, na.rm = T),
                   Mediane = round(median(eval(parse(text=VariableClasses)), na.rm = T),0),
                   Moyenne = round(mean(eval(parse(text=VariableClasses)), na.rm = T),1), 
                   Quartile3 = quantile(eval(parse(text=VariableClasses)), .75, na.rm = T),
                   Max = max(eval(parse(text=VariableClasses)), na.rm = T), 
                   NbreNA = sum(is.na(eval(parse(text=VariableClasses)))))
      t
    }
    
  })
  
  
  # Selection de la variable pour représentation des individus dans les graphiques :
  
  output$SelectVarClassesGraphe <- renderUI ({
    
    ## Validations / erreurs :
    validate(
      need(nrow(donnees_entree())!=0, "")
    )
    
    validate(
      need(input$ID!= " ", "")
    )
    
    selectInput("VarClassesGraphe", "Variables en couleurs (classes, par défaut) :",
                choices=as.list(c(" ",names(test3()))),selected=" ")
  })
  
  
  
  output$TableVarClasses <- renderTable({
    TableVarClasses <- TableVarClasses ()
  })

  # output$plot2 <- renderPlot({
  #  cah <- cah()
  #  NbreClasses <- input$NbreClasses
  #  plot(cah, choice = "tree")
  # })
  
  # output$plot4<-renderPlot({
  #    cah <- cah()
  #  plot(cah,choice='3D.map')
  # })
  
  #output$graph3<-renderPlot({
  #  tmp<-HCPCInput()
  #  plot.HCPC(tmp,choice='tree',title='Arbre hiérarchique') 
  #})
  
  # output$bilan_HCPC <- renderTable({
  #    tmp<-HCPCInput()
  #    summary(tmp)
  #  })
  
  # F/ Boutons et téléchargements ------------
  #\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
  
  # A REVOIR / FAIRE - NON UTILISE
  #\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\====
  
  output$DLMetaDonnees <- downloadHandler( 
    
    #write.csv2(IndEtClasses, "IndividusEtClasses", na="", row.names = F)
    filename=function() {
      paste0("MetaDonneesACM_",Sys.Date(),".csv")
    },
    content = function(file) {
      TableMeta <- TableMeta()
      write.csv2(TableMeta, file, fileEncoding = "UTF-8", na = "", row.names = FALSE )
    }
  )
  output$TableIndividusClasses <- downloadHandler( 
    
    #write.csv2(IndEtClasses, "IndividusEtClasses", na="", row.names = F)
    filename=function() {
      paste0("IndividusEtClasses",".csv")
    },
    content = function(file) {
      IndEtClasses <- IndEtClasses()
      write.csv2(IndEtClasses, file, fileEncoding = "UTF-8", na = "", row.names = FALSE )
    }
  )
  
  
  output$TableDepClasses <- downloadHandler( 
    
    filename=function() {
      paste0("TableDepartEtClasses",".csv")
    },
    content = function(file) {
      IndEtTableDep <- IndEtTableDep()
      write.csv2(IndEtTableDep, file, fileEncoding = "UTF-8", na = "", row.names = FALSE )
    }
  )
  
  output$DlTableVar1 <- downloadHandler( 
    filename=function() {
      paste0("TableVariables",".csv")
    },
    content = function(file) {
      
      TableVar <- TableVar()
      write.csv2(TableVar, file, fileEncoding = "UTF-8", na = "", row.names = FALSE )
    }
  )
  
  output$DlTableVar2 <- downloadHandler( 
    filename=function() {
      paste0("TableVariables2",".csv")
    },
    content = function(file) {
      
      TableVar2 <- TableVar2()
      write.csv2(TableVar2, file, fileEncoding = "UTF-8", na = "", row.names = FALSE )
    }
  )
  
  output$DlTableInd1 <- downloadHandler( 
    filename=function() {
      paste0("TableIndividus",".csv")
    },
    content = function(file) {
      
      TableInd <- TableInd()
      write.csv2(TableInd, file, fileEncoding = "UTF-8", na = "", row.names = FALSE )
    }
  )
  
  
  output$DlTableInd2 <- downloadHandler( 
    filename=function() {
      paste0("TableIndividus2",".csv")
    },
    content = function(file) {
      
      TableInd2 <- TableInd2()
      write.csv2(TableInd2, file, fileEncoding = "UTF-8", na = "", row.names = FALSE )
    }
  )
  
  output$PasDID <- downloadHandler( 
    filename=function() {
      paste0("TablePourACM_",Sys.Date(),".csv")
    },
    content = function(file) {
      
      donnees_entree <- donnees_entree()
      donnees_entree$ID <- row.names(donnees_entree)
      donnees_entree <- donnees_entree[, c(ncol(donnees_entree), 1:ncol(donnees_entree)-1)]
      write.csv2(donnees_entree, file, fileEncoding = "UTF-8", na = "", row.names = FALSE )
    }
  )
  
  output$DlCroisCl <- downloadHandler(
    
    
    filename=function() {
      
      VariableClasses <-  input$VariableClasses
      paste0("Table_",VariableClasses, "_Classes.csv")
    },
    content = function(file) {
      
      
      TableVarClasses <- TableVarClasses() 
      
      write.csv2(TableVarClasses, file, fileEncoding = "UTF-8", na = "", row.names = FALSE , dec=",")
    }
  )
  
  
  
  # output$PasDID <- renderUI ({
  #   checkboxInput("NoID", "Je n'ai pas d'identifiant" )
  # })
  
  
  
  
  # output$DLTableVar <- downloadHandler( 
  
  #    filename = function() {
  #     paste('TableVar_', Sys.Date(), '.csv', sep='')
  #  },
  #  content = function(con) {
  #    write.csv2(TableVar, con, na="")
  #  }
  #    )
  
  
  #   observeEvent(input$PasDID, {
  
  #   donnees_entree <- donnees_entree()
  #   write.csv2(donnees_entree, "Donnees_ACM.csv", na="", fileEncoding = "UTF-8")
  #   donnees_entree <- read.csv2("Donnees_ACM.csv", fileEncoding = "UTF-8")
  # })
  
  
  # output$downloadPlot <- downloadHandler(
  #  filename = "Shinyplot.png",
  # content = function(file) {
  #  png(file, width = 850, height = 500)
  # graphVar()
  #dev.off()
  # }) 
  
  # Noms des variables illustratives (REMONTER) :
  
  VarsIllusMeta <- reactive({
    #  if (is.null(input$VarIllusPourACM)) return (NULL)
    VarsIllus <- input$VarIllusPourACM
    TableACM <- TableACM()
    
    
    if (length(VarsIllus)==0){
      c<-""
    }else if (length(VarsIllus)==1){
      c<-VarsIllus[1]
    }else{
      
      c<- VarsIllus[1]
      for (i in 2:length(VarsIllus)) {
        c <- paste(c, VarsIllus[i], sep=", ")
      }
    }
    c
  })
  
  VarsIllusQuantiMeta <- reactive({
    #  if (is.null(input$VarIllusPourACM)) return (NULL)
    VarsIllusQuanti <- input$VarIllusQuantiPourACM
    
    if (length(VarsIllusQuanti)==0){
      c<-""
    }else if (length(VarsIllusQuanti)==1){
      c<-VarsIllusQuanti[1]
    }else{
      
      c<- VarsIllusQuanti[1]
      for (i in 2:length(VarsIllusQuanti)) {
        c <- paste(c, VarsIllusQuanti[i], sep=", ")
      }
    }
    c
  })
  
  # G/ Informations synthétiques sur les options de la session -------------
  #\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
  
  
  TableMeta <- reactive ({
    
    # Nom du fichier téléchargé :
    
    NomFichier <-  file_name()
    
    # Noms des variables conservées pour l'ACM :
    
    TableACM <- TableACM()
    NomVar <- names(TableACM[1])
    for (i in 2:ncol(TableACM)) {
      NomVar <-  paste(NomVar, names(TableACM[i]), sep=", ")
    }
    
    # Noms des variables illustratives :
    
    Illus <-  VarsIllusMeta()
    IllusQuanti <- VarsIllusQuantiMeta()
    
    # Filtres sur les individus :
    
    Variable1 <- input$Variable1
    Modalite1 <- input$Modalite1
    Operateur1 <- input$Operateur1
    OperateurMid <- input$OperateurMid
    Variable2 <- input$Variable2
    Modalite2 <- input$Modalite2
    Operateur2 <- input$Operateur2
    OperateurMid2 <- input$OperateurMid2
    Variable3 <- input$Variable3
    Modalite3 <- input$Modalite3
    Operateur3 <- input$Operateur3
    OperateurMid3 <- input$OperateurMid3
    Variable4 <- input$Variable4
    Modalite4 <- input$Modalite4
    Operateur4 <- input$Operateur4
    filtre <- paste(Variable1, Operateur1, Modalite1, OperateurMid,
                    Variable2, Operateur2, Modalite2, OperateurMid2,
                    Variable3, Operateur3, Modalite3,OperateurMid3,
                    Variable4, Operateur4, Modalite4,
                    sep=" ")
    Type.Cl <- input$Type.Cl
    Metric.Cl <- input$Metric.Cl
    Agreg.Cl <- input$Agreg.Cl
    NbAxes.Cl <- input$NbAxes.Cl
    Part.H.Cl <- input$Part.H.Cl
    NbreClasses <- input$NbreClasses
    VarClassesGraphe <- input$VarClassesGraphe
      
      
    # Tables synthétisant les informations métadonnées :
    
    TableMeta <- data.frame (Champs = c("Date et heure",
                                        "Nom du fichier",
                                        "Identifiant principal",
                                        "Identifiant graphique des individus",
                                        "Couleurs dans le graphe des individus",
                                        "Filtre(s) sur les individus",
                                        "Variables incluses dans l'ACM",
                                        "Dont variables illustratives qualitatives",
                                        "Dont variables illustratives quantitatives",
                                        "Type de classification",
                                        "Options sur la classification"), Valeurs = rep("", 11),
                             stringsAsFactors = F)
    
    TableMeta$Valeurs <- ifelse (TableMeta$Champs=="Date et heure",
                                 format(Sys.time(), "%d/%m/%Y, %H:%m"), TableMeta$Valeurs)
    
    TableMeta$Valeurs <- ifelse (TableMeta$Champs=="Nom du fichier",
                                 NomFichier, TableMeta$Valeurs)
    
    TableMeta$Valeurs <- ifelse (TableMeta$Champs=="Identifiant principal",
                                 input$ID, TableMeta$Valeurs)
    TableMeta$Valeurs <- ifelse (TableMeta$Champs=="Identifiant graphique des individus",
                                 input$LabelGraphInd, TableMeta$Valeurs)
    TableMeta$Valeurs <- ifelse (TableMeta$Champs=="Couleurs dans le graphe des individus",
                                 ifelse(VarClassesGraphe == " ",
                                        ifelse(NbreClasses == 1,
                                               "Aucune", 
                                               "Les classes"),
                                        VarClassesGraphe), 
                                 TableMeta$Valeurs)
    
    TableMeta$Valeurs <- ifelse (TableMeta$Champs=="Filtre(s) sur les individus",
                                 filtre, TableMeta$Valeurs)
    TableMeta$Valeurs <- ifelse (TableMeta$Champs=="Variables incluses dans l'ACM",
                                 NomVar, TableMeta$Valeurs)
    TableMeta$Valeurs <- ifelse (TableMeta$Champs=="Dont variables illustratives qualitatives",
                                 Illus, TableMeta$Valeurs)
    TableMeta$Valeurs <- ifelse (TableMeta$Champs=="Dont variables illustratives quantitatives",
                                 IllusQuanti, TableMeta$Valeurs)
    
    TableMeta$Valeurs <- ifelse (TableMeta$Champs=="Type de classification",
                                 Type.Cl, TableMeta$Valeurs)
    
    TableMeta$Valeurs <- ifelse (TableMeta$Champs=="Options sur la classification",
                                 ifelse(Type.Cl == "Pas de classification","-",
                                        paste0("Métrique ",
                                               Metric.Cl,
                                               ", agrégation ",
                                               Agreg.Cl,
                                               ", class. ",
                                               Part.H.Cl,", ", NbreClasses, " classes")), TableMeta$Valeurs)
    
    
    
    TableMeta
  })
  
  output$TableMeta <- renderTable ({
    
    TableMeta()
  })
  
  output$Resume <- renderTable({
    tmp <- donnees_entree()
    if (is.null(tmp)) {return (NULL)}else{
      
      donnees_entree <- data.frame( Variable = names(tmp[1]),
                                    Type = class(tmp[,1]),
                                    NbreValeursDiff = nrow(unique(tmp[1])))
      for (i in (2:ncol(tmp))) {
        donnees_entree <-rbind(donnees_entree, data.frame( Variable = names(tmp[i]),
                                                           Type = class(tmp[,i]),
                                                           NbreValeursDiff = nrow(unique(tmp[i]))))
      }
      donnees_entree
    }
  })
  
})



