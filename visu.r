# Installer les packages Shiny et ggplot2 s'ils ne sont pas déjà installés
# install.packages("shiny")
# install.packages("ggplot2")
if(!require('imbalance')) {
  install.packages('imbalance')
  library('imbalance')
}

library(magrittr)
library(dplyr)
library(shiny)
library(ggplot2)
source('utils.r')
# Interface utilisateur Shiny
ui <- fluidPage(
  titlePanel("Analyse Exploratoire - Choix de Variables"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("fichier", "Choisir le fichier .data"),
      selectInput("colonne1", "Choisir la première variable :", ""),
      selectInput("colonne2", "Choisir la deuxième variable :", ""),
      selectInput("type_viz", "Choisir le type de visualisation :", c("Comparaison de Deux Variables", "Visualisation d'une Seule Variable")),
      actionButton("analyser", "Lancer l'analyse"),
      actionButton("button_to_NA", "Lancer le Preprocessing")
      
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel( "Affichage des données",
                  tabsetPanel(
                    tabPanel("Donnéees" ,tableOutput("myDataTable"))
                  )                  
        ),
        tabPanel( "Preproprecessing",
                  tabsetPanel(
                    textInput("string_to_replace","Entrer le string que vous voulez remplacer par des NA"),
                    selectInput("numericMethod", "Choose Method for Numeric Variables to replace NA:",
                                choices = c("None","Mean", "Median")),
                    selectInput("categoricalMethod", "Choose Method for Categorical Variables to replace NA:",
                                choices = c("None","Most Frequent", "Least Frequent")),
                    selectInput("variable_classe", "Variable qui vaut Classe :", ""),
                    checkboxInput('do_normalisation', 'Voulez vous normaliser le dataset',value=FALSE)
                    
                  )
        ),
        
        tabPanel("Comparaison de Deux Variables", 
                 tabsetPanel(
                   tabPanel("Nuage de Points", plotOutput("nuage_points")),
                   tabPanel("Caractéristiques par Valeur", plotOutput("caracteristiques_valeurs")),
                   tabPanel("Tableau Récapitulatif", tableOutput("tableau_recap"))
                 )
        ),
        tabPanel("Visualisation d'une Seule Variable", 
                 tabsetPanel(
                   tabPanel("Diagramme en Barre", plotOutput("barplot")),
                   tabPanel("Boîte à Moustaches", plotOutput("boxplot")),
                   tabPanel("Courbe des Fréquences Cumulées", plotOutput("courbe_freq_cumulee")),
                   tabPanel("Tableau Statistique", tableOutput("tableau_stats"))
                 )
        )
      )
    )
  )
)

# Serveur Shiny
server <- function(input, output, session) {
  
  donnees <- reactive({
    req(input$fichier)
    read.table(input$fichier$datapath, header = TRUE, sep = ",")
  })
  
  observe({
    req(input$fichier)
    
    # Mettre à jour les choix des variables lorsque le fichier est chargé
    col_choices <- names(donnees())
    updateSelectInput(session, "colonne1", choices = col_choices)
    updateSelectInput(session, "colonne2", choices = col_choices)
    updateSelectInput(session, "variable_classe", choices = col_choices)
  })
  
  
  observeEvent(input$analyser, {
    req(input$colonne1)
    
    if (input$type_viz == "Comparaison de Deux Variables") {
      req(input$colonne2)
      
      # Créer le nuage de points
      nuage_points_gg <- ggplot(donnees(), aes_string(x = input$colonne1, y = input$colonne2)) +
        geom_point() +
        labs(title = paste("Nuage de Points entre", input$colonne1, "et", input$colonne2),
             x = input$colonne1, y = input$colonne2) +
        theme_minimal()
      
      # Afficher le nuage de points
      output$nuage_points <- renderPlot({
        print(nuage_points_gg)
      })
      
      # Créer les caractéristiques par valeur
      caracteristiques_valeurs_gg <- ggplot(donnees(), aes_string(x = input$colonne1, y = input$colonne2)) +
        geom_boxplot() +
        labs(title = paste("Caractéristiques par Valeur pour", input$colonne1, "et", input$colonne2),
             x = input$colonne1, y = input$colonne2) +
        theme_minimal()
      
      # Afficher les caractéristiques par valeur
      output$caracteristiques_valeurs <- renderPlot({
        print(caracteristiques_valeurs_gg)
      })
      
      tableau_recap <- as.data.frame(t(sapply(donnees()[, c(input$colonne1, input$colonne2), drop = FALSE], summary)))
      
      # Renommer les colonnes
      colnames(tableau_recap) <- paste(rep(c(""), each = 1), colnames(tableau_recap), sep = "")
      
      # Afficher le tableau récapitulatif
      output$tableau_recap <- renderTable({
        print(tableau_recap)
      })
      
      # Créer la visualisation en fonction du choix de l'utilisateur
      gg <- ggplot(donnees(), aes_string(x = input$colonne1, y = input$colonne2)) +
        geom_point() +
        labs(title = paste("Nuage de Points entre", input$colonne1, "et", input$colonne2),
             x = input$colonne1, y = input$colonne2) +
        theme_minimal()
      
      # Afficher la visualisation
      output$graphique <- renderPlot({
        print(gg)
      })
      
    } else if (input$type_viz == "Visualisation d'une Seule Variable") {
      
      # Créer le diagramme en barre
      barplot_gg <- ggplot(donnees(), aes_string(x = input$colonne1)) +
        geom_bar() +
        labs(title = paste("Diagramme en Barre pour", input$colonne1),
             x = input$colonne1, y = "Count") +
        theme_minimal()
      
      # Afficher le diagramme en barre
      output$barplot <- renderPlot({
        print(barplot_gg)
      })
      
      # Créer la boîte à moustaches
      boxplot_gg <- ggplot(donnees(), aes_string(y = input$colonne1)) +
        geom_boxplot() +
        labs(title = paste("Boîte à Moustaches pour", input$colonne1),
             x = "", y = input$colonne1) +
        theme_minimal()
      
      # Afficher la boîte à moustaches
      output$boxplot <- renderPlot({
        print(boxplot_gg)
      })
      
      # Créer la courbe des fréquences cumulées
      freq_cumulee_gg <- ggplot(donnees(), aes_string(x = input$colonne1)) +
        stat_ecdf(geom = "step", pad = FALSE) +
        labs(title = paste("Courbe des Fréquences Cumulées pour", input$colonne1),
             x = input$colonne1, y = "Fréquence Cumulée") +
        theme_minimal()
      
      # Afficher la courbe des fréquences cumulées
      output$courbe_freq_cumulee <- renderPlot({
        print(freq_cumulee_gg)
      })
      
      # Créer le tableau statistique
      stats_table <- summary(donnees()[, input$colonne1, drop = FALSE])
      
      # Afficher le tableau statistique
      output$tableau_stats <- renderTable({
        print(stats_table)
      })
    }
  })
  # Remplacement de certain champs par des NA
  #Lancement du preprocessing
  observeEvent(input$button_to_NA, {
    donnees <- (replace_by_NA(donnees(),input$string_to_replace))
    #Remplacement de tout les NA
    donnees <- replace_missing_values(donnees,input$numericMethod, input$categoricalMethod)
    
 #  if(input$do_normalisation){
  #   normaliser(df)
  # }
    #print(input$variable_classe)
    #print(class_diff(donnees,input$variable_classe))
    #if (class_diff<0.8){
    #oversample(donnees(),classAttr=input$variable_classe,ratio=0.8)
    #}  
    output$myDataTable <- renderTable(donnees())
    
  })
  # Print les données
 output$myDataTable <- renderTable(donnees())
  
}

# Lancer l'application Shiny
shinyApp(ui = ui, server = server)
