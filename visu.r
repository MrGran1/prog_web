# install.packages("shiny")
# install.packages("ggplot2")
# install.packages("randomForest")
# install.packages("caTools")
# install.packages("caret") #confusionMatrix
# install.packages("yardstick")
# install.packages("ggplot2")
# install.packages("ROCR")
# install.packages("e1071")


library(shiny)
library(ggplot2)
library(dplyr)
library(randomForest)
library(caTools)
library(caret)
library(yardstick)
library(ROCR)
library(e1071)


# Interface utilisateur Shiny
ui <- fluidPage(
  titlePanel("Analyse Exploratoire - Choix de Variables"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("fichier", "Choisir le fichier .data"),
      selectInput("colonne1", "Choisir la première variable :", ""),
      selectInput("colonne2", "Choisir la deuxième variable :", ""),
      selectInput("type_viz", "Choisir le type de visualisation :", c("Comparaison de Deux Variables", "Visualisation d'une Seule Variable")),
      actionButton("analyser", "Lancer l'analyse")
    ),
    
    mainPanel(
      tabsetPanel(
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
        ),
        tabPanel("RandomForest",
                 fileInput("fichier_modele_rf", "Choisir le fichier .data"),
                 actionButton("lancer_modele_rf", "Lancer le modèle RandomForest"),
                 textOutput("resultats_modele_rf"),
                 plotOutput("courbe_roc_rf")
        ),
        tabPanel("SVM",
                 fileInput("fichier_modele_svm", "Choisir le fichier .data"),
                 actionButton("lancer_modele_svm", "Lancer le modèle SVM"),
                 textOutput("resultats_modele_svm"),
                 plotOutput("courbe_roc_svm"),
                 textOutput("resultats_modele_svmr"),
                 plotOutput("courbe_roc_svmr")
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
  })
  
  observeEvent(input$analyser, {
    req(input$colonne1)
    
    ###### VISU DOUBLE #######
    
    if (input$type_viz == "Comparaison de Deux Variables") {
      req(input$colonne2)
      
      # Créer le nuage de points
      nuage_points_gg <- ggplot(donnees(), aes(x = !!sym(input$colonne1), y = !!sym(input$colonne2), color = as.factor(donnees()[[input$colonne2]]))) +
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
        coord_flip() +
        theme_minimal() 
      #scale_y_continuous(limits = c(min(donnees()[[input$colonne2]]), max(donnees()[[input$colonne2]])))
      
      
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
      
      ####### VISU SIMPLE #######
      
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
  
  ###### MODELE #######
  
  observeEvent(input$lancer_modele_rf, {
    req(input$fichier_modele_rf$datapath)
    
    getPrecision_Recall_FScore <- function(mat_conf) {
      tp <- mat_conf[2, 2]
      fp <- mat_conf[1, 2]
      fn <- mat_conf[2, 1]
      
      precision <- tp / (tp + fp)
      recall <- tp / (tp + fn)
      fscore <- 2 * (precision * recall) / (precision + recall)
      
      cat("Precision:", precision, "\n")
      cat("Recall:", recall, "\n")
      cat("Fscore:", fscore, "\n")
      
      return(c(precision, recall, fscore))
    }
    
    fonctionRF <- function(donnees, interet) {
      set.seed(123)
      donnees[[interet]] <- as.factor(donnees[[interet]])
      
      ind <- sample(2, nrow(donnees), replace = TRUE, prob = c(0.7, 0.3))
      data_train <- donnees[ind == 1, ]
      data_test <- donnees[ind == 2, ]
      
      rf <- randomForest(as.formula(paste(interet, "~ .")), data = data_train, ntree = 5000, mtry = 2)
      print(rf)
      cat("\n\n")
      
      pred <- predict(rf, data_test)
      mat_conf <- table(observed = data_test[[interet]], predicted = pred)
      cat("Matrice de confusion sur de nouvelles données:\n\n")
      print(mat_conf)
      
      return(list(rf, data_train, data_test, mat_conf))
    }
    
    afficheROC <- function(rf, donnees_test, interet) {
      pred_prob <- predict(rf, newdata = donnees_test, type = "prob")
      pred_positive <- pred_prob[, 2]
      true_labels <- donnees_test[[interet]]
      perf <- prediction(pred_positive, true_labels)
      auc <- performance(perf, "auc")
      cat("AUC =", auc@y.values[[1]], "\n")
      
      pred3 <- performance(perf, "tpr", "fpr")
      
      plot(pred3, main = "ROC Curve pour le Random Forest", col = 2, lwd = 2)
      abline(a = 0, b = 1, lwd = 2, lty = 2, col = "gray")
    }
    
    donnees_modele <- read.table(input$fichier_modele_rf$datapath, header = TRUE, sep = ",")
    model_rf <- fonctionRF(donnees_modele, param_interet <- "X1.1")
    
    res <- getPrecision_Recall_FScore(model_rf[[4]])
    
    output$resultats_modele_rf <- renderText({
      paste("Precision:", res[1], "\n",
            "Recall:", res[2], "\n",
            "Fscore:", res[3], "\n")
    })
    
    output$courbe_roc_rf <- renderPlot({
      afficheROC(model_rf[[1]], model_rf[[3]], param_interet)
    })
  })
  
  observeEvent(input$lancer_modele_svm, {
    req(input$fichier_modele_svm$datapath)
    
    getPrecision_Recall_FScore <- function(mat_conf) {
      #On extrait les valeurs de la matrice de confusion
      tp <- mat_conf[2, 2]  # True Positives
      fp <- mat_conf[1, 2]  # False Positives
      fn <- mat_conf[2, 1]  # False Negatives
      
      
      # Calculer la précision, le rappel et le F-score
      precision <- tp / (tp + fp)
      recall <- tp / (tp + fn)
      fscore <- 2 * (precision * recall) / (precision + recall)
      
      
      # Afficher les résultats
      cat("Precision:", precision, "\n")
      cat("Recall:", recall, "\n")
      cat("Fscore:", fscore, "\n")
      
      return(c(precision,recall,fscore))
    }
    
    #Fonction permettant l'application du modèle svm lineaire
    # parametres : le dataset, le paramètre d'interet
    
    fonctionSVM_lineaire <- function(donnees, interet) {
      SVM = NULL
      set.seed(123)
      
      #on recupère l'index du parametre d'interet
      index_X = which(colnames(donnees) == interet)
      
      #Faire de la variable dependante un facteur (categorique)
      #donnees[[interet]] pour donnees$interet
      donnees[[interet]] <- as.factor(donnees[[interet]])
      
      #echantillonnage trainset et testset
      ind <- sample(2, nrow(donnees), replace = TRUE, prob=c(0.7, 0.3))
      data_train <- donnees[ind == 1,]
      data_test <- donnees[ind == 2,]
      
      #Etape importante dans de nombreux algorithmes d'apprentissage automatique,
      # y compris les machines à vecteurs de support (SVM). Cette pratique vise à normaliser
      # les caractéristiques du jeu de données, c'est-à-dire à les ramener à une échelle commune
      
      #Feature Scaling sauf sur le parametre d'interet
      data_train[-index_X] <- scale(data_train[-index_X])
      data_test[-index_X] <- scale(data_test[-index_X])
      
      #construction du modele
      SVM = svm(as.formula(paste(interet, "~ .")), data = data_train, type = 'C-classification', kernel = 'linear')
      print(SVM)
      
      cat("\n\n")
      
      #prediction
      pred <- predict(SVM, data_test)
      
      #Matrice de confusion
      mat_conf <- table(observed = data_test[[interet]], predicted = pred)
      cat("Matrice de condusion sur de nouvelles données:\n\n")
      print(mat_conf)
      
      return(list(SVM, data_train, data_test, mat_conf))
    }
    
    #Fonction permettant l'application du modèle svm radial
    # parametres : le dataset, le paramètre d'interet
    
    fonctionSVM_radial <- function(donnees, interet) {
      SVM = NULL
      set.seed(123)
      
      #on recupère l'index du parametre d'interet
      index_X = which(colnames(donnees) == interet)
      
      #Faire de la variable dependante un facteur (categorique)
      #donnees[[interet]] pour donnees$interet
      donnees[[interet]] <- as.factor(donnees[[interet]])
      
      #echantillonnage trainset et testset
      ind <- sample(2, nrow(donnees), replace = TRUE, prob=c(0.7, 0.3))
      data_train <- donnees[ind == 1,]
      data_test <- donnees[ind == 2,]
      
      #Feature Scaling sauf sur le parametre d'interet
      data_train[-index_X] <- scale(data_train[-index_X])
      data_test[-index_X] <- scale(data_test[-index_X])
      
      #construction du modele
      SVM = svm(as.formula(paste(interet, "~ .")), data = data_train, type = 'C-classification', kernel = 'radial', gamma = 0.01)
      print(SVM)
      
      cat("\n\n")
      
      #prediction
      pred <- predict(SVM, data_test)
      
      #Matrice de confusion
      mat_conf <- table(observed = data_test[[interet]], predicted = pred)
      cat("Matrice de condusion sur de nouvelles données:\n\n")
      print(mat_conf)
      
      return(list(SVM, data_train, data_test, mat_conf))
    }
    
    
    afficheROC_SVM <- function(SVM,donnees_test,interet, type){
      
      #Prédiction des probabilités avec le modèle svm sur le jeu de données de test
      pred_prob <- predict(SVM, newdata = donnees_test, type = "prob")
      
      #Extraction des probabilités associées à la classe positive (2ème colonne)
      pred_positive <- as.numeric(levels(pred_prob))[pred_prob]
      
      #Obtention des vraies étiquettes (valeurs de la variable cible) à partir du jeu de données de test
      true_labels <- donnees_test[[interet]]
      
      #Création d'un objet de performance en utilisant les probabilités prédites et les vraies étiquettes
      perf <- prediction(pred_positive, true_labels)
      
      #AUC
      auc <- performance(perf, "auc")
      cat("AUC = ", auc@y.values[[1]])
      
      #TPR FPR
      pred3 <- performance(perf, "tpr","fpr")
      
      if(type=="linéaire"){
        #ROC curve
        plot(pred3,main="ROC Curve pour le SVM Linéaire",col=2,lwd=2)
        abline(a=0,b=1,lwd=2,lty=2,col="gray")
      }
      
      else if(type=="radiale"){
        #ROC curve
        plot(pred3,main="ROC Curve pour le SVM Radiale",col=2,lwd=2)
        abline(a=0,b=1,lwd=2,lty=2,col="gray")
      }
    }
    
    donnees_modele_svm <- read.table(input$fichier_modele_svm$datapath, header = TRUE, sep = ",")
    
    #SVM Linéaire
    model_svm <- fonctionSVM_lineaire(donnees_modele_svm, param_interet <- "X1.2")
    
    #SMV Radial
    model_svmr <- fonctionSVM_radial(donnees_modele_svm, param_interet <- "X1.2")
    
    res_l <- getPrecision_Recall_FScore(model_svm[[4]])
    res_r <- getPrecision_Recall_FScore(model_svmr[[4]])
    
    output$resultats_modele_svm <- renderText({
      paste("Precision:", res_l[1], "\n",
            "Recall:", res_l[2], "\n",
            "Fscore:", res_l[3], "\n")
    })
    
    output$resultats_modele_svmr <- renderText({
      paste("Precision:", res_r[1], "\n",
            "Recall:", res_r[2], "\n",
            "Fscore:", res_r[3], "\n")
    })
    
    output$courbe_roc_svm <- renderPlot({
      afficheROC_SVM(model_svm[[1]], model_svm[[3]], param_interet, "linéaire")
    })
    
    output$courbe_roc_svmr <- renderPlot({
      afficheROC_SVM(model_svmr[[1]], model_svm[[3]], param_interet, "radiale")
    })
  })
  
}

# Lancer l'application Shiny
shinyApp(ui = ui, server = server)