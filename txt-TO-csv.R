# Text to CSV
# www.codeandcortex.fr

#install.packages(c("shiny", "dplyr", "readr", "shinyFiles"))

# Charger les librairies nécessaires
library(shiny)
library(dplyr)

# Augmenter la taille limite d'upload (exemple : 10 Mo)
options(shiny.maxRequestSize = 10*1024^2)

# Interface utilisateur
ui <- fluidPage(
  titlePanel("Txt-To-CSV"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("file_txt", "Choisissez le fichier texte (.txt)", accept = c("text/plain")),
      textInput("directory", "Chemin du répertoire cible :", value = fs::path_home()),
      actionButton("process", "Transformer en CSV"),
      checkboxInput("preview", "Aperçu des données transformées", TRUE)
    ),
    
    mainPanel(
      tableOutput("data_preview"),
      textOutput("status")
    )
  )
)

# Fonction serveur
server <- function(input, output, session) {
  
  observeEvent(input$process, {
    req(input$file_txt, input$directory)
    
    # Lire le fichier texte
    texte <- tryCatch({
      readLines(input$file_txt$datapath, encoding = "UTF-8", warn = FALSE)
    }, warning = function(w) {
      message("Attention : Fichier avec une ligne incomplète.")
      readLines(input$file_txt$datapath, encoding = "UTF-8")
    })
    
    # Nettoyer les lignes
    texte <- trimws(texte)  # Supprimer les espaces inutiles
    texte <- texte[texte != ""]  # Supprimer les lignes vides
    
    if (length(texte) == 0) {
      output$status <- renderText({
        "Le fichier texte est vide ou mal formaté."
      })
      return(NULL)
    }
    
    # Initialiser des listes pour stocker les variables étoilées et les paragraphes associés
    variables_list <- list()
    paragraphes_list <- list()
    
    variables_temp <- NULL
    paragraphe_temp <- NULL
    
    # Parcourir le texte ligne par ligne
    for (i in 1:length(texte)) {
      ligne <- texte[i]
      
      # Si la ligne commence par '****', c'est une ligne de variables étoilées
      if (startsWith(ligne, "****")) {
        # Si un bloc de variables étoilées et paragraphe existe déjà, on l'ajoute aux listes
        if (!is.null(variables_temp) && !is.null(paragraphe_temp)) {
          variables_list <- append(variables_list, list(variables_temp))
          paragraphes_list <- append(paragraphes_list, list(paragraphe_temp))
        }
        # Retirer '****'
        variables_temp <- unlist(strsplit(sub("^\\*\\*\\*\\*", "", ligne), " "))
        paragraphe_temp <- NULL  # Réinitialiser le paragraphe temporaire
      } else {
        # Si c'est une ligne de texte, on l'ajoute au paragraphe en cours
        if (!is.null(variables_temp)) {
          paragraphe_temp <- if (is.null(paragraphe_temp)) {
            ligne
          } else {
            paste(paragraphe_temp, ligne)  # Fusionner les lignes de texte
          }
        }
      }
    }
    
    # Ajouter le dernier bloc
    if (!is.null(variables_temp) && !is.null(paragraphe_temp)) {
      variables_list <- append(variables_list, list(variables_temp))
      paragraphes_list <- append(paragraphes_list, list(paragraphe_temp))
    }
    
    # Déterminer le nombre maximal de variables étoilées pour définir les colonnes
    max_vars <- max(sapply(variables_list, length))
    
    # Créer un DataFrame avec autant de colonnes que nécessaire pour les variables
    variables_df <- do.call(rbind, lapply(variables_list, function(x) {
      length(x) <- max_vars  # Remplir avec des NA pour équilibrer le nombre de colonnes
      return(x)
    }))
    
    # Ajouter les paragraphes correspondants
    df <- data.frame(variables_df, Paragraphe = unlist(paragraphes_list), stringsAsFactors = FALSE)
    
    # Renommer les colonnes des variables pour plus de clarté
    colnames(df)[1:max_vars] <- paste0("Variable_", 1:max_vars)
    
    # Récupérer le chemin du répertoire cible
    dir_path <- input$directory
    
    if (!dir.exists(dir_path)) {
      output$status <- renderText({
        "Le répertoire cible n'existe pas. Veuillez vérifier le chemin."
      })
      return(NULL)
    }
    
    # Chemin complet pour sauvegarder le fichier CSV
    csv_path <- file.path(dir_path, "corpus_txtTOcsv.csv")
    write.csv(df, csv_path, row.names = FALSE)
    
    output$status <- renderText({
      paste("Fichier sauvegardé dans :", csv_path)
    })
    
    # Afficher un aperçu si demandé
    if (input$preview) {
      output$data_preview <- renderTable({
        head(df, 10)  # Afficher les 10 premières lignes du tableau fusionné
      })
    }
  })
}

# Lancer l'application Shiny
shinyApp(ui = ui, server = server)
