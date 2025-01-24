library(shiny)
library(httr)
library(ggplot2)
library(DT)
library(dplyr)

IP <- "localhost"

ui <- navbarPage("TextminR",
  tabPanel("All Documents",
    sidebarLayout(
        sidebarPanel("Wähle ein Topic aus: ",
          div(
            style = "overflow-y: scroll; max-height: 400px;",
            uiOutput("topicsList")
          )
        ),
        mainPanel(
           plotOutput("selectedTopic")
        )
    )
  ),
  tabPanel("Specific Document",
    sidebarLayout(
      sidebarPanel(
        textInput("such_string", "Suchfeld"),
        helpText("Eine Suchfunktion um spezielle Werke zu filtern"),
        div(
          style = "overflow-y: scroll; max-height: 400px;",
          uiOutput("documents_list"),
          div(
            style = "position: absolute; bottom: -10px; left: 10px; width: calc(100% - 20px);",
            actionButton(
              inputId = "plot_button",
              label = "Topics anzeigen",
              style = "width: 100%;"
            )
          )
        )
      ),
      mainPanel(
        verbatimTextOutput("document_content")
      )
    )
  )
)

server <- function(input, output, session) {
    topics <- reactiveVal()
    dependencies <- reactiveVal()
    
    observe({
      IP <- IP
      Pfad <- "/topiccount/lda"
      url <- sprintf("http://%s:8000%s", IP, Pfad)
      
      res <- httr::GET(url)
      
      status <- httr::status_code(res)
      
      if (status == 200) {
        num_topics <- as.numeric(httr::content(res, as = "text"))
        if (!is.na(num_topics) && num_topics > 0) {
          topics(data.frame(
            TopicName = paste0("Topic ", 0:(num_topics-1)),
            TopicNumber = 0:(num_topics-1)
            ))
        } else {
          topics(NULL)
        }
      } else {
        error_message <- paste("Fehler beim API-Aufruf! HTTP Status:", status)
        topics(error_message)
        print(error_message)
      }
    })
    
    output$topicsList <- renderUI({
      req(topics())
      lapply(1:nrow(topics()), function(i) {
        topic_name <- topics()$TopicName[i]
        topic_number <- topics()$TopicNumber[i]
        actionLink(inputId = paste0("topic_", topic_number), label = topic_name, style = "display: block;")
      })
    })
    
    
    observe({
      req(topics())
      lapply(1:nrow(topics()), function(i) {
        topic_number <- topics()$TopicNumber[i]
        observeEvent(input[[paste0("topic_", topic_number)]], {
            IP <- IP
            url <- sprintf("http://%s:8000/topics/lda/%d", IP, topic_number)
            
            res <- httr::GET(url)
            
            if (httr::status_code(res) == 200) {
              dependencies_data <- httr::content(res, as = "parsed")
              
              dependencies_df <- data.frame(
                name = names(dependencies_data),
                wert = unlist(dependencies_data, use.names = FALSE),
                stringsAsFactors = FALSE
              )

              dependencies(dependencies_df)

              output$selectedTopic <- renderPlot({
                ggplot(dependencies_df, aes(x = wert, y = reorder(name, wert), fill = wert)) +
                  geom_bar(stat = "identity", color = "black") +
                  scale_fill_gradient(low = "#03a1fc", high = "#1803fc") +
                  labs(
                    title = sprintf("Topic %d: Relevanz der Wörter", topic_number),
                    x = "",
                    y = ""
                  ) +
                  theme_minimal(base_size = 14) +
                  theme(
                    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
                    axis.title.x = element_text(margin = margin(t = 10)),
                    axis.title.y = element_text(margin = margin(r = 10)),
                    axis.text.y = element_text(size = 12),
                    axis.text.x = element_text(size = 12),
                    legend.position = "none"
                  )
              })
            } else {
              output$selectedTopic <- renderText({
                paste("Error: Failed to load dependencies for topic number:", topic_number)
              })
            }
        })
      })
    })
    
    df <- reactiveVal()
    document_content <- reactiveVal()
    content_id <- reactiveVal()
    
    observe({
      IP <- IP
      Pfad <- "/documents"
      url <- sprintf("http://%s:8000%s", IP, Pfad)
      
      res <- httr::GET(url)
      
      status <- httr::status_code(res)
      
      if (status == 200) {
        documents <- httr::content(res, as = "parsed")
        
        df(data.frame(
          id = names(documents),
          titel = unlist(documents, use.names = FALSE),
          stringsAsFactors = FALSE
        ))
      } else {
        error_message <- paste("Fehler beim API-Aufruf! HTTP Status:", status)
        
      }
    })
    
    filtered_documents <- reactive({
      req(df())
      print(df())
      if (!"titel" %in% colnames(df())) {
        stop("Die Spalte 'titel' wurde nicht gefunden.")
      }
      
      if (input$such_string != "") {
        df() %>%
          filter(grepl(input$such_string, titel, ignore.case = TRUE))
      } else {
        df()
      }
    })
    
    output$documents_list <- renderUI({
      req(filtered_documents())
      
      if (nrow(filtered_documents()) == 0) {
        tags$p("Keine Dokumente gefunden.")
      } else {
        tags$ul(
          lapply(1:nrow(filtered_documents()), function(i) {
            documents_name <- filtered_documents()$titel[i]
            document_id <- filtered_documents()$id[i]
            actionLink(
              inputId = paste0("document_", document_id), 
              label = documents_name, 
              style = "display: block;")
          })
        )
      }
    })
    
    observe({
      req(filtered_documents())
      
      lapply(1:nrow(filtered_documents()), function(i) {
        documents_id <- filtered_documents()$id[i]
        action_link_id <- paste0("document_", documents_id)
        
        observeEvent(input[[action_link_id]], {
          content_id(documents_id)
          IP <- IP
          url <- sprintf("http://%s:8000/document/content/%s", IP, documents_id)
          
          res <- httr::GET(url)
          
          if (httr::status_code(res) == 200) {
            documents_data <- httr::content(res, as = "parsed")
            
            df_inhalt <- data.frame(
              inhalt = documents_data,
              stringsAsFactors = FALSE
            )
            
            document_content(paste(df_inhalt, collapse = "\n"))
          } else {
            document_content("Fehler beim Laden des Dokuments.")
          }
        })
      })
    })
    
    output$document_content <- renderText({
      if (is.null(document_content())) {
        return("Noch kein Dokument ausgewählt oder kein Inhalt verfügbar.")
      } else {
        req(document_content())
        document_content()
      }
    })
    
    observeEvent(input$plot_button, {
      showModal(
        modalDialog(
          title = "Beispiel-Plot",
          plotOutput("modal_plot"),
          size = "l",
          easyClose = TRUE,
          footer = modalButton("Schließen")
        )
      )
    })
    
    output$modal_plot <- renderPlot({
      req(content_id())
      
      IP <- IP
      url <- sprintf("http://%s:8000/document/topics/%s", IP, content_id())
      
      res <- httr::GET(url)
      
      doc_topics <- data.frame(doc_TopicName = character(), doc_TopicWert = numeric())
      
      if (httr::status_code(res) == 200) {
        doc_topics_data <- httr::content(res, as = "text")
        
        doc_topics_data <- gsub('\\"', '"', doc_topics_data)
        
        doc_topics_data <- jsonlite::fromJSON(doc_topics_data)
        
        if (!is.null(doc_topics_data)) {
          doc_topics <- data.frame(
            doc_TopicName = names(doc_topics_data),
            doc_TopicWert = unlist(doc_topics_data, use.names = FALSE),
            stringsAsFactors = FALSE
          )
        } else {
          showNotification("Keine Daten verfügbar für die Topics des Dokuments.", type = "warning")
        }
      } else {
        showNotification(sprintf("Fehler beim API-Aufruf: HTTP %d", httr::status_code(res)), type = "error")
        return(NULL)
      }
      
      ggplot(doc_topics, aes(x = doc_TopicWert, y = reorder(doc_TopicName, doc_TopicWert), fill = doc_TopicWert)) +
          geom_bar(stat = "identity", color = "black") +
          scale_fill_gradient(low = "#03a1fc", high = "#1803fc") +
          labs(
            title = sprintf("Topics des ausgewählten Werkes"),
            x = "",
            y = ""
          ) +
          theme_minimal(base_size = 14) +
          theme(
            plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
            axis.title.x = element_text(margin = margin(t = 10)),
            axis.title.y = element_text(margin = margin(r = 10)),
            axis.text.y = element_text(size = 12),
            axis.text.x = element_text(size = 12),
            legend.position = "none"
          )
      })
}

shinyApp(ui = ui, server = server)
