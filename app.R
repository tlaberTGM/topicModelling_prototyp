library(shiny)
library(httr)
library(ggplot2)

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
        sliderInput("bins",
                  "Number of bins:",
                min = 1,
                max = 50,
                value = 30)
        ),
      mainPanel(
          
        )
      )
    )
)

server <- function(input, output) {
    topics <- reactiveVal()
    dependencies <- reactiveVal()
    
    observe({
      IP <- "172.20.10.4"
      Pfad <- "/topiccount/lda"
      url <- sprintf("http://%s:8000%s", IP, Pfad)
      
      res <- httr::GET(url)
      
      status <- httr::status_code(res)
      
      if (status == 200) {
        num_topics <- as.numeric(httr::content(res, as = "text"))
        if (!is.na(num_topics) && num_topics > 0) {
          topics(data.frame(
            TopicName = paste0("Topic ", 0:num_topics),
            TopicNumber = 0:num_topics
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
            IP <- "172.20.10.4"
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
}

shinyApp(ui = ui, server = server)
