library(shiny)
library(shinydashboard)
library(lubridate)
library(htmlwidgets)
library(dplyr)
library(twitteR)
library(tidytext)
library(wordcloud2)
library(ggplot2)
library(magrittr)
library(syuzhet)
library(promises)
library(tibble)
library(future)

plan(multiprocess)

ui <- dashboardPage(

  dashboardHeader(title = span(tagList("3LBirds",
                                       icon("twitter"), icon("twitter"), icon("twitter")))),

  dashboardSidebar(disable = TRUE),

  dashboardBody(



    includeCSS("www/styles.css"),

    fillPage(
# fluidPage(
      fluidRow(class = "pagerow", style = paste('height: 100vh'),

               column(width = 3,

                      box(width = 12, title = 'Ferramenta de busca', height = "52vh",
                          status = 'primary', solidHeader = TRUE,

                          textInput(inputId = 'hashtag', label = 'Informe a Hastag',
                                    placeholder = 'Exemplo: #BRAxSUI'),

                          numericInput(inputId = 'n_tw', label = 'Máximo de Tweets',
                                       min = 1, max = 500, value = 200, step = 10),

                          dateRangeInput(inputId = 'date_tw', label = 'Período de busca',
                                         start = Sys.Date()-7,
                                         end = Sys.Date(), startview = 'month', weekstart = 1,
                                         language = 'pt-BR', autoclose = TRUE),

                          br(),

                          div(id='search_buttom',
                              style="display:inline-block;width:100%;text-align: center;",

                              actionButton(inputId = 'search', label = "Buscar Tweets", class = "butt",
                                           icon = icon("twitter"))
                              
                              )
                          ),
                      
                      box(width = 12, height = "33vh",
                          status = 'primary', solidHeader = FALSE,
                          footer =
                            
                            fluidRow(fluidRow("Desenvolvido por", strong("Adelmo Filho"), align = "center"),
                                     
                                     fluidRow(p(
                                       
                                       column(width = 2),
                                       
                                       column(width = 2,
                                              a(icon("github", "fa-2x"),
                                                href="http://github.com/adelmofilho")),
                                       column(width = 2,
                                              a(icon("internet-explorer", "fa-2x"),
                                                href="http://adelmofilho.github.io/aboutme")),
                                       
                                       column(width = 2,
                                              a(icon("twitter", "fa-2x"),
                                                href="https://twitter.com/AdelmoFilho42")),
                                       
                                       column(width = 2,
                                              a(icon("envelope", "fa-2x"),
                                                href="mailto:adelmo.aguiar.filho@gmail.com")),
                                       column(width = 2),
                                       align = "center"))),
                          
                          p(a(id = "gamma", href = "http://www.gamma.ufba.br",
                              img(src = "https://i.imgur.com/b4EKMpi.png", width="70%")),
                            align = "center"),
                          
                          p(class = "grupo", tags$b("Grupo de Pesquisa em Análise Multivariada e Modelagem Aplicada"),
                            align = "center")
                          
                      )
                      ),
               
               column(width = 9,
                      
                      fluidRow(
                        
                        column(width = 8,
                               
                               box(width = 12, wordcloud2Output("distPLot", width = '100%', height = '42vh'),
                                   status = "primary", title = "Wordcloud", solidHeader = TRUE)
                               
                               ), 
                        
                        column(width = 4,
                               
                               div(height = '45vh', style="display:inline-block;width:100%;vertical-align: center;",
                               
                               valueBox("Tweets", textOutput('n_twi'), icon = icon("twitter-square"), width = 12),
                               
                               valueBox("Sentimento", textOutput('sentimento'), icon = icon("heartbeat"), width = 12),

                               valueBox("Alcance", textOutput('alc'), icon = icon("eye"), width = 12)
                               
                               )
                        )),
                      
                      fluidRow(
                        
                        column(width = 12,
                               
                               box(width = 12, plotOutput('timeline',height = '20vh'),
                                   status = "primary", title = "Timeline", solidHeader = TRUE)
                               
                        ))
                      
                      
                      )
               
               
               )
      )
    )
  )

server <- function(input, output) {
  
  
  d <- reactiveValues(dados = NULL)
  
  custom_stop_words <- bind_rows(stop_words,
                                 data_frame(word = c("t.co",'rt','https', 'de', 'a', 'o', 'da', 'ele', 'ela',
                                                     'é','e','el', 'do', 'nele', 'nela', 'pelo', 'pro', 'pra','para',
                                                     'na','em', 'la','por','en', 'um', 'se', paste(1:10),letters, LETTERS),
                                            lexicon = "custom"))
  
  n_wer <- reactiveValues(n = NULL)
  
  a <- reactiveValues(tw = NULL)


observeEvent(input$search, {
  
    consumer_key <- "1Bid7kzybKDxpuNxsnkPMpiGk"
    consumer_secret <- "B6hXiS5DiguAtZwIanYGBzPnVVWss9xzDkuOghW2lqQxBc5xSh"
    access_token <- "998404470106095616-IcqcwPQe8ACOezO776z5oiEEWBZR8WX"
    access_secret <- "bAh5TqS8Ds7k4IztIb5l3Hoa48380T2QuTSrq5hMN1kuj"
    
    setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)
    token <- get("oauth_token", twitteR:::oauth_cache)
    
    a$tw = twitteR::searchTwitter(searchString = input$hashtag, n = input$n_tw, 
                                since = paste(input$date_tw[1]), until = paste(input$date_tw[2]))
    
    n_wer$n = length(a$tw)
    
    if (n_wer$n != 0) {
      
      d$dados <- twitteR::twListToDF(a$tw)
      
    } else {
      
      showModal(modalDialog(
        title = strong("Sua pesquisa retornou 0 resultados"),
        "Por favor, tente novamente com outra hastag ou período de busca.",br(),br(),
        tags$p(tags$img(src="https://i.giphy.com/media/H6KRGhgRMbBwA/giphy.webp", width="300", height="300"),align = "center"),
        easyClose = TRUE,
        footer = NULL))
      
    }
    
  })


  output$distPLot <- renderWordcloud2({
    
    if (!is.null(d$dados)) {
    
      
    d$dados %>% 
        select(text) %>% 
        unnest_tokens(word, text) %>%
        anti_join(custom_stop_words) %>% 
       group_by(word) %>% 
       count() %>% 
      filter(n > 1) %>% 
       arrange(desc(n)) %>% 
      wordcloud2(size = 1.5,color = "skyblue")

          
    }
    
  })
    
    
    output$timeline <- renderPlot({
      
      if (!is.null(d$dados)) {
        
        future({
      
      d$dados %>% 
        select(created) %>%
        mutate(dia = as_date(ymd_hms(created))) %>% 
        group_by(dia) %>% 
        count() %>% 
        ggplot(aes(x = dia, y = n)) +
        geom_bar(stat = "identity",  fill = "dodgerblue1",col = "white")
        })
      
      }
    })
    
    output$n_twi <- renderText({
      
      if (!is.null(d$dados)) {
      
        nrow(d$dados)
          
      } else {
        
        "Aguardando..."
        
      }
      
    })
    
    output$alc <- renderText({
      
      if (!is.null(d$dados)) {
        
        sum(d$dados$retweetCount)
        
      } else {
        
        "Aguardando..."
        
      }
      
    })

    
    output$sentimento <- renderText({
      
      if (!is.null(d$dados)) {
        
       
        
        future({ d$dados %>%
            select(text) %>%
            unnest_tokens(word, text) %>%
            anti_join(custom_stop_words) %>% .[,1] %>% 
            get_nrc_sentiment(language = "portuguese") }) %...>% 
          colSums() %...>% 
          as.data.frame() %...>% 
          rownames_to_column() %...>% 
          rename(sentimento = rowname, n = ".")  %...>% 
          filter(n == max(n)) %...>% 
          slice(1) %...>% 
          unlist() %...>% 
          as.vector()  %...>% 
          .[1]
     
      } else{
        
        "Aguardando..."
        
      }
    })


}

shinyApp(ui, server)