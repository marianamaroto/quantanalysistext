library(shiny)
library(quanteda.dictionaries)
library(tm)
library(SnowballC)
library(wordcloud)

ui <- fluidPage(
  tabsetPanel(
    
    # SET UP TAB
    tabPanel("Set Up",
             headerPanel(title=div(img(src="skim.png", height = 50), "Quant Analysis of Text or Open Ends")),
             sidebarLayout(
               sidebarPanel(
                 fileInput("data_file", "Choose File", multiple = FALSE, accept = c(
                   "text/csv",
                   "text/comma-separated-values,text/plain",
                   ".csv")),
                 numericInput("colnum", "Choose column number for your text", value = 1),
                 submitButton("Apply", icon = NULL),
                 uiOutput("choosewords"), width = 4),
               mainPanel(
                 h2("Common Stem Words"),
                 h3(plotOutput("wordcloud"),
                    h4(dataTableOutput("dtm")))
               ))
    ),
    
    tabPanel("Emotions",
             headerPanel(title=div(img(src="skim.png", height = 50), "Quant Analysis of Text or Open Ends")),
               mainPanel(
                 h2("Emotion Analysis"),
                 h3(dataTableOutput("emotions")))
    ),
    
    tabPanel("Virtues",
             headerPanel(title=div(img(src="skim.png", height = 50), "Quant Analysis of Text or Open Ends")),
               mainPanel(
                 h2("Virtues Analysis"),
                 h3(dataTableOutput("virtues")))
    ),  
    
    tabPanel("Psychology",
             headerPanel(title=div(img(src="skim.png", height = 50), "Quant Analysis of Text or Open Ends")),
               mainPanel(
                 h2("Psychology Analysis"),
                 h3(dataTableOutput("psych")))
    ),  
    
    tabPanel("Economics",
             headerPanel(title=div(img(src="skim.png", height = 50), "Quant Analysis of Text or Open Ends")),
               mainPanel(
                 h2("Economics Analysis"),
                 h3(dataTableOutput("economics")))
    ),
    tabPanel("Dummy Coding",
             headerPanel(title=div(img(src="skim.png", height = 50), "Quant Analysis of Text or Open Ends")),
               mainPanel(
                 h2("Dummy Coding"),
                 h3(dataTableOutput("dummy")))
    ),
    tabPanel("Download",
             headerPanel(title=div(img(src="skim.png", height = 50), "Quant Analysis of Text or Open Ends")),
               mainPanel(
                 h2("Download Data Tables"),
                 h3(downloadButton("downemotions","Emotions")),
                 h3(downloadButton("downvirtue","Virtues")),
                 h3(downloadButton("downpsych","Psychology")),
                 h3(downloadButton("downeco","Economics")),
                 h3(downloadButton("downdummy","Dummy Coding")))
    )
  ))


server <- function(input, output) {
    inFile <- reactive({input$data_file})
    text_table <- reactive({read.csv(inFile()$datapath)})
    for_analysis <- reactive({(text_table())[,input$colnum]})
    for_analysis_cf <- reactive({as.character.factor(text_table()[,input$colnum])})
    corpus = reactive({VCorpus(VectorSource(for_analysis()))})
    corpus1 = reactive({tm_map(corpus(), content_transformer(tolower))})
    corpus2 = reactive({tm_map(corpus1(), removeNumbers)})
    corpus3 = reactive({tm_map(corpus2(), removePunctuation)})
    corpus4 = reactive({tm_map(corpus3(), removeWords, stopwords())})
    corpus5 = reactive({tm_map(corpus4(), stemDocument)})
    corpus6 = reactive({tm_map(corpus5(), stripWhitespace)})
    
    tdm = reactive({TermDocumentMatrix(corpus6())})
    
    count <- reactive({rowSums(as.matrix(tdm()))})
    count1 <- reactive({data.frame("stem_word" = rownames(as.matrix(tdm())), "count" = count())})
    count2 <- reactive({count1()[order(-count1()$count),]})
    fifty <- reactive({head(count2()$stem_word,50)})
    
    load("data_dictionary_NRC.rda")
    load("data_dictionary_MFD.rda")
    load("data_dictionary_RID.rda")
    load("data_dictionary_LaverGarry.rda")
    emotions <- reactive({liwcalike(for_analysis_cf(), dictionary = data_dictionary_NRC)})
    virtues <- reactive({liwcalike(for_analysis_cf(), dictionary = data_dictionary_MFD)})
    psych <- reactive({liwcalike(for_analysis_cf(), dictionary = data_dictionary_RID)})
    economics <- reactive({liwcalike(for_analysis_cf(), dictionary = data_dictionary_LaverGarry)})

  output$wordcloud <- renderPlot({
    if (is.data.frame(input$data_file)) {
      v <- rowSums(as.matrix(tdm()))
      names(v) <- rownames(as.matrix(tdm()))
      v <- sort(v, decreasing=TRUE)
      wordcloud(names(v), v, min.freq=10, max.words = 50)
    }})
  output$dtm <- renderDataTable({
    if (is.data.frame(input$data_file)) {
      count2()
    }}) 
  output$choosewords <- renderUI({
    if (is.data.frame(input$data_file)) {
      checkboxGroupInput("chosenwords", "Select stem words:", fifty())
    }}) 
  
  output$emotions <- renderDataTable({emotions()})
  output$virtues <- renderDataTable({virtues()})
  output$psych <- renderDataTable({psych()})
  output$economics <- renderDataTable({economics()})
  
  output$dummy <- renderDataTable({
    if (is.data.frame(input$data_file)) {
      dtm = DocumentTermMatrix(corpus6(), control = list(dictionary=input$chosenwords))
      dtm
    }})
  output$downemotions <- downloadHandler(
    filename = "emotions.csv", content = function (file) {write.csv(emotions(),file)})
  output$downvirtue <- downloadHandler(
    filename = "virtues.csv", content = function (file) {write.csv(virtues(),file)})
  output$downpsych <- downloadHandler(
    filename = "psychology.csv", content = function (file) {write.csv(psych(),file)})
  output$downeco <- downloadHandler(
    filename = "economics.csv", content = function (file) {write.csv(economics(),file)})
  output$downdummy <- downloadHandler(
    filename = "dummy_coding.csv", content = function (file) {
      dtm = DocumentTermMatrix(corpus6(), control = list(dictionary=input$chosenwords))
      dtm <- as.matrix(dtm)
      write.csv(dtm,file)})
}

shinyApp(ui,server)