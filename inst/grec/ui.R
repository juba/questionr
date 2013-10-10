library(shiny)

#shinyUI(pageWithSidebar(
shinyUI(bootstrapPage(

    HTML('<style type="text/css">'),
    HTML('span.hl.str { color: #d14;}'),
    HTML('span.hl.kwa { color: #099;}'),
    HTML('span.hl.kwd { color: #333;}'),
    HTML('</style>'),
    
    div(class="container-fluid",
        div(class="row",
            ## Application title
            headerPanel("Interactive recoding")),
        div(class="row-fluid",
            div(class="span12",
                div(class="alert alert-dismissable",
                    HTML('<button type="button" class="close" data-dismiss="alert" aria-hidden="true">&times;</button>'),
                    HTML('<strong>Attention :</strong> Pensez à copier/coller le code.')
                    ))),
        
        div(class="row-fluid",
            div(class="span12",
                tags$form(class="well",
                          uiOutput("levelsInput")))),
        mainPanel(
            tabsetPanel(
                tabPanel("Code", htmlOutput("recodeOut")),
                tabPanel("Table", tableOutput("tableOut"))
                )
            ))
        
    ))
