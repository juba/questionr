library(shiny)

#shinyUI(pageWithSidebar(
shinyUI(bootstrapPage(
##    HTML('<link rel="stylesheet" href="highlight.js/styles/tomorrow-night.css">'),
##    HTML('<script src="highlight.js/highlight.pack.js"></script>'),
##    HTML('<script>hljs.highlightBlock($("#codeout"));</script>'),

    HTML('<link href="http://yandex.st/highlightjs/7.3/styles/default.min.css" rel="stylesheet">'),
    HTML('<script src="http://cdnjs.cloudflare.com/ajax/libs/highlight.js/7.3/highlight.min.js"></script>'),
    HTML('<script src="http://yandex.st/highlightjs/7.3/languages/r.min.js"></script>'),

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
            )),
        
    HTML('<script>'),
    HTML("$('.shiny-bound-input').change(function() { $('#codeout').each(function(i, e) {hljs.highlightBlock(e)});});"),
    HTML('</script>')
    
    ))
