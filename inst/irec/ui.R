library(shiny)

df_name <- get("*questionr_irec_df*", .GlobalEnv)
oldvar_name <- get("*questionr_irec_oldvar*", .GlobalEnv)
src_var <- ifelse(grepl(" ", oldvar_name),
                  sprintf('%s[,"%s"]', df_name, oldvar_name),
                  sprintf('%s$%s', df_name, oldvar_name))

shinyUI(bootstrapPage(

    HTML('<style type="text/css">'),
    HTML('span.hl.str { color: #d14;}'),
    HTML('span.hl.kwa { color: #099;}'),
    HTML('span.hl.num { color: #099;}'),
    HTML('span.hl.kwd { color: #333; font-weight: bold;}'),
    ## HTML('td.checkquote { padding-left: 1em;}'),
    ## HTML('td.checkquote label { font-size:10px;}'),
    HTML('td.selstyle { padding-left: 2em;}'),
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
                          HTML("<table><tr>"),
                          HTML("<td>New variable : </td><td>"), textInput("newvarname","", paste0(oldvar_name,".rec")),HTML("</td>"),
                          HTML('<td class="selstyle">Recoding style : </td><td>'),
                          selectInput("recstyle", "", c("Default"="default", "Character - complete"="charcomp", "Character - minimal"="charmin","Factor"="factor")),
                          HTML("</td>"),
                          HTML("</tr></table>")
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
