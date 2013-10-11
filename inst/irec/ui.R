library(shiny)

df_name <- get(".questionr_irec_df", .GlobalEnv)
oldvar_name <- get(".questionr_irec_oldvar", .GlobalEnv)
src_var <- ifelse(grepl(" ", oldvar_name),
                  sprintf('%s[,"%s"]', df_name, oldvar_name),
                  sprintf('%s$%s', df_name, oldvar_name))
display_alert <- !exists(".questionr_displayed_alert", .GlobalEnv)
if (display_alert) assign(".questionr_displayed_alert", FALSE, envir=.GlobalEnv)

shinyUI(bootstrapPage(

    HTML('<script type="text/javascript" src="ZeroClipboard.min.js"></script>'),
    
    HTML('<style type="text/css">'),
    HTML('span.hl.str { color: #d14;}'),
    HTML('span.hl.kwa { color: #099;}'),
    HTML('span.hl.num { color: #099;}'),
    HTML('span.hl.kwd { color: #333; font-weight: bold;}'),
    HTML('span.hl.com { color: #888; font-style: italic;}'),
    ## HTML('td.checkquote { padding-left: 1em;}'),
    ## HTML('td.checkquote label { font-size:10px;}'),
    HTML('td.selstyle { padding-left: 2em;}'),
    HTML('</style>'),
    
    div(class="container-fluid",
        div(class="row",
            ## Application title
            headerPanel("Interactive recoding")),

        if (display_alert) {
               div(class="row-fluid",
                   div(class="span12",
                       div(class="alert alert-dismissable",
                           HTML('<button type="button" class="close" data-dismiss="alert" aria-hidden="true">&times;</button>'),
                           HTML("<strong>Warning :</strong> This inteface doesn't do anything by itself. It only generates R code you'll have to copy/paste into your script and execute yourself.")
                           )))} else "",

        div(class="row-fluid",
            div(class="span12",
                tags$form(class="well",
                          HTML("<table><tr>"),
                          HTML("<td>New variable : </td><td>"), textInput("newvarname","", paste0(oldvar_name,".rec")),HTML("</td>"),
                          HTML('<td class="selstyle">Recoding style : </td><td>'),
                          selectInput("recstyle", "", c("Character - complete"="charcomp", "Character - minimal"="charmin")),
                          HTML("</td><td class='selstyle'>"),
                          checkboxInput("facconv", "Convert to factor", FALSE),
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
                tabPanel("Check",
                         HTML("<p style='font-size: 11px;'>Old variable as rows, new variable as columns.</p>"),
                         tableOutput("tableOut"))
                ),
            HTML('<p style="margin-top:15px;"><button id="copy-button" class="btn btn-primary" data-clipboard-target="codeout"><i class="icon-share icon-white"></i> Copy code to Clipboard</button> <button id="closebutton" type="button" class="btn btn-success action-button shiny-bound-input" onclick="window.close();"><i class="icon-ok icon-white"></i> Done, close this interface</button></p>')
            ),

        HTML('<script type="text/javascript">'),
        HTML(' var clip = new ZeroClipboard($("#copy-button"));'),
        HTML("clip.on( 'mouseup', function ( client, args ) { alert( \"Code copied to clipboard\" );} );"),
        HTML('</script>')

        )))
