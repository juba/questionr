library(shiny)

## Global variables
df_name <- get(".questionr_iorder_df", .GlobalEnv)
df <- get(df_name)
oldvar_name <- get(".questionr_iorder_oldvar", .GlobalEnv)
oldvar <- df[,oldvar_name]
## Formatted source variable name
src_var <- ifelse(grepl(" ", oldvar_name),
                  sprintf('%s[,"%s"]', df_name, oldvar_name),
                  sprintf('%s$%s', df_name, oldvar_name))

## Flag to display the alert on first time launch
display_alert <- !exists(".questionr_displayed_alert", .GlobalEnv)
if (display_alert) assign(".questionr_displayed_alert", FALSE, envir=.GlobalEnv)

generate_levels_ol <- function(oldvar) {
    out <- "<ol id='sortable'>"
    ## List of levels
    if (is.factor(oldvar)) levs <- levels(oldvar)
    else levs <- na.omit(unique(oldvar))
    ## Generate fields
    for (l in levs) out <- paste0(out,'<li><i class="icon-move"></i> ',l,'</li>')
    out <- paste0(out, "</ol>")
    HTML(out)     
}


shinyUI(bootstrapPage(
    
    ## Copy to clipboard javascript
    HTML('<script type="text/javascript" src="ZeroClipboard.min.js"></script>'),
    ## Sortable
    HTML('<script type="text/javascript" src="jquery-ui.js"></script>'),
    ## Custom CSS
    HTML('<link href="main.css" rel="stylesheet" />'),

    ## Page title
    div(class="container-fluid",
        div(class="row",
            headerPanel("Interactive levels ordering")),

        ## Display an alert, only on first launch for the current session
        if (display_alert) {
               div(class="row-fluid",
                   div(class="span12",
                       div(class="alert alert-dismissable",
                           HTML('<button type="button" class="close" data-dismiss="alert" aria-hidden="true">&times;</button>'),
                           HTML("<strong>Warning :</strong> This inteface doesn't do anything by itself. It only generates R code you'll have to copy/paste into your script and execute yourself.")
                           )))} else "",

        ## First panel : new variable name and recoding style
        div(class="row-fluid",
            div(class="span12",
                tags$form(class="well",
                          HTML("<table><tr>"),
                          HTML("<td>New variable : </td><td>"),
                          textInput("newvarname","", oldvar_name),
                          HTML("</td>"),
                          HTML("</tr></table>")
                          ))),

        ## Second panel : recoding fields, dynamically generated
        div(class="row-fluid",
            div(class="span12 well",
                    generate_levels_ol(oldvar)
                    )),
        ## Main panel with tabs
        mainPanel(
            tabsetPanel(
                ## Code tab
                tabPanel(HTML("Code"), htmlOutput("codeOut")),
                ## Table check tab
                tabPanel(HTML("Check"),
                         HTML("<p class='header'></p>"),
                         tableOutput("tableOut"))
                ),

            ## Bottom buttons
            HTML('<p class="bottom-buttons"><button id="copy-button" class="btn btn-primary" data-clipboard-target="codeout"><i class="icon-share icon-white"></i> Copy code to clipboard</button>'),
            HTML('<button id="closebutton" type="button" class="btn btn-success action-button shiny-bound-input"><i class="icon-ok icon-white"></i> Done, close this interface</button></p>')
            ),

        ## Custom javascript
        HTML('<script type="text/javascript" src="main.js"></script>')

        )))
