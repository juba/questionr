library(shiny)

## Global variables
df_name <- getOption("questionr_irec_df")
oldvar_name <- getOption("questionr_irec_oldvar")
## Formatted source variable name
src_var <- ifelse(grepl(" ", oldvar_name),
                  sprintf('%s[,"%s"]', df_name, oldvar_name),
                  sprintf('%s$%s', df_name, oldvar_name))

## Flag to display the alert on first time launch
display_alert <- is.null(getOption("questionr_displayed_alert"))
if (display_alert) options(questionr_displayed_alert=TRUE)

shinyUI(bootstrapPage(

    ## Copy to clipboard javascript
    HTML('<script type="text/javascript" src="ZeroClipboard.min.js"></script>'),

    ## Custom CSS
    HTML('<link href="main.css" rel="stylesheet" />'),

    ## Page title
    div(class="container-fluid",
        div(class="row",
            headerPanel("Interactive recoding")),

        ## Display an alert, only on first launch for the current session
        if (display_alert) {
               div(class="row-fluid",
                   div(class="span12",
                       div(class="alert alert-dismissable",
                           HTML('<button type="button" class="close" data-dismiss="alert" aria-hidden="true">&times;</button>'),
                           HTML("<strong>Warning :</strong> This interface doesn't do anything by itself. It only generates R code you'll have to copy/paste into your script and execute yourself.")
                           )))} else "",

        ## First panel : new variable name and recoding style
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

        ## Second panel : recoding fields, dynamically generated
        div(class="row-fluid",
            div(class="span12",
                tags$form(class="well",
                          uiOutput("levelsInput")))),
        ## Main panel with tabs
        mainPanel(
            tabsetPanel(
                ## Code tab
                tabPanel(HTML("Code"), htmlOutput("recodeOut")),
                ## Table check tab
                tabPanel(HTML("Check"),
                         HTML("<p class='header'>Old variable as rows, new variable as columns.</p>"),
                         tableOutput("tableOut"))
                ),

            ## Bottom buttons
            HTML('<p class="bottom-buttons"><button id="copy-button" class="btn btn-primary" data-clipboard-target="codeout"><i class="icon-share icon-white"></i> Copy code to clipboard</button>'),
            HTML('<button id="closebutton" type="button" class="btn btn-success action-button shiny-bound-input"><i class="icon-ok icon-white"></i> Done, close this interface</button></p>')
            ),

        ## Custom javascript
        HTML('<script type="text/javascript" src="main.js"></script>')

        )))
