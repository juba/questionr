library(shiny)
library(xtable)

## Global variables
df_name <- get(".questionr_icut_df", .GlobalEnv)
df <- get(df_name)
oldvar_name <- get(".questionr_icut_oldvar", .GlobalEnv)
oldvar <- df[,oldvar_name]
## Formatted source variable name
src_var <- ifelse(grepl(" ", oldvar_name),
                  sprintf('%s[,"%s"]', df_name, oldvar_name),
                  sprintf('%s$%s', df_name, oldvar_name))

## Flag to display the alert on first time launch
display_alert <- !exists(".questionr_displayed_alert", .GlobalEnv)
if (display_alert) assign(".questionr_displayed_alert", FALSE, envir=.GlobalEnv)

summary_table <- function(v) {
    out <- "<table class='table table-bordered table-condensed' id='sumtable'>"
    out <- paste0(out, "<thead><tr>")
    out <- paste0(out, "<th>Min</th><th>1st quartile</th><th>Median</th><th>Mean</th><th>3rd quartile</th><th>Max</th><th>NA</th>")
    out <- paste0(out, "</tr></thead><tbody><tr>")
    out <- paste0(out, sprintf("<td>%s</td><td>%s</td><td>%s</td><td>%s</td><td>%s</td><td>%s</td><td>%s</td>",
                          min(v, na.rm=TRUE), quantile(v, prob=0.25, na.rm=TRUE),
                          median(v, na.rm=TRUE), mean(v, na.rm=TRUE),
                          quantile(v, prob=0.75, na.rm=TRUE), max(v, na.rm=TRUE), sum(is.na(v))))
    out <- paste0(out, "</tr></tbody></table>")
    out
}


shinyUI(bootstrapPage(

    ## Copy to clipboard javascript
    HTML('<script type="text/javascript" src="ZeroClipboard.min.js"></script>'),

    ## Custom CSS
    HTML('<link href="main.css" rel="stylesheet" />'),

    ## Page title
    div(class="container-fluid",
        div(class="row",
            headerPanel("Interactive cutting")),

        ## Display an alert, only on first launch for the current session
        if (display_alert) {
               div(class="row-fluid",
                   div(class="span12",
                       div(class="alert alert-dismissable",
                           HTML('<button type="button" class="close" data-dismiss="alert" aria-hidden="true">&times;</button>'),
                           HTML("<strong>Warning :</strong> This inteface doesn't do anything by itself. It only generates R code you'll have to copy/paste into your script and execute yourself.")
                           )))} else "",

        ## First panel : new variable name
        div(class="row-fluid",
            div(class="span8",
                div(class="row-fluid",
                    div(class="span12",
                        tags$form(class="well",
                                  HTML("<table><tr>"),
                                  HTML("<td>New variable : </td><td>"), textInput("newvarname","", paste0(oldvar_name,".rec")),HTML("</td>"),
                                  HTML("</tr></table>")
                                  )),
                    div(class="span12 inner",
                        tags$form(class="well",
                                  HTML(sprintf("<p>Statistics of <tt>%s</tt> :</p>", oldvar_name)),
                                  HTML(summary_table(oldvar)),
                                  textInput("breaks", "Breaks"),
                                  checkboxInput("right", HTML("Right-closed intervals (<tt>right</tt>)"), TRUE),
                                  checkboxInput("inclowest", HTML("Include extreme (<tt>include.lowest</tt>)"), FALSE),
                                  checkboxInput("addext", "Append extreme values if necessary", FALSE)
                                  )))), 

                    
            div(class="span4",
                wellPanel(plotOutput("histOut")))),

        ## Second panel : recoding fields,
  
        ## Main panel with tabs
        div(class="row-fluid",
            div(class="span10",
                tabsetPanel(
                    ## Code tab
                    tabPanel(HTML("Code"), htmlOutput("codeOut")),
                    ## Table check tab
                    tabPanel(HTML("Check"),
                             div(class="span4",
                                 HTML("<p class='header'></p>"),
                                 tableOutput("tableOut")),
                             div(class="span8",
                                 plotOutput("barOut"))
                )),
                
                ## Bottom buttons
                HTML('<p class="bottom-buttons"><button id="copy-button" class="btn btn-primary" data-clipboard-target="codeout"><i class="icon-share icon-white"></i> Copy code to clipboard</button>'),
                HTML('<button id="closebutton" type="button" class="btn btn-success action-button shiny-bound-input"><i class="icon-ok icon-white"></i> Done, close this interface</button></p>')
            )),

        ## Custom javascript
        HTML('<script type="text/javascript" src="main.js"></script>')

        )))
