##' Interactive reordering of factor levels
##'
##' This function launches a shiny app in a web browser in order to do
##' interactive reordering of the levels of a categorical variable (character
##' or factor).
##'
##' @param dfobject data frame to operate on, as an object or a character string
##' @param oldvar name of the variable to be reordered, as a character string (possibly without quotes)
##' @details
##' The generated convert the variable into a factor, as only those allow for levels ordering.
##' @return
##' The function launches a shiny app in the system web browser. The reordering code is returned in the console
##' when the app is closed with the "Done" button.
##' @author Julien Barnier <julien.barnier@@ens-lyon.fr>
##' @examples
##' \dontrun{data(hdv2003)
##' iorder(hdv2003, "qualif")}
##' @import shiny
##' @importFrom highr hi_html
##' @export

iorder <- function(dfobject, oldvar) {
    ## Check if dfobject is an object or a character string
    if (!is.character(dfobject)) dfobject <- deparse(substitute(dfobject))
    ## Prevents get() conflicts
    if (dfobject=="dfobject") stop(sQuote(paste0(dfobject, ' must not be an object named "dfobject".')))
    ## Check if dfobject is a data frame
    if (!is.data.frame(get(dfobject))) stop(sQuote(paste0(dfobject, ' must be a data frame.')))
        ## If oldvar is not a character string, deparse it
    is_char <- FALSE
    try(if(is.character(oldvar)) is_char <- TRUE, silent=TRUE)
    if (!is_char) oldvar <- deparse(substitute(oldvar))
    ## Check if oldvar is a column of dfobject
    if (!(oldvar %in% names(get(dfobject)))) stop(sQuote(paste0(oldvar, ' must be a column of ', dfobject, '.')))    
    
    ## Global variables
    ## Original data frame name and object
    df_name <- dfobject
    df <- get(df_name)
    if (inherits(df, "tbl_df") || inherits(df, "data.table")) df <- as.data.frame(df)
    ## Variable to be recoded, name and object
    oldvar_name <- oldvar
    oldvar <- df[,oldvar_name]
    ## Formatted source variable name
    src_var <- ifelse(grepl(" ", oldvar_name),
                      sprintf('%s[,"%s"]', df_name, oldvar_name),
                      sprintf('%s$%s', df_name, oldvar_name))

    ## Flag to display the alert on first time launch
    show_alert <- is.null(getOption("questionr_hide_alert"))
    if (show_alert) options(questionr_hide_alert=TRUE)      

    ## CSS file
    css.file <- system.file(file.path("shiny", "css", "ifuncs.css"), package = "questionr")
    css.content <- paste(readLines(css.file),collapse="\n")
    ## JS files
    jquery.ui.file <- system.file(file.path("shiny", "js", "jquery-ui.js"), package = "questionr")
    jquery.ui.content <- paste(readLines(jquery.ui.file),collapse="\n")
    js.file <- system.file(file.path("shiny", "js", "iorder.js"), package = "questionr")
    js.content <- paste(readLines(js.file),collapse="\n")
    
    
    generate_levels_ol <- function(oldvar) {
      out <- "<ol id='sortable' class='sortable'>"
      ## List of levels
      if (is.factor(oldvar)) levs <- levels(oldvar)
      else levs <- na.omit(unique(oldvar))
      ## Generate fields
       for (l in levs) out <- paste0(out,'<li><span class="glyphicon glyphicon-move"> </span>&nbsp; <span class="level">',htmltools::htmlEscape(l),'</span></li>')
      out <- paste0(out, "</ol>")
      HTML(out)     
    }
    
    
    ## Run shiny app
    shiny::shinyApp(ui=bootstrapPage(
      header=tags$head(
        ## Custom CSS and JS
        tags$style(HTML(css.content)),
        tags$script(HTML(jquery.ui.content)),
        tags$script(HTML(js.content))),
 
      ## Page title
      div(class="container",
          div(class="row",
              headerPanel(gettext("Interactive levels ordering", domain="R-questionr"))),
          
          ## Display an alert, only on first launch for the current session
          if (show_alert) {
            div(class="row",
                div(class="col-md-12",
                    div(class="alert alert-warning alert-dismissible",
                        HTML('<button type="button" class="close" data-dismiss="alert" aria-label="Close"><span aria-hidden="true">&times;</span></button>'),
                        HTML(gettext("<strong>Warning :</strong> This interface doesn't do anything by itself. It only generates R code you'll have to copy/paste into your script and execute yourself.", domain="R-questionr"))
                    )))} else "",
          
          ## First panel : new variable name and recoding style
          div(class="row",
              div(class="col-md-12 well",
                  tags$form(
                            textInput("newvarname",
                                      gettext("<td>New variable : </td><td>", domain="R-questionr"), 
                                      oldvar_name)
                  ))),
          
          ## Second panel : recoding fields, dynamically generated
          div(class="row",
              div(class="col-md-12 well",
                  generate_levels_ol(oldvar)
              )),
          ## Main panel with tabs
          div(class="row",
              mainPanel(width=12,
                tabsetPanel(
                  ## Code tab
                  tabPanel(gettext("Code", domain="R-questionr"), htmlOutput("codeOut")),
                  ## Table check tab
                  tabPanel(gettext("Check", domain="R-questionr"),
                           HTML("<p class='header'></p>"),
                           tableOutput("tableOut"))
                ),
                
                ## Bottom buttons
                p(class='bottom-buttons',
                  tags$button(id="donebutton", type="button", class="btn action-button btn-success", 
                              onclick="javascript:window.close();", 
                              list(icon=icon("share")), 
                              gettext("Send code to console and exit", domain="R-questionr"))
                ),
                textOutput("done")
      
      )))),
      
      server=function(input, output) {
        
        ## Generate reordering code
        generate_code <- function(check=FALSE) {
          newvar_name <- input$newvarname
          ## if null, create temporary variable for check table
          if (check) dest_var <- ".iorder_tmp"
          ## else, format new variable for code
          else
            dest_var <- ifelse(grepl(" ", newvar_name),
                               sprintf('%s[,"%s"]', df_name, newvar_name),
                               sprintf('%s$%s', df_name, newvar_name))
          newlevels <- paste0(capture.output(dput(input$sortable)), collapse="")
          out <- gettextf("## Reordering %s", src_var, domain="R-questionr")
          if (src_var != dest_var) out <- paste0(out, sprintf(" into %s", dest_var))
          out <- paste0(out, sprintf("\n%s <- factor(%s, levels=", dest_var, src_var))
          out <- paste0(out, newlevels, ')')
          out
        }
        
        ## Generate the code in the interface
        output$codeOut <- renderText({
          ## Header
          header <- HTML(paste0(gettextf("<p class='header'>Reordering <tt>%s</tt> of class <tt>%s</tt>.</p>", oldvar_name, class(oldvar), domain="R-questionr")))
          ## Generate code
          out <- generate_code()
          ## Generated code syntax highlighting
          out <- paste(highr::hi_html(out), collapse="\n")
          ## Final paste
          out <- paste0(header, "<pre class='r'><code class='r' id='codeout'>",out,"</code></pre>")
          out
        })
        
        output$done <- renderText({
          ## Generate code
          out <- generate_code()
          ## If "Done" button is pressed, exit and cat generated code in the console
          if (input$donebutton > 0) {
            cat(gettext("\n-------- Start recoding code --------\n\n", domain="R-questionr"))
            cat(out)
            cat(gettext("\n--------- End recoding code ---------\n", domain="R-questionr"))
            shiny::stopApp()
          }
          return("")
        })
        
        ## Generate the check table
        output$tableOut <- renderTable({
          ## Generate the recoding code with a temporary variable
          code <- generate_code(check=TRUE)
          ## Eval generated code
          eval(parse(text=code))
          ## Display table
          tab <- freq(get(".iorder_tmp"))
          tab
        })
        
        ## Text fileds for levels, dynamically generated
        output$levelsInput <- renderUI({
          out <- "<ol class='sortable'>"
          ## List of levels
          if (is.factor(oldvar)) levs <- levels(oldvar)
          else levs <- na.omit(unique(oldvar))
          ## Generate fields
          for (l in levs) out <- paste0(out,'<li>',l,'</li>')
          out <- paste0(out, "</ol>")
          HTML(out)
        })
  })
    
}
