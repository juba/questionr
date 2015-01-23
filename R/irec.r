##' Interactive recoding
##'
##' This function launches a shiny app in a web browser in order to do
##' interactive recoding of a categorical variable (character or factor).
##'
##' @param dfobject data frame to operate on, as an object or a character string
##' @param oldvar name of the variable to be recoded, as a character string (possibly without quotes)
##' @return
##' The function launches a shiny app in the system web browser. The recoding code is returned in the console
##' when the app is closed with the "Done" button.
##' @author Julien Barnier <julien.barnier@@ens-lyon.fr>
##' @examples
##' \dontrun{data(hdv2003)
##' irec(hdv2003, "qualif")
##' irec(hdv2003, sexe) ## this also works}
##' @import shiny
##' @importFrom highr hi_html
##' @importFrom htmltools htmlEscape
##' @export irec

irec <- function(dfobject, oldvar) {
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
    df_name <- dfobject
    df <- get(df_name)
    if (inherits(df, "tbl_df") || inherits(df, "data.table")) df <- as.data.frame(df)
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
        
    ## Run shiny app
    shiny::shinyApp(ui=bootstrapPage(
      header=tags$head(
        ## Custom CSS
        tags$style(HTML(css.content))),
      
      ## Page title
      div(class="container",
          div(class="row",
              headerPanel(gettext("Interactive recoding", domain="R-questionr"))),
          
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
              div(class="col-sm-12",
                  tags$form(class="well",
                            div(class="row",
                               div(class="col-sm-4", 
                                   textInput("newvarname", gettext("New variable", domain="R-questionr"), 
                                             paste0(oldvar_name,".rec"))),
                               div(class="col-sm-4", 
                                   selectInput("recstyle", gettext("Recoding style", domain="R-questionr"), 
                                               c("Character - complete"="charcomp", "Character - minimal"="charmin"))),
                               div(class="col-sm-3",                           
                                  checkboxInput("facconv", gettext("Convert to factor", domain="R-questionr"), FALSE))
                  )))),
          
          ## Second panel : recoding fields, dynamically generated
          div(class="row",
              div(class="col-md-12",
                  tags$form(class="well",
                            uiOutput("levelsInput")))),
          ## Main panel with tabs
          mainPanel(width=12,
            tabsetPanel(
              ## Code tab
              tabPanel(gettext("Code", domain="R-questionr"), htmlOutput("recodeOut")),
              ## Table check tab
              tabPanel(gettext("Check", domain="R-questionr"),
                       p(class='header', gettext('Old variable as rows, new variable as columns.', domain="R-questionr")),
                       tableOutput("tableOut"))
            ),
            
            ## Bottom buttons
            p(class='bottom-buttons',
              tags$button(id="donebutton", type="button", class="btn action-button btn-success", 
                          onclick="javascript:window.close();", 
                          list(icon=icon("ok", lib="glyphicon")), 
                          gettext("Send code to console and exit", domain="R-questionr"))
              ),
            textOutput("done")
          )
          
      )),
      
      server=function(input, output) {
        
        ## Format a value from a text input to code
        get_value <- function(val) {
          if (is.null(val)) return()
          if (val %in% c("NA", "TRUE", "FALSE")) return(val)
          ## Encoding conversion for Windows
          Encoding(val) <- "UTF-8"
          val <- enc2native(val)
          val <- capture.output(dput(val))
          val
        }
        
        ## Generate recoding code
        generate_code_character <- function(newvar_name, dest_var, style) {
          ## Initial comment
          out <- gettextf("## Recoding %s into %s\n", src_var, dest_var, domain="R-questionr")
          ## Create new variable
          if (!is.character(oldvar))
            out <- paste0(out, sprintf("%s <- as.character(%s)\n", dest_var, src_var))
          else
            out <- paste0(out, sprintf("%s <- %s\n", dest_var, src_var))
          ## List levels
          if (is.factor(oldvar)) levs <- levels(oldvar)
          else levs <- na.omit(unique(oldvar))
          if (any(is.na(oldvar))) levs <- c(levs, NA)
          for (l in levs) {
            ## Special NA placeholder
            if (is.na(l)) l <- "*irec_NA_id*"
            ## Special empty string placeholder
            if (l=="") l <- "*irec_emptystr_id*"
            value <- get_value(input[[l]])
            ## If minimal style, values unchanged are omitted
            if (style=="min") {
              if (l==input[[l]]) next
              if (l=="*irec_NA_id*" && value=="NA") next
              if (l=="*irec_emptystr_id*" && value=="\"\"") next
            }
            ## NA values
            if (l=="*irec_NA_id*")
              out <- paste0(out, sprintf('%s[is.na(%s)] <- %s\n', dest_var, src_var, value))
            ## Empty strings
            else if (l=="*irec_emptystr_id*")
              out <- paste0(out, sprintf('%s[%s==""] <- %s\n', dest_var, src_var, value))
            ## Normal values
            else
              out <- paste0(out, sprintf('%s[%s == %s] <- %s\n', dest_var, src_var, capture.output(dput(l)), value))
          }
          ## Optional factor conversion
          if (input$facconv) out <- paste0(out, sprintf("%s <- factor(%s)\n", dest_var, dest_var))
          out
        }
        
        ## Call recoding code generation function based on style
        generate_code <- function(check=FALSE) {
          newvar_name <- input$newvarname
          ## if null, create temporary variable for check table
          if (check) dest_var <- ".irec_tmp"
          ## else, format new variable for code
          else
            dest_var <- ifelse(grepl(" ", newvar_name),
                               sprintf('%s[,"%s"]', df_name, newvar_name),
                               sprintf('%s$%s', df_name, newvar_name))
          ## Invoke recoding code generation function
          recstyle <- input$recstyle
          if(recstyle=="charcomp") return(generate_code_character(newvar_name, dest_var, style="comp"))
          if(recstyle=="charmin") return(generate_code_character(newvar_name, dest_var, style="min"))
        }
        
        ## Generate the code in the interface
        output$recodeOut <- renderText({
          ## Header
          header <- HTML(gettextf("<p class='header'>Recoding <tt>%s</tt> from <tt>%s</tt> of class <tt>%s</tt>.</p>", oldvar_name, df_name, class(oldvar), domain="R-questionr"))
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
          tab <- table(oldvar, get(".irec_tmp"), useNA="always")
          rownames(tab)[is.na(rownames(tab))] <- "NA"
          colnames(tab)[is.na(colnames(tab))] <- "NA"
          tab
        })
        
        
        ## Text fileds for levels, dynamically generated
        output$levelsInput <- renderUI({
          out <- "<table><tbody>"
          ## List of levels
          if (is.factor(oldvar)) levs <- levels(oldvar)
          else levs <- na.omit(unique(oldvar))
          ## Add NA level if there is any NA value
          if (any(is.na(oldvar))) levs <- c(levs, NA)
          ## Generate fields
          for (l in levs) {
            out <- paste0(out, '<tr>')
            out <- paste0(out,'<td class="right vertical-align">',htmltools::htmlEscape(l),
                              '&nbsp;<span class="glyphicon glyphicon-arrow-right left-sep" aria-hidden="true"></span> &nbsp;</td>')
            id <- l
            label <- l
            ## If the level is NA, replace by the NA value placeholder
            if (is.na(id)) {
              id <- "*irec_NA_id*"
              label <- "NA"
            }
            ## If the level is an empty string, replace by a placeholder
            if (id=="") {
              id <- "*irec_emptystr_id*"
              label <- ""
            }
            out <- paste0(out,'<td class="vertical-align">',textInput(id,"",label),'</td>')
            out <- paste0(out,'</tr>')
          }
          out <- paste0(out, "</tbody></table>")
          HTML(out)
        })
      }
      
        
    )
        
}
