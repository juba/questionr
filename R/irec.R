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
    if (!is.data.frame(get(dfobject, envir = sys.parent()))) stop(sQuote(paste0(dfobject, ' must be a data frame.')))
    ## If oldvar is not a character string, deparse it
    is_char <- FALSE
    try(if(is.character(oldvar)) is_char <- TRUE, silent=TRUE)
    if (!is_char) oldvar <- deparse(substitute(oldvar))
    ## Check if oldvar is a column of dfobject
    if (!(oldvar %in% names(get(dfobject, envir = sys.parent())))) stop(sQuote(paste0(oldvar, ' must be a column of ', dfobject, '.')))    

    ## Global variables
    df_name <- dfobject
    df <- get(df_name, envir = sys.parent())
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

    
    ## Gadget UI        
    ui <- miniUI::miniPage(
      miniUI::gadgetTitleBar(gettext("Interactive recoding", domain="R-questionr")),
      tags$style(HTML(css.content)),
      ## Page title
      miniUI::miniTabstripPanel(
        miniUI::miniTabPanel(gettext("Recoding", domain="R-questionr"), icon = icon("wrench"),
                             miniUI::miniContentPanel(
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
                                             paste0(oldvar_name,"_rec"))),
                               div(class="col-sm-4", 
                                   selectInput("recstyle", gettext("Recoding style", domain="R-questionr"), 
                                               c("Character - minimal"="charmin", "Character - complete"="charcomp"))),
                               div(class="col-sm-4",
                                   selectInput("outconv", gettext("Output type", domain="R-questionr"), 
                                               c("Character"="character", "Factor"="factor", "Numeric"="numeric")))
                  )))),
          
          ## Second panel : recoding fields, dynamically generated
                               tags$form(class="well",
                                         uiOutput("levelsInput")))),
              ## Code tab
              miniUI::miniTabPanel(gettext("Code and result", domain="R-questionr"), icon = icon("code"), 
                                   miniUI::miniContentPanel(
                                     htmlOutput("recodeOut"),
              ## Table check tab
                        p(class='header', gettext('Old variable as rows, new variable as columns.', domain="R-questionr")),
                       tableOutput("tableOut")))
            )
          )
          
      
      server <- function(input, output, session) {
        
        ## Format a value from a text input to code
        get_value <- function(val) {
          if (is.null(val)) return()
          if (val %in% c("NA", "TRUE", "FALSE")) return(val)
          val <- gsub("^\\s+|\\s+$", "", val)
          ## Encoding conversion for Windows
          Encoding(val) <- "UTF-8"
          val <- enc2native(val)
          val <- utils::capture.output(dput(val))
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
          else {
            levs <- stats::na.omit(unique(oldvar))
            levs <- as.character(levs)
          }
          if (any(is.na(oldvar))) levs <- c(levs, NA)
          for (l in levs) {
            ## Special NA placeholder
            if (is.na(l)) l <- "*irec_NA_id*"
            ## Special empty string placeholder
            if (l=="") l <- "*irec_emptystr_id*"
            value <- get_value(input[[l]])
            ## If minimal style, values unchanged are omitted
            if (style=="min" && !is.null(input[[l]])) {
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
              out <- paste0(out, sprintf('%s[%s == %s] <- %s\n', dest_var, src_var, utils::capture.output(dput(l)), value))
          }
          ## Optional output conversion
          if (input$outconv == "factor") out <- paste0(out, sprintf("%s <- factor(%s)\n", dest_var, dest_var))
          if (input$outconv == "numeric") out <- paste0(out, sprintf("%s <- as.numeric(%s)\n", dest_var, dest_var))          
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
        
        
        # Handle the Done button being pressed.
        observeEvent(input$done, {
          ## Generate code
          out <- generate_code()
          out <- paste0(gettext("\n-------- Start recoding code --------\n\n", domain="R-questionr"),
                        out,
                        gettext("\n--------- End recoding code ---------\n", domain="R-questionr"))
          cat(out)
          stopApp()
        })
        
        ## Generate the check table
        output$tableOut <- renderTable({
          ## Generate the recoding code with a temporary variable
          code <- generate_code(check=TRUE)
          ## Eval generated code
          eval(parse(text=code), envir = .GlobalEnv)
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
          else levs <- stats::na.omit(unique(oldvar))
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
      
        
    runGadget(ui, server, viewer = dialogViewer("irec", width = 800, height = 700))
        
}
