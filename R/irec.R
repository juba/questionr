##' Interactive recoding
##'
##' This function launches a shiny app in a web browser in order to do
##' interactive recoding of a categorical variable (character or factor).
##'
##' @param obj vector to recode or data frame to operate on
##' @param var_name if obj is a data frame, name of the column to be recoded, as a character string (possibly without quotes)
##' @return
##' The function launches a shiny app in the system web browser. The recoding code is returned in the console
##' when the app is closed with the "Done" button.
##' @author Julien Barnier <julien.barnier@@ens-lyon.fr>
##' @examples
##' \dontrun{data(hdv2003)
##' irec()
##' v <- sample(c("Red","Green","Blue"), 50, replace=TRUE)
##' irec(v)
##' irec(hdv2003, "qualif")
##' irec(hdv2003, sexe) ## this also works}
##' @import shiny
##' @import rstudioapi
##' @import miniUI
##' @importFrom highr hi_html
##' @importFrom htmltools htmlEscape
##' @export irec



irec <- function(obj = NULL, var_name = NULL) {
  
  run_as_addin <- rstudioapi::isAvailable() && rstudioapi::getActiveDocumentContext()$id != "#console"
  
  if (is.null(obj)) {
    obj_name <- NULL      
  }
  else {
    
    obj_name <- deparse(substitute(obj))
    if (inherits(obj, "tbl_df") || inherits(obj, "data.table")) obj <- as.data.frame(obj)
    
    ## Check if obj is a data frame or a vector
    if (!is.data.frame(obj) && !is.vector(obj) && !is.factor(obj)) {
      stop(sQuote(paste0(obj_name, ' must be a vector, a factor or a data frame.')))
    }
    
    ## If obj is a data.frame
    if (is.data.frame(obj)) {
      ## If var_name is not a character string, deparse it
      is_char <- FALSE
      is_null <- FALSE
      try({if(is.character(var_name)) is_char <- TRUE
      if(is.null(var_name)) is_null <- TRUE}, silent=TRUE)
      if (!is_char && !is_null) {
        var_name <- deparse(substitute(var_name))
      }
      ## Check if var_name is a column of robject
      if (!is.null(var_name) && !(var_name %in% names(obj))) {
        stop(sQuote(paste0(var_name, ' must be a column of ', obj_name, '.')))
      }
    }
  }
  
  
  ## Flag to display the alert on first time launch
  show_alert <- is.null(getOption("questionr_hide_alert"))
  if (show_alert) options(questionr_hide_alert=TRUE)  
  
  ## CSS file
  css.file <- system.file(file.path("shiny", "css", "ifuncs.css"), package = "questionr")
  css.content <- paste(readLines(css.file),collapse="\n")
  
  
  ## Gadget UI        
  ui <- miniUI::miniPage(
    
    ## Page title
    miniUI::gadgetTitleBar(gettext("Interactive recoding", domain = "R-questionr")),
    ## Custom CSS
    tags$style(HTML(css.content)),
    
    miniUI::miniTabstripPanel(
      miniUI::miniTabPanel(gettext("Variable and settings", domain = "R-questionr"), icon = icon("sliders"),
                           miniUI::miniContentPanel(

                             ## Display an alert, only on first launch for the current session
                             if (show_alert) {
                               div(class="alert alert-warning alert-dismissible",
                                   HTML('<button type="button" class="close" data-dismiss="alert" aria-label="Close"><span aria-hidden="true">&times;</span></button>'),
                                   HTML(gettext("<strong>Warning :</strong> This interface doesn't do anything by itself.", domain = "R-questionr")),
                                   if (run_as_addin) {
                                     HTML(gettext("It will generate R code, insert it in your current R script, and you'll have to run it yourself.", domain = "R-questionr"))
                                   } else {   
                                     HTML(gettext("It only generates R code you'll have to copy/paste into your script and run yourself.", domain = "R-questionr"))
                                   }
                               )},
                             
                             ## First panel : new variable name and recoding style
                             tags$h4(icon("columns"), gettext("Variable to be recoded", domain = "R-questionr")),
                             tags$form(class="well",
                                       div(class="row",
                                           div(class="col-sm-6", 
                                               selectizeInput("obj_name",
                                                              gettext("Data frame or vector to recode from", 
                                                                      domain = "R-questionr"),
                                                              choices = Filter(
                                                                function(x) {
                                                                  inherits(get(x, envir = sys.parent()), "data.frame") || 
                                                                    is.vector(get(x, envir = sys.parent()))
                                                                }, ls(.GlobalEnv)),
                                                              selected = obj_name, multiple = FALSE)),
                                           div(class="col-sm-6", "",
                                               uiOutput("varInput"))
                                       )),
                             uiOutput("nblevelsAlert"),
                             tags$h4(icon("sliders"), gettext("Recoding settings", domain = "R-questionr")),
                             tags$form(class="well",
                                       div(class="row",
                                           div(class="col-sm-4",
                                               uiOutput("newvarInput")),
                                           div(class="col-sm-4", 
                                               selectInput("recstyle", gettext("Recoding style", domain = "R-questionr"), 
                                                           c("Character - minimal"="charmin", "Character - complete"="charcomp"))),
                                           div(class="col-sm-4",
                                               selectInput("outconv", gettext("Output type", domain = "R-questionr"), 
                                                           c("Character"="character", "Factor"="factor", "Numeric"="numeric")))
                                       )))),
      
      ## Second panel : recoding fields, dynamically generated
      miniUI::miniTabPanel(gettext("Recoding", domain = "R-questionr"), icon = icon("wrench"),
                           miniUI::miniContentPanel(
                             tags$form(class="well",
                                       uiOutput("levelsInput")))),
      ## Code tab
      miniUI::miniTabPanel(gettext("Code and result", domain = "R-questionr"), icon = icon("code"), 
                           miniUI::miniContentPanel(
                             tags$h4(icon("code"), gettext("Code", domain = "R-questionr")),
                             htmlOutput("recodeOut"),
                             tags$h4(icon("table"), gettext("Check", domain = "R-questionr")),
                             ## Table check tab
                             p(class='header', 
                               gettext('Old variable as rows, new variable as columns.', 
                                       domain = "R-questionr")),
                             tableOutput("tableOut")))
    )
  )
  
  
  server <- function(input, output, session) {
    
    ## reactive first level object (vector or data frame)
    robj <- reactive({
      obj <- get(req(input$obj_name), envir = sys.parent())
      if (inherits(obj, "tbl_df") || inherits(obj, "data.table")) obj <- as.data.frame(obj)
      obj
    })
    
    ## reactive variable object (vector or data frame column)
    rvar <- reactive({
      invisible(input$obj_name)
      if (is.data.frame(robj())) {
        return(robj()[[req(input$var_name)]])
      }
      if (is.vector(robj()) || is.factor(robj())) {
        return(robj())
      }
      return(NULL)
    })
    
    ## Text fileds for levels, dynamically generated
    output$levelsInput <- renderUI({
      out <- "<table><tbody>"
      ## List of levels
      if (is.factor(rvar())) levs <- levels(rvar())
      else levs <- stats::na.omit(unique(rvar()))
      ## Add NA level if there is any NA value
      if (any(is.na(rvar()))) levs <- c(levs, NA)
      ## Generate fields
      for (l in levs) {
        out <- paste0(out, '<tr>')
        out <- paste0(out,'<td class="right vertical-align">',htmltools::htmlEscape(l),
                      '&nbsp;<span class="glyphicon glyphicon-arrow-right left-sep" aria-hidden="true"></span> &nbsp;</td>')
        id <- paste0("ireclev_", l)
        label <- l
        ## If the level is NA, replace by the NA value placeholder
        if (is.na(id)) {
          id <- "*irec_NA_id*"
          label <- "NA"
        }
        ## If the level is an empty string, replace by a placeholder
        if (id == "") {
          id <- "*irec_emptystr_id*"
          label <- ""
        }
        out <- paste0(out,'<td class="vertical-align">',textInput(id,"",label),'</td>')
        out <- paste0(out,'</tr>')
      }
      out <- paste0(out, "</tbody></table>")
      HTML(out)
    })
    
    ## If obj is a data frame, column to recode, dynamically generated
    output$varInput <- renderUI({
      if (is.data.frame(robj())) {
        selectizeInput("var_name",
                       gettext("Data frame column to recode", 
                               domain = "R-questionr"),
                       choices = names(robj()),
                       selected = var_name,
                       multiple = FALSE)
      }
    }) 
    
    ## Recoded variable name, dynamically generated
    output$newvarInput <- renderUI({
      new_name <- NULL
      if (is.data.frame(robj())) {
        new_name <- paste0(req(input$var_name), "_rec")
      } 
      if (is.vector(robj()) || is.factor(robj())) {
        new_name <- paste0(req(input$obj_name), "_rec")
      }
      if (!is.null(new_name)) {
        textInput("newvar_name", 
                  gettext("New variable name", domain = "R-questionr"), 
                  new_name)
      }
    })
    
    output$nblevelsAlert <- renderUI({
      if (length(unique(rvar())) > 50) {
        div(class = "alert alert-warning alert-dismissible",
            HTML('<button type="button" class="close" data-dismiss="alert" aria-label="Close"><span aria-hidden="true">&times;</span></button>'),
            HTML(gettext("<strong>Warning :</strong> The variable to be recoded has more than 50 levels.", domain = "R-questionr")))
      }
    })
    
    ## Reactive 
    src_var <- reactive({
      if (is.data.frame(robj())) {
        ## Formatted source variable name
        result <- ifelse(grepl(" ", req(input$var_name)),
                         sprintf('%s[,"%s"]', req(input$obj_name), req(input$var_name)),
                         sprintf('%s$%s', req(input$obj_name), req(input$var_name)))
      }
      if (is.vector(robj())) {
        result <- req(input$obj_name)
      }
      return(result)
    })
    
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
    generate_code_character <- function(dest_var, style) {
      ## Initial comment
      out <- gettextf("## Recoding %s into %s\n", src_var(), dest_var, domain = "R-questionr")
      ## Create new variable
      if (!is.character(rvar()))
        out <- paste0(out, sprintf("%s <- as.character(%s)\n", dest_var, src_var()))
      else
        out <- paste0(out, sprintf("%s <- %s\n", dest_var, src_var()))
      ## List levels
      if (is.factor(rvar())) levs <- levels(rvar())
      else {
        levs <- stats::na.omit(unique(rvar()))
        levs <- as.character(levs)
      }
      if (any(is.na(rvar()))) levs <- c(levs, NA)
      for (l in levs) {
        ## Special NA placeholder
        if (is.na(l)) l <- "*irec_NA_id*"
        ## Special empty string placeholder
        if (l == "") l <- "*irec_emptystr_id*"
        value <- get_value(input[[paste0("ireclev_", l)]])
        ## If minimal style, values unchanged are omitted
        if (style == "min" && !is.null(input[[paste0("ireclev_", l)]])) {
          if (l == input[[paste0("ireclev_", l)]]) next
          if (l == "*irec_NA_id*" && value == "NA") next
          if (l == "*irec_emptystr_id*" && value == "\"\"") next
        }
        ## NA values
        if (l == "*irec_NA_id*")
          out <- paste0(out, sprintf('%s[is.na(%s)] <- %s\n', dest_var, src_var(), value))
        ## Empty strings
        else if (l == "*irec_emptystr_id*")
          out <- paste0(out, sprintf('%s[%s==""] <- %s\n', dest_var, src_var(), value))
        ## Normal values
        else
          out <- paste0(out, sprintf('%s[%s == %s] <- %s\n', 
                                     dest_var, src_var(), utils::capture.output(dput(l)), value))
      }
      ## Optional output conversion
      if (input$outconv == "factor") out <- paste0(out, sprintf("%s <- factor(%s)\n", dest_var, dest_var))
      if (input$outconv == "numeric") out <- paste0(out, sprintf("%s <- as.numeric(%s)\n", dest_var, dest_var))          
      out
    }
    
    ## Call recoding code generation function based on style
    generate_code <- function(check=FALSE) {
      ## else, format new variable for code
      if (is.data.frame(robj())) {
        dest_var <- ifelse(grepl(" ", req(input$newvar_name)),
                           sprintf('%s[,"%s"]', req(input$obj_name), req(input$newvar_name)),
                           sprintf('%s$%s', req(input$obj_name), req(input$newvar_name)))
      }
      if (is.vector(robj()) || is.factor(robj())) {
        dest_var <- req(input$newvar_name)
      }
      ## if check, create temporary variable for check table
      if (check) dest_var <- ".irec_tmp"
      
      ## Invoke recoding code generation function
      recstyle <- input$recstyle
      if (recstyle == "charcomp") return(generate_code_character(dest_var, style = "comp"))
      if (recstyle == "charmin") return(generate_code_character(dest_var, style = "min"))
    }
    
    ## Generate the code in the interface
    output$recodeOut <- renderText({
      ## Header
      if (is.data.frame(robj())) {
        header <- HTML(gettextf("<p class='header'>Recoding <tt>%s</tt> from <tt>%s</tt> of class <tt>%s</tt>.</p>", req(input$var_name), req(input$obj_name), class(rvar()), domain = "R-questionr"))
      }
      if (is.vector(robj()) || is.factor(robj())) {
        header <- HTML(gettextf("<p class='header'>Recoding <tt>%s</tt> of class <tt>%s</tt>.</p>", req(input$obj_name), class(rvar()), domain = "R-questionr"))
      }
      ## Generate code
      out <- generate_code()
      ## Generated code syntax highlighting
      out <- paste(highr::hi_html(out), collapse = "\n")
      ## Final paste
      out <- paste0(header, "<pre class='r'><code class='r' id='codeout'>",out,"</code></pre>")
      out
    })
    
    
    # Handle the Done button being pressed.
    observeEvent(input$done, {
      ## Generate code
      out <- generate_code()
      if (run_as_addin) {
        rstudioapi::insertText(text = out)
      } else {
        out <- paste0(gettext("\n-------- Start recoding code --------\n\n", domain = "R-questionr"),
                      out,
                      gettext("\n--------- End recoding code ---------\n", domain = "R-questionr"))
        cat(out)
      }
      stopApp()
    })
    
    ## Generate the check table
    output$tableOut <- renderTable({
      ## Generate the recoding code with a temporary variable
      code <- generate_code(check = TRUE)
      ## Eval generated code
      eval(parse(text = code), envir = .GlobalEnv)
      ## Display table
      tab <- table(rvar(), get(".irec_tmp"), useNA = "always")
      rownames(tab)[is.na(rownames(tab))] <- "NA"
      colnames(tab)[is.na(colnames(tab))] <- "NA"
      tab
    })
    
    
    
    
  }
  
  
  runGadget(ui, server, viewer = dialogViewer("irec", width = 800, height = 700))
  
}
