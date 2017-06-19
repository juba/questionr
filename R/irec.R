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

  recoding_styles <- c("Character - minimal" = "charmin", 
                       "Character - complete" = "charcomp",
                       "fct_recode (forcats)" = "forcats",
                       "recode (dplyr)" = "recode")
  selected_recoding_style <- "charmin"
  selected_outconv <- "character"

  ## If forcats is loaded
  if (isNamespaceLoaded("forcats")) {
    selected_recoding_style <- "forcats"
    selected_outconv <- "factor"
  }

  ## If dplyr is loaded
  if (isNamespaceLoaded("dplyr")) {
    selected_recoding_style <- "recode"
    selected_outconv <- "character"
  }

  run_as_addin <- ifunc_run_as_addin()

  if (is.null(obj)) {
    if (ifunc_run_as_addin()) {
      context <- rstudioapi::getActiveDocumentContext()
      obj <- context$selection[[1]]$text
      if (obj == "") obj <- NULL
    }
    obj_name <- NULL
    var_name <- NULL
  }
  if (!is.null(obj)) {
    ## If first arg is a string
    if (is.character(obj) && length(obj) == 1) {
      obj_name <- obj
      try({
        obj <- get(obj_name, envir = sys.parent())
      }, silent = TRUE)
    }
    else {
      obj_name <- deparse(substitute(obj))
    }
    ## If first arg is of the form d$x
    if (grepl("\\$", obj_name)) {
        s <- strsplit(obj_name, "\\$")
        obj_name <- gsub("^\\s*", "", s[[1]][1])
        var_name <- gsub("\\s*$", "", s[[1]][2])
        var_name <- gsub("`", "", var_name)
        obj <- get(obj_name, envir = sys.parent())
    }
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
      try({
        if (is.character(var_name)) is_char <- TRUE
        if (is.null(var_name)) is_null <- TRUE
      }, silent = TRUE)
      if (!is_char && !is_null) {
        var_name <- deparse(substitute(var_name))
      }
      ## Check if var_name is a column of robject
      if (!is.null(var_name) && !(var_name %in% names(obj))) {
        stop(sQuote(paste0(var_name, ' must be a column of ', obj_name, '.')))
      }
    }
  }


  ## Gadget UI
  ui <- miniUI::miniPage(

    ## Page title
    miniUI::gadgetTitleBar(gettext("Interactive recoding", domain="R-questionr")),
    ## Custom CSS
    tags$style(ifunc_get_css()),

    miniUI::miniTabstripPanel(
      miniUI::miniTabPanel(
        value = "settings",
        gettext("Variable and settings", domain="R-questionr"), icon = icon("sliders"),
        miniUI::miniContentPanel(

          ifunc_show_alert(run_as_addin),

          ## First panel : new variable name and recoding style
          tags$h4(icon("columns"), gettext("Variable to be recoded", domain="R-questionr")),
          wellPanel(
            fluidRow(
              column(6,
                     selectizeInput(
                       "obj_name",
                       gettext("Data frame or vector to recode from", domain="R-questionr"),
                       choices = Filter(
                         function(x) {
                           inherits(get(x, envir = sys.parent()), "data.frame") ||
                             is.vector(get(x, envir = sys.parent())) ||
                             is.factor(get(x, envir = sys.parent()))
                         }, ls(.GlobalEnv)),
                       selected = obj_name, multiple = FALSE)),
              column(6, uiOutput("varInput")))),
          uiOutput("nblevelsAlert"),
          tags$h4(icon("sliders"), gettext("Recoding settings", domain="R-questionr")),
          wellPanel(
            fluidRow(
              column(4, uiOutput("newvarInput")),
              column(4,selectInput("recstyle", gettext("Recoding style", domain="R-questionr"),
                                   recoding_styles,
                                   selected = selected_recoding_style)),
              column(4, uiOutput("outconvUI"))
            )),
          uiOutput("alreadyexistsAlert"),
          uiOutput("loadedforcatsAlert"),
          uiOutput("loadeddplyrAlert")
          )),

      ## Second panel : recoding fields, dynamically generated
      miniUI::miniTabPanel(
        value = "recoding",
        gettext("Recoding"), icon = icon("wrench"),
        miniUI::miniContentPanel(
          wellPanel(uiOutput("levelsInput")))),
      ## Third panel : generated code and results checking
      miniUI::miniTabPanel(
        value = "code",
        gettext("Code and result"), icon = icon("code"),
        miniUI::miniContentPanel(
          tags$h4(icon("code"), gettext("Code", domain="R-questionr")),
          htmlOutput("recodeOut"),
          tags$h4(icon("table"), gettext("Check", domain="R-questionr")),
          ## Table check tab
          p(class = 'header',
            gettext('Old variable as rows, new variable as columns.', domain="R-questionr")),
          tableOutput("tableOut")))
    )
  )


  server <- function(input, output, session) {

    if (!is.null(obj_name)) {
      updateSelectizeInput(session, "obj_name", selected = obj_name)
    }
    if (!is.null(var_name)) {
      updateSelectizeInput(session, "var_name", selected = var_name)
    }
    
    ## reactive first level object (vector or data frame)
    robj <- reactive({
      obj <- get(req(input$obj_name %||% obj_name), envir = .GlobalEnv)
      if (inherits(obj, "tbl_df") || inherits(obj, "data.table")) obj <- as.data.frame(obj)
      obj
    })

    ## reactive variable object (vector or data frame column)
    rvar <- reactive({
      invisible(input$obj_name)
      if (is.data.frame(robj())) {
        return(robj()[[req(input$var_name %||% var_name)]])
      }
      if (is.vector(robj()) || is.factor(robj())) {
        return(robj())
      }
      return(NULL)
    })

    output$outconvUI <- renderUI({
      choices <- c("Character" = "character", "Factor" = "factor", "Numeric" = "numeric")
      if (input$recstyle == "forcats")
        choices <- c("Factor" = "factor", "Character" = "character", "Numeric" = "numeric")
      selectInput("outconv", gettext("Output type", domain="R-questionr"), choices)
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
        label <- l
        l <- gsub(":", "_", l)
        id <- paste0("ireclev_", l)
        ## If the level is NA
        if (id == "ireclev_NA") {
          label <- "NA"
        }
        ## If the level is an empty string
        if (id == "ireclev_") {
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
                       gettext("Data frame column to recode", domain="R-questionr"),
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
                  gettext("New variable name", domain="R-questionr"),
                  new_name)
      }
    })

    output$nblevelsAlert <- renderUI({
      if (length(unique(rvar())) > 50) {
        div(class = "alert alert-warning alert-dismissible",
            HTML('<button type="button" class="close" data-dismiss="alert" aria-label="Close"><span aria-hidden="true">&times;</span></button>'),
            HTML(gettext("<strong>Warning :</strong> The variable to be recoded has more than 50 levels.", domain="R-questionr")))
      }
    })
    
    output$loadedforcatsAlert <- renderUI({
      if (input$recstyle == "forcats" && !requireNamespace("forcats", quietly = TRUE)) {
        div(class = "alert alert-warning alert-dismissible",
            HTML('<button type="button" class="close" data-dismiss="alert" aria-label="Close"><span aria-hidden="true">&times;</span></button>'),
            HTML(gettext("<strong>Warning :</strong> The <tt>forcats</tt> package must be installed and loaded for the generated code to be run.", domain="R-questionr")))
      }
    })
    
    output$loadeddplyrAlert <- renderUI({
      if (input$recstyle == "recode" && !requireNamespace("dplyr", quietly = TRUE)) {
        div(class = "alert alert-warning alert-dismissible",
            HTML('<button type="button" class="close" data-dismiss="alert" aria-label="Close"><span aria-hidden="true">&times;</span></button>'),
            HTML(gettext("<strong>Warning :</strong> The <tt>dplyr</tt> package must be installed and loaded for the generated code to be run.", domain="R-questionr")))
      }
    })
    
    
    output$alreadyexistsAlert <- renderUI({
      exists <- FALSE
      if (is.data.frame(robj()) && req(input$newvar_name) %in% names(robj())) 
        exists <- TRUE
      if (is.vector(robj()) && exists(req(input$newvar_name), envir=.GlobalEnv))
        exists <- TRUE
      if (exists) {
        div(class = "alert alert-warning alert-dismissible",
            HTML('<button type="button" class="close" data-dismiss="alert" aria-label="Close"><span aria-hidden="true">&times;</span></button>'),
            HTML(gettext("<strong>Warning :</strong> This new variable already exists.", domain="R-questionr")))
      }
    })

    ## Reactive source variable name
    src_var <- reactive({
      if (is.data.frame(robj())) {
        ## Formatted source variable name
        result <- ifelse(grepl(" ", req(input$var_name)),
                         sprintf('%s[,"%s"]', req(input$obj_name), req(input$var_name)),
                         sprintf('%s$%s', req(input$obj_name), req(input$var_name)))
      }
      if (is.vector(robj()) || is.factor(robj())) {
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

    ## Generate character style recoding code
    generate_code_character <- function(dest_var, style) {
      out <- ""
      ## List levels
      if (is.factor(rvar())) levs <- levels(rvar())
      else {
        levs <- stats::na.omit(unique(rvar()))
        levs <- as.character(levs)
      }
      if (any(is.na(rvar()))) levs <- c(levs, NA)
      for (l in levs) {
        l_clean <- gsub(":", "_", l)
        value <- get_value(input[[paste0("ireclev_", l_clean)]])
        ## If minimal style, values unchanged are omitted
        if (style == "min" && !is.null(input[[paste0("ireclev_", l_clean)]])) {
          if (is.na(l) && value == "NA") next
          if (!is.na(l)) {
            if (l == input[[paste0("ireclev_", l_clean)]]) next
            if (l == "" && value == "\"\"") next
          }
        }
        ## NA values
        if (is.na(l)) {
          out <- paste0(out, sprintf('%s[is.na(%s)] <- %s\n', dest_var, src_var(), value))
        }
        ## Empty strings
        if (!is.na(l) && l == "") {
          out <- paste0(out, sprintf('%s[%s == ""] <- %s\n', dest_var, src_var(), value))
        }
        ## Normal values
        if (!is.na(l) && l != "") {
          out <- paste0(out, sprintf('%s[%s == %s] <- %s\n',
                                     dest_var, src_var(), 
                                     utils::capture.output(dput(l)), 
                                     value))
        }
      }
      ## Optional output conversion
      if (input$outconv == "factor") out <- paste0(out, sprintf("%s <- factor(%s)\n", dest_var, dest_var))
      if (input$outconv == "numeric") out <- paste0(out, sprintf("%s <- as.numeric(%s)\n", dest_var, dest_var))
      ## If any recoding has been done
      if (out != "") {
          ## Create new variable
          if (!is.character(rvar())) {
              out <- paste0(sprintf("%s <- as.character(%s)\n", dest_var, src_var()), out)
          } else if (dest_var != src_var()) {
              out <- paste0(sprintf("%s <- %s\n", dest_var, src_var()), out)
          }
          ## Initial comment
         if (dest_var != src_var()) {
          out <- paste0(gettextf("## Recoding %s into %s\n", src_var(), dest_var, domain = "R-questionr"), out)
         } else {
           out <- paste0(gettextf("## Recoding %s\n", src_var(), domain = "R-questionr"), out)
         }
      }
      out
    }

    ## Generate recoding code for forcats
    generate_code_forcats <- function(dest_var) {
        out <- ""
        out_na <- ""
      ## List levels
      if (is.factor(rvar())) levs <- levels(rvar())
      else {
        levs <- stats::na.omit(unique(rvar()))
        levs <- as.character(levs)
      }
      if (any(is.na(rvar()))) levs <- c(levs, NA)
      out <- ""
      na_recode <- ""
      has_recode_to_na <- FALSE
      for (l in levs) {
        l_clean <- gsub(":", "_", l)
        value <- get_value(input[[paste0("ireclev_", l_clean)]])
        ## Values unchanged are omitted
        if (is.na(l) && value == "NA") next
        if (!is.na(l)) {
          if (l == input[[paste0("ireclev_", l_clean)]]) next
          if (l == "" && value == "\"\"") next
        }
        ## We can't recode to empty string
        if (value == '""') { value <- '"-"' }
        ## NA values
        if (is.na(l)) { na_recode <- value }
        ## Recode to NA   
        if (value == "NA") {
          value <- "NULL"
          has_recode_to_na <- TRUE
        }
        ## Normal values
        if (!is.na(l)) {
          out <- paste0(out, sprintf(',\n               %s = %s',
                                     value,
                                     utils::capture.output(dput(l))))
        }
      }
      
      ## fct_recode      
      if (out != "") {
        source <- src_var()
        if (na_recode != "" && has_recode_to_na) { source <- dest_var }
        out <- paste0(sprintf("%s <- fct_recode(%s", dest_var, source), out)
        out <- paste0(out, ")\n")
      }
      
      ## fct_explicit_na
      if (na_recode != "") {
        if (out != "" && !has_recode_to_na) {
          out <- paste0(out, sprintf('%s <- fct_explicit_na(%s, %s)\n', dest_var, dest_var, na_recode))
        } else {
          out <- paste0(sprintf('%s <- fct_explicit_na(%s, %s)\n', dest_var, src_var(), na_recode), out)
        }
      }
      
      ## Optional output conversion
      if (input$outconv == "character") out <- paste0(out, sprintf("%s <- as.character(%s)\n", dest_var, dest_var))
      if (input$outconv == "numeric") out <- paste0(out, sprintf("%s <- as.numeric(as.character(%s))\n", dest_var, dest_var))

      ## Initial comment
      comment <- ""
      if (out != "" || na_recode != "") {
        if (dest_var != src_var()) {
          comment <- gettextf("## Recoding %s into %s\n", src_var(), dest_var, domain = "R-questionr")
        } else {
          comment <- gettextf("## Recoding %s\n", src_var(), domain = "R-questionr")
        }
      }
      out <- paste0(comment, out)  
      out
    }
    

    ## Generate recoding code for dplyr::recode
    generate_code_recode <- function(dest_var) {
      out <- ""
      recode_NA <- FALSE
      ## List levels
      if (is.factor(rvar())) levs <- levels(rvar())
      else {
        levs <- stats::na.omit(unique(rvar()))
        levs <- as.character(levs)
      }
      if (any(is.na(rvar()))) levs <- c(levs, NA)
      out <- ""
      for (l in levs) {
        l_clean <- gsub(":", "_", l)
        value <- get_value(input[[paste0("ireclev_", l_clean)]])
        ## Values unchanged are omitted
        if (is.na(l) && value == "NA") next
        if (!is.na(l)) {
          if (l == input[[paste0("ireclev_", l_clean)]]) next
          if (l == "" && value == "\"\"") next
        }
        ## We can't recode to empty string
        if (value == '""') { value <- '"-"' }
        ## Recode to NA   
        if (value == "NA") {
          value <- "NA_character_"
        }
        ## Normal values
        if (!is.na(l)) {
          out <- paste0(out, sprintf(',\n               %s = %s',
                                     utils::capture.output(dput(l)),
                                     value))
        } 
        ## NA values
        else {
          recode_NA <- TRUE
          out <- paste0(out, sprintf(',\n               .missing = %s',
                                     value))
        }
      }
      
      ## recode      
      if (out != "") {
        source <- src_var()
        if (recode_NA && is.factor(rvar())) source <- dest_var
        out <- paste0(sprintf("%s <- recode(%s", dest_var, source), out)
        out <- paste0(out, ")\n")
        ## .missing is not supported for factors
        if (recode_NA && is.factor(rvar())) {
          out <- paste0(sprintf("%s <- as.character(%s)\n", dest_var, src_var()), out)
        }
      }
      
      ## Optional output conversion
      output_is_factor <- is.factor(rvar()) && !recode_NA
      if (!output_is_factor && input$outconv == "factor") out <- paste0(out, sprintf("%s <- factor(%s)\n", dest_var, dest_var))
      if (output_is_factor && input$outconv == "character") out <- paste0(out, sprintf("%s <- as.character(%s)\n", dest_var, dest_var))
      if (input$outconv == "numeric") {
        if (output_is_factor) {
          out <- paste0(out, sprintf("%s <- as.numeric(as.character(%s))\n", dest_var, dest_var))
        } else {
          out <- paste0(out, sprintf("%s <- as.numeric(%s)\n", dest_var, dest_var))
        }
      }
      
      ## Initial comment
      comment <- ""
      if (dest_var != src_var()) {
        comment <- gettextf("## Recoding %s into %s\n", src_var(), dest_var, domain = "R-questionr")
      } else {
        comment <- gettextf("## Recoding %s\n", src_var(), domain = "R-questionr")
      }
      out <- paste0(comment, out)  
      out
    }
    
    
    
    
    ## Call recoding code generation function based on style
    generate_code <- function(check=FALSE) {
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
      if (recstyle == "forcats") return(generate_code_forcats(dest_var))
      if (recstyle == "recode") return(generate_code_recode(dest_var))
    }

    ## Generate the code in the interface
    output$recodeOut <- renderText({
      ## Header
      if (is.data.frame(robj())) {
        header <- HTML(gettextf("<p class='header'>Recoding <tt>%s</tt> from <tt>%s</tt> of class <tt>%s</tt>.</p>",
                   req(input$var_name), req(input$obj_name), class(rvar()), domain = "R-questionr"))
      }
      if (is.vector(robj()) || is.factor(robj())) {
        header <- HTML(gettextf("<p class='header'>Recoding <tt>%s</tt> of class <tt>%s</tt>.</p>",
                                req(input$obj_name), class(rvar()), domain = "R-questionr"))
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
        out <- paste0(gettext("\n-------- Start recoding code --------\n\n", domain="R-questionr"),
                      out,
                      gettext("\n--------- End recoding code ---------\n", domain="R-questionr"))
        cat(out)
      }
      stopApp()
    })
    
    # Handle the Cancel button being pressed.
    observeEvent(input$cancel, { 
      invisible(stopApp()) 
    })

    ## Generate the check table
    output$tableOut <- renderTable({
      ## Generate the recoding code with a temporary variable
      code <- generate_code(check = TRUE)
      if (code != "") {
          ## Eval generated code
          eval(parse(text = code), envir = .GlobalEnv)
          ## Display table
          tab <- table(rvar(), get(".irec_tmp"), useNA = "always")
          rownames(tab)[is.na(rownames(tab))] <- "NA"
          colnames(tab)[is.na(colnames(tab))] <- "NA"
      } else {
          ## Abort, return nothing
          req(FALSE)
      }
      as.data.frame.matrix(tab)
    }, rownames = TRUE)

  }


  runGadget(ui, server, viewer = dialogViewer("irec", width = 800, height = 700))

}
