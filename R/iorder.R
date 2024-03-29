#' Interactive reordering of factor levels
#'
#' This function launches a shiny app in a web browser in order to do
#' interactive reordering of the levels of a categorical variable (character
#' or factor).
#'
#' @param obj vector to recode or data frame to operate on
#' @param var_name if obj is a data frame, name of the column to be recoded,
#' as a character string possibly without quotes)
#' @details
#' The generated convert the variable into a factor, as only those allow for
#' levels ordering.
#' @return
#' The function launches a shiny app in the system web browser. The reordering
#' code is returned in he console when the app is closed with the "Done" button.
#' @examples
#' \dontrun{
#' data(hdv2003)
#' iorder(hdv2003, "qualif")
#' }
#' @import shiny
#' @import rstudioapi
#' @import miniUI
#' @importFrom highr hi_html
#' @export



iorder <- function(obj = NULL, var_name = NULL) {

  # Deactivate styler cache to avoid blocking message at
  # first launch
  styler::cache_deactivate(verbose = FALSE)

  recoding_styles <- c(
    "factor" = "factor",
    "fct_relevel (forcats)" = "forcats"
  )
  selected_recoding_style <- "factor"
  ## If forcats is loaded
  if (exists("fct_relevel")) {
    selected_recoding_style <- "forcats"
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
      try(
        {
          obj <- get(obj_name, envir = .GlobalEnv)
        },
        silent = TRUE
      )
    } else {
      obj_name <- deparse(substitute(obj))
    }
    ## If first arg is of the form d$x
    if (grepl("\\$", obj_name)) {
      s <- strsplit(obj_name, "\\$")
      obj_name <- gsub("^\\s*", "", s[[1]][1])
      var_name <- gsub("\\s*$", "", s[[1]][2])
      var_name <- gsub("`", "", var_name)
      obj <- get(obj_name, envir = .GlobalEnv)
    }
    if (inherits(obj, "tbl_df") || inherits(obj, "data.table")) obj <- as.data.frame(obj)

    ## Check if obj is a data frame or a vector
    if (!is.data.frame(obj) && !is.vector(obj) && !is.factor(obj)) {
      stop(sQuote(paste0(obj_name, " must be a vector, a factor or a data frame.")))
    }

    ## If obj is a data.frame
    if (is.data.frame(obj)) {
      ## If var_name is not a character string, deparse it
      is_char <- FALSE
      is_null <- FALSE
      try(
        {
          if (is.character(var_name)) is_char <- TRUE
          if (is.null(var_name)) is_null <- TRUE
        },
        silent = TRUE
      )
      if (!is_char && !is_null) {
        var_name <- deparse(substitute(var_name))
      }
      ## Check if var_name is a column of robject
      if (!is.null(var_name) && !(var_name %in% names(obj))) {
        stop(sQuote(paste0(var_name, " must be a column of ", obj_name, ".")))
      }
    }
  }

  ## JS files
  jquery.ui.file <- system.file(file.path("shiny", "js", "jquery-ui.js"), package = "questionr")
  jquery.ui.content <- paste(readLines(jquery.ui.file), collapse = "\n")
  js.file <- system.file(file.path("shiny", "js", "iorder.js"), package = "questionr")
  js.content <- paste(readLines(js.file), collapse = "\n")

  ## Gadget UI
  ui <- miniUI::miniPage(
    tags$head(
      ## Custom JS
      tags$script(HTML(jquery.ui.content)),
      tags$script(HTML(js.content)),
      ## Custom CSS
      tags$style(ifunc_get_css())
    ),
    ## Page title
    miniUI::gadgetTitleBar(gettext("Interactive levels ordering", domain = "R-questionr")),
    miniUI::miniTabstripPanel(
      miniUI::miniTabPanel(
        gettext("Variable and settings", domain = "R-questionr"),
        icon = icon("sliders-h"),
        miniUI::miniContentPanel(
          ifunc_show_alert(run_as_addin),

          ## First panel : new variable name
          tags$h4(icon("columns"), gettext("Variable to be recoded", domain = "R-questionr")),
          wellPanel(
            fluidRow(
              column(
                6,
                selectizeInput(
                  "obj_name",
                  gettext("Data frame or vector to recode from", domain = "R-questionr"),
                  choices = Filter(
                    function(x) {
                      inherits(get(x, envir = .GlobalEnv), "data.frame") ||
                        is.vector(get(x, envir = .GlobalEnv)) ||
                        is.factor(get(x, envir = .GlobalEnv))
                    }, ls(.GlobalEnv)
                  ),
                  selected = obj_name, multiple = FALSE
                )
              ),
              column(6, uiOutput("varInput"))
            )
          ),
          uiOutput("nblevelsAlert"),
          tags$h4(icon("sliders-h"), gettext("Recoding settings", domain = "R-questionr")),
          wellPanel(
            fluidRow(
              column(4, uiOutput("newvarInput")),
              column(4, selectInput("recstyle", gettext("Recoding style", domain = "R-questionr"),
                recoding_styles,
                selected = selected_recoding_style
              )),
            )
          ),
          uiOutput("alreadyexistsAlert"),
          uiOutput("loadedforcatsAlert")
        )
      ),

      ## Second panel : recoding fields, dynamically generated
      miniUI::miniTabPanel(
        gettext("Ordering", domain = "R-questionr"),
        icon = icon("arrows-alt"),
        miniUI::miniContentPanel(
          wellPanel(htmlOutput("levelsInput"))
        )
      ),
      ## Third panel : generated code and results checking
      miniUI::miniTabPanel(
        gettext("Code and result", domain = "R-questionr"),
        icon = icon("code"),
        miniUI::miniContentPanel(
          tags$h4(icon("code"), gettext("Code", domain = "R-questionr")),
          htmlOutput("codeOut"),
          tags$h4(icon("table"), gettext("Check", domain = "R-questionr")),
          ## Table check tab
          p(
            class = "header",
            gettext("Old variable as rows, new variable as columns.", domain = "R-questionr")
          ),
          tableOutput("tableOut")
        )
      )
    )
  )




  server <- function(input, output) {

    ## reactive first level object (vector or data frame)
    robj <- reactive({
      obj <- get(req(input$obj_name), envir = .GlobalEnv)
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

    ## Reactive name of the source variable
    src_var <- reactive({
      if (is.data.frame(robj())) {
        ## Formatted source variable name
        var_name <- req(input$var_name)
        if (make.names(var_name) != var_name) {
          var_name <- paste0('`', var_name, '`')
        }
        result <- sprintf("%s$%s", req(input$obj_name), var_name)        
      }
      if (is.vector(robj()) || is.factor(robj())) {
        result <- req(input$obj_name)
      }
      return(result)
    })

    ## Sortable list of levels, dynamically generated
    output$levelsInput <- renderText({
      out <- "<ol id='sortable' class='sortable'>"
      ## List of levels
      if (is.factor(rvar())) {
        levs <- levels(rvar())
      } else {
        levs <- sort(stats::na.omit(unique(rvar())))
      }
      ## Generate fields
      for (l in levs) {
        out <- paste0(
          out,
          '<li><span class="glyphicon glyphicon-move"> </span>&nbsp; <span class="level">',
          htmltools::htmlEscape(l),
          "</span></li>"
        )
      }
      out <- paste0(out, "</ol>")
      HTML(out)
    })


    ## If obj is a data frame, column to recode, dynamically generated
    output$varInput <- renderUI({
      if (is.data.frame(robj())) {
        selectizeInput("var_name",
          gettext("Data frame column to recode", domain = "R-questionr"),
          choices = names(robj()),
          selected = var_name,
          multiple = FALSE
        )
      }
    })

    ## Recoded variable name, dynamically generated
    output$newvarInput <- renderUI({
      new_name <- NULL
      if (is.data.frame(robj())) {
        new_name <- req(input$var_name)
      }
      if (is.vector(robj()) || is.factor(robj())) {
        new_name <- req(input$obj_name)
      }
      if (!is.null(new_name)) {
        textInput(
          "newvar_name",
          gettext("New variable name", domain = "R-questionr"),
          new_name
        )
      }
    })

    output$nblevelsAlert <- renderUI({
      if (length(unique(rvar())) > 50) {
        div(
          class = "alert alert-warning alert-dismissible",
          HTML('<button type="button" class="close" data-dismiss="alert" aria-label="Close"><span aria-hidden="true">&times;</span></button>'),
          HTML(gettext("<strong>Warning :</strong> The variable to be recoded has more than 50 levels.", domain = "R-questionr"))
        )
      }
    })

    output$loadedforcatsAlert <- renderUI({
      if (input$recstyle == "forcats" && !exists("fct_recode")) {
        div(
          class = "alert alert-warning alert-dismissible",
          HTML('<button type="button" class="close" data-dismiss="alert" aria-label="Close"><span aria-hidden="true">&times;</span></button>'),
          HTML(gettext("<strong>Warning :</strong> The <tt>forcats</tt> package must be installed and loaded for the generated code to be run.", domain = "R-questionr"))
        )
      }
    })

    output$alreadyexistsAlert <- renderUI({
      exists <- FALSE
      if (is.data.frame(robj()) && req(input$newvar_name) %in% names(robj())) {
        exists <- TRUE
        orig_name <- req(input$var_name)
      }
      if (is.vector(robj()) && exists(req(input$newvar_name), envir = .GlobalEnv)) {
        exists <- TRUE
        orig_name <- req(input$obj_name)
      }
      if (exists && req(input$newvar_name) != orig_name) {
        div(
          class = "alert alert-warning alert-dismissible",
          HTML('<button type="button" class="close" data-dismiss="alert" aria-label="Close"><span aria-hidden="true">&times;</span></button>'),
          HTML(gettext("<strong>Warning :</strong> This new variable already exists.", domain = "R-questionr"))
        )
      }
    })


    ## Code generation for base::factor
    generate_code_factor <- function(dest_var) {
      newlevels <- paste0(utils::capture.output(dput(input$sortable)), collapse = "\n")
      sprintf("%s <- factor(%s,\n levels=%s)", dest_var, src_var(), newlevels)
    }

    ## Code generation for forcats::fct_relevel
    generate_code_forcats <- function(dest_var) {
      out <- sprintf("%s <- %s %%>%%\n  ", dest_var, src_var())
      if (is.numeric(rvar())) {
          out <- paste0(out, "as.character() %>%\n  ")
      }
      newlevels <- paste0(utils::capture.output(dput(input$sortable)), collapse = "\n")
      newlevels <- gsub("(^c\\(|\\)$)", "", newlevels)
      paste0(out, sprintf("fct_relevel(\n    %s\n  )", newlevels))
    }

    ## Generate reordering code
    generate_code <- function(check = FALSE) {
      if (is.data.frame(robj())) {
        dest_var <- req(input$newvar_name)
        if (make.names(dest_var) != dest_var) {
          dest_var <- paste0('`', dest_var, '`')
        }
        dest_var <- sprintf("%s$%s", req(input$obj_name), dest_var)        
      }
      if (is.vector(robj()) || is.factor(robj())) {
        dest_var <- req(input$newvar_name)
      }
      ## if check, create temporary variable for check table
      if (check) dest_var <- ".iorder_tmp"

      out <- gettextf("## Reordering %s", src_var(), domain = "R-questionr")
      if (src_var() != dest_var) out <- paste0(out, gettextf(" into %s", dest_var, domain = "R-questionr"))
      out <- paste0(out, "\n")

      ## Invoke recoding code generation function
      recstyle <- input$recstyle
      if (recstyle == "factor") out <- paste0(out, generate_code_factor(dest_var))
      if (recstyle == "forcats") out <- paste0(out, generate_code_forcats(dest_var))

      return(out)
    }

    ## Generate the code in the interface
    output$codeOut <- renderText({
      ## Header
      if (is.data.frame(robj())) {
        header <- HTML(gettextf("<p class='header'>Reordering <tt>%s</tt> from <tt>%s</tt> of class <tt>%s</tt>.</p>",
          req(input$var_name), req(input$obj_name), class(rvar()),
          domain = "R-questionr"
        ))
      }
      if (is.vector(robj()) || is.factor(robj())) {
        header <- HTML(gettextf("<p class='header'>Reordering <tt>%s</tt> of class <tt>%s</tt>.</p>",
          req(input$obj_name), class(rvar()),
          domain = "R-questionr"
        ))
      }
      ## Generate code
      out <- generate_code()
      out <- styler::style_text(out)
      ## Generated code syntax highlighting
      out <- paste(highr::hi_html(out), collapse = "\n")
      ## Final paste
      out <- paste0(header, "<pre class='r'><code class='r' id='codeout'>", out, "</code></pre>")
      out
    })

    # Handle the Done button being pressed.
    observeEvent(input$done, {
      ## Generate code
      out <- generate_code()
      out <- styler::style_text(out)
      out <- paste(out, collapse = "\n")
      if (run_as_addin) {
        rstudioapi::insertText(text = out)
      }
     out <- paste0(
        gettext("\n-------- Start recoding code --------\n\n", domain = "R-questionr"),
        out,
        gettext("\n--------- End recoding code ---------\n", domain = "R-questionr")
      )
      cat(out)
      stopApp()
    })

    # Handle the Cancel button being pressed.
    observeEvent(input$cancel, {
      invisible(stopApp())
    })

    ## Generate the check table
    output$tableOut <- renderTable(
      {
        ## Generate the recoding code with a temporary variable
        code <- generate_code(check = TRUE)
        if (!exists("fct_relevel") && input$recstyle == "forcats") {
          return(NULL)
        }
        ## Eval generated code
        eval(parse(text = code), envir = .GlobalEnv)
        ## Display table
        tab <- freq(get(".iorder_tmp"))
        tab
      },
      rownames = TRUE
    )
  }

  runGadget(ui, server, viewer = dialogViewer("iorder", width = 800, height = 700))
}