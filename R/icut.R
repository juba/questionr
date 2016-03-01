##' Interactive conversion from numeric to factor
##'
##' This function launches a shiny app in a web browser in order to do
##' interactive conversion of a numeric variable into a categorical one.
##'
##' @param obj vector to recode or data frame to operate on
##' @param var_name if obj is a data frame, name of the column to be recoded, as a character string (possibly without quotes)
##' @return
##' The function launches a shiny app in the system web browser. The recoding code is returned in the console
##' when the app is closed with the "Done" button.
##' @author Julien Barnier <julien.barnier@@ens-lyon.fr>
##' @examples
##' \dontrun{data(hdv2003)
##' icut(hdv2003, "age")
##' irec(hdv2003, heures.tv) ## this also works}
##'
##' @import shiny
##' @import rstudioapi
##' @import miniUI
##' @importFrom highr hi_html
##' @importFrom htmltools htmlEscape
##' @export





icut <- function(obj = NULL, var_name = NULL) {

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
        miniUI::gadgetTitleBar(gettext("Interactive cutting", domain="R-questionr")),
        ## Custom CSS
        tags$style(ifunc_get_css()),

        miniUI::miniTabstripPanel(
          miniUI::miniTabPanel(
            gettext("Variable and settings", domain="R-questionr"), icon = icon("sliders"),
            miniUI::miniContentPanel(

              ifunc_show_alert(run_as_addin),

              ## First panel : new variable name
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
                                 is.numeric(get(x, envir = sys.parent()))
                             }, ls(.GlobalEnv)),
                           selected = obj_name, multiple = FALSE)),
                  column(6, uiOutput("varInput")))),
              tags$h4(icon("sliders"), gettext("Recoding settings", domain="R-questionr")),
              wellPanel(
                fluidRow(
                  column(4, uiOutput("newvarInput"))
                )),
              uiOutput("alreadyexistsAlert")
              )),

          ## Second panel : recoding fields, dynamically generated
          miniUI::miniTabPanel(
            gettext("Cutting", domain="R-questionr"), icon = icon("cut"),
            miniUI::miniContentPanel(
                fluidRow(
                    column(6,
                        htmlOutput("summary_table"),
                        selectizeInput("cutMethod", gettext('Cutting method',domain='R-questionr'), choices=c("Manual" = "fixed", "Standard deviation" = "sd", "Equal width" = "equal", "Pretty" = "pretty", "Quantile" = "quantile", "K-means" = "kmeans", "Hierarchical cluster" = "hclust", "Bagged clustering" = "bclust", "Fisher algorithm" = "fisher", "Jenks algorithm" = "jenks")),
                        uiOutput("ui"),
                        textInput("breaks", "Breaks"),
                        checkboxInput("right", HTML(gettext("Right-closed intervals (<tt>right</tt>)", domain="R-questionr")), TRUE),
                        checkboxInput("inclowest", HTML(gettext("Include extreme (<tt>include.lowest</tt>)", domain="R-questionr")), FALSE),
                        checkboxInput("addext", gettext("Append extreme values if necessary", domain="R-questionr"), FALSE)),
                    column(6,
                        wellPanel(plotOutput("histOut")))

              ))),
          ## Third panel : generated code and results checking
          miniUI::miniTabPanel(
            gettext("Code and result", domain="R-questionr"), icon = icon("code"),
            miniUI::miniContentPanel(
              tags$h4(icon("code"), gettext("Code", domain="R-questionr")),
              htmlOutput("codeOut"),
              tags$h4(icon("table"), gettext("Check - table", domain="R-questionr")),
              fluidRow(
                      ## Table check tab
                      p(class = 'header'),
                      tableOutput("tableOut")),
              tags$h4(icon("bar-chart"), gettext("Check - barplot", domain="R-questionr")),
              fluidRow(
                      plotOutput("barOut"))))

        )
      )


      server <- function(input, output, session) {

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
            if (is.numeric(robj())) {
              return(robj())
            }
            return(NULL)
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

          ## If obj is a data frame, column to recode, dynamically generated
          output$varInput <- renderUI({
            if (is.data.frame(robj())) {
              choices <- names(robj())[unlist(lapply(robj(), is.numeric))]
              selectizeInput("var_name",
                             gettext("Data frame column to recode", domain="R-questionr"),
                             choices = choices,
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

          output$summary_table <- renderText({
            v <- rvar()
            out <- gettextf("<p>Statistics of <tt>%s</tt> :</p>", src_var(), domain="R-questionr")
            out <- paste0(out, "<table class='table table-bordered table-condensed' id='sumtable'>")
            out <- paste0(out, "<thead><tr>")
            out <- paste0(out, "<th>Min</th><th>1st quartile</th><th>Median</th><th>Mean</th><th>3rd quartile</th><th>Max</th><th>NA</th>")
            out <- paste0(out, "</tr></thead><tbody><tr>")
            out <- paste0(out, sprintf("<td>%s</td><td>%s</td><td>%s</td><td>%s</td><td>%s</td><td>%s</td><td>%s</td>",
                                       round(min(v, na.rm=TRUE), 4),
                                       round(stats::quantile(v, prob=0.25, na.rm=TRUE), 4),
                                       round(stats::median(v, na.rm=TRUE), 4),
                                       round(mean(v, na.rm=TRUE), 4),
                                       round(stats::quantile(v, prob=0.75, na.rm=TRUE), 4),
                                       round(max(v, na.rm=TRUE), 4),
                                       sum(is.na(v))))
            out <- paste0(out, "</tr></tbody></table>")
            HTML(out)
          })


          output$ui <- renderUI({
            if (req(input$cutMethod) == "fixed") return()
            numericInput(inputId="nb_breaks", label=gettext("Breaks number", domain="R-questionr"), value=6, min=2, step=1)
          })

          observe(if (input$cutMethod != "fixed") {
            nb_breaks <- reactive({
              if (is.null(req(input$nb_breaks))) return(2)
              if (is.na(req(input$nb_breaks))) return(2)
              if (req(input$nb_breaks) < 2) return(2)
              return(input$nb_breaks)
            })
            updateTextInput(session, "breaks", value=classInt::classIntervals(rvar(), n=ifelse(is.null(nb_breaks()), 6, nb_breaks()), style=req(input$cutMethod))$brks)
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
          
        get_breaks <- function(b, compute=FALSE) {
          if (b=="") return(NULL)
          b <- gsub(", *$", "",b)
          b <- paste0("c(",b,")")
          breaks <- sort(unique(eval(parse(text=b))))
          ## Code taken directly from `cut` source code
          if (length(breaks) == 1L && compute) {
            if (is.na(breaks) || breaks < 2L)
              stop("invalid number of intervals")
            nb <- as.integer(breaks + 1)
            dx <- diff(rx <- range(rvar(), na.rm = TRUE))
            if (dx == 0)
              dx <- abs(rx[1L])
            breaks <- seq.int(rx[1L] - dx/1000, rx[2L] + dx/1000,
                              length.out = nb)
          }
          if (length(breaks) > 1 && input$addext) {
            if (min(breaks, na.rm=TRUE) > min(rvar(), na.rm=TRUE)) breaks <- c(min(rvar(), na.rm=TRUE), breaks)
            if (max(breaks, na.rm=TRUE) < max(rvar(), na.rm=TRUE)) breaks <- c(breaks, max(rvar(), na.rm=TRUE))
          }
          breaks
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
            if (check) dest_var <- ".icut_tmp"

          out <- sprintf(gettextf("## Cutting %s into %s\n", src_var(), dest_var, domain="R-questionr"))
          out <- paste0(out, sprintf("%s <- cut(%s, include.lowest=%s,  right=%s,\n", dest_var, src_var(), input$inclowest, input$right))
          breaks <- paste0(utils::capture.output(dput(get_breaks(input$breaks))), collapse="")
          out <- paste0(out, paste0(rep(" ",nchar(dest_var)+8),collapse=""),sprintf("breaks=%s)\n", breaks))
          out
        }


        output$histOut <- renderPlot({
          graphics::hist(rvar(), col="#bbd8e9", border="white", main=gettext("Original histogram", domain="R-questionr"), xlab=src_var())
          breaks <- get_breaks(input$breaks, compute=TRUE)
          for (b in breaks) graphics::abline(v=b, col="#dd1144", lwd=1, lty=2)
        })


        ## Generate the code in the interface
        output$codeOut <- renderText({
          ## Generate code
          out <- generate_code()
          ## Generated code syntax highlighting
          out <- paste(highr::hi_html(out), collapse="\n")
          ## Final paste
          out <- paste0("<pre class='r'><code class='r' id='codeout'>",out,"</code></pre>")
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
          code <- generate_code(check=TRUE)
          ## Eval generated code
          eval(parse(text=code), envir = .GlobalEnv)
          ## Display table
          tab <- freq(get(".icut_tmp"))
          tab
        })

        ## Generate the barplot
        output$barOut <- renderPlot({
          ## Generate the recoding code with a temporary variable
          code <- generate_code(check=TRUE)
          ## Eval generated code
          eval(parse(text=code), envir = .GlobalEnv)
          ## Display table
          graphics::plot(get(".icut_tmp"), col="#bbd8e9", border="white")
        })
    }

    runGadget(ui, server, viewer = dialogViewer("icut", width = 800, height = 700))
}
