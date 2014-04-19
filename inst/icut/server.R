library(shiny)
library(highr)
library(classInt)

## Global variables
## Original data frame name and object
df_name <- getOption("questionr_icut_df")
df <- get(df_name)
## Variable to be recoded, name and object
oldvar_name <- getOption("questionr_icut_oldvar")
oldvar <- df[,oldvar_name]
## Formatted source variable name
src_var <- ifelse(grepl(" ", oldvar_name),
                  sprintf('%s[,"%s"]', df_name, oldvar_name),
                  sprintf('%s$%s', df_name, oldvar_name))


shinyServer(function(input, output, session) {
    
    observe({
      output$ui <- renderUI({
        if (input$cutMethod == "fixed") return()
          numericInput(inputId="nb_breaks", label="Breaks number", value=6, min=2, step=1)
        })
      if (input$cutMethod != "fixed") {
        updateTextInput(session, "breaks", value=classIntervals(oldvar, n=ifelse(is.null(input$nb_breaks), 6, input$nb_breaks), style=input$cutMethod)$brks)
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
            dx <- diff(rx <- range(oldvar, na.rm = TRUE))
            if (dx == 0) 
                dx <- abs(rx[1L])
            breaks <- seq.int(rx[1L] - dx/1000, rx[2L] + dx/1000, 
                              length.out = nb)
        }
        if (length(breaks) > 1 && input$addext) {
            if (min(breaks, na.rm=TRUE) > min(oldvar, na.rm=TRUE)) breaks <- c(min(oldvar, na.rm=TRUE), breaks)
            if (max(breaks, na.rm=TRUE) < max(oldvar, na.rm=TRUE)) breaks <- c(breaks, max(oldvar, na.rm=TRUE))
        }
        breaks
    }
 

    ## Call recoding code generation function based on style
    generate_code <- function(newvar_name) {
        ## if null, create temporary variable for check table
        if (is.null(newvar_name)) dest_var <- ".icut_tmp"
        ## else, format new variable for code
        else
            dest_var <- ifelse(grepl(" ", newvar_name),
                               sprintf('%s[,"%s"]', df_name, newvar_name),
                               sprintf('%s$%s', df_name, newvar_name))
        out <- sprintf("## Cutting %s into %s\n", oldvar_name, newvar_name)
        out <- paste0(out, sprintf("%s <- cut(%s, include.lowest=%s,  right=%s,\n", dest_var, src_var, input$inclowest, input$right))
        breaks <- paste0(capture.output(dput(get_breaks(input$breaks))), collapse="")
        out <- paste0(out, paste0(rep(" ",nchar(dest_var)+8),collapse=""),sprintf("breaks=%s)\n", breaks))
        out
    }


    output$histOut <- renderPlot({
        hist(oldvar, col="#bbd8e9", border="white", main="Original histogram", xlab=oldvar_name)
        breaks <- get_breaks(input$breaks, compute=TRUE)
        for (b in breaks) abline(v=b, col="#dd1144", lwd=1, lty=2)
    })
    
    
    ## Generate the code in the interface
    output$codeOut <- renderText({
        ## Generate code
        out <- generate_code(input$newvarname)
        ## If "Done" button is pressed, exit and cat generated code in the console
        if (input$closebutton > 0) {
            cat("\n-------- Start recoding code --------\n\n")
            cat(out)
            cat("\n--------- End recoding code ---------\n")
            shiny::stopApp()
        }
        ## Generated code syntax highlighting
        out <- paste(hi_html(out), collapse="\n")
        ## Final paste
        out <- paste0("<pre class='r'><code class='r' id='codeout'>",out,"</code></pre>")
        out
    })

    ## Generate the check table
    output$tableOut <- renderTable({
        ## Generate the recoding code with a temporary variable
        code <- generate_code(newvar_name=NULL)
        ## Eval generated code
        eval(parse(text=code))
        ## Display table
        tab <- freq(.icut_tmp)
        tab
    })

    ## Generate the barplot
    output$barOut <- renderPlot({
        ## Generate the recoding code with a temporary variable
        code <- generate_code(newvar_name=NULL)
        ## Eval generated code
        eval(parse(text=code))
        ## Display table
        plot(.icut_tmp, col="#bbd8e9", border="white")
    })


})


