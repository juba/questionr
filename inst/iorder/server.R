library(shiny)
library(highr)

## Global variables
## Original data frame name and object
df_name <- get(".questionr_iorder_df", .GlobalEnv)
df <- get(df_name)
## Variable to be recoded, name and object
oldvar_name <- get(".questionr_iorder_oldvar", .GlobalEnv)
oldvar <- df[,oldvar_name]
## Formatted source variable name
src_var <- ifelse(grepl(" ", oldvar_name),
                  sprintf('%s[,"%s"]', df_name, oldvar_name),
                  sprintf('%s$%s', df_name, oldvar_name))


shinyServer(function(input, output) {

    ## Format a value from a text input to code
    get_value <- function(val) {
        if (is.null(val)) return()
        if (val %in% c("NA", "TRUE", "FALSE")) return(val)
        ## TODO : find a way to escape double quotes
        val <- gsub('"', "", val)
        val <- paste0('"',val,'"')
        val
    }

    ## Generate recoding code
    generate_code_character <- function(newvar_name, dest_var, style) {
        ## Initial comment
        out <- sprintf("## Recoding %s into %s\n", src_var, dest_var)
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
            if (is.na(l)) l <- "*iorder_NA_id*"
            value <- get_value(input[[l]])
            ## If minimal style, values unchanged are omitted
            if (style=="min") {
                if (l==input[[l]]) next
                if (l=="*iorder_NA_id*" && value=="NA") next
            }
            ## Normal values
            if (l!="*iorder_NA_id*")
                out <- paste0(out, sprintf('%s[%s == "%s"] <- %s\n', dest_var, src_var, l, value))
            ## NA values
            else
                out <- paste0(out, sprintf('%s[is.na(%s)] <- %s\n', dest_var, src_var, value))
        }
        ## Optional factor conversion
        if (input$facconv) out <- paste0(out, sprintf("%s <- factor(%s)\n", dest_var, dest_var))
        out
    }

    ## Call recoding code generation function based on style
    generate_code <- function(newvar_name) {
        ## if null, create temporary variable for check table
        if (is.null(newvar_name)) dest_var <- "iorder_tmp"
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
    output$codeOut <- renderText({
        ## Header
        header <- HTML(paste0("<p class='header'>Reordering <tt>", oldvar_name, "</tt> from <tt>", df_name, "</tt> of class <tt>", class(oldvar), "</tt>.</p>"))
        ## Generate code
        out <- generate_code(input$newvarname)
        ## If "Done" button is pressed, exit and cat generated code in the console
        if (input$closebutton > 0) {
            cat("\n-------- Start reordering code --------\n\n")
            cat(out)
            cat("\n--------- End reordering code ---------\n")
            shiny::stopApp()
        }
        ## Generated code syntax highlighting
        out <- paste(hi_html(out), collapse="\n")
        ## Final paste
        out <- paste0(header, "<pre class='r'><code class='r' id='codeout'>",out,"</code></pre>")
        out
    })

    ## Generate the check table
    output$tableOut <- renderTable({
        ## Generate the recoding code with a temporary variable
        code <- generate_code(newvar_name=NULL)
        ## Eval generated code
        eval(parse(text=code))
        ## Display table
        tab <- table(oldvar, iorder_tmp, useNA="always")
        rownames(tab)[is.na(rownames(tab))] <- "NA"
        colnames(tab)[is.na(colnames(tab))] <- "NA"
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


