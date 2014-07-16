library(shiny)
library(highr)

## Global variables
## Original data frame name and object
df_name <- getOption("questionr_irec_df")
df <- get(df_name)
## Variable to be recoded, name and object
oldvar_name <- getOption("questionr_irec_oldvar")
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
        ## Encoding conversion for Windows
        Encoding(val) <- "UTF-8"
        val <- enc2native(val)
        val <- capture.output(dput(val))
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
    generate_code <- function(newvar_name) {
        ## if null, create temporary variable for check table
        if (is.null(newvar_name)) dest_var <- ".irec_tmp"
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
        header <- HTML(paste0("<p class='header'>Recoding <tt>", oldvar_name, "</tt> from <tt>", df_name, "</tt> of class <tt>", class(oldvar), "</tt>.</p>"))
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
        tab <- table(oldvar, .irec_tmp, useNA="always")
        rownames(tab)[is.na(rownames(tab))] <- "NA"
        colnames(tab)[is.na(colnames(tab))] <- "NA"
        tab
    })

    ## Text fileds for levels, dynamically generated
    output$levelsInput <- renderUI({
        out <- "<table>"
        ## List of levels
        if (is.factor(oldvar)) levs <- levels(oldvar)
        else levs <- na.omit(unique(oldvar))
        ## Add NA level if there is any NA value
        if (any(is.na(oldvar))) levs <- c(levs, NA)
        ## Generate fields
        for (l in levs) {
            out <- paste0(out,'<tr><td class="right">',htmltools::htmlEscape(l),'</td>')
            out <- paste0(out,'<td>&nbsp;<i class="icon-arrow-right"></i>&nbsp;</td>')
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
            out <- paste0(out,'<td>',textInput(id,"",label),'</td>')
            out <- paste0(out,'</tr>')
        }
        out <- paste0(out, "</table>")
        HTML(out)
    })
})


