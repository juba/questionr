library(shiny)
library(highr)

df_name <- get(".questionr_irec_df", .GlobalEnv)
df <- get(df_name)
oldvar_name <- get(".questionr_irec_oldvar", .GlobalEnv)
oldvar <- df[,oldvar_name]
src_var <- ifelse(grepl(" ", oldvar_name),
                  sprintf('%s[,"%s"]', df_name, oldvar_name),
                  sprintf('%s$%s', df_name, oldvar_name))


shinyServer(function(input, output) {


    
    get_value <- function(val) {
        if (is.null(val)) return()
        if (val %in% c("NA", "TRUE", "FALSE")) return(val)
        ## TODO : find a way to escape double quotes
        val <- gsub('"', "", val)
        val <- paste0('"',val,'"')
        val
    }

    generate_code_character <- function(newvar_name, dest_var, style) {
        out <- sprintf("## Recoding %s into %s\n", src_var, dest_var)
        if (!is.character(oldvar))
            out <- paste0(out, sprintf("%s <- as.character(%s)\n", dest_var, src_var))
        else
            out <- paste0(out, sprintf("%s <- %s\n", dest_var, src_var))
        if (is.factor(oldvar)) levs <- levels(oldvar)
        else levs <- na.omit(unique(oldvar))
        if (any(is.na(oldvar))) levs <- c(levs, NA)
        for (l in levs) {
            if (is.na(l)) l <- "*irec_NA_id*"
            value <- get_value(input[[l]])
            if (style=="min") {
                if (l==input[[l]]) next
                if (l=="*irec_NA_id*" && value=="NA") next
            }
            if (l!="*irec_NA_id*")
                out <- paste0(out, sprintf('%s[%s == "%s"] <- %s\n', dest_var, src_var, l, value))
            else
                out <- paste0(out, sprintf('%s[is.na(%s)] <- %s\n', dest_var, src_var, value))
        }
        if (input$facconv) out <- paste0(out, sprintf("%s <- factor(%s)\n", dest_var, dest_var))
        out
    }

    generate_code <- function(newvar_name) {
        if (is.null(newvar_name)) dest_var <- "irec_tmp"
        else
            dest_var <- ifelse(grepl(" ", newvar_name),
                               sprintf('%s[,"%s"]', df_name, newvar_name),
                               sprintf('%s$%s', df_name, newvar_name))
        recstyle <- input$recstyle
        if(recstyle=="charcomp") return(generate_code_character(newvar_name, dest_var, style="comp"))
        if(recstyle=="charmin") return(generate_code_character(newvar_name, dest_var, style="min"))
    }


    
    output$recodeOut <- renderText({
        header <- HTML(paste0("<p style='font-size: 11px;'>Recoding <tt>", oldvar_name, "</tt> from <tt>", df_name, "</tt> of class <tt>", class(oldvar), "</tt>.</p>"))
        out <- generate_code(input$newvarname)
        if (input$closebutton > 0) {
            cat("\n-------- Start recoding code --------\n\n")
            cat(out)
            cat("\n--------- End recoding code ---------\n")
            shiny::stopApp()
        }
        ## for (l in levels(oldvar)) {
        ##     out <- paste0(out, 'for (i in c(1:10, TRUE)) ', l, " <- \"", input[[l]],"\"\n")
        ## }
        out <- paste(hi_html(out), collapse="\n")
        out <- paste0(header, "<pre class='r'><code class='r' id='codeout'>",out,"</code></pre>")
        out
    })

    output$tableOut <- renderTable({
        code <- generate_code(newvar_name=NULL)
        eval(parse(text=code))
        tab <- table(oldvar, irec_tmp, useNA="always")
        rownames(tab)[is.na(rownames(tab))] <- "NA"
        colnames(tab)[is.na(colnames(tab))] <- "NA"
        tab
    })
    
    output$levelsInput <- renderUI({
        out <- "<table>"
        if (is.factor(oldvar)) levs <- levels(oldvar)
        else levs <- na.omit(unique(oldvar))
        if (any(is.na(oldvar))) levs <- c(levs, NA)
        for (l in levs) {
            out <- paste0(out,'<tr><td style="text-align: right;">',l,'</td>')
            out <- paste0(out,'<td>&nbsp;<i class="icon-arrow-right"></i>&nbsp;</td>')
            out <- paste0(out,'<td>',textInput(ifelse(is.na(l), "*irec_NA_id*",l),"", ifelse(is.na(l), "NA",l)),'</td>')
            out <- paste0(out,'</tr>')
        }
        out <- paste0(out, "</table>")
        HTML(out)
    })
})
