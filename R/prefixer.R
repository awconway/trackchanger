#' @title Shiny Gadget to interactively prefix function in a script
#'
#' @description Open a script, load package(s) used in that script, then launch this addin via RStudio menu.
#' It will propose to prefix function with their package (if a function is in several packages, a choice can be made),
#' validate the suggested prefix or skip to the next function.
#'
#' @export
#'
#' @importFrom miniUI miniPage miniContentPanel
#' @importFrom rstudioapi getSourceEditorContext insertText
#' @importFrom shiny tags fillPage uiOutput splitLayout actionButton
#' tagList icon actionLink reactiveValues observeEvent showModal
#' modalDialog modalButton selectizeInput removeModal renderUI HTML
#' isolate radioButtons removeUI insertUI stopApp runGadget paneViewer
#' @importFrom shinyWidgets confirmSweetAlert
#' @importFrom stringr str_replace str_remove_all
#'
#' @examples
#' \dontrun{
#'
#' prefixer()
#'
#' }
prefixer <- function() {
  
  script <- rstudioapi::getSourceEditorContext()$contents
  if (sum(nchar(script), na.rm = TRUE) == 0) {
    warning("It seems there are nothing to prefix...")
    return(invisible())
  }
  
  
  
  script_funs <- get_script_funs(script)
  
  if (nrow(script_funs) == 0) {
    warning("It seems there are nothing to prefix...")
    return(invisible())
  }

  
  #to_load <- get_unloaded_packages(script = script)
  
  ui <- miniPage(
    tags$head(tags$style(".highlight-context {color: red !important;}")),
    tags$head(tags$style("ins, del{
  text-decoration: none;
  display: inline-block;
  padding: 0 .3em;
}
ins {
  background: #83d5a8;
  height: auto;
  border-radius: .3em;
  display: inline; 
  -webkit-box-decoration-break: clone;
  -o-box-decoration-break: clone;
  box-decoration-break: clone;
  margin-left: .07em;
  margin-right: .07em;
}
del {
  background: rgba(231, 76, 60,.5);
  cursor: no-drop;
  position: relative;
  top: .2em;
  height: 1.31em;
  width: .4em;
  line-height: 1.35;
  overflow: hidden;
  color: transparent;
  margin: -.4em .07em -.05em .07em;
  padding: 0 0 .1em 0;
  -webkit-box-decoration-break: clone;
  -o-box-decoration-break: clone;
  box-decoration-break: clone;
  border-radius: .3em;
  transition: padding-left ease .3s, padding-right ease .3s, color ease .15s;
}
del:hover {
  width: auto;
  color: #962e22;
  display: inline;
  padding: 0 .35em 0em 0.35em;
  line-height: unset;
  position: unset;
  top: unset;
  height: unset;
  transition: padding-left ease .3s, padding-right ease .3s, color ease .7s;
}")),
    tags$div(
      class = "gadget-title", style = "background-color: rgb(16,34,70);",
      tags$h1("trackchanger", style = "font-weight: bold; color: #FFF;"),
      tags$button(
        id="cancel", type="button", "Cancel",
        class="btn btn-default btn-sm action-button pull-right"
      )
    ),
    miniContentPanel(
      fillPage(
        # padding = 10, title = "Prefixer",
        tags$br(),
       uiOutput(outputId = "ui_fun"),
        
        
        tags$div(id = "button-close-placeholder")
      )
    ),
    tags$div(
      style = "padding: 15px;",
      splitLayout(
        id = "buttons-navigate",
        actionButton(
          inputId = "add_prefix", label = "Accept", width = "100%",
          icon = tags$i(tags$b("::", style = "font-style: normal;"))
        ),
        actionButton(
          inputId = "reject", label = "Reject", width = "100%",
          icon = tags$i(tags$b("::", style = "font-style: normal;"))
        ),
        tagList(
          actionButton(
            inputId = "skip", label = "Skip", width = "100%",
            icon = icon("forward")
          ), 
          
          
          tags$br(),
          #actionLink(
          #  inputId = "skip_all", label = "Skip all occurences",
          #  icon = icon("fast-forward"), style = "font-size: 80%;"
          #)
        )
      ), 
      #tags$span(
       # style = "font-size: 70%;",
      #  "If your script use unloaded packages, functions from these packages won't be prefixed. You can load packages by ",
      #  actionLink(inputId = "load_packages", label = "clicking here"), "."
      #)
    )
  )
  
  server <- function(input, output, session) {
    
    script_funs_r <- reactiveValues(x = script_funs)

    # if (length(to_load) > 0) {
    #   confirmSweetAlert(
    #     session = session, inputId = "confirm_load", title = "Unloaded packages",
    #     text = paste0(
    #       "These packages seems to be used but are not loaded : ", paste(to_load, collapse = ", "),
    #       ". Do you want to load them ?"
    #     )
    #   )
    # }

    # observeEvent(input$load_packages, {
    #   showModal(modalDialog(
    #     easyClose = FALSE, footer = modalButton("Cancel"),
    #     selectizeInput(
    #       inputId = "packages_to_load",
    #       label = "Packages to load:",
    #       choices = .packages(all.available = TRUE),
    #       multiple = TRUE, options = list(plugins = list("remove_button")),
    #       width = "100%"
    #     ),
    #     actionButton(
    #       inputId = "load_more_packages", label = "Load selected packages",
    #       width = "100%", class = "btn-primary"
    #     )
    #   ))
    # })

    # observeEvent(input$load_more_packages, {
    #   invisible(lapply(input$packages_to_load, library, character.only = TRUE))
    #   script_funs <- get_script_funs(script)
    #   script_funs_r$x <- script_funs
    #   # Update selection
    #   set_selection(script_funs, 1)
    #   # Update fun list
    #   fun$actual_fun <- script_funs$funs[1]
    #   fun$suggested_fun <- paste(script_funs$package[1], script_funs$funs[1], sep = "::")
    #   fun$fun_context <- script_funs$fun_context[1]
    #   fun$multiple_occurence <- sum(script_funs$funs[1] == script_funs$funs) > 1
    #   fun$multiple_package <- unique(script_funs$package[script_funs$funs == script_funs$funs[1]])
    #   fun$max <- nrow(script_funs)
    #   removeModal()
    # })
    #
    # observeEvent(input$confirm_load, {
    #   if (input$confirm_load) {
    #     invisible(lapply(to_load, library, character.only = TRUE))
    #     # Update script funs table
    #     script_funs <- get_script_funs(script)
    #     script_funs_r$x <- script_funs
    #     # Update selection
    #     set_selection(script_funs, 1)
    #     # Update fun list
    #     fun$actual_fun <- script_funs$funs[1]
    #     fun$suggested_fun <- paste(script_funs$package[1], script_funs$funs[1], sep = "::")
    #     fun$fun_context <- script_funs$fun_context[1]
    #     fun$multiple_occurence <- sum(script_funs$funs[1] == script_funs$funs) > 1
    #     fun$multiple_package <- unique(script_funs$package[script_funs$funs == script_funs$funs[1]])
    #     fun$max <- nrow(script_funs)
    #   }
    # })

    fun <- reactiveValues(
      counter = 1,
      # actual_fun = script_funs$funs[1],
      actual_fun = str_remove_all(script_funs$funs[1], pattern = "<.*?>"),#removes ins and del tags
      # suggested_fun = paste(script_funs$package[1], script_funs$funs[1], sep = "::"),
      # suggested_fun = str_remove_all(script_funs$funs[1], pattern = "<.*?>"),#removes ins and del tags
      fun_context = script_funs$fun_context[1],
      # multiple_occurence = sum(script_funs$funs[1] == script_funs$funs) > 1,
      # multiple_package = unique(script_funs$package[script_funs$funs == script_funs$funs[1]]),
      max = nrow(script_funs),
      change_type = script_funs$package[1]
      )

    set_selection(script_funs, 1)

    output$ui_fun <- renderUI({
      actual_fun <- fun$actual_fun
      change_type <- fun$change_type
      # suggested_fun <- fun$suggested_fun
      fun_context <- HTML(fun$fun_context)

      # print(reactiveValuesToList(isolate(fun)))
      if (isolate(fun$counter) <= fun$max) {
        if (script_funs_r$x$package[fun$counter] =="insertion"){
          text_insertion <- "Inserted text: "
        } else if(script_funs_r$x$package[fun$counter]=="deletion"){
          text_insertion <- "Deleteted text: "
        }
        # if (length(fun$multiple_package) == 1) {
          tagList(
            tags$div(tags$b(text_insertion), tags$pre(actual_fun)),
            tags$div(tags$b("Context: "),
              tags$br(),
            tags$body(fun_context)),
            # tags$br(),
            #tags$span(tags$b("Suggested prefix: "), tags$pre(suggested_fun))
          )
        # } else {
        #   fun$counter <- isolate(fun$counter) + length(fun$multiple_package) - 1
        #   tagList(
        #     tags$div(tags$b("Function: "), tags$pre(actual_fun)),
        #     tags$div(tags$b("Context: "), tags$pre(fun_context)),
        #     tags$br(),
        #     tags$div(
        #       tags$b("Suggested prefix: "),
        #       radioButtons(
        #         inputId = "choice_multiple", label = NULL, width = "100%",
        #         choiceNames = lapply(
        #           X = fun$multiple_package,
        #           FUN = function(x) {
        #             tags$pre(paste(x, actual_fun, sep = "::"), style = "width: 100%;")
        #           }
        #         ), choiceValues = paste(fun$multiple_package, actual_fun, sep = "::")
        #       ),
        #       tags$style(".radio input[type='radio'] {margin-top: 12px !important;}"),
        #       tags$style("label {width: 100%}")
        #     )
        #   )
        # }
      } else {
        removeUI(selector = "#buttons-navigate", immediate = TRUE)
        insertUI(
          selector = "#button-close-placeholder",
          ui = actionButton(
            inputId = "close_addin", label = "Close",
            icon = icon("remove"), width = "100%"
          ),
          immediate = TRUE
        )
        tags$div(
          class = "alert alert-success",
          tags$b("Done!"), "all changes have been reviewed."
        )
      }
    })

    observeEvent(input$add_prefix, {
       if (script_funs_r$x$package[fun$counter] == "insertion") {
        text2add <- fun$actual_fun
       } else if (script_funs_r$x$package[fun$counter] == "deletion"){
         text2add <- ""
       }
      insertText(
        location = get_position(script_funs_r$x, fun$counter),
        text = text2add
      )

      # Update pos
      row <- script_funs_r$x$numrow[fun$counter]
      if (sum(script_funs_r$x$numrow == row) > 1) {
        x <-  script_funs_r$x
        if (script_funs_r$x$package[fun$counter] == "insertion") {
        x$start[x$numrow == row] <- x$start[x$numrow == row] -11 #take away number of characters in <ins></ins>
        x$end[x$numrow == row] <- x$end[x$numrow == row] -11
        script_funs_r$x <- x
        } else if (script_funs_r$x$package[fun$counter] == "deletion"){
          x$start[x$numrow == row] <- x$start[x$numrow == row] - nchar(script_funs_r$x$funs[fun$counter]) #take away number of characters in script_funs #take away number of characters in <ins></ins>
          x$end[x$numrow == row] <- x$end[x$numrow == row] - nchar(script_funs_r$x$funs[fun$counter])
          script_funs_r$x <- x
        }
      }

      fun$counter <- fun$counter + 1
       if (fun$counter <= fun$max) {
        set_selection(script_funs_r$x, fun$counter)
        # fun$actual_fun <- script_funs_r$x$funs[fun$counter]
        fun$actual_fun <-  str_remove_all(script_funs_r$x$funs[fun$counter], pattern = "<.*?>") #removes ins and del tags
        # fun$suggested_fun <- paste(script_funs_r$x$package[fun$counter], fun$actual_fun, sep = "::")
         # fun$suggested_fun <- str_remove_all(script_funs_r$x$funs[fun$counter], pattern = "<.*?>")#removes ins and del tags
        
        fun$fun_context <- script_funs_r$x$fun_context[fun$counter]
         # fun$multiple_occurence <- sum(fun$actual_fun == script_funs_r$x$funs) > 1
        # fun$multiple_package <- unique(script_funs_r$x$package[script_funs_r$x$funs == fun$actual_fun])
      } else {
        fun$actual_fun <- ""
      }
    })
    
    observeEvent(input$reject, {
      if (script_funs_r$x$package[fun$counter] == "insertion") {
        text2add <- ""
      } else if (script_funs_r$x$package[fun$counter] == "deletion"){
        text2add <- fun$actual_fun
      }
      insertText(
        location = get_position(script_funs_r$x, fun$counter),
        text = text2add
      )
      
      # Update pos
      row <- script_funs_r$x$numrow[fun$counter]
      if (sum(script_funs_r$x$numrow == row) > 1) {
        x <-  script_funs_r$x
        if (script_funs_r$x$package[fun$counter] == "insertion") {
          x$start[x$numrow == row] <- x$start[x$numrow == row] - nchar(script_funs_r$x$funs[fun$counter]) #take away number of characters in script_funs
          x$end[x$numrow == row] <- x$end[x$numrow == row] - nchar(script_funs_r$x$funs[fun$counter])
          script_funs_r$x <- x
        } else if (script_funs_r$x$package[fun$counter] == "deletion"){
          x$start[x$numrow == row] <- x$start[x$numrow == row] -11 #take away number of characters in <ins></ins>
          x$end[x$numrow == row] <- x$end[x$numrow == row] -11
          script_funs_r$x <- x
        }
      }
      
      fun$counter <- fun$counter + 1
      if (fun$counter <= fun$max) {
        set_selection(script_funs_r$x, fun$counter)
        # fun$actual_fun <- script_funs_r$x$funs[fun$counter]
        fun$actual_fun <-  str_remove_all(script_funs_r$x$funs[fun$counter], pattern = "<.*?>") #removes ins and del tags
        # fun$suggested_fun <- paste(script_funs_r$x$package[fun$counter], fun$actual_fun, sep = "::")
        # fun$suggested_fun <- str_remove_all(script_funs_r$x$funs[fun$counter], pattern = "<.*?>")#removes ins and del tags
        
        fun$fun_context <- script_funs_r$x$fun_context[fun$counter]
        # fun$multiple_occurence <- sum(fun$actual_fun == script_funs_r$x$funs) > 1
        # fun$multiple_package <- unique(script_funs_r$x$package[script_funs_r$x$funs == fun$actual_fun])
      } else {
        fun$actual_fun <- ""
      }
    })

    observeEvent(input$skip, {
      fun$counter <- fun$counter + 1
      if (fun$counter <= fun$max) {
        # print(script_funs_r$fun_context)
        set_selection(script_funs_r$x, fun$counter)
        # fun$actual_fun <- script_funs_r$x$funs[fun$counter]
        fun$actual_fun <-  str_remove_all(script_funs_r$x$funs[fun$counter], pattern = "<.*?>") #removes ins and del tags
        # fun$suggested_fun <- paste(script_funs_r$x$package[fun$counter], fun$actual_fun, sep = "::")
        # fun$suggested_fun <- str_remove_all(script_funs_r$x$funs[fun$counter], pattern = "<.*?>")#removes ins and del tags
        
        fun$fun_context <- script_funs_r$x$fun_context[fun$counter]
        # fun$multiple_occurence <- sum(fun$actual_fun == script_funs_r$x$funs) > 1
        # fun$multiple_package <- unique(script_funs_r$x$package[script_funs_r$x$funs == fun$actual_fun])
      } else {
        fun$actual_fun <- ""
      }
    })

    # observeEvent(input$skip_all, {
    #   # fun$counter <- fun$counter + 1
    #   script_funs_r$x <- script_funs_r$x[!script_funs_r$x$funs %in% fun$actual_fun, ]
    #   fun$max <- nrow(script_funs_r$x)
    #   if (fun$counter <= fun$max) {
    #     # print(script_funs_r$fun_context)
    #     set_selection(script_funs_r$x, fun$counter)
    #     fun$actual_fun <- script_funs_r$x$funs[fun$counter]
    #     fun$suggested_fun <- paste(script_funs_r$x$package[fun$counter], fun$actual_fun, sep = "::")
    #     fun$fun_context <- script_funs_r$x$fun_context[fun$counter]
    #     fun$multiple_occurence <- sum(fun$actual_fun == script_funs_r$x$funs) > 1
    #     fun$multiple_package <- unique(script_funs_r$x$package[script_funs_r$x$funs == fun$actual_fun])
    #   } else {
    #     fun$actual_fun <- ""
    #   }
    # })



    observeEvent(input$close_addin, {
      stopApp()
    })

    observeEvent(input$cancel, {
      stopApp()
    })

   }
  
  runGadget(app = ui, server = server, viewer = paneViewer())
}









