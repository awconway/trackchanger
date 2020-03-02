#' @title Shiny Gadget to interactively accept or reject chaanges in rmarkdown
#'
#' @description Open a rmd document, then launch this addin via RStudio menu.
#' It will propose to accept or reject the change or skip to the next function.
#'
#' @export
#'
#'
#' @examples
#' \dontrun{
#'
#' trackchanger()
#'
#' }
trackchanger <- function() {
  
  script <- rstudioapi::getSourceEditorContext()$contents
  if (sum(nchar(script), na.rm = TRUE) == 0) {
    warning("It seems there are nothing to track change...")
    return(invisible())
  }
  
  
  
  script_changes <- get_script_changes(script)
  
  if (nrow(script_changes) == 0) {
    warning("It seems there are nothing to track change...")
    return(invisible())
  }

  
  #to_load <- get_unloaded_packages(script = script)
  
  ui <- miniUI::miniPage(
    shiny::tags$head(shiny::tags$style(".highlight-context {color: red !important;}")),
    shiny::tags$head(shiny::tags$style('.comment {
    position: relative;
    display: inline-block;
}

.comment .commenttext {
    visibility: hidden;
    width: 120px;
    background-color: #ffe392;
    color: black;
    text-align: center;
    border-radius: 6px;
    padding: 5px 0;
    position: absolute;
    z-index: 1;
    top: -5px;
    left: 110%;
}

.comment .commenttext::after {
    content: "";
    position: absolute;
    top: 50%;
    right: 100%;
    margin-top: -5px;
    border-width: 5px;
    border-style: solid;
    border-color: transparent #ffe392 transparent transparent;
}

.comment:hover .commenttext {
    visibility: visible;
}
ins, del{
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
}')),
    shiny::tags$div(
      class = "gadget-title", style = "background-color: rgb(16,34,70);",
      shiny::tags$h1("trackchanger", style = "font-weight: bold; color: #FFF;"),
      shiny::tags$button(
        id="cancel", type="button", "Cancel",
        class="btn btn-default btn-sm action-button pull-right"
      )
    ),
    miniUI::miniContentPanel(
        shiny::tags$br(),
       shiny::uiOutput(outputId = "ui_fun"),
    ),
       # shiny::uiOutput(outputId = "ui_buttons"),
       
        shiny::tags$div(id = "buttons-navigate",
                       miniUI::miniButtonBlock(
                                   shiny::actionButton(
                                     inputId = "add", label = "Accept", width = "100%",
                                     icon = shiny::tags$i(shiny::tags$b(shiny::icon("check"), style = "font-style: normal;"))
                                   ),
                                   shiny::actionButton(
                                     inputId = "reject", label = "Reject", width = "100%",
                                     icon = shiny::tags$i(shiny::tags$b(shiny::icon("times-circle"), style = "font-style: normal;"))
                                   ),
                                   shiny::actionButton(
                                     inputId = "skip", label = "Skip", width = "100%",
                                     icon = shiny::tags$i(shiny::tags$b(shiny::icon("forward"), style = "font-style: normal;"))
                                   ),
                                     shiny::actionButton(
                                       inputId = "delete", label = "Delete comment",
                                       icon = shiny::icon("trash"), width = "100%", style="color: #fff; 
                                                                                           background-color: #337ab7; 
                                                                                           border-color: #337ab7"
                                     ),
                                  )
        ),
       # shiny::tags$div(id = "#buttons-comment",
       #                         miniUI::miniButtonBlock(
       #                           shiny::actionButton(
       #                             inputId = "delete", label = "Delete comment",
       #                             icon = shiny::icon("trash"), width = "100%"
       #                           ),
       #                           shiny::actionButton(
       #                             inputId = "skip", label = "Skip",
       #                             icon = shiny::tags$i(shiny::tags$b(shiny::icon("forward"), style = "font-style: normal;")),
       #                             width = "100%"
       #                           )
       #                         )),
       
        shiny::tags$div(id = "button-close-placeholder")
       
       
      # )
    )
  
  server <- function(input, output, session) {
    
    script_changes_r <- shiny::reactiveValues(x = script_changes)

    fun <- shiny::reactiveValues(
      counter = 1,
      changes = script_changes$changes[1],
      changes_context = script_changes$changes_context[1],
      max = nrow(script_changes),
      change_type = script_changes$change_type[1],
      changes_tags = script_changes$changes_tags[1]
      )

    set_selection(script_changes, 1)

    output$ui_fun <- shiny::renderUI({
      changes <- fun$changes
      change_type <- fun$change_type
      changes_context <- shiny::HTML(fun$changes_context)

      if (shiny::isolate(fun$counter) <= fun$max) {
        if (script_changes_r$x$change_type[fun$counter] =="insertion"){
          text_insertion <- "Inserted text: "
        } else if(script_changes_r$x$change_type[fun$counter]=="deletion"){
          text_insertion <- "Deleteted text: "
        } else if(script_changes_r$x$change_type[fun$counter]=="comment"){
          text_insertion <- "Comment: "
        }
          shiny::tagList(
            shiny::tags$div(shiny::tags$b(text_insertion), shiny::tags$pre(changes)),
            shiny::tags$div(shiny::tags$b("Context: "),
              shiny::tags$br(),
            shiny::tags$body(changes_context)),
          )
      } else {
        shiny::removeUI(selector = "#buttons-navigate", immediate = TRUE)
        shiny::insertUI(
          selector = "#button-close-placeholder",
          ui = shiny::actionButton(
            inputId = "close_addin", label = "Close",
            icon = shiny::icon("remove"), width = "100%"
          ),
          immediate = TRUE
        )
        shiny::tags$div(
          class = "alert alert-success",
          shiny::tags$b("Done!"), "all changes have been reviewed."
        )
      }
    })
    
    # output$ui_buttons <- shiny::renderUI({
    #   if(script_changes_r$x$change_type[fun$counter]=="comment"){
    #     shiny::removeUI( selector = "#buttons-navigate",immediate = TRUE)
    #   } else {
    #       shiny::removeUI(immediate = TRUE,selector = "#buttons-comment")
    #   }
    # })

    shiny::observeEvent(input$add, {
       if (script_changes_r$x$change_type[fun$counter] == "insertion") {
        text2add <- fun$changes
        
       } else if (script_changes_r$x$change_type[fun$counter] == "deletion"){
         text2add <- ""
       } 
      rstudioapi::insertText(
        location = get_position(script_changes_r$x, fun$counter),
        text = text2add
      )

      # Update pos
      row <- script_changes_r$x$numrow[fun$counter]
      if (sum(script_changes_r$x$numrow == row) > 1) {
        x <-  script_changes_r$x
        if (script_changes_r$x$change_type[fun$counter] == "insertion") {
        x$start[x$numrow == row] <- x$start[x$numrow == row] -11 #take away number of characters in <ins></ins>
        x$end[x$numrow == row] <- x$end[x$numrow == row] -11
        script_changes_r$x <- x
        } else if (script_changes_r$x$change_type[fun$counter] == "deletion"){
          x$start[x$numrow == row] <- x$start[x$numrow == row] - nchar(script_changes_r$x$changes_tags[fun$counter]) #take away number of characters in script_changes #take away number of characters in <ins></ins>
          x$end[x$numrow == row] <- x$end[x$numrow == row] - nchar(script_changes_r$x$changes_tags[fun$counter])
          script_changes_r$x <- x
        } 
      }

      fun$counter <- fun$counter + 1
       if (fun$counter <= fun$max) {
        set_selection(script_changes_r$x, fun$counter)
        fun$changes <-  script_changes_r$x$changes[fun$counter]
        fun$changes_context <- script_changes_r$x$changes_context[fun$counter]
      } else {
        fun$changes <- ""
      }
    })
    
    shiny::observeEvent(input$reject, {
      if (script_changes_r$x$change_type[fun$counter] == "insertion") {
        text2add <- ""
      } else if (script_changes_r$x$change_type[fun$counter] == "deletion"){
        text2add <- fun$changes
      } 
      rstudioapi::insertText(
        location = get_position(script_changes_r$x, fun$counter),
        text = text2add
      )
      
      # Update pos
      row <- script_changes_r$x$numrow[fun$counter]
      if (sum(script_changes_r$x$numrow == row) > 1) {
        x <-  script_changes_r$x
        if (script_changes_r$x$change_type[fun$counter] == "insertion") {
          x$start[x$numrow == row] <- x$start[x$numrow == row] - nchar(script_changes_r$x$changes_tags[fun$counter]) #take away number of characters in script_changes
          x$end[x$numrow == row] <- x$end[x$numrow == row] - nchar(script_changes_r$x$changes_tags[fun$counter])
          script_changes_r$x <- x
        } else if (script_changes_r$x$change_type[fun$counter] == "deletion"){
          x$start[x$numrow == row] <- x$start[x$numrow == row] -11 #take away number of characters in <ins></ins>
          x$end[x$numrow == row] <- x$end[x$numrow == row] -11
          script_changes_r$x <- x
        } 
      }
      
      fun$counter <- fun$counter + 1
      if (fun$counter <= fun$max) {
        set_selection(script_changes_r$x, fun$counter)
        fun$changes <-  script_changes_r$x$changes[fun$counter]
        fun$changes_context <- script_changes_r$x$changes_context[fun$counter]
      } else {
        fun$changes <- ""
      }
      
    })
    
    
    shiny::observeEvent(input$delete, {
        text2add <- ""
      rstudioapi::insertText(
        location = get_position(script_changes_r$x, fun$counter),
        text = text2add
      )
      # Update pos
      row <- script_changes_r$x$numrow[fun$counter]
      if (sum(script_changes_r$x$numrow == row) > 1) {
        x <-  script_changes_r$x
          x$start[x$numrow == row] <- x$start[x$numrow == row] - nchar(script_changes_r$x$changes_tags[fun$counter]) #take away number of characters in script_changes #take away number of characters in <ins></ins>
          x$end[x$numrow == row] <- x$end[x$numrow == row] - nchar(script_changes_r$x$changes_tags[fun$counter])
          script_changes_r$x <- x
      }
      
      fun$counter <- fun$counter + 1
      if (fun$counter <= fun$max) {
        set_selection(script_changes_r$x, fun$counter)
        fun$changes <-  script_changes_r$x$changes[fun$counter]
        fun$changes_context <- script_changes_r$x$changes_context[fun$counter]
      } else {
        fun$changes <- ""
      }
    })

    shiny::observeEvent(input$skip, {
      fun$counter <- fun$counter + 1
      if (fun$counter <= fun$max) {
        set_selection(script_changes_r$x, fun$counter)
        fun$changes <-  script_changes_r$x$changes[fun$counter]
        fun$changes_context <- script_changes_r$x$changes_context[fun$counter]
      } else {
        fun$changes <- ""
      }
    })

    shiny::observeEvent(input$close_addin, {
      shiny::stopApp()
    })

    shiny::observeEvent(input$cancel, {
      shiny::stopApp()
    })

   }
  
  shiny::runGadget(app = ui, server = server, viewer = shiny::paneViewer())
}









