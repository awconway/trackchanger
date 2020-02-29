#' Add a comment
#'
#' Call this function as an addin to add comment at the cursor postion.
#'
#' @export
add_comment <- function() {
  con <- rstudioapi::getActiveDocumentContext()
  
  # get cursor position
  docPos <- con$selection[[1]]$range$end
  text <- '<div class="comment"><span class="fas fa-comments" style="color: #ffe392;"></span><span class="commenttext"></span></div>'
  # Add markup
  rstudioapi::insertText(text, 
                         id = con$id)
  
  # move cursor
  docPosNew <- docPos + c(0, 108)
  rstudioapi::setCursorPosition(docPosNew, id = con$id)
}