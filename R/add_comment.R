#' Add a comment
#'
#' Call this function as an addin to add comment at the cursor postion.
#'
#' @export
add_comment <- function() {
  con <- rstudioapi::getActiveDocumentContext()
  
  # get cursor position
  docPos <- con$selection[[1]]$range$end
  
  # Add markup
  rstudioapi::insertText('<div class="comment"><span class="fas fa-comments fa-2x" style="color: red;"></span><span class="commenttext"></span></div>', 
                         id = con$id)
  
  # move cursor
  docPosNew <- docPos + c(0, 110)
  rstudioapi::setCursorPosition(docPosNew, id = con$id)
}