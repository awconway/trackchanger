#' Tracking highlighting
#'
#' Call this function as an addin to add the markdown track changes output for highlighting
#'
#' @export
trackHighlight <- function() {
  con <- rstudioapi::getSourceEditorContext()
  
  # Get selected text
  selection <- con$selection[[1]]$text
  
  # Add markup
  docPos <- con$selection[[1]]$range$end
  rstudioapi::insertText(paste0('<div class="comment"><mark>', selection, '</mark><span class="commenttext"></span></div>'),
                         id = con$id)
  # Move cursor
  docPosNew <- docPos + c(0, 60)
  rstudioapi::setCursorPosition(docPosNew, id = con$id)
}