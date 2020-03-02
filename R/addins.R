#' Add a comment
#'
#' Call this function as an addin to add comment at the cursor postion.
#'
#' @export
trackComment <- function() {
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

#' Tracking an addition
#'
#' Call this function as an addin to add text at the cursor postion.
#'
#' @export
trackAdd <- function() {
  con <- rstudioapi::getActiveDocumentContext()
  
  # get cursor position
  docPos <- con$selection[[1]]$range$end
  
  # Add markup
  rstudioapi::insertText("<ins></ins>", id = con$id)
  
  # move cursor
  docPosNew <- docPos + c(0, 5)
  rstudioapi::setCursorPosition(docPosNew, id = con$id)
}

#' Tracking a deletion
#'
#' Call this function as an addin to delete highlighted text.
#'
#' @export
trackDelete <- function() {
  con <- rstudioapi::getActiveDocumentContext()
  
  # start of highlight
  startPos <- con$selection[[1]]$range$start
  
  # end of highlight
  endPos <- con$selection[[1]]$range$end
  
  # Add markup
  rstudioapi::insertText(location = startPos, "<del>", id = con$id)
  rstudioapi::insertText(location = endPos + c(0, 5), "</del>",
                         id = con$id)
  
  # move cursor
  startPosNew <- endPos + c(0, 11)
  rstudioapi::setCursorPosition(startPosNew, id = con$id)
}

#' Tracking a substitution
#'
#' Call this function as an addin to add the markdown track changes output for substitution.
#'
#' @export
trackSubstitute <- function() {
  con <- rstudioapi::getSourceEditorContext()
  
  # Get selected text
  selection <- con$selection[[1]]$text
  
  # Add markup
  docPos <- con$selection[[1]]$range$end
  rstudioapi::insertText(paste0("<del>", selection, "</del><ins></ins>"),
                         id = con$id)
  
  # Move cursor
  docPosNew <- docPos + c(0, 16)
  rstudioapi::setCursorPosition(docPosNew, id = con$id)
}
