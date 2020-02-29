#' @title Comment as tooltip with icon
#' @rdname comment
#' @name comment
#' @description Add comment using in-line code to rmarkdown to show icon with comment as 
#' toolip
#'
#' @export
#' 
comment <- function(comment){
  tippy::tippy("<span class='fas fa-comments fa-1.5x' style='color: red;'></span>", 
               comment, 
               allowHTMl= TRUE, 
               size = "large")
}