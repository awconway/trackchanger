#' Find the position of a word in a script
#'
#' @param script A script (character string)
#' @param word Name of a function
#'
#' @return a \code{data.frame}
#' @noRd
#'
#' @examples
#' find_position(
#'  script = "dat <- data.frame(var1 = letters); head(dat)",
#'  word = "head"
#' )
find_position <- function(script) {
  pos <- stringr::str_locate_all(string = script, pattern = '<ins>(.*?)</ins>|<del>(.*?)</del>|<span class="comment"><span class="fas fa-comments" style="color: #ffe392;"></span><span class="commenttext">(.*?)</span></span>')
  
  num_row <- unlist(lapply(pos, nrow))
  pos <- do.call("rbind", pos)
  pos <- as.data.frame(pos)
  pos$numrow <- rep(which(num_row > 0), num_row[num_row > 0])
  
  changes_tags <- stringr::str_extract_all(string = script, pattern = '<ins>(.*?)</ins>|<del>(.*?)</del>|<span class="comment"><span class="fas fa-comments" style="color: #ffe392;"></span><span class="commenttext">(.*?)</span></span>')
  changes_tags <- unique(unlist(changes_tags))
  df <- data.frame(changes_tags,
                   stringsAsFactors = FALSE)
  df <- df %>% 
    dplyr::mutate(change_type = dplyr::case_when(
      stringr::str_detect(changes_tags, "<ins>(.*?)</ins>") ~ "insertion",
      stringr::str_detect(changes_tags, "<del>(.*?)</del>") ~ "deletion",
      stringr::str_detect(changes_tags, '<span class="comment"><span class="fas fa-comments" style="color: #ffe392;"></span><span class="commenttext">(.*?)</span></span>') ~ "comment"
    ) 
    )
  
  df <-  df %>% 
    dplyr::mutate(changes = stringr::str_remove_all(changes_tags, 
                                           pattern = '<ins>|</ins>|<del>|</del>|<span class="comment"><span class="fas fa-comments" style="color: #ffe392;"></span><span class="commenttext">|</span></span>')) #removes ins and del tags

  df <- df %>% 
    dplyr::mutate(changes_context = script[pos$numrow])
  cbind(pos, df)
}



#' Get changes in rmarkdown document with their position and others infos
#'
#' @param script A character string
#'
#' @return a \code{data.frame}
#' @noRd
#'
#'
get_script_changes <- function(script) {

  
  
  script_changes <- find_position(script)

    hico <- stringr::str_sub(string = script_changes$changes_context, start = script_changes$start, end = script_changes$end)
    hico <- paste0("<b class='highlight-context'>", hico, "</b>")
    stringr::str_sub(string = script_changes$changes_context, start = script_changes$start, end = script_changes$end) <- hico
  script_changes
}


#' @importFrom rstudioapi setSelectionRanges
set_selection <- function(dat, indice) {
  doc_pos <- get_position(dat, indice)
  rstudioapi::setSelectionRanges(doc_pos)
}

#' @importFrom rstudioapi document_position document_range
get_position <- function(dat, indice) {
  row <- dat$numrow[indice]
  start <- dat$start[indice]
  start_pos <- rstudioapi::document_position(row, start)
  end <- dat$end[indice] + 1
  end_pos <- rstudioapi::document_position(row, end)
  rstudioapi::document_range(start_pos, end_pos)
}