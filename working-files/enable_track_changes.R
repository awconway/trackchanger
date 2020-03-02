#' Enable css for track changes in knitted rmarkdown
#'
#' @return CSS and javascript text snippet 
#'
#' @export
enable_track_changes <- function() {
  content <- rstudioapi::getActiveDocumentContext()
  jssnippet <- '```{css echo=FALSE}\n
  ins, del{'
#   '```{css echo=FALSE}
# ins, del{
#   text-decoration: none;
#   display: inline-block;
#   padding: 0 .3em;
# }
# ins {
#   background: #83d5a8;
#   height: auto;
#   border-radius: .3em;
#   display: inline;
#   -webkit-box-decoration-break: clone;
#   -o-box-decoration-break: clone;
#   box-decoration-break: clone;
#   margin-left: .07em;
#   margin-right: .07em;
# }
# del {
#   background: rgba(231, 76, 60,.5);
#   cursor: no-drop;
#   position: relative;
#   top: .2em;
#   height: 1.31em;
#   width: .4em;
#   line-height: 1.35;
#   overflow: hidden;
#   color: transparent;
#   margin: -.4em .07em -.05em .07em;
#   padding: 0 0 .1em 0;
#   -webkit-box-decoration-break: clone;
#   -o-box-decoration-break: clone;
#   box-decoration-break: clone;
#   border-radius: .3em;
#   transition: padding-left ease .3s, padding-right ease .3s, color ease .15s;
# }
# del:hover {
#   width: auto;
#   color: #962e22;
#   display: inline;
#   padding: 0 .35em 0em 0.35em;
#   line-height: unset;
#   position: unset;
#   top: unset;
#   height: unset;
#   transition: padding-left ease .3s, padding-right ease .3s, color ease .7s;
# }
# ```
# 
# <script src="https://kit.fontawesome.com/f3ae6d3fea.js" crossorigin="anonymous"></script>
# '
  
  if (!any(grepl(jssnippet, content$contents))) {
    rstudioapi::insertText(Inf, jssnippet)
    message("CSS code chunk and script for 
            fontawesome icons is inserted at the 
            bottom of your markdown document")
  } else {
    message("Time for a coffee, you hit the button twice. ")
  }
}