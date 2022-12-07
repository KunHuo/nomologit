get_global_languange <- function(language = NULL, default = "en"){
  if(is.null(language)){
    language <-  getOption("language", default = default)
  }
  language
}


get_global_family <- function(family = NULL, default = "serif"){
  if(is.null(family)){
    family <-  getOption("family", default = default)
  }
  family
}


get_global_fontsize <- function(font.size = NULL, default = 12){
  if(is.null(font.size)){
    font.size <- getOption("font.size", default = default)
  }
  font.size
}


get_global_palette <- function(palette = NULL, default = NULL){
  if(is.null(palette)){
    palette <-  getOption("palette", default = NULL)
  }
  palette
}

get_global_labels <- function(labels = NULL, default = NULL){
  if(is.null(labels)){
    labels <-  getOption("labels", default = NULL)
  }
  labels
}



