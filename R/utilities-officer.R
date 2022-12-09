format_flex <- function(data, headers = NULL, font.size = 11){
  if(length(headers) == 0L){
    ft <- flextable::flextable(data)
  }else{
    newnames <- headers[[1]]
    newnames <- newnames[!grepl(x = newnames, pattern = "^blank\\d+$")]
    names(data) <- newnames

    ft <- flextable::flextable(data, col_keys = headers$col_keys)
    ft <- flextable::set_header_df(ft, mapping = headers, key = "col_keys")
  }
  border <- officer::fp_border(color = "black")

  ft <- flextable::padding(ft, padding = 3, part = "all")
  ft <- flextable::fontsize(ft, size = font.size, part = "all")
  ft <- flextable::font(ft, fontname = "Times New Roman", part = "all")
  ft <- flextable::merge_v(ft, part = "header")
  ft <- flextable::merge_h(ft, part = "header")
  ft <- flextable::align(ft, align = "center", part = "all")
  ft <- flextable::align(ft, j = 1, align = "left", part = "all")
  ft <- flextable::autofit(ft)

  ft <- flextable::empty_blanks(ft)

  ft <- flextable::hline_bottom(ft, part = "body",   border = border)
  ft <- flextable::hline_top(ft,    part = "body",   border = border)
  ft <- flextable::hline_top(ft,    part = "header", border = border)

  ft <- flextable::fix_border_issues(ft)
  ft
}


make_headers <- function(data, headerlist = NULL, sep = "__"){
  if(is.null(sep)){
    return(NULL)
  }

  if(length(headerlist) != 0L){
    for (i in seq_along(headerlist)) {
      index <- headerlist[i][[1]]
      names(data)[index] <- paste(names(headerlist)[i], names(data)[index], sep = sep)
    }
  }

  if(!any(grepl(x = names(data), pattern = sep, fixed = TRUE))){
    return(NULL)
  }

  col_keys <- names(data)
  lines <- strsplit(names(data), split = sep, fixed = TRUE)
  lines <- do.call(cbind, lines)
  line2 <- lines[1, ]
  line3 <- lines[2, ]

  index <- sapply(unique(line2), function(x){
    if(length(which(x == line2)) > 1 ){
      x
    }else{
      NA
    }
  })
  index <- index[!is.na(index)]

  for(i in seq_along(index)){
    tmp <- which(index[i] == line2)
    col_keys[tmp] <- sprintf("%d_%d", seq_along(tmp), i)
  }

  out <- data.frame(col_keys = col_keys, line2 = line2, line3 = line3)
  out <- split(out, f = line2)
  out <- out[unique(line2)]

  for (i in seq_along(out)){
    if(i < length(out)){
      if((names(out)[i] %in% index) & (names(out)[i + 1] %in% index)){
        blank <- sprintf("blank%s", substr(out[[i]][1, 1], start = 3, stop = nchar(out[[i]][1, 1])))
        out[[i]] <- rbind(out[[i]], c(blank, " ", " "))
      }
    }
  }
  do.call(rbind, out)
}


get_template <- function(template) {
  template <- paste("templates", template, sep = "/")
  # package  <- methods::getPackageName()
  template <- file.path(system.file(package = "nomologit"), template)
  regression <- regexpr(paste("(\\.(?i)(docx))$", sep = ""), template)

  if (regression < 1) {
    stop("invalid template name, it must have extension .docx", call. = FALSE)
  }
  template <- R.utils::getAbsolutePath(template, expandTilde = TRUE)

  if (!file.exists(template)) {
    stop(template , " can not be found.")
  }
  return(template)
}



flex_table <- function(data, headerlist = NULL, sep = "__", font.size = 11, ...){
  headers <- make_headers(data = data, sep = sep, headerlist = headerlist)
  format_flex(data, headers, font.size = font.size)
}



get_docx <- function(landscape = FALSE){
  if(landscape){
    path <- get_template("english_landscape.docx")
  }else{
    path <- get_template("english_portrait.docx")
  }
  officer::read_docx(path = path)
}


file_ext <- function(path){
  regmatches(path,
             regexpr(pattern = "(?<=\\.)[^\\.]+$",
                     path,
                     ignore.case = FALSE,
                     perl = TRUE,
                     fixed = FALSE,
                     useBytes = FALSE))
}


file_path <- function(path, ext = "docx"){
  tmp <- file_ext(path = path)
  if(length(tmp) == 0L){
    path <- paste(path, ext, sep = ".")
  }else{
    if(tolower(tmp) != ext){
      path <- paste(path, ext, sep = ".")
    }
  }
  path
}


dir_create <- function(path){
  path.name <- dirname(path)
  if(path.name != "."){
    if(!dir.exists(path.name)){
      dir.create(path.name, recursive = TRUE)
    }
  }
}



body_add_par2 <- function(x, value, style = NULL, pos = "after"){
  value <- paste(value, collapse = "\n")
  values <- strsplit(value, split = "\n", fixed = TRUE)
  values <- values[[1]]

  for(i in seq_along(values)){

   # print(values[i])
    mdpars <- md_to_officer(values[i])
    for(pgraph in mdpars){
      x <- officer::body_add_fpar(x, value = eval(parse(text = pgraph$fpar_cmd)), style = style)
    }
    # x <- officer::body_add_par(x, value = values[i], style = style, pos = "after")
  }
  x
}


# mdpars = md_to_officer(mdtext)
# for(pgraph in mdpars){
#   my_doc = body_add_fpar(my_doc, value=eval(parse(text=pgraph$fpar_cmd)), style="Normal") }


body_add_tablenote <- function(x, value) {
  value <- paste(value, collapse = "\n")
  values <- strsplit(value, split = "\n", fixed = TRUE)
  values <- values[[1]]

  for(i in seq_along(values)){
    if (i == 1L){
      padding.top <- 5
    }else{
      padding.top <- 0
    }

    mdpars <- md_to_officer(values[i])

    for(pgraph in mdpars){

      text <- eval(parse(text = pgraph$fpar_cmd))
      text$fp_p$line_spacing <- 1.5
      text$fp_p$padding.top  <- padding.top
      # cat("\n------------------\n")

      x <- officer::body_add_fpar(x, value = text)
    }

     # paragraph <- officer::fpar( officer::ftext(values[i]), fp_p = officer::fp_par(text.align = "left", padding.top = padding.top, line_spacing = 1.5))
     #
     #
     # x <- officer::body_add_fpar(x, value = paragraph)
  }
  x
}



body_add_heading1 <- function(x, value){
  body_add_par2(x, value = value, style = "heading 1")
}



body_add_heading2 <- function(x, value){
  body_add_par2(x, value = value, style = "heading 2")
}



body_add_heading3 <- function(x, value){
  body_add_par2(x, value = value, style = "heading 3")
}



body_add_dataframe <- function(x, value, ...){
  if(!("flextable" %in% class(x))){
    value <- flex_table(data = value, ...)
  }
  flextable::body_add_flextable(x = x,
                                value = value,
                                align = "left",
                                split = TRUE,
                                keepnext = FALSE)
}



heading1 <- function(x){
  out <- list(text = as.character(x), style = "heading 1")
  class(out) <- c("style")
  out
}


heading2 <- function(x){
  out <- list(text = as.character(x), style = "heading 2")
  class(out) <- c("style")
  out
}



heading3 <- function(x){
  out <- list(text = as.character(x), style = "heading 3")
  class(out) <- c("style")
  out
}


write_rdocx <- function(x, path, ...){
  if(is.null(path)){
    stop("Path can not be empty.", call. = FALSE)
  }else{
    path <- trimws(path)
  }
  if(length(path) == 0L | path == ""){
    stop("Path can not be empty.", call. = FALSE)
  }
  path <- file_path(path)
  dir_create(path = path)
  print(x, path)
}


write_docx <- function(x, path = "", landscape = FALSE, ...){
  UseMethod("write_docx")
}



write_docx.default <- function(x, path = "", landscape = FALSE, ...){
  get_docx(landscape = landscape) |>
    body_add_par2(value = as.character(x)) |>
    write_rdocx(path)
}



write_docx.data.frame <- function(x, path = "", landscape = FALSE, ...){
  doc <- get_docx(landscape = landscape)
  title <- attr(x, "title")
  note <- attr(x, "note")
  if(length(title) != 0L){
    doc <- body_add_par2(doc, value = title, style = "table title")
  }
  doc <- body_add_dataframe(doc, value = x, ...)
  if(length(note) != 0L){
    doc <- body_add_tablenote(doc, value = note)
  }
  write_rdocx(doc, path = path)
}



write_docx.list <- function(x, path = "", landscape = FALSE, ...){
  doc <- get_docx(landscape = landscape)
  for(i in 1:length(x)){
    if(is.character(x[[i]]) | is.numeric(x)){
      doc <- body_add_par2(doc, value = x[[i]], style = "Normal")
    }else if(is.data.frame(x[[i]])){
      title <- attr(x[[i]], "title")
      note <- attr(x[[i]], "note")
      if(length(title) != 0L){
        doc <- body_add_par2(doc, value = title, style = "table title")
      }
      if(i != 1L){
        if(is.data.frame(x[[i-1]])){
          doc <- body_add_par2(doc, value = " ")
        }
      }
      doc <- body_add_dataframe(doc, value = x[[i]])
      if(length(note) != 0L){
        doc <- body_add_tablenote(doc, value = note)
      }
    }else if(class(x[[i]])[1] == "spubr"){

    }else if(class(x[[i]])[1] == "flextable"){
      doc <- body_add_dataframe(doc, value = x[[i]])
    }else if(class(x[[i]])[1] == "style"){
      value <- x[[i]]$text
      style <- x[[i]]$style
      doc <- body_add_par2(doc, value = value, style = style)
    }else{
      doc <- body_add_par2(doc, value = as.character(x[[i]]), style = "Normal")
    }
  }
  write_rdocx(doc, path = path)
}



write_docx.rdocx <- function(x, path = "", landscape = FALSE, ...){
  write_rdocx(x, path = path)
}


md_to_officer <- function(str){

  # First we find paragraphs:
  pgraphs = unlist(base::strsplit(str, split="(\r\n|\r|\n){2,}"))


  md_info = data.frame(
    md_name = c( "subscript",         "superscript",     "bold_us",  "bold_st",     "italic",            "color",                     "shading_color",                     "font_family"             ),
    pattern = c( "~.+?~",             "\\^.+?\\^",       "_.+?_",    "\\*.+?\\*",   "\\%\\%.+?\\%\\%",   "<color:\\S+?>.+?</color>",  "<shade:\\S+?>.+?</shade>",          "<ff:\\S+?>.+?</ff>"      ),
    start   = c( "~",                 "\\^",             "_",        "\\*",         "\\%\\%",            "<color:\\S+?>",             "<shade:\\S+?>",                     "<ff:\\S+?>"              ),
    end     = c( "~",                 "\\^",             "_",        "\\*",         "\\%\\%",            "</color>",                  "</shade>",                          "</ff>"                   ),
    prop    = c( "vertical.align",    "vertical.align",  "bold",     "bold",        "italic",            "color",                     "shading_color",                     "font.family"             ))


  pos_start = c()
  pos_stop  = c()

  # Saving the parsed paragraphs
  pgraphs_parse = list()

  # Now we walk through each paragraph
  pgraph_idx = 1
  for(pgraph_raw in pgraphs){

    # Removing all of the carriage returns in the paragraph:
     pgraph = gsub(pattern="(\r\n|\r|\n)", replacement=" ", pgraph_raw)

    # pgraph = pgraph_raw

    # Storing the locations of the markdown in the string
    locs      = NULL

    # Visual id of md elements to debug finding stuff
    md_visual = c()

    # Converting the ** to %% to make it easier to distinguish between bold and
    # italics
    pgraph = gsub(pgraph, pattern="\\*\\*", replacement="%%")
    pgraph = gsub(pgraph, pattern="__", replacement="%%")

    # Finding the locations of the markdown elements
    for(md_idx  in 1:nrow(md_info)){
      pattern = as.character(md_info[md_idx, ]$pattern)
      md_name = as.character(md_info[md_idx, ]$md_name)

      # tmplocs = stringr::str_locate_all(pgraph, pattern)[[1]]
      tmplocs = regex_locate_all(pgraph, pattern)[[1]]

      if(nrow(tmplocs) > 0){
        tmplocs = as.data.frame(tmplocs)
        tmplocs$md_name = md_name
        locs = rbind(locs, tmplocs)
      }
    }


    # if locs is NULL then no markdown elements were found in the current
    # current paragraph so we just raap that up
    if(is.null(locs)){
      pele     = list()
      pele$p_1 = list(text      = pgraph,
                      props     = c("NULL"),
                      props_cmd = "prop=NULL")
    } else {
      # If locs isn't NULL we start working trough the markdown elements:

      # We begin by grouping nested markdown elements
      locs =locs[order(locs$start), ]
      locs$group = 1
      if(nrow(locs) > 1){
        for(loc_idx in 2:nrow(locs)){
          # If the current md element starts before the last one stops
          # they are grouped together, otherwise they become part of a new group
          if(locs[loc_idx,]$start < locs[loc_idx-1,]$end){
            locs[loc_idx,]$group = locs[loc_idx-1,]$group
          } else {
            locs[loc_idx,]$group = locs[loc_idx-1,]$group + 1
          }
        }
      }

      # Pulling out the separate paragraph elements
      pele     = list()
      pele_idx = 1
      # Processing each group
      for(group in unique(locs$group)){
        # pulling out the markdown elements for that group
        gr_md   = locs[locs$group == group, ]


        # if we're dealing with the first group and it starts after the first
        # character then we add that first chunk of text
        if(group == 1 & gr_md[1, ]$start > 1){
          #pele_tmp = list(text = pgraph)
          pele[[paste('p_', pele_idx, sep="")]] =
            list(text      = substr(pgraph, start=1, stop=(gr_md$start-1)),
                 props     = c("NULL"),
                 props_cmd = "prop=NULL")
          pele_idx = pele_idx + 1
        }

        # If we're dealing with a group after the first we need to pull out any
        # text between groups
        if(group > 1){
          # Previous group:
          gr_md_prev   = locs[locs$group == group-1, ]

          # If there is more than 1 character difference between the last md
          # element from the previous group and the first of the current group
          # then we need to add that text
          if(gr_md[1, ]$start-gr_md_prev[1, ]$end > 1){
            pele[[paste('p_', pele_idx, sep="")]] =
              list(text      = substr(pgraph,
                                      start =(gr_md_prev[1, ]$end + 1),
                                      stop  =(gr_md[1, ]$start - 1)),
                   props     = c("NULL"),
                   props_cmd = "prop=NULL")
            pele_idx = pele_idx + 1
          }
        }

        # Processing the markdown for a group
        # First we pull out the text from the inner most markdown element
        md_text = substr(pgraph,
                         start =gr_md[nrow(gr_md), ]$start,
                         stop  =gr_md[nrow(gr_md), ]$end)

        # now we strip off the beginning and ending of the markdown
        md_name  = gr_md[nrow(gr_md), ]$md_name

        # patterns to strip off the beginning and end
        md_start = paste("^", as.character(md_info[md_info$md_name == md_name, ]$start), sep="")
        md_end   = paste(as.character(md_info[md_info$md_name == md_name, ]$end), "$", sep="")

        # Stripping those patterns off
        md_text = sub(md_text, pattern=md_start, replacement="")
        md_text = sub(md_text, pattern=md_end, replacement="")

        if(group == 4){
          #browser()
        }

        # Now we save the text:
        pele[[paste('p_', pele_idx, sep="")]] =
          list(text      = md_text,
               props     = c(),
               props_cmd = NULL)

        tmp_props = c()
        # Next we add the properties associated with the markdown
        for(md_name in (gr_md$md_name)){
          md_start = as.character(md_info[md_info$md_name == md_name, ]$start)
          md_end   = as.character(md_info[md_info$md_name == md_name, ]$end)
          md_prop  = as.character(md_info[md_info$md_name == md_name, ]$prop)

          # Setting properties based on the type of markdown selected
          if(md_name == "bold_st" | md_name == "bold_us"){
            tmp_props = c(tmp_props, "bold = TRUE")
          }

          if(md_name == "italic"){
            tmp_props = c(tmp_props, "italic = TRUE")
          }

          if(md_name == "superscript"){
            tmp_props = c(tmp_props, 'vertical.align = "superscript"')
          }

          if(md_name == "subscript"){
            tmp_props = c(tmp_props, 'vertical.align = "subscript"')
          }

          if(md_name == "color"){
            # pulling out the color markdown text. It uses the first entry so
            # the outer most. There shouldn't be more than one.
            md_text = substr(pgraph,
                             start = gr_md[gr_md$md_name == "color", ]$start[1],
                             stop  = gr_md[gr_md$md_name == "color", ]$end[1])

            #extracting the color
            # color = stringr::str_extract(md_text, md_start)
            color = regex_extract(md_text, md_start)

            color= gsub(color, pattern="<color:", replacement="")
            color= gsub(color, pattern=">", replacement="")

            tmp_props = c(tmp_props, paste('color = "', color, '"', sep=""))
          }
          if(md_name == "shading_color"){
            # pulling out the color markdown text. It uses the first entry so
            # the outer most. There shouldn't be more than one.
            md_text = substr(pgraph,
                             start = gr_md[gr_md$md_name == "shading_color", ]$start[1],
                             stop  = gr_md[gr_md$md_name == "shading_color", ]$end[1])

            #extracting the color
            #color = stringr::str_extract(md_text, md_start)
            color = regex_extract(md_text, md_start)
            color = gsub(color, pattern="<shade:", replacement="")
            color = gsub(color, pattern=">", replacement="")

            tmp_props = c(tmp_props, paste('shading.color = "', color, '"', sep=""))
          }

          if(md_name == "font_family"){
            md_text = substr(pgraph,
                             start = gr_md[gr_md$md_name == "font_family", ]$start[1],
                             stop  = gr_md[gr_md$md_name == "font_family", ]$end[1])

            #extracting the font family
            #ff = stringr::str_extract(md_text, md_start)
            ff = regex_extract(md_text, md_start)
            ff = gsub(ff, pattern="<ff:", replacement="")
            ff = gsub(ff, pattern=">", replacement="")
            tmp_props = c(tmp_props, paste('font.family = "', ff, '"', sep=""))
          }

        }

        pele[[paste('p_', pele_idx, sep="")]]$props     = tmp_props
        pele[[paste('p_', pele_idx, sep="")]]$props_cmd = paste("prop=officer::fp_text(", paste(tmp_props, collapse=", "), ")", sep="")

        pele_idx = pele_idx + 1


        # If we're at the last group and it doesn't go to the end we add the
        # last part as well
        if(group == max(unique(locs$group))){
          # First we get the last piece of text:
          text_end = substr(pgraph, start=(gr_md[1, ]$end+1), stop=nchar(pgraph))
          # If that string isn't empty we add a paragraph element for it
          if(text_end != ""){
            pele[[paste('p_', pele_idx, sep="")]] =
              list(text      = text_end,
                   props     = c("NULL"),
                   props_cmd = "prop=NULL")
            pele_idx = pele_idx + 1
          }
        }

      }

      for(loc_idx in 1:nrow(locs)){
        tmpstr = paste(rep(" ", nchar(pgraph)), collapse="")
        tmpstr = paste(tmpstr, ":",  locs[loc_idx, ]$md_name, sep="")
        substr(tmpstr, locs[loc_idx, ]$start, locs[loc_idx, ]$start)  = "|"
        substr(tmpstr, locs[loc_idx, ]$end  , locs[loc_idx, ]$end  )  = "|"
        md_visual = c(md_visual, pgraph, tmpstr)
      }
    }


    fpar_cmd = ""
    for(tmpele in pele){
      if(fpar_cmd != ""){
        fpar_cmd = paste(fpar_cmd, ',\n') }
      fpar_cmd = paste(fpar_cmd, 'officer::ftext("', tmpele$text, '", ', tmpele$props_cmd, ')', sep="")
    }
    fpar_cmd = paste("officer::fpar(", fpar_cmd, ")", sep="")

    pgraphs_parse[[paste("pgraph_", pgraph_idx, sep="")]]$pele      = pele
    pgraphs_parse[[paste("pgraph_", pgraph_idx, sep="")]]$locs      = locs
    pgraphs_parse[[paste("pgraph_", pgraph_idx, sep="")]]$md_visual = md_visual
    pgraphs_parse[[paste("pgraph_", pgraph_idx, sep="")]]$fpar_cmd  = fpar_cmd

    pgraph_idx = pgraph_idx + 1
  }
  pgraphs_parse}
