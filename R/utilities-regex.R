
regex_extract <- function(string,
                          pattern,
                          ignore.case = FALSE,
                          perl = FALSE,
                          fixed = FALSE,
                          useBytes = FALSE){
  regmatches(string,
             regexpr(pattern,
                     string,
                     ignore.case = ignore.case,
                     perl = perl,
                     fixed = fixed,
                     useBytes = useBytes))
}


regex_extract_all <- function(string,
                              pattern,
                              ignore.case = FALSE,
                              perl = FALSE,
                              fixed = FALSE,
                              useBytes = FALSE){
  regmatches(string,
             gregexpr(pattern,
                      string,
                      ignore.case = ignore.case,
                      perl = perl,
                      fixed = fixed,
                      useBytes = useBytes))
}


regex_replace <- function(string,
                          pattern,
                          replacement,
                          ignore.case = FALSE,
                          perl = FALSE,
                          fixed = FALSE,
                          useBytes = FALSE){

  sub(pattern = pattern,
      replacement = replacement,
      x = string,
      ignore.case = ignore.case,
      perl = perl,
      fixed = fixed,
      useBytes = useBytes)
}


regex_replace_all <- function(string,
                              pattern,
                              replacement,
                              ignore.case = FALSE,
                              perl = FALSE,
                              fixed = FALSE,
                              useBytes = FALSE){
  gsub(pattern = pattern,
       replacement = replacement,
       x = string,
       ignore.case = ignore.case,
       perl = perl,
       fixed = fixed,
       useBytes = useBytes)
}




regex_locate <- function(string,
                         pattern,
                         ignore.case = FALSE,
                         perl = FALSE,
                         fixed = FALSE,
                         useBytes = FALSE){
  res <- regexpr(pattern,
                 string,
                 ignore.case = ignore.case,
                 perl = perl,
                 fixed = fixed,
                 useBytes = useBytes)
  locate_start_end(res)
}


regex_locate_all <- function(string,
                             pattern,
                             ignore.case = FALSE,
                             perl = FALSE,
                             fixed = FALSE,
                             useBytes = FALSE){
  res <- gregexpr(pattern,
                  string,
                  ignore.case = ignore.case,
                  perl = perl,
                  fixed = fixed,
                  useBytes = useBytes)
  lapply(res, function(x) locate_start_end(x))
}


locate_start_end <- function(data){
  start <- data
  start[start == -1] <- NA
  end <- start + attr(data, "match.length") - 1
  res <- cbind(start, end)

  if(any(is.na(res))){
    res <- res[-1, ]
  }

  res
}


regex_detect <- function(string,
                         pattern,
                         ignore.case = FALSE,
                         perl = FALSE,
                         fixed = FALSE,
                         useBytes = FALSE){
  grepl(
    pattern,
    string,
    ignore.case = ignore.case,
    perl = perl,
    fixed = fixed,
    useBytes = useBytes)
}


regex_split <- function(string,
                        pattern,
                        perl = FALSE,
                        fixed = FALSE,
                        useBytes = FALSE){
  strsplit(
    string,
    pattern,
    perl = perl,
    fixed = fixed,
    useBytes = useBytes)
}
