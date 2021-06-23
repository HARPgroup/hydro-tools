find_name <- function(haystack, needle) {
  # this fn came from https://stackoverflow.com/questions/58400176/r-find-object-by-name-in-deeply-nested-list
  if (hasName(haystack, needle)) {
    haystack[[needle]]
  } else if (is.list(haystack)) {
    for (obj in haystack) {
      ret <- Recall(obj, needle)
      if (!is.null(ret)) return(ret)
    }
  } else {
    NULL
  }
}