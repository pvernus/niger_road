
# Inserts a pattern at a specific position in string, see: https://statisticsglobe.com/insert-character-pattern-in-string-r
str_insert <- function(x, pos, insert) {       
  gsub(paste0("^(.{", pos, "})(.*)$"),
       paste0("\\1", insert, "\\2"),
       x)
}

