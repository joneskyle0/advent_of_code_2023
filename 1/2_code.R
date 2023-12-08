calibration_document <- read.csv('input.txt', header = FALSE, stringsAsFactors = FALSE, col.names = 'hidden_cal_val')

str_reverse <- function(x) sapply(lapply(strsplit(x, NULL), rev), paste, collapse = "")

nums <- c('one', 'two', 'three', 'four', 'five', 'six', 'seven', 'eight', 'nine')

replace_nums <- function(string) {
  
}

decoder <- function(string, pattern="[[:digit:]]") {
  digit_1 <- regmatches(string, regexpr(pattern, text=string))
  digit_2 <- regmatches(strReverse(string), regexpr(pattern, text=strReverse(string)))
  output <- as.numeric(noquote(paste(digit_1, digit_2, sep = '')))
  return(output)
}

sum(apply(calibration_document, MARGIN = 1, FUN = decoder))