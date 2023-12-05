calibration_document <- read.csv('input.txt', header = FALSE, stringsAsFactors = FALSE, col.names = 'hidden_cal_val')

str_reverse <- function(x) sapply(lapply(strsplit(x, NULL), rev), paste, collapse = "")

decoder <- function(string, pattern="[[:digit:]]") {
  digit_1 <- regmatches(string, regexpr(pattern, text=string))
  digit_2 <- regmatches(str_reverse(string), regexpr(pattern, text=str_reverse(string)))
  output <- as.numeric(noquote(paste(digit_1, digit_2, sep = '')))
  return(output)
}

sum(apply(calibration_document, MARGIN = 1, FUN = decoder))