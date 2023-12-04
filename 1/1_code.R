calibration_document <- read.csv('1_input.txt', header = F, stringsAsFactors = F, col.names = 'hidden_cal_val')

strReverse <- function(x) sapply(lapply(strsplit(x, NULL), rev), paste, collapse = "")

get_calibration_values <- function(x) {
  output_1 <- unlist(strsplit(x, '[[:alpha:]]'))
  output_1 <- noquote(output_1[output_1!=""])[1]
  output_1 <- if(nchar(output_1) > 1) {substring(output_1, 1, 1)} else {output_1}
  
  x <- strReverse(x)
  output_2 <- unlist(strsplit(x, '[[:alpha:]]'))
  output_2 <- noquote(output_2[output_2!=""])[1]
  output_2 <- if(nchar(output_2) > 1) {substring(output_2, 1, 1)} else {output_2}
 
  output <- as.numeric(noquote(paste(output_1, output_2, sep = '')))
  return(output)
}

calibration_document$cal_val <- NA

for(i in 1:nrow(calibration_document)) {
  calibration_document[i,2] <- get_calibration_values(calibration_document[i,1])
}

sum(calibration_document$cal_val)
