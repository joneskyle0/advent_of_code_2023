input <- read.table('input.txt')
cards <- input[paste0('V',seq(from = 3, to = 12))]
winners <- input[paste0('V', seq(from = 14, to = 38))]

check_winner <- function(card, winners=winners) {
  num_winners <- sum(grepl(paste0("^",card, "$"), winners))
  return(num_winners)
}


result <- data.frame(matrix(nrow = 203, ncol = 10, data = NA))
for(i in 1:nrow(cards)) {
  for(j in 1:ncol(cards)) {
  result[i,j] <- check_winner(cards[i,j], winners[i,])
  }
}
