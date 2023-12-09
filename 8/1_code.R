input <- read.table('input.txt', skip = 1, col.names = c('lookup', '=', 'L','R'), check.names = F)
directions <- colnames(read.csv('input.txt'))

input <- input[,-2]
input$L <- gsub(".*[(]([^.]+)[,]", "\\1", input$L)
input$R <- gsub("\\).*", "\\1", input$R)
directions <- unlist(strsplit(directions, ''))

next_val <- input[1,1]
lookup_col <- NA
counter <- 0
i <- 1

next_val_tracker <- vector(mode = 'character', length = 10e6)

while(next_val != 'ZZZ') {
    lookup_col <- directions[i]
    next_val <- input[which(input$lookup==next_val), lookup_col]
    i <- i + 1
    if(i > length(directions)) { i <- 1 }
    counter <- counter + 1
    next_val_tracker[counter] <- next_val
    print(counter)
}
