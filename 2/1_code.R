input <- data.frame(readLines('input.txt'))

num_matches <- 1+apply(input, 1, FUN = function(x) length(unlist(regmatches(x, gregexpr(pattern = ';', x)))))

make_max_num <- function(str) {
  if(length(str) == max(num_matches)) {
    return(str)
    } 
    else if (length(str) < max(num_matches)) {
      str <- c(str, NA)
      make_max_num(str)
    }
}
draws <- apply(input, 1, FUN = function(x) trimws(unlist(strsplit(trimws(unlist(strsplit(x, ":"))[2]), ";"))))
draws <- lapply(X = draws, FUN = make_max_num)

dat <- data.frame(do.call(rbind, draws))
colnames(dat) <- paste0('draw_num_', 1:max(num_matches))
rownames(dat) <- apply(input, 1, FUN = function(x) unlist(strsplit(x, split = ':'))[1])

count_cubes <- function(str) {
  num_red <- as.integer(sub(".*?(\\d+)\\s*red.*", "\\1", str))
  num_green <- as.integer(sub(".*?(\\d+)\\s*green.*", "\\1", str))
  num_blue <- as.integer(sub(".*?(\\d+)\\s*blue.*", "\\1", str))
  cubes <- c(red = num_red, green = num_green, blue = num_blue)
  return(cubes)
}

output <- data.frame(apply(dat, 1, count_cubes))
