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

count_row <- function(row) {
  rowSums(sapply(row, count_cubes), na.rm = T)
}

counts <- data.frame(t(apply(dat, 1, count_row)))

num_per_draw <- function(str) {
  num_per_draw <- as.numeric(trimws(unlist(strsplit(str, split = '[[:alpha:]]|,'))))
  num_per_draw <- num_per_draw[!is.na(num_per_draw)]
  return(sum(num_per_draw, na.rm = T))
}

a <- matrix(nrow = nrow(dat), ncol = ncol(dat))
rownames(a) <- rownames(dat)
colnames(a) <- colnames(dat)
for(i in 1:nrow(dat)) {
  for(j in 1:ncol(dat)) {
    a[i,j] <- num_per_draw(dat[i,j])
  }
}

IDs <- rownames(a[rowSums(a > 39) > 0, ])

IDs <- c(IDs, rownames(counts[counts$red<=12& counts$green<=13& counts$blue<=14,]))
IDs <- unique(IDs)

sum(as.numeric(unlist(strsplit(IDs, ' '))), na.rm = T)

is_valid <- function(game) {
  ifelse(is.na(game), return(NA),
  num_red <- as.integer(sub(".*?(\\d+)\\s*red.*", "\\1", str))
  num_green <- as.integer(sub(".*?(\\d+)\\s*green.*", "\\1", str))
  num_blue <- as.integer(sub(".*?(\\d+)\\s*blue.*", "\\1", str)))
  if(num_red > 12 | num_green > 13 | num_blue > 14) {
    return(rownames(game)) 
  }
}
