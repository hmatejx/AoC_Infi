# read input
raw_input <- readChar("input.txt", file.size("input.txt"))

# parse input to coordinates and instructions
input <- strsplit(raw_input, ",|\\]\\[|\\]\\(|\\)\\(")[[1]]
input[1] <- gsub("\\[", "", input[1])
input[length(input)] <- gsub("\\)", "", input[length(input)])
input <- as.numeric(input)

# get number of robots
N <- length(gregexpr("\\[", raw_input)[[1]])

# set initial robot state (global variable)
robots <- data.frame(x = input[2*(1:N) - 1], y = input[2*(1:N)])

# form the command list
cmds <- data.frame(robot = 1:N,
                   dx = input[2*((N + 1):(length(input) / 2)) - 1], 
                   dy = input[2*((N + 1):(length(input) / 2))])

# apply commands to robots
res <- as.data.frame(t(apply(cmds, 1, function(cmdline) {
  robots.old <- robots
  robots[cmdline[1], ] <<- robots[cmdline[1], ] + cmdline[2:3]  # ugh! modify global state...
  crowd <- sum(duplicated(robots))
  return(c(as.numeric(t(robots.old)), as.numeric(t(robots)), crowd))
})))
names(res) <- c(paste0(c("x", "y"), rep(1:N, each = 2), "_i"),
                paste0(c("x", "y"), rep(1:N, each = 2), "_f"),
                "crowd")

# final result
cat("Number of times it's a crowd =", nrow(res[res$crowd == N - 1 & (res$dx != 0 | res$dy != 0), ]), "\n")

# decode secret message
xres <- max(res[, 2*(1:N) - 1]) + 1
yres <- max(res[, 2*(1:N)]) + 1
image <- matrix(0, ncol = xres, nrow = yres)
for (i in 1:nrow(res)) { 
  if (res$crowd[i] > 1) {
    x <- res$x1_i[i] + 1
    y <- yres - res$y1_i[i]   # flip Y
    image[y, x] <- res$crowd[i]
  }
}
image(t(image), col = c("black", "gray"))
