# read input
raw_input <- readChar("input.txt", file.size("input.txt"))

# parse input to coordinates and instructions
input <- strsplit(raw_input, ",|\\]\\[|\\]\\(|\\)\\(")[[1]]
input[1] <- gsub("\\[", "", input[1])
input[length(input)] <- gsub("\\)", "", input[length(input)])
input <- as.numeric(input)

# get number of robots
N <- length(gregexpr("\\[", raw_input)[[1]])

# get initial robot state
robots <- data.frame(x = input[2*(1:N) - 1], y = input[2*(1:N)])

# form the command list
cmds <- data.frame(x = input[2*((N + 1):(length(input) / 2)) - 1], 
                   y = input[2*((N + 1):(length(input) / 2))])

# apply commands to robots
robot <- 1
res <- as.data.frame(t(apply(cmds, 1, function(vec) {
  old <- robots
  robots[robot, ] <<- robots[robot, ] + vec
  occupancy <- sum(duplicated(robots))
  j <- robot
  robot <<- robot %% N + 1
  return(c(j, as.numeric(t(old)), vec, as.numeric(t(robots)), occupancy))
})))
names(res) <- c("robot", paste0(c("x", "y"), rep(1:N, each = 2), "i"),
                "dx", "dy", paste0(c("x", "y"), rep(1:N, each = 2), "f"),
                "occupancy")

# final result
cat("Number of times all robots occupy the same location =", 
    nrow(res[res$occupancy == 2 & (res$dx != 0 | res$dy != 0), ]), "\n")

# decode secret message
image <- matrix(0, ncol = 47, nrow = 29)
for (i in 1:nrow(res)) { 
  x <- res$x1i[i] + 1
  y <- (28 - res$y1i[i]) + 1
  image[y, x] <- image[y, x] + res$occupancy[i]
}
image(t(image), col = gray((0:256)/256))
