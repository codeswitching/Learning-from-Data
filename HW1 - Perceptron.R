# HW1 - Perceptron
# Lauren Steely

library(tidyverse)

# FUNCTION isLeft ---------------------------------------------------------

# Returns +1 if pt (cx, cy) is to left of line defined by (a[1], a[2]) and (b[1], b[2]), -1 otherwise

isLeft <- function(a, b, cx, cy) {
  ifelse(((b[1] - a[1]) * (cy - a[2]) - (b[2] - a[2]) * (cx - a[1])) > 0, +1, -1) # examine sign of AC x AB
  }

# PERCEPTRON --------------------------------------------------------------

maxruns <- 1 # number of times to run perceptron with fresh random data
n <- 100      # number of observations
iterations <- rep(0, maxruns) # initialize the iterations vector

#for (run in 1:maxruns) {
  
  # Generate input data x

  x <- data.frame(x0 = rep(1, n), x1 = runif(n, -1, 1), x2 = runif(n, -1, 1)) # generate x from uniform dist {-1, 1}

  # Choose a target function as a random line in the plane passing through p1 and p2.
  # This will create a linear separation between the data.

  p1 <- runif(2, -1, 1) # first point
  p2 <- runif(2, -1, 1) # second point
  m <- (p1[2] - p2[2]) / (p1[1] - p2[1]) # slope of line
  b <- p1[2] - p1[1] * m # y-intercept of line
  x$y <- isLeft(p1, p2, x$x1, x$x2) # Apply target function to input points to generate y

  # Plot the data

  inputplot <- ggplot(x, aes(x1, x2, color = as.factor(y))) + geom_point(size=2.5) +
   geom_abline(slope = m, intercept = b, size = 1.2, color = "gray60") +
    theme(legend.position="none")
  inputplot

  # Initialize y, x, and w matrices

  y <- x$y # convert y to a vector
  y.guess <- rep(1, n) # initialize vector of predictions
  x.mat <- matrix(c(x$x0, x$x1, x$x2), nrow = 3, ncol = n, byrow = T) # convert x df to matrix (observations -> columns, factors -> rows)
  w <- c(0, 0, 0) # initialize vector of weights

  # Implement the Perceptron algorithm

  w.history <- data.frame(w0 = 0, w1 = 0, w2 = 0)
  
  iterations[run] <- 1 # initialize the iterations counter for this run
  repeat {

    # Predict new y values using weights w

    for (i in 1:n) {
      y.guess[i] <- sign(x.mat[,i] %*% w) # for each observation x(i), compute y.guess as dot product of x and w
    }

    if (identical(y.guess, y)) break # repeat until y.guess == y
  
    # Choose a point randomly from the set of misclassified points

    j <- sample(which(y.guess != y), 1) # randomly select a j for which y.guess[j] != y[j]

    # Update the weight vector

    w <- w + y[j] * x.mat[,j] # y[j] is +1 or -1

    iterations[run] <- iterations[run] + 1
    
    w.history <- rbind(w.history, w)
    
    # Redraw the graph
    
#    readline(prompt = "Press enter to iterate...")
#    x$y.guess <- y.guess
#    inputplot <- ggplot(x, aes(x1, x2, color = as.factor(y.guess))) + geom_point(size=2.5) +
#      geom_abline(slope = m, intercept = b, size = 1.2, color = "gray60") +
#      theme(legend.position="none")
#    inputplot

  }
#}

cat("\nCompleted", maxruns, "runs of n =", n, "points with an average of", round(mean(iterations), 0), "iterations per run.\n")

w.history$i <- 1:iterations
ggplot(w.history, aes(i, w1)) + geom_line(size=1, color = "darkred") +
  geom_line(data=w.history, aes(i, w2), size=1, color = "darkblue")
ggsave("convergence.png")

