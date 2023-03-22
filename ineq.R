library(tidyverse)

theil_L <- function(x) {
  #x <- x/sum(x)
  n <- length(x)
  xi <- x/sum(x)
  log_ratio <- log(xi/(1/n))
  L <- sum(xi*log_ratio)
  U <- sum((1/n)*log_ratio)
  L_star <- L - U
  return(L_star)
}

theil_T <- function(x) {
  #x <- x/sum(x)
  n <- length(x)
  xi <- x/sum(x)
  log_ratio <- log(xi/(1/n))
  T <- sum(xi*log_ratio)
  return(T)
}

theil_T_alpha1 <- function(x) {
  #x <- x/sum(x)
  n <- length(x)
  mu <- mean(x)
  T <- sum((x/mu) * log(x/mu)) / n
  return(T)
}

theil_T_alpha0 <- function(x) {
  #x <- x/sum(x)
  n <- length(x)
  mu <- mean(x)
  T <- sum(log(mu/x)) / n
  return(T)
}

gini <- function(x) {
  n <- length(x)
  x_sort <- sort(x)
  i <- 1:n
  gini_coef <- 1 - (2/n) * sum((n + 1 - i) * x_sort) / sum(x_sort)
  return(gini_coef)
}




x <- c(10, 10, 10, 50, 50, 100, 100, 100)
y <- c(1, 1, 1, 50, 50, 109, 109, 109)

# Compute indices for x
index_x <- c(theil_L(x), theil_T(x), theil_T_alpha0(x), gini(x))

# Compute indices for y
index_y <- c(theil_L(y), theil_T(y), theil_T_alpha0(y), gini(y))

# Combine results into matrix
result_matrix <- rbind(index_x, index_y)

# Add row and column labels

colnames(result_matrix) <- c("Theil L", "Theil T", "Theil T alpha 0", "Gini")
# Print matrix
result_matrix


result_matrix %>% 
  as_tibble() %>% 
  mutate(set=c("x","y")) %>% 
  pivot_longer(`Theil L`:Gini) %>% ggplot() +
  geom_point(aes(name,value, colour = set)) + theme_classic()

                                         