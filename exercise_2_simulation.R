par(mfrow = c(3, 2))

#n = 10
n <- 10
nums <- runif(10, min = 0, max = 1)

a <- matrix(nums, nrow = 10, ncol = 1)

heads <- 0
tails <- 0

#If it is less than 0.5, tails; otherwise, heads
for (i in 1:10) {
  
  if (a[i,1] < 0.5) {
    tails <- tails + 1
  } else {
    heads <- heads + 1
  }
}

#For each element: create a vector where head = 1, tail = 0
head_vec <- ifelse(a[,1] >= 0.5, 1, 0)

#Sum the heads
cum_heads <- cumsum(head_vec)


proportion <- cum_heads / (1:10)

# plot
plot(1:10, proportion,
     type = "p",
     pch = 16,       
     col = "cyan",   
     cex = 0.8,      
     ylim = c(0, 1),
     xlab = "Number of coin tosses",
     ylab = "Proportion of heads",
     main = "Simulation n = 10",
     axes = FALSE)

axis(1, labels = TRUE,  tck = 0.05)
axis(2, labels = TRUE,  tck = 0.05)
axis(3, labels = FALSE, tck = 0.05)
axis(4, labels = FALSE, tck = 0.05)

box()

abline(h = 0.5, lty = 2, col = "red")

#Heads (top row)
text(x = max(1:n), y = 0.95,
     labels = paste("Heads =", heads),
     adj = c(1, 0))  #right alignment

#Tails (bottom row)
text(x = max(1:n), y = 0.05,
     labels = paste("Tails =", tails),
     adj = c(1, 0))


#n = 25
n <- 25
nums <- runif(25, min = 0, max = 1)
a <- matrix(nums, nrow = 25, ncol = 1)

heads <- 0
tails <- 0

for (i in 1:25) {
  if (a[i,1] < 0.5) {
    tails <- tails + 1
  } else {
    heads <- heads + 1
  }
}

head_vec <- ifelse(a[,1] >= 0.5, 1, 0)
cum_heads <- cumsum(head_vec)
proportion <- cum_heads / (1:25)

# plot
plot(1:25, proportion,
     type = "p",
     pch = 16,
     col = "cyan",
     cex = 0.8,
     ylim = c(0, 1),
     xlab = "Number of coin tosses",
     ylab = "Proportion of heads",
     main = "Simulation n = 25",
     axes = FALSE)

axis(1, labels = TRUE,  tck = 0.05)
axis(2, labels = TRUE,  tck = 0.05)
axis(3, labels = FALSE, tck = 0.05)
axis(4, labels = FALSE, tck = 0.05)

box()

abline(h = 0.5, lty = 2, col = "red")

#Heads (top row)
text(x = max(1:n), y = 0.95,
     labels = paste("Heads =", heads),
     adj = c(1, 0))  #right alignment

#Tails (bottom row)
text(x = max(1:n), y = 0.05,
     labels = paste("Tails =", tails),
     adj = c(1, 0))

#n = 50
n <- 50
nums <- runif(50, min = 0, max = 1)
a <- matrix(nums, nrow = 50, ncol = 1)

heads <- 0
tails <- 0

for (i in 1:50) {
  if (a[i,1] < 0.5) {
    tails <- tails + 1
  } else {
    heads <- heads + 1
  }
}

head_vec <- ifelse(a[,1] >= 0.5, 1, 0)
cum_heads <- cumsum(head_vec)
proportion <- cum_heads / (1:50)

# plot
plot(1:50, proportion,
     type = "p",
     pch = 16,
     col = "cyan",
     cex = 0.8,            
     ylim = c(0, 1),
     xlab = "Number of coin tosses",
     ylab = "Proportion of heads",
     main = "Simulation n = 50",
     axes = FALSE)

axis(1, labels = TRUE,  tck = 0.05)
axis(2, labels = TRUE,  tck = 0.05)
axis(3, labels = FALSE, tck = 0.05)
axis(4, labels = FALSE, tck = 0.05)

box()

abline(h = 0.5, lty = 2, col = "red")

#Heads (top row)
text(x = max(1:n), y = 0.95,
     labels = paste("Heads =", heads),
     adj = c(1, 0))  #right alignment

#Tails (bottom row)
text(x = max(1:n), y = 0.05,
     labels = paste("Tails =", tails),
     adj = c(1, 0))

#n = 100
n <- 100  
nums <- runif(100, min = 0, max = 1)
a <- matrix(nums, nrow = 100, ncol = 1)

heads <- 0
tails <- 0

for (i in 1:100) {
  if (a[i,1] < 0.5) {
    tails <- tails + 1
  } else {
    heads <- heads + 1
  }
}

head_vec <- ifelse(a[,1] >= 0.5, 1, 0)
cum_heads <- cumsum(head_vec)
proportion <- cum_heads / (1:100)

# plot
plot(1:100, proportion,
     type = "p",
     pch = 16,
     col = "cyan",      
     cex = 0.8,       
     ylim = c(0, 1),
     xlab = "Number of coin tosses",
     ylab = "Proportion of heads",
     main = "Simulation n = 100",
     axes = FALSE)

axis(1, labels = TRUE,  tck = 0.05)
axis(2, labels = TRUE,  tck = 0.05)
axis(3, labels = FALSE, tck = 0.05)
axis(4, labels = FALSE, tck = 0.05)

box()

abline(h = 0.5, lty = 2, col = "red")

#Heads (top row)
text(x = max(1:n), y = 0.95,
     labels = paste("Heads =", heads),
     adj = c(1, 0))  #right alignment

#Tails (bottom row)
text(x = max(1:n), y = 0.05,
     labels = paste("Tails =", tails),
     adj = c(1, 0))

#n = 250
n <- 250
nums <- runif(250, min = 0, max = 1)
a <- matrix(nums, nrow = 250, ncol = 1)

heads <- 0
tails <- 0

for (i in 1:250) {
  if (a[i,1] < 0.5) {
    tails <- tails + 1
  } else {
    heads <- heads + 1
  }
}

head_vec <- ifelse(a[,1] >= 0.5, 1, 0)
cum_heads <- cumsum(head_vec)
proportion <- cum_heads / (1:250)

# plot
plot(1:250, proportion,
     type = "p",
     pch = 16,
     col = "cyan",
     cex = 0.8,
     ylim = c(0, 1),
     xlab = "Number of coin tosses",
     ylab = "Proportion of heads",
     main = "Simulation n = 250",
     axes = FALSE)

axis(1, labels = TRUE,  tck = 0.05)
axis(2, labels = TRUE,  tck = 0.05)
axis(3, labels = FALSE, tck = 0.05)
axis(4, labels = FALSE, tck = 0.05)

box()

abline(h = 0.5, lty = 2, col = "red")

#Heads (top row)
text(x = max(1:n), y = 0.95,
     labels = paste("Heads =", heads),
     adj = c(1, 0))  #right alignment

#Tails (bottom row)
text(x = max(1:n), y = 0.05,
     labels = paste("Tails =", tails),
     adj = c(1, 0))

#n = 1000
n <- 1000
nums <- runif(1000, min = 0, max = 1)
a <- matrix(nums, nrow = 1000, ncol = 1)

heads <- 0
tails <- 0

for (i in 1:1000) {
  if (a[i,1] < 0.5) {
    tails <- tails + 1
  } else {
    heads <- heads + 1
  }
}

head_vec <- ifelse(a[,1] >= 0.5, 1, 0)
cum_heads <- cumsum(head_vec)
proportion <- cum_heads / (1:1000)

# plot
plot(1:1000, proportion,
     type = "p",
     pch = 16,
     col = "cyan",
     cex = 0.8,
     ylim = c(0, 1),
     xlab = "Number of coin tosses",
     ylab = "Proportion of heads",
     main = "Simulation n = 1000",
     axes = FALSE)

axis(1, labels = TRUE,  tck = 0.05)
axis(2, labels = TRUE,  tck = 0.05)
axis(3, labels = FALSE, tck = 0.05)
axis(4, labels = FALSE, tck = 0.05)

box()

abline(h = 0.5, lty = 2, col = "red")

#Heads (top row)
text(x = max(1:n), y = 0.95,
     labels = paste("Heads =", heads),
     adj = c(1, 0))  #right alignment

#Tails (bottom row)
text(x = max(1:n), y = 0.05,
     labels = paste("Tails =", tails),
     adj = c(1, 0))

#GRAPH COMMENT
#In the n = 10 and n = 25 simulations, the proportion of heads fluctuates significantly and large deviations
#from the true probability (0.5) are observed in the short term. In n = 50 and n = 100,
#the fluctuations begin to decrease and the proportion converges toward 0.5.
#In the n = 250 and especially n = 1000 simulations, the fluctuations almost completely disappear,
#and the proportion stabilizes around 0.5. This behavior shows that as the number of tosses increases,
#the observed proportion converges to the true probability.


#toss coin section
count_41 <- 0
count_82 <- 0

for (j in 1:1000000) {
  nums <- runif(82, min = 0, max = 1)
  head_vec <- ifelse(nums >= 0.5, 1, 0)
  heads <- sum(head_vec)
  
  if (heads == 41) {
    count_41 <- count_41 + 1
  }
  
  if (heads == 82) {
    count_82 <- count_82 + 1
  }
}

count_41
count_82

#COMMENT
#As a result of 1,000,000 simulations, the number of experiments in which exactly 41 heads occurred varied between 87,586–87,916.
#This value is extremely close to the theoretically expected mean of 86,200
#and shows a small deviation of approximately 1.5%.

#Additionally, no simulation with 82 heads was observed. This is because,
#due to an astronomically small probability such as (0.5^82),
#such an outcome is not expected to appear in 1 million simulations.

#As a result, the obtained numbers are fully consistent with the theoretical behavior
#of the binomial distribution, and the simulation is working correctly.
