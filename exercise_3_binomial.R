#Set a fixed seed to be able to reproduce the same randomness results
set.seed(123)
#n = 450 

n1 <- 450
#450 games, 4 throws per game, probability of getting a 6 in each throw is 1/6

g1_counts_450 <- rbinom(n1, 4, 1/6)

#Probability of getting double 6: 1/36

g2_counts_450 <- rbinom(n1, 24, 1/36)

#We mark won games as 1 and lost games as 0

g1_wins_450 <- ifelse(g1_counts_450 > 0, 1, 0)
g2_wins_450 <- ifelse(g2_counts_450 > 0, 1, 0)

#Cumulative winning ratios

g1_prop_450 <- cumsum(g1_wins_450) / (1:n1)
g2_prop_450 <- cumsum(g2_wins_450) / (1:n1)

#n = 10000 

n2 <- 10000

g1_counts_10000 <- rbinom(n2, 4, 1/6)
g2_counts_10000 <- rbinom(n2, 24, 1/36)

g1_wins_10000 <- ifelse(g1_counts_10000 > 0, 1, 0)
g2_wins_10000 <- ifelse(g2_counts_10000 > 0, 1, 0)

g1_prop_10000 <- cumsum(g1_wins_10000) / (1:n2)
g2_prop_10000 <- cumsum(g2_wins_10000) / (1:n2)

#plotting two graphs side by side:

par(mfrow = c(2, 1),
    mar   = c(4, 4, 2, 2))  


plot(1:n1, g1_prop_450,
     type = "l",
     ylim = c(0, 1),
     xlim = c(0, 500),
     xlab = "Number of games",
     ylab = "Proportion of wins",
     col = "blue",
     lwd = 2,
     axes = FALSE)
box()

axis(1, labels = TRUE,  tck = 0.05)   
axis(2, labels = TRUE,  tck = 0.05)
axis(3, labels = FALSE, tck = 0.05)
axis(4, labels = FALSE, tck = 0.05)

lines(1:n1, g2_prop_450,
      col = "red",
      lwd = 2)

abline(h = 0.5, lty = 2)

legend("topright",
       legend = c("One die", "Two dice"),
       col    = c("blue", "red"),
       lwd    = 2,
       bty    = "n")    

title("Simulation n = 450")


plot(1:n2, g1_prop_10000,
     type = "l",
     ylim = c(0, 1),
     xlab = "Number of games",
     ylab = "Proportion of wins",
     col = "blue",
     lwd = 2,
     axes = FALSE)
box()

axis(1, labels = TRUE,  tck = 0.05)   
axis(2, labels = TRUE,  tck = 0.05)
axis(3, labels = FALSE, tck = 0.05)
axis(4, labels = FALSE, tck = 0.05)

lines(1:n2, g2_prop_10000,
      col = "red",
      lwd = 2)

abline(h = 0.5, lty = 2)

legend("topright",
       legend = c("One die", "Two dice"),
       col    = c("blue", "red"),
       lwd    = 2,
       bty    = "n")    

title("Simulation n = 10000")

#COMMENT
#In this study, we compared two different dice games using multiple simulations.
#In the n = 450 simulation, the winning rates of both games fluctuate noticeably
#and remain quite close to each other. This situation is an expected result
#due to the high variance at small sample sizes.
#Since short-term fluctuations are dominant, it is not possible to distinguish
#which game is more advantageous based on only 450 games.

#However, in the n = 10,000 simulation, it is observed that the fluctuations almost completely disappear
#and the rates converge to their theoretical values. The theoretical winning probability of
#Game 1 played with a single die is 1 – (5/6)^4 ≈ 0.5177, whereas the winning probability of
#Game 2 played with two dice is 1 – (35/36)^24 ≈ 0.4914. In large samples,
#the stabilization of the lines around these values confirms the true probabilities of the games
#and clearly shows that Game 1 is more advantageous in the long run.

#As a result, while the games cannot be distinguished at the small sample size of n = 450,
#the n = 10,000 simulation clearly reveals the superiority of Game 1.
