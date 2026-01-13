data<-read.csv("AssignmentData.csv", sep = ";", header = TRUE)

#First, each plot is generated separately. Then, all plots are
#displayed together in a single plot. 
#Since the final plot is large, R automatically zooms it in a separate tab.

#for each plot, the comment is at the end of the relevant code

#Converting all columns to numeric
data_num <- as.data.frame(
  lapply(data, function(col) as.numeric(col))
)

# plot 1
xname <- "away" #column name to be used for the x-axis
yname <- "away.1" #column name to be used for the y-axis

#Selecting two columns and creating a new data frame
pair1 <- data_num[, c(xname, yname)]

#Removing rows with NA values completely
pair1 <- pair1[complete.cases(pair1), ]

#Extracting columns as vectors
x <- pair1[[xname]]
y <- pair1[[yname]]

mean_x <- mean(x)
sd_x   <- sd(x)
corr_xy <- cor(x, y)

mean_x
sd_x
corr_xy

plot(x, y,
     xlab = "x-axis title",
     ylab = "y-axis title",
     main = "Dataset: away vs away.1",
     pch = 16,
     col = "blue",
     axes = FALSE)

axis(1, labels = TRUE,  tck = 0.05)   
axis(2, labels = TRUE,  tck = 0.05)
axis(3, labels = FALSE, tck = 0.05)
axis(4, labels = FALSE, tck = 0.05)

box()

text(x = min(x)*0.90, y = max(y)*0.90,
     labels = "191701021",
     pos = 4)

text(x = max(x)*0.92, 
     y = max(y),
     labels = paste(
       "SD\u2093 =",   round(sd_x, 2),
       "\nMean\u2093 =", round(mean_x, 2),
       "\nCorr =",  round(corr_xy, 3)
     ),
     pos = 1,)
#The data is completely randomly distributed. There is no distinct pattern.
#A noisy distribution formation is observed.
#There is no linear relationship between x and y.
#SD, with a value of 16.74 relative to the 0–100 range, indicates a moderate level of dispersion.

#from this point on, the following steps are the versions of plot 1’s code applied to the other pairs
# plot 2
xname <- "bullseye"
yname <- "bullseye.1"

pair2 <- data_num[, c(xname, yname)]
pair2 <- pair2[complete.cases(pair2), ]

x <- pair2[[xname]]
y <- pair2[[yname]]

mean_x  <- mean(x)
sd_x    <- sd(x)
corr_xy <- cor(x, y)

mean_x
sd_x
corr_xy

plot(x, y,
     xlab = "x-axis title",
     ylab = "y-axis title",
     main = "Dataset: bullseye vs bullseye.1",
     pch = 16,
     col = "orange",
     axes = FALSE)

axis(1, labels = TRUE,  tck = 0.05)   
axis(2, labels = TRUE,  tck = 0.05)
axis(3, labels = FALSE, tck = 0.05)
axis(4, labels = FALSE, tck = 0.05)

box()

text(x = min(x)*0.90, y = max(y)*0.90,
     labels = "191701021",
     pos = 4)

text(x = max(x)*0.92, 
     y = max(y),
     labels = paste(
       "SD\u2093 =",   round(sd_x, 2),
       "\nMean\u2093 =", round(mean_x, 2),
       "\nCorr =",  round(corr_xy, 3)
     ),
     pos = 1)
#Since the data exhibits a circular distribution, it does not show a meaningful linear relationship.
#Therefore, the correlation is close to zero.
#SD again has a moderate level of dispersion, and the data contains a geometric relationship.

# plot 3
xname <- "circle"
yname <- "circle.1"

pair3 <- data_num[, c(xname, yname)]
pair3 <- pair3[complete.cases(pair3), ]

x <- pair3[[xname]]
y <- pair3[[yname]]

mean_x  <- mean(x)
sd_x    <- sd(x)
corr_xy <- cor(x, y)

plot(x, y,
     xlab = "x-axis title",
     ylab = "y-axis title",
     main = "Dataset: circle vs circle.1",
     pch = 16,
     col = "red",
     axes = FALSE)

axis(1, labels = TRUE,  tck = 0.05)
axis(2, labels = TRUE,  tck = 0.05)
axis(3, labels = FALSE, tck = 0.05)
axis(4, labels = FALSE, tck = 0.05)

box()

text(x = min(x)*0.90, y = max(y)*0.90,
     labels = "191701021",
     pos = 4)

text(x = max(x)*0.92, 
     y = max(y),
     labels = paste(
       "SD\u2093 =",   round(sd_x, 2),
       "\nMean\u2093 =", round(mean_x, 2),
       "\nCorr =",  round(corr_xy, 3)
     ),
     pos = 1)
#In this structure as well, since there is no linear relationship, the correlation turned out to be low
#Again, as a result of the geometric shape, SD turned out to be at a moderate level

# plot 4
xname <- "dots"
yname <- "dots.1"

pair4 <- data_num[, c(xname, yname)]
pair4 <- pair4[complete.cases(pair4), ]

x <- pair4[[xname]]
y <- pair4[[yname]]

mean_x  <- mean(x)
sd_x    <- sd(x)
corr_xy <- cor(x, y)

plot(x, y,
     xlab = "x-axis title",
     ylab = "y-axis title",
     main = "Dataset: dots vs dots.1",
     pch = 16,
     col = "purple",
     axes = FALSE)

axis(1, labels = TRUE,  tck = 0.05)
axis(2, labels = TRUE,  tck = 0.05)
axis(3, labels = FALSE, tck = 0.05)
axis(4, labels = FALSE, tck = 0.05)

box()

text(x = min(x)*0.90, y = max(y)*0.90,
     labels = "191701021",
     pos = 4)

text(x = max(x)*0.92, 
     y = max(y),
     labels = paste(
       "SD\u2093 =",   round(sd_x, 2),
       "\nMean\u2093 =", round(mean_x, 2),
       "\nCorr =",  round(corr_xy, 3)
     ),
     pos = 1)
#The data has a clearly "clustered" structure. This irregular group structure disrupts the linear relationship
#and causes the correlation to be close to zero.
#Because the distance between clusters increases the data spread, SD is at a moderate level.

# plot 5
xname <- "h_lines"
yname <- "h_lines.1"

pair5 <- data_num[, c(xname, yname)]
pair5 <- pair5[complete.cases(pair5), ]

x <- pair5[[xname]]
y <- pair5[[yname]]

mean_x  <- mean(x)
sd_x    <- sd(x)
corr_xy <- cor(x, y)

plot(x, y,
     xlab = "x-axis title",
     ylab = "y-axis title",
     main = "Dataset: h_lines vs h_lines.1",
     pch = 16,
     col = "darkgreen",
     axes = FALSE)

axis(1, labels = TRUE,  tck = 0.05)
axis(2, labels = TRUE,  tck = 0.05)
axis(3, labels = FALSE, tck = 0.05)
axis(4, labels = FALSE, tck = 0.05)

box()

text(x = min(x)*0.90, y = max(y)*0.90,
     labels = "191701021",
     pos = 4)

text(x = max(x)*0.92, 
     y = max(y),
     labels = paste(
       "SD\u2093 =",   round(sd_x, 2),
       "\nMean\u2093 =", round(mean_x, 2),
       "\nCorr =",  round(corr_xy, 3)
     ),
     pos = 1)
#In the data, the y variable is clustered at several constant levels.
#However, x is spread over a wide range across bands.
#Similarly, there is no linear relationship, and the correlation value is close to zero.

# plot 6
xname <- "high_lines"
yname <- "high_lines.1"

pair6 <- data_num[, c(xname, yname)]
pair6 <- pair6[complete.cases(pair6), ]

x <- pair6[[xname]]
y <- pair6[[yname]]

mean_x  <- mean(x)
sd_x    <- sd(x)
corr_xy <- cor(x, y)

plot(x, y,
     xlab = "x-axis title",
     ylab = "y-axis title",
     main = "Dataset: high_lines vs high_lines.1",
     pch = 16,
     col = "brown",
     axes = FALSE)

axis(1, labels = TRUE,  tck = 0.05)
axis(2, labels = TRUE,  tck = 0.05)
axis(3, labels = FALSE, tck = 0.05)
axis(4, labels = FALSE, tck = 0.05)

box()

text(x = min(x)*0.90, y = max(y)*0.90,
     labels = "191701021",
     pos = 4)

text(x = max(x)*0.92, 
     y = max(y),
     labels = paste(
       "SD\u2093 =",   round(sd_x, 2),
       "\nMean\u2093 =", round(mean_x, 2),
       "\nCorr =",  round(corr_xy, 3)
     ),
     pos = 1)
#While there is a wide distribution along the x-axis, the y values are positioned at high levels.
#Due to the structural characteristics, the correlation value is again close to zero and there is no linear relationship.

# plot 7
xname <- "slant_down"
yname <- "slant_down.1"

pair7 <- data_num[, c(xname, yname)]
pair7 <- pair7[complete.cases(pair7), ]

x <- pair7[[xname]]
y <- pair7[[yname]]

mean_x  <- mean(x)
sd_x    <- sd(x)
corr_xy <- cor(x, y)

plot(x, y,
     xlab = "x-axis title",
     ylab = "y-axis title",
     main = "Dataset: slant_down vs slant_down.1",
     pch = 16,
     col = "darkred",
     axes = FALSE)

axis(1, labels = TRUE,  tck = 0.05)
axis(2, labels = TRUE,  tck = 0.05)
axis(3, labels = FALSE, tck = 0.05)
axis(4, labels = FALSE, tck = 0.05)

box()

text(x = min(x)*0.90, y = max(y)*0.90,
     labels = "191701021",
     pos = 4)

text(x = max(x)*0.92, 
     y = max(y),
     labels = paste(
       "SD\u2093 =",   round(sd_x, 2),
       "\nMean\u2093 =", round(mean_x, 2),
       "\nCorr =",  round(corr_xy, 3)
     ),
     pos = 1)
#The data set forms a negatively sloped linear structure. Therefore, the correlation value
#confirms a moderate level of relationship. Overall, this dataset is the most meaningful so far.

# plot 8
xname <- "slant_up"
yname <- "slant_up.1"

pair8 <- data_num[, c(xname, yname)]
pair8 <- pair8[complete.cases(pair8), ]

x <- pair8[[xname]]
y <- pair8[[yname]]

mean_x  <- mean(x)
sd_x    <- sd(x)
corr_xy <- cor(x, y)

plot(x, y,
     xlab = "x-axis title",
     ylab = "y-axis title",
     main = "Dataset: slant_up vs slant_up.1",
     pch = 16,
     col = "goldenrod",
     axes = FALSE)

axis(1, labels = TRUE,  tck = 0.05)
axis(2, labels = TRUE,  tck = 0.05)
axis(3, labels = FALSE, tck = 0.05)
axis(4, labels = FALSE, tck = 0.05)

box()

text(x = min(x)*0.90, y = max(y)*0.90,
     labels = "191701021",
     pos = 4)

text(x = max(x)*0.92, 
     y = max(y),
     labels = paste(
       "SD\u2093 =",   round(sd_x, 2),
       "\nMean\u2093 =", round(mean_x, 2),
       "\nCorr =",  round(corr_xy, 3)
     ),
     pos = 1)
#The data appears to be a positively correlated version of the previous dataset.
#The correlation value being positive and far from zero also indicates a moderate level of meaningful difference.
#An SD value of 19 indicates that the data is spread over a wide range.

# plot 9
xname <- "star"
yname <- "star.1"

pair9 <- data_num[, c(xname, yname)]
pair9 <- pair9[complete.cases(pair9), ]

x <- pair9[[xname]]
y <- pair9[[yname]]

mean_x  <- mean(x)
sd_x    <- sd(x)
corr_xy <- cor(x, y)

plot(x, y,
     xlab = "x-axis title",
     ylab = "y-axis title",
     main = "Dataset: star vs star.1",
     pch = 16,
     col = "darkblue",
     axes = FALSE)

axis(1, labels = TRUE,  tck = 0.05)
axis(2, labels = TRUE,  tck = 0.05)
axis(3, labels = FALSE, tck = 0.05)
axis(4, labels = FALSE, tck = 0.05)

box()

text(x = min(x)*0.90, y = max(y)*0.90,
     labels = "191701021",
     pos = 4)

text(x = max(x)*0.92, 
     y = max(y),
     labels = paste(
       "SD\u2093 =",   round(sd_x, 2),
       "\nMean\u2093 =", round(mean_x, 2),
       "\nCorr =",  round(corr_xy, 3)
     ),
     pos = 1)
#The structure is clearly not linear, but due to the star-shaped arms with positive and negative slopes
#neutralizing each other, the correlation value turned out to be very close to zero.
#SD tells us that the data is evenly distributed in different directions.

# plot 10
xname <- "v_lines"
yname <- "v_lines.1"

pair10 <- data_num[, c(xname, yname)]
pair10 <- pair10[complete.cases(pair10), ]

x <- pair10[[xname]]
y <- pair10[[yname]]

mean_x  <- mean(x)
sd_x    <- sd(x)
corr_xy <- cor(x, y)

plot(x, y,
     xlab = "x-axis title",
     ylab = "y-axis title",
     main = "Dataset: v_lines vs v_lines.1",
     pch = 16,
     col = "cyan4",
     axes = FALSE)

axis(1, labels = TRUE,  tck = 0.05)
axis(2, labels = TRUE,  tck = 0.05)
axis(3, labels = FALSE, tck = 0.05)
axis(4, labels = FALSE, tck = 0.05)

box()

text(x = min(x)*0.90, y = max(y)*0.90,
     labels = "191701021",
     pos = 4)

text(x = max(x)*0.92, 
     y = max(y),
     labels = paste(
       "SD\u2093 =",   round(sd_x, 2),
       "\nMean\u2093 =", round(mean_x, 2),
       "\nCorr =",  round(corr_xy, 3)
     ),
     pos = 1)
#since the structural characteristic of the data completely prevents linearity
#the correlation value is very close to zero. It is not linear, but a vertical band structure
#containing distinct level groups.

# plot 11
xname <- "wide_lines"
yname <- "wide_lines.1"

pair11 <- data_num[, c(xname, yname)]
pair11 <- pair11[complete.cases(pair11), ]

x <- pair11[[xname]]
y <- pair11[[yname]]

mean_x  <- mean(x)
sd_x    <- sd(x)
corr_xy <- cor(x, y)

plot(x, y,
     xlab = "x-axis title",
     ylab = "y-axis title",
     main = "Dataset: wide_lines vs wide_lines.1",
     pch = 16,
     col = "darkorchid4",
     axes = FALSE)

axis(1, labels = TRUE,  tck = 0.05)
axis(2, labels = TRUE,  tck = 0.05)
axis(3, labels = FALSE, tck = 0.05)
axis(4, labels = FALSE, tck = 0.05)

box()

text(x = min(x)*0.90, y = max(y)*0.90,
     labels = "191701021",
     pos = 4)

text(x = max(x)*0.92, 
     y = max(y),
     labels = paste(
       "SD\u2093 =",   round(sd_x, 2),
       "\nMean\u2093 =", round(mean_x, 2),
       "\nCorr =",  round(corr_xy, 3)
     ),
     pos = 1)
#Again, similar to the previous data, the vertical positioning and the fact that the points
#are positioned as a few large blocks eliminate the linear relationship. Therefore, the correlation value is again
#very close to zero. SD is close to a value of 19, which is due to this structure being spread over a wide area.

# plot 12
xname <- "dino"
yname <- "dino.1"

pair12 <- data_num[, c(xname, yname)]
pair12 <- pair12[complete.cases(pair12), ]

x <- pair12[[xname]]
y <- pair12[[yname]]

mean_x  <- mean(x)
sd_x    <- sd(x)
corr_xy <- cor(x, y)

plot(x, y,
     xlab = "x-axis title",
     ylab = "y-axis title",
     main = "Dataset: dino vs dino.1",
     pch = 16,
     col = "black",
     axes = FALSE)

axis(1, labels = TRUE,  tck = 0.05)
axis(2, labels = TRUE,  tck = 0.05)
axis(3, labels = FALSE, tck = 0.05)
axis(4, labels = FALSE, tck = 0.05)

box()

text(x = min(x)*0.90, y = max(y)*0.90,
     labels = "191701021",
     pos = 4)

text(x = max(x)*0.92, 
     y = max(y),
     labels = paste(
       "SD\u2093 =",   round(sd_x, 2),
       "\nMean\u2093 =", round(mean_x, 2),
       "\nCorr =",  round(corr_xy, 3)
     ),
     pos = 1)
#The data deliberately forms a dinosaur silhouette. The correlation value is very close to zero,
#but this does not indicate that the data is meaningless; it results from the neutralization of
#positive and negative trends in different regions.
#Again, SD indicates that the data is spread around the mean with a certain width
#but does not provide information about the shape.

#FINAL COMMENT
#From all of this, we understand that summary statistics alone may be insufficient
#to reveal the true structure of the data. Sometimes, not only focusing on numerical values
#but also visualizing the data is necessary. The dinosaur dataset has been a strong example of this.

#drawing all plots in the same place
windows(width = 12, height = 16)
par(mfrow = c(3, 4))

pairs <- c(
  "away", "away.1",
  "bullseye", "bullseye.1",
  "circle", "circle.1",
  "dots", "dots.1",
  "h_lines", "h_lines.1",
  "high_lines", "high_lines.1",
  "slant_down", "slant_down.1",
  "slant_up", "slant_up.1",
  "star", "star.1",
  "v_lines", "v_lines.1",
  "wide_lines", "wide_lines.1",
  "dino", "dino.1"
)

colors <- c(
  "blue", "orange", "red", "purple",
  "darkgreen", "brown", "darkred", "goldenrod",
  "darkblue", "cyan4", "darkorchid4", "black"
)

c_index <- 1

for (i in seq(1, length(pairs), by = 2)) {
  
  xname <- pairs[i]
  yname <- pairs[i + 1]
  
  pair <- data_num[, c(xname, yname)]
  pair <- pair[complete.cases(pair), ]
  
  x <- pair[[xname]]
  y <- pair[[yname]]
  
  mean_x  <- mean(x)
  sd_x  <- sd(x)
  corr_xy <- cor(x, y)
  
  plot(x, y,
       xlab = "x-axis title",
       ylab = "y-axis title",
       main = paste("Dataset:", xname, "vs", yname),
       pch = 16,
       col = colors[c_index],
       axes = FALSE)
  
  axis(1, labels = TRUE,  tck = 0.05)
  axis(2, labels = TRUE,  tck = 0.05)
  axis(3, labels = FALSE, tck = 0.05)
  axis(4, labels = FALSE, tck = 0.05)
  
  box()
  
  text(x = min(x)*0.90, y = max(y)*0.90,
       labels = "191701021",
       pos = 4)
  
  text(x = max(x)*0.92, 
       y = max(y),
       labels = paste(
         "SD\u2093 =",   round(sd_x, 2),
         "\nMean\u2093 =", round(mean_x, 2),
         "\nCorr =",  round(corr_xy, 3)
       ),
       pos = 1)
  
  c_index <- c_index + 1
}


