data("banknote")
class(banknote)
?banknote
lab = names(banknote)
lab

#banknote = banknote[-1]

names(banknote)
x1 = banknote$Length
x2 = banknote$Left
x3 = banknote$Right
x4 = banknote$Bottom
x5 = banknote$Top
x6 = banknote$Diagonal

x1_genuine = x1[banknote$Status == "genuine"]
x1_counter = x1[banknote$Status == "counterfeit"]
x2_genuine = x2[banknote$Status == "genuine"]
x2_counter = x2[banknote$Status == "counterfeit"]
x3_genuine = x3[banknote$Status == "genuine"]
x3_counter = x3[banknote$Status == "counterfeit"]
x4_genuine = x4[banknote$Status == "genuine"]
x4_counter = x4[banknote$Status == "counterfeit"]
x5_genuine = x5[banknote$Status == "genuine"]
x5_counter = x5[banknote$Status == "counterfeit"]
x6_genuine = x6[banknote$Status == "genuine"]
x6_counter = x6[banknote$Status == "counterfeit"]

par(mfrow = c(2, 3))

boxplot(x1_genuine, x1_counter, main = "Length", xlab="Real vs Counterfeit", col=c("Light blue", "Pink"),
        names = c("Real", "Counterfeit"))

boxplot(x2_genuine, x2_counter, main = "Left", xlab="Real vs Counterfeit", col=c("Light blue", "Pink"),
        names = c("Real", "Counterfeit"))

boxplot(x3_genuine, x3_counter, main = "Right", xlab="Real vs Counterfeit", col=c("Light blue", "Pink"),
        names = c("Real", "Counterfeit"))

boxplot(x4_genuine, x4_counter, main = "Bottom", xlab="Real vs Counterfeit", col=c("Light blue", "Pink"),
        names = c("Real", "Counterfeit"))

boxplot(x5_genuine, x5_counter, main = "Top", xlab="Real vs Counterfeit", col=c("Light blue", "Pink"),
        names = c("Real", "Counterfeit"))

boxplot(x6_genuine, x6_counter, main = "Diagonal", xlab="Real vs Counterfeit", col=c("Light blue", "Pink"),
        names = c("Real", "Counterfeit"))

var_names = setdiff(names(banknote), "Status")
for (col in var_names) {
  data_genuine = banknote[[col]][banknote$Status == "genuine"]
  data_counter = banknote[[col]][banknote$Status == "counterfeit"]
  
  boxplot(data_genuine, data_counter, 
          main = col, xlab = "Real vs Counterfeit", 
          col = c("Light blue", "Pink"), names = c("Real", "Counterfeit"))
}


par(mfrow=c(1, 1))
hist(x1, breaks=8)

par(mfrow = c(2, 3))
temp_banknote = banknote[-1]
for(col in names(banknote)){
  data1 = temp_banknote[col]
  data2 = temp_banknote[col]
  data1
  boxplot(data1[col], data2[col], main = col, xlab="Real vs Counterfeit", col=c("Light blue", "Pink"),
          names = c("Real", "Counterfeit"))
}

x_c = cut(banknote[,2], 10)
y_c = cut(banknote[,3], 10)
z = table(x_c, y_c)
hist3D(z=z, border="black")
image2D(z=z, border="black")
heatmap(round(cor(z), 2), revC=T)

banknote = banknote[-1]
heatmaply_cor(cor(banknote), revC=T)

library(ggplot2)

# Create violin plot
p <- ggplot(banknote, aes(x = Status, y = Length, fill = Status)) + 
  geom_violin(trim = FALSE, alpha = 0.6) + 
  geom_boxplot(width = 0.1, outlier.shape = NA) + 
  labs(title = "Violin Plot of Length by Status", 
       x = "Status", y = "Length") +
  theme_minimal() + 
  scale_fill_manual(values = c("genuine" = "lightblue", "counterfeit" = "pink")) +
  geom_jitter(height = 0.1, width = 0.1)

# Display plot
p

library(ggplot2)
library(patchwork)

plots <- list()

for(col in names(banknote)[-which(names(banknote) == "Status")]) {
  p <- ggplot(banknote, aes(x = Status, y = .data[[col]], fill = Status)) + 
    geom_violin(trim = FALSE, alpha = 0.6) + 
    geom_boxplot(width = 0.1, outlier.shape = NA, fill = "grey", alpha=0.4) + 
    labs(title = paste("Violin Plot of", col, "by Status"),
         x = "Status", y = col) +
    theme_minimal() + 
    scale_fill_manual(values = c("genuine" = "lightblue", "counterfeit" = "pink"))
  
  plots[[col]] <- p
}

wrap_plots(plots, ncol = 3)



par(mfrow = c(1, 1))
plot(banknote$Left, banknote$Right, type="p", col = c("blue", "red"), pch=20, main="Scatter Plot", xlab = "Length", ylab = "Right")
scatter3D(banknote$Left, banknote$Right, banknote$Diagonal, col=banknote$Status)
plot(banknote[,-1], col=banknote$Status)


andrews(banknote, clr="Status", type=4)

data("iris")

andrews(iris, clr="Species", type=5)

install.packages("aplpack")
library("aplpack")

faces(banknote[50:100,-1], face.type=1, fill=T)
stars(banknote[50:100, -1],key.loc = c(0, 10), draw.segments=T)
stars

setwd("C:/Users/mateu/OneDrive/Pulpit/R_labs_Master_Degree_Studies")
wine_red = read.csv("winequality-red.csv", sep=";")
wine_white = read.csv("winequality-white.csv", sep=";")
wine_red$type = "red"
wine_white$type = "white"
wine = rbind(wine_white, wine_red)

ggplot(wine, aes(quality, color=wine$type, fill=c("red", "blue"))) + geom_bar()

barplot(table(wine_red$quality), col="red")
barplot(table(wine_white$quality), col="white")

par(mfrow= c(1, 2))
barplot(table(wine_red$quality), col=1:6, legend=T, main="Red")
barplot(table(wine_white$quality), col=1:6, legend=T, main="White")

par(mfrow= c(1, 1))
white_qual <- table(wine_white$quality)
red_qual <- table(wine_red$quality)

white_pct <- white_qual / sum(white_qual) * 100
red_pct <- red_qual / sum(red_qual) * 100
qual_pct_matrix <- rbind(white_pct, red_pct)

barplot(qual_pct_matrix,
        beside = TRUE,
        col = c("white", "red"),
        legend.text = c("White Wine", "Red Wine"),
        args.legend = list(x = "topright"),
        xlab = "Quality",
        ylab = "Percentage",
        main = "Wine Quality Distribution (Percentage)")

# POLSKA MENTIONED RAAAAH, POLSKA GUROM RAAAAAAAAAH

hist(wine_white$sulphates, col="white", freq=F)
hist(wine_red$sulphates, col="red", freq=F, add=T)

par(mfrow= c(2, 1))
boxplot(alcohol ~ quality, data=wine_white, col=2:8, main="white")
boxplot(alcohol ~ quality, data=wine_red, col=2:8, main="red")

# Task 3
data("USairpollution")
par(mfrow= c(1, 1))
USairpollution
hist(USairpollution$temp)

par(mfrow= c(1, 2))
plot(USairpollution$popul, USairpollution$manu, col=c("red"))
rug(USairpollution$popul)
rug(USairpollution$manu, side=2)
plot(USairpollution$manu, USairpollution$SO2, col=c("pink"))
rug(USairpollution$manu)
rug(USairpollution$SO2, side=2)


library(ggplot2)
library(patchwork)  # for side-by-side plots
library(HSAUR2)

# First plot
p1 <- ggplot(USairpollution, aes(x = popul, y = manu)) +
  geom_point(color = "red") +
  geom_rug(sides = "b", aes(x = popul)) +
  geom_rug(sides = "l", aes(y = manu)) +
  labs(title = "Population vs Manufacturing") +
  theme_minimal()

# Second plot
p2 <- ggplot(USairpollution, aes(x = manu, y = SO2)) +
  geom_point(color = "pink") +
  geom_rug(sides = "b", aes(x = manu)) +
  geom_rug(sides = "l", aes(y = SO2)) +
  labs(title = "Manufacturing vs SO2") +
  theme_minimal()

# Combine plots side by side
p1 + p2

parallel_abline <- function(x, y, ...){
  points(x,y, ...)
  abline(lm(x~y), col="red", lwd=1)
}

pairs(USairpollution, panel = parallel_abline)
heatmaply_cor(cor(USairpollution), main="Heatmap", revC=T)

labels <- rownames(USairpollution)
par(mfrow= c(2, 2))
#layout(matrix(c(1, 0, 2, 3), nrow=2, byrow=T), heights=c(1,3))
hist(USairpollution$manu, ylab="density")
plot(USairpollution$popul, USairpollution$manu)
text(USairpollution$popul, USairpollution$manu, labels=labels)
boxplot(USairpollution$popul)

library(MVA)
library(plotly)

par(mfrow= c(1, 1))
bvbox(USairpollution[3:4], ylab="manufatures", xlab="population", type=)
text(USairpollution$popul, USairpollution$manu, labels=rownames(USairpollution))
     
h = chull(USairpollution$manu, USairpollution$popul)
plot(USairpollution$manu, USairpollution$popul)
polygon(USairpollution$manu[h], USairpollution$popul[h])

library(gMOIP)
plotHull3D(cbind(USairpollution$popul, USairpollution$manu, USairpollution$SO2))
