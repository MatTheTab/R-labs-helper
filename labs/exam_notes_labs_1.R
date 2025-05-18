# Exercises -> Labs 1

## Exercise 1

library(MixGHD)

data("banknote")
banknote

### Boxplots
x1 = banknote$Length
x1_genuine = x1[banknote$Status == "genuine"]
x1_counter = x1[banknote$Status == "counterfeit"]

boxplot(x1_genuine, x1_counter, col = c("green", "red"), names = c("Real", "Counterfeit"), main="Length Distribution", xlab="Real vs Counterfeit", ylab="Length")

multi_boxplot_by_group <- function(data, group_col, xlabs = NULL, ylabs = NULL, titles = NULL, colors = NULL, group_labels = NULL) {
  group <- as.factor(data[[group_col]])
  group_levels <- levels(group)
  n_groups <- length(group_levels)
  
  # Handle color input
  if (is.null(colors)) {
    colors <- rainbow(n_groups)
  } else if (length(colors) < n_groups) {
    warning("Not enough colors provided; recycling colors.")
    colors <- rep(colors, length.out = n_groups)
  }
  
  # Handle x-axis group names
  if (is.null(group_labels)) {
    group_labels <- group_levels
  } else if (length(group_labels) != n_groups) {
    stop("Length of 'group_labels' must match the number of groups.")
  }
  
  # Get numeric columns excluding group_col
  cols_to_plot <- setdiff(names(data)[sapply(data, is.numeric)], group_col)
  
  for (i in seq_along(cols_to_plot)) {
    colname <- cols_to_plot[i]
    values <- data[[colname]]
    split_values <- split(values, group)
    
    main_title <- if (!is.null(titles)) titles[i] else paste(colname, "Distribution")
    xlabel     <- if (!is.null(xlabs))  xlabs[i]   else paste(group_col, "Groups")
    ylabel     <- if (!is.null(ylabs))  ylabs[i]   else colname
    
    boxplot(split_values,
            col = colors,
            names = group_labels,
            main = main_title,
            xlab = xlabel,
            ylab = ylabel)
  }
}


par(mfrow=c(3, 2))
multi_boxplot_by_group(banknote, "Status")

par(mfrow=c(3, 2))
multi_boxplot_by_group(banknote, 
                       group_col = "Status",
                       xlabs = rep("Status Group", 5),
                       ylabs = c("Length", "Left", "Right", "Bottom", "Top"),
                       titles = c("Length", "Left", "Right", "Bottom", "Top", "Diagonal"))


### Boxplots with Iris to double-check
par(mfrow=c(2, 2))
multi_boxplot_by_group(iris, "Species", colors = c("blue", "gold", "grey"))

### Boxplot shows:
# - Box shows the interquartile range (IQR): from the 25th percentile (Q1) to the 75th percentile (Q3).
# - Horizontal line inside the box: the median (50th percentile).
# - "Whiskers" extend to the smallest/largest values within 1.5 × IQR from Q1 and Q3.
# - Points outside whiskers: outliers.
# - Boxplots are useful for spotting skewness, spread, and outliers in each group.


### Histograms
par(mfrow=c(2, 2))
hist(banknote$Diagonal, breaks=8, xlab="Diagonal Value", ylab="Distribution", main="Histogram of banknote")
hist(banknote$Left, breaks=8, xlab="Left Value", ylab="Distribution", main="Histogram of banknote")
hist(banknote$Top, breaks=8, xlab="Top Value", ylab="Distribution", main="Histogram of banknote")
hist(banknote$Length, breaks=8, xlab="Length Value", ylab="Distribution", main="Histogram of banknote")

par(mfrow=c(1, 2))
x_c = banknote$Length
y_c = banknote$Right
z = table(x_c, y_c)
hist3D(z=z, border="black", xlab="Length", ylab="Right", zlab="Distribution", main="3D histogram length vs right")

x_c = banknote$Length
y_c = banknote$Status
z = table(x_c, y_c)
hist3D(z=z, border="black", xlab="Length", ylab="Status", zlab="Distribution", main="3D histogram length vs status")

### Histograms show:
# - distribution of data 

### Violin Plots
library(ggplot2)
par(mfrow=c(1, 1))
p <- ggplot(banknote, aes(x = Status, y = Length, fill = Status)) + 
  geom_violin(trim = FALSE, alpha = 0.6) + 
  geom_boxplot(width = 0.1, outlier.shape = NA) + 
  labs(title = "Violin Plot of Length by Status", 
       x = "Status", y = "Length") +
  theme_minimal() + 
  scale_fill_manual(values = c("genuine" = "lightblue", "counterfeit" = "pink")) +
  geom_jitter(height = 0.1, width = 0.1)
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

### Violin plots show:
# - distribution of data

### Scatterplot

par(mfrow = c(1, 1))
plot(banknote$Left, banknote$Right, type="p", col = c("blue", "red"), pch=20, main="Scatter Plot", xlab = "Length", ylab = "Right")
scatter3D(banknote$Left, banknote$Right, banknote$Diagonal, col=banknote$Status)
plot(banknote[,-1], col=banknote$Status)
plot(iris, col=iris$Species)

# Scatter plots shows:
# - individual examples
# - distribution
# - outliers

### Andrews
library(andrews)
andrews(banknote, clr="Status", type=4)
andrews(iris, clr="Species", type=4)

### Faces
library("aplpack")
faces(banknote[50:100,-1], face.type=1, fill=T)

### Stars
stars(banknote[50:100, -1],key.loc = c(0, 10), draw.segments=T)

### Heatmaps
library(heatmaply)
heatmaply_cor(cor(banknote[,-1]), revC=T)

### Hetampas like this show:
# - correlation (dependecies) between data attributes

## Exercise 2

setwd("C:/Users/mateu/OneDrive/Pulpit/R_labs_Master_Degree_Studies")
wine_red = read.csv("winequality-red.csv", sep=";")
wine_white = read.csv("winequality-white.csv", sep=";")
wine_red$type = "red"
wine_white$type = "white"
wine = rbind(wine_red, wine_white)

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


## Exercise 3
library(HSAUR2)
USairpollution

boxplot(USairpollution)

library(ggplot2)
library(HSAUR2)

p1 <- ggplot(USairpollution, aes(x = popul, y = manu)) +
  geom_point(color = "red") +
  geom_rug(sides = "b", aes(x = popul)) +
  geom_rug(sides = "l", aes(y = manu)) +
  labs(title = "Population vs Manufacturing") +
  theme_minimal()

p2 <- ggplot(USairpollution, aes(x = manu, y = SO2)) +
  geom_point(color = "pink") +
  geom_rug(sides = "b", aes(x = manu)) +
  geom_rug(sides = "l", aes(y = SO2)) +
  labs(title = "Manufacturing vs SO2") +
  theme_minimal()

p1 + p2


parallel_abline <- function(x, y, ...){
  points(x,y, ...)
  abline(lm(x~y), col="red", lwd=1)
}

pairs(USairpollution, panel = parallel_abline)
heatmaply_cor(cor(USairpollution), main="Heatmap", revC=T)

labels <- rownames(USairpollution)
par(mfrow= c(2, 2))
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

