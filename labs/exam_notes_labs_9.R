# Exercises:
## 1 -> PCA and all the visualizations
## 2, 3, 4 -> PCA and chosen plots

#########################################################################################################################################################
# Exercise 1 -> PCA and all visualizations

iris
library(factoextra)

species_data = iris[, 5]
data = iris[, -5]
pca = prcomp(data, scale. = T) # Can be true or not
pca2 = princomp(data)
prcomp(data)$rotation
std_dev = prcomp(data)$sdev
summary(prcomp(data))
prcomp(data)$x

screeplot(prcomp(data), type = "lines")
fviz_eig(pca)
fviz_pca_var(pca, col.var = "contrib", repel = T, gradient.cols=c("orange", "blue"))
fviz_pca_var(pca2, col.var = "contrib", repel = T, gradient.cols=c("red", "blue"))
# Above plots show the impact of each feature on the PCA dimensions, like Dim1= PCA1, Dim2 = PCA2

fviz_pca_ind(pca, col.var = "contrib", repel = T)
fviz_pca_ind(pca)

scores = prcomp(data)$x
plot(scores[, 1:2], col=species_data, pch=20)

fviz_pca_biplot(pca)
fviz_pca_biplot(pca2)

plot(scores[, 1:2], col=species_data, pch=20)
sepal_length1 = as.matrix(pca$rotation)[1, 1]
sepal_length2 = as.matrix(pca$rotation)[1, 2]
sepal_width1 = as.matrix(pca$rotation)[2, 1]
sepal_width2 = as.matrix(pca$rotation)[2, 2]
petal_length1 = as.matrix(pca$rotation)[3, 1]
petal_length2 = as.matrix(pca$rotation)[3, 2]
petal_width1 = as.matrix(pca$rotation)[4, 1]
petal_width2 = as.matrix(pca$rotation)[4, 2]

# Add arrows for each component
arrows(0, 0, sepal_length1, sepal_length2,
       col = "red", length = 0.1, lwd = 1.5)
arrows(0, 0, sepal_width1, sepal_width2,
       col = "blue", length = 0.1, lwd = 1.5)
arrows(0, 0, petal_length1, petal_length2,
       col = "darkgreen", length = 0.1, lwd = 1.5)
arrows(0, 0, petal_width1, petal_width2,
       col = "purple", length = 0.1, lwd = 1.5)

# Add text labels for each component
text(sepal_length1 * 1.1, sepal_length2 * 1.1,
     labels = "Sepal.Length", col = "red", cex = 0.8)
text(sepal_width1 * 1.1, sepal_width2 * 1.1,
     labels = "Sepal.Width", col = "blue", cex = 0.8)
text(petal_length1 * 1.1, petal_length2 * 1.1,
     labels = "Petal.Length", col = "darkgreen", cex = 0.8)
text(petal_width1 * 1.1, petal_width2 * 1.1,
     labels = "Petal.Width", col = "purple", cex = 0.8)

################# Another plot #################

fviz_pca_biplot(pca, habillage = species_data, addEllipses = T, palette = c("orange", "pink", "blue"))

library(corrplot)
library(FactoMineR)
pca_facto = PCA(data)
corrplot(pca_facto$var$cor)

fviz_pca_ind(pca, habillage = species_data, addEllipses = T, ellipse.type = "convex", palette = c("orange", "pink", "blue"))

#########################################################################################################################################################
# Exercise 2 -> PCA and some visualizations

library(HSAUR2)
pottery
group = pottery[, 10]
x = pottery[, -10]
pca1 = prcomp(x, scale. = T)
pca2 = princomp(x)

screeplot(pca1, type = "lines")
fviz_pca_biplot(pca1, habillage = group, addEllipses = T, palette = c("orange", "pink", "blue", "chartreuse3", "slategray"))


#########################################################################################################################################################
# Exercise 3 -> PCA and some visualizations
library(HSAUR2)
names = rownames(smoking)
data = as.matrix(smoking[, 1:4])
pca1 = prcomp(data, scale. = T)
fviz_eig(pca1)
fviz_pca_biplot(pca1, habillage = names, addEllipses = T)


#########################################################################################################################################################
# Exercise 4 -> PCA and some visualizations
names = rownames(swiss)
data = as.matrix(swiss)
pca1 = prcomp(data, scale. = T)
fviz_eig(pca1)
fviz_pca_biplot(pca1, habillage = names, addEllipses = T)
