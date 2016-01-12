library(rgl)
library(FactoMineR)
library(factoextra)

# Read the data for year 2013
dat <- read.csv("~/R/PCA_Tirtha_backup/DE-SfN_2013.csv")
dat[dat == -9999] = NA
dat <- na.omit(dat)

# Select the variables to determine relationships
ch4.var <- c("CH4_1_1_1","FCH4_1_1_1","G_1_1_1","H_1_1_1",
             "LE_1_1_1","H2O_1_1_1")

dat_ch4 <- dat[,ch4.var]
dat_ch4 <- scale(dat_ch4)
colnames(dat_ch4) <-  c("CH4","FCH4","G","H","LE","H20")

# Run principal component analysis on the selected variables
ch4.pca <- PCA(dat_ch4)

# Plot the variables on a factor map
plot(ch4.pca, choix=c("var"), col.var = "blue", 
     col.ind = "gray", scale.unit = TRUE, graph = TRUE)

# Plot the variables based on their contributions
fviz_pca_var(ch4.pca, label = "var", 
             labelsize = 3, alpha.var = "contrib") + theme_minimal()

# Obtain a 3d plot for the variables
plot3d(ch4.pca$var$coord, col="black", expand = 1.48)
text3d(ch4.pca$var$coord, texts=rownames(ch4.pca$var$coord), col="black")
coords <- NULL
for (i in 1:nrow(ch4.pca$var$coord)) {
  coords <- rbind(coords, rbind(c(0,0,0),ch4.pca$var$coord[i,]))
}
lines3d(coords, col="blue", lwd=2)

