#' # Principal component analysis to determine relative linkages of Canopy level Methane Flux
#'  
#'  Load the datasets into the R environment
  dat <- read.csv("C:/Users/Avipsa/Google Drive/collaboration_PCA_methane/Methane2012_2015_cleanMethane2012_2015_clean.csv")
  dat <- na.omit(dat)
  ch4.var <- c("CH4_1_1_1","FCH4_1_1_1","G_1_1_1","H_1_1_1",
               "LE_1_1_1","H2O_1_1_1","MO_LENGTH_1_1_1",
               "NETRAD_1_1_1","PPFD_IN_1_1_1","RH_1_1_1",
               "T_CANOPY_1_1_1","T_DP_1_1_1","TA_1_1_1",
               "T_SONIC_1_1_1","TS_1_1_1","U_SIGMA_1_1_1",
               "USTAR_1_1_1","VPD_PI_1_1_1","WS_1_1_1","ZL_1_1_1")
  
  dat_ch4 <- dat[,ch4.var]
  head(dat_ch4)
  co2.var <- c("CO2_1_1_1","FC_1_1_1","G_1_1_1","H_1_1_1",
               "LE_1_1_1","H2O_1_1_1","MO_LENGTH_1_1_1",
               "NETRAD_1_1_1","PPFD_IN_1_1_1","RH_1_1_1",
               "T_CANOPY_1_1_1","T_DP_1_1_1","TA_1_1_1",
               "T_SONIC_1_1_1","TS_1_1_1","U_SIGMA_1_1_1",
               "USTAR_1_1_1","VPD_PI_1_1_1","WS_1_1_1","ZL_1_1_1")
  dat_co2 <- dat[,co2.var]
  head(dat_co2)
  
  #Find the Pearson correlation for the variables for CH4 and CO2
  p.cor1 <- round(cor(dat_ch4, use = "complete.obs"), 2)
  p.cor1
  p.cor2 <- round(cor(dat_co2, use = "complete.obs"), 2)
  p.cor2
  
  # Standardise the data set using scale()
  dat_ch4 <- scale(dat_ch4)
  
  # Rename the variables 
  colnames(dat_ch4) <-  c("CH4","FCH4","G","H","LE","H20")
  plot(dat_ch4)
  dat_co2 <- scale(dat_co2)
  plot(dat_co2)
  
  #Apply principal component analysis
  ch4_pca <- prcomp(dat_ch4, center = TRUE, scale = TRUE)
  print(ch4_pca)
  co2_pca <- prcomp(dat_co2, center = TRUE, scale = TRUE)
  print(co2_pca)
  
  # Check which principal components should be retained
  plot(ch4_pca)
  screeplot(ch4_pca, type = "l")
  round((ch4_pca$sdev)^2,4)
  
  #biplot(ch4_pca, xlabs = rep(" ", nrow(dat_ch4)), expand = 3)
  
  plot(co2_pca)
  screeplot(co2_pca, type = "l")
  round((co2_pca$sdev)^2,4)
  
  #biplot(co2_pca, xlabs = rep(" ", nrow(dat_ch4)), expand = 3)
  
  library(FactoMineR)
  ch4.pca <- PCA(dat_ch4)
  co2.pca <- PCA(dat_co2)
  
  plot(ch4.pca, choix=c("var"), col.var = "blue", col.ind = "gray", scale.unit = TRUE, graph = TRUE)
  
  library(factoextra)
  fviz_pca_var(ch4.pca, label = "var", labelsize = 3, alpha.var = "contrib") + theme_minimal()
  fviz_pca_var(ch4.pca, col.var = "contrib", jitter = list(what = "label", height = 0.2, width = 0.5)) + theme_minimal() 
  plot(co2.pca, choix=c("var"), col.var = "blue", col.ind = "gray", scale.unit = TRUE, graph = TRUE)
  
  plot3d(ch4.pca$var$coord, col="black", expand = 1.48)
  text3d(ch4.pca$var$coord, texts=rownames(ch4.pca$var$coord), col="black")
  coords <- NULL
  for (i in 1:nrow(ch4.pca$var$coord)) {
    coords <- rbind(coords, rbind(c(0,0,0),ch4.pca$var$coord[i,]))
  }
  lines3d(coords, col="blue", lwd=2)
  
  plot3d(co2.pca$var$coord, col="black", expand = 1.42)
  text3d(co2.pca$var$coord, texts=rownames(co2.pca$var$coord), col="black")
  coords <- NULL
  for (i in 1:nrow(co2.pca$var$coord)) {
    coords <- rbind(coords, rbind(c(0,0,0),co2.pca$var$coord[i,]))
  
    }
  lines3d(coords, col="blue", lwd=2)
