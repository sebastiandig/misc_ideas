# ============================================================================ #
#
# ---- Results of Using BiodiversityR ----
#
# ============================================================================ #
# library(BiodiversityR)
# BiodiversityR::BiodiversityRGUI()

library(aplpack, pos=43)
with(dat.match, stem.leaf(sal_permil, na.rm=TRUE))
# Table for temperature_c:
Tapply(temperature_c ~ expocode + cruise_id, mean, na.action=na.omit, 
  data=discrt) # mean by groups
# Table for salinity_unitless:
Tapply(salinity_unitless ~ expocode + cruise_id, mean, na.action=na.omit, 
  data=discrt) # mean by groups
# Table for dic_umol_kg:
Tapply(dic_umol_kg ~ expocode + cruise_id, mean, na.action=na.omit, 
  data=discrt) # mean by groups
# Table for ta_umol_kg:
Tapply(ta_umol_kg ~ expocode + cruise_id, mean, na.action=na.omit, 
  data=discrt) # mean by groups
# Table for p_h_ts_20c:
Tapply(p_h_ts_20c ~ expocode + cruise_id, mean, na.action=na.omit, 
  data=discrt) # mean by groups
Tapply(dic_umol_kg ~ expocode, var, na.action=na.omit, data=discrt) 
  # variances by group
bartlett.test(dic_umol_kg ~ expocode, data=discrt)
local({
  .FA <- factanal(~temperature_c+salinity_unitless+dic_umol_kg, factors=1, 
  rotation="varimax", scores="none", data=discrt)
  print(.FA)
})
local({
  .PC <- princomp(~temperature_c+salinity_unitless+dic_umol_kg, cor=TRUE, 
  data=discrt)
  cat("\nComponent loadings:\n")
  print(unclass(loadings(.PC)))
  cat("\nComponent variances:\n")
  print(.PC$sd^2)
  cat("\n")
  print(summary(.PC))
})
library(rgl, pos=44)
library(nlme, pos=45)
library(mgcv, pos=45)
scatter3d(temperature_c~lat+lon, data=discrt, surface=FALSE, residuals=TRUE,
   bg="white", axis.scales=TRUE, grid=TRUE, ellipsoid=FALSE)
scatter3d(ta_umol_kg~temperature_c+salinity_unitless, data=discrt, 
  surface=FALSE, residuals=TRUE, bg="white", axis.scales=TRUE, grid=TRUE, 
  ellipsoid=FALSE)
scatter3d(ta_umol_kg~temperature_c+salinity_unitless|expocode, data=discrt, 
  surface=FALSE, residuals=TRUE, parallel=FALSE, bg="white", axis.scales=TRUE,
   grid=TRUE, ellipsoid=FALSE)
scatter3d(ta_umol_kg~temperature_c+salinity_unitless|expocode, data=dis, 
  surface=FALSE, residuals=TRUE, parallel=FALSE, bg="white", axis.scales=TRUE,
   grid=TRUE, ellipsoid=FALSE)
scatter3d(ta_umol_kg~temperature_c+salinity_unitless, data=dis, 
  surface=FALSE, residuals=TRUE, bg="white", axis.scales=TRUE, grid=TRUE, 
  ellipsoid=FALSE)
scatter3d(ta_umol_kg~temperature_c+salinity_unitless, data=dis, 
  surface=FALSE, residuals=TRUE, bg="black", axis.scales=TRUE, grid=TRUE, 
  ellipsoid=FALSE)
scatter3d(ta_umol_kg~temperature_c+salinity_unitless, data=dis, 
  fit="linear", residuals=TRUE, bg="black", axis.scales=TRUE, grid=TRUE, 
  ellipsoid=FALSE)
scatter3d(ta_umol_kg~temperature_c+salinity_unitless, data=dis, 
  fit="quadratic", residuals=TRUE, bg="black", axis.scales=TRUE, grid=TRUE, 
  ellipsoid=FALSE)
RegModel.1 <- lm(ta_umol_kg~temperature_c+salinity_unitless, data=dis)
summary(RegModel.1)
library(MASS, pos=47)
Confint(RegModel.1, level=0.95)
summary(RegModel.1)
library(survival, pos=48)
library(Formula, pos=48)
library(Hmisc, pos=48)
rcorr.adjust(dis[,c("temperature_c","salinity_unitless","dic_umol_kg",
  "ta_umol_kg","silicate_umol_kg")], type="pearson", use="pairwise.complete")
HClust.1 <- hclust(dist(model.matrix(~-1 + 
  lat+lon+temperature_c+salinity_unitless, dis)) , method= "ward")
plot(HClust.1, main= "Cluster Dendrogram for Solution HClust.1", xlab= 
  "Observation Number in Data Set dis", sub="Method=ward; Distance=euclidian")
summary(as.factor(cutree(HClust.1, k = 4))) # Cluster Sizes
by(model.matrix(~-1 + lat + lon + temperature_c + salinity_unitless, dis), 
  as.factor(cutree(HClust.1, k = 4)), colMeans) # Cluster Centroids
biplot(princomp(model.matrix(~-1 + lat + lon + temperature_c + 
  salinity_unitless, dis)), xlabs = as.character(cutree(HClust.1, k = 4)))
local({
  .FA <- factanal(~temperature_c+salinity_unitless+dic_umol_kg+ta_umol_kg, factors=1, rotation="varimax", scores="none", data=dis)
  print(.FA)
})
library(sem, pos=51)

