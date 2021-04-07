if (require(vegan)) {
  data(mite)  # Community composition data, 70 peat cores, 35 species
  head(mite)
  # Select rows 1:30. Species 35 is absent from these rows. Transform to log
  mite.log <- log(mite[1:30,-35]+1)  # Equivalent: log1p(mite[1:30,-35])
  
  # Principal coordinate analysis and simple ordination plot
  mite.D <- vegdist(mite.log, "bray")
  head(mite.D)
  res <- pcoa(mite.D)
  res$values
  biplot(res)
  
  # Project unstandardized and standardized species on the PCoA ordination plot
  mite.log.st = apply(mite.log, 2, scale, center=TRUE, scale=TRUE)
  
  par(mfrow=c(1,2))
  biplot(res, mite.log)
  biplot(res, mite.log.st)
  
  # Reverse the ordination axes in the  plot
  par(mfrow=c(1,2))
  biplot(res, mite.log, dir.axis1=-1, dir.axis2=-1)
  biplot(res, mite.log.st, dir.axis1=-1, dir.axis2=-1)
}
# }


data(varespec)

# Open the dataset and look if you can find any patterns
View(varespec)
# It is probably very difficult to see any patterns by just looking at the data frame!

# With this command, you`ll perform a NMDS and plot the results
varespec %>%
  metaMDS(trace = F) %>%
  ordiplot(type = "none") %>%
  text("sites")

PCA <- rda(varespec, scale = FALSE)
# Use scale = TRUE if your variables are on different scales (e.g. for abiotic variables).
# Here, all species are measured on the same scale 
# So use scale = FALSE

# Now plot a bar plot of relative eigenvalues. This is the percentage variance explained by each axis
barplot(as.vector(PCA$CA$eig)/sum(PCA$CA$eig)) 
# How much of the variance in our dataset is explained by the first principal component?

# Calculate the percent of variance explained by first two axes
sum((as.vector(PCA$CA$eig)/sum(PCA$CA$eig))[1:2]) # 79%, this is ok.
# Also try to do it for the first three axes

# Now, we`ll plot our results with the plot function
plot(PCA)
plot(PCA, display = "sites", type = "points")
plot(PCA, display = "species", type = "text")


sitePCA <- PCA$CA$u # Site scores
speciesPCA <- PCA$CA$v # Species scores

# In a biplot of a PCA, species' scores are drawn as arrows 
# that point in the direction of increasing values for that variable
biplot(PCA, choices = c(1,2), type = c("text", "points"), xlim = c(-5,10)) # biplot of axis 1 vs 2
biplot(PCA, choices = c(1,3), type = c("text","points")) # biplot of axis 1 vs 3

PCA <- rda(trt_matrix, scale = FALSE)

trt_matrix[is.na(trt_matrix)] <- 0

barplot(as.vector(PCA$CA$eig)/sum(PCA$CA$eig)) 
# How much of the variance in our dataset is explained by the first principal component?

# Calculate the percent of variance explained by first two axes
sum((as.vector(PCA$CA$eig)/sum(PCA$CA$eig))[1:2]) # 79%, this is ok.
# Also try to do it for the first three axes

# Now, we`ll plot our results with the plot function
plot(PCA)
plot(PCA, display = "sites", type = "points")
plot(PCA, display = "species", type = "text")


sitePCA <- PCA$CA$u # Site scores
speciesPCA <- PCA$CA$v # Species scores

# In a biplot of a PCA, species' scores are drawn as arrows 
# that point in the direction of increasing values for that variable
biplot(PCA, choices = c(1,2), type = c("text", "points"), xlim = c(-5,10)) # biplot of axis 1 vs 2
biplot(PCA, choices = c(1,3), type = c("text","points")) # biplot of axis 1 vs 3

### PCoA

# Here we use Bray-Curtis distance metric
dist <- vegdist(varespec,  method = "bray")

# PCoA is not included in vegan. 
# We will use the ape package instead
library(ape)
PCOA <- pcoa(dist)

# plot the eigenvalues and interpret
barplot(PCOA$values$Relative_eig[1:10])
# Can you also calculate the cumulative explained variance of the first 3 axes?

# Some distance measures may result in negative eigenvalues. In that case, add a correction:
PCOA <- pcoa(dist, correction = "cailliez")

# Plot your results
biplot.pcoa(PCOA)

# You see what`s missing? 
# Indeed, there are no species plotted on this biplot. 
# That's because we used a dissimilarity matrix (sites x sites) 
# as input for the PCOA function. 
# Hence, no species scores could be calculated. 
#However, we could work around this problem like this:
biplot.pcoa(PCOA, varespec)

PCOAaxes <- PCOA$vectors[,c(1,2)]

# Compare this result with the PCA plot
par(mfrow = c(1, 2)) 
biplot.pcoa(PCOA)
plot(PCA)

# reset plot window
par(mfrow = c(1, 1)) 

## test with trait data
# trt_matrix <- read.csv("output_data/01a_trait_matrix_weighted_by_abundance_transformed.csv")
str(trt_matrix)
dim(trt_matrix)

trt_matrix_sub <- trt_matrix[1:1000,]
# Here we use Bray-Curtis distance metric
# dist <- vegdist(trt_matrix_sub,  method = "gower", na.rm = T) 

dist <- gowdis(trt_matrix)
sum(is.na(dist))
?vegdist
##
# matrix_dissim_gower <- as.matrix(dist) 
# rm(dist) 
# gc() 
# isSymmetric(matrix_dissim_gower) 

length(dist)
# PCoA is not included in vegan. 
# We will use the ape package instead
library(ape)
PCOA <- pcoa(dist) # Error in min(D.eig$values) : invalid 'type' (complex) of argument

# plot the eigenvalues and interpret
barplot(PCOA$values$Relative_eig[1:10])
# Can you also calculate the cumulative explained variance of the first 3 axes?

# Some distance measures may result in negative eigenvalues. In that case, add a correction:
PCOA <- pcoa(dist, correction = "cailliez")

# Plot your results
biplot.pcoa(PCOA)
?biplot.pcoa
# You see what`s missing? 
# Indeed, there are no species plotted on this biplot. 
# That's because we used a dissimilarity matrix (sites x sites) 
# as input for the PCOA function. 
# Hence, no species scores could be calculated. 
#However, we could work around this problem like this:
biplot.pcoa(PCOA, trt_matrix_sub)

model.matrix(~ ., model.frame(df, na.action = na.pass))

PCOAaxes <- PCOA$vectors[,c(1,2)]


library(vegan)
data(dune)
distmatrix <-vegdist(dune, method="euc")
# Principal coordinates analysis with 19 axes to estimate total variance
Ordination.model1 <- cmdscale (distmatrix, k=19, eig=TRUE, add=FALSE)
# Change scores for second axis
Ordination.model1$points[,2] <- -1.0 * Ordination.model1$points[,2]
class(Ordination.model1)
str(Ordination.model1)
Ordination.model1 <- add.spec.scores(Ordination.model1, dune, 
                                     method='pcoa.scores', Rscale=TRUE, scaling=1, multi=1)

# Compare Ordination.model1 with PCA
Ordination.model2 <- rda(dune, scale=FALSE)
#
par(mfrow=c(1,2))
ordiplot(Ordination.model1, type="text")
abline(h = 0, lty = 3)
abline(v = 0, lty = 3)
plot(Ordination.model2, type="text", scaling=1)


