# install package
install.packages("vegan") 
# load package
library("vegan")

#read data
station = read.csv("cca_table.csv", row.names=1, header= TRUE)


# make Habitat an ordered variable
station$location <- ordered(station$location, c("Isfjorden", "Wijdefjorden", "Rijpfjorden", "Nordaustlandet", "Edgeoya" ))

# mosquito abundance matrix
abundance.matrix <- station[,12:ncol(station)]

abundance.matrix <- sqrt(abundance.matrix)

# pca
my.ca <- cca(abundance.matrix)

# barchart for variance of CA explained
barplot(my.ca$CA$eig/my.ca$tot.chi, names.arg = 1:my.ca$CA$rank, cex.names = 1, ylab="Proportion of variance explained", xlab="CA axis")

# CA proportions of variance explained
head(my.ca$CA$eig/my.ca$CA$tot.chi)

# plot
plot(my.ca)

# 4x4 PCA plots with different scaling options
layout(matrix(1:4,2,2,byrow=TRUE))
plot(my.cca, scaling="none", main="scaling = 0, raw")
plot(my.cca, scaling="sites", main="scaling = 1, sites")
plot(my.cca, scaling="species", main="scaling = 2, species (default)")
plot(my.cca, scaling="symmetric", main="scaling = 3, both")

# 2 panels
layout(matrix(1:2,1,2))

# plot sites

plot(my.ca, display="sites", type="n", main="Sites", scaling="sites")
text(my.ca, display="sites", col=site.cols, scaling="sites")

# predictor variables
fixed <- station[ ,6:11]


# the formula notation y ~ . means that all variables in 'fixed' should be included
my.cca <- cca(abundance.matrix ~ ., data=fixed)

#plot 2 types
site.cols <- rep(terrain.colors(6)[5:1],9)
plot(my.cca, display=c("species", "bp"), type="n", ylab="", main="Species", scaling=3)
text(my.cca, display="sites", col=site.cols, scaling=3, cex=.8, pos=rep(c(1,3), 5))
points(my.cca, pch=16, scaling=3, display="sites", col="blue")

text(my.cca, display="bp", col="red")
points(my.cca, pch=16, scaling=3, display="sp", col="red")
text(my.cca, dis="species", scaling=3, col="black", cex=.8,  mar=c(0.5,0.5,0.5,0.5), pos=rep(c(1,3), 5))


# VIFs
vif.cca(my.cca)

drop1(my.cca, test="perm")

barplot(my.cca$CA$eig/my.cca$tot.chi, names.arg = 1:my.cca$CA$rank, cex.names = 0.5, ylab="Proportion of variance explained", xlab="CCA axis")

# 2 panels
layout(matrix(1:2,1,2))

# plot sites
plot(my.cca, display=c("sites", "bp"), type="n", main="Sites", scaling="sites")
text(my.cca, display="sites", col=site.cols, scaling="sites")
text(my.cca, display="bp", col="red")


# plot species
plot(my.cca, display=c("species", "bp"), type="n", ylab="", main="Species", scaling="species")
text(my.cca, display="species", col="black", scaling="species")
text(my.cca, display="bp", col="red")




layout(matrix(1:4,2,2,byrow=TRUE))
plot(my.cca, scaling="none", main="scaling = 0, raw")
plot(my.cca, scaling="sites", main="scaling = 1, sites")
plot(my.cca, scaling="species", main="scaling = 2, species (default)")
plot(my.cca, scaling="symmetric", main="scaling = 3, both")

# proportion of variance explained
my.cca$CCA$tot.chi/my.cca$tot.chi

# significance test for individual predictors (type 3 test)
anova(my.cca, by="margin") 

# significance test for entire model
anova(my.cca)
