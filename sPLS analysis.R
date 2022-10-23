install.packages("mixOmics")
library(mixOmics)
sessionInfo()

#read data 

data(liver.toxicity)
X <- liver.toxicity$gene
Y <- liver.toxicity$clinic
C <- liver.toxicity$treatment

#PCA analysis

pca.gene <- pca(X, ncomp = 10, center = TRUE, scale = TRUE)

pca.gene

plot(pca.gene)

pca.clinical <- pca(Y, ncomp = 10, center = TRUE, scale = TRUE)

pca.clinical

#plot PCA

plotIndiv(pca.gene, comp = c(1, 2), group = liver.toxicity$treatment[, 4],
          ind.names = liver.toxicity$treatment[, 3],
          legend = TRUE, title = 'Liver gene, PCA comp 1 - 2')

plotIndiv(pca.clinical, comp = c(1, 2), group = liver.toxicity$treatment[, 4],
          ind.names = liver.toxicity$treatment[, 3],
          legend = TRUE,title = 'Liver clinical, PCA comp 1 - 2')

# sPLS analysis #regression
 
liver.pls <- pls(X, Y, ncomp = 10, mode = "regression")

liver.spls <- spls(X, Y, ncomp =10, keepX = c(10,10,10), keepY= c(10,10,10), mode = "regression")

tune.pls <- perf(liver.pls, validation = "Mfold", folds = 10, progressBar = FALSE, nrepeat = 10)
tune.spls <- perf(liver.spls, validation = "Mfold", folds = 10, progressBar = FALSE, nrepeat = 10)

tune.spls$measures$R2$summary

# Sample Plots

par(mfrow = c(1,3))

plotIndiv(liver.spls, comp = 1:2, rep.space= 'Y-variate', group = liver.toxicity$treatment[, 4],
          ind.names = liver.toxicity$treatment[, 3],
          legend = TRUE, title = 'Liver, sPLS comp 1 - 2, Y-space')

plotIndiv(liver.spls, comp = 1:2, rep.space= 'X-variate', group = liver.toxicity$treatment[, 4],
          ind.names = liver.toxicity$treatment[, 3],
          legend = TRUE, title = 'Liver, sPLS comp 1 - 2, X-space')

sectioninfo()

# 3D using style = 3D

plotIndiv(liver.spls, comp = 1:2, rep.space= 'XY-variate', group = liver.toxicity$treatment[, 4],
          ind.names = liver.toxicity$treatment[, 3],
          legend = TRUE, title = 'Liver, sPLS comp 1 - 2, XY-space')

plotVar(liver.spls, comp =1:2, 
        var.names = list(X.label = liver.toxicity$gene.ID[,'geneBank'], 
                         Y.label = TRUE), cex = c(4, 5))


col.tox <- color.mixo(as.numeric(as.factor(liver.toxicity$treatment[, 4])))
plotIndiv(liver.spls, ind.names = F, axes.box = "both", col = col.tox, style = '3d')



# define red and green colors for the edges
color.edge <- color.GreenRed(50)  
# to save as a pdf
network(liver.spls, comp = 1:2, shape.node = c("rectangle", "rectangle"),
        color.node = c("white", "pink"), color.edge = color.edge)

cim(liver.spls, comp = 1:3, xlab = "clinic", ylab = "genes", 
    margins = c(7, 7))
