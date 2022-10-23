install.packages("factoextra")
library(factoextra)

install.packages("ggplot2")
library(ggplot2)

pc = read.csv("cca.csv",row.names=1, header= T)

pc.active <- pc[1:15, 3:9]

head(pc.active[, 1:4])

data(abundance1)

res.pca <- prcomp(station, scale = TRUE)

fviz_eig(res.pca)


fviz_pca_ind(res.pca,
             col.ind = "cos2", # Color by the quality of representation
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)


fviz_pca_var(res.pca,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)

fviz_pca_biplot(res.pca, repel = TRUE,
                
                col.var = "#2E9FDF", # Variables color
                col.ind = "#696969"  # Individuals color
)

fviz_pca_biplot(res.pca, repel = TRUE)

install.packages("ggrepel")
library(ggrepel)

                