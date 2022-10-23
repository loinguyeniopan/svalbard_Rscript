ano = anosim(com, pc$method, distance = "bray", permutations = 99999)
ano


library(pvclust)

pc = read.csv("abundance2.csv",row.names = 1, header= TRUE)

cluster <- pvclust(pc, method.hclust="ward.D2", method.dist="binary", 
                   nboot=1000, parallel=F)



plot(cluster)
pvrect(cluster, alpha=.90, pv="au", type="geq")

plot(cluster, cex = 1)
rect.hclust(cluster, k = 2, border = 0:2)

sub_grp <- cutree(cluster, k = 4)

cluster <- pvclust(pc, method.hclust = "average",
        method.dist = "correlation", nboot = 100)