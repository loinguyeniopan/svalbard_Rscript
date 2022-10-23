
library(mixOmics)

#read data 

S = read.csv("sieved.csv",row.names = 1, header= TRUE)
com = read.csv("com.csv",row.names = 1, header= TRUE)
U = read.csv("unsieved.csv",row.names = 1, header= TRUE)

env = read.csv("env.csv",row.names = 1, header= TRUE)
E = env[,3:ncol(env)]

env = read.csv("env2.csv",row.names = 1, header= TRUE)
E2 = env[,3:ncol(env)]

ano = read.csv("anotation.csv",row.names = 1, header= TRUE)
ano1 = read.csv("anotation2.csv",row.names = 1, header= TRUE)


ano = as.matrix.data.frame(ano)
ano1 = as.matrix.data.frame(ano1)


head(cbind(rownames(com), rownames(E)))

com<-com/rowSums(com)




#plot PCA

#sPLS analysis

s.spls <- spls(S, E, ncomp = 10, keepX = c(1354,1354,1354), keepY = c(7,7,7), mode = "regression")


s.spls <- spls(S, E, ncomp = 10, keepX = c(100,100,100), keepY = c(7,7,7), mode = "regression")


u.spls <- spls(U, E2, ncomp = 10, keepX = c(1053,1053,1053), keepY = c(7,7,7), mode = "regression")


s.spls <- spls(S, E, ncomp = 2,  mode = "regression")

tune.pls <- perf(s.spls, ncomp = 2, validation = "loo", folds = 10)

sessionInfo()

#plot CIM

col.class <- c("Globothalamea" = "cyan4", "Monothalamea" = "lightblue3", "Tubothalamea" = "lightcoral", "Foraminifera_X"= "white")

cim(s.spls, comp = 1:2,
    xlab = "Environmental Parameters",
    ylab = "ASVs",
    margins = c(10,2),
    row.sideColors = col.class[ano],
    row.names = FALSE)
    
    legend=list(legend = names(col.class), 
                col = col.class,title = "Taxonomy",
                cex = 0.7,
                position = "bottom"))
  
cim(s.spls, comp = 1:2,
    xlab = "Environmental Parameters",
    ylab = "ASVs",
    margins = c(10,4),
    row.sideColors = col.class[ano],
    title = "Sieved, component 1:2",
    legend=list(legend = names(col.class), 
                col = col.class,title = "Taxonomy",
                cex = 0.7,
                position = "bottom"))



  x <- cim(s.spls, comp = 1:2, xlab = "Environmental Parameters", ylab = "ASVs", 
           margins = c(7, 7))
  
  write.csv(x$mat,'cor_sieved.csv')
  
  color.edge<- colorRampPalette(c("darkblue", "darkred"))(50)

  col.class <- c("Globothalamea" = "darkgreen", "Monothalamea" = "orange", "Tubothalamea" = "red")
  
  
  cim(u.spls, comp = 1:2, 
      xlab = "Environmental Parameters",
      ylab = "ASVs",
      margins = c(10,2),
      row.sideColors = col.class[ano1],
      row.names = FALSE,
      legend=list(legend = names(col.class), 
                  col = col.class,title = "Taxonomy",
                  cex = 0.7,
                  position = "bottom"))
  
  
  cim(u.spls, comp = 1:2, 
      xlab = "Environmental Parameters",
      ylab = "ASVs",
      margins = c(10,3),
      row.sideColors = col.class[ano1],
      title = "Unsieved, component 1:2",
      row.names = FALSE,
      legend=list(legend = names(col.class), 
                  col = col.class,title = "Class",
                  cex = 0.7,
                  position = "bottom"))

  x1 <- cim(u.spls, comp = 1:2, xlab = "Environmental Parameter", ylab = "ASVs", 
            margins = c(7, 7))
  
  write.csv(x1$mat,'cor_unsieved.csv')
  
  
  
  
# define red and green colors for the edges
color.edge <- color.GreenRed(50) 



# to save as a pdf

cim(s.spls, comp = 1:2, 
    xlab = "Environmental Parameters",
    ylab = "ASVs", 
    margins = c(10, 5),
    row.names = FALSE,
    title = "Sieved, component 1:2"
      )


dends <- cim(u.spls, comp = 1:2, xlab = "Environmental Parameter", ylab = "ASVs", 
             margins = c(10, 10))

op <- par(mar = c(5, 4, 4, 4), cex = 0.5)			 
plot(dends$ddr, axes = FALSE, horiz = TRUE)
par(op)




x <- cim(s.spls, comp = 1:2, xlab = "Environmental Parameter", ylab = "ASVs", 
    margins = c(7, 7))

write.csv(x$mat,'cor_sieved.csv')

x1 <- cim(u.spls, comp = 1:2, xlab = "Environmental Parameter", ylab = "ASVs", 
         margins = c(7, 7))

write.csv(x1$mat,'cor_unsieved.csv')





# unsieved



cim(u.spls, comp = 1:2, xlab = "Environmental Parameter", ylab = "ASVs", 
    margins = c(7, 7))

x1 <- cim(u.spls, comp = 1:2, xlab = "Environmental Parameter", ylab = "ASVs", 
         margins = c(7, 7))

