## install iNEXT package from CRAN
install.packages("iNEXT")

## install the latest version from github
install.packages('devtools')

install_github('JohnsonHsieh/iNEXT')

install.packages("iNextPD")

install.packages("ggrepel")
## import packages
library(iNEXT)
library(ggplot2)
library(vegan)
library(devtools)
library(ggrepel)

#structure of iNEXT
iNEXT(x, q=0, datatype="abundance", size=NULL, endpoint=NULL, knots=40, se=TRUE, conf=0.95, nboot=50)

#foram data 
pc <- read.csv("abundance.csv", row.names = 1, header = TRUE)
#make community matrix - extract columns with abundance information
otu = pc[,4:ncol(pc)]

foram <- data.frame(otu)

foram <- t(as.matrix(foram))

str(foram)


ggiNEXT(out) 

ggiNEXT(out, type=3, facet.var="site")

xx <- ggiNEXT(out, type=1, facet.var="site") +
  geom_line() +
  geom_label_repel(aes(label = "Guides"),
                   nudge_x = 1,
                   na.rm = TRUE) +
  theme(legend.position = "none")

out <-iNEXT(foram, q=0, datatype="abundance")

ggiNEXT(out, type=1, color.var="site") + 
  theme_bw(base_size = 18) + 
  theme(legend.position="none")

ggiNEXT(out, type=2, facet.var="order")

ggiNEXT(out, type=1, facet.var="order", color.var="site")


foram <- log1p(foram)



ggiNEXT(out.inc, type=1, color.var="site") + 
  theme_bw(base_size = 18) 

warning()

# set a series of sample sizes (m) for R/E computation


out <- iNEXT(foram, q=c(0, 1, 2), datatype="abundance", size=NULL)

out <- iNEXT(foram, q=c(0, 1, 2), datatype="abundance", endpoint=650000)



xx <- ggiNEXT(out, type=1, facet.var="order") +
  facet_wrap(~order, scales="free") +
  theme_bw(base_size = 18) +
  theme(legend.position="right") +
  theme_gray() +
  ylab("Number of ASVs") + ylim(0, 600) + xlim(0, 80000) + xlab("Number of reads") +

  scale_shape_manual(values = c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA ))

xx 


xx <- ggiNEXT(out, type=1, facet.var="order") +
  facet_wrap(~order, scales="free") +
  theme_bw(base_size = 18) +
  theme(legend.position="right") + ylim(0, 1300) + scale_y_continuous(breaks=seq(0,1500,200))+
  theme_gray() +
  ylab("Number of ASVs") + xlab("Number of reads") +
  
  scale_fill_manual(values = c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA))

xx



# Sample-size-based R/E curves
xx <- ggiNEXT(out, type=2, color.var="site") + 
  xlim(c(0,10000))+
  theme_bw(base_size = 18) + 
  theme(legend.position="none")
xx

specaccum(comm, method = "exact", permutations = 100, conditioned =TRUE, gamma = "jack1",  w = NULL, subset, ...)
"plot"(x, add = FALSE, random = FALSE, ci = 2,  ci.type = c("bar", "line", "polygon"), col = par("fg"), lty = 1, ci.col = col, ci.lty = 1, xlab, ylab = x$method, ylim, xvar = c("sites", "individuals", "effort"), ...)
"boxplot"(x, add = FALSE, ...)
fitspecaccum(object, model, method = "random", ...)
"plot"(x, col = par("fg"), lty = 1, xlab = "Sites",  ylab = x$method, ...) 
"predict"(object, newdata, interpolation = c("linear", "spline"), ...)
"predict"(object, newdata, ...)
specslope(object, at)

pc<- t(as.matrix(pc))
pc <- read.csv("abundance.csv", row.names = 1, header = TRUE)
sp1 <- specaccum(pc,"random")
sp1
summary(sp1)


plot(curve_all, ylim = c(0,1500), ci.type="polygon", col="red", lwd=2, ci.lty=0, ci.col="lightgray")

pc2 <- read.csv("abundance2.csv", row.names = 1, header = TRUE)
sp2 <- specaccum(pc,"random")
sp2
summary(sp2)
plot(curve_all2, add = TRUE, ci.type="poly", col="blue", lwd=2, ci.lty=0, ci.col=rgb(150,150,180,127, maxColorValue=255))



legend('bottomright', c('Unsieved', 'Sieved'), col=1:2, lty=1, 
       bty='n', inset=0.025)


plot(l, ci.type="poly", col="blue", lwd=2, ci.lty=0, ci.col="lightblue")

boxplot(sp2, col="yellow", add=TRUE, pch="+")

data(BCI)
sp1 <- specaccum(BCI)
sp2 <- specaccum(BCI, "random")
sp2
summary(sp2)
plot(sp1, ci.type="poly", col="blue", lwd=2, ci.lty=0, ci.col="lightblue")
boxplot(sp2, col="yellow", add=TRUE, pch="+")

mod1 <- fitspecaccum(sp1, "lomolino")
coef(mod1)
fitted(mod1)
plot(sp1)

plot(sp2, add = TRUE, col=2, lwd=2)

data(bird)
str(bird)

