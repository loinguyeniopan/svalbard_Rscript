#https://programmersought.com/article/8288843012/



library(statnet)
library(circlize)

install.packages("statnet")
install.packages("circlize") 

# setwd(...)# Set your own working environment, follow your own preferences
# Rmd does not need to set the working directory, the default is all directories of the file
data<-read.csv("abundance3.csv",row.names=1, header= TRUE)

my.data<-as.matrix(data) # matrix
# Manually set the row and column name (optional)
rownames(my.data) <-c("Foraminifera_X", "Globothalamea", "Monothalamea", "Tubothalamea")
colnames(my.data) <-c( "EDG1",	"EDG2",	"EDG3",	"IS1",	"IS2",	"NAL4",	"NAL5",	"NAL6",	"NAL7",	"RIJ1",	"RIJ2",	"RIJ3",	"RIJ4",	"WIJ1",	"WIJ3") 
# Row and column naming, I am used to manually naming here, if you feel trouble, you can edit it directly according to the name in the input document.

grid.col = NULL

# Define the color of the processing, here are randomly selected 4 colors, you can make a good color according to your preferences
grid.col[c("Foraminifera_X", "Globothalamea", "Monothalamea", "Tubothalamea")] = c("blue", "black", "orange", "chocolate") 

# Define the color of each door of the microorganism,
grid.col[colnames(my.data)] = c("lavender", "khaki","mistyrose", 
                                "sienna1", "skyblue", "brown1", 
                                "gold", "maroon", "salmon", "moccasin",
                                "wheat","black","green","cyan","orange") 

# parameter settings 
circos.par(gap.degree = c(rep(2, nrow(my.data)-1), 10, rep(2, ncol(my.data)-1), 10),
           start.degree = 180)

# , I only use a small number of parameters here, all parameters see the help file of this package, or see below
chordDiagram(my.data,
             directional = TRUE,
             diffHeight = 0.06,
             grid.col = grid.col, 
             transparency = 0.5) 

# 
legend("right",pch=20,legend=colnames(my.data),
       col=grid.col[colnames(my.data)],bty="n",
       cex=1,pt.cex=3,border="black") #Set the legend


# Set image file name, length and width, and font size
pdf(file="circlize.pdf", width=8, height=5, pointsize=8)
#  and legend code
chordDiagram(my.data,directional = TRUE,diffHeight = 0.06,grid.col = grid.col, transparency = 0.5) 
legend("right",pch=20,legend=colnames(my.data),col=grid.col[colnames(my.data)],bty="n",cex=1,pt.cex=3,border="black") 
# Write to file after drawing ends
dev.off()
