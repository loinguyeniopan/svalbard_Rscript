#install
install.packages("ggimage")
install.packages("ggtree")
install.packages("TDbook")
install.packages("ggnewscale")
install.packages("ggtreeExtra")

if (!require("BiocManager", quietly = TRUE))
  install.packages("BiocManager")

BiocManager::install("ggtreeExtra")

library(ggplot2)
library(ggimage)
library(ggtree)
library(ape)
library(Biostrings)
library(data.table)
library(tibble)

# load tree file (in NEWICK format)

tree <- read.tree("790_al.fasta.treefile")

tree <- root(tree, outgroup = "AJ457814_Gromia_oviformis_Guam__", resolve.root = TRUE)

groupInfo <- split(tree$tip.label, dt3)

tree1 <- groupOTU(tree, groupInfo)


tree_plot <- ggtree(tree1, aes(color=group, size=label < 70), layout="circular" )+
  scale_size_manual(values=c(0.5, 0.1)) +
  scale_colour_manual(values=cbPalette) + theme(legend.position="bottom")

tree_plot

p <- ggtree(tree1, layout="slanted") + geom_treescale(x=6, y=0, fontsize=1.2, linesize=0.3) + layout_circular()
p


tree_plot <- ggtree(tree, layout="circular", aes(size=label < 70))+
  scale_size_manual(values=c(0.5, 0.1))

tree_plot <- ggtree(tree,  aes(size=label < 70))+ geom_tiplab(size =3) +
  scale_size_manual(values=c(1, .1)) + geom_tiplab(offset = .6, hjust = .5) + theme(legend.position='none')

tree_plot

#check the label with clade range

write.csv(tree$tip.label, file = "tiplabel.csv")
write.csv(tree$node.label, file = "node.label.csv")

dt = read.csv("clade.csv",row.names = 1, header= T)
dt1 <- data.frame(ccz = dt$ccz)

rownames(dt1) <- tree$tip.label

dt2 <- data.frame(sub_ccz = dt$sub_ccz)
rownames(dt2) <- tree$tip.label


dt3 <- data.frame(ref = dt$ref)
rownames(dt3) <- tree$tip.label

#bootstraps value

root <- rootnode(tree) 

dt4 <- read.csv("support.csv", header= T)

dt4 =  as.character(dt4)
tree$node.label <- dt4


#plot tree ref
tree_plot1 <- gheatmap(tree_plot, dt3, offset= 0.001, width=0.3,
                       colnames_angle=90, colnames_offset_y = .25) + 
  scale_fill_viridis_d(option = "plasma", name="Order (Reference)") +
  geom_treescale(x = -0.2 , y = 0.1,  offset= 4) 

tree_plot1

#add CCZ clade 

library(ggnewscale)

tree_plot2 <- tree_plot1 + new_scale_fill()

p2 <- gheatmap(tree_plot2, dt1, offset= 1, width=.3,
               colnames_angle=90, colnames_offset_y = .25) + scale_fill_viridis_d(option="magma", name="Linage CCZ (phylo)")

p2


tree_plot3 <- tree_plot2 + new_scale_fill()

p3 <- gheatmap(tree_plot3, dt2, offset= 1, width=.3,
               colnames_angle=90, colnames_offset_y = .25) + scale_fill_viridis_d(option="inferno", name="Linage CCZ (signature)")

p3



tree_plot2 <- tree_plot1 + new_scale_fill()

p2 <- gheatmap(tree_plot2, dt2, offset= 1, width=.3,
               colnames_angle=90, colnames_offset_y = .25) + scale_fill_viridis_d(option="D", name="Linage CCZ (signature)")

p2


library(ggtreeExtra)



dt4 <- data.frame(abundance = dt$abundance)
dt4$ID <- tree$tip.label
dt4$CCZ <- dt1$ccz

p3 <- tree_plot + new_scale_fill() + geom_fruit(data=dt4, geom=geom_bar,
             mapping=aes(y=ID, x= abundance, fill = CCZ ),
             pwidth=0.3, 
             orientation="y", 
             stat="identity") +  scale_fill_viridis_d(option="D", name="Clade CCZ")


p3



p3 <- tree_plot  + geom_fruit(data=dt4, geom=geom_bar,
                                                mapping=aes(y=ID, x= abundance, fill = CCZ ),
                                                pwidth=0.3, 
                                                orientation="y", 
                                                stat="identity") +  scale_fill_viridis_d(option="D", name="Clade CCZ")


p3


p4 <- p3 + new_scale_fill()
p4 <- gheatmap(p3, dt3, offset= 0.001, width=0.3,
                       colnames_angle=90, colnames_offset_y = .25) + 
  scale_fill_viridis_d(option = "plasma", name="Order") +
  geom_treescale(x = -0.2 , y = 0.1,  offset= 4) 

p4


p4 <- p3 + new_scale_fill()

p4 <- gheatmap(p3, dt3, offset= 0.01, width=.3,
               colnames_angle=95, colnames_offset_y = .25) + scale_fill_viridis_d(option="A", name="Order")

p4





#geom_treescale

tree_plot1 <- ggtree(tree, layout="circular") + 
  geom_treescale(x = 1, y = 1,  offset= 2, color='red')

tree_plot1




tree_plot1 <- ggtree(tree, layout="circular", branch.length="none") + 
  geom_treescale(x = 1, y = 1,  offset= 2, color='red') +
   geom_point2(aes(subset=!isTip & node != root, 
                fill=cut(support, c(0, 70, 90, 100))), 
            shape=21, size= 4) +
theme_tree(legend.position=c(0.2, 0.2)) + 
  scale_fill_manual(values=c("white", "grey", "black"), guide='legend', 
                    name='Bootstrap Percentage(BP)', 
                    breaks=c('(90,100]', '(70,90]', '(0,70]'), 
                    labels=expression(BP>=90,70 <= BP * " < 90", BP < 70))



tree_plot1


tree_plot1 <- ggtree(tree, layout="circular") + 
  geom_treescale(x = 1, y = 1,  offset= 2, color='red') +
  geom_point2(aes(subset=!isTip & node != root, 
                  fill=cut(support, c(70))), 
              shape=21, size= 1) +
  theme_tree(legend.position=c(0.2, 0.2)) + 
  scale_fill_manual(values=c("white", "grey", "black"), guide='legend', 
                    name='Bootstrap Percentage(BP)', 
                    breaks=c('(70,100]'), 
                    labels=expression(BP>=90,70 <= BP * " < 90", BP < 70))



tree_plot1






