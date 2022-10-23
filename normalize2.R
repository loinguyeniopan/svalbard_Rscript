library(vegan)
library(metagenomeSeq)

install.packages("devtools")
library("devtools")
pc <- read.csv("abundance.csv", row.names = 1, header = TRUE)
OTU_read_count = as.data.frame(pc)

# convert OTU table into package format

metaSeqObject      = newMRexperiment(OTU_read_count) 

# CSS normalization

metaSeqObject_CSS  = cumNorm( metaSeqObject , p=cumNormStatFast(metaSeqObject) )

# convert CSS normalized data into data.frame-formatted OTU table (log transformed data)

OTU_read_count_CSS = data.frame(MRcounts(metaSeqObject_CSS, norm=TRUE, log=TRUE))

write.csv(OTU_read_count_CSS,"ASV_CSS.csv")
