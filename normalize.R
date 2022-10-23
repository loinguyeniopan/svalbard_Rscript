sessionInfo()

library(metagenomeSeq)

data(lungData)
S = as.matrix(S)
p = cumNormStatFast(S)

d = phyloseq_to_metagenomeSeq(S) 


install.packages("MetaLonDA")
library(MetaLonDA) 

D = normalize(S, method = "css")