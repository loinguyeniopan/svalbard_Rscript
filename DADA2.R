install.packages("readr")     # To read and write files
install.packages("readxl")    # To read excel files

install.packages("dplyr")     # To manipulate dataframes
install.packages("tibble")    # To work with data frames
install.packages("tidyr")     # To work with data frames

install.packages("stringr")   # To manipulate strings

install.packages("ggplot2")   # To do plots


source("https://bioconductor.org/biocLite.R")
biocLite('dada2')             # metabarcode data analysis
biocLite('phyloseq')          # metabarcode data analysis
biocLite('Biostrings')        # needed for fastq.geometry


library("dada2")
library("phyloseq")
library("Biostrings")

library("ggplot2")

library("dplyr")
library("tidyr")
library("tibble")

library("readxl")
library("readr")

library("stringr")

library("kableExtra")  # necessary for nice table formatting with knitr