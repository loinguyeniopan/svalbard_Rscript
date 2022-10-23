library(devtools)
install_github("tobiasgf/lulu")

library(lulu)


curated_result$curated_table

curated_result <- lulu(asvs.table, match_list)

# ....Which is equivalent of running LULU with default settings for the options minimum_ratio_type, minimum_ratio, minimum_relative_cooccurence  

curated_result <- lulu(asvs.table, match_list, minimum_ratio_type = "min", minimum_ratio = 1000, minimum_match = 90, minimum_relative_cooccurence = 0.9)

curated_result$curated_count

head(curated_result$curated_otus)

curated_result$discarded_count

head(curated_result$discarded_otus)

head(curated_result$otu_map)

curated_result$minimum_relative_cooccurence

curated_result$curated_table

write.table(curated_result$curated_table, file='test0.95.tsv', quote=FALSE, sep='\t', col.names = NA)