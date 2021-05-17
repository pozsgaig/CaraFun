## Must be logically organized and a and further data input should be made possible

taxon_cleanup<-function(taxon_list, write2file=T, output_file=NULL)
{
# install.packages("rgbif", dep=T)
# install.packages("taxize", dep=T)
# taxon_list should be a vector, containing taxon names, output_file a path to a csv
library(rgbif)
library(taxize)

taxon_list<-as.character(taxon_list)
taxon_hierarchy<-NULL
for (n in taxon_list)
{
# n="Trechidae"
# n = "Abacetus"
class_res<-NULL
subfamily<-NULL

nbb<-name_backbone(as.character(n), strict = T, curlopts = list(timeout_ms=20000))
out <- get_ids(sci_com = as.character(n), db = c("ncbi"), rows=1, timeout_ms=20000)
if (!is.na(out))
{
    class_res<-classification(out)
ncbi_taxon<-class_res[[1]][[1]]
subfamily<-ncbi_taxon[ncbi_taxon$rank=="subfamily", "name"]
}
# names(nbb)
original<-n

# used to get synonyms' GBIF IDs

if(nbb$synonym==T)
{
    taxon_GBIFID<-ifelse(is.null(nbb$speciesKey), nbb$genusKey, nbb$speciesKey)
} else
{taxon_GBIFID<-ifelse(is.null(nbb$usageKey), NA, nbb$usageKey)}

# taxon_GBIFID<-ifelse(is.null(nbb$usageKey), NA, nbb$usageKey)

taxon_NCBIID<-ifelse(is.null(names(class_res[[1]][1])[1]), NA, names(class_res[[1]][1])[1])
taxon_rank<-ifelse(is.null(nbb$rank), NA, nbb$rank)
taxon_fullname<-ifelse(is.null(nbb$scientificName), NA, nbb$scientificName)
taxon_kingdom<-ifelse(is.null(nbb$kingdom), NA, nbb$kingdom)
taxon_phylum<-ifelse(is.null(nbb$phylum), NA, nbb$phylum)
taxon_class<-ifelse(is.null(nbb$class), NA, nbb$class)
taxon_order<-ifelse(is.null(nbb$order), NA, nbb$order)
taxon_family<-ifelse(is.null(nbb$family), NA, nbb$family) 
taxon_subfamily<-ifelse(is.null(subfamily), NA, subfamily)
taxon_genus<-ifelse(is.null(nbb$genus), NA, nbb$genus) 
taxon_species<-ifelse(is.null(nbb$species), NA, nbb$species)

taxon_line<-c(original, taxon_GBIFID, taxon_NCBIID, taxon_rank, taxon_fullname, taxon_kingdom, taxon_phylum,
              taxon_class, taxon_order, taxon_family, taxon_subfamily, taxon_genus,
              taxon_species)
taxon_line<-as.data.frame(t(taxon_line))
colnames(taxon_line)<-c("searched", "GBIFID", "NCBIID", "rank", "full_name", "kingdom", "phylum", "class",
                             "order", "family", "subfamily", "genus", "species")

print(paste0(original, " - ", taxon_subfamily))
taxon_hierarchy<-rbind(taxon_hierarchy, taxon_line)

if (write2file==T)
{
    
write.table(taxon_line, file=output_file, sep=",", col.names = T, row.names = F, append = T)

}




}

warnings()

colnames(taxon_hierarchy)<-c("searched", "GBIFID", "NCBIID", "rank", "full_name", "kingdom", "phylum", "class",
                             "order", "family", "subfamily", "genus", "species")


return(taxon_hierarchy)
}


 