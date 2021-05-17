# increasing memory for java to read large excel files 
options( java.parameters = "-Xmx8g" )

library(rstudioapi)    
setwd(dirname(rstudioapi::getSourceEditorContext()$path))
library(xlsx)
library(sf) #for GIS data
library(dplyr)
library(tidyr)
library(zoteror)
library(RMySQL)
source("zot_read_item.R")
library(httr) # for GET
library(reshape2) # for melt
library(stringi)
## Must be logically organized and a and further data input should be made possible


#### reading in master datasets ####



connections<-read.xlsx2("../Raw_data/Master_list/Master_list.xlsx", 
                        sheetName = "Master_list", as.data.frame = T, header = T, 
                        stringsAsFactors=FALSE, encoding ="UTF-8")

species_done<-read.xlsx2("../Raw_data/Master_list/raw_carab_list_updated_final.xlsx", 
                        sheetName = "Done", as.data.frame = T, header=T, stringsAsFactors=FALSE, encoding = "UTF-8")

species_non_carabid<-read.xlsx2("../Raw_data/Master_list/raw_carab_list_updated_final.xlsx", 
                                sheetName = "Non_carabid", as.data.frame = T, header=T, stringsAsFactors=FALSE, encoding = "UTF-8")

species_solved<-read.xlsx2("../Raw_data/Master_list/raw_carab_list_updated_final.xlsx", 
                                sheetName = "Solved", as.data.frame = T, header=T, stringsAsFactors=FALSE, encoding = "UTF-8")

used_lit<-read.xlsx2("../Raw_data/Master_list/Master_list.xlsx", 
                                sheetName = "Literature", as.data.frame = T, header=T, 
                     stringsAsFactors=FALSE, encoding ="UTF-8")


notes<-read.table("../Raw_data/Master_list/Notes_updated.csv", 
                                    header = T, sep=",", dec = ".",quote = "\"", fileEncoding = "Latin1", 
                                    colClasses = "character")

# run this only if taxonomy needs to be cleaned
 # source("taxon_cleaning.R")
# OR

updated_carabid_species<-read.table("../Raw_data/Master_list/pre_carab_list_updated_solved.csv", 
                            header = T, sep=",", dec = ".",quote = "\"", fileEncoding = "Latin1", 
                            colClasses = "character")

updated_fungus_species<-read.table("../Raw_data/Master_list/pre_fungal_list_updated.csv", 
                                    header = T, sep=",", dec = ".",quote = "\"", fileEncoding = "Latin1", 
                                    colClasses = "character")

carabid_subspecies_update<-read.table("../Raw_data/Master_list/carabidae_subfams_update.csv", 
                                      header = T, sep=",", dec = ".",quote = "\"", fileEncoding = "Latin1",
                                      colClasses = "character")

# reading GIS shape file for countries

# shp <- st_read("../Raw_data/Master_list/Background_datasets/TM_WORLD_BORDERS-0.3_modified_2.shp")

shp <- st_read("../Raw_data/Master_list/Background_datasets/TM_WORLD_BORDERS-0.3_modified_small30.shp")
##### SUB REGION NAMES SHOULD BE MERGED FROM THE UNSD_subregions.csv FILE!
shp_names <- read.table("../Raw_data/Master_list/Background_datasets/UNSD_subregions.csv", sep=",", header = T,
                        stringsAsFactors=FALSE, encoding ="UTF-8")

shp_names$Sub.region.Code<-as.character(shp_names$Sub.region.Code)
shp_names$Intermediate.Region.Code<-as.character(shp_names$Intermediate.Region.Code)
shp_names$Region.Code<-as.character(shp_names$Region.Code)

# shp <- st_read("../common_data/GIS_data_mod/TM_WORLD_BORDERS-0.3_modified_2.shp")
shp$NAME <- as.character(shp$NAME)
shp$SUBREGION<-as.character(shp$SUBREGION)
#setting projection to WGS84
shp <- st_set_crs(shp, 4326)

# # providing space for 'count' and human readable subregion names
# shp$Freq <- 0 
# shp$SubRegSum <- NA # ID column (helper column) to summarise the subregion counts

colnames(shp_names)
head(shp_names)
nrow(shp)



shp<-merge(shp, unique(shp_names[, c("Sub.region.Code", "Sub.region.Name")]), by.x = "SUBREGION", by.y = "Sub.region.Code", all.x=T, all.y=F, all=F)
shp<-merge(shp, unique(shp_names[, c("Intermediate.Region.Code", "Intermediate.Region.Name")]), by.x = "SUBREGION", by.y = "Intermediate.Region.Code", all.x=T, all.y=F, all=F)
shp<-merge(shp, unique(shp_names[, c("Region.Code", "Region.Name")]), by.x = "REGION", by.y = "Region.Code", all.x=T, all.y=F, all=F)

nosubregion<-shp[shp$REGION==0,]

getsubr<-function(x){
    ifelse(is.na(x["Sub.region.Name"]),as.character(x["Intermediate.Region.Name"]), 
           as.character(x["Sub.region.Name"]))
}
shp$Subregion_name<-apply(shp, 1, getsubr)
    
shp$Sub.region.Name<-NULL
shp$Intermediate.Region.Name<-NULL


# subreg_name_lookupTab <- subreg_name_lookupTab[names(subreg_name_lookupTab) %chin% shp$NAME]
# id <- fastmatch::fmatch(names(subreg_name_lookupTab), shp$NAME)
# shp$SubRegSum[id] <- as.integer(shp$NAME[id])

# purely human readable names now
# shp$NAME[id] <- subreg_name_lookupTab



### long process, run only if Zotero library needs to be updated

# # reading in bibliography database from the exported Zotero data
# all_lit<-read.table("../Raw_data/Master_list/Background_datasets/all_literature2.csv", header = T, sep=",",
#                    quote = "\"", encoding = "UTF-8")
# head(all_lit)
# colnames(all_lit)[1]<-"Key"
# 
# getbibcit<-function(x){
#     item<-zot_read_item_G(id = x["Key"], user = 123456, credentials = "XXXXXXXXX",
#                           format="json", include = "data,citation,bibtex")
#     print(x[1])
#     ref<-paste(item$meta$creatorSummary, substr(item$meta$parsedDate,1,4), sep = ", ")
#     line<-c(Bibtex = as.character(item$bibtex), Citation = as.character(ref))
#     return(line)
# }
# 
# all_lit<-all_lit[!is.na(all_lit$Publication.Year), ]
# 
# all_lit$Key<-as.character(all_lit$Key)
# bibcit<-as.data.frame(t(apply(all_lit, 1, getbibcit)))
# all_lit<-cbind(bibcit, all_lit)
# 
# # to skip the re-running of a long process
# save(all_lit, file="../Raw_data/master_list/Background_datasets/all_lit.RDA")

# only load if no update was done before
load("../Raw_data/master_list/Background_datasets/all_lit.RDA")
colnames(all_lit)
head(all_lit)


# reading in country database

############################################
##### Cleaning and writing taxonomies ######
###########################################


#### cleaning and merging taxonomy datasets ####
updated_fungus_species<-updated_fungus_species[!is.na(updated_fungus_species$GBIFID), ]
updated_fungus_species<-updated_fungus_species[!duplicated(updated_fungus_species[,'searched']),]
updated_fungus_species<-updated_fungus_species[updated_fungus_species$kingdom=="Fungi",]
updated_fungus_species<-updated_fungus_species[!is.na(updated_fungus_species$genus),]

species_done<-species_done[,colnames(updated_carabid_species)]
nrow(updated_carabid_species)+nrow(species_done)

nrow(species_solved)
newly_searched_carabid_species<-merge(species_solved[, c("searched", "jav_fajnev")], unique(updated_carabid_species), by.x = "jav_fajnev", by.y = "searched", 
                                      all.y=F, all.x = T)



colnames(newly_searched_carabid_species)

nrow(newly_searched_carabid_species)
newly_searched_carabid_species$jav_fajnev<-NULL


cleaned_carabid_species<-rbind(species_done, newly_searched_carabid_species)

carabidae_dup<-cleaned_carabid_species[duplicated(cleaned_carabid_species[,"searched"]),"searched"]
cleaned_carabid_species[cleaned_carabid_species$searched %in% carabidae_dup,]


cleaned_carabid_species<-cleaned_carabid_species[!duplicated(cleaned_carabid_species[,'searched']),]



# removing two questionable taxa
cleaned_carabid_species<-cleaned_carabid_species[!cleaned_carabid_species$genus %in% c("Procarabus", "Scaritides"),]
cleaned_carabid_species<-cleaned_carabid_species[cleaned_carabid_species$family=="Carabidae",]

nrow(cleaned_carabid_species)
cleaned_carabid_species[cleaned_carabid_species=="NA"]=NA


for (n in unique(cleaned_carabid_species$genus))
{
    print(n)
    
    gendat<-cleaned_carabid_species[cleaned_carabid_species[,"genus"]==n,]
    subfam<-gendat[,"subfamily"]
    subfam<-subfam[!is.na(subfam)]
    if (length(unique(subfam))>1)
    {warning(paste("More than one subfamily is associated with genus", n, ":", 
                   paste(unique(subfam), collapse = "," )))} else
                   {if (length(unique(subfam))==0)
                   {rep(NA, nrow(gendat))} else 
                   {cleaned_carabid_species[cleaned_carabid_species[,"genus"]==n,"subfamily"]<-
                       rep(unique(subfam), nrow(gendat))
                   cat("done")}
                   }    
}

cleaned_carabid_species[cleaned_carabid_species[,"genus"]=="Bembidion","subfamily"]<-"Trechinae"
cleaned_carabid_species[cleaned_carabid_species[,"genus"]=="Elaphropus","subfamily"]<-"Trechinae"
cleaned_carabid_species[cleaned_carabid_species[,"genus"]=="Drypta","subfamily"]<-"Dryptinae"

nosubfam<-cleaned_carabid_species[is.na(cleaned_carabid_species$subfamily), c("genus", "subfamily")]

nrow(nosubfam)

subf_update<-merge(nosubfam, carabid_subspecies_update, by="genus", sort = T)
subf_update<-subf_update[match(nosubfam$genus, subf_update$genus),]
nrow(subf_update)

cleaned_carabid_species[is.na(cleaned_carabid_species$subfamily), "subfamily"]<-subf_update$subfamily.y

# making a common taxonomy table and writing it to mysql
taxonomy_table<-rbind(cleaned_carabid_species, updated_fungus_species)
length(unique(taxonomy_table$GBIFID))
dupIDs<-taxonomy_table[duplicated(taxonomy_table$GBIFID), "GBIFID"]

duplicated_taxa<-taxonomy_table[taxonomy_table$GBIFID %in% dupIDs, ]

unique_taxa<-taxonomy_table[!duplicated(taxonomy_table$GBIFID),]
colnames(unique_taxa)
nrow(unique_taxa)



##################################################
####### Txonomy data cleaning and writing ######
##################################################
taxonomy2write<-unique_taxa

head(taxonomy2write)
taxonomy2write$searched<-NULL
taxonomy2write<-taxonomy2write[, c(1,2,4,3, 5:ncol(taxonomy2write))]

colnames(taxonomy2write)<-c("GBIFID", "NCBIID", "Full_name", "Rank", "Kingdom", "Phylum", "Class",
                            "Ordo", "Family", "Subfamily", "Genus", "Species")

is.data.frame(taxonomy2write)
taxonomy2write<-as.data.frame(apply(taxonomy2write, 2, function(x) trimws(x, "both")))
taxonomy2write[] <- lapply(taxonomy2write, as.character)

# Writing taxonomy database into MySQL

taxonomy2write$Full_name<- gsub("'","`",taxonomy2write$Full_name)
# Encoding(taxonomy2write$Full_name) <- "Latin1"
taxonomy2write$Full_name<-stri_encode(taxonomy2write$Full_name, "", "UTF-8")

# writing to MySQL

con <- dbConnect(MySQL(),
                 user="user", password="",
                 dbname="XXXX", host="localhost")

dbSendQuery(con,'SET NAMES utf8') 
for (n in 1:nrow(taxonomy2write))
{
    query <- paste0("INSERT INTO taxonomy(", paste0(colnames(taxonomy2write), collapse = ","), ") VALUES('", paste0(taxonomy2write[n,], collapse = "', '"), "')")
    
    dbSendQuery(con, query)
}
dbDisconnect(con)

# write to csv
write.table(taxonomy2write, file = "../Raw_data/Master_list/taxonomy.csv", 
            sep=",", row.names = F, fileEncoding = "UTF-8", quote = TRUE)



# papers which are not in our checked list
missing_lit<-all_lit[!all_lit$Key %in% unique(cits$final_ref_code), 
                     c("Key", "Bibtex", "Citation")]





missing_subfams<-unique(unique_taxa[is.na(unique_taxa$subfamily) & unique_taxa$kingdom=="Animalia", c("genus", "subfamily")])
# write.table(missing_subfams, file = "../Calculated_data/missing_subfams.csv", sep=",", col.names = T)

#### cleaning up master dataset ####
# removing non-carabid connections
connections_limited<-connections[!connections$Carabidae_name %in% species_non_carabid$searched, ]

nrow(species_non_carabid)
nrow(connections_limited)

# removing connections with non resolved carabid names
nonresolved_remove<-connections_limited[!connections_limited$Carabidae_name %in% cleaned_carabid_species$searched, ]

connections_limited<-connections_limited[connections_limited$Carabidae_name %in% cleaned_carabid_species$searched, ]

# removing connections with non resolved fungal names
connections_limited<-connections_limited[connections_limited$Fungi_name %in% updated_fungus_species$searched, ]
connections_limited$order<-1:nrow(connections_limited)

# getting references merged with Zotero database
cits<-connections_limited[,c("References", "Ref_code", "order")]

cits$final_ref_code<-NULL

head(used_lit)

cits$final_ref_code<-apply(cits, 1, FUN = 
    function(x) 
        {ifelse(x["Ref_code"]=="", used_lit[used_lit$Ref==x["References"],"Code"], x["Ref_code"])})

cits$Ref_code<-NULL
colnames(cits)
is.character(cits$final_ref_code)
is.character(all_lit$Key)
colnames(all_lit[,c("Key", "Bibtex", "Citation", "Publication.Year")])
# removing messy citations from main database and replacing them with ZoteroID and publication year
cits<-merge(cits, all_lit[,c("Key", "Bibtex", "Citation", "Publication.Year")], 
            by.x = "final_ref_code", by.y =  "Key", all.x = T, sort = F)


cits<-cits[order(cits$order),]


cits$citation<-apply(cits, 1, FUN=function(x) {res<-GET(paste0("https://api.zotero.org/users/",12345, "/items/", x["final_ref_code"], "?key=xxxxxxxx&format=bib&style=apa&itemType=-attachment"))
content(res, as = "text", encoding = "UTF-8")}
)

# 

nrow(cits)
head(cits)
nrow(connections_limited)
connections_limited<-cbind(connections_limited, cits)

colnames(connections_limited)
connections_limited$References<-NULL
connections_limited$Ref_code<-NULL
connections_limited$Ref_code<-NULL
connections_limited$References<-NULL
connections_limited$order<-NULL

length(unique(cits$final_ref_code))


# papers which are not in our checked list
missing_lit<-all_lit[!all_lit$Key %in% unique(cits$final_ref_code), 
                     c("Key", "Bibtex", "Citation")]

as.character(missing_lit$Citation)
# run this only if you want to write Zotero folders
# source("zotero_link.R")


# replacing messy species names with GBIFIDs
colnames(cleaned_carabid_species)
colnames(connections_limited)
nrow(connections_limited)
nrow(cleaned_carabid_species)



connections_limited<-merge(connections_limited,cleaned_carabid_species,
            by.x = "Carabidae_name", by.y = "searched", all.x=T)

colnames(connections_limited)
colnames(connections_limited)[13]<-"Short_citation"
colnames(connections_limited)[15]<-"Full_citation"

colnames(connections_limited)[16]<-"Carabidae_GBIFID"
colnames(connections_limited)[17]<-"Carabidae_NCBIID"
colnames(connections_limited)[18]<-"Carabidae_rank"
colnames(connections_limited)[19]<-"Carabidae_full_name"
colnames(connections_limited)[20]<-"Carabidae_kingdom"
colnames(connections_limited)[21]<-"Carabidae_phylum"
colnames(connections_limited)[22]<-"Carabidae_class"
colnames(connections_limited)[23]<-"Carabidae_order"
colnames(connections_limited)[24]<-"Carabidae_family"
colnames(connections_limited)[25]<-"Carabidae_subfamily"
colnames(connections_limited)[26]<-"Carabidae_genus"
colnames(connections_limited)[27]<-"Carabidae_species"


connections_limited<-merge(connections_limited, 
                           updated_fungus_species,
                           by.x = "Fungi_name", by.y = "searched", all.x = T, all.y = F)

colnames(connections_limited)

colnames(connections_limited)[28]<-"Fungus_GBIFID"
colnames(connections_limited)[29]<-"Fungus_NCBIID"
colnames(connections_limited)[30]<-"Fungus_rank"
colnames(connections_limited)[31]<-"Fungus_full_name"
colnames(connections_limited)[32]<-"Fungus_kingdom"
colnames(connections_limited)[33]<-"Fungus_phylum"
colnames(connections_limited)[34]<-"Fungus_class"
colnames(connections_limited)[35]<-"Fungus_order"
colnames(connections_limited)[36]<-"Fungus_family"
colnames(connections_limited)[37]<-"Fungus_subfamily"
colnames(connections_limited)[38]<-"Fungus_genus"
colnames(connections_limited)[39]<-"Fungus_species"


# adding country IDs

colnames(shp)
connections_limited<-merge(connections_limited, 
                           shp,
                           by.x = "Country", by.y = "NAME", all.x = T)

colnames(connections_limited)


# removing data lines for replicated carabidae-fungus-country unique values

# keeping only the first mention
head(connections_limited)
CL_2_write<-connections_limited
CL_2_write$geometry<-NULL
write.table(CL_2_write, "../Raw_data/Master_list/cleaned_master_list.csv", 
            sep=",", row.names = F, fileEncoding = "UTF-8", quote = TRUE)

tokeep<-connections_limited %>%
    group_by(UN, Carabidae_GBIFID, Fungus_GBIFID) %>% 
    #filter(Publication.Year == min(Publication.Year, na.rm = T))
    summarize(minyear = min(Publication.Year, na.rm = T), 
              order = order.x[which(Publication.Year == min(Publication.Year, na.rm = T))]) %>% 
    distinct(UN, Carabidae_GBIFID, Fungus_GBIFID, minyear, order=first(order))

### high data priority over poor data 
# removing data points with missing countries if the same connection exists with a known country
# AND that connection has been reported at least as early as the one with the missing country


tokeep<-as.data.frame(tokeep)
unilist<-unique(tokeep[,c("Fungus_GBIFID", "Carabidae_GBIFID")])

keepornot<-function(x, NAmin, NNAmin)
{if (is.na(x["UN"]))
{ifelse(NAmin<NNAmin, T, F)} else
{return(T)}
}


tokeep$keep<-"YES"

for (n in 1:nrow(unilist))
{
    m<-unilist[n,]
    df<-tokeep[tokeep$Fungus_GBIFID==as.character(m[1]) & 
                   tokeep$Carabidae_GBIFID==as.character(m[2]),]
    
    NAmin<-min(df[is.na(df$UN), "minyear"], na.rm = T)
    NNAmin<-min(df[!is.na(df$UN), "minyear"], na.rm = T)
    
    if(NAmin=="Inf") {out<-T}
    
    else
        
    {out<-apply(df, 1, function(x) keepornot(x, NAmin, NNAmin))} 
    
    tokeep[tokeep$Fungus_GBIFID==as.character(m[1]) & 
               tokeep$Carabidae_GBIFID==as.character(m[2]), "keep"]<-out 
}

nrow(tokeep)
tokeep<-tokeep[tokeep$keep==T,]
is.character(tokeep$order)
final_connections<-connections_limited[connections_limited$order.x %in% tokeep$order,]
nrow(final_connections)

### should decide whether to keep genera in connections if 
### a species from the same genus is present with the same fungus/same place



### cleaning interaction types

unique(final_connections$Inter_type)
rownames(final_connections)[nrow(final_connections)]
final_connections[is.na(final_connections$Carabidae_GBIFID) & is.na(final_connections$Fungus_GBIFID),]


final_connections$Interaction_note<-final_connections$Inter_type
final_connections[final_connections$Inter_type=="feeding on", "Inter_type"]<-"Trophic"
final_connections[final_connections$Inter_type=="Feeding on fungus", "Inter_type"]<-"Trophic"
final_connections[final_connections$Inter_type=="x", "Inter_type"]<-"Unknown"
final_connections[final_connections$Inter_type=="", "Inter_type"]<-"Unknown"
final_connections[final_connections$Inter_type=="unknown", "Inter_type"]<-"Unknown"
final_connections[final_connections$Inter_type=="non-specified guest", "Inter_type"]<-"Unknown"
final_connections[final_connections$Inter_type=="Feeding on fungus/Living in fungus", "Inter_type"]<-"Unknown"
final_connections[final_connections$Inter_type=="Ectoparasite", "Inter_type"]<-"Parasitic"
final_connections[final_connections$Inter_type=="ectoparasite", "Inter_type"]<-"Parasitic"
final_connections[final_connections$Inter_type=="ectoparacitic", "Inter_type"]<-"Parasitic"
final_connections[final_connections$Inter_type=="ectoparaiste", "Inter_type"]<-"Parasitic"
final_connections[final_connections$Inter_type=="Ectoparasitic", "Inter_type"]<-"Parasitic"
final_connections[final_connections$Inter_type=="Ecotoparasite", "Inter_type"]<-"Parasitic"
final_connections[final_connections$Inter_type=="parasite", "Inter_type"]<-"Parasitic"
final_connections[final_connections$Inter_type=="Parasite", "Inter_type"]<-"Parasitic"
final_connections[final_connections$Inter_type=="parasitic", "Inter_type"]<-"Parasitic"
final_connections[final_connections$Inter_type=="endoparasite", "Inter_type"]<-"Parasitic"
final_connections[final_connections$Inter_type=="Entomopathogenic", "Inter_type"]<-"Pathogenic"
final_connections[final_connections$Inter_type=="entomopathogenic", "Inter_type"]<-"Pathogenic"
final_connections[final_connections$Inter_type=="entomopathogen", "Inter_type"]<-"Pathogenic"
final_connections[final_connections$Inter_type=="Entomopathogens", "Inter_type"]<-"Pathogenic"
final_connections[final_connections$Inter_type=="pathogens", "Inter_type"]<-"Pathogenic"
final_connections[final_connections$Inter_type=="Entomopathogen", "Inter_type"]<-"Pathogenic"
final_connections[final_connections$Inter_type=="Symbiont", "Inter_type"]<-"Mutualistic"
final_connections[final_connections$Fungus_genus=="Laboulbenia" & !is.na(final_connections$Fungus_genus), "Inter_type"]<-"Parasitic"


final_connections[final_connections$Inter_type=="Neutralism ", "Inter_type"]<-"Neutralistic"


final_connections$Carabidae_subfamily<-trimws(final_connections$Carabidae_subfamily, "both")
# 179 Laboulbeniales Mecyclothorax ambiguus
unknown_inter<-final_connections[final_connections$Inter_type=="Unknown", c("Fungus_full_name", "Inter_type")]
entomogenous_inter<-final_connections[final_connections$Inter_type=="entomogenous", c("Fungus_full_name", "Inter_type")]

all_unknown<-rbind(unknown_inter, entomogenous_inter)
# writing out unknown connections for manual checks

# write.table(all_unknown,
# file="../Calculated_data/temp_unknown_interactions2.csv",
# sep=",", row.names = F, fileEncoding = "UTF-8", quote = TRUE)


# Reading back manually updates unknowns interaction types
unknown_interactions_updated<-read.table("../Calculated_data/updated_unknown_interactions.csv", 
                                         header=T, sep=",", quote = "\"")
unknown_interactions_updated<-unique(unknown_interactions_updated)


known_inter<-unique(final_connections[final_connections$Fungus_full_name %in% unknown_interactions_updated$Fungus_full_name &
                      !final_connections$Inter_type=="Unknown" &
                      !final_connections$Inter_type=="entomogenous", c("Fungus_full_name", "Inter_type")])



all_unknown[!all_unknown$Fungus_full_name %in% unknown_interactions_updated$Fungus_full_name,] 

final_connections[final_connections$Inter_type=="Unknown", "Inter_type"]<-
    
    unknown_inter %>%
    left_join(unknown_interactions_updated, by="Fungus_full_name") %>%
    select(Inter_IBF) %>%
    mutate(Inter_type = ifelse(Inter_IBF == "", "Unknown",as.character(Inter_IBF))) %>%
    select(Inter_type)

final_connections[final_connections$Inter_type=="entomogenous", "Inter_type"]<-
    
    entomogenous_inter %>%
    left_join(unknown_interactions_updated, by="Fungus_full_name") %>%
    select(Inter_IBF) %>%
    mutate(Inter_type = ifelse(Inter_IBF == "", "Unknown",as.character(Inter_IBF))) %>%
    select(Inter_type)



unique(final_connections$Inter_type)

uncertain<-final_connections[is.na(final_connections$Carabidae_genus) | is.na(final_connections$Fungus_genus), 
                       c("Fungi_name", "Carabidae_name")]
uncertain[!uncertain$Carabidae_name %in% 
              final_connections[!is.na(final_connections$Fungus_genus) & 
                                    final_connections$Fungus_order=="Laboulbeniales", "Carabidae_name"],]

final_connections$Short_citation<-as.character(final_connections$Short_citation)
final_connections$Bibtex<-as.character(final_connections$Bibtex)
final_connections$Full_citation<-as.character(final_connections$Full_citation)


nrow(notes)
nrow(final_connections)
final_connections<-merge(final_connections, notes, by="Number", all.x = T)
final_connections$InteractionID<-1:nrow(final_connections)


##################################################
####### Note data cleaning and writing ##########
##################################################

colnames(final_connections)
notes2write<-final_connections[, c("InteractionID", "Fungus_note", "Carabidae_note", 
                                   "Connection_note",
                                   "Ref_note", "Country_note")]

notes2write<-melt(notes2write, id="InteractionID")
notes2write<-notes2write[!is.na(notes2write$value),]
notes2write<-notes2write[!notes2write$value=="",]
colnames(notes2write)<-c("InteractionID", "Column_name", "Note")

unique(notes2write$Column_name)
notes2write$Column_name<-as.character(notes2write$Column_name)
notes2write$InteractionID<-as.character(notes2write$InteractionID)

notes2write[notes2write$Column_name=="Fungus_note", "Column_name"]<-"Fungus_GBIF"
notes2write[notes2write$Column_name=="Carabidae_note", "Column_name"]<-"Carabidae_GBIF"
notes2write[notes2write$Column_name=="Connection_note", "Column_name"]<-"Association_type"
notes2write[notes2write$Column_name=="Ref_note", "Column_name"]<-"Reference_code"
notes2write[notes2write$Column_name=="Country_note", "Column_name"]<-"Country_code"


# write mysql
con <- dbConnect(MySQL(),
                 user="user", password="",
                 dbname="xxxx", host="localhost")

for (n in 1:nrow(notes2write))
{
    query <- paste0("INSERT INTO interaction_notes (",paste0(colnames(notes2write), collapse = ",") ,") 
                VALUES('", paste0(notes2write[n,], collapse = "', '"), "')")
    
    dbSendQuery(con, query)
}

dbDisconnect(con)

#write csv
write.table(notes2write, file="../Raw_data/Master_list/interaction_notes.csv", 
            sep=",", row.names = F, fileEncoding = "UTF-8", quote = TRUE)


##################################################
####### Interaction data cleaning and writing ##########
##################################################

colnames(final_connections)
connections2write<-final_connections[, c("InteractionID", "Carabidae_GBIFID", "Fungus_GBIFID","Inter_type.x", 
                                         "UN", "final_ref_code")]
colnames(connections2write)<-c("InteractionID","Carabidae_GBIF","Fungus_GBIF", "Association_type", 
                               "Country_code", "Reference_code")


con <- dbConnect(MySQL(),
                 user="user", password="",
                 dbname="xxxx", host="localhost")

for (n in 1:nrow(connections2write))
{
    query <- paste0("INSERT INTO interactions (",paste0(colnames(connections2write), collapse = ",") ,") 
                VALUES('", paste0(connections2write[n,], collapse = "', '"), "')")
    
    dbSendQuery(con, query)
}

# RMySQL writes 0-es instead of NA or NULL which prevents linking relationships in the table
query <- "UPDATE interactions
SET Country_code = NULL
WHERE Country_code = 0"
dbGetQuery(con, query)

dbDisconnect(con)

# writing connections into a csv
write.table(connections2write, file = "../Raw_data/Master_list/interactions.csv", 
            sep=",", row.names = F, fileEncoding = "UTF-8", quote = TRUE)

# Writing connections database into a single csv
final_connections$geometry<-NULL
Encoding(final_connections$Short_citation)<-"UTF-8"
Encoding(final_connections$Full_citation)<-"UTF-8"
Encoding(final_connections$Bibtex)<-"UTF-8"

final_connections$Full_citation<-gsub("\"", "'", final_connections$Full_citation)
colnames(final_connections)

# dropping unncessary columns
final_connections<-final_connections[,c(2, 5,10, 12:13, 15:43,47:48,55:59)]
### ezt kell futtatni
colnames(final_connections)[c(1:4,31:35)]<-c("Country_name", "Inter_type", "ReferenceID", "BibTex",
                                           "Region_code", "Subregion_code", "Country_abbreviation",
                                           "Country_code", "Region_name")

colnames(final_connections)
final_connections<-final_connections[,c("Carabidae_GBIFID", "Carabidae_full_name", 
                                        "Fungus_GBIFID", "Fungus_full_name", "Inter_type", 
                                        "Country_code","Country_name","Country_abbreviation",
                                        "Region_code", "Region_name", 
                                        "Subregion_code","Subregion_name", 
                                        "ReferenceID","Short_citation","Full_citation","BibTex",
                                        "Carabidae_NCBIID", "Carabidae_rank", 
                                        "Carabidae_kingdom", "Carabidae_phylum", 
                                        "Carabidae_class", "Carabidae_order", 
                                        "Carabidae_family", "Carabidae_subfamily", 
                                        "Carabidae_genus", "Carabidae_species", 
                                        "Fungus_NCBIID", "Fungus_rank",  
                                        "Fungus_kingdom", "Fungus_phylum", 
                                        "Fungus_class", "Fungus_order", 
                                        "Fungus_family", "Fungus_subfamily", 
                                        "Fungus_genus", "Fungus_species", 
                                        "Carabidae_note","Fungus_note", 
                                        "Connection_note","Country_note","Ref_note")]   
                                        
write.table(final_connections, file = "../Raw_data/Master_list/all_data_finalized.csv", 
            sep=",", row.names = F, fileEncoding = "UTF-8", quote = TRUE)



##################################################
####### Literature data cleaning and writing #####
##################################################

Literature_table<-all_lit[all_lit$Key %in% unique(cits$final_ref_code), c("Key", "Bibtex", "Citation")]

colnames(Literature_table)
Literature_table<-Literature_table[,c(1,3,2)]
head(Literature_table)
Literature_table$Citation<-gsub(pattern = "'", replacement = "\\\\'", as.character(Literature_table$Citation))
Literature_table$Bibtex<-gsub(pattern = "'", replacement = "\\\\'", as.character(Literature_table$Bibtex))
colnames(Literature_table)<-c("RefID", "Citation", "BibTex")

Literature_table<-Literature_table[Literature_table$RefID %in% unique(connections2write$Reference_code) , ]
nrow(Literature_table)

# Writing literature database into MySQL
Encoding(Literature_table$Citation) <- "UTF-8"
Encoding(Literature_table$BibTex) <- "UTF-8"
con <- dbConnect(MySQL(),
                 user="user", password="",
                 dbname="xxxx", host="localhost")

dbSendQuery(con,'SET NAMES utf8') 
for (n in 1:nrow(Literature_table))
{
    query <- paste0("INSERT INTO references_list (RefID, Citation, `BibTex`)
                VALUES('", paste0(Literature_table[n,], collapse = "', '"), "')")

    dbSendQuery(con, query)
}
dbDisconnect(con)


# writing csv

write.table(Literature_table, file="../Raw_data/Master_list/reference_list.csv", 
            sep=",", row.names = F, fileEncoding = "UTF-8", quote = TRUE)



##################################################
####### Geography data cleaning and writing ######
##################################################


colnames(shp)
head(shp)

shp2<-shp[,c(4, 5, 3, 2, 11, 1, 9, 10)]
colnames(shp2)
# CountryID = UN, Country_name = NAME, Country_abbreviation = ISO2, Subregion_code = SUBREGION, Subregion_name=Subregion_name, Region_code = REGION, Region_name = Region.Name, Country_GIS = geometry, Note 	
colnames(shp2)[1]<-"CountryID"
colnames(shp2)[2]<-"Country_name"
colnames(shp2)[3]<-"Country_abbreviation"
colnames(shp2)[4]<-"Subregion_code"
colnames(shp2)[5]<-"Subregion_name"
colnames(shp2)[6]<-"Region_code"
colnames(shp2)[7]<-"Region_name"

shp2<-st_make_valid(shp2)
# converting geometry to text and removing geometry - writing into MySQL does not work othervise

shp2$Country_GIS<- sf::st_as_text(shp2$geometry)
shp2<- shp2 %>% sf::st_set_geometry(NULL)

shp2<-shp2[!shp2$Subregion_code==900,]
shp2[shp2$Subregion_code==999,"Subregion_name"]<-"CCAMLR"


# Writing spatial database into MySQL
shp2$Country_GIS<- paste0("ST_MPointFromText('", shp2$Country_GIS, "')")

con <- dbConnect(MySQL(),
                 user="user", password="",
                 dbname="xxxx", host="localhost")


for (n in 1:nrow(shp2))
{
query <- paste0("INSERT INTO geography VALUES('", paste0(shp2[n,1:7], collapse = "', '"),
               "',", shp2[n,8], ")")

dbGetQuery(con, query)
}
dbDisconnect(con)
# Writing spatial database into a csv table

nrow(shp2)
# Multipolygon cannot be stored in
shp2$Country_GIS<-NULL
write.table(shp2, file = "../Raw_data/Master_list/geography.csv", 
                        sep=",", row.names = F, fileEncoding = "UTF-8", quote = TRUE)

