## Must be logically organized and a and further data input should be made possible

# Generating Carabidae and Fungi species lists

carabid_species_pre<-unique(connections$Carabidae_name)

taxon_exclude<-c("x", "X", "NA", "", "Carabidae")
carabid_species_pre<-carabid_species_pre[!carabid_species_pre %in% taxon_exclude]
carabid_species_pre<-trimws(carabid_species_pre, "both")
carabid_species_pre<-unique(carabid_species_pre)

# first Carabidae taxon cleanup through GBIF
carabid_species_list<-taxon_cleanup(carabid_species_pre, write2file = T, 
                                    output_file = "../Raw_data/Master_list/pre_carab_list_updated.csv")
# at this stage there are some manual adjustments in the files which is saved on a new name


# new Carabidae taxon hierarchy should be read back and taxonomy chech ran again
species_solved<-read.xlsx2("../Raw_data/Master_list/raw_carab_list_updated_final.xlsx", 
                           sheetName = "Solved", as.data.frame = T, header = T, stringsAsFactors=FALSE)
carabid_species_list_2<-taxon_cleanup(species_solved$jav_fajnev , write2file = T, 
                                    output_file = "../Raw_data/Master_list/pre_carab_list_updated_solved.csv")



fungal_species_pre<-unique(connections$Fungi_name)
fungal_species_pre<-fungal_species_pre[!carabid_species_pre %in% taxon_exclude]
fungal_species_pre<-trimws(fungal_species_pre, "both")
fungal_species_pre<-unique(fungal_species_pre)

fungal_species_list<-taxon_cleanup(fungal_species_pre, write2file = T, output_file = "../Raw_data/Master_list/pre_fungal_list_updated.csv")

