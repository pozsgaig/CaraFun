# DESCRIPTION
# 
# Generate a table with taxonomic data for a vector of species/genus/family names querying 
# the GBIF db as well as for subfamily and NCBI ID information the NCBI db. Synonym names
# are updated to accepted names based on the GBIF db. Information of subspecies are returned
# on the species level.
#
#
# ARGUMENTS:
# 
# [REQUIRED ARGUMENTS]
#	taxon_list		a vector of taxonomic names
# 
# [OPTIONAL ARGUEMENTS]
#	TwoWords    	when set to TRUE the function will attempt to cut taxonomic names at the
# 					second space. Autors as well as all other characters will be removed.
# 					This potentially reduces the run time since the unique set of name
# 					might get smaller. On the other hand ambiguities might be introduced.
# 					E.g: "Dendrocalamus latiflorus Munro" ==> "Dendrocalamus latiflorus"
# 					(default = FALSE)
#
#	TwoWordsOnFail	Up to two attempts to find a match in the GBIF db are made. The first
# 					uses the complete character string, in case of a negative result only
# 					the first two words are used for a second lookup (similar to TwoWords = T).
# 					The TwoWords works vectorised on the entire list, while TwoWordsOnFail
# 					modifies names on a case to case basis.
# 					(default = T)
# 
#	fixCase			if TRUE, standardising the case of scientific names (sentence case:
# 					first letter to upper case and up to the end of the second word (space
# 					delimited) to lower case). All characters after the second space
# 					(including subspecies names) are ignored.
# 					E.g: "dendroCalamus Latiflorus Munro" ==> "Dendrocalamus latiflorus Munro"
# 					(default = FALSE)
# 					
#	removeBrackets	matches opening and closing round brackets and removes them including
# 					every character in between. This might target author names but outdated
# 					genus names as well. In case removeBrackets = T, brackets are removed
# 					prior to all other modifications.
# 					E.g: Acyrthosiphon pisum (Harris) ==> Acyrthosiphon pisum
# 					     Entomophthora (Tarichium) atrosperma ==> Entomophthora atrosperma
# 					But: Entomophthora coronata (Costantin) Kevorkian ==> Entomophthora coronata Kevorkian
# 					(default = T)
# 
#	strict			The strict argument of rgbif::name_backbone. In case the lookup in the 
# 					GBIF db returns a FUZZY match, this fuzzy match is used to query the
# 					NCBI db. This switch is not visible in the output table.
# 					(default = T)
# 
#	unknownSpecies	character placeholder for unknown species. All matches in taxon_list
# 					will be excluded from the lookup process and return an empty row.
#
#	interactive_correction	This script makes an attempt to identify special characters
# 					independent if this option. This means all unicode characters > 125
# 					are detected and the corresponding names are listed in the console.
# 					If interactive_correction = TRUE the user can deside if those listed
# 					names should be manually changed or not before continuing. (With 'return'
# 					single names can be skipped.) This is done in a loop, so that after
# 					modifying all names the user can deside to go a second time through the
# 					list and continue (not restart) the modification before finally allowing
# 					the script to continue. The manually done modifications will manifest
# 					in the searchTerm column in the output.
# 					(default = T)
#
#	truncateCF		If FALSE 'cf' itself will be removed from the name: "Sipha cf. flava" <- "Sipha flava"
# 					If TRUE only text in front of 'cf' will be kept: "Sipha cf. flava" <- "Sipha"
# 					(BUT: "Cf. Sipha flava" <- ""). Resulting empty strings are replace with 
# 					the value of the unknownSpecies argument.
#					'cf' and 'cf.' are treated similarly.
# 					(default = T)
#
#	independentNCBI	If TRUE the original search term (synonym and subspecies to species updated)
# 					is used to query the NCBI db. If FALSE the species name of a (EXACT
#					or FUZZY) GBIF lookup result is used to query the NCBI db. (In case of
# 					a subspecies or a synonym as judged by the GBIF db, the search term of the
# 					second search (GBIF db) and the NCBI db search term are identical - with
# 					other words: a second GBIF search does not update the search term used
# 					to query the NCBI db).
# 					(default = F)
#
#	output_file 	Wether or not to write results to a file. output_file can either be logical
# 					(in case output_file = T the file is named Taxon_list_<system Date>), or
# 					accepts a custom file name. The file ending is '.csv' if sep ="," and ".txt"
#					otherwise.
# 					This script looks up unique names so that this initial and temporary
# 					result table is potentially shorter than the input vector. If output_file,
# 					is TRUE or holds an file name, two temporary files are created while 
# 					running, which store the index pairs in the unique list and the input
# 					vector (*_IDs.txt) and the result table (*_tmp). The file with the 
# 					result table is updated after each lookup, but does not contain the
# 					original input term (only the searchTerm). Once all names have been
# 					queried successfully, the final result table (including the initial
# 					input terms) are saved to a file and the two previously described, 
# 					temporary files are deleted.
# 					(default = FALSE)
# 
#	sep         	sep argument of write.table() defaulting to tab.
#
#	append_Table	A data frame as produced by this functions (R-object, not file!) which
# 					should be appended by rbind. Alternatively a new table will be created.
# 
#	verbose     	text output to the console about the look-up process (not to be confused
# 					with rgbif::name_backbone_versone() which is not used in this script)
# 					(default = T)
#
#
#
# VALUES:
#
# returns a data frame with the following columns:
#	input		search term as passed to the function
#	searchTerm	search term used for the lookup (after automated corrections)
#	GBIFID		ID of the GBIF database
#	NCBIID		ID of the NCBI database
#	rank		taxonomic rank (GBIF db)
#	full_name	complete scientific name including author
#	kingdom		taxonomic kingdom (GBIF db)
#	phylum		taxonomic phylum (GBIF db)
#	class		taxonomic class (GBIF db)
#	order		taxonomic order (GBIF db)
#	family		taxonomic family (GBIF db)
#	subfamily	taxonomic subfamily (NCBI db)
#	genus		taxonomic genus (GBIF db)
#	species		taxonomic species (GBIF db)
#	matchType	type of match to the GBIF db. In case 'searchTearm' is either a synonym or a subspecies, two qualifiers are concatenated: 
# 				the first for the synonym (or subspecies) match, the second for the update with the 'species' result of the first match.
# 				The qualifier of the initial query starts with t/f (for TRUE/FALSE) evaluating if the result is a synonym and a subspecies.
# 				(E.g: "tfEXACT" stands for an EXACT match of a synonym of anything but a subspecies;
# 				      "ftEXACT" stands for an EXACT match of not-a-synonym of a subspecies
# 				      "ttEXACT" stands for an EXACT match of a synonym of a subspecies
# 				      "ftFUZZY" stands for a  FUZZY match of not a synonym which is a subspecies)
# 				(see rgbif::name_backbone() for details)
#
#
#
# NOTE:
#
# Unusual whitespace are automatically converted to a standard space using gsub(pattern="(*UCP)\\s+", " ", x, perl=T). 
# In taxon_names " sp." and " spp." is automatically removed (note the initial space). The period is optional.
#


checkSpecies<- function(taxon_list, kingdom = NULL, phylum = NULL, class = NULL, TwoWords=F, TwoWordsOnFail=T, fixCase=F, removeBrackets=T,
						strict, unknownSpecies, truncateCF=T, interactive_correction=T, independentNCBI,
						output_file, sep, append_Table, verbose=T) {
	require(rgbif)
	require(taxize)
	require(grr)

	trim <- function(x) {gsub('^\\s+|\\s+$','',x)}

	# -- handeling arguments ---
	if (missing(sep)) { sep <- "\t" }
		
	if (missing(output_file)) {
		output_file <- ""
	}else{
		if ((output_file == FALSE) | (output_file == "")) {
			output_file = ""
		}else{
			if (output_file==TRUE) {
				output_file <- paste0("Taxon_list_", gsub("-","",Sys.Date()))
			}
			output_file <- gsub("\\..*", "", output_file)
			output_file <- ifelse (sep==',',
				paste0(output_file,".csv"),
				paste0(output_file,".txt"))

			output_file_tmp <- paste0(substring(first=1, last=nchar(output_file)-4, text=output_file),
									  "_tmp",
									  substring(first=nchar(output_file)-3, last=nchar(output_file), text=output_file))
			output_file_index <- paste0(substring(first=1, last=nchar(output_file)-4, text=output_file),
									  "_IDs",
									  substring(first=nchar(output_file)-3, last=nchar(output_file), text=output_file))
		}
	}
	
	
	ifelse (missing(unknownSpecies),
			unknownSpecies <- "",
			unknownSpecies<- tolower(unknownSpecies))
	
	if (missing(strict)) {
		strict <- T
 	}
	
	if (missing(independentNCBI)) { independentNCBI <- F }

	if (missing(append_Table)) {
		taxon_table <- NULL
	}else{
		if(length(append_Table) == 15) {
			taxon_table <- append_Table
		}else{
			if (!is.null(append_Table)) {
				cat("\nDifferent column number of 'append_Table' and the function's result detected.\nSetting 'append_Table' to NULL.\n\n")
			}
			taxon_table <- NULL
		}
	}
	
	
	# ----------------------------------------------------

	# --- formating species names ---	

	taxon_list <- as.character(taxon_list)
	
	# spaces: remove leading, tailing spaces as well as reduce multiple spaces to single space.
	# in addition: all Unicode whitespace are set to the regular whitespace (ascii: 32)
	taxon_list <- trim(gsub("(*UCP)\\s+", " ", taxon_list,perl=T))
	
	taxon_list_original <- taxon_list

	# remove brackets with all contnet
	if (removeBrackets) {
		# multiple spaces might be newly introduced need removal. (... and the original names should have been treated before too)
		taxon_list <- gsub("\\s+", " ", gsub("\\(.*?\\)", "", taxon_list))
	}

	
	# --- check for cf. ---	
	cf_IDs <- grep(pattern='\\b[Cc][Ff]\\b\\.?', taxon_list)

	if (length(cf_IDs)>0) {
		cat("\nSpecies names containig 'cf.' detected.\n")
		print(data.frame(ID=cf_IDs, Name=taxon_list[cf_IDs], stringsAsFactors=F))
		if (truncateCF) {
			taxon_list[cf_IDs] <- trim(gsub(pattern='\\b[Cc][Ff]\\b.*', replacement='', x=taxon_list[cf_IDs]))
			taxon_list[cf_IDs[taxon_list[cf_IDs]== ""]] <- unknownSpecies
			cat("\n\nNames were truncated before 'cf.'. (arg: truncateCF=T)\n")
			print(data.frame(ID=cf_IDs, Name=taxon_list[cf_IDs], stringsAsFactors=F))
		}else{
			taxon_list[cf_IDs] <- trim(gsub(pattern="\\s+", replacement=" ",
											x=gsub(pattern='\\b[Cc][Ff]\\b\\.?', replacement='',
												   x=taxon_list[cf_IDs])))
			cat("\n\n'cf.' has been removed from name(s). (arg: truncateCF=F)\n")
			print(data.frame(ID=cf_IDs, Name=taxon_list[cf_IDs], stringsAsFactors=F))
			cat("\n")
		}
	}
	
	# --- reduce to the first two words and/or control capitalisation of the first two words
	if (TwoWords | fixCase) {
		taxon_list <-  strsplit(taxon_list, split="")
		
		# add index of second space as first character
		taxon_list <- lapply(taxon_list, function(x) {
			n <- which(x==" ")[2]
			n[is.na(n)] <- 0
			return(c(n,x))
		})
		
		secSpace <- as.logical(as.numeric(unlist(lapply(taxon_list, function(x) {x[1]}))))
		
		# keep everything until the second space
		if (TwoWords) {
			taxon_list[secSpace] <- lapply(taxon_list[secSpace], 
				function(specName) {
					specName <- specName[1:(as.numeric(specName[1]))] # we added the ID in the front: no subtraction necessary
				})
		}
		
		# make 1st and 2nd word lower case except 1st letter upper case (consequentially possible author names are not affected)
		if (fixCase){
			taxon_list[secSpace] <- lapply(taxon_list[secSpace],
				function(specName) {
					lastID <- as.numeric(specName[1])
					specName[2] <- toupper(specName[2])
					specName[3:lastID] <- tolower(specName[3:lastID])
					return(specName)
				})
			taxon_list[!secSpace] <- lapply(taxon_list[!secSpace],
				function(specName) {
					specName[-1] <- tolower(specName[-1])
					specName[2] <- toupper(specName[2])
					return(specName)
				})
		}
		
		# collapse species names w/o the 2nd space index
		taxon_list <- unlist(lapply(taxon_list, function(x) {paste0(x[-1], collapse='')}))
	}

	# --- check for special characters ---	
	specialChars <- which(unlist(lapply(
		X=sapply(taxon_list, utf8ToInt), 
		FUN=function(x) {
#			x[x==160] <- 32 # ignore 'non-breaking space' : no need anymore because of (*UCP)
			return(any(x > 125))
		}
	)))
	
	specialChars <- data.frame(
		ID=specialChars,
		Name=taxon_list[specialChars],
		stringsAsFactors=F)
	
	# --- feedback about special characters ---
	# --- IF (interactive_correction is TRUE) THEN { user can change names }
	if (nrow(specialChars) != 0) {
		cat("Warning:\nSpecial characters detected in the following names.\n\n")
		print(specialChars) # Rhopalosiphon
		if (interactive_correction) {
			specCharCorrect <- readline(prompt="\nChange listed names one by one?\n(y/n): ")
			if (tolower(specCharCorrect)=="y") {
				cat("\n\nEnter the corrected name and confirm with 'return'.\nPress 'enter' to skip (preserves previous corrections).\nPress 'esc' to aboard and do not make any corrections.\n\n")
				specialChars <- data.frame(specialChars, Correction=rep("", nrow(specialChars)))
				continue <- F
				while (!continue) {
					specCharCorrect <- F
					for (i in seq(1,nrow(specialChars))) {
						newName <-ifelse(specialChars[i,"Correction"]=="", "", paste0(' ("', specialChars[i,"Correction"], '")'))
						newName <- readline(prompt=paste0('Change "', specialChars[i,"Name"], '"', newName, ' in: '))
						specialChars[i,"Correction"] <- ifelse (newName=="", 
								ifelse(specialChars[i,"Correction"]== "",
									specialChars[i,"Name"],
									specialChars[i,"Correction"]),
								newName)
					}
					cat("\n")
					print(specialChars)
					cat("\n")
					
					specCharCorrect <-T
					continue <- ifelse(tolower(readline(prompt="Make further changes?\n(y/n): ")) == "y", F, T)
					cat("\n")
				}
				if (specCharCorrect) {
					taxon_list[specialChars$ID] <- specialChars$Correction
					cat("\nNames have been updated ...\n\n")
				}else{
					cat("\nContinue without updating names ...\n\n")
				}
				rm(continue, newName, specCharCorrect, specialChars)
			}
		}
	}else{
		cat("\nNo indication of special characters found. Format check successful.\n\n")
	}

	# --- remove sp. and spp. ---
	taxon_list <- sub("\\ssp\\.?$", "", taxon_list)
	taxon_list <- trim( sub("\\sspp\\.?$", "", taxon_list) )

	# --- look up each name only once ---
	taxon_list_unique <- unique(taxon_list)
	
	# --- index to re-contstruct the original sequence	
	specIndex <- grr::matches(taxon_list_unique, taxon_list,list=TRUE,all.y=FALSE) # fast version
	#specIndex <- lapply(B, function(x) which(A %in% x)) # slow version
	specIndex <- data.frame(tmp_file=rep(as.integer(names(specIndex)), lengths(specIndex)),
							originalSequence=as.integer(unlist(specIndex)))

	if (nchar(output_file)>0){
		write.table(specIndex, file=output_file_index, sep=sep, col.names = T, row.names = F)
	}


	taxon_list_lkup <- 
		sapply(taxon_list_unique, simplify=F, function(species_name) {
						
			if (verbose){
				cat(paste0("\nProcessing:  ", species_name, "\n\n"))
			}
	
			taxon_line <- tryCatch({
	
				taxon_line <-  vector('character', 14L)
				names(taxon_line)<-c("searchTerm", "GBIFID", "NCBIID", "rank", "full_name", "kingdom", "phylum", "class", "order", "family", "subfamily", "genus", "species", "matchType")
	
				if (tolower(species_name)==unknownSpecies){
					taxon_line["searchTerm"] <- species_name
				}else{
					nbb <- lookupSpecies(species_name=species_name, strict=strict, verbose=verbose, 
					kingdom = kingdom, phylum = phylum, class = class)		
					if (!TwoWords & TwoWordsOnFail & nbb$nbb$matchType=="NONE") {
						species_name <- unlist(strsplit(species_name, split=" "))
						n <- which(species_name==" ")[2]-1
						n[is.na(n)] <- length(species_name)
						species_name <- paste0(species_name[1:n], collapse=" ")
				
						nbb <- lookupSpecies(species_name=species_name, strict=strict, verbose=verbose,
						kingdom = kingdom, phylum = phylum, class = class)
					}
					
					ncbi <- nbb$ncbi
					nbb <- nbb$nbb
					
					# --- IF ( subfamily exists)    THEN { get subfamily from NCBI } ---
					# --- IF ( subspecies NCBI-ID ) THEN { switch to species NCBI-ID }  ---
					subfamily <- NA
					if (!is.na(ncbi)) {
						ncbi_taxon <- classification(ncbi[[1]])[[1]] # taxonomy table; columns: name - rank - id
						if (ncbi_taxon[ncbi_taxon$id==ncbi[[1]], "rank"] == "subspecies") {
							ncbi[[1]] <- ncbi_taxon[ncbi_taxon$rank=="species", "id"]
						}
						if (any(ncbi_taxon$rank=="subfamily")) {
							subfamily <- ncbi_taxon[ncbi_taxon$rank=="subfamily", "name"]
						}
						rm(ncbi_taxon)
					}
			
					# prepare output
					# FYI: These assignments follow the idea that the responsibility of
					# not returning a subspecies record lies at the lookupSpecies() function
					taxon_line['searchTerm'] <- species_name
					taxon_line['NCBIID'] <- as.character(ncbi[[1]])

					if ("rank" %in% names(nbb)) {
						taxon_line['GBIFID'] <- as.character(nbb[paste0(tolower(nbb$rank),"Key")])
						taxon_line['rank'] <- nbb$rank
					}else{
						taxon_line['GBIFID'] <- NA
						taxon_line['rank'] <- NA
					}
					## FYI: The next two out-commented lines does not fit together. The GBIF above and the rank below do fit.
					## The GBIF below works alternatively to the one above if reliably no subspecies or synonym is returned.
					# taxon_line['GBIFID'] <- ifelse("usageKey" %in% names(nbb), nbb$usageKey, NA)
					# taxon_line['rank'] <- ifelse("rank" %in% names(nbb),
					# 							 ifelse(nbb$rank=="subspecies", "species", nbb$rank),
					# 							 NA)
					taxon_line['full_name'] <- ifelse("scientificName" %in% names(nbb), nbb$scientificName, NA)					
					taxon_line['kingdom'] <- ifelse("kingdom" %in% names(nbb), nbb$kingdom, NA)
					taxon_line['phylum'] <- ifelse("phylum" %in% names(nbb), nbb$phylum, NA)
					taxon_line['class'] <- ifelse("class" %in% names(nbb), nbb$class, NA)
					taxon_line['order'] <- ifelse("order" %in% names(nbb), nbb$order, NA)
					taxon_line['family'] <- ifelse("family" %in% names(nbb), nbb$family, NA)
					taxon_line['subfamily'] <- subfamily
					taxon_line['genus'] <- ifelse("genus" %in% names(nbb), nbb$genus, NA)
					taxon_line['species'] <- ifelse("species" %in% names(nbb), nbb$species, NA)
					taxon_line['matchType'] <- ifelse("matchType" %in% names(nbb), nbb$matchType, NA)
					
					if (nchar(output_file)>0){
						suppressWarnings(
							write.table(as.data.frame(t(taxon_line)), file=output_file_tmp, sep=sep, col.names = !file.exists(output_file_tmp), row.names = F, append = T)
						)
					}
			
					if (verbose){
						cat(paste0("\nSearch finished for: ", species_name, " - Subfamily: ", taxon_line['subfamily'],'\n# ',paste0(rep('-',80),collapse=''),' #\n\n'))
					}
				}
				taxon_line
			},
			error=function(e) {
				print(species_name)
				print(e)
				taxon_line <-  vector('character', 14L)
				names(taxon_line)<-c("searchTerm", "GBIFID", "NCBIID", "rank", "full_name", "kingdom", "phylum", "class", "order", "family", "subfamily", "genus", "species", "matchType")
	
				taxon_line["searchTerm"] <- species_name
			})
			return(taxon_line)
	})

	taxon_list_lkup <- data.frame(
		do.call(rbind,taxon_list_lkup),
		stringsAsFactors=F)
	
	taxon_list <- as.data.frame(matrix(rep("", 14*length(taxon_list)), ncol=14), stringsAsFactors =F)
	colnames(taxon_list) <- c("searchTerm", "GBIFID", "NCBIID", "rank", "full_name", "kingdom", "phylum", "class", "order", "family", "subfamily", "genus", "species", "matchType")

	taxon_list[specIndex[,2],] <- taxon_list_lkup[specIndex[,1],]
	taxon_list <- data.frame(input=taxon_list_original, taxon_list)
	
	taxon_table <- rbind(taxon_table, taxon_list, stringsAsFactors=F)
	
	if (nchar(output_file)>0){
		suppressWarnings(
			write.table(as.data.frame(taxon_table), file=output_file, sep=sep, col.names = T, row.names = F)
		)
		if(file.exists(output_file)) {
			if(file.exists(output_file_tmp)) { file.remove(output_file_tmp)}
			if(file.exists(output_file_index)) { file.remove(output_file_index)}
		}
	}

	return(invisible(taxon_table))
}


lookupSpecies <- function(species_name, strict, verbose=T, independentNCBI=F, kingdom = NULL, phylum = NULL, class = NULL){
	if (!verbose) {
		sink(tempfile())
		on.exit(sink())
	}
	cat(paste0("rgbif::name_backbone(,", species_name, ",strict = ", strict, ", curlopts = list(timeout_ms=20000), 
			kingdom = ", kingdom, ", phylum = ", phylum, ", class = ", class, ")"))
	lookup_res <- try({
		nbb <- tryCatch(
			nbb <- rgbif::name_backbone(species_name, strict = strict, curlopts = list(timeout_ms=20000), 
			kingdom = kingdom, phylum = phylum, class = class),
				error = function(e) { print(e) },
			finally={
				if(!exists("nbb")) {
					cat(paste0("\nLOOKUP EXCEPTION: ", species_name, " not found in GBIF.\n"))
					nbb <- setNames(object=c("FALSE", "NONE"), nm=c("synonym", "matchType") )
				}
			}
		)
		# --- IF (subspecies or synonym) THEN {
		# 	1. use the returned 'species' term and run a new lookup;
		# 	2. use the returned 'species' term also for the NCBI database;
		# 	3. finally concatenate the 'matchType' of both runs (GBIF db) }
		isSubspecies <- ifelse('rank' %in% names(nbb), ifelse(nbb$rank=='SUBSPECIES', T, F), F)
		
		if (as.logical(nbb["synonym"]) | isSubspecies) { # (matchType="NONE" results in synonym=FALSE)
			initial_matchType <- paste0(tolower(substring(first=1, last=1, nbb["synonym"])),
										tolower(substring(first=1, last=1, isSubspecies)),
										nbb$matchType)
			species_name <- ifelse (isSubspecies,
									as.character(nbb["species"]),
									as.character(nbb[tolower(nbb$rank)]))
			nbb <- try(rgbif::name_backbone(species_name, strict = strict, curlopts = list(timeout_ms=20000),
			kingdom = kingdom, phylum = phylum, class = class))
			nbb$matchType <- paste0(initial_matchType, "/", nbb$matchType)
		}
		
		if (independentNCBI) {
			species_name_ncbi <- species_name
		}else{
			species_name_ncbi <- ifelse (nbb["matchType"] %in% c("EXACT", "FUZZY"),
										 as.character(nbb[tolower(nbb$rank)]),
										 species_name)
		}
		
		ncbi <- tryCatch(
			ncbi <- ifelse(verbose,
						   taxize::get_ids(sci_com = species_name_ncbi, db = "ncbi", rows=1, timeout_ms=20000),
						   suppressMessages(taxize::get_ids(sci_com = species_name_ncbi, db = "ncbi", rows=1, timeout_ms=20000))),
				error = function(e) { print(e) },
				finally={
					if(!exists("ncbi")) {
						cat(paste0("\n\nLOOKUP EXCEPTION: ", species_name_ncbi, " not found in NCBI.\n"))
						ncbi <- setNames(object=NA, nm="ncbi" )
					}
				}
		)
		
		lookup_res <- list(nbb=nbb, ncbi=ncbi)
	})

	return(lookup_res)
}
