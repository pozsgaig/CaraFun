#
# Description
# ===========
#
# This function takes a bibliography table in data.table format (e.g. a '*.csv' output of
# Zotero) and concatenates selected fields and HTML tags to character string which can be
# used as a citation record similar to a bibliography item in scientific publications.
#
#
# Arguments
# =========
#
# bibRecord		data.table object of an exported Zotero table (.csv)
#
# ignore		character pattern which should be ignored when generating title case
#
# hide_with		single string used to temporarily replaces character strings passed to the ignore argument.
# 				This string should not appear anywhere else in the data to be processed and work with regex.
#
# trim          trim function to remove leading and tailing spaces. Attempt to avoid a function declaration within this function.
#
# VALUE
# =====
#
# This function returns a data.table of 2 columns.
# The first column holds the record key,
# the second the citation string.

cite <- function(bib, ignore=c("<i>.*?</i>", "<b>", "</b>"), hide_with="#####", trim){
	require("stringr")
	require("data.table")
	
    if (missing(trim)) {
        trim <- function(x) {gsub('^\\s+|\\s+$','',x)}
    }
     
 	# ===================== #
 	# --- TITLE format: --- #
 	# --------------------- #
 	# use title case for 'eng' records #
 	# ===================== #

 
	dummyText <- paste0(" ", rep(hide_with, length(ignore)), seq(along=ignore), " ")
 	
	bibRecord <- data.table::copy(bib)
   	originalText <- vector(mode="list", length=length(dummyText))
	engIDs <- which(bib$Language == "eng")

	# save specified pattern matches (see arg. 'ignore')
	for (i in seq(along=dummyText)){
		originalText[[i]] <- stringr::str_match_all(bib[engIDs, Title], ignore[i])
		bib[engIDs, Title := gsub(pattern=ignore[i], replacement=dummyText[i], Title)]
	}
	
	bib[engIDs, Title := tools::toTitleCase(Title)]
 
 	replacements <- as.logical(unlist(lapply(originalText[[1]], length)))

 	for (i in seq(along=dummyText)){
	 	for (j in seq(along=engIDs)[replacements]){
 			for (sciName in originalText[[i]][[j]]) {
			 	bib[engIDs[j], Title := sub(pattern=dummyText[i], replacement=sciName, Title)]
			}
		}
	}
	
 	# ========================= #
 	# --- concatenate text: --- #
 	# ========================= #
 
	item <- paste0(
	    	paste0(unlist(lapply(X=strsplit(as.character(bib$"Author"), split="(; )|,"),          #### AUTHOR
	    	                     FUN=function(x) {
	    	                         x[2] <- trim(gsub("(?<=\\s.).*?(\\s|$)",".",x[2],perl=T))
	    	                         x <- paste0(x, collapse=", ")
	    	                         x <- paste0("<b>", x, "</b> (", collapse=", ") 
	    	                         return(x)
	    	                         })),
	    	        bib$"Publication Year", "). ",                                                #### YEAR
	    	        bib$Title, ". "),                                                             #### TITLE
		# publisher = 
		sub(pattern = "<em></em> ",
		    replacement = "",
			x=ifelse(bib$"Item Type" %in% c("journalArticle", "bookSection", "conferencePaper"),
					 paste0("<em>", bib$"Publication Title", "</em> "),
					 ifelse(bib$"Item Type" %in% c("book", "bookSection", "thesis"),
						    paste0("<em>", bib$Publisher, "</em> "), ""))),
		# vol = 
		ifelse(as.logical(nchar(bib$Volume)),
					 paste0("<b>", bib$Volume, "</b>"), ""),
		# issue = 
		ifelse(as.logical(nchar(bib$Issue)),
					 paste0("(",bib$Issue,")"),""),
		# pages = 
		ifelse(as.logical(nchar(bib$Pages)),
					 paste0(": (",bib$Pages,") ")," ")) #last character is a space to find the middle
	
	item <- gsub(pattern="..", replacement=".", x=item, fixed=TRUE)

	# --- line break management: ---
	# output should have at least in 2 rows + a third for the DOI
	# stringr::str_wrap() did not work, since HTML tags can potentially be split
	midSpaceID <- unlist(lapply(stringr::str_locate_all(item, " "),
		function(x) {
			x <- x[x[,1] > x[nrow(x),1]/2,1][1]
			}))

	item <- paste0(substr(item, 1, midSpaceID-1), "<br>", substr(item, midSpaceID+1, nchar(item)))

	# --- DOI: ---
	item <- ifelse(as.logical(nchar(bib$DOI)),
				   paste0(item, "<br>doi: ", bib$DOI),
				   item)

    return(data.table::data.table(Key=bib$Key, Citation=item))
}