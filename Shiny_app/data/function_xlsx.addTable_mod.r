xlsx.addTable_mod <- function (wb, sheet, data, startRow = 9, startCol = 2, col.names = TRUE, 
    row.names = FALSE, columnWidth = 14, fontColor = "#FFFFFF", 
    fontSize = 12, colnamesFill = "white", 
    rowFill = c("white", "white")) 
{
	require("xlsx")
    
	col.n = ncol(data)
    
	column.style <- vector(mode="list", length=col.n)
	column.style[1:col.n] <- list(xlsx::CellStyle(wb, font = Font(wb, color = fontColor, heightInPoints = fontSize)))
	names(column.style) <- as.character(1:ncol(data))

	# row name style is not plugged in since it should not be used. The variabel do exist though, because column name style is build on it, and it can easily reversed (see addDataFrame() function).
	TABLE_ROWNAMES_STYLE <- xlsx::CellStyle(wb,
    										font = Font(wb,
    								  					isBold = TRUE,
    								  					color = fontColor,
    								  					heightInPoints = fontSize))
    
	TABLE_COLNAMES_STYLE <- TABLE_ROWNAMES_STYLE +
    						xlsx::Alignment(wrapText = TRUE,
    										horizontal = "ALIGN_CENTER") + 
    						xlsx::Border(color = "black", 
										 position = c("TOP", "BOTTOM"),
										 pen = c("BORDER_THIN", "BORDER_THICK"))
            
	if (colnamesFill != "white") {
        TABLE_COLNAMES_STYLE <- TABLE_COLNAMES_STYLE + 
								xlsx::Fill(foregroundColor = colnamesFill, 
										   backgroundColor = colnamesFill)
	}
      
	xlsx::addDataFrame(data, sheet, 
    				   startRow = startRow,
    				   startColumn = startCol,
    				   col.names = col.names,
    				   row.names = row.names,
    				   colnamesStyle = TABLE_COLNAMES_STYLE,
    				   rownamesStyle = NULL, # TABLE_ROWNAMES_STYLE,
    				   colStyle = column.style)
    			 
	xlsx::setColumnWidth(sheet,
    					 colIndex =1:(col.n + startCol),
    					 colWidth = columnWidth)

	if (!all(rowFill == c("white", "white"))) {
    
		col.n = col.n + row.names
		row.n = nrow(data)

		cb <- xlsx::CellBlock(sheet,
							  startRow = startRow + col.names,
							  startColumn = startCol, 
							  noRows = row.n,
							  noColumns = col.n,
							  create = FALSE)
            
#		fill_even = xlsx::Fill(foregroundColor = rowFill[2], backgroundColor = rowFill[2])
#		fill_odd = xlsx::Fill(foregroundColor = rowFill[1], backgroundColor = rowFill[1])

		CB.setFill(cb,
				   fill = xlsx::Fill(foregroundColor = rowFill[2], backgroundColor = rowFill[2]), # fill_even,
				   rowIndex = rep(seq(2,row.n,2), each=col.n),
				   colIndex = rep(1:col.n, times=floor(row.n/2)))
		CB.setFill(cb,
				   fill = xlsx::Fill(foregroundColor = rowFill[1], backgroundColor = rowFill[1]), # fill_odd,
				   rowIndex = rep(seq(1,row.n,2), each=col.n),
				   colIndex = rep(1:col.n, times=ceiling(row.n/2)))
    }
}