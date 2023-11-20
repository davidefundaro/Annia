#example of dataframe for testing
#data <- data.frame(
#  Name = c("Alice", "Bob", "Charlie", "David"),
#  Age = c(25, 30, 28, 22),
#  Score = c(95, 88, 92, 78000))

# Create a workbook and add a worksheet
wb <- createWorkbook()
sheet <- addWorksheet(wb, sheetName = "Report_Loans", gridLines = FALSE)
sheetname <- "Report_Loans"

# Create styles for the title, header row, even rows, and odd rows, and for numeric values to set as k
ReportTitleStyle <- createStyle(fontSize = 20,textDecoration = c('bold','underline'))
titleStyle <- createStyle(fontSize = 14, textDecoration = "bold", border = "Bottom", borderColour = "black")
header_style <- createStyle(fontColour = "white", fgFill = "#6495ED", border = "TopBottom",
                            textDecoration = "bold", fontSize = 12)
total_style <- createStyle(fontSize = 12, fgFill = "#7FFFD4", textDecoration = "bold", halign = "right") #7FFFD4 #66CDAA
even_row_style <- createStyle(fontSize = 12, fgFill = "white", halign = "right") #63B8FF skyblue1
odd_row_style <- createStyle(fontSize = 12, fgFill = "white", halign = "right")
row_style <- createStyle(fontSize = 12, fgFill = "white", halign = "right")
custom_format <- createStyle(numFmt = "0.0,\"k\"")


###------------------------------------------###
#---function to create table with k for miles-----
###------------------------------------------###
create_table_k_for_miles <- function(workbook, sheetname, table, titolo, col_iniziale, row_iniziale) {
  nrows <- nrow(table)
  ncols <- ncol(table)
  
  writeData(workbook, sheetname, x = titolo, startCol = col_iniziale, startRow = row_iniziale -1)
  addStyle(workbook, sheet = sheetname, style = titleStyle, rows = row_iniziale -1, cols = col_iniziale)
  
  writeDataTable(workbook, sheet = sheetname, x = table, startCol = col_iniziale, startRow = row_iniziale)
 
  for (col in 1:ncols) {
    addStyle(workbook, sheet = sheetname, style = header_style, rows = row_iniziale, cols = col + col_iniziale - 1)
    for (row in 2:(nrows + 1)) {
      cell_value <- table[row - 1, col]
      if (is.numeric(cell_value) && cell_value >= 1000) {
        addStyle(workbook, sheet = sheetname, style = custom_format, rows = row + row_iniziale - 1, cols = col + col_iniziale - 1, stack = TRUE)
      }
      if (row %% 2 == 0) {
        addStyle(workbook, sheet = sheetname, style = even_row_style, rows = row + row_iniziale - 1, cols = col + col_iniziale - 1, stack = TRUE)
      } else {
        addStyle(workbook, sheet = sheetname, style = odd_row_style, rows = row + row_iniziale - 1, cols = col + col_iniziale - 1, stack = TRUE)
      }
    }
    setColWidths(workbook, sheet = sheetname, cols = col + col_iniziale - 1, widths = "auto")
  }
}


###------------------------------------------###
#--- function to create table without k -----
###------------------------------------------###
create_table_no_k <- function(workbook, sheetname, table, titolo, col_iniziale, row_iniziale) {
  nrows <- nrow(table)
  ncols <- ncol(table)
  
  writeData(workbook, sheetname, x = titolo, startCol = col_iniziale, startRow = row_iniziale -1)
  addStyle(workbook, sheet = sheetname, style = titleStyle, rows = row_iniziale -1, cols = col_iniziale)
  
  writeDataTable(workbook, sheet = sheetname, x = table, startCol = col_iniziale, startRow = row_iniziale)
  
  for (col in 1:ncols) {
    addStyle(workbook, sheet = sheetname, style = header_style, rows = row_iniziale, cols = col + col_iniziale - 1)
    for (row in 2:(nrows + 1)) {
      cell_value <- table[row - 1, col]
      if (row %% 2 == 0) {
        addStyle(workbook, sheet = sheetname, style = even_row_style, rows = row + row_iniziale - 1, cols = col + col_iniziale - 1, stack = TRUE)
      } else {
        addStyle(workbook, sheet = sheetname, style = odd_row_style, rows = row + row_iniziale - 1, cols = col + col_iniziale - 1, stack = TRUE)
      }
    }
    setColWidths(workbook, sheet = sheetname, cols = col + col_iniziale - 1, widths = "auto")
  }
}


###------------------------------------------###
#---   create table with totals -----
###------------------------------------------###
create_table_with_totals <- function(workbook, sheetname, table, titolo, col_iniziale, row_iniziale) {
  nrows <- nrow(table)
  ncols <- ncol(table)
  
  writeData(workbook, sheetname, x = titolo, startCol = col_iniziale, startRow = row_iniziale -1)
  addStyle(workbook, sheet = sheetname, style = titleStyle, rows = row_iniziale -1, cols = col_iniziale)
  
  writeDataTable(workbook, sheet = sheetname, x = table, startCol = col_iniziale, startRow = row_iniziale)
  
  for (col in 1:ncols) {
    addStyle(workbook, sheet = sheetname, style = header_style, rows = row_iniziale, cols = col + col_iniziale - 1)
    addStyle(workbook, sheet = sheetname, style = total_style, rows = 1 + row_iniziale, cols = col + col_iniziale - 1, stack = TRUE)
    for (row in 2:nrows) {
      addStyle(workbook, sheet = sheetname, style = row_style, rows = row + row_iniziale, cols = col + col_iniziale - 1, stack = TRUE)
    }
    setColWidths(workbook, sheet = sheetname, cols = col + col_iniziale - 1, widths = "auto")
  }
}






wb <- createWorkbook()
sheet <- addWorksheet(wb, sheetName = "Report_Loans", gridLines = FALSE)
sheetname <- "Report_Loans"
writeData(wb,"Report_Loans",'Loans Data Report',1,1)
addStyle(wb,sheet = "Report_Loans",style = ReportTitleStyle,1,1)

resume <- 'SUMMARY: \n The portfolio Vasari involves 123 borrowers for a total of  +25M. \n In the first sheet we have the analysis on Loan level.\n In the second sheet we have the analysis on Borrower level.\n In the third sheet we have the analysis on Infoproviding Level, both Persone Fisiche and Persone Giuridiche. \n In the fourth sheet we have the analysis on Agreement level.'
lines <- unlist(strsplit(resume, "\n"))
df <- data.frame(Text = lines)
for(i in 1:nrow(df)){
  writeData(wb,"Report_Loans",df$Text[i],1,i+1)
}



#apply the functions as follows:   workbook, "sheetname", dataframe, "table title", ncol, nrow
#create_table_k_for_miles(wb, "Report", r.introductionP6, "Summary", 3, 5)

create_table_no_k(wb, "Report_Loans", r.introductionP6, "Summary", 3, 10) #3 rows
create_table_with_totals(wb, "Report_Loans", r.type.gbv, "Loans by Type", 3, 15) #5 rows
create_table_with_totals(wb, "Report_Loans", r.status.gbv, "Loans by Status", 3, 23) #4 rows
create_table_with_totals(wb, "Report_Loans", r.p31.gbvByGuaranteePresence, "Guarantee Status", 3, 30) #5 rows
create_table_with_totals(wb, "Report_Loans", r.p31.gbvByGuaranteedType, "Guarantee Type", 7, 30) #5 rows



ggsave("Charts/gbv_residual_%_loan_size.png",plot = r.p28.g.gbvByLoanSize)
insertImage(wb,sheet = "Report_Loans","Charts/gbv_residual_%_loan_size.png",startCol = 13, startRow = 8, width = 6, height = 4.5, dpi = 300)
ggsave("Charts/histogram_loans_amount.png",plot = gg)
insertImage(wb,sheet = "Report_Loans","Charts/histogram_loans_amount.png",startCol = 13, startRow = 30, width = 6, height = 4.5, dpi = 300)
ggsave("Charts/line.plot.status.change.png",plot = chart.date.status.cases)
insertImage(wb,sheet = "Report_Loans","Charts/line.plot.status.change.png",startCol = 13, startRow = 45, width = 6, height = 4.5, dpi = 300)







sheet <- addWorksheet(wb, sheetName = "Report_Borrowers", gridLines = FALSE)
sheetname <- "Report_Borrowers"
writeData(wb,"Report_Borrowers",'Borrowers Data Report',1,1)
addStyle(wb,sheet = "Report_Borrowers",style = ReportTitleStyle,1,1)

#apply the functions as follows:   workbook, "sheetname", dataframe, "table title", ncol, nrow
#create_table_k_for_miles(wb, "Report", r.introductionP6, "Summary", 3, 5)
create_table_with_totals(wb, "Report_Borrowers", r.type.subject.n.cases, "Borrowers by Type", 3, 5) #5 rows
create_table_with_totals(wb, "Report_Borrowers", r.individuals.range.age, "Borrowers by Age Range", 7, 5) #8 rows
create_table_with_totals(wb, "Report_Borrowers", r.borrower.n.entities, "Borrowers by N Entities", 3, 14) #6 rows
create_table_with_totals(wb, "Report_Borrowers", r.n.borrowers.n.loans, "Borrowers by N Loans", 3, 23) #7 rows


ggsave("Charts/borrowers_%_per_area.png",plot = r.p27.borrowerByArea)
insertImage(wb,sheet = "Report_Borrowers","Charts/borrowers_%_per_area.png",startCol = 12, startRow = 10, width = 6, height = 4.5, dpi = 300)
ggsave("Charts/gbv_%_per_area.png",plot = r.p27.gbvByArea)
insertImage(wb,sheet = "Report_Borrowers","Charts/gbv_%_per_area.png",startCol = 12, startRow = 32, width = 6, height = 4.5, dpi = 300)




sheet <- addWorksheet(wb, sheetName = "Report_Infoproviding", gridLines = FALSE)
sheetname <- "Report_Infoproviding"
writeData(wb,"Report_Infoproviding",'Infoproviding Data Report',1,1)
addStyle(wb,sheet = "Report_Infoproviding",style = ReportTitleStyle,1,1)

#apply the functions as follows:   workbook, "sheetname", dataframe, "table title", ncol, nrow
#create_table_k_for_miles(wb, "Report", r.introductionP6, "Summary", 3, 5)
create_table_with_totals(wb, "Report_Infoproviding", table.info.income.solvency, "Net Income per Solvency", 3, 5) #6 rows
create_table_with_totals(wb, "Report_Infoproviding", table.info.income.region, "Net Income per Region", 8, 5) #7 rows
create_table_no_k(wb, "Report_Infoproviding", table.info.piva.type, "% Status Cases per Corporate Type", 3, 18) #8 rows
#create_table_no_k(wb, "Report_Infoproviding", table.info.piva.status.region, "N Status Cases per Region", 3, 71) #10 rows


ggsave("Charts/solvency.class.cases.png",plot = chart.solvency.class.cases)
insertImage(wb,sheet = "Report_Infoproviding","Charts/solvency.class.cases.png",startCol = 13, startRow = 10, width = 6, height = 4.5, dpi = 300)




sheet <- addWorksheet(wb, sheetName = "Report_Agreement", gridLines = FALSE)
sheetname <- "Report_Agreement"
writeData(wb,"Report_Agreement",'Agreement Data Report',1,1)
addStyle(wb,sheet = "Report_Agreement",style = ReportTitleStyle,1,1)

#apply the functions as follows:   workbook, "sheetname", dataframe, "table title", ncol, nrow
#create_table_k_for_miles(wb, "Report", r.introductionP6, "Summary", 3, 5)
create_table_with_totals(wb, "Report_Agreement", agreement.summary.status, "Agreement Status Payments", 3, 5) #6 rows
create_table_with_totals(wb, "Report_Agreement", agreement.summary.n.cases.status, "N agreements and Mean Instalments per Status", 3, 15) #7 rows
create_table_with_totals(wb, "Report_Agreement", agree.count.ind.corp.age, "Agreement Type", 3, 25) #7 rows

ggsave("Charts/chart.date.amount.paid.png",plot = chart.date.amount.paid)
insertImage(wb,sheet = "Report_Agreement","Charts/chart.date.amount.paid.png",startCol = 13, startRow = 10, width = 6, height = 4.5, dpi = 300)
ggsave("Charts/p.png",plot = p)
insertImage(wb,sheet = "Report_Agreement","Charts/p.png",startCol = 13, startRow = 10, width = 6, height = 4.5, dpi = 300)




sheet <- addWorksheet(wb, sheetName = "Report_Guarantees", gridLines = FALSE)
sheetname <- "Report_Guarantees"
writeData(wb,"Report_Guarantees",'Guarantees Data Report',1,1)
addStyle(wb,sheet = "Report_Guarantees",style = ReportTitleStyle,1,1)
resume <- 'SUMMARY: \n Guarantees data report by type, origin lien, and status'
lines <- unlist(strsplit(resume, "\n"))
df <- data.frame(Text = lines)
for(i in 1:nrow(df)){
  writeData(wb,"Report_Guarantees",df$Text[i],1,i+1)
}

create_table_with_totals(wb, "Report_Guarantees", guar.type.table, "Guarantees by Type", 3, 6) #6 rows
create_table_with_totals(wb, "Report_Guarantees", guar.origin.lien.table, "Guarantees by Origin Lien", 3, 15) #7 rows
create_table_with_totals(wb, "Report_Guarantees", status.guar, "Guarantees by Status", 3, 25) #7 rows
resume <- 'SUMMARY: \n Guarantees data report by type, origin lien, and status'

saveWorkbook(wb, "Report.xlsx", overwrite = TRUE)
