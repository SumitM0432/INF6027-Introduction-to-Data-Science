print_description = function(df) {
  print (paste0("No. of Rows :: ", nrow(df)))
  print (paste0("No. of Columns :: ", ncol(df)))
  
  # Column Names
  ls = colnames(df)
  
  return (ls)
}