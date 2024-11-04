source("0_libraries.R", local = TRUE)

test_benzene_08 = fread("Data/London Atmospheric Emissions Inventory/2. Emissions/Mobile/Road Transport/Excel/MajorRoads/Benzene_2008.csv")
test_benzene_08$V1 = NULL
test_benzene_08 = test_benzene_08 %>%
  distinct()

for (col_name in names(test_benzene_08)) {
  if (class(test_benzene_08[[col_name]]) == 'character'){
    print (paste0(col_name, " :: ", n_distinct(test_benzene_08[[col_name]])))
  } else{
    print (paste0(col_name, " :: ", n_distinct(test_benzene_08[[col_name]])))
  }
}
test_benzene_10 = fread("Data/London Atmospheric Emissions Inventory/2. Emissions/Mobile/Road Transport/Excel/MajorRoads/Benzene_2010.csv")

