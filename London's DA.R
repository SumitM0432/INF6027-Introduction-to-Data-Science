source("0_libraries.R", local = TRUE)

geo_data = st_read("/Users/sumitmishra/Downloads/Geospatial test/spatial_signatures_GB_clean.gpkg")
geo_data = as.data.frame(geo_data)
geo_data = data.table(geo_data)

test = fread("/Users/sumitmishra/Downloads/Geospatial test/per_geometry.csv")

test_air = fread("/Users/sumitmishra/Downloads/2. Emissions/Area/Gas/Excel/LAEI2010_GridSplits_Gas_Areas_2008.csv")

test_road = fread("/Users/sumitmishra/Downloads/2. Emissions/Mobile/Road Transport/Excel/Excel - Evaporatives.csv")

test_benzene_08 = fread("/Users/sumitmishra/Downloads/2. Emissions/Mobile/Road Transport/Excel/MajorRoads/Benzene_2008.csv")

test_benzene_10 = fread("/Users/sumitmishra/Downloads/2. Emissions/Mobile/Road Transport/Excel/MajorRoads/Benzene_2010.csv")

