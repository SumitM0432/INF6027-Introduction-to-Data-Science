# Libraries
source("0_libraries.R", local = TRUE)
source("0_functions.R", local = TRUE)

# Convert the Scientific notation to Decimal
options(scipen = 999)

# Garbage Collection
gc()

road_transport_column_desc <- read_excel("Data/London Atmospheric Emissions Inventory/2. Emissions/Mobile/Road Transport/README - road transport.xls", 
                                         sheet = "major", skip = 4) %>%
                                         select(-c('...3'))
  
# Ingesting the Benzene Pollutant for Analysis
test_benzene_08 = fread("Data/London Atmospheric Emissions Inventory/2. Emissions/Mobile/Road Transport/Excel/MajorRoads/Benzene_2008.csv")
test_benzene_10 = fread("Data/London Atmospheric Emissions Inventory/2. Emissions/Mobile/Road Transport/Excel/MajorRoads/Benzene_2010.csv")
test_benzene_12 = fread("Data/London Atmospheric Emissions Inventory/2. Emissions/Mobile/Road Transport/Excel/MajorRoads/Benzene_2012.csv")
test_benzene_15 = fread("Data/London Atmospheric Emissions Inventory/2. Emissions/Mobile/Road Transport/Excel/MajorRoads/Benzene_2015.csv")
test_benzene_20 = fread("Data/London Atmospheric Emissions Inventory/2. Emissions/Mobile/Road Transport/Excel/MajorRoads/Benzene_2020.csv")

test_no_08 = fread("Data/London Atmospheric Emissions Inventory/2. Emissions/Mobile/Road Transport/Excel/MajorRoads/no2_2008.csv")

test_no_08 = test_no_08 %>%
  mutate(emission_sum = rowSums(across(c('Motorcycle', 'Taxi', 'Car', 'PetrolCar', 'DieselCar', 'Lgv', 'PetrolLgv', 'DieselLgv', 'BusAndCoach', 'LTBus', 'Coach'))))

# test on 2008 data
test_benzene_08$V1 =  NULL

print (paste0("08 :: ", nrow(test_benzene_08), ",", ncol(test_benzene_08),
              " - 10 :: ", nrow(test_benzene_10), ",", ncol(test_benzene_10),
              " - 12 :: ", nrow(test_benzene_12), ",", ncol(test_benzene_12),
              " - 15 :: ", nrow(test_benzene_15), ",", ncol(test_benzene_15),
              " - 20 :: ", nrow(test_benzene_20), ",", ncol(test_benzene_20)))

for (col_name in names(test_benzene_08)) {
  if (class(test_benzene_08[[col_name]]) == 'character'){
    print (paste0(col_name, " :: ", n_distinct(test_benzene_08[[col_name]])))
  } else{
    print (paste0(col_name, " :: ", n_distinct(test_benzene_08[[col_name]])))
  }
}

# Preserving the BNG coordinates for further use
test_benzene_08 = test_benzene_08 %>%
  mutate(
    X_COORD_BNG = X_COORD,
    Y_COORD_BNG = Y_COORD
  ) %>%
  distinct()

# Changing the coordinates
test_benzene_08 <- test_benzene_08 %>%
  # Convert to spatial object in to Geometric Points
  st_as_sf(coords = c("X_COORD", "Y_COORD"), crs = 27700) %>%
  # Transform the BNG to WGS84 for compatibility with most mapping tools
  st_transform(crs = 4326)

temp = test_benzene_08 %>%
  group_by(GRID_ID, Year, Pollutant) %>%
  summarise(
    taxi_sum = sum(Taxi)
  )

# Visualization
plot(st_geometry(test_benzene_08$geometry))

test_benzene_08 %>%
  ggplot(aes(x = X_COORD_BNG, y = Y_COORD_BNG)) +
  geom_point(color = "steelblue", alpha = 0.5) +
  labs(title = "Spatial Distribution of Emissions Data",
       x = "X Coordinate (British National Grid)",
       y = "Y Coordinate (British National Grid)") +
  theme_minimal()
