library(leaflet)

popup = c("Robin", "Jakub", "Jannes")
leaflet() |>
  addProviderTiles("NASAGIBS.ViirsEarthAtNight2012") |>
  addMarkers(lng = c(-3, 23, 11),
             lat = c(52, 53, 49), 
             popup = popup)

# install.packages("sf")
# install.packages("terra")
# install.packages("spData")
# install.packages("spDataLarge", repos = "https://nowosad.r-universe.dev")

library(sf)           # classes and functions for vector data
library(terra)        # classes and functions for raster data
library(spData)       # load geographic data
library(spDataLarge)  # load larger geographic data

vignette(package = "sf") # see which vignettes are available
vignette("sf1")          # an introduction to the package

world |> head()
class(world)
# [1] "sf"         "tbl_df"     "tbl"        "data.frame"
names(world)
# [1] "iso_a2"    "name_long" "continent" "region_un" "subregion" "type"
# [7] "area_km2"  "pop"       "lifeExp"   "gdpPercap" "geom"
world$geom
# Geometry set for 177 features 
# Geometry type: MULTIPOLYGON
# Dimension:     XY
# Bounding box:  xmin: -180 ymin: -89.9 xmax: 180 ymax: 83.64513
# Geodetic CRS:  WGS 84
# First 5 geometries:
# MULTIPOLYGON (((-180 -16.55522, -179.9174 -16.5...
# MULTIPOLYGON (((33.90371 -0.95, 31.86617 -1.027...
# MULTIPOLYGON (((-8.66559 27.65643, -8.817828 27...
# MULTIPOLYGON (((-132.71 54.04001, -133.18 54.16...
# MULTIPOLYGON (((-171.7317 63.78252, -171.7911 6...
plot(world)
summary(world["lifeExp"])
# lifeExp                 geom    
# Min.   :50.62   MULTIPOLYGON :177  
# 1st Qu.:64.96   epsg:4326    :  0  
# Median :72.87   +proj=long...:  0  
# Mean   :70.85                      
# 3rd Qu.:76.78                      
# Max.   :83.59                      
# NA's   :10

world_mini = world[1:2, 1:3]
world_mini

## Porownanie dwoch metod odczytu
# st_read() - odczyt w podstawowywm data frame-ie
# read_sf() - odczyt wspierajacy tidyverse (format tibble) <- preferowany
world_dfr = st_read(system.file("shapes/world.shp", package = "spData"))
class(world_dfr)
world_tbl = read_sf(system.file("shapes/world.shp", package = "spData"))
class(world_tbl)
# sf is now the go-to package for analysis of spatial vector data in R

# Mozliwosc konwersji plikow typu sf "w" i "z" plikow typu sp
library(sp)
world_sp = as(world, "Spatial")         # from an sf object to sp
# sp functions ...
world_sf = st_as_sf(world_sp)           # from sp to sf

## Podstawowe rysowanie map
# Domyslnie tworzone jest wiele wykresow, jeden podwykres dla każdej zmiennej
plot(world[3:6])
# Wykres o ciągłym kolorze jest tworzony, jeśli obiekt do narysowania ma jedną zmienną
plot(world["pop"])

# Polaczenie wszystkich krajow Azji w jeden single feature (w kontynent)
world_asia = world[world$continent == "Asia", ]
asia = st_union(world_asia)

# If the first plot has a key, reset = FALSE must be used (?)
plot(world["pop"], reset = FALSE)
plot(asia, add = TRUE, col = "red") # "add" - naklada na juz wywolana mape, dany obiekt/warstwe

# Mapa swiata z nalozonymi kolami, których średnice (ustawione za pomocą cex =) reprezentują populacje państw
cex = sqrt(world$pop) / 10000
# wykorzystanie funkcji st_centroid() do konwersji jednego typu geometrii (wielokąty)
# na inny (punkty), którego estetykę zmieniamy za pomocą argumentu cex.
world_cents = st_centroid(world, of_largest = TRUE)
plot(world["continent"], reset = FALSE)
plot(st_geometry(world_cents), add = TRUE, cex = cex)

# Uzycie expandBB pokazujace jak mozna rysowac okreslone obszary wzgledem zadanego punktu
india = world[world$name_long == "India",]
plot(st_geometry(india), expandBB = c(0, 0.2, 0.1, 1), col = "gray", lwd = 3) #lwd - linewidth
plot(st_geometry(world_asia), add = TRUE)

# Utworzenie klasy SF od podstaw
lnd_point = st_point(c(0.1, 51.5))                 # sfg object - simple feature geometry
lnd_geom = st_sfc(lnd_point, crs = 4326)           # sfc object - simple feature geometry column
lnd_attrib = data.frame(                           # data.frame object
  name = "London",
  temperature = 25,
  date = as.Date("2017-06-21")
)
lnd_sf = st_sf(lnd_attrib, geometry = lnd_geom)    # sf object
lnd_sf
class(lnd_sf)
# [1] "sf"         "data.frame"
# Simple features are simply data frames (square tables), but with
# spatial attributes stored in a list column, usually called geometry

## SFG - simple feature geometry
# The sfg class represents the different simple feature geometry types in R: point,linestring,
# polygon (and their ‘multi’ equivalents, such as multipoints) or geometry collection. 
st_point(c(5, 2))                 # XY point
#> POINT (5 2)
st_point(c(5, 2, 3))              # XYZ point
#> POINT Z (5 2 3)
st_point(c(5, 2, 1), dim = "XYM") # XYM point
#> POINT M (5 2 1)
st_point(c(5, 2, 3, 1))           # XYZM point
#> POINT ZM (5 2 3 1)
# M - additional variable, typically measurement accuracy

# the rbind function simplifies the creation of matrices
## MULTIPOINT
multipoint_matrix = rbind(c(5, 2), c(1, 3), c(3, 4), c(3, 2))
# [,1] [,2]
# [1,]    5    2
# [2,]    1    3
# [3,]    3    4
# [4,]    3    2
st_multipoint(multipoint_matrix)
#> MULTIPOINT ((5 2), (1 3), (3 4), (3 2))
## LINESTRING
linestring_matrix = rbind(c(1, 5), c(4, 4), c(4, 1), c(2, 2), c(3, 2))
# [,1] [,2]
# [1,]    1    5
# [2,]    4    4
# [3,]    4    1
# [4,]    2    2
# [5,]    3    2
st_linestring(linestring_matrix)
#> LINESTRING (1 5, 4 4, 4 1, 2 2, 3 2)

## POLYGON
polygon_list = list(rbind(c(1, 5), c(2, 2), c(4, 1), c(4, 4), c(1, 5)))
# [[1]]
# [,1] [,2]
# [1,]    1    5
# [2,]    2    2
# [3,]    4    1
# [4,]    4    4
# [5,]    1    5
st_polygon(polygon_list)
#> POLYGON ((1 5, 2 2, 4 1, 4 4, 1 5))

## POLYGON with a hole
polygon_border = rbind(c(1, 5), c(2, 2), c(4, 1), c(4, 4), c(1, 5))
polygon_hole = rbind(c(2, 4), c(3, 4), c(3, 3), c(2, 3), c(2, 4))
polygon_with_hole_list = list(polygon_border, polygon_hole)
st_polygon(polygon_with_hole_list)
#> POLYGON ((1 5, 2 2, 4 1, 4 4, 1 5), (2 4, 3 4, 3 3, 2 3, 2 4))

## MULTILINESTRING
multilinestring_list = list(rbind(c(1, 5), c(4, 4), c(4, 1), c(2, 2), c(3, 2)), 
                            rbind(c(1, 2), c(2, 4)))
st_multilinestring((multilinestring_list))
#> MULTILINESTRING ((1 5, 4 4, 4 1, 2 2, 3 2), (1 2, 2 4))

## MULTIPOLYGON
multipolygon_list = list(list(rbind(c(1, 5), c(2, 2), c(4, 1), c(4, 4), c(1, 5))),
                         list(rbind(c(0, 2), c(1, 2), c(1, 3), c(0, 3), c(0, 2))))
st_multipolygon(multipolygon_list)
#> MULTIPOLYGON (((1 5, 2 2, 4 1, 4 4, 1 5)), ((0 2, 1 2, 1 3, 0 3, 0 2)))

## GEOMETRYCOLLECTION
geometrycollection_list = list(st_multipoint(multipoint_matrix),
                               st_linestring(linestring_matrix))
st_geometrycollection(geometrycollection_list)
#> GEOMETRYCOLLECTION (MULTIPOINT (5 2, 1 3, 3 4, 3 2),
#>   LINESTRING (1 5, 4 4, 4 1, 2 2, 3 2))

## SFC - simple feature geometry column
# One sfg object contains only a single simple feature geometry.
# A simple feature geometry column is a list of sfg objects, which is additionally
# able to contain information about the coordinate reference system in use
# (sfc represents the geometry column in sf data frames).
# sfc POINT
point1 = st_point(c(5, 2))
point2 = st_point(c(1, 3))
points_sfc = st_sfc(point1, point2)
points_sfc
#> Geometry set for 2 features 
#> Geometry type: POINT
#> Dimension:     XY
#> Bounding box:  xmin: 1 ymin: 2 xmax: 5 ymax: 3
#> CRS:           NA
#> POINT (5 2)
#> POINT (1 3)

# In most cases, an sfc object contains objects of the same geometry type.
# sfc POLYGON
polygon_list1 = list(rbind(c(1, 5), c(2, 2), c(4, 1), c(4, 4), c(1, 5)))
polygon1 = st_polygon(polygon_list1)
polygon_list2 = list(rbind(c(0, 2), c(1, 2), c(1, 3), c(0, 3), c(0, 2)))
polygon2 = st_polygon(polygon_list2)
polygon_sfc = st_sfc(polygon1, polygon2)
st_geometry_type(polygon_sfc)
#> [1] POLYGON POLYGON
#> 18 Levels: GEOMETRY POINT LINESTRING POLYGON MULTIPOINT ... TRIANGLE

# sfc MULTILINESTRING
multilinestring_list1 = list(rbind(c(1, 5), c(4, 4), c(4, 1), c(2, 2), c(3, 2)), 
                             rbind(c(1, 2), c(2, 4)))
multilinestring1 = st_multilinestring((multilinestring_list1))
multilinestring_list2 = list(rbind(c(2, 9), c(7, 9), c(5, 6), c(4, 7), c(2, 7)), 
                             rbind(c(1, 7), c(3, 8)))
multilinestring2 = st_multilinestring((multilinestring_list2))
multilinestring_sfc = st_sfc(multilinestring1, multilinestring2)
st_geometry_type(multilinestring_sfc)
#> [1] MULTILINESTRING MULTILINESTRING
#> 18 Levels: GEOMETRY POINT LINESTRING POLYGON MULTIPOINT ... TRIANGLE

# It is also possible to create an sfc object from sfg objects with different geometry types:
# sfc GEOMETRY
point_multilinestring_sfc = st_sfc(point1, multilinestring1)
st_geometry_type(point_multilinestring_sfc)
#> [1] POINT           MULTILINESTRING
#> 18 Levels: GEOMETRY POINT LINESTRING POLYGON MULTIPOINT ... TRIANGLE

st_crs(points_sfc)
#> Coordinate Reference System: NA
# All geometries in sfc objects must have the same CRS!

# Set the CRS with an identifier referring to an 'EPSG' CRS code:
points_sfc_wgs = st_sfc(point1, point2, crs = "EPSG:4326")
st_crs(points_sfc_wgs) # print CRS (only first 4 lines of output shown)
#> Coordinate Reference System:
#>   User input: EPSG:4326 
#>   wkt:
#> GEOGCRS["WGS 84",
#> ...

## The sfheaders package - an R package that speeds-up the construction, conversion and manipulation of sf objects
v = c(1, 1)
v_sfg_sfh = sfheaders::sfg_point(obj = v)
# the sfg object v_sfg_sfh is printed when sf is not loaded
v_sfg_sfh # printing without sf loaded
#>      [,1] [,2]
#> [1,]    1    1
#> attr(,"class")
#> [1] "XY"    "POINT" "sfg" 

# when sf is loaded
v_sfg_sf = st_point(v)
print(v_sfg_sf) == print(v_sfg_sfh)
#> POINT (1 1)
#> POINT (1 1)
#> [1] TRUE

# SFG
# matrices
m = matrix(1:8, ncol = 2)
sfheaders::sfg_linestring(obj = m)
#> LINESTRING (1 5, 2 6, 3 7, 4 8)
# data.frames
df = data.frame(x = 1:4, y = 4:1)
sfheaders::sfg_polygon(obj = df)
#> POLYGON ((1 4, 2 3, 3 2, 4 1, 1 4))

# SFC
sfheaders::sfc_point(obj = v)
sfheaders::sfc_linestring(obj = m)
sfheaders::sfc_polygon(obj = df)

# SF
sfheaders::sf_point(obj = v)
sfheaders::sf_linestring(obj = m)
sfheaders::sf_polygon(obj = df)

# Dont forget about CRS!
df_sf = sfheaders::sf_polygon(obj = df)
st_crs(df_sf) = "EPSG:4326"
df_sf

## S2 - paczka od Google, która obsługuje operacje związane z geometrią sferyczną

# sf can run in two modes with respect to S2: on and off
sf_use_s2()
#> [1] TRUE

# An example of the consequences of turning the geometry engine off is shown below,
# by creating buffers around the india
india_buffer_with_s2 = st_buffer(india[,1], 1)
plot(india_buffer_with_s2)
sf_use_s2(FALSE)
#> Spherical geometry (s2) switched off
india_buffer_without_s2 = st_buffer(india[,1], 1)
plot(india_buffer_without_s2)
#> Warning in st_buffer.sfc(st_geometry(x), dist, nQuadSegs, endCapStyle =
#> endCapStyle, : st_buffer does not correctly buffer longitude/latitude data
#> dist is assumed to be in decimal degrees (arc_degrees).
sf_use_s2(TRUE)
#> Spherical geometry (s2) switched on

# If you see error message such as #> Error in s2_geography_from_wkb ... it may be worth
# trying the command that generated the error message again, after turning off S2.

## Raster data
# The spatial raster data model represents the world with the continuous grid of cells
# (often also called pixels). This data model often refers to so-called regular grids,
# in which each cell has the same, constant size.

# Terra - a package that supports raster objects in R
# The SpatRaster class represents rasters object in terra.
raster_filepath = system.file("raster/srtm.tif", package = "spDataLarge")
my_rast = rast(raster_filepath)
my_rastclass(my_rast)
#> [1] "SpatRaster"
#> attr(,"package")
#> [1] "terra"
my_rast # raster header - dimensions, resolution, extent, CRS, class, data source, summary of the raster values
#> class       : SpatRaster 
#> dimensions  : 457, 465, 1  (nrow, ncol, nlyr)
#> resolution  : 0.000833, 0.000833  (x, y)
#> extent      : -113, -113, 37.1, 37.5  (xmin, xmax, ymin, ymax)
#> coord. ref. : lon/lat WGS 84 (EPSG:4326) 
#> source      : srtm.tif 
#> name        : srtm 
#> min value   : 1024 
#> max value   : 2892

my_rast |> dim()      # the number of rows, columns and layers
my_rast |> ncell()    # the number of cells
my_rast |> res()      # the spatial resolution
my_rast |> ext()      # its spatial extent
my_rast |> crs()      # its coordinate reference system
my_rast |> inMemory() # reports whether the raster data is stored in memory or on disk

plot(my_rast)

# read-in a raster file from disk or from a server
single_raster_file = system.file("raster/srtm.tif", package = "spDataLarge")
single_rast = rast(raster_filepath)

# Rasters can also be created from scratch using the same rast() function. 
new_raster = rast(nrows = 6, ncols = 6, resolution = 0.5, 
                  xmin = -1.5, xmax = 1.5, ymin = -1.5, ymax = 1.5,
                  vals = 1:36)
# The resulting raster consists of 36 cells (6 columns and 6 rows specified by nrows and ncols)
# centered around the Prime Meridian and the Equator (see xmin, xmax, ymin and ymax parameters). 

# The SpatRaster class also handles multiple layers, which typically correspond to
# a single multispectral satellite file or a time-series of rasters.
multi_raster_file = system.file("raster/landsat.tif", package = "spDataLarge")
multi_rast = rast(multi_raster_file)
multi_rast
#> class       : SpatRaster 
#> dimensions  : 1428, 1128, 4  (nrow, ncol, nlyr)
#> resolution  : 30, 30  (x, y)
#> extent      : 301905, 335745, 4111245, 4154085  (xmin, xmax, ymin, ymax)
#> coord. ref. : WGS 84 / UTM zone 12N (EPSG:32612) 
#> source      : landsat.tif 
#> names       : landsat_1, landsat_2, landsat_3, landsat_4 
#> min values  :      7550,      6404,      5678,      5252 
#> max values  :     19071,     22051,     25780,     31961

nlyr(multi_rast) # retrieves the number of layers stored in a SpatRaster object
#> [1] 4

# For multi-layer raster objects, layers can be selected with terra::subset().
multi_rast3 = subset(multi_rast, 3)
multi_rast4 = subset(multi_rast, "landsat_4")

# combining several SpatRaster objects into one
multi_rast34 = c(multi_rast3, multi_rast4)

# Most SpatRaster objects do not store raster values, but rather a pointer to the file itself.
# This has a significant side-effect – they cannot be directly saved to ".rds" or ".rda" files.

# A novel feature of geometry data in sf objects is that they have native support for units.
luxembourg = world[world$name_long == "Luxembourg", ]
st_area(luxembourg) # requires the s2 package in recent versions of sf
#> 2408817306 [m^2]

# Nieprawidlowa konwersja w km^2
st_area(luxembourg) / 1000000
#> 2408.817 [m^2]

# Prawidlowa konwersja w km^2
units::set_units(st_area(luxembourg), km^2)
#> 2408.817 [km^2]

# The my_rast object uses a WGS84 projection with decimal degrees as units.
# Consequently, its resolution is also given in decimal degrees but you have to know it,
# since the res() function simply returns a numeric vector.
res(my_rast)
#> [1] 0.000833 0.000833

# If we used the UTM projection, the units would change.
repr = project(my_rast, "EPSG:26912")
res(repr)
#> [1] 83.5 83.5

# Exercises
summary(world)
# Its geometry type?                        MULTIPOLYGON
# The number of countries?                  177
# Its coordinate reference system (CRS)?    4326

plot(world[world$name_long == "Nigeria",]$geom,
     lwd = 5,
     expandBB = c(0, 0.2, 0.1, 1),
     col = "dark green")
plot(world[world$continent == "Africa",]$geom, add = TRUE)




