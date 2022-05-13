# load libraries

# use tidyverse for utility (includes ggplot2)
library(tidyverse)
# use ggmap to plot maps
library(ggmap)
# use DBI for database connections
library(DBI)

# connect to sqlite database
treeCensus <- dbConnect(RSQLite::SQLite(), "D:/ProjectsSyncDesktop/GIS/NYCtrees/data/NycTreeCensus.sqlite")

# register a google maps API key
register_google(key="jibberish")


# PLOT TREE POPULATION OVER TIME

# get the row count from each table
q <- '
SELECT count( * ) AS count
FROM new_york_tree_census_1995
UNION ALL
SELECT count( * ) AS count
FROM new_york_tree_census_2005_fixed
UNION ALL
SELECT count( * ) AS count
FROM new_york_tree_census_2015;'
treePop <- dbGetQuery(treeCensus, q)
# add years column to table
treePop$year <- c(1995, 2005, 2015)
# show the first few rows
head(treePop)
#   count year
#1 516989 1995
#2 592372 2005
#3 683788 2015

# need to define the font to use
windowsFonts(RobotoCond = windowsFont("Roboto Condensed"))

# define styles for reuse
titleMargin <- margin(t=10,r=10,b=20,l=10)
gridColor <- "#DDDDDD"
axisColor <- "#AAAAAA"
lineColor <- "#666666"

#define a theme
verticalTheme <- theme(text=element_text(family = "RobotoCond",
									   color="#333333"),
					 title=element_text(size=14),
					 plot.title=element_text(margin=titleMargin,
					 						size=20),
					 axis.title.y=element_text(margin=titleMargin),
					 axis.title.x=element_text(margin=titleMargin),
					 axis.text=element_text(size=10),
					 axis.ticks.y=element_blank(),
					 axis.ticks.x=element_line(color=axisColor),
					 axis.ticks.length=unit(5, units="pt"),
					 axis.line.x=element_line(color=axisColor),
					 legend.text=element_text(size=12),
					 plot.background=element_blank(),
					 plot.margin=margin(10,20,0,5),
					 panel.background=element_blank(),
					 panel.grid.major=element_blank(),
					 panel.grid.minor=element_blank(),
					 panel.grid.major.y=element_line(color=gridColor))

# plot the data
ggplot(data=treePop, aes(y=count, x=year)) +
	geom_line(size=1,
			  color=lineColor) +
	geom_point(shape=21,
			   size=3,
			   stroke=1.5,
			   fill="white",
			   color=lineColor) +
	labs(title="Street Tree Population in NYC",
		 x="Year",
		 y="Number of Trees"
		 ) +
	verticalTheme +
	scale_y_continuous(limits=c(0, 800000),
					   expand=c(0,0),
					   labels=scales::comma) +
	scale_x_continuous(expand=c(0,0.5))


# PLOT LIVING TREES

# get the alive status counts
q <- '
SELECT count( * ) AS alive_count
FROM new_york_tree_census_1995
WHERE status != "Dead" AND 
      status != "Planting Space" AND 
      status != "Shaft" AND 
      status != "Stump" AND 
      status != "Unknown"
UNION ALL
SELECT count( * ) AS alive_count
FROM new_york_tree_census_2005_fixed
WHERE status != "Dead"
UNION ALL
SELECT count( * ) AS alive_count
FROM new_york_tree_census_2015
WHERE status != "Dead" AND 
      status != "Stump";'
treesAlive <- dbGetQuery(treeCensus, q)
head(treesAlive)
#  alive_count
#1      471748
#2      584252
#3      652173

# add alive counts to tree table
treePop$alive_count <- treesAlive$alive_count
head(treePop)
#   count year alive_count
#1 516989 1995      471748
#2 592372 2005      584252
#3 683788 2015      652173

# create plot
ggplot(data=treePop, aes(x=year, y=count)) +
	geom_area(aes(fill="Dead")) +
	geom_area(aes(x=year, y=alive_count, fill="Alive")) +
	geom_line(size=1,
			  color=lineColor) +
	geom_point(shape=21,
			   size=3,
			   stroke=1.5,
			   fill="white",
			   color=lineColor) +
	labs(title="Street Tree Population in NYC",
		 x="Year",
		 y="Number of Trees",
		 fill="Status"
	) +
	verticalTheme +
	scale_y_continuous(limits=c(0, 800000),
					   expand=c(0,0),
					   labels=scales::comma) +
	scale_x_continuous(expand=c(0,0.5)) +
	scale_fill_manual(values=c("#8db39f", "#d4cbc0"))


# PLOT TREE SPECIES

# get the unique tree genus and their counts
q <- '
SELECT *
FROM (
         SELECT *
         FROM (
                  SELECT CASE instr(trim(spc_latin), " ") 
                			WHEN 0 THEN lower(trim(spc_latin) ) 
                			ELSE lower(substr(spc_latin, instr(trim(spc_latin), " "), -100) ) 
                			END genus,
                         count( * ) AS count,
                         ( (count( * ) * 1.0) / (
                                                    SELECT count( * ) 
                                                    FROM new_york_tree_census_2015
                                                )
                         ) * 100 AS pct,
                         "2015" AS year
                  FROM new_york_tree_census_2015
                  GROUP BY genus
              )
         UNION
         SELECT *
         FROM (
                  SELECT CASE instr(trim(spc_latin), " ") 
                			WHEN 0 THEN lower(trim(spc_latin) ) 
                			ELSE lower(substr(spc_latin, instr(trim(spc_latin), " "), -100) ) 
                			END genus,
                         count( * ) AS count,
                         ( (count( * ) * 1.0) / (
                                                    SELECT count( * ) 
                                                    FROM new_york_tree_census_2005_fixed
                                                )
                         ) * 100 AS pct,
                         "2005" AS year
                  FROM new_york_tree_census_2005_fixed
                  GROUP BY genus
              )
         UNION
         SELECT *
         FROM (
                  SELECT CASE instr(trim(spc_latin), " ") 
                			WHEN 0 THEN lower(trim(spc_latin) ) 
                			ELSE lower(substr(spc_latin, instr(trim(spc_latin), " "), -100) ) 
                			END genus,
                         count( * ) AS count,
                         ( (count( * ) * 1.0) / (
                                                    SELECT count( * ) 
                                                    FROM new_york_tree_census_1995
                                                )
                         ) * 100 AS pct,
                         "1995" AS year
                  FROM new_york_tree_census_1995
                  GROUP BY genus
              )
     )
     JOIN
     (
         SELECT *,
                count( * ) AS count_total
         FROM (
                  SELECT CASE instr(trim(spc_latin), " ") 
                	WHEN 0 THEN lower(trim(spc_latin) ) 
                	ELSE lower(substr(spc_latin, instr(trim(spc_latin), " "), -100) ) 
                	END genus_total
                  FROM new_york_tree_census_1995
                  UNION ALL
                  SELECT CASE instr(trim(spc_latin), " ") 
                	WHEN 0 THEN lower(trim(spc_latin) ) 
                	ELSE lower(substr(spc_latin, instr(trim(spc_latin), " "), -100) ) 
                	END genus_total
                  FROM new_york_tree_census_2005_fixed
                  UNION ALL
                  SELECT CASE instr(trim(spc_latin), " ") 
                	WHEN 0 THEN lower(trim(spc_latin) ) 
                	ELSE lower(substr(spc_latin, instr(trim(spc_latin), " "), -100) ) 
                	END genus_total
                  FROM new_york_tree_census_2015
              )
         GROUP BY genus_total
     )
     ON genus = genus_total
ORDER BY count_total DESC,
         year;'
treeGenus <- dbGetQuery(treeCensus, q)
head(treeGenus)
#      genus  count      pct year genus_total count_total
# 1     acer 173960 33.64868 1995        acer      399671
# 2     acer 136972 23.12263 2005        acer      399671
# 3     acer  88739 12.97756 2015        acer      399671
# 4 platanus  88055 17.03228 1995    platanus      264598
# 5 platanus  89529 15.11364 2005    platanus      264598
# 6 platanus  87014 12.72529 2015    platanus      264598

# remove NA value from table
treeGenus <- drop_na(treeGenus)

# subset the top 10
tgTop <- head(treeGenus, n=30)

# reverse sort so ggplot displays correctly
#tgTop <- tgTop[order(tgTop$count),]

# set genus as a factor to prevent ggplot from ordering alphabetically
#tgTop$count <- factor(tgTop$count, levels=tgTop$count)

# bar plot of the common genus names
ggplot(data=tgTop) +
	geom_col(mapping=aes(x=genus, y=pct, fill=year), position="dodge", width=0.7) +
	scale_y_continuous(limits=c(0, 35), expand=c(0,0)) +
	scale_x_discrete(labels=str_to_title) +
	labs(title="NYC Street Tree Genus Population",
		 x="Tree Genus",
		 y="Percentage of Tree Population",
		 fill="Year"
	) +
	verticalTheme +
	scale_fill_viridis_d(direction=-1)

# line plot of the common genus names
ggplot(data=tgTop, mapping=aes(x=year, y=pct, group=genus, color=genus)) +
	geom_line() +
	geom_point() +
	scale_y_continuous(limits=c(0, 35), expand=c(0,0)) +
	labs(title="NYC Street Tree Genus Population",
		 x="Year",
		 y="Percentage of Tree Population",
		 color="Genus"
	) +
	verticalTheme


# MAP A SUBSET OF TREES

# trees from Wallabout, NY in 1995
q <- '
SELECT status, longitude, latitude
FROM new_york_tree_census_1995
WHERE zip_new = 11205;'
# trees from Wallabout, NY in 2015
q <- '
SELECT status, longitude, latitude
FROM new_york_tree_census_2015
WHERE zipcode = 11205;'
treeSample <- dbGetQuery(treeCensus, q)
# drop empty values from the table
treeSample <- drop_na(treeSample)
head(treeSample)
# status longitude latitude
# 1  Alive -73.96905 40.69390
# 2  Alive -73.95652 40.69134
# 3  Stump -73.96766 40.69201
# 4  Alive -73.96869 40.68939
# 5  Alive -73.95163 40.68990
# 6  Alive -73.97234 40.69026

# create simple features
#treeSF <- st_as_sf(treeSample, coords=c("longitude", "latitude"), crs=4326)

# get longitude and latitude values
lon <- treeSample$longitude
lat <- treeSample$latitude
# create a bounding box from the lat and lon values
treeBB <- make_bbox(lon, lat)
# calculate the center of the bounding box
# this will not work for every hemisphere due to the nature of latitude and longitude
(treeCenter <- c((treeBB['left'] + treeBB['right'])/2, (treeBB['top'] + treeBB['bottom'])/2))

# use google because it allows larger scales
# request black and white roadmap with no labels
# overlay (darken) the base map with white to improve visibility of tree points
ggmap(get_googlemap(center=treeCenter, 
					maptype="roadmap", 
					zoom=15, 
					color="bw", 
					style="feature:all|element:labels|visibility:off"),
	  darken=c(0.6, "white")) +
	geom_point(data=treeSample, 
			   mapping=aes(x=longitude, y=latitude), 
			   stroke=0, 
			   alpha=0.5, 
			   color="palegreen4") +
	theme_void()

# ggmap map
# get_stamenmap doesn't require an API key and allows a bounding box
# darken fades the map tiles to a color (white)
ggmap(get_stamenmap(bbox=treeBB, maptype="toner-background"), darken=c(0.8, "white")) +
	geom_point(data=treeSample, mapping=aes(x=longitude, y=latitude), stroke=0, alpha=0.3, color="darkolivegreen") +
	theme_void()


# PLOT THE DENSITY OF TREE DIAMETERS

q <- '
SELECT tree_dbh,
       "2015" AS year
FROM new_york_tree_census_2015
WHERE tree_dbh > 0
UNION ALL
SELECT tree_dbh,
       "2005" AS year
FROM new_york_tree_census_2005_fixed
WHERE tree_dbh > 0
UNION ALL
SELECT diameter,
       "1995" AS year
FROM new_york_tree_census_1995
WHERE diameter > 0
ORDER BY tree_dbh DESC;'
treeDiameters <- dbGetQuery(treeCensus, q)
head(treeDiameters)
# tree_dbh year
# 1     2100 2005
# 2     1635 2005
# 3     1605 2005
# 4     1222 2005
# 5      818 2005
# 6      505 2005

summary(treeDiameters)
# tree_dbh           year          
# Min.   :   1.00   Length:1749718    
# 1st Qu.:   5.00   Class :character  
# Median :  10.00   Mode  :character  
# Mean   :  12.13                     
# 3rd Qu.:  17.00                     
# Max.   :2100.00  

densityTheme <- theme(text=element_text(family = "RobotoCond",
										 color="#333333"),
					   title=element_text(size=14),
					   plot.title=element_text(margin=titleMargin,
					   						size=20),
					   axis.title.y=element_blank(),
					   axis.title.x=element_text(margin=titleMargin),
					   axis.text.y=element_blank(),
					   axis.text.x=element_text(size=10),
					   axis.ticks.y=element_blank(),
					   axis.ticks.x=element_line(color=axisColor),
					   axis.ticks.length=unit(5, units="pt"),
					   axis.line.x=element_line(color=axisColor),
					   legend.text=element_text(size=12),
					   plot.background=element_blank(),
					   plot.margin=margin(10,20,0,5),
					   panel.background=element_blank(),
					   panel.grid.major=element_blank(),
					   panel.grid.minor=element_blank(),
					   panel.grid.major.x=element_line(color=gridColor))

# create a violin plot overlayed with a box plot
# densityTheme is a theme definition not required to make this plot
# you can find the theme definition at the bottom of this post
ggplot(data=treeDiameters, aes(x=tree_dbh, y=0)) +
	geom_violin(adjust=1.2, color="darkseagreen", fill="darkseagreen") +
	geom_boxplot(alpha=0.5, width=0.5) +
	labs(title="NYC Street Tree Diameter Distribution",
		 x="Tree Diameter (inches)"
	) +
	scale_x_continuous(limits=c(0, 50), expand=expansion(0, c(0, 0))) +
	densityTheme

# MAP THE OLDEST TREES

q <- '
SELECT trees2015.tree_dbh,
       trees2015.status,
       lower(trees2015.spc_common) AS species_common,
       trees2015.health,
       trees2015.zipcode,
       trees2015.zip_city,
       trees2015.latitude,
       trees2015.longitude,
       CASE instr(trim(trees2015.spc_latin), " ") 
    	WHEN 0 THEN lower(trim(trees2015.spc_latin) ) 
    	ELSE lower(substr(trees2015.spc_latin, instr(trim(trees2015.spc_latin), " "), -100) ) 
    	END genus2015,
       CASE instr(trim(trees1995.spc_latin), " ") 
    	WHEN 0 THEN lower(trim(trees1995.spc_latin) ) 
    	ELSE lower(substr(trees1995.spc_latin, instr(trim(trees1995.spc_latin), " "), -100) ) 
    	END genus1995
FROM new_york_tree_census_2015 AS trees2015
     JOIN
     new_york_tree_census_1995 AS trees1995 ON trees2015.tree_id = trees1995.recordid
WHERE genus2015 = genus1995 AND 
      trees2015.tree_dbh > 5 AND 
      trees2015.status = "Alive";'
oldTrees <- dbGetQuery(treeCensus, q)
head(oldTrees)
#   tree_dbh status   species_common health zipcode      zip_city latitude longitude genus2015 genus1995
# 1       13  Alive     silver maple   Good   10312 Staten Island 40.55740 -74.17193      acer      acer
# 2        7  Alive           ginkgo   Good   11374     Rego Park 40.72244 -73.86302    ginkgo    ginkgo
# 3       17  Alive london planetree   Good   11207      Brooklyn 40.65968 -73.88943  platanus  platanus
# 4       16  Alive          pin oak   Good   11001   Floral Park 40.73641 -73.70546   quercus   quercus
# 5       29  Alive london planetree   Good   10022      New York 40.75725 -73.96023  platanus  platanus
# 6       10  Alive   sycamore maple   Fair   11358      Flushing 40.75578 -73.80656      acer      acer

# get bounding box of the trees
# limit the sample size to 1000 out of 36k
lon <- sample(oldTrees$longitude, size=1000)
lat <- sample(oldTrees$latitude, size=1000)
oldTreeBB <- make_bbox(lon, lat)
(oldTreeCenter <- c((oldTreeBB['left'] + oldTreeBB['right'])/2, (oldTreeBB['top'] + oldTreeBB['bottom'])/2))

# use stamenmap here because it allows a bounding box
ggmap(get_stamenmap(bbox=oldTreeBB, 
					maptype="toner-background"), 
	  darken=c(0.8, "white")) +
	geom_point(data=oldTrees, 
			   mapping=aes(x=longitude, y=latitude), 
			   stroke=0, 
			   alpha=0.1, 
			   color="palegreen4") +
	theme_void()

# google maps forces specific zoom levels, which don't fit this dataset well
ggmap(get_googlemap(center=oldTreeCenter, maptype="roadmap", zoom=10, color="bw", style="feature:all|element:labels|visibility:off"), darken=c(0.6, "white")) +
	geom_point(data=oldTrees, mapping=aes(x=longitude, y=latitude), stroke=0, alpha=0.1, color="darkolivegreen") +
	theme_void()


# FIND ZIP CODE WITH OLDEST TREES

# get the unique zip codes
uniqueZips <- unique(oldTrees$zipcode)
# convert zip codes to factors so they are considered as categories and not integers
oldTrees$zipcode <- factor(oldTrees$zipcode, levels=uniqueZips)
# create a tibble with unique zip codes and their counts
zipCodes <- tibble(zipcode=uniqueZips, count=tabulate(oldTrees$zipcode))
# sort by count
zipCodes <- arrange(zipCodes, desc(count))
# top 6
head(zipCodes)
# A tibble: 6 × 2
#   zipcode count
#   <int>   <int>
# 1 10312    1862
# 2 10314    1681
# 3 10306    1358
# 4 10309    1143
# 5 11230     679
# 6 11385     673

# filter table by zip code
oldestZip <- oldTrees[oldTrees$zipcode==10312,]

# get bounding box of the trees
lon <- oldestZip$longitude
lat <- oldestZip$latitude
oldestZipBB <- make_bbox(lon, lat)
# find the center of the bounding box
(oldestZipCenter <- c((oldestZipBB['left'] + oldestZipBB['right'])/2, (oldestZipBB['top'] + oldestZipBB['bottom'])/2))

# filter table by zip code, exclusive
otherZips <- oldTrees[oldTrees$zipcode!=10312,]

# map the result
ggmap(get_googlemap(center=oldestZipCenter, 
					maptype="roadmap", 
					zoom=13, 
					color="bw", 
					style="feature:all|element:labels|visibility:off"), 
	  darken=c(0.6, "white")) +
	geom_point(data=otherZips, 
			   mapping=aes(x=longitude, y=latitude), 
			   stroke=0, 
			   alpha=0.1, 
			   color="dodgerblue4") +
	geom_point(data=oldestZip, 
			   mapping=aes(x=longitude, y=latitude), 
			   stroke=0, 
			   alpha=0.5, 
			   color="palegreen4") +
	theme_void()

ggmap(get_stamenmap(bbox=oldestZipBB, maptype="toner-background"), darken=c(0.8, "white")) +
	geom_point(data=oldestZip, mapping=aes(x=longitude, y=latitude), stroke=0, alpha=0.5, color="darkolivegreen") +
	theme_void()


# MAP A RANDOM SUBSET OF TREES
# used for the thumbnail

# apporx 70000 trees
q <- '
SELECT health, longitude, latitude
FROM new_york_tree_census_2015
WHERE status = "Alive" AND abs(random() % 100) < 10;'
randomTrees <- dbGetQuery(treeCensus, q)
head(randomTrees)

# get bounding box of the trees
lon <- sample(randomTrees$longitude, size=1000)
lat <- sample(randomTrees$latitude, size=1000)
randomTreesBB <- make_bbox(lon, lat)
(randomTreesCenter <- c((randomTreesBB['left'] + randomTreesBB['right'])/2, (randomTreesBB['top'] + randomTreesBB['bottom'])/2))

# google maps is cleaner
ggmap(get_googlemap(center=randomTreesCenter, 
					maptype="roadmap", 
					zoom=10, 
					color="bw", 
					style="feature:all|element:labels|visibility:off"),
	  darken=c(0.6, "white")) +
	geom_point(data=randomTrees, 
			   mapping=aes(x=longitude, y=latitude, color=health), 
			   stroke=0, 
			   alpha=0.1) +
	theme(text=element_text(family = "RobotoCond",
							color="#333333",
							size=16),
		  legend.position = c(0.3, 0.7),
		  axis.ticks = element_blank(),
		  axis.title = element_blank(),
		  axis.text = element_blank(),
		  legend.key = element_blank(),
		  legend.background = element_rect(fill="white", color="#CCCCCC")) +
	guides(color = guide_legend(override.aes = list(alpha = 1))) +
	scale_color_viridis_d(direction=-1) +
	labs(color="NYC Tree Health")

# DISCONNECT

dbDisconnect(treeCensus)












