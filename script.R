library(ggplot2)
library(acs)
library(maps)
library(maptools)

#Set up Census API key
key = "ba67d3a427e1f785987b9c8bc59341bf7c8a7cc1"
api.key.install(key)

#Create tracts for the state
ct.tracts = geo.make(state = "CT", county = "*", tract = "*", check = T)

#Percent of foreign-born population from Caribbean
B05006 = acs.fetch(geography = ct.tracts, table.number = "B05006", col.names = "pretty")

B05006.tract = data.frame(geo=geography(B05006)[[1]],
                          caribbean = as.numeric(estimate(B05006[,123])),
                          dr = as.numeric(estimate(B05006[,129])),
                          haiti = as.numeric(estimate(B05006[,131])),
                          jamaica = as.numeric(estimate(B05006[,132])))

B05006.tract$geo= gsub("Census Tract ", "", B05006.tract$geo)
B05006.tract$geo= gsub(", (Fairfield|Hartford|Litchfield|Middlesex|New Haven|New London|Tolland|Windham) County, Connecticut","", B05006.tract$geo)

#Load the UConn tract and town-level shapefiles
CTTracts <- readShapeSpatial(fn="../regionalreport/tractct_37800_0000_2010_s100_census_1_shp/wgs84/tractct_37800_0000_2010_s100_census_1_shp_wgs84")
CTTracts <- fortify(CTTracts, region = "NAME10")
CTTracts <- CTTracts[order(CTTracts$order),]
#Merge with data
library(classInt)
jenks <- classIntervals(B05006.tract$jamaica, 
                        n=3, style="fisher")
choropleth=merge(CTTracts, B05006.tract, by.x = "id", by.y="geo")
choropleth=choropleth[order(choropleth$order), ]
choropleth$jamaica=cut(choropleth$jamaica, 
                     breaks=jenks$brks,
                     include.lowest=T, dig.lab = 4)
#Make the map
ggplot(data = choropleth, aes(long, lat, group = group)) +
  geom_polygon(aes(fill = jamaica)) + 
  scale_x_continuous(breaks = NULL) +
  scale_y_continuous(breaks = NULL) +
  labs(x = NULL, y = NULL) + 
  coord_equal() +
  geom_polygon(data = CTTracts, colour = "grey", alpha = 0.1, size = 0.1, fill = NA) +
  scale_fill_brewer(palette = "Purples", name = "Born in\nJamaica") +
  theme_minimal() 
