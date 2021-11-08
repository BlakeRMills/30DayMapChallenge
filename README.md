# 30DayMapChallenge
Data, Code, and Visualizations for the #30DayMapChallenge


## [Day 1 (Points)](https://twitter.com/BlakeRobMills/status/1455383350613626883)
For this plot, I wanted to show community spaces in Manhattan. Relatively simple for my first plot. All data comes from NYC Open Data, using the [Borough Boundaries](https://data.cityofnewyork.us/City-Government/Borough-Boundaries/tqmj-j8zm) for the outline of Manhattan and the [Pluto Data](https://www1.nyc.gov/site/planning/data-maps/open-data/dwn-pluto-mappluto.page) for the points. 
[Full Code](https://github.com/BlakeRMills/30DayMapChallenge/blob/main/Day%201%20(Points)/Day%201%20(Points).R), [Full Map](https://raw.githubusercontent.com/BlakeRMills/30DayMapChallenge/main/Day%201%20(Points)/Manhattan%20Spaces%20(Day%201%20Points).png)

![PlotDay1](https://raw.githubusercontent.com/BlakeRMills/30DayMapChallenge/main/Day%201%20(Points)/Manhattan%20Spaces%20(Day%201%20Points).png)

## [Day 2 (Lines)](https://twitter.com/BlakeRobMills/status/1455691876091170820)
For this map, I tried something a little more ambitious. I wanted to plot the different bike lanes in global cities. All data comes from each cities open data, but I have attached a copy in my repository. I'm thinking about coming back to this plot and adding more cities. Thoughts?
The following links take you to the source for each city's data: [Barcelona](https://opendata-ajuntament.barcelona.cat/data/en/dataset/carril-bici), [Berlin](https://daten.odis-berlin.de/archive/radverkehrsanlagen/), [Chicago](https://data.cityofchicago.org/Transportation/Bike-Routes/3w5d-sru8), [London](https://data.london.gov.uk/dataset/cycling-infrastructure-database), [Melbourne](https://data.melbourne.vic.gov.au/Transport/Bicycle-routes-including-informal-on-road-and-off-/24aw-nd3i), [Milan](https://dati.comune.milano.it/dataset/ds60_infogeo_piste_ciclabili_localizzazione_), [New York](https://data.cityofnewyork.us/Transportation/Bicycle-Routes/7vsa-caz7), [Sydney](https://data.cityofsydney.nsw.gov.au/datasets/cityofsydney::cycle-network/explore?location=-33.888968%2C151.206972%2C13.33), and [Toronto](https://open.toronto.ca/dataset/bikeways/).

[Full Map](https://ibb.co/yF1mk3g), [Full Code](https://github.com/BlakeRMills/30DayMapChallenge/blob/main/Day%202%20(Lines)/Day%202%20(Lines).R), [Map Download](https://github.com/BlakeRMills/30DayMapChallenge/blob/main/Day%202%20(Lines)/Day%202%20Lines.png), [All Files](https://github.com/BlakeRMills/30DayMapChallenge/tree/main/Day%202%20(Lines)/Data)
![PlotDay2](https://github.com/BlakeRMills/30DayMapChallenge/blob/main/Day%202%20(Lines)/Day%202%20Lines.png)

## [Day 3 (Polygons)](https://twitter.com/BlakeRobMills/status/1456108247568560129)
I've always loved the parks of New York, so I decided to use this day to plot parks of major cities. This map also serves as a sister to the bike lanes map, using the same color scheme. Interesting to see how different cities compare.
The following links take you to the source for each city's data: [Amsterdam](https://maps.amsterdam.nl/stadsparken/?LANG=en), [Chicago](https://data.cityofchicago.org/Parks-Recreation/Parks-Chicago-Park-District-Park-Boundaries-curren/ej32-qgdr), [Los Angeles](https://data.lacity.org/Parks-Recreation/Department-of-Recreation-and-Parks-GIS-Map-of-Park/nuub-r4zx), [New York](https://data.cityofnewyork.us/Recreation/Open-Space-Parks-/g84h-jbjm), [Paris](https://opendata.paris.fr/explore/dataset/espaces_verts/export/?disjunctive.type_ev&disjunctive.categorie&disjunctive.adresse_codepostal&disjunctive.presence_cloture), [Perth](https://geohub-perth.opendata.arcgis.com/datasets/parks-1/explore), [Singapore](https://data.gov.sg/dataset/nparks-parks), and [Toronto](https://open.toronto.ca/dataset/parks/).

[Full Map](https://raw.githubusercontent.com/BlakeRMills/30DayMapChallenge/main/Day%203%20(Polygon)/Park%20Plot.png), [Full Code](https://github.com/BlakeRMills/30DayMapChallenge/blob/main/Day%203%20(Polygon)/Day%203%20(Polygons).R), [Map Download](https://github.com/BlakeRMills/30DayMapChallenge/blob/main/Day%203%20(Polygon)/Park%20Plot.png), [All Files](https://github.com/BlakeRMills/30DayMapChallenge/tree/main/Day%203%20(Polygon)/Data)
![PlotDay3](https://raw.githubusercontent.com/BlakeRMills/30DayMapChallenge/main/Day%203%20(Polygon)/Park%20Plot.png)

## [Day 4 (Hexagons)](https://twitter.com/BlakeRobMills/status/1456439375223472128)
For the theme of Hexgons, I felt I had to do something bee related. Found a beautiful dataset from the USDA regarding bee colonies. I collected data from all years (available on my GitHub), but chose to only use data from 1990, 2000, 2010, and 2020. Map shows how the extimated number of colonies have changed since 1990. 
The following link takes you to the USDA's Bee Survey: [USDA Bee Data](https://usda.library.cornell.edu/concern/publications/hd76s004z?locale=en)

[Full Map](https://raw.githubusercontent.com/BlakeRMills/30DayMapChallenge/main/Day%204%20(Hexagons)/BeePlot.png), [Full Code](https://github.com/BlakeRMills/30DayMapChallenge/blob/main/Day%204%20(Hexagons)/Day%204%20(Hex).R), [Map Download](https://github.com/BlakeRMills/30DayMapChallenge/blob/main/Day%204%20(Hexagons)/BeePlot.png), [Colony Data](https://github.com/BlakeRMills/30DayMapChallenge/blob/main/Day%204%20(Hexagons)/Bees%2087-20.csv)
![PlotDay4](https://github.com/BlakeRMills/30DayMapChallenge/blob/main/Day%204%20(Hexagons)/BeePlot.png)

## [Day 5 (Open Street Map)](https://twitter.com/BlakeRobMills/status/1456999383657066502)
When it comes to maps and plots, I am a go big or go home kind of person. I love mapping cities, so I decided to show the primary, secondary, teritary pedestrian, and service roads of some major cities. The original plot was suppose to be 39 cities, but do to size and time constraints, I decided to limit it to 21. I also decided to make a plot of each city for resolution clarity. What other cities should I make? 

The following link takes you to the Open Street Map's website: [Open Street Maps](https://www.openstreetmap.org/#map=10/57.7076/11.9670)

[Full Main Map](https://github.com/BlakeRMills/30DayMapChallenge/blob/main/Day%205%20(Open%20Street%20Maps)/OpenStreetMap%20Total.png), [Full Code](https://github.com/BlakeRMills/30DayMapChallenge/blob/main/Day%205%20(Open%20Street%20Maps)/Day%205%20(OpenStreetMap).R), [Map Download](https://github.com/BlakeRMills/30DayMapChallenge/blob/main/Day%205%20(Open%20Street%20Maps)/OpenStreetMap%20Total.png), [Individual Maps](https://github.com/BlakeRMills/30DayMapChallenge/tree/main/Day%205%20(Open%20Street%20Maps)/Individual%20Maps)
![PlotDay5](https://github.com/BlakeRMills/30DayMapChallenge/blob/main/Day%205%20(Open%20Street%20Maps)/OpenStreetMap%20Total.png)
![AmsterdamMap](https://github.com/BlakeRMills/30DayMapChallenge/blob/main/Day%205%20(Open%20Street%20Maps)/Individual%20Maps/Amsterdam.png)
![MexicoCityMap](https://github.com/BlakeRMills/30DayMapChallenge/blob/main/Day%205%20(Open%20Street%20Maps)/Individual%20Maps/MexicoCity.png)
![NewYorkMap](https://github.com/BlakeRMills/30DayMapChallenge/blob/main/Day%205%20(Open%20Street%20Maps)/Individual%20Maps/NewYork.png)
![ParisMap](https://github.com/BlakeRMills/30DayMapChallenge/blob/main/Day%205%20(Open%20Street%20Maps)/Individual%20Maps/Paris.png)


