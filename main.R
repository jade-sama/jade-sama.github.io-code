source("maps.R")
library(htmlwidgets)

Sys.setlocale("LC_TIME","Polish")
Sys.setlocale("LC_CTYPE","Polish")

azja_data <- load_data("Azja 2017", "azja-2017", GroupByCategory = TRUE)
azja_map <- create_map(connections = azja_data[[1]], places = azja_data[[2]], type = 'DayBubble',MinDaysForBubble=21.0)
setwd("../pelican/jade-sama.github.io-source/content/maps")
saveWidget(azja_map, file="azja-2017.html", selfcontained = FALSE, libdir="leaflet")
setwd("../../../../jade-sama.github.io-r")
azja_map

indie_data <- load_data("Azja 2017", "indie")
indie_map <- create_map(connections = indie_data[[1]], places = indie_data[[2]], type = 'OrderBubble')
setwd("../pelican/jade-sama.github.io-source/content/maps")
saveWidget(indie_map, file="indie.html", selfcontained = FALSE, libdir="leaflet")
setwd("../../../../jade-sama.github.io-r")
indie_map

malezja_data <- load_data("Azja 2017", "malezja-i-brunei")
malezja_map <- create_map(connections = malezja_data[[1]], places = malezja_data[[2]], type = 'OrderBubble')
setwd("../pelican/jade-sama.github.io-source/content/maps")
saveWidget(malezja_map, file="malezja-i-brunei.html", selfcontained = FALSE, libdir="leaflet")
setwd("../../../../jade-sama.github.io-r")
malezja_map

filipiny_data <- load_data("Azja 2017", "filipiny")
filipiny_map <- create_map(connections = filipiny_data[[1]], places = filipiny_data[[2]], type = 'OrderBubble')
setwd("../pelican/jade-sama.github.io-source/content/maps")
saveWidget(filipiny_map, file="filipiny_selfcontained.html", selfcontained = FALSE, libdir="leaflet")
setwd("../../../../jade-sama.github.io-r")
filipiny_map

tajwan_data <- load_data("Azja 2017", "tajwan")
tajwan_map <- create_map(connections = tajwan_data[[1]], places = tajwan_data[[2]], type = 'OrderBubble')
setwd("../pelican/jade-sama.github.io-source/content/maps")
saveWidget(tajwan_map, file="tajwan.html", selfcontained = FALSE, libdir="leaflet")
setwd("../../../../jade-sama.github.io-r")
tajwan_map
