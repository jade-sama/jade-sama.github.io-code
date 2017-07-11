source("maps.R")
library(htmlwidgets)

Sys.setlocale("LC_TIME","Polish")
Sys.setlocale("LC_CTYPE","Polish")

azja_data <- load_data("Azja 2017", "azja-2017", GroupByCategory = TRUE)
azja_map <- create_map(connections = azja_data[[1]], places = azja_data[[2]], type = 'DayBubble')
setwd("../jade-sama.github.io/maps")
saveWidget(azja_map, file="azja-2017.html", selfcontained = FALSE, libdir="leaflet")
setwd("../../jade-sama.github.io-r")
azja_map

indie_data <- load_data("Azja 2017", "indie")
indie_map <- create_map(connections = indie_data[[1]], places = indie_data[[2]], type = 'OrderBubble')
setwd("../jade-sama.github.io/maps")
saveWidget(indie_map, file="indie.html", selfcontained = FALSE, libdir="leaflet")
setwd("../../jade-sama.github.io-r")
indie_map

malezja_data <- load_data("Azja 2017", "malezja-i-brunei")
malezja_map <- create_map(connections = malezja_data[[1]], places = malezja_data[[2]], type = 'OrderBubble')
setwd("../jade-sama.github.io/maps")
saveWidget(malezja_map, file="malezja-i-brunei.html", selfcontained = FALSE, libdir="leaflet")
setwd("../../jade-sama.github.io-r")
malezja_map

