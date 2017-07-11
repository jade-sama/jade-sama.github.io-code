source("icons.R")
source("extra_functions.R")
library("leaflet")
library("googlesheets")
library("ggmap")
library("dplyr")

DateFormat="%d %b %y"

load_data <- function (WorkbookTitle, SheetName, GroupByCategory = FALSE) {

    # read data
    gs <- gs_title(WorkbookTitle)

    connections <- gs %>%
        gs_read(ws = SheetName)

    # populate departure columns if empty (with lag)
    connections$Departure <- if.na(connections$Departure,lag(connections$Arrival))
    connections$DepartureName <- if.na(connections$DepartureName,lag(connections$ArrivalName))
    connections$DepartureDate <- as.Date(connections$DepartureDate,"%Y-%m-%d")
    connections$DepartureDate[is.na(connections$DepartureDate)] <- lag(connections$LeaveDate)[is.na(connections$DepartureDate)]

    # label columns
    connections <- connections %>%
        mutate( Stay = paste0("<br/>",capwords(format(ArrivalDate,DateFormat)),
                             ifelse(LeaveDate==ArrivalDate,
                                    "",
                                    paste0(" - ",if.na(capwords(format(LeaveDate,DateFormat),"?")))
                                    )
                             ),
                Journey = paste0("<b>",DepartureName,
                                 ifelse(Return==1, " <-> ", " -> "),
                                ArrivalName, "</b>",
                                "</br>",capwords(format(DepartureDate,DateFormat)),
                                ifelse(Return==1, paste0(" (",Length," d)"), "")
                                )
              )

    # aggregate places data
    if (GroupByCategory) {
        places <- connections %>%
            group_by (Category, CategoryName, Link) %>%
            summarise (MinDate=min(ArrivalDate), MaxDate=max(LeaveDate),
                       Length=sum(Length,na.rm = TRUE),
                       Stay=paste0(Stay,collapse="")
                       ) %>%
            as.data.frame()
        colnames(places)[1:2] <- c("Arrival","ArrivalName")
    } else {
        places <- connections %>%
            group_by (Arrival, ArrivalName, Link) %>%
            summarise (MinDate=min(ArrivalDate), MaxDate=max(LeaveDate),
                       Length=sum(Length,na.rm = TRUE),
                       Stay=paste0(Stay,collapse="")
                       ) %>%
            as.data.frame()

    }

    # extra columns for chart
    places$Order <- as.character(rank(places$MinDate,ties.method= "first"))

    places <- places %>%
        mutate(Month=month_name(MinDate,MaxDate))


    # get coordinates
    places <- places %>%
        do(geocode(.$Arrival)) %>%
        cbind(places)
    colnames(places)[1:2] <- c("ArrivalLon", "ArrivalLat")

    connections <- connections[!is.na(connections$Arrival),] %>%
        do(geocode(.$Arrival)) %>%
        cbind(connections[!is.na(connections$Arrival),])
    colnames(connections)[1:2] <- c("ArrivalLon", "ArrivalLat")

    connections <- connections[!is.na(connections$Departure),] %>%
        do(geocode(.$Departure)) %>%
        cbind(connections[!is.na(connections$Departure),])
    colnames(connections)[1:2] <- c("DepartureLon", "DepartureLat")

    # mode of transport markers
    connections$MarkerLon <- (connections$DepartureLon+connections$ArrivalLon)/2
    connections$MarkerLat <- (connections$DepartureLat+connections$ArrivalLat)/2

    return (list(connections, places))
}

create_map <- function(connections, places, type, # c("DayBubble", "OrderBubble")
                       TileOpacity = 0.35, MarkerOpacity = 0.8,
                       AdjRadius = 50000, MinDaysForBubble = 14,
                       BorderWeight = 2, LineWeight = 4, LineDash = "1,10",
                       LabelFontSize1 = "18px", LabelFontSize2 = "16px", LabelFontSize3 = "14px") {

    # bounds for the map
    MinLat=min(places$ArrivalLat)-1.5
    MaxLat=max(places$ArrivalLat)+1.5
    MinLon=min(places$ArrivalLon)-1.5
    MaxLon=max(places$ArrivalLon)+1.5

    # map tiles
    map <- leaflet(places) %>%
        fitBounds(MinLon, MinLat,MaxLon,MaxLat) %>%
        addProviderTiles(providers$Stamen.Watercolor) %>%
        addProviderTiles(providers$Stamen.TonerLines,
                         options = providerTileOptions(opacity = TileOpacity))

    if (type=="OrderBubble") {
        map <- map %>%
            addProviderTiles(providers$Stamen.TonerLabels,
                             options = providerTileOptions(opacity = MarkerOpacity))

        # markers and order for places
        map <- map %>%
            addCircleMarkers(lng = ~ArrivalLon, lat = ~ArrivalLat,
                             radius = MinDaysForBubble,
                             fillOpacity = MarkerOpacity, fillColor = BlogColours[1],
                             stroke = TRUE, color = BlogColours[4], weight = BorderWeight,
                             popup = ~paste0("<b><a href='",Link,"'>", ArrivalName,"</a></b>", Stay),
                             label = ~Order,
                             labelOptions = labelOptions(noHide = TRUE, textOnly = TRUE, direction="top", offset=c(0,-MinDaysForBubble),
                                                         style = list("color" = BlogColours[4], "font-size" = LabelFontSize3)))

    }

    if (type=="DayBubble") {
        # places circles based on stay length
        map <- map %>%
            addCircles(lng = ~ArrivalLon, lat = ~ArrivalLat,
                       radius = ~sqrt(if.zero(Length,MinDaysForBubble))*AdjRadius,
                       fillOpacity = MarkerOpacity, fillColor = BlogColours[1],
                       stroke = TRUE, color = BlogColours[4], weight = BorderWeight)

        # labels for stay length
        map <- map %>%
            addLabelOnlyMarkers(lng = ~ArrivalLon, lat = ~ArrivalLat-0.5,
                                label=~paste(if.zero(Length,"?"),"d"),
                                labelOptions = labelOptions(noHide = TRUE, textOnly = TRUE, direction = "top",
                                                            style = list("color" = BlogColours[4], "font-size" = LabelFontSize2)))

        # labels for places
        map <- map %>%
            addCircleMarkers(lng = ~ArrivalLon, lat = ~ArrivalLat+sqrt(max(Length,MinDaysForBubble))-1.5,
                             radius = MinDaysForBubble, fillOpacity = 0, stroke = FALSE,
                             popup = ~paste0("<b><a href='",Link,"'>", ArrivalName,"</a></b>",Stay),
                             label = ~ArrivalName,
                             labelOptions = labelOptions(noHide = TRUE, textOnly = FALSE,direction = "top",
                                                         style = list("color" = BlogColours[1], "font-size" = LabelFontSize1,
                                                                      "border-width"="0px", "fill-opacity" = MarkerOpacity)))

        # labels for month
        map <- map %>%
            addLabelOnlyMarkers(lng = ~ArrivalLon, lat = ~ArrivalLat-sqrt(max(Length,MinDaysForBubble))+1.5,
                                label=~Month,
                                labelOptions = labelOptions(noHide = TRUE, textOnly = TRUE, direction = "top",
                                                            style = list("color" = BlogColours[2], "font-size" = LabelFontSize2,
                                                                         "border-width"="0px","fill-opacity" = MarkerOpacity)))
    }

    # add connection lines
    for(i in 1:nrow(connections)){
        map <- map %>%
            addPolylines(data = connections,
                         lat = as.numeric(connections[i, c("DepartureLat","ArrivalLat")]),
                         lng = as.numeric(connections[i, c("DepartureLon","ArrivalLon")]),
                         color = BlogColours[2], opacity = MarkerOpacity,
                         weight = LineWeight, dashArray = LineDash)
    }

    # add mode of transport markers
    map <- map %>%
        addMarkers(data = connections[connections$Return!=-1,], lat=~MarkerLat, lng=~MarkerLon,
                   icon = ~TransportIcons[TransportToArrival],
                   popup = ~Journey)

    return(map)

}
