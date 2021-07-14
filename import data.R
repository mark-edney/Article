library(tidyverse)
library(dataRetrieval)
library(ggmap)
library(maps)
library(mapdata)
library(RColorBrewer)
library(lubridate)
library(ggthemes)

wqp.data <- readWQPdata('startDateLo' = "2017-01-01", 
                        'startDateHi' = "2018-12-31", 
                        characteristicName=c("Phosphorus"),
                        providers= "NWIS") %>%
        filter(ResultSampleFractionText=="Total") %>%
        mutate(Result = as.numeric(ResultMeasureValue)) %>%
        filter(!is.na(Result),
               ActivityMediaName=="Water",
               Result > 0,
               ResultMeasure.MeasureUnitCode == "mg/l as P")

saveRDS(wqp.data, "wqpdata.RDS")
wqp.data <- readRDS("wqpdata.RDS")

wqp.data <- wqp.data %>%
        select(MonitoringLocationIdentifier, CharacteristicName, Result) %>%
        group_by(MonitoringLocationIdentifier, CharacteristicName) %>%
        summarise(Result = mean(Result), .groups = "keep")

wqp.site <- whatWQPsites(siteid=wqp.data$MonitoringLocationIdentifier) %>%
        select(MonitoringLocationIdentifier, lng = LongitudeMeasure,
               lat = LatitudeMeasure)

USA <- wqp.data %>% 
        left_join(wqp.site, by = "MonitoringLocationIdentifier") %>%
        arrange(Result)

#Canada Data
canurl <- "https://www.canada.ca/content/dam/eccc/documents/csv/cesindicators/water-quality-canadian-rivers/2020/wqi-federal-raw-data-2020-iqe-donnees-brutes-fed.csv"
download.file(url = canurl, "Candata.csv")
Can.Data <- read_csv("Candata.csv", col_types = cols(GUIDELINE_REFERENCE_RECOMMANDATION = col_character())) %>%
        filter(VARIABLE_NAME=="PHOSPHORUS") %>%
        mutate(DATE=dmy(DATE)) %>%
        select(DATE, VALUE_VALEUR, lat = LATITUDE, lng = LONGITUDE) %>%
        filter(DATE > dmy(01012017), DATE < dmy(3121018)) %>%
        group_by(lat, lng) %>%
        summarise(Avg_Con = mean(VALUE_VALEUR)) %>%
        arrange(Avg_Con)

#Europe Data
eurourl <- "https://cmshare.eea.europa.eu/s/XdNc9oFeXyEJknN/download"
download.file(url = eurourl, "eurodata.zip", mode="wb")
unzip("eurodata.zip", exdir = ".")
eurosite <- read_csv("Waterbase_v2020_1_S_WISE6_SpatialObject_DerivedData.csv") %>%
        select(monitoringSiteIdentifier, lat, lon)
eurodata <- read_csv("Waterbase_v2020_1_T_WISE6_AggregatedData.csv") %>%
        select(monitoringSiteIdentifier, param = observedPropertyDeterminandLabel, year = phenomenonTimeReferenceYear,Result = resultMeanValue ) %>%
        filter(param == "Total phosphorus") %>%
        filter(year == "2017"| year == "2018" ) %>%
        left_join(eurosite) %>%
        filter(!is.na(lat), !is.na(lon)) %>%
        arrange(Result)

Data <- data.frame(lon = c(USA$lng, Can.Data$lng, eurodata$lon),
                   lat = c(USA$lat, Can.Data$lat, eurodata$lat))
world_box <- make_bbox(lat=lat, lon = lon, data = Data)
gworld <- get_stamenmap(bbox = world_box,zoom = 4, maptype = "terrain-background")

colfunc<-colorRampPalette(c("yellow","red","springgreen","royalblue"))
gplot <- ggmap(gworld, extent = "device", padding = 0) +
        geom_point(data = USA, aes(x =lng, y = lat, color = Result), size = 2, alpha = 0.5) +
        geom_point(data = Can.Data, aes(x =lng, y = lat, color = Avg_Con), size = 2, alpha = 0.5) +
        geom_point(data = eurodata, aes(x =lon, y = lat, color = Result), size = 2, alpha = 0.5) + 
        scale_color_gradientn(colours = colfunc(4), name= "Total Phophorus \n Concentration (mg/L) \n log10",
                              trans = "log10") +
        theme_map() +
        theme(legend.background = element_blank())
gplot

