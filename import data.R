library(tidyverse)
library(dataRetrieval)
library(ggmap)
library(maps)
library(mapdata)
library(mapsapi)
library(RColorBrewer)
library(lubridate)

#register_google(key="AIzaSyDUDSTklS9m7VW8j5Hotu_mZxe-a4j9CP0", write = TRUE)

wqp.data <- readWQPdata('startDateLo' = "2017-01-01", 
                        'startDateHi' = "2018-12-31", 
                        characteristicName=c("Phosphorus")) %>%
        filter(ResultSampleFractionText=="Total") %>%
        mutate(Result = as.numeric(ResultMeasureValue)) %>%
        filter(!is.na(Result),
               ActivityMediaName=="Water")

saveRDS(wqp.data, "wqpdata.RDS")
wqp.data <- readRDS("wqpdata.RDS")

#Clean up of units
Case1 <- wqp.data$ResultMeasure.MeasureUnitCode=="ug/l"| wqp.data$ResultMeasure.MeasureUnitCode=="ug"
Case2 <- wqp.data$ResultMeasure.MeasureUnitCode=="ppm"| wqp.data$ResultMeasure.MeasureUnitCode=="mg/l as P"
Case3 <- wqp.data$ResultMeasure.MeasureUnitCode=="ppb"
Case4 <- wqp.data$ResultMeasure.MeasureUnitCode=="mg/kg"
Case5 <- wqp.data$ResultMeasure.MeasureUnitCode=="%"

wqp.data$Result[Case1|Case3] <- wqp.data$Result[Case1|Case3]/1000
wqp.data$Result[Case5] <- wqp.data$Result[Case5]*10

wqp.data$ResultMeasure.MeasureUnitCode[Case1|Case2|Case3|Case4|Case5] <- "mg/l"
wqp.data <- wqp.data %>% 
        filter(ResultMeasure.MeasureUnitCode=="mg/l") %>%
        filter(Result > 0, Result<30)

wqp.data <- wqp.data %>%
        select(MonitoringLocationIdentifier, CharacteristicName, Result) %>%
        group_by(MonitoringLocationIdentifier, CharacteristicName) %>%
        summarise(Result = mean(Result), .groups = "keep")

wqp.site <- whatWQPsites(siteid=wqp.data$MonitoringLocationIdentifier) %>%
        select(MonitoringLocationIdentifier, lng = LongitudeMeasure,
               lat = LatitudeMeasure)

gworld <- get_map(location = c(lon = -42, lat  = 54),
                  maptype = "watercolor",
                  zoom = 2,
                  source = "stamen")
world <- map_data("worldHires")
USA <- wqp.data %>% 
        left_join(wqp.site, by = "MonitoringLocationIdentifier") %>%
        arrange(Result)

#Canada Data
canurl <- "https://www.canada.ca/content/dam/eccc/documents/csv/cesindicators/water-quality-canadian-rivers/2020/wqi-federal-raw-data-2020-iqe-donnees-brutes-fed.csv"
download.file(url = canurl, "Candata.csv")
Can.Data <- read_csv("Cansite.csv", col_types = cols(GUIDELINE_REFERENCE_RECOMMANDATION = col_character())) %>%
        filter(VARIABLE_NAME=="PHOSPHORUS") %>%
        mutate(DATE=dmy(DATE)) %>%
        select(DATE, VALUE_VALEUR, lat = LATITUDE, lng = LONGITUDE) %>%
        filter(DATE > dmy(01012017)) %>%
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
        arrange(Result)


Cluster <- DF %>% ungroup %>%
        select(Result, lng, lat) %>%
        kmeans(10)


ClusterTbl <- as.data.frame(Cluster$centers)

colfunc<-colorRampPalette(c("yellow","red","springgreen","royalblue"))
gplot <- ggmap(gworld) + 
        geom_polygon(data = world, aes(x=long, y = lat, group = group),fill = NA, color="black") + 
        geom_point(data = USA, aes(x =lng, y = lat, color = Result), size = 2, alpha = 0.5) +
        geom_point(data = Can.Data, aes(x =lng, y = lat, color = Avg_Con), size = 2, alpha = 0.5) +
        geom_point(data = eurodata, aes(x =lon, y = lat, color = Result), size = 2, alpha = 0.5) + 
        scale_color_gradientn(colours = colfunc(4),
                              trans ="log10") 
gplot

