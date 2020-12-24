library(tidyverse)
library(leaflet)
library(leaflet.extras)
library(htmltools)
library(htmlwidgets)
library(RColorBrewer)
library(scales)
library(rgdal)
library(sp)
getwd()
dat <- read.csv('C:/Users/USER/Documents/sh_R/jeju_gis/KRI_DAC_Jeju.csv')
dat %>%
  group_by(long, lat) %>%
  summarise(TotalSpent_sum = round(sum(TotalSpent) / 100000000,2),
            DisSpent_sum = round(sum(DisSpent) / 100000000,2),
            NumofSpent_sum = sum(NumofSpent),
            NumofDisSpent_sum = sum(NumofDisSpent)) -> dat_longlat
#write.csv(dat_longlat, '5-8월_총이용금액+이용건수.csv')
dat %>%
  filter(FranClass == '일반' ) %>%
  group_by(Type, long, lat) %>%
  summarise(TotalSpent_sum = sum(TotalSpent),
            NumofSpent_sum = sum(NumofSpent)) %>% 
  arrange(desc(TotalSpent_sum, NumofSpent_sum)) -> jeju_big_type
dat %>%
  filter(FranClass == '영세' ) %>%
  group_by(Type, long, lat) %>%
  summarise(TotalSpent_sum = sum(TotalSpent),
            NumofSpent_sum = sum(NumofSpent)) %>% 
  arrange(desc(TotalSpent_sum, NumofSpent_sum)) -> jeju_small_type
dat %>%
  filter(FranClass == '중소' ) %>%
  group_by(Type, long, lat) %>%
  summarise(TotalSpent_sum = sum(TotalSpent),
            NumofSpent_sum = sum(NumofSpent)) %>% 
  arrange(desc(TotalSpent_sum, NumofSpent_sum)) -> jeju_mid1_type
dat %>%
  filter(FranClass == '중소1' ) %>%
  group_by(Type, long, lat) %>%
  summarise(TotalSpent_sum = sum(TotalSpent),
            NumofSpent_sum = sum(NumofSpent)) %>% 
  arrange(desc(TotalSpent_sum, NumofSpent_sum)) -> jeju_mid2_type
dat %>%
  filter(FranClass == '중소2' ) %>%
  group_by(Type, long, lat) %>%
  summarise(TotalSpent_sum = sum(TotalSpent),
            NumofSpent_sum = sum(NumofSpent)) %>% 
  arrange(desc(TotalSpent_sum, NumofSpent_sum)) -> jeju_mid3_type

#이용금액이 업종별 가장 많은 장소 TOP10 (제주시)
dat %>%
  filter(SIGUNGU == 'jeju-si') %>%
  group_by(FranClass,Type,long,lat) %>%
  summarise(TotalSpent_sum = round(sum(TotalSpent) / 100000000,2),
            NumofSpent_sum = sum(NumofSpent)) %>% 
  arrange(desc(TotalSpent_sum, NumofSpent_sum)) %>% head(100) -> total_jejusi_top100_type

#이용금액이 업종별 가장 많은 장소 TOP10 (서귀포시)
dat %>%
  filter(SIGUNGU == 'seoguipo-si') %>%
  group_by(FranClass,Type,long,lat) %>%
  summarise(TotalSpent_sum = round(sum(TotalSpent) / 100000000,2),
            NumofSpent_sum = sum(NumofSpent)) %>% 
  arrange(desc(TotalSpent_sum, NumofSpent_sum)) %>% head(100) -> total_seogui_top100_type

emd_total_df <- read.csv('C:/Users/USER/Documents/sh_R/jeju_gis/total_spent-num_emd.csv')
emd_total_df %>%
  mutate(EMD_CD = as.character(EMD_CD)) %>%
  group_by(EMD_CD, EMD_KOR_NM) %>%
  summarise(TotalSpent_sum = sum(TotalSpent),
            DisSpent_sum = sum(DisSpent_s),
            NumofSpent_sum = sum(NumofSpent),
            NumofDisSpent_sum = sum(NumofDisSp)) -> emd_sum_df

# 생산가능인구
jeju_emd_val <- readOGR('C:/Users/USER/Documents/sh_R/jeju_gis/vl_blk.shp')
to.crs = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
jeju_emd_val <- spTransform(jeju_emd_val, to.crs)
jeju_emd_val@data <- jeju_emd_val@data[,c('gid','val')]
jeju_emd_val@data <- rename(jeju_emd_val@data, "EMD_CD" = "gid")

jeju_emd <- readOGR('C:/Users/USER/Documents/sh_R/jeju_gis/jeju_emd.shp')
to.crs = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs") # 좌표변환
jeju_emd <- spTransform(jeju_emd, to.crs)
jeju_emd@data <- inner_join(jeju_emd@data, emd_sum_df)
rm(emd_sum_df); rm(emd_total_df);
library(reshape2)
jeju_emd@data <- inner_join(jeju_emd@data, jeju_emd_val@data)
head(jeju_emd@data); rm(jeju_emd_val);
jeju_emd@data %>%
  melt()
jeju_emd@data %>%
  ggplot(aes(x = val, y = TotalSpent_sum / 100000000)) + 
  geom_point() + stat_smooth(method = 'lm') + theme_bw()
jeju_emd@data %>%
  ggplot(aes(x = val, y = NumofSpent_sum)) + 
  geom_point() + stat_smooth(method = 'lm') + theme_bw()

labels <- sprintf(
  "<strong>%s</strong><br/>총 %g억 원 <br>총 이용건수: %g <br>생산가능인구: %g",
  jeju_emd@data$EMD_KOR_NM, round(jeju_emd@data$TotalSpent_sum / 100000000,2), jeju_emd@data$NumofSpent_sum, jeju_emd@data$val
) %>% lapply(htmltools::HTML)

bins_spent <- c(0, 100, 200, 300, 400, 500, Inf)
bins_num <- c(1000, 5000, 10000, 50000, 100000, 500000, 1000000, Inf)
bins_pop <- c(0, 1000, 5000, 10000, 20000, 30000, Inf)
qpal <- colorQuantile('YlOrRd', domain = jeju_emd@data$NumofSpent_sum, n = 5 )
pal <- colorBin("YlOrRd", jeju_emd@data$TotalSpent_sum, bins = bins_spent)
pal2 <- colorBin("YlOrRd", jeju_emd@data$TotalSpent_sum, bins = bins_num)
pal3 <- colorBin("YlOrRd", jeju_emd@data$TotalSpent_sum, bins = bins_pop)

map <- leaflet(jeju_emd) %>%
  addTiles(group = "OSM(default)") %>%
  addCircleMarkers(data = jeju_small_type,
                   group = "소상공인_영세",
                   lng = ~long, lat = ~lat,
                   label = ~htmlEscape(Type), popup = ~paste0("총 ",comma(TotalSpent_sum, format = "d"),"원"),
                   radius = 5,
                   color = 'red',
                   stroke = FALSE, fillOpacity = 0.5) %>%
  addCircleMarkers(data = jeju_big_type,
                   group = "소상공인_일반",
                   lng = ~long, lat = ~lat,
                   label = ~htmlEscape(Type), popup = ~paste0("총 ",comma(TotalSpent_sum, format = "d"),"원"),
                   radius = 5,
                   color = 'blue',
                   stroke = FALSE, fillOpacity = 0.5) %>%
  addCircleMarkers(data = jeju_mid1_type,
                   group = "소상공인_중소1",
                   lng = ~long, lat = ~lat,
                   label = ~htmlEscape(Type), popup = ~paste0("총 ",comma(TotalSpent_sum, format = "d"),"원"),
                   radius = 5,
                   color = 'orange',
                   stroke = FALSE, fillOpacity = 0.5) %>%
  addCircleMarkers(data = jeju_mid2_type,
                   group = "소상공인_중소2",
                   lng = ~long, lat = ~lat,
                   label = ~htmlEscape(Type), popup = ~paste0("총 ",comma(TotalSpent_sum, format = "d"),"원"),
                   radius = 5,
                   color = 'green',
                   stroke = FALSE, fillOpacity = 0.5) %>%
  addCircleMarkers(data = jeju_mid3_type,
                   group = "소상공인_중소3",
                   lng = ~long, lat = ~lat,
                   label = ~htmlEscape(Type), popup = ~paste0("총 ",comma(TotalSpent_sum, format = "d"),"원"),
                   radius = 5,
                   color = 'skyblue',
                   stroke = FALSE, fillOpacity = 0.5) %>%
  
  
  addMarkers(data = total_jejusi_top100_type,
             lng = ~long, lat = ~lat,
             label = ~htmlEscape(paste0(Type,"(",FranClass,")")), popup = ~paste0("약",as.character(TotalSpent_sum),"억원" ),
             group = "제주시 총이용금액 TOP100") %>%
  addMarkers(data = total_seogui_top100_type,
             lng = ~long, lat = ~lat,
             label = ~htmlEscape(Type), popup = ~paste0("약",as.character(TotalSpent_sum),"억원"),
             group = '서귀포시 총이용금액 TOP100') %>%
  
  addHeatmap(data = dat_longlat,
             lng = ~long, lat = ~lat,
             intensity = ~NumofSpent_sum, blur = 30, max = 100000, radius = 20,
             group = "총 이용건수 히트맵") %>%
  
  addHeatmap(data = dat_longlat,
             lng = ~long, lat = ~lat,
             intensity = ~TotalSpent_sum, blur = 30, max = 10, radius = 20,
             group = "총 이용금액 히트맵") %>%
  
  addPolygons(data = jeju_emd,
              group = "읍면동별_생산가능인구",
              fillColor = ~pal3(val),
              weight = 1,  opacity = 1, color = "black", dashArray = "1", fillOpacity = 0.7,
              highlight = highlightOptions(
                weight = 5,
                color = "#666",
                dashArray = "",
                fillOpacity = 0.7,
                bringToFront = TRUE),
              label = labels,
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto")) %>% 
  addLegend(pal = pal3, values = ~val, opacity = 0.7, 
            title = "생산가능인구",
            labFormat = labelFormat(suffix = "명"),
            position = "bottomleft",
            group = "읍면동별_생산가능인구") %>%
  
  addPolygons(data = jeju_emd,
              group = "읍면동별_총이용금액",
              fillColor = ~pal(TotalSpent_sum / 100000000),
              weight = 1,  opacity = 1, color = "black", dashArray = "1", fillOpacity = 0.7,
              highlight = highlightOptions(
                weight = 5,
                color = "#666",
                dashArray = "",
                fillOpacity = 0.7,
                bringToFront = TRUE),
              label = labels,
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto")) %>% 
  addLegend(pal = pal, values = ~TotalSpent_sum / 100000000, opacity = 0.7, 
            title = "총 이용금액",
            labFormat = labelFormat(suffix = "억 원"),
            position = "bottomleft",
            group = "읍면동별_총이용금액") %>%
  
  addPolygons(data = jeju_emd,
              group = "읍면동별_총이용건수",
              fillColor = ~qpal(NumofSpent_sum),
              weight = 1,
              opacity = 1,
              color = "black",
              dashArray = "1",
              fillOpacity = 0.7,
              highlight = highlightOptions(
                weight = 5,
                color = "#666",
                dashArray = "",
                fillOpacity = 0.7,
                bringToFront = TRUE),
              label = labels,
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto")) %>% 
  addLegend(pal = pal2, values = ~NumofSpent_sum, opacity = 0.7, 
            title = "총 이용건수",
            labFormat = labelFormat(suffix = "건"),
            position = "bottomleft",
            group = "읍면동별_총이용건수") %>%
  
  addLayersControl(baseGroups = c("OSM(default)"),
                   overlayGroups = c("소상공인_영세","소상공인_일반","소상공인_중소1","소상공인_중소2","소상공인_중소3",
                                     "제주시 총이용금액 TOP100", "서귀포시 총이용금액 TOP100", 
                                     "총 이용건수 히트맵", "총 이용금액 히트맵",
                                     "읍면동별_생산가능인구", "읍면동별_총이용금액", "읍면동별_총이용건수"),
                   options = layersControlOptions(collapsed = FALSE)) %>%
  hideGroup(c("제주시 총이용금액 TOP100", "서귀포시 총이용금액 TOP100",
              "총 이용건수 히트맵", "총 이용금액 히트맵", 
              "읍면동별_총이용건수", "읍면동별_생산가능인구",
              "소상공인_영세", "소상공인_일반","소상공인_중소1","소상공인_중소2","소상공인_중소3"))

map 


