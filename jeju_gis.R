getwd()
setwd("C:/Users/USER/Documents/sh_R/jeju_gis")
library(tidyverse)
library(extrafont)
library(ggplot2)
library(gridExtra)
library(reshape2)
library(leaflet)
library(leaflet.extras)
library(htmltools)
library(htmlwidgets)
library(maptools)
library(rgdal)
library(sp)
options(digits = 10)
dat5 <- read.table('KRI-DAC_Jeju_data5.txt', fileEncoding = 'UTF-8', sep = ',', header = TRUE)
dat6 <- read.table('KRI-DAC_Jeju_data6.txt', fileEncoding = 'UTF-8', sep = ',', header = TRUE)
dat7 <- read.table('KRI-DAC_Jeju_data7.txt', fileEncoding = 'UTF-8', sep = ',', header = TRUE)
dat8 <- read.table('KRI-DAC_Jeju_data8.txt', fileEncoding = 'UTF-8', sep = ',', header = TRUE)
dat7 %>%
  select(-X, -Y) -> dat7

# EDA
# 데이터 정제 및 전처리
dat <- rbind(dat5,dat6,dat7,dat8)
str(dat)
dat <- dat %>%
  select(-OBJECTID, -Field1)

dat <- dat %>%
  mutate(timeName = ifelse(Time == "02시" | Time == "03시" | Time == "04시" | Time == "05시", "새벽",
                           ifelse(Time == "06시" | Time == "07시" | Time == "08시" | Time == "09시" | Time == "10시" | 
                                    Time == "11시" , "오전",
                                  ifelse(Time == "12시" | Time == "13시" | Time == "14시", "점심",
                                         ifelse(Time == "15시" | Time == "16시" | Time == "17시" , "오후", 
                                                ifelse(Time == "18시" | Time == "19시" | Time == "20시" | Time == "21시", "저녁",
                                                       ifelse(Time == "22시" | Time == "23시" | Time == "00시" | Time == "01시", "심야", "무승인")))))))
factor(dat$timeName)
dat$timeName <- factor(dat$timeName, levels = c('새벽','오전','점심','오후','심야','무승인'))
# 좌표계변환 
convertCoordSystem <- function(long, lat, from.crs, to.crs){
  xy <- data.frame(long=long, lat=lat)
  coordinates(xy) <- ~long+lat
  
  from.crs <- CRS(from.crs)
  from.coordinates <- SpatialPoints(xy, proj4string=from.crs)
  
  to.crs <- CRS(to.crs)
  changed <- as.data.frame(SpatialPoints(spTransform(from.coordinates, to.crs)))
  names(changed) <- c("long", "lat")
  
  return(changed)
}
# from.crs = "+proj=longlat +ellps=GRS80 +no_defs"
from.crs = "+proj=tmerc +lat_0=38 +lon_0=127.5 +k=0.9996 +x_0=1000000 +y_0=2000000 +ellps=GRS80 +units=m +no_defs"
to.crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
coord <- convertCoordSystem(dat$POINT_X, dat$POINT_Y, from.crs, to.crs)
dat <- cbind(dat, coord)

# df_5 <- dat %>%
#   filter(YM == '202005') %>%
#   group_by(long, lat) %>%
#   summarise(TotalSpent_sum = sum(TotalSpent),
#             DisSpent_sum = sum(DisSpent),
#             NumofSpent_sum = sum(NumofSpent),
#             NumofDisSpent_sum = sum(NumofDisSpent))
# write.csv(df_5, '5월_이용금액+이용건수_총합.csv')
# write.csv(dat, 'KRI_DAC_Jeju.csv')

# 제주시 vs 서귀포시 총 이용금액
dat %>%
  group_by(SIGUNGU) %>% 
  summarise(TotalSpent_sum = sum(TotalSpent) / 100000000,
            DisSpent_sum = sum(DisSpent) / 100000000) %>%
  mutate(DisSpent_ratio = round((DisSpent_sum / TotalSpent_sum),3)  * 100) %>%
  ggplot(aes(x = "", y = TotalSpent_sum, fill = SIGUNGU)) +
  geom_bar(stat='identity', color = 'white') +
  coord_polar(theta = "y")

# 5~8월까지  업종별 이용금액 총합 TOP10(재난지원금포함)
dat %>%
  group_by(Type) %>%
  summarise(sum = sum(TotalSpent)) %>%
  arrange(desc(sum)) %>%
  head(10) %>%
  mutate(sum = sum / 100000000) %>%
  ggplot(aes(x = reorder(Type, sum), y=sum)) +
  geom_bar(stat='identity') +
  coord_flip() + theme_bw() +
  ggtitle("5~8월 업종별 이용금액 총합 TOP10(재난지원금포함)") +
  labs(x = "업종", y = "이용금액(억)") +
  theme(plot.title = element_text(size = 14, hjust = 0.5, family = 'NanumGothic', face = 'bold'))

##일반한식, 슈퍼마켓, 편의점, 면세점, 주유소, 서양음식, 농축협직영매장, 유아원, 대형할인점, 골프경기장

# 5~8월까지 총 이용금액(지원금제외)이 가장 큰 업종 TOP10
dat %>%
  group_by(Type) %>%
  mutate(Spent = TotalSpent - DisSpent) %>%
  summarise(sum = sum(Spent)) %>%
  arrange(desc(sum)) %>%
  head(10) %>%
  mutate(sum = sum / 100000000) %>%
  ggplot(aes(x = reorder(Type, sum), y=sum)) +
  geom_bar(stat='identity') +
  coord_flip() + theme_bw() +
  scale_y_continuous(breaks = seq(0,1100, 100)) +
  ggtitle("5~8월 총 이용금액 TOP10 업종(지원금제외)") +
  labs(x = "업종", y = "이용금액(억)") +
  theme(plot.title = element_text(size = 13, hjust = 0.5, family = 'NanumGothic', face = 'bold'),
        axis.title = element_text(size = 11, family = 'NanumGothic', face = 'bold'))

# 업종별 총 이용건수
dat %>%
  group_by(Type) %>%
  summarise(NumofSpent_sum = sum(NumofSpent)) %>%
  arrange(desc(NumofSpent_sum))

# 업종별 재난지원금 이용건수
dat %>%
  group_by(Type) %>%
  summarise(NumofDisSpent_sum = sum(NumofDisSpent)) %>%
  arrange(desc(NumofDisSpent_sum))

# 재난지원금 이용금액 TOP10 업종
dat %>%
  group_by(Type) %>%
  summarise(sum = sum(DisSpent)) %>%
  arrange(desc(sum)) %>%
  head(10) %>%
  mutate(sum = sum / 100000000) %>%
  ggplot(aes(x = reorder(Type, sum), y=sum)) +
  geom_bar(stat='identity') +
  coord_flip() + theme_bw() +
  ggtitle("5~8월 총 재난지원금 이용금액 TOP10 업종") +
  labs(x = "업종", y = "이용금액(억)") +
  scale_y_continuous(breaks = seq(0,70, 10)) +
  theme(plot.title = element_text(size = 13, hjust = 0.5, family = 'NanumGothic', face = 'bold'),
        axis.title = element_text(size = 11, family = 'NanumGothic', face = 'bold'))

# 소상공인 구분
dat %>%
  group_by(FranClass) %>%
  summarise(TotalSpent_sum = sum(TotalSpent) / 100000000) %>%
  ggplot(aes(x = FranClass, y = TotalSpent_sum)) +
  geom_bar(stat='identity') +
  theme_bw()

dat %>%
  group_by(FranClass) %>%
  summarise(DisSpent_sum = sum(DisSpent)) %>%
  ggplot(aes(x = FranClass, y = DisSpent_sum)) +
  geom_bar(stat='identity') + 
  theme_bw()

#소상공인타입별 이용금액
dat %>%
  group_by(FranClass) %>%
  summarise(TotalSpent_sum = sum(TotalSpent) / 100000000,
            DisSpent_sum = sum(DisSpent) / 100000000) %>% 
  mutate(Spent_sum = TotalSpent_sum - DisSpent_sum) %>% 
  melt(id.vars = 'FranClass', measure.vars = c('TotalSpent_sum', 'DisSpent_sum', 'Spent_sum')) %>%
  ggplot(aes(x = FranClass, y = value, fill = variable)) + 
  geom_bar(stat='identity', position = 'dodge', color = 'black') +
  theme_bw()


#시간대별 이용금액
dat %>%
  filter(Time != 'x시') %>%
  group_by(YM, Time) %>%
  summarise(TotalSpent_sum = round(sum(TotalSpent) / 100000000,1)) %>%
  ggplot(aes(x = Time, y = TotalSpent_sum, group = as.character(YM), color = as.character(YM))) +
  geom_line(size = 1) +
  scale_color_discrete(name="월별") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.3, hjust = 0.5))

dat %>%
  filter(Time != 'x시') %>%
  mutate(Type = factor(Type)) %>%
  group_by(Type, Time) %>%
  summarise(TotalSpent_sum = round(sum(TotalSpent) / 100000000,1)) %>%
  arrange(Type, TotalSpent_sum) %>% 
  ggplot(aes(x = Time, y = TotalSpent_sum, group = as.character(Type), color = as.character(Type))) +
  geom_line(size = 1) +
  scale_color_discrete(name="월별") +
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.3, hjust = 0.5))

dat %>%
  group_by(timeName) %>%
  summarise(TotalSpent_sum = sum(TotalSpent) / 100000000) %>%
  ggplot(aes(x = timeName, y = TotalSpent_sum)) +
  geom_bar(stat='identity')

dat %>%
  group_by(YM, timeName) %>%
  summarise(TotalSpent_sum = sum(TotalSpent) / 100000000) %>%
  ggplot(aes(x = timeName, y = TotalSpent_sum, group = as.character(YM), fill= as.character(YM))) +
  geom_bar(stat='identity', position = 'dodge', color = 'black') +
  theme_bw()

dat %>%
  filter(Type == '일반한식' | Type == '슈퍼마켓' |Type == '면세점' | Type == '주유소' | Type == '서양음식' | 
           Type == '농축협직영매장' | Type == '유아원' | Type == '대형할인점' | Type == '골프경기장' | Type == '편의점') %>%
  group_by(YM, Type) %>%
  summarise(sum = sum(TotalSpent) / 100000000) %>%
  ggplot(aes(x = YM, y = sum, fill = Type)) +
  geom_bar(stat='identity', position = 'dodge', color = 'black')

# 월별 총이용금액 대비 재난지원금사용금액 비율 (1차 재난지원금 4~5월)
dat %>%
  group_by(YM) %>%
  summarise(TotalSpent_sum = sum(TotalSpent),
            DisSpent_sum = sum(DisSpent)) %>%
  mutate(DisSpent_ratio = round((DisSpent_sum / TotalSpent_sum),3)  * 100)

dat5$POINT_X <- as.numeric(dat5$POINT_X)()
dat5$POINT_Y <- as.numeric(dat5$POINT_Y)
leaflet(dat5) %>%
  addTiles() %>%
  addMarkers(lng = ~long, lat = ~lat, clusterOptions = markerClusterOptions())

dat %>%
  filter(YM == '202005') %>%
  group_by(FranClass, Type, long, lat) %>%
  summarise(TotalSpent_sum = sum(TotalSpent),
            DisSpent_sum = sum(DisSpent),
            NumofSpent_sum = sum(NumofSpent),
            NumofDisSpent_sum = sum(NumofDisSpent)) -> month5_df
month5_df %>%
  leaflet() %>% 
  addTiles() %>%
  setView(lng = 126.503448, lat =  33.321349  , zoom = 10) %>%
  addHeatmap(lng = ~long, lat = ~lat, intensity = ~TotalSpent_sum, blur = 30, max = 1000000000, radius = 20)

#이용금액이 업종별 가장 많은 장소 TOP10(전체)
dat %>%
  group_by(Type,long,lat) %>%
  summarise(sum = round(sum(TotalSpent) / 100000000),2) %>% 
  arrange(desc(sum)) %>% head(10)  %>%
  leaflet() %>%
  addTiles() %>%
  addMarkers(lng = ~long, lat = ~lat, label = ~htmlEscape(Type), popup = ~paste0("약",as.character(sum),"억원"))

#이용금액이 업종별 가장 많은 장소 TOP10 (제주시)
dat %>%
  filter(SIGUNGU == '제주시') %>%
  group_by(Type,long,lat) %>%
  summarise(TotalSpent_sum = round(sum(TotalSpent) / 100000000,2),
            NumofSpent_sum = sum(NumofSpent)) %>% 
  arrange(desc(TotalSpent_sum, NumofSpent_sum)) %>% head(10) -> total_jejusi_top10_type
total_jejusi_top10_type %>%
  leaflet() %>%
  addTiles() %>%
  addMarkers(lng = ~long, lat = ~lat, 
             label = ~htmlEscape(Type), 
             popup = ~paste0("약",as.character(sum),"억원"))

#이용금액이 업종별 가장 많은 장소 TOP10 (서귀포시)
dat %>%
  filter(SIGUNGU == '서귀포시') %>%
  group_by(Type,long,lat) %>%
  summarise(sum = round(sum(TotalSpent) / 100000000),2) %>% 
  arrange(desc(sum)) %>% head(10) -> total_seogui_top10_type
total_seogui_top10_type %>%
  leaflet() %>%
  addTiles() %>%
  addMarkers(lng = ~long, lat = ~lat, 
             label = ~htmlEscape(Type), 
             popup = ~paste0("약",as.character(sum),"억원"))

#이용금액이 업종별 가장 많은 장소 TOP10 (제주시)
dat %>%
  filter(SIGUNGU == '제주시' & FranClass == '영세') %>%
  group_by(Type,long,lat) %>%
  summarise(TotalSpent_sum = round(sum(TotalSpent) / 100000000,2),
            NumofSpent_sum = sum(NumofSpent)) %>%
  arrange(desc(sum)) %>% head(10) %>%
  leaflet() %>%
  addTiles() %>%
  addMarkers(lng = ~long, lat = ~lat, label = ~htmlEscape(Type), popup = ~paste0("약",as.character(sum),"억원"))


# 읍면동 shp 불러오기
emd_may <- read.csv('읍면동_5월_총합.csv')
emd_may %>% 
  mutate(EMD_CD = as.character(EMD_CD)) %>%
  group_by(EMD_CD, EMD_KOR_NM, val) %>%
  summarise(TotalSpent_sum = round(sum(TotalSpent) / 100000000,2),
            DisSpent_sum = round(sum(DisSpent_s) / 100000000,2),
            NumofSpent_sum = sum(NumofSpent),
            NumofDisSpent_sum = sum(NumofDisSp)) -> emd_may_total

jeju_emd <- readOGR('제주읍면동.shp')
to.crs = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs") # 좌표변환
jeju_emd <- spTransform(jeju_emd, to.crs)

jeju_emd@data <- left_join(jeju_emd@data, emd_may_total)

pal <- colorBin("YlOrRd", domain = jeju_emd@data$TotalSpent_sum)
labels <- sprintf(
  "<strong>%s</strong><br/>총 %g억 원 <br>총 이용건수: %g <br>생산가능인구: %g",
  jeju_emd@data$EMD_KOR_NM, jeju_emd@data$TotalSpent_sum, jeju_emd@data$NumofSpent_sum, jeju_emd@data$val
) %>% lapply(htmltools::HTML)

leaflet(jeju_emd) %>%
  addTiles() %>%
  addPolygons(fillColor = ~pal(TotalSpent_sum),
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
              label = labels)


labels <- sprintf(
  "<strong>%s</strong><br/>총 %g억 원 <br>총 이용건수: %g <br>생산가능인구: %g",
  jeju_emd@data$EMD_KOR_NM, jeju_emd@data$TotalSpent_sum, jeju_emd@data$NumofSpent_sum, jeju_emd@data$val
) %>% lapply(htmltools::HTML)

bins <- c(0, 10, 30, 50, 70, 90, 100, Inf)
qpal <- colorQuantile('YlOrRd', domain = jeju_emd@data$NumofSpent_sum, n = 5 )
pal <- colorBin("YlOrRd", jeju_emd@data$TotalSpent_sum, bins = bins)

map <- leaflet(jeju_emd) %>%
  addTiles(group = "OSM(default)") %>%
  addMarkers(data = total_jejusi_top10_type,
             lng = ~long, lat = ~lat,
             group = "Jejusi_Top10") %>%
  addMarkers(data = total_seogui_top10_type,
            lng = ~long, lat = ~lat,
            group = 'Seogui_Top10') %>%
  
  addPolygons(data = jeju_emd,
              group = "읍면동별_총이용금액",
              fillColor = ~pal(TotalSpent_sum),
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
  addLegend(pal = pal, values = ~TotalSpent_sum, opacity = 0.7, 
            title = "총 이용금액",
            labFormat = labelFormat(suffix = "억 원"),
            position = "bottomright",
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
  addLegend(pal = pal, values = ~NumofSpent_sum, opacity = 0.7, 
            title = "총 이용건수",
            labFormat = labelFormat(suffix = "건"),
            position = "bottomright",
            group = "읍면동별_총이용건수") %>%
  
  addHeatmap(data = jeju_emd,
             lng = ~long, lat = ~lat,
             intensity = ~NumofSpent_sum, blur = 30, max = 1000000000, radius = 20,
             group = "총 이용건수 히트맵") %>%
  
  addLayersControl(
    baseGroups = c("OSM(default)"),
    overlayGroups = c("Jejusi_Top10", "Seogui_Top10", "읍면동별_총이용금액", "읍면동별_총이용건수", "총 이용건수 히트맵"),
    options = layersControlOptions(collapsed = FALSE)) %>%
  
  hideGroup(c("Jejusi_Top10", "Seogui_Top10", "총 이용건수 히트맵", "읍면동별_총이용건수"))

map
# AIzaSyDq8b6vNJ3jLG6gpgMdxAVyYB0SWIrU0WQ