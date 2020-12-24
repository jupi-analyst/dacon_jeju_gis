# 작업디렉토리 설정, 필요한 라이브러리 설치 및 로드
getwd()
setwd("C:/Users/USER/Documents/sh_R/jeju_gis")
library(tidyverse)
library(extrafont)
library(ggplot2)
library(RColorBrewer)
library(scales)
library(gridExtra)
library(reshape2)
library(leaflet)
library(leaflet.extras)
library(htmltools)
library(htmlwidgets)
library(rgdal)
library(sp)
#font_import(pattern = 'NanumGothic')
loadfonts()
loadfonts(dev="win")
windowsFonts()
theme_update(text=element_text(family="NanumGothic"))

# 데이터 로드

options(digits = 10)
dat5 <- read.table('KRI-DAC_Jeju_data5.txt', fileEncoding = 'UTF-8', sep = ',', header = TRUE)
dat6 <- read.table('KRI-DAC_Jeju_data6.txt', fileEncoding = 'UTF-8', sep = ',', header = TRUE)
dat7 <- read.table('KRI-DAC_Jeju_data7.txt', fileEncoding = 'UTF-8', sep = ',', header = TRUE)
dat8 <- read.table('KRI-DAC_Jeju_data8.txt', fileEncoding = 'UTF-8', sep = ',', header = TRUE)
dat7 %>%
  select(-X, -Y) -> dat7

# 데이터 정제 및 전처리

dat <- rbind(dat5,dat6,dat7,dat8)
dat$YM <- as.character(dat$YM)
dat <- dat %>%
  select(-OBJECTID, -Field1)

# dat %>%
#   mutate(SIGUNGU = ifelse(SIGUNGU == '제주시', 'jeju-si', 'seoguipo-si')) -> dat
dat <- dat %>%
  mutate(timeName = ifelse(Time %in% c('03시','04시','05시', '06시'), '새벽', 
                           ifelse(Time %in% c('07시','08시','09시','10시','11시'), '오전',
                                  ifelse(Time %in% c('12시','13시','14시'), '점심', 
                                         ifelse(Time %in% c('15시','16시','17시','18시'), '오후',
                                                ifelse(Time %in% c('19시','20시','21시', '22시'), '저녁',
                                                       ifelse(Time  %in% c('23시','00시','01시','02시'), '심야', '무승인')))))))
## 좌표계변환 
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
from.crs = "+proj=tmerc +lat_0=38 +lon_0=127.5 +k=0.9996 +x_0=1000000 +y_0=2000000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
# from.crs = "+proj=tmerc +lat_0=38 +lon_0=127.5 +k=0.9996 +x_0=1000000 +y_0=2000000 +ellps=GRS80 +units=m +no_defs" 
to.crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs" # 위경도 좌표계
coord <- convertCoordSystem(dat$POINT_X, dat$POINT_Y, from.crs, to.crs)
dat <- cbind(dat, coord)
rm(coord); rm(from.crs); rm(to.crs); rm(convertCoordSystem);
rm(dat5);rm(dat6);rm(dat7);rm(dat8);

# write.csv(dat, 'KRI_DAC_Jeju.csv') # 데 이터 저장
dat <- read.csv('C:/Users/USER/Documents/sh_R/jeju_gis/KRI_DAC_Jeju.csv')

dat$timeName <- factor(dat$timeName, levels = c('새벽','오전','점심','오후','저녁','심야','무승인'))
# EDA
## 제주시, 서귀포시 5~8월까지 총 이용금액 비교

dat %>%
  group_by(SIGUNGU) %>% 
  summarise(TotalSpent_sum = round(sum(TotalSpent) / 100000000,1),
            DisSpent_sum = round(sum(DisSpent) / 100000000,1)) %>%
  mutate(DisSpent_ratio = round((DisSpent_sum / TotalSpent_sum),3)  * 100) %>%
  ggplot(aes(x = SIGUNGU, y = TotalSpent_sum, fill = SIGUNGU)) +
  geom_bar(stat='identity', color = 'black', width = 0.5) +
  geom_text(aes(label=TotalSpent_sum), vjust=-0.4, size = 3.5, family = 'NanumGothic', fontface = 'bold') +
  scale_fill_manual(values = c('skyblue', 'red')) + 
  theme_bw() +
  ggtitle("제주시, 서귀포시 5~8월까지 총 이용금액 비교") +
  labs(x = "시군구", y = "총 이용금액(억 원)") +
  theme(plot.title = element_text(size = 14, hjust = 0.5, family = 'NanumGothic', face = 'bold'),
        axis.title = element_text(size = 11, family = 'NanumGothic', face = 'bold'),
        legend.position = 'none')

## 시별 월별 총 이용금액 비교
dat %>%
  group_by(SIGUNGU, YM) %>% 
  summarise(TotalSpent_sum = round(sum(TotalSpent) / 100000000,1),
            DisSpent_sum = round(sum(DisSpent) / 100000000,1)) %>%
  mutate(DisSpent_ratio = round((DisSpent_sum / TotalSpent_sum),3)  * 100) %>%
  ggplot(aes(x = YM, y = TotalSpent_sum, fill = SIGUNGU)) +
  geom_bar(stat='identity', position = 'dodge', color = 'black') +
  scale_fill_brewer(palette = 'Blues') +
  geom_text(aes(label=TotalSpent_sum), vjust=-0.4, size = 3, 
            position=position_dodge(.85),
            family = 'NanumGothic', fontface = 'bold') +
  theme_bw() +
  ggtitle("시별 월별 총 이용금액 비교") +
  labs(x = "월", y = "총 이용금액(억 원)") + 
  lims(y = c(0,2000)) +
  theme(plot.title = element_text(size = 14, hjust = 0.5, family = 'NanumGothic', face = 'bold'),
        axis.title = element_text(size = 11, family = 'NanumGothic', face = 'bold'))

# qgis을 이용해서 데이터를 처리함. 
emd_total_df <- read.csv('total_spent-num_emd.csv')
emd_total_df %>%
  mutate(EMD_CD = as.character(EMD_CD)) %>%
  group_by(EMD_CD, EMD_KOR_NM) %>%
  summarise(TotalSpent_sum = sum(TotalSpent),
            DisSpent_sum = sum(DisSpent_s),
            NumofSpent_sum = sum(NumofSpent),
            NumofDisSpent_sum = sum(NumofDisSp)) -> emd_sum_df

emd_sum_df %>% arrange(desc(TotalSpent_sum)) %>% head(10) %>%
  ggplot(aes(x = reorder(EMD_KOR_NM, TotalSpent_sum), y = TotalSpent_sum / 100000000, fill = TotalSpent_sum)) +
  geom_bar(stat = 'identity') +
  coord_flip() + theme_bw() +
  ggtitle("읍면동별 총 이용금액") +
  labs(x = "", y = "총 이용금액(억 원)") + 
  lims(y = c(0,800)) +
  theme(plot.title = element_text(size = 13, hjust = 0.5, family = 'NanumGothic', face = 'bold'),
        axis.title = element_text(size = 10, family = 'NanumGothic', face = 'bold'),
        axis.text.y = element_text(size = 9),
        legend.position = 'none')

# 제주시 읍면동 총 이용금액
emd_sum_df %>%
  filter(substr(EMD_CD, 1, 4) == '5011') %>%
  ggplot(aes(x = reorder(EMD_KOR_NM, TotalSpent_sum), y = TotalSpent_sum / 100000000, fill = TotalSpent_sum)) +
  geom_bar(stat = 'identity') +
  coord_flip() + theme_bw() +
  ggtitle("제주시 읍면동 총 이용금액") +
  labs(x = "", y = "총 이용금액(억 원)") + 
  lims(y = c(0,800)) +
  theme(plot.title = element_text(size = 13, hjust = 0.5, family = 'NanumGothic', face = 'bold'),
        axis.title = element_text(size = 10, family = 'NanumGothic', face = 'bold'),
        axis.text.y = element_text(size = 7),
        legend.position = 'none')

# 서귀포시 읍면동 총 이용금액
emd_sum_df %>%
  filter(substr(EMD_CD, 1, 4) == '5013') %>%
  ggplot(aes(x = reorder(EMD_KOR_NM, TotalSpent_sum), y = TotalSpent_sum / 100000000, fill = TotalSpent_sum)) +
  geom_bar(stat = 'identity') +
  coord_flip() + theme_bw() +
  ggtitle("서귀포시 읍면동 총 이용금액") +
  labs(x = "", y = "총 이용금액(억 원)") + 
  lims(y = c(0,300)) +
  theme(plot.title = element_text(size = 13, hjust = 0.5, family = 'NanumGothic', face = 'bold'),
        axis.title = element_text(size = 10, family = 'NanumGothic', face = 'bold'),
        axis.text.y = element_text(size = 7),
        legend.position = 'none')

## 월별 총 이용금액 비교
dat %>%
  group_by(YM) %>% 
  summarise(TotalSpent_sum = round(sum(TotalSpent) / 100000000,1),
            DisSpent_sum = round(sum(DisSpent) / 100000000,1)) %>%
  mutate(DisSpent_ratio = round((DisSpent_sum / TotalSpent_sum),3)  * 100) %>%
  ggplot(aes(x = as.numeric(YM), y = TotalSpent_sum)) +
  geom_bar(stat='identity', color = 'black', width = 0.5, fill = 'skyblue') +
  geom_line() + geom_point() +
  geom_text(aes(label=TotalSpent_sum), vjust=-0.4, size = 3.5, family = 'NanumGothic', fontface = 'bold') +
  theme_bw() +
  ggtitle("월별 총 이용금액 비교") +
  labs(x = "월", y = "총 이용금액(억 원)") + 
  lims(y = c(0,2000)) +
  theme(plot.title = element_text(size = 14, hjust = 0.5, family = 'NanumGothic', face = 'bold'),
        axis.title = element_text(size = 11, family = 'NanumGothic', face = 'bold'),
        legend.position = 'none')

# 월별 시간별 이용금액
dat %>%
  filter(Time != 'x시') %>%
  group_by(YM, Time) %>%
  summarise(TotalSpent_sum = round(sum(TotalSpent) / 100000000,1)) %>%
  ggplot(aes(x = Time, y = TotalSpent_sum, group = as.character(YM), color = as.character(YM))) +
  geom_line(size = 1) +
  scale_x_discrete(breaks = c('00시', '03시','06시','09시', '12시','15시','18시', '21시', '23시')) + 
  scale_color_discrete(name="월별") +
  ggtitle("월별 시간별 총 이용금액") +
  labs(x = "시간", y = "총 이용금액(억 원)") +
  theme_bw() +
  theme(plot.title = element_text(size = 14, hjust = 0.5, family = 'NanumGothic', face = 'bold'),
        axis.title = element_text(size = 11, family = 'NanumGothic', face = 'bold'),
        axis.text.x = element_text(angle = 90, vjust = 0.3, hjust = 0.5))

# 시간대별 이용금액
dat %>%
  group_by(timeName) %>%
  summarise(TotalSpent_sum = round(sum(TotalSpent) / 100000000),1) %>%
  ggplot(aes(x = timeName, y = TotalSpent_sum, fill = timeName)) +
  geom_bar(stat='identity', color = 'black') +
  geom_text(aes(label=TotalSpent_sum), vjust=-0.4, size = 3, 
            position=position_dodge(.85),
            family = 'NanumGothic', fontface = 'bold') +
  theme_bw() +
  scale_fill_brewer(palette = 'Spectral') +
  ggtitle("시간대별 총 이용금액") +
  labs(x = "시간대", y = "총 이용금액(억 원)") +
  theme(plot.title = element_text(size = 14, hjust = 0.5, family = 'NanumGothic', face = 'bold'),
        axis.title = element_text(size = 11, family = 'NanumGothic', face = 'bold'),
        legend.position = 'none')

# 소상공인별 숫자
dat %>%
  group_by(FranClass) %>%
  summarise(n = n()) %>%
  ggplot(aes(x = FranClass, y = n, fill = FranClass)) +
  geom_bar(stat='identity', color = 'black') +
  theme_bw() +
  scale_fill_brewer(palette = 'Spectral') +
  geom_text(aes(label=n), vjust= 0.5, size = 3, 
            position=position_dodge(.85),
            family = 'NanumGothic', fontface = 'bold') +
  theme_bw() +
  ggtitle("소상공인별 개수") +
  labs(x = "소상공인", y = "총 이용금액(억 원)") + 
  theme(plot.title = element_text(size = 14, hjust = 0.5, family = 'NanumGothic', face = 'bold'),
        axis.title = element_text(size = 11, family = 'NanumGothic', face = 'bold'),
        legend.position = 'none') -> p1

# 소상공인별 총 이용금액
dat %>%
  group_by(FranClass) %>%
  summarise(TotalSpent_sum = round(sum(TotalSpent) / 100000000,1)) %>%
  ggplot(aes(x = FranClass, y = TotalSpent_sum, fill = FranClass)) +
  geom_bar(stat='identity', color = 'black') +
  theme_bw() +
  scale_fill_brewer(palette = 'Spectral') +
  geom_text(aes(label=TotalSpent_sum), vjust=0.5, size = 3, 
            position=position_dodge(.85),
            family = 'NanumGothic', fontface = 'bold') +
  theme_bw() +
  ggtitle("소상공인별 총 이용금액") +
  labs(x = "소상공인", y = "총 이용금액(억 원)") + 
  theme(plot.title = element_text(size = 14, hjust = 0.5, family = 'NanumGothic', face = 'bold'),
        axis.title = element_text(size = 11, family = 'NanumGothic', face = 'bold'),
        legend.position = 'none') -> p2

grid.arrange(p1,p2)

# 소상공인 영세와 일반만 업종별로 비교해보자
dat %>%
  filter(FranClass == '영세') %>%
  group_by(Type) %>%
  summarise(TotalSpent_sum = round(sum(TotalSpent) / 100000000,1)) %>%
  arrange(desc(TotalSpent_sum)) %>% head(10) %>%
  ggplot(aes(x = reorder(Type, TotalSpent_sum), y = TotalSpent_sum, fill = TotalSpent_sum)) +
  geom_bar(stat='identity', color = 'black') +
  theme_bw() +
  coord_flip() +
  ggtitle("영세 소상공인 업종별 총 이용금액") +
  labs(x = "영세", y = "총 이용금액(억 원)") + 
  theme(plot.title = element_text(size = 14, hjust = 0.5, family = 'NanumGothic', face = 'bold'),
        axis.title = element_text(size = 11, family = 'NanumGothic', face = 'bold'),
        legend.position = 'none') -> p1

dat %>%
  filter(FranClass == '일반') %>%
  group_by(Type) %>%
  summarise(TotalSpent_sum = round(sum(TotalSpent) / 100000000,1)) %>%
  arrange(desc(TotalSpent_sum)) %>% head(10) %>%
  ggplot(aes(x = reorder(Type, TotalSpent_sum), y = TotalSpent_sum, fill = TotalSpent_sum)) +
  geom_bar(stat='identity', color = 'black') +
  theme_bw() +
  coord_flip() +
  ggtitle("일반(대형) 소상공인 업종별 총 이용금액") +
  labs(x = "일반", y = "총 이용금액(억 원)") + 
  theme(plot.title = element_text(size = 14, hjust = 0.5, family = 'NanumGothic', face = 'bold'),
        axis.title = element_text(size = 11, family = 'NanumGothic', face = 'bold'),
        legend.position = 'none') -> p2

grid.arrange(p1,p2)

## 5~8월 총 이용금액 TOP10 업종
dat %>%
  group_by(Type) %>% 
  summarise(TotalSpent_sum = round(sum(TotalSpent) / 100000000,1),
            DisSpent_sum = round(sum(DisSpent) / 100000000,1)) %>%
  mutate(DisSpent_ratio = round((DisSpent_sum / TotalSpent_sum),3)  * 100) %>%
  arrange(desc(TotalSpent_sum)) %>% head(10) %>%
  ggplot(aes(x = reorder(Type, TotalSpent_sum), y = TotalSpent_sum, fill = TotalSpent_sum)) +
  geom_bar(stat='identity', color = 'black') +
  geom_text(aes(label=TotalSpent_sum), vjust= -0.8,size=3,
            fontface = 'bold', angle = 270) +
  coord_flip() + 
  theme_bw() +
  ggtitle("5~8월 총 이용금액 TOP10 업종") +
  labs(x = "월", y = "총 이용금액(억 원)") + 
  lims(y = c(0,2000)) +
  theme(plot.title = element_text(size = 14, family = 'NanumGothic', hjust = 0.5,face = 'bold'),
        axis.title = element_text(size = 11,family = 'NanumGothic', face = 'bold'),
        legend.position = 'none')

## 5~8월 총 이용건수 TOP10 업종
dat %>%
  group_by(Type) %>% 
  summarise(NumofSpent_sum = sum(NumofSpent)) %>%
  arrange(desc(NumofSpent_sum)) %>% head(10) %>%
  ggplot(aes(x = reorder(Type, NumofSpent_sum), y = NumofSpent_sum, fill = NumofSpent_sum)) +
  geom_bar(stat='identity', color = 'black') +
  geom_text(aes(label=NumofSpent_sum, vjust= -0.8),
            size = 3,
            fontface = 'bold', angle = 270) +
  coord_flip() + 
  theme_bw() +
  ggtitle("5~8월 총 이용금액 TOP10 업종") +
  labs(x = "", y = "총 이용금액(억 원)") +
  theme(plot.title = element_text(size = 14, family = 'NanumGothic', hjust = 0.5,face = 'bold'),
        axis.title = element_text(size = 11,family = 'NanumGothic', face = 'bold'),
        legend.position = 'none')

## 시별 이용금액 TOP10 업종
dat %>%
  group_by(SIGUNGU, Type) %>% 
  filter(SIGUNGU == '제주시') %>%
  summarise(TotalSpent_sum = round(sum(TotalSpent) / 100000000,1),
            DisSpent_sum = round(sum(DisSpent) / 100000000,1)) %>%
  mutate(DisSpent_ratio = round((DisSpent_sum / TotalSpent_sum),3)  * 100) %>%
  arrange(desc(TotalSpent_sum)) %>% head(10) %>%
  ggplot(aes(x = reorder(Type, TotalSpent_sum), y = TotalSpent_sum, fill = TotalSpent_sum)) +
  geom_bar(stat='identity', color = 'black') +
  coord_flip() + 
  theme_bw() +
  ggtitle("5~8월 총 이용금액 TOP10 업종(시별)") +
  labs(x = "제주시", y = "") + 
  lims(y = c(0,1000)) +
  theme(plot.title = element_text(size = 13, hjust = 0.5, family = 'NanumGothic', face = 'bold'),
        axis.title = element_text(size = 11, family = 'NanumGothic', face = 'bold'),
        legend.position = 'none') -> p1

dat %>%
  group_by(SIGUNGU, Type) %>% 
  filter(SIGUNGU == '서귀포시') %>%
  summarise(TotalSpent_sum = round(sum(TotalSpent) / 100000000,1),
            DisSpent_sum = round(sum(DisSpent) / 100000000,1)) %>%
  mutate(DisSpent_ratio = round((DisSpent_sum / TotalSpent_sum),3)  * 100) %>%
  arrange(desc(TotalSpent_sum)) %>% head(10) %>%
  ggplot(aes(x = reorder(Type, TotalSpent_sum), y = TotalSpent_sum, fill = TotalSpent_sum)) +
  geom_bar(stat='identity', color = 'black') +
  coord_flip() + 
  theme_bw() +
  labs(x = "서귀포시", y = "총 이용금액(억 원)") + 
  lims(y = c(0,1000)) +
  theme(axis.title = element_text(size = 11, family = 'NanumGothic', face = 'bold'),
        legend.position = 'none') -> p2
grid.arrange(p1,p2)

## 시간대별 업종 top5
dat %>% 
  group_by(timeName, Type) %>%
  summarise(TotalSpent_sum = round(sum(TotalSpent) / 100000000,1),
            DisSpent_sum = round(sum(DisSpent) / 100000000,1)) %>%
  mutate(DisSpent_ratio = round((DisSpent_sum / TotalSpent_sum),3)  * 100) %>%
  filter(timeName == '오전') %>%
  arrange(desc(TotalSpent_sum)) %>% head(5) %>%
  ggplot(aes(x = reorder(Type, TotalSpent_sum), y = TotalSpent_sum)) +
  geom_bar(stat='identity', color = 'black', width = 0.7, fill = 'skyblue')  +
  theme_bw() +
  ggtitle("시간대별 업종 TOP5") +
  labs(x = "", y = "오전") + 
  theme(plot.title = element_text(size = 14, hjust = 0.5, family = 'NanumGothic', face = 'bold'),
        axis.title = element_text(size = 11, family = 'NanumGothic', face = 'bold'),
        legend.position = 'none') -> p1
dat %>% 
  group_by(timeName, Type) %>%
  summarise(TotalSpent_sum = round(sum(TotalSpent) / 100000000,1),
            DisSpent_sum = round(sum(DisSpent) / 100000000,1)) %>%
  mutate(DisSpent_ratio = round((DisSpent_sum / TotalSpent_sum),3)  * 100) %>%
  filter(timeName == '점심') %>%
  arrange(desc(TotalSpent_sum)) %>% head(5) %>%
  ggplot(aes(x = reorder(Type, TotalSpent_sum), y = TotalSpent_sum)) +
  geom_bar(stat='identity', color = 'black', width = 0.7, fill = 'Orange')  +
  theme_bw() +
  labs(x = "", y = "점심") + 
  theme(plot.title = element_text(size = 14, hjust = 0.5, family = 'NanumGothic', face = 'bold'),
        axis.title = element_text(size = 11, family = 'NanumGothic', face = 'bold')) -> p2
dat %>% 
  group_by(timeName, Type) %>%
  summarise(TotalSpent_sum = round(sum(TotalSpent) / 100000000,1),
            DisSpent_sum = round(sum(DisSpent) / 100000000,1)) %>%
  mutate(DisSpent_ratio = round((DisSpent_sum / TotalSpent_sum),3)  * 100) %>%
  filter(timeName == '오후') %>%
  arrange(desc(TotalSpent_sum)) %>% head(5) %>%
  ggplot(aes(x = reorder(Type, TotalSpent_sum), y = TotalSpent_sum)) +
  geom_bar(stat='identity', color = 'black', width = 0.7, fill = 'Red')  +
  theme_bw() +
  labs(x = "", y = "오후") + 
  theme(plot.title = element_text(size = 14, hjust = 0.5, family = 'NanumGothic', face = 'bold'),
        axis.title = element_text(size = 11, family = 'NanumGothic', face = 'bold')) -> p3

grid.arrange(p1,p2,p3, nrow = 3)

dat %>% 
  group_by(timeName, Type) %>%
  summarise(TotalSpent_sum = round(sum(TotalSpent) / 100000000,1),
            DisSpent_sum = round(sum(DisSpent) / 100000000,1)) %>%
  mutate(DisSpent_ratio = round((DisSpent_sum / TotalSpent_sum),3)  * 100) %>%
  filter(timeName == '저녁') %>%
  arrange(desc(TotalSpent_sum)) %>% head(5) %>%
  ggplot(aes(x = reorder(Type, TotalSpent_sum), y = TotalSpent_sum)) +
  geom_bar(stat='identity', color = 'black', width = 0.7)  +
  theme_bw() + 
  ggtitle("시간대별 업종TOP5") +
  labs(x = "", y = "저녁") + 
  theme(plot.title = element_text(size = 14, hjust = 0.5, family = 'NanumGothic', face = 'bold'),
        axis.title = element_text(size = 11, family = 'NanumGothic', face = 'bold'))-> p4

dat %>% 
  group_by(timeName, Type) %>%
  summarise(TotalSpent_sum = round(sum(TotalSpent) / 100000000,1),
            DisSpent_sum = round(sum(DisSpent) / 100000000,1)) %>%
  mutate(DisSpent_ratio = round((DisSpent_sum / TotalSpent_sum),3)  * 100) %>%
  filter(timeName == '심야') %>%
  arrange(desc(TotalSpent_sum)) %>% head(5) %>%
  ggplot(aes(x = reorder(Type, TotalSpent_sum), y = TotalSpent_sum)) +
  geom_bar(stat='identity', color = 'black', width = 0.7, fill = 'black')  +
  theme_bw() + 
  labs(x = "", y = "심야") + 
  theme(plot.title = element_text(size = 14, hjust = 0.5, family = 'NanumGothic', face = 'bold'),
        axis.title = element_text(size = 11, family = 'NanumGothic', face = 'bold'))-> p5
dat %>%  
  group_by(timeName, Type) %>%
  summarise(TotalSpent_sum = round(sum(TotalSpent) / 100000000,1),
            DisSpent_sum = round(sum(DisSpent) / 100000000,1)) %>%
  mutate(DisSpent_ratio = round((DisSpent_sum / TotalSpent_sum),3)  * 100) %>%
  filter(timeName == '새벽') %>%
  arrange(desc(TotalSpent_sum)) %>% head(5) %>%
  ggplot(aes(x = reorder(Type, TotalSpent_sum), y = TotalSpent_sum)) +
  geom_bar(stat='identity', color = 'black', width = 0.7, fill = 'Gray')  +
  theme_bw() + 
  labs(x = "", y = "새벽") + 
  theme(plot.title = element_text(size = 14, hjust = 0.5, family = 'NanumGothic', face = 'bold'),
        axis.title = element_text(size = 11, family = 'NanumGothic', face = 'bold'))-> p6

grid.arrange(p4,p5,p6)
rm(p1,p2,p3,p4,p5,p6)






dat_emd <- read.csv("C:/Users/USER/Documents/sh_R/jeju_gis/KRI_DAC_Jeju_EMD.csv")
head(dat_emd)

dat_emd %>%
  group_by(EMD_CD, EMD_KOR_NM, FranClass) %>%
  summarise(n = n()) %>%
  dcast(EMD_CD+EMD_KOR_NM ~ FranClass) -> dat_emd_franclass

dat_emd %>%
  group_by(EMD_CD, EMD_KOR_NM, FranClass) %>%
  summarise(n = n()) %>%
  melt(id.vars = c('EMD_CD', 'EMD_KOR_NM', 'FranClass'), measure.vars = 'n') -> dat_emd_franclass

dat_emd %>%
  group_by(EMD_CD,EMD_KOR_NM, FranClass) %>%
  summarise(TotalSpent_sum = sum(TotalSpent),
            NumofSpent_sum = sum(NumofSpent)) %>%
  melt(id.vars = c('EMD_CD','EMD_KOR_NM', 'FranClass'), measure.vars = 'TotalSpent_sum') -> dat_emd_tS
dat_emd %>%
  group_by(EMD_CD,EMD_KOR_NM, FranClass) %>%
  summarise(TotalSpent_sum = sum(TotalSpent),
            NumofSpent_sum = sum(NumofSpent)) %>%
  melt(id.vars = c('EMD_CD','EMD_KOR_NM', 'FranClass'), measure.vars = 'NumofSpent_sum') -> dat_emd_noS
inner_join(dat_emd_tS, dat_emd_noS, by = c('EMD_CD', 'EMD_KOR_NM', 'FranClass'))

inner_join(jeju_emd@data, dat_emd_franclass)

dat_emd %>%
  group_by(EMD_KOR_NM, Time) %>%
  summarise(TotalSpent_sum = sum(TotalSpent)) %>%
  ggplot(aes(x = Time, y = TotalSpent_sum / 100000000, group = EMD_KOR_NM)) +
  geom_line() + theme_minimal()

dat %>%
  group_by(Time) %>%
  filter(Time != 'x시') %>%
  summarise(TotalSpent_sum = sum(TotalSpent),
            DisSpent_sum = sum(DisSpent),
            NumofSpent_sum = sum(NumofSpent),
            NumofDisSpent_sum = sum(NumofDisSpent)) %>%
  ggplot(aes(x = Time, y = TotalSpent_sum, group = 1)) +
  geom_line()
