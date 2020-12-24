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
loadfonts()
loadfonts(dev="win")
windowsFonts()
theme_update(text=element_text(family="NanumGothic") ?ons(digits = 10)
dat5 <- read.table('KRI-DAC_Jeju_data5.txt', fileEncoding = 'UTF-8', sep = ',', header = TRUE)
dat6 <- read.table('KRI-DAC_Jeju_data6.txt', fileEncoding = 'UTF-8', sep = ',', header = TRUE)
dat7 <- read.table('KRI-DAC_Jeju_data7.txt', fileEncoding = 'UTF-8', sep = ',', header = TRUE)
dat8 <- read.table('KRI-DAC_Jeju_data8.txt', fileEncoding = 'UTF-8', sep = ',', header = TRUE)
dat7 %>%
  select(-X, -Y) -> dat7

# ??at <- rbind(dat5,dat6,dat7,dat8)
dat$YM <- as.character(dat$YM)
dat <- dat %>%
  select(-OBJECTID, -Field1)

dat <- dat %>%
  mutate(timeName = ifelse(Time %in% c('03시','04시','05시', '06시'), '새벽', 
                           ifelse(Time %in% c('07시','08시','09시','10시','11시'), '오전',
                                  ifelse(Time %in% c('12시','13시','14시'), '점심', 
                                         ifelse(Time %in% c('15시','16시','17시','18시'), '오후',
                                                ifelse(Time %in% c('19시','20시','21시', '22시'), '저녁',
                                                       ifelse(Time  %in% c('23시','00시','01시','02시'), '심야', '무승인')))))))

dat$timeName <- factor(dat$timeName, levels = c('새벽','오전','점심','오후','저녁','심야','무승인'))

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
rm(dat5);rm(dat6);rm(dat7);rm( ???????? 5~8?????? ?? ?̿??ݾ? ????
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
  ggtitle("��?ֽ?, ???????? 5~8?????? ?? ?̿??ݾ? ????") +
  labs(x = "?ñ???", y = "?? ?̿??ݾ?(?? ??)") +
  theme(plot.title = element_text(size = 14, hjust = 0.5, family = 'NanumGothic', face = 'bold'),
        axis.title = element_text(size = 11, family = 'NanumGothic', face = 'bold'),
        legend.position = 'none')

## ???? ?? ?̿??ݾ? ????
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
  ggtitle("???? ?? ?̿??ݾ? ????") +
  labs(x = "??", y = "?? ?̿??ݾ?(?? ??)") + 
  lims(y = c(0,2000)) +
  theme(plot.title = element_text(size = 14, hjust = 0.5, family = 'NanumGothic', face = 'bold'),
        axis.title = element_text(size = 11, family = 'NanumGothic', face = 'bold'),
        legend.position = 'none')

## ?ú? ???? ?? ?̿??ݾ? ????
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
  ggtitle("?ú? ???? ?? ?̿??ݾ? ????") +
  labs(x = "??", y = "?? ?̿??ݾ?(?? ??)") + 
  lims(y = c(0,2000)) +
  theme(plot.title = element_text(size = 14, hjust = 0.5, family = 'NanumGothic', face = 'bold'),
        axis.title = element_text(size = 11, family = 'NanumGothic', face = 'bold'))

# ???? ?ð??? ?̿??ݾ?
dat %>%
  filter(Time != 'x??') %>%
  group_by(YM, Time) %>%
  summarise(TotalSpent_sum = round(sum(TotalSpent) / 100000000,1)) %>%
  ggplot(aes(x = Time, y = TotalSpent_sum, group = as.character(YM), color = as.character(YM))) +
  geom_line(size = 1) +
  scale_color_discrete(name="????") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.3, hjust = 0.5))

# ?ð??뺰 ?̿??ݾ?
dat %>%
  group_by(timeName) %>%
  summarise(TotalSpent_sum = round(sum(TotalSpent) / 100000000),1) %>%
  ggplot(aes(x = timeName, y = TotalSpent_sum, fill = timeName)) +
  geom_bar(stat='identity', color = 'black') +
  theme_bw() +
  scale_fill_brewer(palette = 'Spectral') +
  ggtitle("?ð??뺰 ?? ?̿??ݾ?") +
  labs(x = "?ð???", y = "?? ?̿??ݾ?(?? ??)") +
  theme(plot.title = element_text(size = 14, hjust = 0.5, family = 'NanumGothic', face = 'bold'),
        axis.title = element_text(size = 11, family = 'NanumGothic', face = 'bold'),
        legend.position = 'none')

# ?һ????κ? ?? ?̿??ݾ?
dat %>%
  group_by(FranClass) %>%
  summarise(TotalSpent_sum = round(sum(TotalSpent) / 100000000,1)) %>%
  ggplot(aes(x = FranClass, y = TotalSpent_sum, fill = FranClass)) +
  geom_bar(stat='identity', color = 'black') +
  theme_bw() +
  scale_fill_brewer(palette = 'Spectral') +
  geom_text(aes(label=TotalSpent_sum), vjust=-0.5, size = 3, 
            position=position_dodge(.85),
            family = 'NanumGothic', fontface = 'bold') +
  theme_bw() +
  ggtitle("?һ????κ? ?? ?̿??ݾ?") +
  labs(x = "??", y = "?? ?̿??ݾ?(?? ??)") + 
  theme(plot.title = element_text(size = 14, hjust = 0.5, family = 'NanumGothic', face = 'bold'),
        axis.title = element_text(size = 11, family = 'NanumGothic', face = 'bold'),
        legend.position = 'none')

## 5~8?? ?? ?̿??ݾ? TOP10 ??��
dat %>%
  group_by(Type) %>% 
  summarise(TotalSpent_sum = round(sum(TotalSpent) / 100000000,1),
            DisSpent_sum = round(sum(DisSpent) / 100000000,1)) %>%
  mutate(DisSpent_ratio = round((DisSpent_sum / TotalSpent_sum),3)  * 100) %>%
  arrange(desc(TotalSpent_sum)) %>% head(10) %>%
  ggplot(aes(x = reorder(Type, TotalSpent_sum), y = TotalSpent_sum, fill = Type)) +
  geom_bar(stat='identity', color = 'black') +
  scale_fill_brewer(palette = 'Spectral') +
  geom_text(aes(label=TotalSpent_sum), vjust= -0.8,size=3,
            family = 'NanumGothic', fontface = 'bold', angle = 270) +
  coord_flip() + 
  theme_bw() +
  ggtitle("5~8?? ?? ?̿??ݾ? TOP10 ??��") +
  labs(x = "??", y = "?? ?̿??ݾ?(?? ??)") + 
  lims(y = c(0,2000)) +
  theme(plot.title = element_text(size = 14, hjust = 0.5, family = 'NanumGothic', face = 'bold'),
        axis.title = element_text(size = 11, family = 'NanumGothic', face = 'bold'),
        legend.position = 'none')

## ?ú? ?̿??ݾ? TOP10 ??��
dat %>%
  group_by(SIGUNGU, Type) %>% 
  filter(SIGUNGU == '��?ֽ?') %>%
  summarise(TotalSpent_sum = round(sum(TotalSpent) / 100000000,1),
            DisSpent_sum = round(sum(DisSpent) / 100000000,1)) %>%
  mutate(DisSpent_ratio = round((DisSpent_sum / TotalSpent_sum),3)  * 100) %>%
  arrange(desc(TotalSpent_sum)) %>% head(10) %>%
  ggplot(aes(x = reorder(Type, TotalSpent_sum), y = TotalSpent_sum, fill = Type)) +
  geom_bar(stat='identity', color = 'black') +
  scale_fill_brewer(palette = 'Blues') +
  coord_flip() + 
  theme_bw() +
  ggtitle("5~8?? ?? ?̿??ݾ? TOP10 ??��") +
  labs(x = "��?ֽ?", y = "") + 
  lims(y = c(0,1000)) +
  theme(plot.title = element_text(size = 14, hjust = 0.5, family = 'NanumGothic', face = 'bold'),
        axis.title = element_text(size = 11, family = 'NanumGothic', face = 'bold'),
        legend.position = 'none') -> p1

dat %>%
  group_by(SIGUNGU, Type) %>% 
  filter(SIGUNGU == '????????') %>%
  summarise(TotalSpent_sum = round(sum(TotalSpent) / 100000000,1),
            DisSpent_sum = round(sum(DisSpent) / 100000000,1)) %>%
  mutate(DisSpent_ratio = round((DisSpent_sum / TotalSpent_sum),3)  * 100) %>%
  arrange(desc(TotalSpent_sum)) %>% head(10) %>%
  ggplot(aes(x = reorder(Type, TotalSpent_sum), y = TotalSpent_sum, fill = Type)) +
  geom_bar(stat='identity', color = 'black') +
  scale_fill_brewer(palette = 'Blues') +
  coord_flip() + 
  theme_bw() +
  labs(x = "????????", y = "?? ?̿??ݾ?(?? ??)") + 
  lims(y = c(0,1000)) +
  theme(axis.title = element_text(size = 11, family = 'NanumGothic', face = 'bold'),
        legend.position = 'none') -> p2
grid.arrange(p1,p2)

## ?ð??뺰 ??�� top5
dat %>% 
  group_by(timeName, Type) %>%
  summarise(TotalSpent_sum = round(sum(TotalSpent) / 100000000,1),
            DisSpent_sum = round(sum(DisSpent) / 100000000,1)) %>%
  mutate(DisSpent_ratio = round((DisSpent_sum / TotalSpent_sum),3)  * 100) %>%
  filter(timeName == '????') %>%
  arrange(desc(TotalSpent_sum)) %>% head(5) %>%
  ggplot(aes(x = reorder(Type, TotalSpent_sum), y = TotalSpent_sum)) +
  geom_bar(stat='identity', color = 'black', width = 0.7)  +
  theme_bw() +
  ggtitle("?ð??뺰 ??�� TOP5") +
  labs(x = "", y = "????") + 
  theme(plot.title = element_text(size = 14, hjust = 0.5, family = 'NanumGothic', face = 'bold'),
        axis.title = element_text(size = 11, family = 'NanumGothic', face = 'bold'),
        legend.position = 'none') -> p1
dat %>% 
  group_by(timeName, Type) %>%
  summarise(TotalSpent_sum = round(sum(TotalSpent) / 100000000,1),
            DisSpent_sum = round(sum(DisSpent) / 100000000,1)) %>%
  mutate(DisSpent_ratio = round((DisSpent_sum / TotalSpent_sum),3)  * 100) %>%
  filter(timeName == '��??') %>%
  arrange(desc(TotalSpent_sum)) %>% head(5) %>%
  ggplot(aes(x = reorder(Type, TotalSpent_sum), y = TotalSpent_sum)) +
  geom_bar(stat='identity', color = 'black', width = 0.7)  +
  theme_bw() +
  labs(x = "", y = "��??") + 
  theme(plot.title = element_text(size = 14, hjust = 0.5, family = 'NanumGothic', face = 'bold'),
        axis.title = element_text(size = 11, family = 'NanumGothic', face = 'bold')) -> p2
dat %>% 
  group_by(timeName, Type) %>%
  summarise(TotalSpent_sum = round(sum(TotalSpent) / 100000000,1),
            DisSpent_sum = round(sum(DisSpent) / 100000000,1)) %>%
  mutate(DisSpent_ratio = round((DisSpent_sum / TotalSpent_sum),3)  * 100) %>%
  filter(timeName == '????') %>%
  arrange(desc(TotalSpent_sum)) %>% head(5) %>%
  ggplot(aes(x = reorder(Type, TotalSpent_sum), y = TotalSpent_sum)) +
  geom_bar(stat='identity', color = 'black', width = 0.7)  +
  theme_bw() +
  labs(x = "", y = "????") + 
  theme(plot.title = element_text(size = 14, hjust = 0.5, family = 'NanumGothic', face = 'bold'),
        axis.title = element_text(size = 11, family = 'NanumGothic', face = 'bold')) -> p3

grid.arrange(p1,p2,p3, nrow = 3)

dat %>% 
  group_by(timeName, Type) %>%
  summarise(TotalSpent_sum = round(sum(TotalSpent) / 100000000,1),
            DisSpent_sum = round(sum(DisSpent) / 100000000,1)) %>%
  mutate(DisSpent_ratio = round((DisSpent_sum / TotalSpent_sum),3)  * 100) %>%
  filter(timeName == '????') %>%
  arrange(desc(TotalSpent_sum)) %>% head(5) %>%
  ggplot(aes(x = reorder(Type, TotalSpent_sum), y = TotalSpent_sum)) +
  geom_bar(stat='identity', color = 'black', width = 0.7)  +
  theme_bw() + 
  ggtitle("?ð??뺰 ??��TOP5") +
  labs(x = "", y = "????") + 
  theme(plot.title = element_text(size = 14, hjust = 0.5, family = 'NanumGothic', face = 'bold'),
        axis.title = element_text(size = 11, family = 'NanumGothic', face = 'bold'))-> p4

dat %>% 
  group_by(timeName, Type) %>%
  summarise(TotalSpent_sum = round(sum(TotalSpent) / 100000000,1),
            DisSpent_sum = round(sum(DisSpent) / 100000000,1)) %>%
  mutate(DisSpent_ratio = round((DisSpent_sum / TotalSpent_sum),3)  * 100) %>%
  filter(timeName == '?ɾ?') %>%
  arrange(desc(TotalSpent_sum)) %>% head(5) %>%
  ggplot(aes(x = reorder(Type, TotalSpent_sum), y = TotalSpent_sum)) +
  geom_bar(stat='identity', color = 'black', width = 0.7)  +
  theme_bw() + 
  labs(x = "", y = "?ɾ?") + 
  theme(plot.title = element_text(size = 14, hjust = 0.5, family = 'NanumGothic', face = 'bold'),
        axis.title = element_text(size = 11, family = 'NanumGothic', face = 'bold'))-> p5
dat %>%  
  group_by(timeName, Type) %>%
  summarise(TotalSpent_sum = round(sum(TotalSpent) / 100000000,1),
            DisSpent_sum = round(sum(DisSpent) / 100000000,1)) %>%
  mutate(DisSpent_ratio = round((DisSpent_sum / TotalSpent_sum),3)  * 100) %>%
  filter(timeName == '????') %>%
  arrange(desc(TotalSpent_sum)) %>% head(5) %>%
  ggplot(aes(x = reorder(Type, TotalSpent_sum), y = TotalSpent_sum)) +
  geom_bar(stat='identity', color = 'black', width = 0.7)  +
  theme_bw() + 
  labs(x = "", y = "????") + 
  theme(plot.title = element_text(size = 14, hjust = 0.5, family = 'NanumGothic', face = 'bold'),
        axis.title = element_text(size = 11, family = 'NanumGothic', face = 'bold'))-> p6
grid.arrange(p4,p5,p6)
# dat %>% 
#   group_by(timeName, Type) %>%
#   summarise(TotalSpent_sum = round(sum(TotalSpent) / 100000000,1),
#             DisSpent_sum = round(sum(DisSpent) / 100000000,1)) %>%
#   mutate(DisSpent_ratio = round((DisSpent_sum / TotalSpent_sum),3)  * 100) %>%
#   filter(timeName == '??????') %>%
#   arrange(desc(TotalSpent_sum)) %>% head(5) %>%
#   ggplot(aes(x = reorder(Type, TotalSpent_sum), y = TotalSpent_sum)) +
#   geom_bar(stat='identity', color = 'black', width = 0.7)  +
#   theme_bw()

# 10MB?????? ?ȵ? ?? Choro ?????? ��???? ?׷??��? ?׷��???.
library(broom)
library(cartography)
library(RColorBrewer)
## ?????? ?ε?
# leaflet ?ð?ȭ�� ��?? qgis?? R�� ?̿??? ?????? ??ó??
dat <- read.csv('KRI_DAC_Jeju.csv')
dat %>%
  group_by(long, lat) %>%
  summarise(TotalSpent_sum = sum(TotalSpent),
            DisSpent_sum = sum(DisSpent),
            NumofSpent_sum = sum(NumofSpent),
            NumofDisSpent_sum = sum(NumofDisSpent)) -> dat_longlat

dat %>%
  filter(SIGUNGU == '��?ֽ?') %>%
  group_by(Type,long,lat) %>%
  summarise(TotalSpent_sum = round(sum(TotalSpent) / 100000000,2),
            NumofSpent_sum = sum(NumofSpent)) %>% 
  arrange(desc(TotalSpent_sum, NumofSpent_sum)) %>% head(10) -> total_jejusi_top10_type

#?̿??ݾ??? ??��?? ???? ??�� ???? TOP10 (????????)
dat %>%
  filter(SIGUNGU == '????????') %>%
  group_by(Type,long,lat) %>%
  summarise(TotalSpent_sum = round(sum(TotalSpent) / 100000000,2),
            NumofSpent_sum = sum(NumofSpent)) %>% 
  arrange(desc(TotalSpent_sum, NumofSpent_sum)) %>% head(10) -> total_seogui_top10_type

# write.csv(dat_longlat, '5-8??_???̿??ݾ?+?̿??Ǽ?.csv')

## ��?鵿?? ?? ?̿??ݾ? + ?Ǽ? 
emd_total_df <- read.csv('5-8??_???̿??ݾ?+?̿??Ǽ?_��?鵿.csv') # qgis?? ��?鵿?? ?????м??? ????
emd_total_df %>%
  mutate(EMD_CD = as.character(EMD_CD)) %>%
  group_by(EMD_CD, EMD_KOR_NM) %>%
  summarise(TotalSpent_sum = sum(TotalSpent),
            DisSpent_sum = sum(DisSpent_s),
            NumofSpent_sum = sum(NumofSpent),
            NumofDisSpent_sum = sum(NumofDisSp)) -> emd_sum_df

## ��?鵿?? ???갡???α? shp???Ϸε?
jeju_emd_val <- readOGR('vl_blk.shp')
to.crs = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
jeju_emd_val <- spTransform(jeju_emd_val, to.crs)  # ??ǥ??ȯ
jeju_emd_val@data <- jeju_emd_val@data[,c('gid','val')]
jeju_emd_val@data <- rename(jeju_emd_val@data, "EMD_CD" = "gid")

## ��?鵿 shp ???? ?ε?
jeju_emd <- readOGR('��??��?鵿.shp')
to.crs = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
jeju_emd <- spTransform(jeju_emd, to.crs)  # ??ǥ??ȯ
jeju_emd@data
jeju_emd@data <- inner_join(jeju_emd@data, emd_sum_df)
rm(emd_sum_df); rm(emd_total_df); rm(to.crs);

jeju_emd@data <- inner_join(jeju_emd@data, jeju_emd_val@data)
head(jeju_emd@data); rm(jeju_emd_val);

## choro packages
choroLayer(spdf = jeju_emd, df = jeju_emd@data,
           var = 'TotalSpent_sum',
           border = 'black',
           col = carto.pal(pal1 = "sand.pal", n1 = 10),
           legend.pos = "bottomright")

choroLayer(spdf = jeju_emd, df = jeju_emd@data,
           var = 'NumofSpent_sum',
           border = 'black',
           col = carto.pal(pal1 = "sand.pal", n1 = 10),
           legend.pos = "bottomright")

choroLayer(spdf = jeju_emd, df = jeju_emd@data,
           var = 'val',
           border = 'black',
           col = carto.pal(pal1 = "sand.pal", n1 = 10),
           legend.pos = "bottomright")

## ggplot�� ?̿??? ?ð?ȭ
jeju_emd_tidy <- as.data.frame(fortify(jeju_emd))
jeju_emd$id <- row.names(jeju_emd)
jeju_emd_tidy <- left_join(jeju_emd_tidy, jeju_emd@data)

ggplot(jeju_emd_tidy, aes(x = long, y = lat, group = group, fill = TotalSpent_sum / 100000000)) +
  geom_polygon() +
  geom_path(color = "white", size = 0.2) +
  theme_void() -> p 
print(p)
#?̿??ݾ??? ??��?? ???? ??�� ???? TOP10 (��?ֽ?)
labels <- sprintf(
  "<strong>%s</strong><br/>?? %g?? ?? <br>?? ?̿??Ǽ?: %g <br>???갡???α?: %g",
  jeju_emd@data$EMD_KOR_NM, round(jeju_emd@data$TotalSpent_sum / 100000000,2), jeju_emd@data$NumofSpent_sum, jeju_emd@data$val
) %>% lapply(htmltools::HTML)

bins_spent <- c(0, 100, 200, 300, 400, 500, Inf)
bins_num <- c(0, 1000, 5000, 10000, 50000, 100000, 500000, 1000000, Inf)
bins_pop <- c(0, 1000, 5000, 10000, 20000, 30000, Inf)
# qpal <- colorQuantile('YlOrRd', domain = jeju_emd@data$NumofSpent_sum, n = 5 )
pal <- colorBin("YlOrRd", jeju_emd@data$val, bins = bins_pop)
pal2 <- colorBin("YlOrRd", jeju_emd@data$TotalSpent_sum, bins = bins_spent)
pal3 <- colorBin("YlOrRd", jeju_emd@data$NumofSpent_sum, bins = bins_num)

map <- leaflet(jeju_emd) %>%
  addTiles(group = "OSM(default)") %>%
  addMarkers(data = total_jejusi_top10_type,
             lng = ~long, lat = ~lat,
             label = ~htmlEscape(Type), popup = ~paste0("??",as.character(TotalSpent_sum),"????"),
             group = "Jejusi_Top10") %>%
  addMarkers(data = total_seogui_top10_type,
             lng = ~long, la약= ~lat,
             label = ~ht억원cape(Type), popup = ~paste0("??",as.character(TotalSpent_sum),"????"),
             group = 'Seogui_Top10') %>%
  
  addPolygons(data = jeju_emd,
              group = "��?鵿??_???갡?약α?",
              fillColor = 억원(val),
              weight = 1,  opacity = 1, color = "black", dashArray = "1", fillOpacity = 0.7,
  "hlightOptions(
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
  addLegend(pal = pal, values = ~val, opacity = 0.7, 
            title = "???갡???α?",
            labFormat = labelFormat(suffix = "??"),
            position = "bottomleft",
            group = "��?鵿??_???갡???α?") %>%
  
  addPolygons(data = jeju_emd,
              group = "��?鵿??_???̿??ݾ?",
              fillColor = ~pal2(round(TotalSpent_sum / 100000000,2)),
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
  addLegend(pal = pal2, values = ~TotalSpent_sum, opacity = 0.7, 
            title = "?? ?̿??ݾ?",
            labFormat = labelFormat(suffix = "?? ??"),
            position = "bottomleft",
            group = "��?鵿??_???̿??ݾ?") %>%
  
  addPolygons(data = jeju_emd,
              group = "��?鵿??_???̿??Ǽ?",
              fillColor = ~pal3(NumofSpent_sum),
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
  addLegend(pal = pal3, values = ~NumofSpent_sum, opacity = 0.7, 
            title = "?? ?̿??Ǽ?",
            labFormat = labelFormat(suffix = "??"),
            position = "bottomright",
            group = "��?鵿??_???̿??Ǽ?") %>%
  
  addHeatmap(data = dat_longlat,
             lng = ~long, lat = ~lat,
             intensity = ~NumofSpent_sum, blur = 30, max = 100000, radius = 20,
             group = "?? ?̿??Ǽ? ??Ʈ??") %>%
  
  addLayersControl(baseGroups = c("OSM(default)"),
                   overlayGroups = c("Jejusi_Top10", "Seogui_Top10","��?鵿??_???갡???α?","��?鵿??_???̿??ݾ?", "��?鵿??_???̿??Ǽ?", "?? ?̿??Ǽ? ??Ʈ??"),
                   options = layersControlOptions(collapsed = FALSE)) %>%
  hideGroup(c("Jejusi_Top10", "Seogui_Top10", "��?鵿??_???갡???α?", "?? ?̿??Ǽ? ??Ʈ??", "��?鵿??_???̿??Ǽ?"))

map
saveWidget(map, file = 'dacon_jeju.html')
