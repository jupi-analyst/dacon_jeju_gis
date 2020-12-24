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
# ������ ���� �� ��ó��
dat <- rbind(dat5,dat6,dat7,dat8)
str(dat)
dat <- dat %>%
  select(-OBJECTID, -Field1)

dat <- dat %>%
  mutate(timeName = ifelse(Time == "02��" | Time == "03��" | Time == "04��" | Time == "05��", "����",
                           ifelse(Time == "06��" | Time == "07��" | Time == "08��" | Time == "09��" | Time == "10��" | 
                                    Time == "11��" , "����",
                                  ifelse(Time == "12��" | Time == "13��" | Time == "14��", "����",
                                         ifelse(Time == "15��" | Time == "16��" | Time == "17��" , "����", 
                                                ifelse(Time == "18��" | Time == "19��" | Time == "20��" | Time == "21��", "����",
                                                       ifelse(Time == "22��" | Time == "23��" | Time == "00��" | Time == "01��", "�ɾ�", "������")))))))
factor(dat$timeName)
dat$timeName <- factor(dat$timeName, levels = c('����','����','����','����','�ɾ�','������'))
# ��ǥ�躯ȯ 
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
# write.csv(df_5, '5��_�̿�ݾ�+�̿�Ǽ�_����.csv')
# write.csv(dat, 'KRI_DAC_Jeju.csv')

# ���ֽ� vs �������� �� �̿�ݾ�
dat %>%
  group_by(SIGUNGU) %>% 
  summarise(TotalSpent_sum = sum(TotalSpent) / 100000000,
            DisSpent_sum = sum(DisSpent) / 100000000) %>%
  mutate(DisSpent_ratio = round((DisSpent_sum / TotalSpent_sum),3)  * 100) %>%
  ggplot(aes(x = "", y = TotalSpent_sum, fill = SIGUNGU)) +
  geom_bar(stat='identity', color = 'white') +
  coord_polar(theta = "y")

# 5~8������  ������ �̿�ݾ� ���� TOP10(�糭����������)
dat %>%
  group_by(Type) %>%
  summarise(sum = sum(TotalSpent)) %>%
  arrange(desc(sum)) %>%
  head(10) %>%
  mutate(sum = sum / 100000000) %>%
  ggplot(aes(x = reorder(Type, sum), y=sum)) +
  geom_bar(stat='identity') +
  coord_flip() + theme_bw() +
  ggtitle("5~8�� ������ �̿�ݾ� ���� TOP10(�糭����������)") +
  labs(x = "����", y = "�̿�ݾ�(��)") +
  theme(plot.title = element_text(size = 14, hjust = 0.5, family = 'NanumGothic', face = 'bold'))

##�Ϲ��ѽ�, ���۸���, ������, �鼼��, ������, ��������, ��������������, ���ƿ�, ����������, ���������

# 5~8������ �� �̿�ݾ�(����������)�� ���� ū ���� TOP10
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
  ggtitle("5~8�� �� �̿�ݾ� TOP10 ����(����������)") +
  labs(x = "����", y = "�̿�ݾ�(��)") +
  theme(plot.title = element_text(size = 13, hjust = 0.5, family = 'NanumGothic', face = 'bold'),
        axis.title = element_text(size = 11, family = 'NanumGothic', face = 'bold'))

# ������ �� �̿�Ǽ�
dat %>%
  group_by(Type) %>%
  summarise(NumofSpent_sum = sum(NumofSpent)) %>%
  arrange(desc(NumofSpent_sum))

# ������ �糭������ �̿�Ǽ�
dat %>%
  group_by(Type) %>%
  summarise(NumofDisSpent_sum = sum(NumofDisSpent)) %>%
  arrange(desc(NumofDisSpent_sum))

# �糭������ �̿�ݾ� TOP10 ����
dat %>%
  group_by(Type) %>%
  summarise(sum = sum(DisSpent)) %>%
  arrange(desc(sum)) %>%
  head(10) %>%
  mutate(sum = sum / 100000000) %>%
  ggplot(aes(x = reorder(Type, sum), y=sum)) +
  geom_bar(stat='identity') +
  coord_flip() + theme_bw() +
  ggtitle("5~8�� �� �糭������ �̿�ݾ� TOP10 ����") +
  labs(x = "����", y = "�̿�ݾ�(��)") +
  scale_y_continuous(breaks = seq(0,70, 10)) +
  theme(plot.title = element_text(size = 13, hjust = 0.5, family = 'NanumGothic', face = 'bold'),
        axis.title = element_text(size = 11, family = 'NanumGothic', face = 'bold'))

# �һ���� ����
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

#�һ����Ÿ�Ժ� �̿�ݾ�
dat %>%
  group_by(FranClass) %>%
  summarise(TotalSpent_sum = sum(TotalSpent) / 100000000,
            DisSpent_sum = sum(DisSpent) / 100000000) %>% 
  mutate(Spent_sum = TotalSpent_sum - DisSpent_sum) %>% 
  melt(id.vars = 'FranClass', measure.vars = c('TotalSpent_sum', 'DisSpent_sum', 'Spent_sum')) %>%
  ggplot(aes(x = FranClass, y = value, fill = variable)) + 
  geom_bar(stat='identity', position = 'dodge', color = 'black') +
  theme_bw()


#�ð��뺰 �̿�ݾ�
dat %>%
  filter(Time != 'x��') %>%
  group_by(YM, Time) %>%
  summarise(TotalSpent_sum = round(sum(TotalSpent) / 100000000,1)) %>%
  ggplot(aes(x = Time, y = TotalSpent_sum, group = as.character(YM), color = as.character(YM))) +
  geom_line(size = 1) +
  scale_color_discrete(name="����") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.3, hjust = 0.5))

dat %>%
  filter(Time != 'x��') %>%
  mutate(Type = factor(Type)) %>%
  group_by(Type, Time) %>%
  summarise(TotalSpent_sum = round(sum(TotalSpent) / 100000000,1)) %>%
  arrange(Type, TotalSpent_sum) %>% 
  ggplot(aes(x = Time, y = TotalSpent_sum, group = as.character(Type), color = as.character(Type))) +
  geom_line(size = 1) +
  scale_color_discrete(name="����") +
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
  filter(Type == '�Ϲ��ѽ�' | Type == '���۸���' |Type == '�鼼��' | Type == '������' | Type == '��������' | 
           Type == '��������������' | Type == '���ƿ�' | Type == '����������' | Type == '���������' | Type == '������') %>%
  group_by(YM, Type) %>%
  summarise(sum = sum(TotalSpent) / 100000000) %>%
  ggplot(aes(x = YM, y = sum, fill = Type)) +
  geom_bar(stat='identity', position = 'dodge', color = 'black')

# ���� ���̿�ݾ� ��� �糭�����ݻ��ݾ� ���� (1�� �糭������ 4~5��)
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

#�̿�ݾ��� ������ ���� ���� ��� TOP10(��ü)
dat %>%
  group_by(Type,long,lat) %>%
  summarise(sum = round(sum(TotalSpent) / 100000000),2) %>% 
  arrange(desc(sum)) %>% head(10)  %>%
  leaflet() %>%
  addTiles() %>%
  addMarkers(lng = ~long, lat = ~lat, label = ~htmlEscape(Type), popup = ~paste0("��",as.character(sum),"���"))

#�̿�ݾ��� ������ ���� ���� ��� TOP10 (���ֽ�)
dat %>%
  filter(SIGUNGU == '���ֽ�') %>%
  group_by(Type,long,lat) %>%
  summarise(TotalSpent_sum = round(sum(TotalSpent) / 100000000,2),
            NumofSpent_sum = sum(NumofSpent)) %>% 
  arrange(desc(TotalSpent_sum, NumofSpent_sum)) %>% head(10) -> total_jejusi_top10_type
total_jejusi_top10_type %>%
  leaflet() %>%
  addTiles() %>%
  addMarkers(lng = ~long, lat = ~lat, 
             label = ~htmlEscape(Type), 
             popup = ~paste0("��",as.character(sum),"���"))

#�̿�ݾ��� ������ ���� ���� ��� TOP10 (��������)
dat %>%
  filter(SIGUNGU == '��������') %>%
  group_by(Type,long,lat) %>%
  summarise(sum = round(sum(TotalSpent) / 100000000),2) %>% 
  arrange(desc(sum)) %>% head(10) -> total_seogui_top10_type
total_seogui_top10_type %>%
  leaflet() %>%
  addTiles() %>%
  addMarkers(lng = ~long, lat = ~lat, 
             label = ~htmlEscape(Type), 
             popup = ~paste0("��",as.character(sum),"���"))

#�̿�ݾ��� ������ ���� ���� ��� TOP10 (���ֽ�)
dat %>%
  filter(SIGUNGU == '���ֽ�' & FranClass == '����') %>%
  group_by(Type,long,lat) %>%
  summarise(TotalSpent_sum = round(sum(TotalSpent) / 100000000,2),
            NumofSpent_sum = sum(NumofSpent)) %>%
  arrange(desc(sum)) %>% head(10) %>%
  leaflet() %>%
  addTiles() %>%
  addMarkers(lng = ~long, lat = ~lat, label = ~htmlEscape(Type), popup = ~paste0("��",as.character(sum),"���"))


# ���鵿 shp �ҷ�����
emd_may <- read.csv('���鵿_5��_����.csv')
emd_may %>% 
  mutate(EMD_CD = as.character(EMD_CD)) %>%
  group_by(EMD_CD, EMD_KOR_NM, val) %>%
  summarise(TotalSpent_sum = round(sum(TotalSpent) / 100000000,2),
            DisSpent_sum = round(sum(DisSpent_s) / 100000000,2),
            NumofSpent_sum = sum(NumofSpent),
            NumofDisSpent_sum = sum(NumofDisSp)) -> emd_may_total

jeju_emd <- readOGR('�������鵿.shp')
to.crs = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs") # ��ǥ��ȯ
jeju_emd <- spTransform(jeju_emd, to.crs)

jeju_emd@data <- left_join(jeju_emd@data, emd_may_total)

pal <- colorBin("YlOrRd", domain = jeju_emd@data$TotalSpent_sum)
labels <- sprintf(
  "<strong>%s</strong><br/>�� %g�� �� <br>�� �̿�Ǽ�: %g <br>���갡���α�: %g",
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
  "<strong>%s</strong><br/>�� %g�� �� <br>�� �̿�Ǽ�: %g <br>���갡���α�: %g",
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
              group = "���鵿��_���̿�ݾ�",
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
            title = "�� �̿�ݾ�",
            labFormat = labelFormat(suffix = "�� ��"),
            position = "bottomright",
            group = "���鵿��_���̿�ݾ�") %>%
  
  addPolygons(data = jeju_emd,
              group = "���鵿��_���̿�Ǽ�",
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
            title = "�� �̿�Ǽ�",
            labFormat = labelFormat(suffix = "��"),
            position = "bottomright",
            group = "���鵿��_���̿�Ǽ�") %>%
  
  addHeatmap(data = jeju_emd,
             lng = ~long, lat = ~lat,
             intensity = ~NumofSpent_sum, blur = 30, max = 1000000000, radius = 20,
             group = "�� �̿�Ǽ� ��Ʈ��") %>%
  
  addLayersControl(
    baseGroups = c("OSM(default)"),
    overlayGroups = c("Jejusi_Top10", "Seogui_Top10", "���鵿��_���̿�ݾ�", "���鵿��_���̿�Ǽ�", "�� �̿�Ǽ� ��Ʈ��"),
    options = layersControlOptions(collapsed = FALSE)) %>%
  
  hideGroup(c("Jejusi_Top10", "Seogui_Top10", "�� �̿�Ǽ� ��Ʈ��", "���鵿��_���̿�Ǽ�"))

map
# AIzaSyDq8b6vNJ3jLG6gpgMdxAVyYB0SWIrU0WQ