library(tidyverse)
library(raster)
library(data.table)
library(maps)
library(rgdal)
library(rgeos)

# 1. 데이터 전처리

## 1.1 지도 파일 전처리

map <- # 대륙별 세계지도 shp 불러오기
  readOGR('4a7d27e1-84a3-4d6a-b4c2-6b6919f3cf4b202034-1-2zg7ul.ht5ut.shp')
map <- fortify(map) # shp파일 데이터프레임화

center <- gCentroid(map, byid=T) # gCentroid 이용 대륙별 중심 위치 저장
center <- center@coords

continent = map$id %>% data.frame # id를 대륙 이름으로 변경
continent <- ifelse(continent=='0', 'Africa', 
             ifelse(continent=='1', 'Asia',
                    ifelse(continent=='2', 'Oceania',
                           ifelse(continent=='3', 'NA',
                                  ifelse(continent=='4', 'Oceania',
                                         ifelse(continent=='5', 'SA',
                                                ifelse(continent =='7', 'Europe', NA)))))))


map <- cbind(map,continent) # 대륙 이름 붙여줌
map <- map %>% rename(continent = '.')

## 1.2 맥주 데이터셋 전처리

data <- fread('wine21_데이터셋_apv_price_수정.csv')

data <- data %>% 
  dplyr::select(-V1) %>% 
  group_by(region_kor) %>% 
  summarise(count=n()) 

con = c('Europe','Oceania','Asia','Asia','Europe','Europe','Asia','Europe','NA','Europe','NA','Asia','Europe','Asia','Europe','Europe','Europe','Asia','SA','Europe','Europe','Europe','Europe','Europe','Asia','Asia','Asia','Asia','Europe','NA','Asia','Asia',
        'Europe','Europe','Europe','Europe','Asia','Oceania')

data <- data %>% 
  mutate(con=con) # 나라별 대륙이름 적어줌

data <- data %>% 
  group_by(con) %>%  # 대륙별로 그룹화
  summarise(count=sum(count)) %>% 
  mutate(per = count / sum(count) * 100) # 대륙별로 맥주 비율 어떻게 되는지 

data[6,] = list('Africa', 0, 0) # Africa 없길래 추가

center <- center %>% # 처음에 저장한 중심위치
  data.frame %>% # 데이터 프레임화
  mutate(con = c('Africa','Asia','Oceania','NA',NA,'SA',NA,'Europe')) # 대륙이름 저장

data <- left_join(data,center,by='con') # data에 대륙별 중심위치 넣어줌

## 1.3 데이터셋 합쳐주기

map <- left_join(map, data, by=c('continent'='con'))

new_map <- map %>% 
  dplyr::select(continent)

new_map <- merge(new_map,data, by.x= 'continent', by.y = 'con') %>% unique()


# 2. 대망의 시각화

map %>% 
  na.omit() %>% # 남극 제거
  ggplot() + 
  geom_polygon(aes(x=long, y=lat,group=group, fill=per))+ # 세계지도 만들어주기
  theme_void() + 
  scale_fill_gradient(low = "#F9CA00", high = "#7D2E03", 
                      space = "Lab", guide = "colourbar") +
  theme(legend.position = 'none') 
  geom_text(data=new_map, # 대륙별 퍼센트 텍스트로 넣어줌
            aes(x=x, y=y,
                label = paste(continent,paste(round(per,2),'%'),sep='\n')))
