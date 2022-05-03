library(tidyverse)
library(tidytext)
library(stringr)
library(textdata)
library(data.table)

# 1. 데이터 전처리

data = read_csv('beer_review.csv')

beer = data %>% select(name_eng, review_eng) # 필요한 것만 쏙쏙
beer$review_eng <- beer$review_eng %>% str_split('], | ] ,') # 리뷰 별로 나눠줘


beer_review <- tibble() # 리뷰별 단어로 나눠줌

for(i in seq_along(beer$name_eng)) {
  
  clean <- tibble(review = seq_along(beer$review_eng[[i]]), 
                  text = beer$review_eng[[i]]) %>%
    unnest_tokens(word, text) %>%  # 리뷰 문장 단어로 나눠줌
    mutate(name = beer$name_eng[i]) %>%
    select(name, everything())
  
  beer_review <- rbind(beer_review, clean)
}


beer_review0 <- tibble() # 단어로 나누지 않고 리뷰 문장으로 남김

for(i in seq_along(beer$name_eng)) {
  
  clean <- tibble(review = seq_along(beer$review_eng[[i]]),
                  text = beer$review_eng[[i]]) %>%
    mutate(name = beer$name_eng[i]) %>%
    select(name, everything())
  
  beer_review0 <- rbind(beer_review0, clean)
}

# 2. 단어별 감성 분석

beer_review %>% 
  inner_join(get_sentiments('bing')) %>% #단어 별로 감성분석
  count(word, sentiment, sort=T) %>% #감성별로 카운트
  group_by(sentiment) %>% 
  top_n(10) %>% #상위 10개만 뽑아
  ggplot(aes(x=reorder(word,n), y=n, fill=sentiment)) +
  geom_col(show.legend = F) +
  theme_bw() +
  facet_wrap(~sentiment, scales = 'free_y') +
  scale_fill_manual(values = c("#F9CA00", "#7D2E03")) +
  labs(y='Contribution to sentiment', x="") +
  coord_flip()

# 3. 리뷰별 감성 분석

Real <- beer_review %>% 
  left_join(get_sentiments('bing'))

Real$sentiment = 
  ifelse(Real$sentiment %>% is.na, 'neutral', Real$sentiment) #결측치 중립으로 처리

Real <- Real %>%
  group_by(review) %>% 
  count(name,review, sentiment) %>% 
  spread(sentiment,n,fill=0) %>% # 감성 별로 카운트
  mutate(sentiment = positive-negative) %>% # 긍정-부정
  mutate(result = ifelse(sentiment>0, 'positive', 
                         ifelse(sentiment<0, 'negative', 'neutral'))) # 양수면 긍정 음수면 부정으로 구분



# 4. 결과 데이터 만들기

Set <- # 리뷰별 감성분석 완료
  inner_join(beer_review0, Real, by=c('name','review')) 

Set$text <- gsub("[[:punct:]]", "", Set$text) # 특수문자 제거
          

Set2 <- # 단어별 감성분석 완료
  beer_review %>% 
  left_join(get_sentiments('bing')) 

Set2$sentiment = 
  ifelse(Set2$sentiment %>% is.na, 'neutral', Set2$sentiment) # 결측치 중립으로 처리



# 5. csv 저장

write.csv(Set, file='Sentiment.csv',row.names = F)
write.csv(Set2, file='Sentiment_word.csv',row.names = F)
