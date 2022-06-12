#Disciplina: Análise de Mídias Sociais e Mineração de Texto
# Professor: Gustavo Mirapalheta
# Grupo: Ana Paula Puddu, Fabio Monteiro, Lucas Sena,Marcos Soares, Wagner Fonseca

##### SET UP AMBIENTE ####
## Setup do ambiente

#install.packages("tidytext")
library(tidytext)
#install.packages("stringr")
library(stringr)
stop_words
library(dplyr)
library(ggplot2)
library(gutenbergr)

#### BAIXANDO LIVROS - AGHATA CRHISTIE ####

gutenberg_works() %>% View()
gutenberg_download(67160, mirror = "http://mirrors.xmission.com/gutenberg/")%>%
  select(text)%>%
  mutate(book = 'The Hunter’s Lodge Case')-> agatha0;agatha0

gutenberg_download(61168, mirror = "http://mirrors.xmission.com/gutenberg/")%>%
  select(text)%>%
  mutate(book = 'THE MAN IN THE BROWN SUIT')-> agatha1;agatha1

gutenberg_download(67173, mirror = "http://mirrors.xmission.com/gutenberg/")%>%
  select(text)%>%
  mutate(book = 'The Missing Will')-> agatha2;agatha2

gutenberg_download(58866, mirror = "http://mirrors.xmission.com/gutenberg/")%>%
  select(text)%>%
  mutate(book = 'The Murder on the Links')-> agatha3;agatha3

gutenberg_download(863, mirror = "http://mirrors.xmission.com/gutenberg/")%>%
  select(text)%>%
  mutate(book = 'The Mysterious Affair at Styles')-> agatha4;agatha4

gutenberg_download(66446, mirror = "http://mirrors.xmission.com/gutenberg/")%>%
  select(text)%>%
  mutate(book = 'THE PLYMOUTH EXPRESS AFFAIR')-> agatha5;agatha5

#### TOKENIZAÇÃO E CONTAGEM DE PALAVRAS ####
 rbind(agatha0, agatha1, agatha2, agatha3, agatha4, agatha5) %>% 
  unnest_tokens(input = "text", output = "word") %>%
  mutate(word = str_extract(word, regex("[a-z']+"))) %>%
  filter(!is.na(word)) %>%
  anti_join(stop_words) %>%
  count(book,word) %>%
  group_by(book) %>%
  mutate(tf = n/sum(n)) %>%
  select(-n) %>%
  spread(key=book,value = tf) #linhas em colunas

#### ANÁLISE DE SENTIMENTOS - POR CATEGORIA - POSITIVO / NEGATIVO ###@

 rbind(agatha0, agatha1, agatha2, agatha3, agatha4, agatha5) %>%
  mutate(l80 = row_number() %/% 80)%>% # cria uma coluna l80 e separa a cada 80 linhas
  ungroup()%>%
  unnest_tokens(input = text, output = word)%>%
  mutate(word = str_extract(word, regex("[a-z']+")))%>% # extrair só texto
  filter(!is.na(word))%>% # Retirar N/A
  inner_join(get_sentiments("bing"), by=c("word" = "word"))%>%  # retirar os negativos
  count(book, l80, sentiment)%>% # quantas vezes aparece no grupo de 80 linhas
  spread(key = sentiment, value = n)%>% # separa qtd neg e pos
  mutate(net_sent = positive - negative)%>% # cria coluna pos-neg
  ggplot(aes(x=l80, y=net_sent, color=book)) +
  geom_col() + theme(legend.position = "none") +
  facet_wrap(~book, scale="free_x",ncol = 2)# dividirpor livro
 
 #### WORDCLOUD - PRETO E BRANCO ### PROBLEMAS AQUI NAO ESTA DIVINDO POR CAPITULO - TALVEZ PELA NAO PADRONIZACAO DOSLIVROS
 rbind(agatha0, agatha1, agatha2, agatha3, agatha4, agatha5) %>%
   group_by(book)%>%
   mutate(chapter = cumsum(str_detect(text,
                                      regex("^chapter [\\divxlc]",
                                            ignore_case = TRUE) ) ) )%>%
   ungroup() %>% unnest_tokens(input = text, output = word,
                               to_lower = TRUE, drop=TRUE)%>%
   mutate(word = str_extract(word, regex("[a-z']+")))%>%
   anti_join(stop_words)%>%
   count(word) %>%
   with(wordcloud(word, n, max.words = 100))
 
#### WORDCLOUD ###
 rbind(agatha0, agatha1, agatha2, agatha3, agatha4, agatha5) %>%
   unnest_tokens(input =  text, output = word)%>%
   mutate(word = str_extract(word, regex("[a-z']+")))%>%
   inner_join(get_sentiments("bing"))%>%
   count(word, sentiment, sort = TRUE)%>%
   filter(word != "miss")%>% # este filtro foi colocado pq miss foi intretado como sentir falta e nao como senhorita, deveria ser palavra neutra
   spread(key = sentiment, value=n)%>%
   mutate(negative = replace_na(negative,0),
          positive = replace_na(positive,0))%>%
   tibble::column_to_rownames(var = "word")%>%
   comparison.cloud(colors=c("red", "blue"),max.words = 100)
 
 ####  LEI DE ZIPF frequencia de uso de todas as palavras - TF Term frequency ####
 
 rbind(agatha0, agatha1, agatha2, agatha3, agatha4, agatha5) %>%
   unnest_tokens(input = text, output = word)%>%
   mutate(word = str_extract(word, regex("[a-z']+")))%>%
   filter(!is.na(word))%>%
   count(book,word)%>%
   bind_tf_idf(term = word, document = book, n = n)%>%
   select(book, word, tf)%>%
   arrange(book,desc(tf))%>%
   group_by(book)%>%
   mutate(rank = row_number())%>%
   ungroup() -> zipf; zipf
 
 zipf %>% filter(rank>=10, rank<=1000) -> zipf2
 zipf %>% ggplot(aes(x=log10(rank), y=log10(tf)))+
   geom_point(aes(color=book))+
   geom_smooth(data=zipf2, method = 'lm', lty=2, color='black')

 #### TF - IDF ####
 rbind(agatha0, agatha1, agatha2, agatha3, agatha4, agatha5) %>%
   unnest_tokens(input = text, output = word)%>%
   mutate(word = str_extract(word, regex("[a-z']+")))%>%
   filter(!is.na(word))%>%
   count(book,word)%>%
   bind_tf_idf(term = word, document = book, n = n)%>%
   group_by(book)%>%
   arrange(book, desc(tf_idf))%>%
   filter(row_number()<=10)%>%
   ggplot(aes(x=reorder(word,tf_idf), y=tf_idf))+
   geom_col() +
   facet_wrap(~book, scales = "free")+
   coord_flip()