# Frequencies

# Load packages
library(tidyverse)
library(quanteda)
library(quanteda.textmodels)
library(quanteda.textplots)
library(quanteda.textstats)
library(ggplot2)
library(readtext)
library(viridis)

# set seed
set.seed(20022901)

# load required objects
load("corpus_objects/metadata.rda")
metadata <- corpus_metadata
rm(corpus_metadata)


# first we need to create a corpus.
# this time we'll be using "quanteda", a package that allows to do corpus analysis very easily

files_list <- list.files("corpus", full.names = T, pattern = "txt") 


corpus <- readtext(files_list, encoding = "UTF-8") %>%  
  as_tibble() %>%
  mutate(doc_id = stringr::str_remove(doc_id, ".txt")) %>% 
  # join metadata
  left_join(metadata, by = "doc_id")


head(corpus)

# and to transform this into a corpus that we can use with the package "quanteda", a friendly package that allows us to analyse various aspects of a corpus

quanteda_texts <- quanteda::corpus(corpus,
                                   docid_field = "doc_id",
                                   text_field = "text",
                                   meta = list("song_name",
                                               "artist_name",
                                               "artist_country",
                                               "is_male"
                                               )
                                   )




# Tokens corpus ---------------

# quanteda mainly works with so called DFM (Document-feature matrix). These
# - Represents frequencies of features in documents in a matrix
# - Have an efficient structure, but do not have information on the position of words
# - Allow for a bag-of-words approach


## Let's create a dfm corpus --------------

## first we need to create a "token" corpus. This file is very big,


quanteda_texts_tok <- tokens(quanteda_texts,
                             # we don't want pucntuation
                             remove_punct = T,
                             # we don't want to keep hyphens
                             split_hyphens = T,
                             # and no symbols
                             remove_symbols = T) 

# create subsets
women_toks <- quanteda_texts %>%
  corpus_subset(is_male == 0) %>%
  tokens(remove_punct = T,
         split_hyphens = T,
         remove_symbols = T)

men_toks <- quanteda_texts %>%
  corpus_subset(is_male == 1) %>%
  tokens(remove_punct = T,
         split_hyphens = T,
         remove_symbols = T)

# save object
save(quanteda_texts_tok, file = "corpus_objects/quanteda_texts_tok.rda")
save(women_toks, file = "corpus_objects/women_toks.rda")
save(men_toks, file = "corpus_objects/men_toks.rda")


## then we can create a dfm

quanteda_texts_dfm <- dfm(quanteda_texts_tok)
women_dfm <- dfm(women_toks)
men_dfm <- dfm(men_toks)

# and we can have a first look at the most frequent words for all corpus, for instance with a wordcloud

textplot_wordcloud(quanteda_texts_dfm, max_words = 100)

textplot_wordcloud(women_dfm, max_words = 100)

textplot_wordcloud(men_dfm, max_words = 100)

# in a "table" form

textstat_frequency(quanteda_texts_dfm) %>%
  head(30)

# or in a plot, such as a this one

quanteda_texts_dfm %>% 
  textstat_frequency(n = 15) %>% 
  ggplot(aes(x = reorder(feature, frequency), y = frequency)) +
  geom_point() +
  coord_flip() +
  labs(x = NULL, y = "Frequency") +
  theme_minimal()

## it is quite evident that we have at the top of the list words that are not 
## extremely interesting (unless you are focusing on conjunctions and pronouns).
## We can thus decide to look only at words that fall into a specific range 
## of our frequency ranking.

# If you look at the table we produced before, you'll see that 
# somewhere around the 500 occurrences words seem to be getting more juicy.
# Let's see how that goes if we set that up.

dfm(quanteda_texts_tok)  %>%
  dfm_trim(max_termfreq = 500) %>% # here we say "we want a frequency max of 500"
  textplot_wordcloud(max_words = 100)

dfm(quanteda_texts_tok)  %>%
  dfm_trim(max_termfreq = 500) %>% 
  textstat_frequency() %>%
  head(20)

dfm(quanteda_texts_tok)  %>%
  dfm_trim(max_termfreq = 500) %>% 
  textstat_frequency(n = 20) %>% 
  ggplot(aes(x = reorder(feature, frequency), y = frequency)) +
  geom_point() +
  coord_flip() +
  labs(x = NULL, y = "Frequency") +
  theme_minimal()


# another thing we can do is to visualize group differences in frequency,
# for instance, we might want to see which words are the most frequently used by women vs men authors.

colnames(quanteda_texts_dfm@docvars) # this will show you which metadta we have

# load in useless words
useless_words <- readtext("useless_words.txt", encoding = "UTF-8") %>% 
  tidytext::unnest_lines(input = "text", output = "words")



# remove stopwords
quanteda_texts_dfm_ns <- dfm(quanteda_texts_tok %>% 
                               tokens_remove(pattern = stopwords("es")) %>% 
                               tokens_remove(pattern = stopwords("en")))

women_dfm_ns <- dfm(women_toks %>%
                      tokens_remove(pattern = stopwords("es")) %>%
                      tokens_remove(pattern = stopwords("en")))

men_dfm_ns <- dfm(men_toks %>%
                      tokens_remove(pattern = stopwords("es")) %>%
                      tokens_remove(pattern = stopwords("en")))

# remove stopwords and useless words
quanteda_texts_dfm_nw <- dfm(quanteda_texts_tok %>% 
                               tokens_remove(useless_words$words) %>% 
                               tokens_remove(pattern = stopwords("es")) %>% 
                               tokens_remove(pattern = stopwords("en")))

women_dfm_nw <- dfm(women_toks %>%
                      tokens_remove(useless_words$words) %>%
                      tokens_remove(pattern = stopwords("es")) %>%
                      tokens_remove(pattern = stopwords("en")))

men_dfm_nw <- dfm(men_toks %>%
                      tokens_remove(useless_words$words) %>%
                      tokens_remove(pattern = stopwords("es")) %>%
                      tokens_remove(pattern = stopwords("en")))
# by country
quanteda_texts_dfm_ns %>% 
  textstat_frequency(n = 8, groups = artist_country) %>% 
  ggplot(aes(x = reorder(feature, frequency), y = frequency, color = group)) +
  geom_point() +
  coord_flip() +
  labs(x = NULL, y = "Frequency") +
  theme_minimal() +
  scale_color_viridis(discrete = TRUE, option = "plasma")

quanteda_texts_dfm_nw %>% 
  textstat_frequency(n = 5, groups = artist_country) %>% 
  ggplot(aes(x = reorder(feature, frequency), y = frequency, color = group)) +
  geom_point() +
  coord_flip() +
  labs(x = NULL, y = "Frequency") +
  theme_minimal() +
  scale_color_viridis(option = "turbo", discrete = TRUE)


women_dfm_nw %>% 
  textstat_frequency(n = 5, groups = artist_country) %>% 
  ggplot(aes(x = reorder(feature, frequency), y = frequency, color = group)) +
  geom_point() +
  coord_flip() +
  labs(x = NULL, y = "Frequency") +
  theme_minimal() +
  scale_color_viridis(option = "turbo", discrete = TRUE)

men_dfm_nw %>% 
  textstat_frequency(n = 5, groups = artist_country) %>% 
  ggplot(aes(x = reorder(feature, frequency), y = frequency, color = group)) +
  geom_point() +
  coord_flip() +
  labs(x = NULL, y = "Frequency") +
  theme_minimal() +
  scale_color_viridis(option = "turbo", discrete = TRUE)
  

# by gender
# quanteda_texts_dfm_ns %>% 
#   textstat_frequency(n = 20, groups = is_male) %>% 
#   ggplot(aes(x = reorder(feature, frequency), y = frequency, color = group)) +
#   geom_point() +
#   coord_flip() +
#   labs(x = NULL, y = "Frequency") +
#   theme_minimal()
# 
# quanteda_texts_dfm_nw %>%
#   textstat_frequency(n = 20, groups = is_male) %>%
#   ggplot(aes(x = reorder(feature, frequency), y = frequency, color = group)) +
#   geom_point() +
#   coord_flip() +
#   labs(x = NULL, y = "Frequency") +
#   theme_minimal()

p1 <- quanteda_texts_dfm_ns %>% 
  textstat_frequency(n = 20, groups = is_male) %>% 
  ggplot(aes(x = reorder(feature, frequency), y = frequency, color = group)) +
  geom_point() +
  coord_flip() +
  labs(x = NULL, y = "Frequency", color = "Artist Gender", title = "Not Cleaned") +
  scale_color_viridis(discrete = TRUE, labels = c("Women+", "Men")) +
  theme_minimal()

p2 <- quanteda_texts_dfm_nw %>% 
  textstat_frequency(n = 20, groups = is_male) %>% 
  ggplot(aes(x = reorder(feature, frequency), y = frequency, color = group)) +
  geom_point() +
  coord_flip() +
  labs(x = NULL, y = "Frequency", color = "Artist Gender", title = "Cleaned") +
  scale_color_viridis(discrete = TRUE, labels = c("Women+", "Men")) +
  theme_minimal() 

p1 + p2 +
  labs(caption = "Fig. 4. Most Common Tokens by Gender")


library(patchwork)

a <- women_dfm_nw %>%
  textstat_frequency(n = 20) %>%
  ggplot(aes(x = reorder(feature, frequency), y = frequency)) +
  geom_point() +
  coord_flip() +
  labs(x = NULL, y = "Frequency") +
  theme_minimal()

b <- men_dfm_nw %>%
  textstat_frequency(n = 20) %>%
  ggplot(aes(x = reorder(feature, frequency), y = frequency)) +
  geom_point() +
  coord_flip() +
  labs(x = NULL, y = "Frequency") +
  theme_minimal()

a + b

# # we can also compare word frequencies by document
# textplot_wordcloud(quanteda_texts_dfm, 
#                    max_words = 100, 
#                    comparison = TRUE,
#                    labelsize = 1, 
#                    color = rev(RColorBrewer::brewer.pal(10, "Dark2")))

# check which other color palettes you can use with
# RColorBrewer::display.brewer.all()


## or look at single words comparisons:

sorted_features <- topfeatures(quanteda_texts_dfm, n = nfeat(quanteda_texts_dfm))
sorted_features[c("ella", "el", "él", "la", "lo")]


# Stats ---------------

## frequencies
## as we saw before, we can easily craete a table with all the information 
## about toke frequencies in our corpus

frequency_table <- textstat_frequency(quanteda_texts_dfm, groups = is_male)

total_token <- frequency_table %>% 
  group_by(group) %>% 
  count() %>% 
  ungroup() %>% 
  select(n)

total_token$n[1]

frequency_table <- frequency_table %>%
  mutate(token_prop = case_when(
    group == 0 ~ frequency / total_token$n[1],
    group == 1 ~ frequency / total_token$n[2]
  ))

frequency_table %>%
  group_by(group) %>% 
  arrange(desc(token_prop)) %>% 
  slice_head(n = 20) %>% 
  count()

# proportion table !!
token_prop_table <- frequency_table %>%
  #filter(!feature %in% useless_words$words) %>% 
  filter(!feature %in% stopwords(language = "es")) %>% 
  filter(!feature %in% stopwords(language = "en")) %>% 
  group_by(group) %>% 
  arrange(desc(token_prop)) %>% 
  slice_head(n = 20) %>% 
  ungroup() %>% 
 # filter(token_prop > 0.1) %>%
  ggplot(aes(x = reorder(feature, token_prop), y = token_prop, color = group)) +
  geom_point() +
  coord_flip() +
  labs(x = NULL, y = "Token Usage Proportion") +
  theme_minimal()

# and observe it either in the console

print(head(frequency_table, 20))

# analyse words individually

frequency_table %>% 
  mutate(group = case_when(group == 0 ~ "Men",
                           group == 1 ~ "Women")) %>% 
  rename(Artist_Gender = group) %>% 
  filter(feature == "ella") %>% 
  rename(Token = feature,
         Frequency = frequency,
         Rank = rank,
         Document_Frequency = docfreq,
         Token_Proportion = token_prop) %>% 
  mutate(Token_Proportion = round(Token_Proportion, 4)) %>% 
  DT::datatable()

frequency_table %>% 
  filter(feature == "culo") # what these results tell me, similar frequency but diff ranks is that lexical diversity might be diff

frequency_table %>% 
  filter(feature == "gata")

frequency_table %>% 
  filter(feature == "perra")

frequency_table %>% 
  filter(feature == "yo")

frequency_table %>% 
  filter(feature %in% c("tu", "tú"))

frequency_table %>% 
  filter(feature == "mujer")

frequency_table %>% 
  group_by(group) %>% 
  count()


# or as a full table

# view(frequency_table)


# or compare the presence of a term by author

quanteda_texts_dfm %>% 
  textstat_frequency(groups = artist_name) %>% 
  filter(feature == "chapa") %>%
  ggplot(aes(x = group, y = frequency, label = feature)) +
  geom_col(width = .1) +
  # geom_point(size = 1) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 7)
  ) 


## Concordance

# with quanteda you can also visualise concordances (KWIC = keywords in context)

kwic_test <- quanteda_texts_tok %>%
  kwic(pattern =  "culo*", window = 10)

kwic_test <- men_toks %>%
  kwic(pattern =  "ella*", window = 5)

head(kwic_test, 10)

kwic_test <- women_toks %>%
  kwic(pattern =  "ella*", window = 5)

head(kwic_test, 10)


kwic_test <- quanteda_texts_tok %>%
  kwic(pattern =  c("ella*","mujer*"), 
       window = 5)

head(kwic_test, 10)

# if you want to find multi-word expressions, separate words by white space and wrap the character vector by phrase().

kwic_test <- quanteda_texts_tok %>%
  kwic(pattern = phrase("ella me"))

kwic_test <- men_toks %>%
  kwic(pattern = phrase("ella se"))


kwic_test <- women_toks %>%
  kwic(pattern = phrase("ella se"))

head(kwic_test)

remove(kwic_test)


# LEXICAL DIVERSITY

# one thing that you might want to do, is to chech how different various documents are
# you can do that with the function "textstat_lexdiv()", which calculates various lexical diversity measures based on the number of unique types of tokens and the length of a document. It is useful, for instance, for analysing speakers’ or writers’ linguistic skills, or the complexity of ideas expressed in documents.

tstat_lexdiv <- quanteda_texts_dfm %>%
  textstat_lexdiv() %>% 
  rename(doc_id = document) %>% 
  left_join(metadata, by = "doc_id") %>% 
  group_by(is_male) %>% 
  summarize(mean_TTR = mean(TTR))


# textstat_dist() calculates similarities of documents or features for various measures. The output is compatible with R’s dist(), so hierarchical clustering can be performed without any transformation.

tstat_dist <- quanteda_texts_dfm_nw %>% 
  textstat_dist() %>%
  as.dist()

tstat_dist %>%
  hclust() %>%
  plot(xlab = "Distance", ylab = NULL)
