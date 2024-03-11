# metadata creation
library(tidyverse)
library(mapdata)


# Set the directory where the files are located
folder_path <- "corpus_women"

# Get the names of all files in the folder
file_names <- list.files(folder_path, full.names = TRUE)


# Extract song names and artist names from file names
song_names <- tools::file_path_sans_ext(basename(file_names))
artist_names <- str_extract(song_names, "_.*$") %>% str_remove("^_")

# Create a tibble
songs_tibble <- tibble(song_name = str_remove(song_names, "_.*$"),
                       artist_name = artist_names)


# Add additional columns


women_metadata <- songs_tibble %>%
  # this column adds an artist country
  mutate(artist_country = case_when(
    artist_name == "Young Miko" ~ "Puerto Rico",
    artist_name == "SASHA SATHYA" ~ "Argentina",
    artist_name == "Gudnana" ~ "Venezuela",
    artist_name == "Chzter" ~ "Mexico",
    artist_name == "Snow Tha Product" ~ "Mexico",
    artist_name == "Valen Etchegoyen" ~ "Argentina",
    artist_name == "Cazzu" ~ "Argentina",
    artist_name == "Nath" ~ "Colombia",
    artist_name == "Maria Becerra" ~ "Argentina",
    artist_name == "KAROL G" ~ "Colombia",
    artist_name == "Villano Antillano" ~ "Puerto Rico",
    artist_name == "Tokischa & Rosalía" ~ "Dominican Republic",
    artist_name == "Isabella Lovestory" ~ "Honduras",
    artist_name == "Kali Uchis & KAROL G" ~ "Colombia",
    artist_name == "Kali Uchis" ~ "Colombia",
    artist_name == "Chocolate Remix" ~ "Argentina",
    TRUE ~ NA_character_  # Set to NA for other cases
  )) %>% 
  # filter out errors
  mutate(doc_id = song_names) %>% 
  mutate(is_male = 0) %>% 
  filter(song_name != "young",
         song_name != "corpus") %>% 
  # add column for gender
  select(doc_id, song_name, artist_name, artist_country, is_male)

# create a table of songs by country
women_metadata %>% 
  ggplot(aes(fct_infreq(artist_country))) +
  geom_bar(fill = "#69b3a2") +
  labs(x = "Artist Country", y = "Frequency") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

# create a table of country percentage
women_metadata %>% 
  group_by(artist_country) %>% 
  summarize(count = n(),
            percent = round(count / nrow(songs_tibble) * 100, 2)) %>% 
  arrange(desc(percent)) %>% 
  DT::datatable()


# write to csv
save(women_metadata, file = "corpus_objects/women_metadata.rda")

######## same for men subcorpus ----


# Set the directory where the files are located
comparative_folder_path <- "corpus_men"

# Get the names of all files in the folder
comparative_file_names <- list.files(comparative_folder_path, full.names = TRUE)

# Extract song names and artist names from file names
song_names <- tools::file_path_sans_ext(basename(comparative_file_names))
artist_names <- str_extract(song_names, "_.*$") %>% str_remove("^_")

# Create a tibble
comparative_songs_tibble <- tibble(song_name = str_remove(song_names, "_.*$"),
                       artist_name = artist_names)

# Add additional columns


men_metadata <- comparative_songs_tibble %>%
  # this column adds an artist country
  mutate(artist_country = case_when(
    artist_name == "Daddy Yankee" ~ "Puerto Rico",
    artist_name == "Jhayco" ~ "Puerto Rico",
    artist_name == "Anuel AA" ~ "Puerto Rico",
    artist_name == "Bad Bunny" ~ "Puerto Rico",
    artist_name == "Ozuna" ~ "Puerto Rico",
    artist_name == "Rauw Alejandro" ~ "Puerto Rico",
    artist_name == "Paulo Londra" ~ "Argentina",
    artist_name == "Tiago PZK" ~ "Argentina",
    artist_name == "Duki" ~ "Argentina",
    artist_name == "Dani Flow" ~ "Mexico",
    artist_name == "Peso Pluma" ~ "Mexico",
    artist_name == "J Balvin" ~ "Colombia",
    artist_name == "Maluma" ~ "Colombia",
    artist_name == "Camilo" ~ "Colombia",
    artist_name == "Sech" ~ "Panama",
    artist_name == "El Alfa" ~ "Dominican Republic",
    TRUE ~ NA_character_  # Set to NA for other cases
  )) %>% 
  mutate(doc_id = song_names) %>% 
  mutate(is_male = 1) %>% 
  filter(song_name != "comparative",
         song_name != "corpus") %>% 
  select(doc_id, song_name, artist_name, artist_country, is_male)

# create a table of songs by country
men_metadata %>% 
  ggplot(aes(fct_infreq(artist_country))) +
  geom_bar(fill = "#69b3a2") +
  labs(x = "Artist Country", y = "Frequency") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

# create a table of country percentage
men_metadata %>% 
  group_by(artist_country) %>% 
  summarize(count = n(),
            percent = round(count / nrow(songs_tibble) * 100, 2)) %>% 
  arrange(desc(percent)) %>% 
  DT::datatable()


# write to rda
save(men_metadata, file = "corpus_objects/men_metadata.rda")

### Bind the two to get metadata for the whole corpus -----
corpus_metadata <- rbind(women_metadata, men_metadata)

# write to rda
save(corpus_metadata, file = "corpus_objects/metadata.rda")
