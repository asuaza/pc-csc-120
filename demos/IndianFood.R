# install.packages("tidytext")
# install.packages("ggwordcloud")

library("tidyverse")
library("tidytext") # For tokenizing text data
library("ggwordcloud") # For plotting word clouds

# data is from https://www.kaggle.com/datasets/kanishk307/6000-indian-food-recipes-dataset:
food <- read_csv("~/Downloads/IndianFoodDatasetCSV.csv")

View(food)

food %>% count(Cuisine)
# Equivalent to: food %>% group_by(Cuisine) %>% summarize(n = n())
food %>% count(Course)
food %>% count(Diet)

food <- food %>%
  select(-Srno, -RecipeName, -Ingredients, -Instructions, -URL) %>%
  rename(RecipeName = TranslatedRecipeName,
         Ingredients = TranslatedIngredients,
         Instructions = TranslatedInstructions)
food %>% pull(Ingredients) %>% head


tokens <- food %>% unnest_tokens(word, Ingredients)
food %>% select(Ingredients) %>% slice_head(n = 1)
tokens %>% select(word) %>% slice_head(n = 10)

wordcloud_dat <- tokens %>%
  count(word) %>%
  slice_max(order_by = n, n = 100)
wordcloud_dat

ggplot(wordcloud_dat) +
  geom_text_wordcloud(aes(label = word, size = n)) +
  theme_minimal()

last_plot() +
  scale_size_area(max_size = 24)

# Discuss. Are all of these words equally informative?

data("stop_words")
View(stop_words)
tokens <- tokens %>%
  anti_join(stop_words, by = "word")
# Equivalent to: tokens <- tokens %>%
#   filter(!(word %in% stop_words$word))

tokens <- tokens %>%
  filter(str_detect(word, "[a-z]+"))

wordcloud_dat <- tokens %>%
  count(word) %>%
  slice_max(order_by = n, n = 100)
ggplot(wordcloud_dat) +
  geom_text_wordcloud(aes(label = word, size = n)) +
  scale_size_area(max_size = 16) +
  theme_minimal()

food %>% slice_sample(n = 5) %>% pull(Ingredients)
tokens <- food %>%
  unnest_tokens(ingredient, Ingredients, token = stringr::str_split, pattern = ",")
View(tokens)

tokens <- tokens %>%
  mutate(ingredient = str_extract(ingredient, "[a-z][^-]+"))
View(tokens)

first_words <- tokens %>%
  mutate(word = str_extract(ingredient, "[a-z]+")) %>%
  count(word, sort = TRUE)

my_stopwords <- tibble(word = c(
  "teaspoon", "teaspoons", "tsp",
  "tablespoon", "tablespoons", "tbsp",
  "cup", "cups",
  "gram", "grams", "kg",
  "inch", "inches",
  "sprig", "sprigs",
  "whole",
  "clove", "cloves",
  "pinch", "pinches",
  "ml", "liter", "liters",
  "extra",
  "stick"
))

tokens <- tokens %>%
  unnest_tokens(word, ingredient) %>%
  anti_join(stop_words) %>%
  anti_join(my_stopwords)

wordcloud_dat <- tokens %>%
  count(word) %>%
  slice_max(order_by = n, n = 100)
ggplot(wordcloud_dat) +
  geom_text_wordcloud(aes(label = word, size = n)) +
  scale_size_area(max_size = 16) +
  theme_minimal()


food %>% count(Course)
courses_to_include <- c(
  "North Indian Breakfast",
  "South Indian Breakfast",
  "World Breakfast"
)

tokens <- tokens %>%
  filter(Course %in% courses_to_include)

wordcloud_dat <- tokens %>%
  group_by(Course) %>%
  count(word) %>%
  slice_max(order_by = n, n = 50)
ggplot(wordcloud_dat) +
  geom_text_wordcloud(aes(label = word, size = n)) +
  scale_size_area(max_size = 10) +
  theme_minimal() +
  facet_wrap(vars(Course))

# Scale n so that the wordclouds are roughly the same size across courses:
wordcloud_dat <- wordcloud_dat %>%
  group_by(Course) %>%
  mutate(n = scale(n, center = FALSE))
ggplot(wordcloud_dat) +
  geom_text_wordcloud(aes(label = word, size = n)) +
  scale_size_area(max_size = 10) +
  theme_minimal() +
  facet_wrap(vars(Course))

word_color <- wordcloud_dat %>%
  group_by(word) %>%
  summarize(color = factor(n()))
wordcloud_dat <- wordcloud_dat %>%
  left_join(word_color)
ggplot(wordcloud_dat) +
  geom_text_wordcloud(aes(label = word, size = n, color = color)) +
  scale_size_area(max_size = 10) +
  scale_color_brewer(palette = "Reds") +
  theme_minimal() +
  facet_wrap(vars(Course))


ggplot(wordcloud_dat) +
  geom_text_wordcloud(
    aes(label = word, size = n, color = color),
    show.legend = TRUE
  ) +
  scale_size_area(max_size = 10, guide = "none") +
  scale_color_brewer(palette = "Reds", direction = -1) +
  labs(color = "Number of wordclouds in which word appears:") +
  theme_minimal() +
  theme(strip.text = element_text(size = 16)) +
  theme(legend.position = "bottom") +
  guides(color = guide_legend(title.position = "top")) +
  facet_wrap(vars(Course))
