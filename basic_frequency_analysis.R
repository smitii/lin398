library(dplyr) # data manipulation
library(ggplot2) # package for generating a word frequency plot
library(tidytext) # package for doing a word frequency analysis
library(janeaustenr)
library(stringr)
data(stop_word)

if (FALSE) {
  text <- c("The Fulton County Grand Jury said Friday an investigation of Atlanta's recent primary election produced no evidence that any irregularities took place.",
            "The jury further said in term-end presentments that the City Executive Committee, which had over-all charge of the election, deserves the praise and thanks of the City of Atlanta for the manner in which the election was conducted.",
            "The September-*october term jury had been charged by Fulton Superior Court Judge Durwood Pye to investigate reports of possible irregularities in the hard-fought primary which was won by Mayor-nominate Ivan Allen Jr..",
            "Only a relative handful of such reports was received, the jury said, considering the widespread interest in the election, the number of voters and the size of this city.") # first 4 lines from the Brown Corpus
  text
  text_df <- data.frame(line = 1:4, text = text) # create a tabular dataset with the columns `line` and `text`
  text_df
  
  is.character(text)
  length(text[1])
  
  text.word.freq <- unnest_tokens(tbl = text_df, input = text, output = words) %>% # tokenize `text` into `words`
    count(words, sort = TRUE) %>% # count `words` and sort them
    head(10) # return only the first ten lines in the dataset
  
    ggplot(data = text.word.freq, aes(x = reorder(words, desc(n)), y = n)) + geom_bar(stat = "identity") + labs(x = "Words", y = "Count", title = "Word frequency plot") # plot the `words` on the x-axis and the count `n` on the y-axis using a bar plot ordered by `n`
}

text <- c("Because I could not stop for Death -",
          "He kindly stopped for me -",
          "The Carriage held but just Ourselves -",
          "and Immortality")
text
text_df <- data_frame(line = 1:4, text = text)
text_df
text_df %>% unnest_tokens(word, text)

original_books <- austen_books() %>%
  group_by(book) %>%
  mutate(linenumber = row_number(),
         chapter = cumsum(str_detect(text, regex("^chapter [\\divxlc]",
                                                 ignore_case = TRUE)))) %>%
  ungroup()

original_books

tidy_books <- original_books %>%
  unnest_tokens(word, text) %>% 
  anti_join(stop_words)

tidy_books %>% count(word, sort = TRUE) %>% 
  filter(n > 600) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()

