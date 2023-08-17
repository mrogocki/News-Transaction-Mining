library(textrecipes)
library(httr)
library(arules)
library(data.table)
library(magrittr)
library(rvest)
library(purrr)
library(checkmate)
library(stopwords)
library(recipes)
library(arulesViz)

url_paths <- c("https://www.tagesschau.de/wirtschaft/streik-bahn-evg-flughaefen-101.html",
               "https://www.tagesschau.de/wirtschaft/bahn-aufspaltung-union-reaktionen-gdl-fahrgastverband-101.html",
               "https://www.tagesschau.de/inland/regional/hamburg/ndr-ab-morgen-gilt-auch-in-hamburg-das-49-euro-ticket-100.html")


# Step 1: Request via GET HTTP availability
html_requests <- purrr::map(url_paths, ~ httr::GET(.x))
# Check for conditions and HTTP code 200
checkmate::assert(length(html_requests) == length(url_paths) &
                    all(purrr::map(length(html_requests), ~ html_requests[[.x]]$status_code) == 200))

# Extract the text corpi from the websites
html_content <- purrr::map(url_paths, ~ rvest::read_html(.x) %>%
                             rvest::html_elements("p") %>%
                             rvest::html_text() %>%
                             paste(.x, collapse = " "))

# Convert data to data.table
html_content <- data.table("ID" = 1:length(html_content),
                           "NEWS" = unlist(html_content))


# Create token-based corpus
# Creating preprocessing recipe

# Create function to map different news_recipes
stopwords_news <- stopwords::stopwords(language = "de")

recipe_generator <- function(column, data, min_times = 1, max_times = 2) {

  news_recipe <- recipes::recipe(formula = noquote(column), x = data) %>%
    step_tokenize(column) %>%
    step_stopwords(column, stopword_source = stopwords_news) %>%
    step_tokenfilter(column, min_times = min_times, max_times = max_times) %>%
    step_tf(NEWS)

  return(news_recipe)
}

#recipes_list <- purrr::map2(1:3, seq(2, 10, by = 4),
#                            ~ recipe_generator(column = "ID ~ NEWS",
#                                               data = html_content,
#                                               min_times = .x,
#                                               max_times = .y))


# Alternative: Single recipe pipeline
news_recipe <- recipes::recipe(ID ~ NEWS, data = html_content) %>%
  step_tokenize(NEWS) %>%
  step_stopwords(NEWS, custom_stopword_source = stopwords_news) %>%
  step_tokenfilter(NEWS, max_tokens = .Machine$integer.max) %>%
  step_tf(NEWS, prefix = NULL)


news_recipe_tf <- prep(news_recipe) %>%
  bake(new_data = NULL) %>%
  setDT()

news_recipe_tf %<>% melt(id.vars = "ID",
                         variable.name = "TOKEN",
                         value.name = "TOKEN_FREQUENCY") %>%
  .[, TOKEN := stringr::str_extract(TOKEN, "[a-z]+$")] %>%
  .[!is.na(TOKEN)]

news_recipe_tf <- dcast(news_recipe_tf, ID ~ TOKEN,
                        fun.aggregate = sum,
                        value.var = "TOKEN_FREQUENCY")

news_recipe_tf[, names(news_recipe_tf) :=
                 map(.SD, ~ as.factor(.x)),
               .SDcols = names(news_recipe_tf)]

news_recipe_transactions <- arules::transactions(news_recipe_tf)

summary(news_recipe_transactions)

# Sort transactions by support
itemFrequencyPlot(news_recipe_transactions, type = "absolute", topN=20)
inspect(head(news_recipe_transactions, by = "support"))

# Single approach
rules <- apriori(news_recipe_transactions, parameter = list(support = 0.3,
                                                            confidence = 0.3,
                                                            maxlen = 2))

# Heuristical approach for generating multiple measure thresholds
# Parameter generator

parameter_thresholds = expand.grid(support = seq(0.1, 1.0, by = 0.1),
                            confidence = seq(0.1, 1.0, by = 0.1),
                            maxlen = 2:5) %>%
  setDT()

rules <- map(1:length(parameter_thresholds), ~ arules::apriori(news_recipe_transactions, parameter = list(support = parameter_thresholds[.x, support],
                                                                                                          confidence = parameter_thresholds[.x, confidence],
                                                                                                          maxlen = parameter_thresholds[.x, maxlen])))


# Plot arulesViz
plot(rules, method = "graph", engine = "html")

# Inspect rules
inspect(head(rules, 100))

