# Download the CBA from Github website

# ---- start --------------------------------------------------------------

# Parse the CBA to a raw text file:
library(pdftools)
library(tidyverse)

# Create a directory for the data
local_dir    <- "raw"
data_source <- paste0(local_dir, "/articles")
man_dir     <- paste0(local_dir, "/manual")
if (!file.exists(local_dir)) dir.create(local_dir, recursive = T)
if (!file.exists(data_source)) dir.create(data_source)
if (!file.exists(man_dir)) dir.create(man_dir)


# ---- download -----------------------------------------------------------

cba_url <- paste0("https://github.com/atlhawksfanatic/",
                  "atlhawksfanatic.github.io/raw/master/research/CBA/",
                  "1999-NBA-NBPA-Collective-Bargaining-Agreement.pdf")

cba_file <- paste(local_dir,
                  "1999-NBA-NBPA-Collective-Bargaining-Agreement.pdf",
                  sep = "/")
if (!file.exists(cba_file)) download.file(cba_url, cba_file)

# ---- parse --------------------------------------------------------------

cba <- pdf_text(cba_file)

# Remove the table of contents and the index, this is specifically for 1999
cba_cut <- cba[18:224]

# Key difference so far:
#  1. There are no page numbers in this CBA compared to the 1995 CBA.
#  2. Second article starts in the middle of a page instead of at the beginning.

# On Page 6, the CBA switches over from Article I to Article II mid-page. This
#  is the only time that happens in the CBA

# Find where an "Article" or "Exhibit" Exists.
cba_articles <- map2(cba_cut, seq(cba_cut), function(x, y) {
  print(y)
  
  exist_regex <- "(ARTICLE|EXHIBIT) [A-z]+\n([A-Z]|\\s|[:punct:])*\n"
  article_exists <- str_locate_all(x, "(ARTICLE|EXHIBIT) [A-z]+\n.*\n")
  # article_exists <- str_locate_all(x, exist_regex)
  
  if (is_empty(article_exists[[1]])) {
    article = NA_character_
    article_name   = NA_character_
    article_roman = NA_character_
  } else {
    article = str_sub(x, article_exists[[1]])
    
    article_type   = word(article, 1)
    article_roman  = str_trim(word(article, 2))
    article_name   = str_trim(word(article, 3, -1)) %>% 
      str_remove_all("\\n") %>% 
      str_squish()
  }
  
  # Punctuation that is in a different font
  txt <- str_replace_all(x, "“|”", '"')
  txt <- str_replace_all(txt, "’", "'")
  txt <- gsub("$", "\\$", txt, fixed = TRUE)
  txt <- gsub("%", "\\%", txt, fixed = TRUE)
  
  if (is_empty(article_exists[[1]])) {
    cba_structure <- tibble(page = y,
                            article = article,
                            text = txt,
                            article_name, article_roman)
    
    ## IF IT IS ARTICLE 2, HERE IS THE BREAK
  } else if (article_roman == "II\n") {
    # All of the text before the new article
    text1 <- str_sub(txt, 1, article_exists[[1]][1] - 1)
    # All of the text after the article number
    text2 <- str_sub(txt, article_exists[[1]][1])
    
    cba_structure <- tibble(page = y,
                            article = c(NA, paste(article_type,
                                                  article_roman)),
                            text = c(text1, text2),
                            article_name = c(NA, article_name),
                            article_roman = c(NA, article_roman))
    
  } else {
    cba_structure <- tibble(page = y,
                            article = paste(article_type, article_roman),
                            text = txt,
                            article_name, article_roman)
  }
  
  return(cba_structure)
})

# Converting the Exhibit values to numbers
exhibit_vals <- c("A" = 43,
                  "B" = 44,
                  "C" = 45,
                  "D" = 46,
                  "E" = 47,
                  "F" = 48,
                  "G" = 49,
                  "H" = 50)

cba_texts <- cba_articles %>% 
  bind_rows() %>% 
  fill(article:article_roman) %>% 
  mutate(article_number = ifelse(grepl("article", article, ignore.case = T),
                                 as.numeric(as.roman(article_roman)),
                                 exhibit_vals[article_roman])) %>% 
  arrange(article_number)


# As simple text files and Rmd
cba_texts %>% 
  group_by(article) %>% 
  summarise(text = paste0(str_trim(text), collapse = ""),
            number = article_number[1],
            article_name = article_name[1] %>% 
              str_remove_all(",") %>% 
              str_replace_all(" ", "-")) %>% 
  as.list() %>% 
  pmap(function(article, article_name, text, number) {
    temp_num  <- str_pad(number, 2, pad = "0")
    
    temp_file <- paste0(data_source, "/", article, ".txt")
    temp_rmd  <- paste0(man_dir, "/", temp_num, "-", article_name, ".Rmd")
    
    cat(text, file = temp_file)
    cat(text, file = temp_rmd)
  })
