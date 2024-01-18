#####################################################################################
################## Scraping Hansard Speeches: Code ##################################
#####################################################################################

#freely taken and modified to suit our purposes from: 
#Daniel Braby's 'Hansard Grabber': https://gist.github.com/dbrby/cab614288b998a5157fa6a87145969f2

require(purrr)
require(tidyverse)
require(rvest)

start_date <- as.Date("2023-01-01") # Set start date of your speech data collection

x <- seq.Date(start_date, Sys.Date(), "day") # all days from start date until now

front_url <- "https://hansard.parliament.uk/commons" #Base URL for Commons website
day_urls <- paste0(front_url, "/", x) #Generate URLs for each day

day_htmls <- lapply(day_urls, read_html) #Read in HTMLs for days

debate_links <- lapply(day_htmls, function(lg) {
  lg %>% 
    html_elements('.card-section') %>%
    html_attr('href')
}) # Write function linkgetter and apply to all files

debate_links <- debate_links %>% unlist() #Unlist and store as a vector


debate_links <- paste0("https://hansard.parliament.uk",
                       debate_links)

debate_htmls <- lapply(debate_links, read_html) #Read in debate HTMLs


#need to embed the map function inside a loop otherwise some common aspects of the webpage (clustered by debate) will only appear for first speaker in the page.

#using the (i) functionality, we'll be able to add the common URL to all texts/speakers in the webpage and then extract the date for each speech with regular expression

out <- map_df(seq_along(debate_htmls), function(i) {
  
  # Extract IDs, names, text, and date
  ID <-  html_elements(debate_htmls[[i]], "[class = 'attributed-to-details with-link']") %>% 
    html_attr("href") %>% 
    purrr::map_chr(~ifelse(length(.) > 0, as.character(floor(parse_number(.))), NA_character_))
  
  speakers <-  html_elements(debate_htmls[[i]], ".primary-text") %>%
    html_text() %>% trimws() %>%
    str_subset("Several", negate = TRUE)
  
  text <- html_elements(debate_htmls[[i]], ".content") %>%
    html_text() %>% trimws()
  
  # Find the maximum length
  max_length <- pmax(length(text), length(speakers), length(ID), length(date))
  
  # Pad vectors with NA to make them of equal length
  text <- c(text, rep(NA_character_, max_length - length(text)))
  speakers <- c(speakers, rep(NA_character_, max_length - length(speakers)))
  ID <- c(ID, rep(NA_character_, max_length - length(ID)))
  
  # Create a tibble with date, text, speakers, ID, and URL
  tibble(ID, speakers, text, URL = debate_links[i])
}) %>%
  filter(!is.na(ID) & !is.na(speakers)) %>%
  mutate(date = str_extract(URL, "\\d{4}-\\d{2}-\\d{2}"))


## Now select your two speakers of interest
# Example: Penny Mordaunt and Gavin Newlands
## NB: use regular expression to extract surname

speech_data <- out %>%
  filter(grepl('Mordaunt', speakers) | grepl('Newlands', speakers))

# save your .csv file - NB: have you set your working directory (setwd()) to the right folder on your computer??
write_excel_csv(speech_data, "scraped_speech_data.csv", col_names = TRUE)
