library(dplyr)
library(rvest)

get_year_dates <- function(x){
    seq(
        as.Date(paste0(x, "-01-01")),
        as.Date(paste0(x, "-12-31")),
        by = 1
    )
    
}

years <- c(1995, 1998, 2002, 
           2006, 2010, 2011, 
           2014)

all_dates <- purrr::map(years, get_year_dates) %>% 
    unlist() %>% 
    as.Date(., origin = "1970-01-01")

scrape_lt_rate <- function(date){
    Sys.sleep(1)
    day <- lubridate::day(date)
    month <- lubridate::month(date)
    year <- lubridate::year(date)
    
    url <- glue::glue("https://www.lb.lt/exchange/Results.asp?Lang=E&id=126&ord=1&dir=ASC&M={month}&D={day}&Y={year}&DD=D&vykdyti=Submit&S=csv&x=77696")
    read.csv(url, header = FALSE)
}

lt_rates <- purrr::map(all_dates, ~scrape_lt_rate(.x), .progres = TRUE)
