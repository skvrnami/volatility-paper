library(rvest)

d96 <- seq.Date(as.Date("1996-01-01"), 
                as.Date("1996-12-31"), by = 1)
d00 <- seq.Date(as.Date("2000-01-01"), 
                as.Date("2000-12-31"), by = 1)
d04 <- seq.Date(as.Date("2004-01-01"), 
                as.Date("2004-12-31"), by = 1)
d08 <- seq.Date(as.Date("2008-01-01"), 
                as.Date("2008-12-31"), by = 1)
d12 <- seq.Date(as.Date("2012-01-01"), 
                as.Date("2012-12-31"), by = 1)

dates <- c(d96, d00, d04, d08, d12) %>% 
    format(., "%Y%m%d") %>% 
    as.character()

parse_currency_rate <- function(x){
    tibble(
        id = x %>% html_node("id") %>% html_text(),
        units = x %>% html_node("units") %>% html_text() %>% 
            as.numeric(), 
        rate = x %>% html_node("rate") %>% 
            html_text() %>% 
            as.numeric()
    )
}

scrape_exchange_rate <- function(x){
    xml <- read_html(paste0("https://www.bank.lv/vk/xml.xml?date=", x))
    date <- xml %>% html_node("date") %>% 
        html_text() %>% 
        as.Date(., format = "%Y%m%d")
    
    tmp <- xml %>% 
        html_nodes("currency") %>% 
        purrr::map_df(., parse_currency_rate)
    
    tmp %>% 
        mutate(date = date)
}

lv_exchange_rate <- purrr::map_df(dates, scrape_exchange_rate)
saveRDS(lv_exchange_rate, "data/currency_data/lat/lat_exchange_rates.rds")


