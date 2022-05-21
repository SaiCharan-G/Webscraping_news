library(tidyverse) ## to cleaning the data    
library(rvest) ## for web scraping statics websites 
install.packages("RSelenium")  ## for websites with load more buttons
library(RSelenium)
library(robotstxt) ## Test the if web scrapping is allowed or not 
library(netstat) ## to create a free network port
library(stringr) ## filter strings
library(xlsx) ## save as excel file 
library(usethis) ## to update the code in github


paths_allowed("https://www.reuters.com/news/archive/americasMergersNews")



MA <- tibble(page_num = 1:5) #number of pages of to scrape

MA <- MA %>%
  mutate(
    page = paste0(
      "https://www.reuters.com/news/archive/euMergersNews?view=page&page=",
      page_num,
      "&pageSize=10"
    )
  )  ## replace 1 with required numbers for iteration 

##function to extract links
get_links <- function(page) {
  page <- read_html(page)
  links <-
    page %>% html_nodes(".col-10 .story-content a") %>% html_attr('href') %>%  
    paste("https://www.reuters.com/", .,sep="")  %>% as.tibble()
}



##extracting titles 
get_titles <- function(page) {
  page <- read_html(page)
  titles <- page %>%
    html_nodes(".col-10 .story-title") %>% 
    html_text() %>% as.tibble() 
}
##extarcting timestap
get_time <-
  function(page) {
    page <- read_html(page)
    pagetime <- page %>% html_nodes(".timestamp") %>% html_text()
    pagetime %>% tibble()
  }

### Extracting links from multiple pages at once
finallinks <- MA %>%
  mutate(links = map(page, get_links)) %>%
  unnest(links) %>% 
  rename(pagelinks = value) 


id <- seq(1,length(finallinks$pagelinks),1) ## creating an unique ID for each link  

finallinks <-  finallinks %>% mutate(id = id)

### Extracting tittles from multiple pages at once
finaltitles <- MA %>%
  mutate(titles = map(page, get_titles)) %>%
  unnest(titles) %>% 
  rename(pagetitles = value) %>%
  mutate(id = id)

### Extracting date or time from multiple pages at once  
finaldate <- MA %>%
  mutate(date = map(page, get_time)) %>%
  unnest(date) %>%
  mutate(id = id)  



reutersfull <- merge(finallinks,finaltitles, by=c("id","page","page_num"))
reutersfull <- merge(reutersfull,finaldate, by=c("id","page","page_num")) %>%
  arrange(page_num)

reuterssubset <-  reutersfull %>% 
  filter(str_detect(pagetitles, "acqu| merge|sold|buy|"))

###dynamic website 
paths_allowed("https://www.newsnow.co.uk/h/Business+&+Finance/Corporate+Finance/Mergers+&+Acquisitions")
page <- "https://www.newsnow.co.uk/h/Business+&+Finance/Corporate+Finance/Mergers+&+Acquisitions"
# page <- read_html(page)
# links <-  page %>% html_nodes(".js-newsmain .hll") %>% html_attr('href')
# Sys.sleep(2)
# titles <- page %>% html_nodes(".js-newsmain .hll") %>% html_text()
# length(titles)
# tail(titles)

RSelenium::rsDriver()
## to run chrome in developer mode
rs_driver_object <- rsDriver(browser = 'chrome',
                             chromever = '101.0.4951.15',
                             verbose= F,
                             port= free_port()
)

remDr <- rs_driver_object$client  ## call the website as a client
remDr$navigate('https://www.newsnow.co.uk/h/Business+&+Finance/Corporate+Finance/Mergers+&+Acquisitions')

clickmultiple <- function(fivetimes) {remDr$findElement( using = 'xpath',  
                                                         '//*[text()="view more headlines"]')$clickElement()}
##function to click load more headlines 

lapply(1:10, clickmultiple)  ## click the loadmore button 5 times

titles <-remDr$findElements(using = "class","hll") ## extract titles  
datetime <- remDr$findElements(using = "class", value = "time") ## extract time


title <- lapply(titles, function (x) x$getElementText()) %>% unlist() 
title <-  data.frame(title) 
id <-  seq(1,length(title$title),1)  ## create an unique id for merger 
title <- title %>% mutate(id = id)

links <- lapply(titles, function (x) x$getElementAttribute("href")) %>% unlist() ##extracting links
links <- data.frame(links) %>% mutate(id=id) ## create an unique id for merger 

date <- lapply(datetime, function (x) x$getElementText()) %>% unlist()
date <- data.frame(date) %>% mutate(id =seq(1,length(date),1))

newsnowfinal <- merge(title,links,by="id")

newsnowfulldata <- merge(newsnowfinal,date,by="id") %>% filter(title!= "")
requireddata <-  newsnowfulldata %>% 
  filter(str_detect(title, "acqu|merge|sold|buy")) 
write.xlsx(newsnowfulldata, file = "news.xlsx", sheetName="fullnewsnow",append=TRUE)
write.xlsx(requireddata, file = "news.xlsx",sheetName = "filterednewsnow",append=TRUE)
write.xlsx(reutersfull, file = "news.xlsx",sheetName = "fullredreuters",append=TRUE)
write.xlsx(reuterssubset, file = "news.xlsx",sheetName = "filteredreuters",append=TRUE)

