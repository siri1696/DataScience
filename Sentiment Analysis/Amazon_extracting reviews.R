setwd("E:\\Bokey\\Excelr Data\\R Codes\\Text Mining_R")
library(rvest)
library(xml2)
library(magrittr)

# Amazon Reviews #############################
aurl <- "https://www.amazon.in/Apple-iPhone-11-128GB-Black/product-reviews/B07XVLW7YK/ref=cm_cr_getr_d_paging_btm_prev_1?ie=UTF8&reviewerType=all_reviews&pageNumber"
amazon_reviews <- NULL
for (i in 1:30){
  murl <- read_html(as.character(paste(aurl,i,sep="")))
  rev <- murl %>%
    html_nodes(".review-text") %>%
    html_text()
  amazon_reviews <- c(amazon_reviews,rev)
}
write.table(amazon_reviews,"apple11.txt",row.names = F)

