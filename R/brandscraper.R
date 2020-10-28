library(rvest)
numbers = seq(0,9,1)
letters = LETTERS("A", "Z",1)
index = c(numbers, letters)

obs <- c(113,506,424,399,260,125,57,129,59,107,
  23898,15628,24035,11804,
  11121,14229,10449,8009,
  9691, 3799,7608, 9592,
  17898, 8351,5610, 15452,
  1198, 10406,23852,14568,
  5526, 6933,5059, 824,
  1449, 1997)

collect <- tibble(
  category = index,
  obs = obs,
  n_pages = ceiling(obs/50)
)



data <- tibble("brandname")


for (k in 21:length(collect$category)) {

  tryCatch({
  for (i in 0:(collect$n_pages[k]-1)) {
    data2 <- read_html(str_c("https://www.brandsoftheworld.com/logos/letter/",collect$category[k], "?page=", i)) %>% html_nodes(".twoCols li , .row a") %>%
      html_text()
    data <- c(data,data2)
    print(paste("Finished page", i, "of category", index[k]))
    Sys.sleep(1)
  }}, error = function(e){message('Caught an error!', print(e))})

}
brandnames <- data %>% unlist() %>% unique()
saveRDS(brandnames, "brandnames.Rds")




