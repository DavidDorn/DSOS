#Utility functions

pull_reddit <- function(subreddit = "") {
  page = fromJSON(paste("http://reddit.com/",subreddit,".json", sep=""))

  data = tibble(
    title = page$data$children$data$title,
    score = page$data$children$data$score,
    num_comments = page$data$children$data$num_comments,
    source = subreddit,
    pulled = Sys.time(),
    subreddit = page$data$children$data$subreddit,
    url = page$data$children$data$url,
    permalink = page$data$children$data$permalink,
    created_utc = page$data$children$data$created_utc
  ) %>%
    mutate_at(4, funs(replace(., is.na(.), "frontpage"))) %>%
    mutate(created_utc = as_datetime(pulled))

  return(data)
}



pull_top_level_comments <- function(reddit_links) {

  progress <- txtProgressBar(min = 0, max = length(reddit_links), style = 3)

  variables <- c(
    "created_utc",
    "permalink",
    "body",
    "score",
    "downs",
    "author",
    "is_submitter",
    "gilded",
    "stickied"
  )

  content <- fromJSON(paste("http://reddit.com",reddit_links[1],".json", sep=""))

  reddit_comment_data <- as_data_frame(select(content$data$children[[2]]$data,variables)) %>%
    filter(!is.na(body)) %>% mutate(datetime = as_datetime(created_utc)) %>%
    mutate(date = date(datetime)) %>% mutate(permalink_comment = permalink) %>%
    mutate(permalink = reddit_links[i])


  for (i in 2:length(reddit_links)) {

    tryCatch({
      content <- fromJSON(paste("http://reddit.com",reddit_links[i],".json", sep=""))
    }, error=function(e){cat("ERROR :",conditionMessage(e), "failed on", i)})
    if (!is.null(content$data$children[[2]]$data)) {

      data <- as_data_frame(select(content$data$children[[2]]$data,variables)) %>%
        filter(!is.na(body)) %>% mutate(datetime = as_datetime(created_utc)) %>%
        mutate(date = date(datetime)) %>% mutate(permalink_comment = permalink) %>%
        mutate(permalink = reddit_links[i])

      reddit_comment_data <- rbind(reddit_comment_data, data)
    } else{next}


    setTxtProgressBar(progress, i)
    print(sprintf("Finished post %d of %d",i, length(reddit_links)))

  }

  return(reddit_comment_data)

}
