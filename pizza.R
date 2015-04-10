library(jsonlite)
library(ggplot2)

d <- fromJSON("train.json")
test <- fromJSON("test.json")

clean.train <- function (d) {
  names(d) <- c("giver_name", "req_downvotes", "req_upvotes", "was_edited", "req_id", "req_comments_ret", "req_text", 
                 "req_text_edit", "req_title", "account_age", "account_age_retrieval", "raop_days", "raop_days_ret",
                 "reqer_comments", "reqer_comments_ret", "reqer_raop_comments", "reqer_raop_comments_ret", "reqer_posts",
                 "reqer_posts_ret", "reqer_raop_posts", "reqer_raop_posts_ret", "reqer_subred", "received", 
                 "subreddits", "reqer_umd", "reqer_umd_ret", "reqer_upd", "reqer_upd_ret", "reqer_flair", "reqer_name",
                 "time", "time_utc")
  d$was_edited[which(d$was_edited != 0)] <- 1
  
  d <- transform(d, text_length = nchar(d$req_text))
  d <- transform(d, date.time = as.POSIXct(d$time_utc, origin="1970-01-01"))
  d <- transform(d, hour = as.numeric(format(date.time, "%H")) + as.numeric(format(date.time, "%M"))/60)
  d <- transform(d, month = as.numeric(format(date.time, "%m")))
  d$weekday = weekdays(d$date.time)
  return(d)
}


clean.test <- function (d) {
  names(d) <- c("giver_name", "req_id", "req_text_edit", "req_title", "account_age", "raop_days", "reqer_comments",
                "reqer_raop_comments", "reqer_posts", "reqer_raop_posts", "reqer_subred", "subreddits", "reqer_umd",
                "reqer_upd", "reqer_name", "time", "time_utc")
  
  d <- transform(d, text_length = nchar(d$req_text))
  d <- transform(d, date.time = as.POSIXct(d$time_utc, origin="1970-01-01"))
  d <- transform(d, hour = as.numeric(format(date.time, "%H")) + as.numeric(format(date.time, "%M"))/60)
  d <- transform(d, month = as.numeric(format(date.time, "%m")))
  d$weekday = weekdays(d$date.time)
  return(d)
}

d <- clean.train(d)
test <- clean.test(test)

# potentially meaningful features
ggplot(d, aes(x = was_edited)) + geom_histogram() + facet_grid(received ~ .)
mean(d[which(d$received == 1), 'was_edited'])
mean(d[which(d$received == 0), 'was_edited'])

ggplot(d, aes(x= log1p(raop_days))) + geom_density() + facet_grid(received ~ .)
days.received <- d$raop_days[which(d$received == 1)]
mean(days.received)
days.not.received <- d$raop_days[which(d$received == 0)]
mean(days.not.received)

ggplot(d, aes(x= log1p(reqer_raop_comments))) + 
  geom_density() + facet_grid(received ~ .)
raop.comments.not.received <- d$reqer_raop_comments[which(d$received == 0)]
raop.comments.received <- d$reqer_raop_comments[which(d$received == 1)]
mean(raop.comments.received)
mean(raop.comments.not.received)


ggplot(d, aes(x= text_length)) +geom_density() + facet_grid(received ~ .)
mean(d[which(d$received == 1), 'text_length'])
mean(d[which(d$received == 0), 'text_length'])

ggplot(d, aes(x= log1p(reqer_raop_posts))) + 
  geom_density() + facet_grid(received ~ .)

lr <- glm(received ~ was_edited + log1p(raop_days) + log1p(reqer_raop_comments) + 
            text_length, data=d, family=binomial(logit))

mean(d$reqer_posts)


# not really promising
#ggplot(d, aes(x= log1p(req_upvotes))) + geom_density() + facet_grid(received ~ .)
#ggplot(d, aes(x= log1p(req_downvotes))) + geom_density() + facet_grid(received ~ .)

# small difference
#d.young <- d[which(d$account_age < 1000),]
#ggplot(d.young, aes(x= account_age)) + geom_density() + facet_grid(received ~ .)

# same
#ggplot(d, aes(x= log1p(reqer_subred))) + geom_density() + facet_grid(received ~ .)

#ggplot(d, aes(x= log1p(reqer_umd))) + geom_density() + facet_grid(received ~ .)
#ggplot(d, aes(x= log1p(reqer_upd))) + geom_density() + facet_grid(received ~ .)

#ggplot(d, aes(x= log1p(reqer_umd_ret))) + geom_density() + facet_grid(received ~ .)
#ggplot(d, aes(x= log1p(reqer_upd_ret))) + geom_density() + facet_grid(received ~ .)

#ggplot(d, aes(x= nchar(req_title))) +geom_density() + facet_grid(received ~ .)
#ggplot(d, aes(x=reqer_comments)) +geom_density() + facet_grid(received ~ .)


#ggplot(d, aes(x = hour)) + geom_density() + facet_grid(received ~ .)
#ggplot(d, aes(x = month)) + geom_density() + facet_grid(received ~ .)
#ggplot(d, aes(x = weekday)) + geom_density() + facet_grid(received ~ .)


