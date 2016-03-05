# Interactions with Presidential candidates' Facebook pages

setwd("~/Dropbox/research/candidates")

library(dplyr)
library(ggplot2)
library(ggthemes)
library(RColorBrewer)

data_list <- list()
for (i in 1:length(list.files("data"))){
    data_list[[i]] <- readr::read_csv(paste0("data/", list.files("data")[i]))
}
list.files("data")

names(data_list) <- c("Bernie Sanders", "Donald Trump", "Hillary Clinton",
                      "John Kasich", "Marco Rubio", "Ted Cruz")

data <- plyr::ldply(data_list)

data_ss <- data %>%
    mutate(year = lubridate::year(status_published),
           month = lubridate::month(status_published, label = T)) %>%
    group_by(year, month, .id) %>%
    summarize(num = sum(num_likes) + sum(num_comments) + sum(num_shares))

data %>%
    group_by(.id) %>%
    summarize(count = n())
           
              normalized_num = count) %>%
    mutate(new_date = paste0(month, ", ", year)) %>%
    filter(year > 2013 & new_date != "Mar, 2016")
str(data_ss)
data_ss$new_date <- factor(data_ss$new_date, levels=unique(data_ss$new_date))

ggplot(data_ss, aes(x = new_date, y = num, color = .id)) +
    geom_line(aes(group = .id)) +
    scale_y_continuous(labels = scales::comma) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    theme(axis.text.y = element_text(angle = 45, hjust = 1)) +
    ylab(NULL) +
    xlab(NULL) +
    theme(text =element_text(size = 15, family = "Georgia")) +
    ggtitle("Interactions with United States Presidential Candidate's Facebook Pages") +
    theme(legend.position = "bottom") +
    labs(color = NULL) +
    scale_color_calc()

ggsave("candidates.png")
