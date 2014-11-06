setwd("/path/to/this/file/")
library(ggplot2)
library(scales)
library(reshape2)
library(RPostgreSQL)
library(dplyr)
source("helpers.R")

posts = query("
  SELECT
    p.id, p.name, p.subreddit, p.posted_at, p.best_front_page_rank,
    p.first_observed_at, p.last_observed_at, p.best_front_page_rank_first_observed_at,
    p.max_score, p.max_num_comments, p.domain, p.imgur_views,
    ofirst.front_page_rank AS first_front_page_rank,
    ofirst.score AS first_score,
    ofirst.num_comments AS first_num_comments,
    olast.front_page_rank AS last_front_page_rank,
    olast.score AS last_score,
    olast.num_comments AS last_num_comments,
    obest.score AS score_at_best_rank,
    obest.num_comments AS num_comments_at_best_rank
  FROM
    posts p
      INNER JOIN observations ofirst ON p.id = ofirst.post_id AND p.first_observed_at = ofirst.observed_at
      INNER JOIN observations olast ON p.id = olast.post_id AND p.last_observed_at = olast.observed_at
      INNER JOIN observations obest ON p.id = obest.post_id AND p.best_front_page_rank_first_observed_at = obest.observed_at
  ORDER BY p.id
")

observations = query("
  SELECT
    o.post_id,
    o.observed_at,
    o.front_page_rank,
    o.score,
    o.num_comments,
    o.best_previous_front_page_rank,
    p.best_front_page_rank,
    p.subreddit,
    p.posted_at,
    p.domain
  FROM
    posts p
      INNER JOIN observations o ON p.id = o.post_id
  ORDER BY o.post_id, o.observed_at
")

posts = convert_timezones_to_utc(posts)
posts$made_front_page = posts$best_front_page_rank <= 25
observations = convert_timezones_to_utc(observations)
observations$made_front_page = observations$best_front_page_rank <= 25
observations$decline_from_best_previous_rank = observations$front_page_rank - observations$best_previous_front_page_rank

conditional_probs = summarize(group_by(filter(observations, best_previous_front_page_rank > 25 & decline_from_best_previous_rank <= 2), front_page_rank),
                              p_top_25 = mean(made_front_page),
                              count = n())

png(filename = "graphs/conditional_probability.png", width = 640, height = 480)
ggplot(data = conditional_probs, aes(x = front_page_rank, y = p_top_25)) +
  geom_bar(stat = "identity", width = 0.845) +
  scale_y_continuous("Probability of reaching top 25\n", labels = percent, lim = c(0, 1), minor_breaks = seq(0, 1, by = 0.05)) +
  scale_x_top_100("\nCurrent rank", breaks = seq(25, 100, by = 5), n_spaces = 21) +
  labs(title = "Conditional probability of a reddit post reaching the\nlogged-out homepage, given that it's in the top 100\nand its rank is not declining\n") +
  theme_gray(base_size = 18)
dev.off()

decline_probs = summarize(group_by(filter(observations, best_previous_front_page_rank > 25), decline = pmin(decline_from_best_previous_rank, 10)),
                          p_top_25 = mean(made_front_page),
                          count = n())

png(filename = "graphs/declining_conditional_probability.png", width = 640, height = 480)
ggplot(data = decline_probs, aes(x = decline, y = p_top_25)) +
  geom_bar(stat = "identity") +
  scale_y_continuous("Probability of reaching top 25\n", labels = percent) +
  scale_x_continuous("\nDecline from best previous rank", breaks = (0:5) * 2, labels = c((0:4) * 2, "10+")) +
  labs(title = "Conditional probability of reaching top 25 as a function of\ndecline from best previous rank, given in top 100\n") +
  theme_gray(base_size = 18)
dev.off()

# histograms for best ranks and all observed ranks

png(filename = "graphs/best_rank_histogram.png", width = 640, height = 360)
ggplot(data = posts, aes(x = best_front_page_rank)) +
  geom_histogram(binwidth = 1) +
  scale_x_top_100("\nBest rank") +
  scale_y_continuous("Number of posts\n") +
  labs(title = paste0("Distribution of best rank for ", nrow(posts), " posts observed in\nreddit top 100 between Sep 18 and Oct 31, 2014\n")) +
  theme_gray(base_size = 18)
dev.off()

png(filename = "graphs/best_rank_histogram_funny.png", width = 640, height = 360)
ggplot(data = filter(posts, subreddit == "funny"), aes(x = best_front_page_rank)) +
  geom_histogram(binwidth = 1) +
  scale_x_top_100("\nBest rank") +
  scale_y_continuous("Number of posts\n") +
  labs(title = "Distribution of best rank for posts in r/funny\n") +
  theme_gray(base_size = 18)
dev.off()

for (sr in c("funny", "personalfinance", "dataisbeautiful")) {
  png(filename = paste0("graphs/rank_histogram_", sr, ".png"), width = 640, height = 360)
  p = ggplot(data = filter(observations, subreddit == sr), aes(x = front_page_rank)) +
        geom_histogram(binwidth = 1, aes(y = ..density..)) +
        scale_x_top_100("\nFront page rank") +
        scale_y_continuous("Observation frequency\n") +
        labs(title = paste0("Observed ranks of posts in r/", sr, "\n")) +
        theme_gray(base_size = 18)
  print(p)
  dev.off()
}

top_subreddits = ordered_table(posts$subreddit)
top_front_page_subreddits = ordered_table(filter(posts, made_front_page)$subreddit)

subreddit_data = data.frame(subreddit = names(top_subreddits),
                            posts = as.numeric(top_subreddits),
                            top_25_posts = as.numeric(top_front_page_subreddits[names(top_subreddits)]))

subreddit_data$top_25_posts[is.na(subreddit_data$top_25_posts)] = 0
subreddit_data$frac_in_top_25 = subreddit_data$top_25_posts / subreddit_data$posts

top_names = names(top_subreddits[top_subreddits > 50])
posts$subreddit_factor = factor(posts$subreddit, levels=names(top_subreddits))
levels(posts$subreddit_factor)[!(levels(posts$subreddit_factor) %in% top_names)] = "other"

observations$subreddit = factor(observations$subreddit, levels = names(top_subreddits))
observations$subreddit_factor = factor(observations$subreddit, levels = names(top_subreddits))
levels(observations$subreddit_factor)[!(levels(observations$subreddit_factor) %in% top_names)] = "other"

calc_histogram_for_kmeans = function(s) {
  hist(filter(observations, subreddit_factor == s)$front_page_rank,
       plot = FALSE, breaks = c(0, 10, 25, 40, 50, 75, 100))$density
}

subreddit_densities = t(sapply(levels(observations$subreddit_factor), calc_histogram_for_kmeans))

set.seed(1)
km = kmeans(subreddit_densities, centers = 3)

cluster_names = c("Cluster 1: most popular subreddits", "Cluster 2: page two subreddits", "Cluster 3: the rest")
observations$cluster = km$cluster[observations$subreddit_factor]
observations$cluster = factor(observations$cluster, levels = c(1, 2, 3), labels = cluster_names)
posts$cluster = km$cluster[posts$subreddit_factor]
posts$cluster = factor(posts$cluster, levels = c(1, 2, 3), labels = cluster_names)
subreddit_data$cluster = km$cluster[as.character(subreddit_data$subreddit)]

png(filename = "graphs/rank_histogram_top_48_in_color.png", width = 640, height = 1440)
ggplot(data = filter(observations, subreddit %in% names(top_subreddits)[1:48]), aes(x = front_page_rank, fill = cluster)) +
  geom_histogram(binwidth = 2, aes(y = ..density..)) +
  scale_x_continuous("\nObserved rank") +
  scale_y_continuous("Observation frequency\n", labels = NULL) +
  scale_fill_discrete("", guide = FALSE) +
  facet_wrap(~subreddit, ncol = 3, scales = "free_y") +
  labs(title = "Observed ranks by subreddit\n") +
  theme_gray(base_size = 12) +
  theme(axis.ticks.y = element_blank(),
        plot.title = element_text(size = 22),
        axis.title.y = element_text(size = 18))
dev.off()

png(filename = "graphs/rank_histogram_by_cluster.png", width = 640, height = 720)
ggplot(data = observations, aes(x = front_page_rank, fill = cluster)) +
  scale_x_top_100("\nObserved rank") +
  scale_y_continuous("Observation frequency\n") +
  scale_fill_discrete("", guide = FALSE) +
  geom_histogram(binwidth = 1, aes(y = ..density..)) +
  facet_wrap(~cluster, ncol = 1, scales = "free_y") +
  labs(title = "Observed ranks by subreddit cluster\n") +
  theme_gray(base_size = 18)
dev.off()

cluster_probs = summarize(group_by(filter(observations, best_previous_front_page_rank > 25 & decline_from_best_previous_rank <= 2), front_page_rank, cluster),
                          p_top_25 = mean(made_front_page),
                          count = n())

png(filename = "graphs/conditional_probability_by_cluster.png", width = 640, height = 720)
ggplot(data = filter(cluster_probs, count > 400), aes(x = front_page_rank, y = p_top_25)) +
  geom_vline(xintercept = c(50, 75), color = "#aaaaaa") +
  geom_bar(stat = "identity", width = 0.83) +
  scale_y_continuous("Probability of reaching top 25\n", labels = percent, lim = c(0, 1), minor_breaks = seq(0, 1, by = 0.05)) +
  scale_x_top_100("\nCurrent rank", breaks = seq(25, 100, by = 5), n_spaces = 21) +
  facet_wrap(~cluster, ncol = 1) +
  labs(title = "Conditional probability of reaching top 25, given in\ntop 100 and not in decline\n") +
  theme_gray(base_size = 18)
dev.off()

totals = mutate(
           group_by(
             summarize(
               group_by(observations, front_page_rank, cluster),
               count = n()
              ), front_page_rank),
            total = sum(count), frac = count / sum(count))

png(filename = "graphs/rank_distribution_by_cluster.png", width = 640, height = 640)
ggplot(data = totals, aes(x = front_page_rank, y = frac, fill = cluster)) +
  geom_area() +
  scale_y_continuous("% of total observations\n", labels = percent) +
  scale_x_top_100("\nFront page rank") +
  scale_fill_discrete("") +
  labs(title = "Distribution of subreddit clusters at each rank\n") +
  theme_gray(base_size = 18) +
  theme(legend.position = "bottom", legend.direction = "vertical")
dev.off()

png(filename = "graphs/best_score_histogram_top_100.png", width = 640, height = 480)
ggplot(data = posts, aes(x = pmin(max_score, 6000), fill = cluster)) +
  geom_histogram(binwidth = 125) +
  scale_x_continuous("\nScore", breaks = (0:6) * 1000, labels = c((0:5) * 1000, ">6000"), minor_breaks = seq(0, 6000, by = 250)) +
  scale_y_continuous("Number of posts\n", labels = comma) +
  scale_fill_discrete("") +
  labs(title = "Distribution of maximum score for reddit top 100\n") +
  theme_gray(base_size = 18) +
  theme(legend.position = "bottom", legend.direction = "vertical")
dev.off()

png(filename = "graphs/best_score_histogram_top_25.png", width = 640, height = 480)
ggplot(data = filter(posts, best_front_page_rank <= 25), aes(x = pmin(max_score, 6000), fill = cluster)) +
  geom_histogram(binwidth = 125) +
  scale_x_continuous("\nScore", breaks = (0:6) * 1000, labels = c((0:5) * 1000, ">6000"), minor_breaks = seq(0, 6000, by = 250)) +
  scale_y_continuous("Number of posts\n", labels = comma) +
  scale_fill_discrete("") +
  labs(title = "Distribution of maximum score for reddit top 25\n") +
  theme_gray(base_size = 18) +
  theme(legend.position = "bottom", legend.direction = "vertical")
dev.off()



rank_labels = paste0(1 + seq(0, 90, by = 10), "-", seq(10, 100, by = 10))
posts$rank_bucket = cut(posts$best_front_page_rank, breaks = seq(0, 100, by = 10), labels = rank_labels)

png(filename = "graphs/distribution_of_score_at_best_rank.png", width = 640, height = 480)
ggplot(data = posts, aes(x = rank_bucket, y = score_at_best_rank)) +
  geom_dotplot(binwidth = 50, binaxis = 'y', stackdir = 'center', dotsize = 0.06) +
  scale_y_continuous("Score\n", labels = comma) +
  scale_x_discrete("\nRank bucket") +
  labs(title = "Distribution of scores by rank bucket\n") +
  theme_gray(base_size = 16)
dev.off()

png(filename = "graphs/distribution_of_max_score.png", width = 640, height = 480)
ggplot(data = posts, aes(x = rank_bucket, y = pmin(max_score, 6000))) +
  geom_dotplot(binwidth = 50, binaxis = 'y', stackdir = 'center', dotsize = 0.1) +
  scale_y_continuous("Score\n", breaks = 2000 * (0:3), labels = c(0, "2,000", "4,000", ">6,000")) +
  scale_x_discrete("\nRank bucket") +
  labs(title = "Distribution of scores by rank bucket\n") +
  theme_gray(base_size = 16)
dev.off()

posts$first_age = as.numeric(difftime(posts$first_observed_at, posts$posted_at, units="hours"))
posts$age_at_best_rank = as.numeric(difftime(posts$best_front_page_rank_first_observed_at, posts$posted_at, units="hours"))
posts$last_age = as.numeric(difftime(posts$last_observed_at, posts$posted_at, units="hours"))
posts$time_in_top_100 = posts$last_age - posts$first_age

observations$logscore = log10(observations$score + 1)
observations$age = as.numeric(difftime(observations$observed_at, observations$posted_at, units = "hours"))
observations$scoreagediff = observations$logscore - observations$age
observations$on_page_2 = as.numeric(observations$front_page_rank >= 26 & observations$front_page_rank <= 50)

score_breaks = c(0, 200, 500, 1000, 2000, 3000, 4000, 10000)
score_labels = c("0-200", "200-500", "500-1k", "1k-2k", "2k-3k", "3k-4k", ">4k")

age_breaks = c(0, 4, 6, 8, 10, 12, 14, 36)
age_labels = c("0-4", "4-6", "6-8", "8-10", "10-12", "12-14", ">14")

observations$scorebucket = cut(observations$score, breaks = score_breaks, labels = score_labels)
observations$agebucket = cut(observations$age, breaks = age_breaks, labels = age_labels)

bucket_aggregates = summarize(group_by(filter(observations, best_previous_front_page_rank > 25 & decline_from_best_previous_rank <= 2),
                                       cluster, agebucket, scorebucket),
                              p_top_25 = mean(made_front_page),
                              score = mean(score),
                              age = mean(age),
                              count = n())

png(filename = "graphs/age_score_heatmap.png", width = 640, height = 640)
ggplot(data = filter(bucket_aggregates, count > 100), aes(x = agebucket, y = scorebucket, fill = p_top_25)) +
  geom_tile() +
  scale_y_discrete("Score\n") +
  scale_x_discrete("\nAge (hours)") +
  scale_fill_gradient("p(top 25)", low = "#000060", high = "#ff0000", labels = percent) +
  facet_wrap(~cluster, ncol = 1) +
  labs(title = "Probability of a post making the top 25 by age and score,\ngiven it's in the top 100 and not declining\n") +
  theme_bw(base_size = 16) +
  theme(axis.ticks = element_blank())
dev.off()

posts$imgur = regexpr("imgur.com", posts$domain) > 0
summarize(group_by(posts, cluster), imgur_frac = mean(imgur))

imgur = filter(posts, !is.na(imgur_views) & imgur_views > 0)

imgur_agg = summarize(group_by(imgur, best_front_page_rank),
                      mean = mean(imgur_views),
                      sd = sd(imgur_views),
                      median = median(imgur_views),
                      pct25 = quantile(imgur_views, 0.25),
                      pct75 = quantile(imgur_views, 0.75),
                      count = n())

imgur_agg$page = floor((imgur_agg$best_front_page_rank - 1) / 25) + 1
imgur_agg$exclude = imgur_agg$best_front_page_rank >= 34 & imgur_agg$best_front_page_rank <= 46
imgur_agg$grp = imgur_agg$best_front_page_rank < 34

png(filename = "graphs/imgur_pageviews.png", width = 640, height = 480)
ggplot(data = filter(imgur_agg, !exclude), aes(x = best_front_page_rank, y = median, ymin = pct25, ymax = pct75, group = grp)) +
  geom_line(aes(color = "median")) +
  geom_ribbon(aes(fill = "25-75\npctile"), alpha = 0.2) +
  scale_color_manual("", values = "#000000") +
  scale_fill_manual("", values = "#222222") +
  scale_y_continuous("Imgur pageviews\n", labels = comma) +
  scale_x_top_100("\nBest reddit rank", n_spaces = 12) +
  labs(title = "Imgur pageviews by reddit front page rank\n") +
  theme_gray(base_size = 18) +
  expand_limits(y = 0)
dev.off()


score_by_rank = summarize(group_by(filter(observations, decline_from_best_previous_rank <= 2), front_page_rank),
                          mean = mean(score),
                          sd = sd(score),
                          median = median(score),
                          pct25 = quantile(score, 0.25),
                          pct75 = quantile(score, 0.75),
                          count = n())

png(filename = "graphs/score_by_rank.png", width = 640, height = 400)
ggplot(data = score_by_rank, aes(x = front_page_rank, y = median, ymin = pct25, ymax = pct75)) +
  geom_line(aes(color = "median")) +
  geom_ribbon(aes(fill = "25-75 pctile"), alpha = 0.2) +
  scale_color_manual("", values = "#000000") +
  scale_fill_manual("", values = "#222222") +
  scale_y_continuous("Score\n", labels = comma) +
  scale_x_top_100("\nRank", n_spaces = 12) +
  labs(title = "Score distribution by front page rank, posts not in decline\n") +
  theme_gray(base_size = 18) +
  expand_limits(y = 0)
dev.off()

cluster_score_by_rank = summarize(group_by(filter(observations, decline_from_best_previous_rank <= 2), front_page_rank, cluster),
                                  mean = mean(score),
                                  sd = sd(score),
                                  median = median(score),
                                  pct25 = quantile(score, 0.25),
                                  pct75 = quantile(score, 0.75),
                                  count = n())

cluster_score_by_rank$page = floor((cluster_score_by_rank$front_page_rank - 1) / 25) + 1
cluster_score_by_rank$grp = (as.numeric(cluster_score_by_rank$cluster) == 1 & cluster_score_by_rank$page == 1)

png(filename = "graphs/cluster_score_by_rank.png", width = 640, height = 640)
ggplot(data = filter(cluster_score_by_rank, count > 200 & !(as.numeric(cluster) == 1 & page == 2)),
       aes(x = front_page_rank, y = median, ymin = pct25, ymax = pct75, group = grp)) +
  geom_line(aes(color = "median")) +
  geom_ribbon(aes(fill = "25-75 pctile"), alpha = 0.2) +
  scale_color_manual("", values = "#000000") +
  scale_fill_manual("", values = "#222222") +
  scale_y_continuous("Score\n", labels = comma) +
  scale_x_top_100("\nRank", n_spaces = 12) +
  labs(title = "Score distribution by front page rank, posts not in decline\n") +
  facet_wrap(~cluster, ncol = 1) +
  theme_gray(base_size = 18) +
  expand_limits(y = 0)
dev.off()

age_by_rank = summarize(group_by(filter(observations, decline_from_best_previous_rank <= 2), front_page_rank),
                        mean = mean(age),
                        sd = sd(age),
                        median = median(age),
                        pct25 = quantile(age, 0.25),
                        pct75 = quantile(age, 0.75),
                        count = n())

png(filename = "graphs/age_by_rank.png", width = 640, height = 400)
ggplot(data = age_by_rank, aes(x = front_page_rank, y = median, ymin = pct25, ymax = pct75)) +
  geom_line(aes(color = "median")) +
  geom_ribbon(aes(fill = "25-75 pctile"), alpha = 0.2) +
  scale_color_manual("", values = "#000000") +
  scale_fill_manual("", values = "#222222") +
  scale_y_continuous("Age (hours)\n", labels = comma) +
  scale_x_top_100("\nRank", n_spaces = 12) +
  labs(title = "Age distribution by front page rank, posts not in decline\n") +
  theme_gray(base_size = 18) +
  expand_limits(y = 0)
dev.off()






# logistic regression

observations_for_model = filter(observations, best_previous_front_page_rank > 25 & decline_from_best_previous_rank <= 2)

model = glm(made_front_page ~ cluster * (on_page_2 * front_page_rank + logscore + age),
            family = binomial(link = "logit"),
            data = observations_for_model)

summary(model)

observations_for_model$predicted = predict(model, type = "response")

results_by_rank = filter(as.data.frame(
                    summarize(
                      group_by(observations_for_model, cluster, front_page_rank),
                      actual = mean(made_front_page),
                      model = mean(predicted),
                      count = n()
                    )
                  ), count > 500)

results_by_rank_melted = filter(melt(results_by_rank, id = c("cluster", "front_page_rank")), variable != "count")

png(filename = "graphs/actual_vs_model.png", width = 640, height = 640)
ggplot(data = results_by_rank_melted, aes(x = front_page_rank, y = value, color = variable)) +
  geom_line() +
  scale_y_continuous("Probability of making top 25\n", labels = percent) +
  scale_x_top_100("\nRank", breaks = seq(25, 100, by = 25), n_spaces = 18) +
  scale_color_manual("", values = c("black", "red")) +
  facet_wrap(~cluster, ncol = 1) +
  labs(title = "Actual probability of top 25 vs model expected\n") +
  theme_gray(base_size = 18)
dev.off()


# experiment with tree-based models

library(rpart)
library(party)

rpart_model = rpart(made_front_page ~ cluster + on_page_2 + front_page_rank + logscore + age,
                    data = observations_for_model, method = "anova")

ctree_model = ctree(made_front_page ~ cluster + front_page_rank + logscore + age,
                    data = observations_for_model)

observations_for_model$rpart_predicted = predict(rpart_model)
observations_for_model$ctree_predicted = as.numeric(predict(ctree_model))

results_by_rank = filter(as.data.frame(
                    summarize(
                      group_by(observations_for_model, cluster, front_page_rank),
                      actual = mean(made_front_page),
                      glm_model = mean(predicted),
                      rpart_model = mean(rpart_predicted),
                      ctree_model = mean(ctree_predicted),
                      count = n()
                    )
                  ), count > 500)

results_by_rank_melted = filter(melt(results_by_rank, id = c("cluster", "front_page_rank")), variable != "count")

ggplot(data = results_by_rank_melted, aes(x = front_page_rank, y = value, color = variable)) +
  geom_line() +
  scale_y_continuous("Probability of making top 25\n", labels = percent) +
  scale_x_top_100("\nRank", breaks = seq(25, 100, by = 25), n_spaces = 18) +
  scale_color_manual("", values = c("black", "red", "green", "blue")) +
  facet_wrap(~cluster, ncol = 1) +
  labs(title = "Actual probability of top 25 vs model expected\n") +
  theme_gray(base_size = 18)
