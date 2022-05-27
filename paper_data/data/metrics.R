source('../data/setup.R')

evaluation_df <- data.frame(read.csv(file = '../data/csv/evaluation.csv'))

n <- evaluation_df %>%
  select(pos) %>%
  distinct() %>%
  slice_max(pos) %>%
  the()

expand <- function(.data) {
  num <- .data %>% slice(1) %>% the(num)
  pos <- .data %>%
    filter(pos > -1) %>%
    arrange(pos) %>%
    pull(pos)
  # generate n, count from pos
  count <- 0
  idx <- 1
  for (i in 1:n) {
    if (idx <= length(pos) && i == pos[idx] + 1) {
      idx <- idx + 1
      count[i + 1] <- count[i] + 1
    } else count[i + 1] <- count[i]
  }

  data.frame(n = 0:n, num = rep(num, n + 1), count = count)
}

expanded_df <- evaluation_df %>%
  group_by(name, type, metric) %>%
  group_modify(~.x %>% expand()) %>%
  ungroup()

beta <- 2
score_df <- expanded_df %>%
  mutate(pre = replace_na(count / n, 1), rec = count / num) %>%
  mutate(f1 = replace_na(2 * pre * rec / (pre + rec), 0),
         f2 = replace_na((1+beta^2) * pre * rec / (beta^2*pre + rec), 0))

avg_score_df <- score_df %>%
  group_by(type, metric, n) %>%
  summarise(pre = mean(pre), rec = mean(rec), f1 = mean(f1), f2 = mean(f2), .groups = 'drop')
