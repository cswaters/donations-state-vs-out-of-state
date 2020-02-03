library(tidyverse)

# add minimal theme with rotated x-axis labels
theme_rot_x <- theme_minimal() +
  theme(axis.text.x  = element_text(angle = 55, hjust = .9))
theme_set(theme_rot_x)


# Get FEC file (hosted on Dropbox)
west_url <- 'http://bit.ly/2OoPYbN'
west_donations_12 <- vroom::vroom(west_url)

# Columns to keep
data_cols <- c(
  'contributor_state',
  'contributor_employer',
  'contribution_receipt_amount',
  'contribution_receipt_date',
  'receipt_type_desc'
)

new_names <- c('state',
               'employer',
               'amount',
               'date',
               'desc')

df <- west_donations_12 %>%
  filter(is_individual == TRUE) %>%
  select(data_cols) %>%
  set_names(new_names)

# drop original data
rm(west_donations_12)

# table(df$receipt_type_desc, useNA = 'ifany')
tbl_sum <- df %>%
  group_by(state) %>%
  summarise(
    transactions = n(),
    total = sum(amount),
    avg = mean(amount),
    oneK_plus = mean(amount >= 1000)
  ) %>%
  arrange(desc(transactions))

# reorder states by top contributing states
df <- df %>%
  mutate(state = factor(state, levels = tbl_sum$state))

# top donating states
top_states <- tbl_sum %>%
  head(15) %>%
  pull(state)

# by daily contribution count
p1 <- df %>%
  filter(state %in% top_states) %>%
  count(date, state, name = 'donations') %>%
  ggplot(aes(date, donations)) +
  geom_line(group = 1) +
  labs(
    x = '',
    y = 'Daily Contributions',
    title = 'Allen West 2012 Election - Daily Number Contributions (individual)',
    subtitle = 'Top 15 States by Number Contributions'
  ) +
  facet_wrap(vars(state),
             nrow = 5,
             scales = 'free_y') +
  scale_x_datetime(date_breaks = "3 months",
                   date_labels = '%b-%y')

nested_df <- df %>%
  filter(state %in% top_states) %>%
  group_nest(state)

get_running_sum <- . %>%
  arrange(date) %>%
  mutate(cum_amt = cumsum(amount)) %>%
  select(date, cum_amt)

# Running amount donations
p2 <- nested_df %>%
  mutate(d = map(data, get_running_sum)) %>%
  unnest(d) %>%
  select(-data) %>%
  ggplot(aes(date, cum_amt)) +
  geom_line(group = 1) +
  facet_wrap(vars(state), nrow = 5, scales = 'free_y') +
  labs(
    x = '',
    y = '',
    title = 'Allen West 2012 Election - Donation Amount (individuals)',
    subtitle = 'Top 15 States'
  ) +
  scale_x_datetime(date_breaks = "4 months",
                   date_labels = '%b-%y') +
  scale_y_continuous(labels = scales::dollar_format())

# Daily contribution amount
p3 <- df %>%
  filter(state %in% top_states) %>%
  group_by(state, date) %>%
  summarise(amt = sum(amount)) %>%
  ungroup() %>%
  ggplot(aes(date, amt)) +
  geom_line(group = 1) +
  scale_x_datetime(date_breaks = "4 months",
                   date_labels = '%b-%y') +
  scale_y_continuous(labels = scales::dollar_format()) +
  labs(
    x = '',
    y = 'Daily Contribution Amount',
    title = 'Allen West 2012 Election - Donation Amount per Day (individuals)',
    subtitle = 'Top 15 States'
  ) +
  facet_wrap(vars(state), nrow = 5, scales = 'free_y')

# Daily Contribution Amount Florida vs Out of State
p4 <- df %>%
  group_by(is_fl = state == 'FL', date) %>%
  summarise(donations = n(),
            amt = sum(amount)) %>%
  mutate(state = ifelse(is_fl, 'Florida', 'Outside Florida')) %>%
  na.omit() %>%
  ggplot(aes(date, amt)) +
  geom_line(group = 1) +
  facet_wrap(vars(state)) +
  labs(x = '', y = 'Daily Contribution Amount',
       title = 'Allen West 2012 - Donations Out of State by Day') +
  scale_x_datetime(date_breaks = "4 months",
                   date_labels = '%b-%y') +
  scale_y_continuous(labels = scales::dollar_format())

# florida vs outside florida donations by day
p4 <- df %>%
  group_by(is_fl = state == 'FL', date) %>%
  summarise(donations = n(),
            amt = sum(amount)) %>%
  mutate(state = ifelse(is_fl, 'Florida', 'Outside Florida')) %>%
  na.omit() %>%
  ggplot(aes(date, amt)) +
  geom_line(group = 1) +
  facet_wrap(vars(state)) +
  labs(x = '', y = 'Daily Contribution Amount',
       title = 'Allen West 2012 - Donations Out of State by Day') +
  scale_x_datetime(date_breaks = "4 months",
                   date_labels = '%b-%y') +
  scale_y_continuous(labels = scales::dollar_format())

## Florida vs Outside Florida bar
p5 <- df %>%
  group_by(is_fl = state == 'FL') %>%
  na.omit() %>%
  mutate(state = ifelse(is_fl,
                        'Florida',
                        'Outside Florida')) %>%
  ggplot(aes(state, amount)) +
  geom_col(color = '#663399') +
  scale_y_continuous(labels = scales::dollar_format()) +
  coord_flip() +
  labs(
    x = '',
    y = '',
    title = 'Allen West 2012 - Total Contributions',
    subtitle = 'Florida vs Outside Florida'
  )

ggsave(
  plot = p1,
  filename = 'top15_daily.png',
  width = 10,
  height = 7
)
ggsave(
  plot = p2,
  filename = 'top15_cum_amt.png',
  width = 10,
  height = 7
)
ggsave(
  plot = p3,
  filename = 'top15_daily_amt.png',
  width = 10,
  height = 7
)
ggsave(
  plot = p4,
  filename = 'fl_daily_amt.png',
  width = 10,
  height = 7
)
ggsave(
  plot = p5,
  filename = 'fl_tot_amt.png',
  width = 10,
  height = 7
)
