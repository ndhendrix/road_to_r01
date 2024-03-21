# install ggsankey from github
devtools::install_github("davidsjoberg/ggsankey")

library(data.table)
library(tidyverse)
library(lubridate)
library(stringr)
library(tidyr)
library(ggsankey)

# IMPORTANT NOTE:
# To run this, you need to download all the project files from the NIH ExPORTER
# site at https://reporter.nih.gov/exporter (don't worry about the '85-'99
# updated files) and store them in a folder.

setwd('/path/to/your/output/folder')

file_list <- list.files(path = "/path/to/your/data",
                        full.names = TRUE)
processed_data_list <- lapply(file_list, function(file_name) {
  dt <- fread(file_name) %>%
    select(APPLICATION_ID, ACTIVITY, FULL_PROJECT_NUM, PI_IDS,
                      PROJECT_START, PROJECT_END) %>%
    filter(str_starts(ACTIVITY, "R") | str_starts(ACTIVITY, "K")) %>%
    mutate(PI_IDS = str_extract_all(PI_IDS, "\\d{7}"))
  dt[, PROJECT_START := parse_date_time(PROJECT_START, orders = c("mdy", "ymd"))]
  dt[, PROJECT_END := parse_date_time(PROJECT_END, orders = c("mdy", "ymd"))]
  return(dt)
})
combined_data <- rbindlist(processed_data_list, use.names = TRUE, fill = TRUE)
combined_data$FULL_PROJECT_NUM = sapply(combined_data$FULL_PROJECT_NUM, function(x) {
  str_split(str_sub(x, 2), pattern = '-')[[1]][1]
})
df <- combined_data %>%
  unnest(PI_IDS) %>%
  select(-APPLICATION_ID) %>%
  group_by(FULL_PROJECT_NUM, PI_IDS) %>%
  distinct() %>%
  mutate(ACTIVITY = 
           case_when(ACTIVITY %in% c('K01', 'K07', 'K08', 'K09', 'K11', 'K14', 
                                     'K15', 'K16', 'K22', 'K23', 'K25', 'K99', 'R00') ~ 'Early Career Development',
                     ACTIVITY %in% c('K02', 'K06', 'K24', 'K26', 'K04', 'K05', 'K26') ~ 'Mid/Senior Career Development',
                     startsWith(ACTIVITY, "K") ~ "Other K",
                     ACTIVITY == 'R01' ~ 'R01',
                     ACTIVITY %in% c('R03', 'R21') ~ 'Small Research Project',
                     ACTIVITY %in% c('R13', 'R25', 'R18', 
                                     'R15', 'R35', 'R24', 'R34') ~ 'Res. Education/Enhancement',
                     TRUE ~ 'Other R'))


# Identify PIs with an R01 project
pis_with_r01 <- df %>%
  filter(ACTIVITY == "R01") %>%
  distinct(PI_IDS) %>%
  pull(PI_IDS)

# Filter the data frame to include only PIs with an R01 project
df_filtered <- df %>%
  filter(PI_IDS %in% pis_with_r01)

# Identify the earliest R01 start date for each PI
earliest_r01_start <- df_filtered %>%
  select(-PROJECT_END) %>%
  filter(ACTIVITY == "R01") %>%
  group_by(PI_IDS) %>%
  summarise(earliest_r01_start = min(PROJECT_START)) %>%
  filter(earliest_r01_start >= "2000-01-01 00:00:00")

# Join the earliest R01 start dates back to the original data frame
df_with_earliest_r01 <- df_filtered %>%
  left_join(earliest_r01_start, by = "PI_IDS") %>%
  filter(earliest_r01_start >= "2000-01-01 00:00:00")

# Filter the data frame to include grants that started before or on the earliest R01 start date
grants_before_and_earliest_r01 <- df_with_earliest_r01 %>%
  filter(PROJECT_START <= earliest_r01_start | is.na(earliest_r01_start))

# Keep only the first event of each grant type for each PI
grants_first_event <- grants_before_and_earliest_r01 %>%
  group_by(PI_IDS, ACTIVITY) %>%
  arrange(PROJECT_START) %>%
  slice(1) %>%
  ungroup()

# Arrange grants in reverse chronological order for each PI
grants_reversed <- grants_first_event %>%
  group_by(PI_IDS) %>%
  arrange(desc(PROJECT_START)) %>%
  ungroup()

# Create a position variable for each grant based on its order before the R01
grants_positioned <- grants_reversed  %>%
  group_by(PI_IDS) %>%
  mutate(position = -row_number()) %>%
  ungroup() %>%
  mutate(position = ifelse(ACTIVITY == "R01", 0, position))  %>%
  filter(!(ACTIVITY == 'Other R' & position == 0)) %>%
  arrange(PI_IDS, PROJECT_START) %>% # Ensure the data is in the correct order
  group_by(PI_IDS) %>% # Assuming you want to lag within groups of ID
  mutate(next_ACTIVITY = lead(ACTIVITY)) %>%
  ungroup()

# Fix off-by-one error
grants_positioned[grants_positioned$position < 0, 'position'] = 
  grants_positioned[grants_positioned$position < 0, 'position'] + 1

# Sample 100 IDs to test plotting method
# grants_positioned <- grants_positioned %>%
#   distinct(PI_IDS) %>%
#   sample_n(100) %>%
#   inner_join(grants_positioned, by = "PI_IDS") 

grants_positioned$ACTIVITY = factor(grants_positioned$ACTIVITY,
                                    levels = c('Early Career Development',
                                               'Mid/Senior Career Development',
                                               'Other K',
                                               'Small Research Project',
                                               'Res. Education/Enhancement',
                                               'Other R',
                                               'R01'))

# Create the Sankey plot
ggplot(grants_positioned %>% filter(!(ACTIVITY == 'Other R' & position == 0)), 
       aes(x = position, 
           next_x = position + 1,
           node = ACTIVITY,
           next_node = next_ACTIVITY,
           label = ACTIVITY,
           fill = factor(ACTIVITY))) +
  geom_alluvial(flow.alpha = 0.7, node.color = "gray30", show.legend = TRUE) +
  scale_x_continuous(labels = function(x) abs(x)) +
  scale_fill_viridis_d(option = "plasma") +
  theme_alluvial() +
  labs(x = "Prior Grants",
       fill = "Grant Type") +
  theme(axis.text.y = element_blank(), 
        axis.ticks = element_blank(),
        legend.position = 'bottom')

# 1) When did the people who got an R01 w/o another R or K get the R01?
grants_counted = grants_positioned %>%
  group_by(PI_IDS) %>%
  summarize(n = n(),
            PROJECT_START = min(PROJECT_START)) %>%
  filter(n == 1)
grants_counted = merge(grants_counted,
                       grants_positioned %>% 
                         select(PI_IDS, PROJECT_START) %>% 
                         mutate(PROJECT_YEAR = year(PROJECT_START)),
                       all.x = TRUE)
nrow(grants_counted) / length(unique(grants_positioned$PI_IDS))
#0.5497186

ggplot(grants_counted %>% group_by(PROJECT_YEAR) %>% count(), 
       aes(x = PROJECT_YEAR, y = n)) +
  geom_line() +
  theme_bw()
  
# 2) Was it a solo R01?

PIs_per_proj = combined_data %>%
  filter(ACTIVITY == 'R01') %>%
  group_by(FULL_PROJECT_NUM) %>%
  summarise(Unique_PI_ID_Count = n_distinct(PI_IDS))
df_solo = merge(grants_positioned %>% filter(ACTIVITY == 'R01'),
                PIs_per_proj,
                all.x = TRUE)

nrow(df_solo %>% filter(Unique_PI_ID_Count > 1)) / nrow(df_solo)
#0.125

# 2B) How many of the R01s per year are first R01s?
first_r01_by_year = grants_positioned %>%
  filter(ACTIVITY == 'R01') %>%
  mutate(PROJECT_YEAR = year(PROJECT_START)) %>%
  group_by(PROJECT_YEAR) %>%
  mutate(n = n_distinct(FULL_PROJECT_NUM))
total_r01s = df_filtered %>%
  ungroup() %>%
  mutate(PROJECT_YEAR = year(PROJECT_START)) %>%
  filter(ACTIVITY == 'R01' & PROJECT_YEAR >= 2000) %>%
  group_by(PROJECT_YEAR) %>%
  mutate(n_total = n_distinct(FULL_PROJECT_NUM)) %>%
  select(n_total, PROJECT_YEAR) %>%
  distinct()
prop_first_r01 = merge(first_r01_by_year,
                       total_r01s,
                       all.x = TRUE) %>%
  mutate(prop = (n / n_total))
ggplot(prop_first_r01,aes(x = PROJECT_YEAR, y = prop)) +
  geom_line() +
  theme_bw() +
  xlab("Project Start Year") +
  ylab("Proportion of R01s with First-Time Awardee")

# 3) Case-control among people with r36s
processed_data_list <- lapply(file_list, function(file_name) {
  dt <- fread(file_name) %>%
    select(APPLICATION_ID, ACTIVITY, FULL_PROJECT_NUM, PI_IDS,
           PROJECT_START, PROJECT_END) %>%
    filter(str_starts(ACTIVITY, "R01") | str_starts(ACTIVITY, "K") 
           | str_starts(ACTIVITY,"R36") | str_starts(ACTIVITY, "F")) %>%
    mutate(PI_IDS = str_extract_all(PI_IDS, "\\d{7}"))
  dt[, PROJECT_START := parse_date_time(PROJECT_START, orders = c("mdy", "ymd"))]
  dt[, PROJECT_END := parse_date_time(PROJECT_END, orders = c("mdy", "ymd"))]
  return(dt)
})
combined_data <- rbindlist(processed_data_list, use.names = TRUE, fill = TRUE)
only_r36s = combined_data %>%
  unnest(PI_IDS) %>%
  filter(PI_IDS %in% (combined_data %>% 
                        filter(ACTIVITY %in% c("R36", "F30", "F31", "F32")) %>%
                        pull(PI_IDS)))

# select only people with r36 awards prior to 2015.
r36s <- only_r36s %>%
  filter(ACTIVITY %in% c("R36", "F30", "F31", "F32")) %>%
  group_by(PI_IDS) %>%
  arrange(PI_IDS, PROJECT_START) %>%
  filter(row_number() == 1) %>%
  filter(PROJECT_START < as.Date("2015-01-01")) %>%
  ungroup() %>%
  rename(r36_START = PROJECT_START)

k_awards <- only_r36s %>%
  filter(ACTIVITY %in% c('K01','K08', 'K23', 'K25')) %>%
  group_by(PI_IDS) %>%
  arrange(PI_IDS, PROJECT_START) %>%
  filter(row_number() == 1) %>%
  filter(PROJECT_START < as.Date("2015-01-01")) %>%
  ungroup() %>%
  rename(K_START = PROJECT_START)

r01s <- only_r36s %>%
  filter(ACTIVITY == "R01") %>%
  group_by(PI_IDS) %>%
  arrange(PI_IDS, PROJECT_START) %>%
  filter(row_number() == 1) %>%
  filter(PROJECT_START < as.Date("2015-01-01")) %>%
  ungroup() %>%
  rename(R01_START = PROJECT_START)

r36_and_k = merge(r36s, k_awards %>% select(PI_IDS, K_START), all.x = TRUE)
r36_k_and_r01 = merge(r36_and_k, r01s %>% select(PI_IDS, R01_START), all.x = TRUE) %>%
  filter(K_START < R01_START | is.na(K_START) | is.na(R01_START)) %>%
  mutate(K_START = as.numeric(!is.na(K_START)),
         R01_START = as.numeric(!is.na(R01_START)))

library(epitools)
dat <- data.frame(
  case_control = r36_k_and_r01$R01_START,
  exposure = r36_k_and_r01$K_START
)
result <- oddsratio(dat$case_control, dat$exposure)
print(result)
