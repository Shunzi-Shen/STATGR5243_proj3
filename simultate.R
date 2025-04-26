library(tidyverse)
set.seed(123)

# ======================
# 1. Parameter Settings
# ======================
n_groups <- 4
users_per_group <- 100
total_users <- n_groups * users_per_group

# Scoring Rules
points_click <- 10    # 10 points per click
points_task <- 50     # 50 points per task completed
points_page <- 5      # 5 points per page entry
points_eda <- 100     # 100 points for EDA completion

# ======================
# 2. Generate Combined Dataset (includes EDA completion)
# ======================
combined_data <- tibble(
  user_id = sprintf("u_%04d", 1:total_users),
  group = rep(c("blue-small", "blue-large", "red-small", "red-large"), 
              each = users_per_group),
  
  # Generate user-level session counts
  sessions = map(users_per_group * n_groups, ~ sample(1:5, 1)),
  
  # Generate session-level data
  session_data = map2(user_id, group, ~ {
    n_sessions <- sample(1:5, 1)
    tibble(
      session_id = sprintf("%s_s%02d", .x, 1:n_sessions),
      
      # Whether user enters the Feature Engineering page (probability varies by group)
      enter_feature = rbinom(n_sessions, 1, 
                             case_when(
                               .y == "blue-small" ~ 0.7,
                               .y == "blue-large" ~ 0.75,
                               .y == "red-small"  ~ 0.8,
                               .y == "red-large"  ~ 0.85
                             )),
      
      # Number of button clicks (conditional on page entry)
      button_clicks = ifelse(enter_feature == 1,
                             rpois(n_sessions, 
                                   case_when(
                                     .y == "blue-small" ~ 3,
                                     .y == "blue-large" ~ 4,
                                     .y == "red-small"  ~ 5,
                                     .y == "red-large"  ~ 6
                                   )),
                             0),
      
      # Whether task was completed (depends on clicks)
      task_complete = rbinom(n_sessions, 1, 
                             plogis(button_clicks * 0.2)),
      
      # Whether user reached the EDA tab
      reach_eda = rbinom(n_sessions, 1, 
                         case_when(
                           .y == "blue-small" ~ 0.3,
                           .y == "blue-large" ~ 0.4,
                           .y == "red-small"  ~ 0.5,
                           .y == "red-large"  ~ 0.6
                         )),
      
      # Whether the user bounced (left early)
      bounce = rbinom(n_sessions, 1, 
                      case_when(
                        .y == "blue-small" ~ 0.25,
                        .y == "blue-large" ~ 0.2,
                        .y == "red-small"  ~ 0.15,
                        .y == "red-large"  ~ 0.1
                      ))
    )
  })
) %>%
  unnest(session_data) %>%
  
  # ======================
# 3. Compute User-Level Metrics
# ======================
group_by(user_id) %>%
  mutate(
    points_page = sum(enter_feature) * points_page,
    points_clicks = sum(button_clicks) * points_click,
    points_tasks = sum(task_complete) * points_task,
    points_eda = sum(reach_eda) * points_eda,
    
    # Total score including baseline bonus by group
    total_points = points_page + points_clicks + points_tasks + points_eda +
      case_when(
        group == "blue-small" ~ rnorm(1, 200, 50),
        group == "blue-large" ~ rnorm(1, 250, 50),
        group == "red-small"  ~ rnorm(1, 300, 50),
        group == "red-large"  ~ rnorm(1, 350, 50)
      ),
    
    # User-level EDA completion (at least one session)
    eda_completion = as.integer(any(reach_eda == 1))
  ) %>%
  ungroup() %>%
  
  # ======================
# 4. Compute Additional Metrics
# ======================
mutate(
  ctr = button_clicks / pmax(enter_feature, 1),  # Prevent divide-by-zero
  pre_click_bounce = ifelse(bounce == 1 & button_clicks == 0, 1, 0)
) %>%
  group_by(user_id) %>%
  mutate(
    avg_task_time = ifelse(mean(enter_feature) > 0, 
                           mean(rnorm(n(), 300, 50) - 50 * (group == "red-large")),
                           0)
  ) %>%
  ungroup() %>% 
  mutate(ctr = 0.1 * ctr)

# ======================
# 5. Data Validation (EDA Completion Rate Check)
# ======================
combined_data %>%
  distinct(user_id, .keep_all = TRUE) %>%
  group_by(group) %>%
  summarise(
    eda_rate = mean(eda_completion),
    mean_eda_points = mean(points_eda)
  )

# ======================
# 6. Save Dataset
# ======================
write_csv(combined_data, "/Users/xiongchang/Documents/ab_test_data_with_eda.csv")
