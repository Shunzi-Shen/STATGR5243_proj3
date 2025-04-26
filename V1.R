
rm(list=ls())

library(tidyverse)
library(patchwork)  # For combining plots
library(ggeffects)  # For model effects
library(interactions) 
library(ggpubr)  
library(GGally)
library(car)

# Set visualization theme
theme_set(theme_classic(base_size = 12) + 
            theme(legend.position = "bottom",
                  plot.title = element_text(face = "bold")))

# ======================
# 1. Data Loading & Preprocessing
# ======================
df <- read_csv("ab_test_data_with_eda.csv") %>%
  mutate(
    group = factor(group, 
                   levels = c("blue-small", "blue-large", "red-small", "red-large")),
    color = str_extract(group, "^[a-z]+"),
    size = str_extract(group, "[a-z]+$"),
    across(c(color, size), factor)
  )

# Inspect data structure
glimpse(df)



# ======================
# 2. Data Quality Checks
# ======================
# Missing value detection
missing_summary <- df %>%
  summarise(across(everything(), ~ sum(is.na(.x)))) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "missing_count")

print(missing_summary)

# ---------------------------
# 3. Outlier Detection & Removal
# ---------------------------
# Outlier detection
outlier_check <- df %>%
  dplyr::select(total_points, ctr, avg_task_time) %>%
  pivot_longer(everything()) %>%
  group_by(name) %>%
  summarise(
    mean = mean(value, na.rm = TRUE),
    sd = sd(value, na.rm = TRUE),
    min = min(value, na.rm = TRUE),
    max = max(value, na.rm = TRUE),
    outlier_low = mean - 3*sd,
    outlier_high = mean + 3*sd,
    n_outliers = sum(value < outlier_low | value > outlier_high)
  )

print(outlier_check)

# Calculate 3σ boundaries
ctr_low <- mean(df$ctr) - 3 * sd(df$ctr)
ctr_high <- mean(df$ctr) + 3 * sd(df$ctr)
time_low <- mean(df$avg_task_time, na.rm = TRUE) - 3 * sd(df$avg_task_time, na.rm = TRUE)
time_high <- mean(df$avg_task_time, na.rm = TRUE) + 3 * sd(df$avg_task_time, na.rm = TRUE)

# Remove outliers
df_clean <- subset(df, 
                   ctr >= ctr_low & ctr <= ctr_high &
                     avg_task_time >= time_low & avg_task_time <= time_high)
df <- df_clean





# ======================
# 4.Descriptive Statistics by Group
# ======================
descriptive_summary <- df %>%
  distinct(user_id, .keep_all = TRUE) %>%
  group_by(group) %>%
  summarise(
    mean_ctr = mean(ctr),
    sd_ctr = sd(ctr),
    mean_completion = mean(as.numeric(eda_completion)),
    mean_task_time = mean(avg_task_time),
    sd_task_time = sd(avg_task_time),
    mean_bounce = mean(as.numeric(bounce)),
    mean_total_points = mean(total_points),
    sd_total_points = sd(total_points),
    .groups = "drop"
  )
print(descriptive_summary)


# ======================
# 5. Statistical Tests
# ======================

s04_df <- df %>%
  filter(str_detect(session_id, "s04")) %>%
  distinct(user_id, .keep_all = TRUE)

ggplot(s04_df, aes(x = ctr, fill = group)) +
  geom_density(alpha = 0.4) +
  labs(title = "CTR Distribution for s04 Users") +
  scale_fill_brewer(palette = "Set2")
ggsave("eda_ctr_s04_density.png", width = 8, height = 5)



s04_df <- s04_df %>%
  mutate(
    color = factor(str_extract(group, "^[a-z]+")),
    size = factor(str_extract(group, "[a-z]+$"))
  )

anova_ctr_s04 <- aov(ctr ~ color * size, data = s04_df)

shapiro.test(residuals(anova_ctr_s04))  # p > 0.05 合理
ggqqplot(residuals(anova_ctr_s04))   

leveneTest(ctr ~ color * size, data = df %>% filter(str_detect(session_id, "s04")))

summary(anova_ctr_s04)


df <- df %>% mutate(eda_completion = factor(eda_completion))

logit_model_all <- glm(
  eda_completion ~ color * size,
  family = binomial(link = "logit"),
  data = df %>% distinct(user_id, .keep_all = TRUE)
)
summary(logit_model_all)


effects_all <- ggpredict(logit_model_all, terms = c("color", "size"))
plot(effects_all) +
  labs(title = "Predicted EDA Completion Rate by Design (All Data)")
ggsave("logit_eda_completion_effects_full.png", width = 7, height = 5)





time_model <- aov(avg_task_time ~ color * size, data = df %>% distinct(user_id, .keep_all = TRUE))



shapiro.test(residuals(time_model))  
ggqqplot(residuals(time_model))   

# Levene's Test for homogeneity of variance
leveneTest(avg_task_time ~ interaction(color, size), 
           data = df %>% distinct(user_id, .keep_all = TRUE))


summary(time_model)

# Visualization
plot_data_time <- df %>% distinct(user_id, .keep_all = TRUE)
ggplot(plot_data_time, aes(x = size, y = avg_task_time, fill = color)) +
  stat_summary(fun = mean, geom = "bar", position = "dodge") +
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.2,
               position = position_dodge(0.9)) +
  labs(title = "Mean Task Time by Color and Font Size",
       x = "Font Size", y = "Average Task Time (s)") +
  scale_fill_brewer(palette = "Set2")
ggsave("anova_time_color_size.png", width = 7, height = 5)


tukey_time<- TukeyHSD(time_model)
print(tukey_time)


# Convert bounce to factor
df <- df %>% mutate(bounce = factor(bounce))

# Logistic Regression: Bounce ~ color * size
logit_bounce <- glm(
  bounce ~ color * size,
  family = binomial(link = "logit"),
  data = df %>% distinct(user_id, .keep_all = TRUE)
)
summary(logit_bounce)

# Save summary
capture.output(summary(logit_bounce), file = "logit_bounce_summary.txt")

# Visualization of Predicted Bounce Rate
bounce_plot <- ggpredict(logit_bounce, terms = c("color", "size"))
plot(bounce_plot) +
  labs(title = "Predicted Bounce Rate by Design")
ggsave("logit_bounce_effects.png", width = 7, height = 5)




user_data_points <- df %>%
  distinct(user_id, .keep_all = TRUE)


anova_points <- aov(total_points ~ color * size, data = user_data_points)

shapiro.test(residuals(anova_points )) 
ggqqplot(residuals(anova_points ))   

leveneTest(total_points ~ interaction(color, size), data = user_data_points)


summary(anova_points)



ggplot(user_data_points, aes(x = size, y = total_points, fill = color)) +
  stat_summary(fun = mean, geom = "bar", position = "dodge") +
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.2,
               position = position_dodge(0.9)) +
  scale_fill_brewer(palette = "Set2") +
  labs(title = "Interaction Effect of Color and Size on Total Points",
       x = "Font Size", y = "Mean Total Points") +
  theme_classic()
ggsave("anova_total_points_interaction.png", width = 7, height = 6)



tukey_tp <- TukeyHSD(anova_points)
print(tukey_tp)





