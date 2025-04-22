library(dplyr)
library(modelsummary)
library(tidyverse)
library(car)
library(lme4)
library(lmerTest)

# Load Data
prime <- read.csv("C:\\Users\\berm0006\\Documents\\First-Year Paper Stuff\\survey-prime2.csv")

# Remove Unnecessary Columns
prime <- prime %>%
  select(-matches("^PK(?!_Total$)", perl = TRUE))

prime <- prime %>%
  mutate(id = row_number())

prime <- prime %>% mutate(Direction = ifelse(group %in% c("A", "D"), "++ to --", "-- to ++")) # Create Direction column

prime <- prime %>% select(-D2_3_TEXT) # Remove D2_3_TEXT column

# Create New Table to Analyze Demographics
dem <- prime %>% select(all_of(c("Duration..in.seconds.", "PK_Total", "A1_1", "A2", "IC1_1", "IC2", "IC3_1", "IC4_1", 
                                 "Confidence_1", "Risk_1", "D1", "D2", 
                                 "D3", "D4", "D5", "group"))) %>%
  rename(
    Duration = Duration..in.seconds.,
    ps_level = PK_Total,
    Auditing_Knowledge = A1_1,
    Auditing_Internship = A2,
    IC_Knowledge = IC1_1,
    IC_Assessment = IC2,
    IC_Familiarity = IC3_1,
    IC_Effectiveness = IC4_1,
    Confidence = Confidence_1,
    Risk_Adverseness = Risk_1,
    Age = D1,
    Gender = D2,
    Graduate = D3,
    Major = D4,
    Audit_Courses = D5
  )

## A1_1 = Auditing Knowledge
## A2 = Auditing Internship
## IC1_1 = Internal Control Knowledge
## IC2 = Assessed Internal Controls?
## IC3 = Familiarity with Internal Controls
## IC4_1 = Internal Control Effectiveness
## Confidence_range = Confidence in assessment of internal controls
## Risk = Risk Adverseness
## D1 = Age
## D2 = Gender
## D3 = Undergraduate/Graduate
## D4 = Major
## D5 = # of audit courses taken
## D6 = Accelerated B/Macc student?

# Clean Data
## Remove Row 1 and Row 70
dem <- dem[-1, ]

## Factorize Columns
dem$Auditing_Knowledge <- as.factor(dem$Auditing_Knowledge)
dem$Auditing_Internship <- as.factor(dem$Auditing_Internship)
dem$IC_Knowledge <- as.factor(dem$IC_Knowledge)
dem$IC_Assessment <- as.factor(dem$IC_Assessment)
dem$IC_Familiarity <- as.factor(dem$IC_Familiarity)
dem$IC_Effectiveness <- as.factor(dem$IC_Effectiveness)
dem$Risk_Adverseness <- as.factor(dem$Risk_Adverseness)
dem$Gender <- as.factor(dem$Gender)
dem$Major <- as.factor(dem$Major)
dem$Graduate <- as.factor(dem$Graduate)
dem$Audit_Courses <- as.factor(dem$Audit_Courses)

# Data Summary
## Turn Confidence into a Range
dem <- dem %>%
  mutate(
    Confidence = as.numeric(as.character(Confidence)),  # convert from factor to numeric
    Confidence_range = case_when(
      Confidence < 20 ~ "0–19",
      Confidence < 40 ~ "20–39",
      Confidence < 60 ~ "40–59",
      Confidence < 80 ~ "60–79",
      TRUE             ~ "80–100"
    )
  )

## Counts per Range
dem %>%
  count(Confidence_range) %>%
  mutate(percent = round(100 * n / sum(n), 1))

## Remove Confidence_1
dem <- dem %>%
  select(-Confidence)

## Turn Age into a Range
dem <- dem %>%
  mutate(
    Age = as.numeric(as.character(Age)),  # convert from factor to numeric
    Age_range = case_when(
      Age < 20 ~ "0–19",
      Age < 30 ~ "20–29",
      Age < 40 ~ "30–39",
      Age < 50 ~ "40–49",
      TRUE             ~ "50+"
    )
  )

## Counts per Range
dem %>%
  count(Age_range) %>%
  mutate(percent = round(100 * n / sum(n), 1))

## Remove Age
dem <- dem %>%
  select(-Age)

## Turn Audit_Courses into a Range
dem <- dem %>%
  mutate(
    Audit_Courses = as.numeric(as.character(Audit_Courses)),  # convert from factor to numeric
    Audit_Courses_range = case_when(
      Audit_Courses < 1 ~ "0",
      Audit_Courses < 2 ~ "1",
      Audit_Courses < 3 ~ "2",
      Audit_Courses < 4 ~ "3",
      TRUE             ~ "4+"
    )
  )

## Counts per Range
dem %>%
  count(Audit_Courses_range) %>%
  mutate(percent = round(100 * n / sum(n), 1))

## Remove Audit_Courses
dem <- dem %>%
  select(-Audit_Courses)

## Turn Duration into Minutes
dem <- dem %>%
  mutate(
    Duration = as.numeric(as.character(Duration)),  # convert from factor to numeric
    Duration = Duration / 60  # convert seconds to minutes
  )

## Recode Factors
dem$Auditing_Knowledge <- fct_recode(dem$Auditing_Knowledge, "Very Little Knowledge" = "1", "Somewhat Knowledgeable" = "2", "Knowledgeable" = "3", "Very Knowledgeable" = "4")
dem$Auditing_Internship <- fct_recode(dem$Auditing_Internship, "Yes" = "1", "No" = "2")
dem$IC_Knowledge <- fct_recode(dem$IC_Knowledge, "Very Little Knowledge" = "1", "Somewhat Knowledgeable" = "2", "Knowledgeable" = "3", "Very Knowledgeable" = "4")
dem$IC_Assessment <- fct_recode(dem$IC_Assessment, "Yes" = "1", "No" = "2")
dem$IC_Familiarity <- fct_recode(dem$IC_Familiarity, "Very Unfamiliar" = "1", "Somewhat Unfamiliar" = "2", "Neutral" = "3", "Somewhat Familiar" = "4", "Very Familiar" = "5")
dem$IC_Effectiveness <- fct_recode(dem$IC_Effectiveness, "Not Confident" = "1", "Somewhat Confident" = "2", "Confident" = "3", "Very Confident" = "4", "Extremely Confident" = "5")
dem$Risk_Adverseness <- fct_recode(dem$Risk_Adverseness, "Extremely Uncomfortable" = "1", "Somewhat Uncomfortable" = "2", "Neutral" = "3", "Somewhat Comfortable" = "4", "Extremely Comfortable" = "5")
dem$Gender <- fct_recode(dem$Gender, "Male" = "1", "Female" = "2", "Prefer Not to Say" = "4")
dem$Graduate <- fct_recode(dem$Graduate, "Undergraduate" = "1", "Graduate" = "2")
dem$Major <- fct_recode(dem$Major, "Accounting" = "1", "Finance & Accounting" = "2")

## Re-code Skepticism
dem <- dem %>%
  mutate(Skepticism = ifelse(ps_level <= median(ps_level, na.rm = TRUE), "Low", "High"))

## Counts per Skepticism
dem %>%
  count(Skepticism) %>%
  mutate(percent = round(100 * n / sum(n), 1))

## Count of Skepticism by Group
dem %>%
  group_by(group, Skepticism) %>%
  summarise(count = n()) %>%
  mutate(percent = round(100 * count / sum(count), 1))

## Shorten Table
dem_short <- dem %>%
  select(
    Gender,
    Graduate,
    Skepticism,
    Auditing_Knowledge,
    Auditing_Internship,
    IC_Knowledge,
    Duration
  )

datasummary_skim(dem)
datasummary_skim(dem_short)

# Create Line Graphs Showing how Judgement Changes as New Evidence is Received
prime <- prime %>%
  mutate(Skepticism = ifelse(PK_Total <= median(PK_Total, na.rm = TRUE), "Low", "High"))

# Define question order
question_labels <- c("Baseline", "Prime", "Evidence 1", "Evidence 2", "Evidence 3", "Evidence 4", "Final")

# Create long format and recode safely
# 1. Create long format belief data
belief_long <- prime %>%
  select(id, group, Skepticism, starts_with("S_")) %>%
  pivot_longer(
    cols = starts_with("S_"),
    names_to = "Question",
    values_to = "Response"
  ) %>%
  mutate(
    Question = dplyr::recode(Question,
                             "S_0_1" = "Baseline",
                             "S_00_1" = "Prime",
                             "S_1_1" = "Evidence 1",
                             "S_2_1" = "Evidence 2",
                             "S_3_1" = "Evidence 3",
                             "S_4_1" = "Evidence 4",
                             "S_5_1" = "Final"
    ),
    Question = factor(Question, levels = c("Baseline", "Prime", "Evidence 1", "Evidence 2", "Evidence 3", "Evidence 4", "Final"))
  )

# 2. Calculate belief revisions in wide format
revision_rows <- prime %>%
  transmute(
    id = as.factor(id),
    group = as.factor(group),
    Skepticism = Skepticism,
    `Baseline - Evidence 4` = S_4_1 - S_0_1,
    `Baseline - Final` = S_5_1 - S_0_1
  ) %>%
  pivot_longer(
    cols = c(`Baseline - Evidence 4`, `Baseline - Final`),
    names_to = "Question",
    values_to = "Response"
  )

# Step 1: Make sure both id columns are factors
belief_long <- belief_long %>%
  mutate(id = as.factor(id))

revision_rows <- revision_rows %>%
  mutate(id = as.factor(id))

# Step 2: Bind rows safely
prime_long_extended <- bind_rows(belief_long, revision_rows) %>%
  mutate(
    Question = factor(Question,
                      levels = c("Baseline", "Prime", "Evidence 1", "Evidence 2", "Evidence 3", "Evidence 4", "Final", "Baseline - Evidence 4", "Baseline - Final")
    )
  )


# 3. Bind to original long format
prime_long_extended <- bind_rows(belief_long, revision_rows) %>%
  mutate(
    Question = factor(Question,
                      levels = c("Baseline", "Prime", "Evidence 1", "Evidence 2", "Evidence 3", "Evidence 4", "Final", "Baseline - Evidence 4", "Baseline - Final")
    )
  )

question_levels <- c(
  "Baseline",
  "Prime",
  "Evidence 1",
  "Evidence 2",
  "Evidence 3",
  "Evidence 4",
  "Final",
  "Baseline - Evidence 4",
  "Baseline - Final"
)

prime_long_extended <- bind_rows(belief_long, revision_rows) %>%
  mutate(
    Question = factor(Question, levels = question_levels),
    id = as.factor(id)
  ) %>%
  arrange(id, Question)

## Add Direction and Prime
prime_long_extended <- prime_long_extended %>%
  mutate(
    Direction = case_when(
      group %in% c("A", "D") ~ "++ to --",
      group %in% c("B", "C") ~ "-- to ++"
    ),
    Prime = case_when(
      group %in% c("A", "B") ~ "Prime",
      group %in% c("C", "D") ~ "No Prime"
    )
  )

# Calculate IQR-based limits
iqr_limits <- prime_long_extended %>%
  summarise(
    Q1 = quantile(Response, 0.25, na.rm = TRUE),
    Q3 = quantile(Response, 0.75, na.rm = TRUE)
  ) %>%
  mutate(
    IQR = Q3 - Q1,
    Lower = Q1 - 1.5 * IQR,
    Upper = Q3 + 1.5 * IQR
  )

# Apply filtering
prime_long_extended <- prime_long_extended %>%
  filter(Response >= iqr_limits$Lower, Response <= iqr_limits$Upper)

# Create prime_long like prime_long_extended without two belief revision rows
prime_long <- prime_long_extended %>%
  filter(!Question %in% c("Baseline - Evidence 4", "Baseline - Final"))

## Plot: Line graph showing mean belief adjustment for each question by group
ggplot(prime_long, aes(x = Question, y = Response, group = group, color = group)) +
  stat_summary(fun = mean, geom = "line", linewidth = 1.2, na.rm = TRUE) +
  stat_summary(fun = mean, geom = "point", size = 2, na.rm = TRUE) +
  labs(title = "Belief Revision by Group",
       x = "Belief Revision Stage",
       y = "Mean Response") +
  theme_minimal() + theme(axis.text.x = element_blank())  # <-- Hides x-axis labels)

### Plot with individual lines + group summary, faceted by group
ggplot(prime_long, aes(x = Question, y = Response, group = group)) +
  geom_line(alpha = 0.3, color = "gray60", na.rm = TRUE) +
  stat_summary(aes(group = group), fun = mean, geom = "line", color = "black", size = 1.2, na.rm = TRUE) +
  stat_summary(aes(group = group), fun = mean, geom = "point", size = 2, color = "black", na.rm = TRUE) +
  stat_summary(aes(group = group), fun.data = mean_se, geom = "errorbar", width = 0.2, na.rm = TRUE) +
  facet_wrap(~ group) +
  labs(
    title = "Belief Revision Over Time – Faceted by Group",
    x = "Belief Stage",
    y = "Belief Revision",
    caption = "Black lines = group mean, gray lines = individual responses"
  ) +
  theme_minimal(base_size = 13) +
  theme(axis.text.x = element_blank())  # <-- Hides x-axis labels

## Plot with individual lines + group summary, faceted by group and skepticism
ggplot(prime_long, aes(x = Question, y = Response, group = id)) +
  geom_line(alpha = 0.3) +
  stat_summary(aes(group = interaction(group, Skepticism)),
               fun = mean, geom = "line", linewidth = 1.2, color = "black") +
  facet_grid(Skepticism ~ group) +
  labs(title = "Belief Revision Over Time by Group and Skepticism") + theme_minimal() + theme(axis.text.x = element_blank()) 

# Manipulation Check
## Create a new data frame with only the columns of interest
manipulate <- prime %>% select(c(S_00_1, MC_1, MC_2, MC_3,  MC_4, MC.Prime_1, MC.Prime_1.1, MC.Prime_1.2, RK1_1, RK2_1, RK3_1, RK4_1, group))

## Factorize Columns
manipulate$MC_1 <- as.factor(manipulate$MC_1)
manipulate$MC_2 <- as.factor(manipulate$MC_2)
manipulate$MC_3 <- as.factor(manipulate$MC_3)
manipulate$MC_4 <- as.factor(manipulate$MC_4)

datasummary_skim(manipulate)

## Divide data into 2 groups --> participants who received the prime and those that didn't
### Groups A and B received the prime, Groups C and D did not
primed_group <- manipulate %>% filter(!is.na(S_00_1))
no_primed_group <- manipulate %>% filter(is.na(S_00_1))

datasummary_skim(primed_group)
datasummary_skim(no_primed_group)

## Number of Participants in each group
group_counts <- prime %>%
  group_by(group, Skepticism) %>%
  summarise(count = n())

print(group_counts)

# Test Hypotheses (h1a and H1b)
belief_df <- prime_long %>%
  filter(Question %in% c("Baseline", "Evidence 4", "Final")) %>%
  pivot_wider(names_from = Question, values_from = Response) %>%
  mutate(belief_revision = `Evidence 4` - Baseline)

print(belief_df)

## Mean Change in belief by group (H1a)
prime$belief_revision <- prime$S_4_1 - prime$S_0_1

prime$belief_revision <- prime$belief_revision/100

prime_only <- prime_long_extended %>% 
  filter(Question == "Baseline - Evidence 4")

prime_only$Response <- prime_only$Response / 100

mean_belief_revision_group <- belief_df %>% 
  group_by(group) %>%
  summarise(
    mean_belief_revision = mean(belief_revision, na.rm = TRUE),
    sd_belief_revision = sd(belief_revision, na.rm = TRUE)
  )

print(mean_belief_revision_group)

## ANOVA
baseline_e4 <- prime_long_extended %>%
  filter(Question == "Baseline - Evidence 4")

baseline_e4$Response <- baseline_e4$Response / 100

#### Run ANOVA (H1a)
aov_prime_only <- aov(belief_revision ~ Prime*Direction, data = belief_df)
summary(aov_prime_only)

# Run Fixed Effects model (H1b)
model <- lmer(Response ~ Question * group + (1 | id), data = prime_long)
summary(model)

## Mean Change in belief by group and skepticism
### Summarize by group and skepticism
mean_belief_revision <- belief_df %>%
  group_by(group, Skepticism) %>%
  summarise(
    mean_belief_revision = mean(belief_revision, na.rm = TRUE),
    sd_belief_revision = sd(belief_revision, na.rm = TRUE),
    n = n()
  ) %>%
  ungroup()

print(mean_belief_revision)

## ANOVA
model3 <- aov(belief_revision ~ Prime * Skepticism, data = belief_df)
summary(model3)

# Subset for High skepticism
high_skeptic <- subset(belief_df, Skepticism == "High")
low_skeptic  <- subset(belief_df, Skepticism == "Low")

# ANOVA: effect of group within High skepticism
anova_high <- aov(belief_revision ~ Prime, data = high_skeptic)
summary(anova_high)

# ANOVA: effect of group within Low skepticism
anova_low <- aov(belief_revision ~ Prime, data = low_skeptic)
summary(anova_low)

# Range
range_comparison <- mean_belief_revision %>%
  group_by(Skepticism) %>%
  summarise(
    max_mean = max(mean_belief_revision),
    min_mean = min(mean_belief_revision),
    range = max_mean - min_mean
  )

print(range_comparison)

# Post-hoc tests
library(emmeans)

## High skepticism
model_high <- aov(belief_revision ~ group, data = high_skeptic)
emmeans(model_high, pairwise ~ group)

## Low skepticism
model_low <- aov(belief_revision ~ Prime, data = low_skeptic)
emmeans(model_low, pairwise ~ Prime)


