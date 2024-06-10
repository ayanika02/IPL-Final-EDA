library(ggplot2)
library(dplyr)
library("kableExtra")
library(knitr)

data <- read.csv("IPL final.csv", header= TRUE)
summary(data)

head(data)
tail(data)

colSums(is.na(data))
unique(data$Extra.Type)
unique(data$Out_Type)

# SETTING UP TEAM COLOURS FOR GRAPH
team_colors <- c("KKR" = "purple", "SRH" = "orange")

# CALCULATING RUNS PER OVER
runs_per_over <- data %>%
  group_by(Team, Over) %>%
  summarise(Total_Runs = sum(Run + Extras)) %>%
  ungroup()

# PLOTTING LINE GRAPH FOR RUNS PER OVER
ggplot(runs_per_over, aes(x = Over, y = Total_Runs, color = Team)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  ggtitle("Runs Per Over") +
  theme(plot.title.position = element_text(hjust=0.5)) +
  ylab("Runs") +
  xlab("Over") +
  scale_x_continuous(breaks = 1:20) +
  theme_light() +
  theme(legend.title = element_text(face = "bold")) +
  scale_color_manual(values = team_colors)

# CALCULATING RUN RATE
run_rate <- runs_per_over %>%
  group_by(Team) %>%
  mutate(Cumulative_Runs = cumsum(Total_Runs)) %>%
  ungroup()
run_rate <- run_rate %>%
  mutate(Run_Rate = Cumulative_Runs / Over)

# PLOTTING LINE GRAPH FOR RUN RATE
ggplot(run_rate, aes(x = Over, y = Run_Rate, color = Team)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  ggtitle("Run Rate") +
  ylab("Run Rate") +
  xlab("Over") +
  scale_x_continuous(breaks = 1:20) +
  theme_minimal() +
  theme(legend.title = element_text(face = "bold")) +
  scale_color_manual(values = team_colors)

# CALCULATING RUNS SCORED BY EACH CRICKETER
cricketer_runs <- data %>%
  group_by(Team, Striker) %>%
  summarise(
    Total_Runs = sum(Run),
    Total_Balls = sum(!Extra.Type %in% c("Wide"))
  ) %>%
  arrange(desc(Total_Runs))

# PLOTTING HORIZONTAL BAR GRAPH FOR RUNS SCORED BY EACH CRICKETER
ggplot(cricketer_runs) +
  geom_col(aes(x = Total_Runs, y = reorder(Striker,Total_Runs), fill = Team), width=0.5) +
  ggtitle("Runs Scored by each Cricketer ") +
  theme(plot.title = element_text(hjust=0.5)) +
  xlab("Total Runs") +
  ylab("") +
  theme_minimal() +
  scale_fill_manual(values = team_colors)

# CALCULATING STRIKE RATE OF CRICKETERS
cricketer_runs <- cricketer_runs %>%
  mutate(Strike_Rate = Total_Runs/Total_Balls * 100)

# DISPLAYING HORIZONTAL BAR GRAPH SORTED BY TOTAL RUNS THEY SCORED
ggplot(cricketer_runs) +
  geom_col(aes(x = Strike_Rate, y = reorder(Striker,Total_R), fill = Team), width=0.5) +
  ggtitle("Strike Rate of each Cricketer ") +
  theme(plot.title = element_text(hjust=0.5)) +
  xlab("Strike") +
  ylab("") +
  theme_minimal() +
  scale_fill_manual(values = team_colors)

# TYPES OF WICKETS AND ITS PIE CHART
out_data <- data[data$Out > 0, ]
out_counts <- table(out_data$Out_Type)
pie(out_counts, main = "Distribution of Wicket Types",
    labels = paste(names(out_counts), "\n", out_counts)

# GROUPING BOWLERS BY WICKETS
wickets_by_bowler <- out_data %>%
  group_by(Bowler, Team) %>%
  summarise(Wickets = n())

# CALCULATING THE ECONOMY RATE, RUNS CONCEDED AND OVERS BOWLED BY EACH BOWLER
bowler_stats <- data1 %>%
  group_by(Bowler, Team) %>%
  summarise(
    Runs_Conceded = sum(Run + if_else(Extra.Type != 'B' & Extra.Type != 'LB', Extras, 0)),
    Overs_Bowled = n_distinct(Over), 
    Economy_Rate = Runs_Conceded / Overs_Bowled,
    .groups = 'drop'
  )

# JOINING TABLES
bowler_stats <- bowler_stats %>%
  left_join(wickets_by_bowler, by = c("Bowler", "Team"))

bowler_stats <- bowler_stats %>%
  mutate(Team = if_else(Team == "KKR", "SRH", "KKR"))

# SELECTING ONLY SPECIFIC COLUMNS TO SHOW IN THE TABLE
selected_columns <- bowler_stats %>%
  select(Bowler, Team, Overs_Bowled, Economy_Rate, Wickets)

# MAKING TABLE USING kableExtra AND knitr FUNCTIONS
selected_columns %>%
mutate(Economy_Rate = round(Economy_Rate, 2)) %>%
arrange(Economy_Rate) %>%
kable("html",
      caption = "Bowler Economy Rates and Wickets in IPL 2024 Finals") %>%
kable_styling(bootstrap_options = "striped", full_width = FALSE) %>%
add_header_above(c("Bowler Stats" = 3, "Performance" = 2))

# CALCULATING CUMULATIVE RUNS AFTER EVERY OVER FOR WORM GRAPH STARTS FROM HERE
data1 <- data %>%
group_by(Team) %>%
mutate(Cumulative_Runs = cumsum(Run + Extras)) %>%
ungroup()

# CALCULATE CUMULATIVE DELIVERIES FOR EACH OVER
data1 <- data1 %>%
  group_by(Team, Over) %>%
  mutate(Cumulative_Balls = row_number()) %>%
  ungroup()

# CALCULATING TOTAL DELIVERIES FOR EACH OVER
over_totals <- data1 %>%
  group_by(Team, Over) %>%
  summarise(Total_Balls = max(Cumulative_Balls)) %>%
  ungroup()

data1 <- data1 %>%
  left_join(over_totals, by = c("Team", "Over"))

data1 <- data1 %>%
  mutate(over_ball = Over + (Cumulative_Balls - 1) / Total_Balls)

# EXTRACT OUT DATA (WICKETS)
out_data <- data1 %>%
  filter(Out > 0)

# PLOTTING FOR WORM GRAPH
ggplot(data = data1, aes(x = over_ball-1, y = Cumulative_Runs, color = Team)) +
  geom_line() +
  geom_point(data = out_data, aes(x = over_ball-1, y = Cumulative_Runs), color = "black", size = 2, shape = 15) +
  labs(title = "Cumulative Runs with Wickets", x = "Over", y = "Cumulative Runs") +
  theme_minimal() +
  scale_color_manual(values = team_colors)

# DIVING THE MATCH INTO ITS 3 DIFFERENT PHASES
phase <- data %>%
 mutate(Phase = case_when(
   Over <= 6 ~ "Powerplay",
   Over <= 15 ~ "Middle",
   TRUE ~ "Death"
 ))
phase <- phase %>%
  mutate(Phase = factor(Phase, levels = c("Powerplay", "Middle", "Death")))

# CALCULATING RUNS, WICKETS, NUMBER OF 4s AND 6s, AND NUMBER OF DOT BALLS ACROSS EVERY PHASE
summary_data <- phase %>%
 group_by(Team, Phase) %>%
  summarise(
   Total_Runs = sum(Run + Extras),
   Total_Wickets = sum(Out > 0),
   Fours_Sixes = sum(Run == 4) + sum(Extras==4) + sum(Run == 6) + sum(Extras==6),
   Dots = sum(Run == 0 & Extras==0) 
 ) %>%
 ungroup()
long_data <- summary_data %>%
  pivot_longer(cols = c(Total_Runs, Total_Wickets, Fours_Sixes, Dots),
               names_to = "Metric",
               values_to = "Count")

# PLOTTING GROUPED BAR CHARTS
ggplot(long_data, aes(x = Phase, y = Count, fill = Metric)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(position = position_dodge(width = 0.9),    
            vjust = -0.5,    
            size = 3,        
            aes(group = Metric, label=Count),    
            color = "black") + 
  facet_wrap(~ Team) +
  labs(title = "Total Runs, Wickets, Fours and Sixes, and Dot balls per Phase", x="", y="") +
  theme_minimal()
