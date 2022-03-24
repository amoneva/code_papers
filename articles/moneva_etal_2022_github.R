# This script contains the code for the analysis of the paper
# "Alerting Consciences to Reduce Cybercrime. A Quasi-experimental Design 
# Using Warning Banners", published in the Journal of Experimental Criminology
# https://link.springer.com/article/10.1007/s11292-022-09504-2#Sec17
# Authors: 
# - Asier Moneva
# - E. Rutger Leukfeldt
# - Wouter Klijnsoon

options(
  scipen = 999,
  kableExtra.auto_format = FALSE
  )
# knitr::opts_chunk$set(echo = FALSE)

# Load packages ----
library(ggsignif)
library(here)
library(kableExtra)
library(knitr)
library(patchwork)
library(readxl)
library(tidyverse)

# Figure 1 ----
# File not provided!
# include_graphics(path = here("documents", "design.png"))

# Table 1 ----

table_1 <- data.frame(
  group = c("control", "experimental 1", "experimental 2", "experimental 3"),
  tone = c("deterrent", "social", "informative", "reorienting"),
  headline_1 = c("A DDoS attack is illegal", "DDoS ruins it for everyone", "Do you want to DDoS a game?", "Play fair; losers do DDoS"),
  headline_2 = c("Gamechangers", "Gamechangers", "Gamechangers", "Gamechangers"),
  description = c("Carrying out a DDoS is illegal in the Netherlands under the Criminal Code", "So you want your friends to be unable to play games because you play a prank?", "Learn more about DDoS attacks and their impact on gaming", "Win fairly in an e-sports competition by training your gaming skills")
)

kable(
  x = table_1,
  col.names = c("Group", "Tone", "Headline 1", "Headline 2", "Description"),
  caption = "Content of the ads by control and experimental groups",
  booktabs = TRUE
) %>% 
  column_spec(
    column = c(3, 5),
    width = "4cm"
  )

# Import data from Google Ads (I) ----
# For all data: Filter by '% of new sessions'

# Import ads data from .xlsx
# Ads & extensions > Ads > Campaign type: Search
df_ads <- read_xlsx(
  path = here("data", "ads_final_300421.xlsx"),
  skip = 2
)

# Rename variables for better handling
df_ads <- df_ads %>% 
  rename(
    head_1 = `Headline 1`,
    top_vs_other = `Top vs. Other`,
    clicks = Clicks,
    impr = Impr.
  ) %>% 
  drop_na(head_1)

competition <- df_ads %>% 
  group_by(top_vs_other) %>% 
  summarise(competition_n = sum(impr)) %>% 
  mutate(competition_p = (competition_n / sum(competition_n)) * 100)

# Import campaign data from .csv
# Overview (Clicks, Impressions, CTR, Avg. CPC)
df_cam <- read_csv(
  file = here("data", "cam_final_300421.csv"),
  col_types = cols(
    Date = col_date(format = "%a, %b %d, %Y"),
    Clicks = col_number(),
    Impressions = col_number(),
    CTR = col_number(),
    `Avg. CPC` = col_number()
  )
)

# Calculate Pearson's correlation between impressions and clicks
cor_cam <- cor.test(
  x = df_cam$Impressions,
  y = df_cam$Clicks
)

# Figure 2 ----

# Plot the campaign interaction trends
figure_1 <- df_cam %>% 
  # Transform impressions for visualization
  mutate(impr = Impressions / 10) %>% 
  pivot_longer(
    cols = - Date,
    names_to = "metric",
    values_to = "value"
  ) %>% 
  filter(metric == "Clicks" | metric == "impr") %>% 
  ggplot(mapping = aes(
    x = Date,
    y = value,
    group = metric,
    colour = metric
  )) +
  geom_line() +
  geom_vline(
    xintercept = as.Date(
      x = "2021-03-22",
      format = "%Y-%m-%d"
    ),
    linetype = 2,
    color = "gray70"
  ) +
  annotate(
    geom = "text",
    label = "Large\nDDoS attack",
    x = as.Date("2021-04-01"),
    y = 300,
    color = "gray70"
  ) +
  scale_x_date() +
  scale_colour_viridis_d(name = "Interaction") +
  labs(
    x = NULL,
    y = NULL
  ) +
  theme_classic() +
  theme(legend.position = c(.15, .8))

print(figure_1)

# Import data from Google Ads (II)----
# For all data: Filter by '% of new sessions'

# Import demographic data from .xlsx
# Demographics > Combinations
df_dem <- read_xlsx(
  path = here("data", "dem_final_300421.xlsx"),
  skip = 2
)

# Import temporal data from .xlsx
# Ad schedule > Day & hour
df_tim <- read_xlsx(
  path = here("data", "tim_final_300421.xlsx"),
  skip = 2
)

# Import keyword data from .xlsx
# Keywords > Search Terms
df_key <- read_xlsx(
  path = here("data", "key_final_300421.xlsx"),
  skip = 2
)

# Rename variables for better handling
df_dem <- df_dem %>% 
  rename(
    age = Age,
    gender = Gender,
    income = `Household income`,
    clicks = Clicks,
    impr = Impr.
  )

# Calculate what percentage of impressions and clicks were attributed to which gender
gender_int <- df_dem %>% 
  select(
    age,
    gender,
    clicks,
    impr
  ) %>% 
  group_by(gender) %>% 
  summarise(
    sum_impr = sum(impr),
    sum_click = sum(clicks)
  ) %>% 
  mutate(
    p_impr = (sum_impr / sum(sum_impr)) * 100,
    p_click = (sum_click / sum(sum_click)) * 100
  )

# Figure 3 ----

# Create a figure to show target group interaction
figure_2 <- df_dem %>% 
  select(
    age,
    gender,
    clicks,
    impr
  ) %>% 
  group_by(
    age,
    gender
  ) %>% 
  summarise(
    sum_impr = sum(impr),
    sum_click = sum(clicks)
  ) %>% 
  ungroup() %>% 
  mutate(
    rate_impr = (sum_impr / sum(sum_impr)) * 100,
    rate_click = (sum_click / sum(sum_click)) * 100
    ) %>% 
  filter(sum_impr != 0) %>% 
  pivot_longer(
    cols = c("rate_impr", "rate_click"),
    names_to = "interact",
    values_to = "values"
  ) %>% 
  ggplot(mapping = aes(
    x = age,
    y = values,
    fill = gender
  )) +
  geom_col(position = "dodge") +
  scale_fill_viridis_d(name = "Gender") +
  labs(
    x = "Age group",
    y = "Interaction\n(%)"
  ) +
  facet_wrap(
    ~ interact, 
    scales = "free_y",
    labeller = as_labeller(c(rate_click = "Clicks", rate_impr = "Impressions"))
  ) +
  theme_classic() +
  theme(axis.title.y = element_text(
    angle = 0,
    vjust = .5
  ))

print(figure_2)

# Rename variables for better handling
df_key <- df_key %>% 
  rename(
    term = `Search term`,
    match = `Match type`,
    campaign = Campaign,
    group = `Ad group`,
    clicks = Clicks,
    impr = Impr.,
    cpc = `Avg. CPC`,
    cost = Cost,
    eng_ratio = CTR,
    keyword = Keyword
  )

# Transform data to find most relevant `keyword`
# Tip: look at `term` too!
df_key <- df_key %>% 
  group_by(
    match,
    keyword
    ) %>% 
  # Exclude to examine search terms instead of keywords
  summarise(
    clicks_ = sum(clicks),
    cost_ = sum(cost),
    impr_ = sum(impr)
  ) %>% 
  ungroup() %>% 
  filter(clicks_ > 24) %>% # clicks
  drop_na() %>% 
  mutate(
    cpc = cost_ / clicks_, # cost / clicks
    eng_ratio = impr_ / clicks_ # impr / clicks
  ) %>% 
  arrange(desc(eng_ratio))

# Add a column with the language of the keywords to compare groups
df_key <- df_key %>% 
  mutate(lang = if_else(
    condition = keyword == "+ddos +aanval" | keyword == "[ddos aanval kopen]" | keyword == "+ddos +aanval +uitvoeren",
    true = "nl",
    false = "en"
  ))

# Compare weighted means of the performance of keywords in both languages
key_lang <- df_key %>% 
  group_by(lang) %>% 
  summarise(
    wmean_cpc = weighted.mean(
      x = cpc, 
      w = clicks_
    ),
    wmean_eng_ratio = weighted.mean(
      x = eng_ratio, 
      w = clicks_
    )
  ) %>% 
  ungroup()

# Calculate median values of variables to split axes
mdn_cpc <- median(df_key$cpc)
mdn_eng_ratio <- median(df_key$eng_ratio)

# Figure 4 ----

# Create a figure to display keyword performance
figure_3 <- df_key %>% 
  ggplot(mapping = aes(
    x = cpc,
    y = eng_ratio,
    colour = match
  )) +
  geom_point() +
  ggrepel::geom_text_repel(
    mapping = aes(label = keyword), # term 
    size = 3,
    min.segment.length = .3,
    show.legend = FALSE
  ) +
  geom_vline(
    xintercept = mdn_cpc,
    colour = "gray70",
    linetype = 2
  ) +
  geom_hline(
    yintercept = mdn_eng_ratio,
    colour = "gray70",
    linetype = 2
  ) +
  scale_colour_viridis_d(
    name = "Type of match",
    labels = c("Broad", "Exact", "Exact (close variant)", "Phrase", "Phrase (close variant)")
  ) +
  labs(
    x = "Mean cost per click (€)",
    y = "Engagement\nratio"
  ) +
  theme_classic() +
  theme(
    axis.title.y = element_text(
      angle = 0,
      vjust = .5
    ),
    legend.position = "bottom"
  ) +
  guides(colour = guide_legend(
    nrow = 2, 
    byrow = TRUE
  ))

print(figure_3)

# Create a table to show ad performance
table_2 <- df_ads %>% 
  select(
    head_1,
    top_vs_other,
    clicks,
    impr
  ) %>% 
  filter(top_vs_other == "Google search: Top") %>% 
  group_by(head_1) %>% 
  summarise(
    sum_impr_top = sum(impr),
    sum_clicks_top = sum(clicks)
  ) %>% 
  ungroup() %>% 
  drop_na() %>% 
  mutate(
    ratio_impr_top = (sum_impr_top / sum(sum_impr_top)) * 100,
    ratio_clicks_top = (sum_clicks_top / sum(sum_clicks_top)) * 100,
    eng_ratio = (sum_clicks_top / sum_impr_top) * 100
    ) %>% 
  relocate(
    ratio_impr_top,
    .after = sum_impr_top
  ) %>% 
  relocate(
    ratio_clicks_top,
    .after = sum_clicks_top
  ) %>% 
  arrange(desc(eng_ratio))

# Chi-square tests and log odds ratio to compare the performance of the deterrent advertisement with the other three designs

# Log odds ratio function
log_or_fun <- function (data) {
  log((data[1, 1] / data[1, 2]) / (data[2, 1] / data[2, 2]))
}

# Deterrent vs social
bla_sta <- table_2 %>% 
  filter(head_1 == "DDoS verpest het voor iedereen" | head_1 == "Een DDoS-aanval is strafbaar") %>% 
  select(
    sum_impr_top,
    sum_clicks_top
  ) %>% 
  arrange(sum_impr_top)
chi_bla_sta <- chisq.test(bla_sta)
or_bla_sta <- log_or_fun(bla_sta)

# Deterrent vs informative
tea_sta <- table_2 %>% 
  filter(head_1 == "Een DDoS-aanval is strafbaar" | head_1 == "Wil je een game DDoS-en?") %>% 
  select(
    sum_impr_top,
    sum_clicks_top
  ) 
chi_tea_sta <- chisq.test(tea_sta)
or_tea_sta <- log_or_fun(tea_sta)

# Deterrent vs reorienting
adv_sta <- table_2 %>% 
  filter(head_1 == "Een DDoS-aanval is strafbaar" | head_1 == "Play fair; verliezers DDoS-en") %>% 
  select(
    sum_impr_top,
    sum_clicks_top
  )
chi_adv_sta <- chisq.test(adv_sta)
or_adv_sta <- log_or_fun(adv_sta)

# informative vs reorienting
tea_adv <- table_2 %>% 
  filter(head_1 == "Wil je een game DDoS-en?" | head_1 == "Play fair; verliezers DDoS-en") %>% 
  select(
    sum_impr_top,
    sum_clicks_top
  )
chi_tea_adv <- chisq.test(tea_adv)
or_tea_adv <- log_or_fun(tea_adv)

# social vs reorienting
bla_adv <- table_2 %>% 
  filter(head_1 == "DDoS verpest het voor iedereen" | head_1 == "Play fair; verliezers DDoS-en") %>% 
  select(
    sum_impr_top,
    sum_clicks_top
  ) 
chi_bla_adv <- chisq.test(bla_adv)
or_bla_adv <- log_or_fun(bla_adv)

# Table 2 ----

# Display the results in a nice table
table_2 %>% 
  mutate(head_1 = factor(
    x = head_1,
    labels = c("DDoS ruins it for everyone", "A DDoS attack is illegal", "Play fair; losers do DDoS", "Do you want to DDoS a game?")
  )) %>% 
  select(-eng_ratio) %>% 
  kable(
  digits = 1,
  col.names = c("Ad headline", "n", "%", "n", "%"),
  caption = "Ad performance",
  booktabs = TRUE
) %>% 
  add_header_above(c(" " = 1, "Impressions" = 2, "Clicks" = 2))

# Figure 5 ----

figure_4 <- table_2 %>% 
  ggplot(mapping = aes(
    x = reorder(
      x = head_1,
      X = eng_ratio
    ),
    y = eng_ratio
  )) +
  geom_col() +
  geom_signif(
    mapping = aes(y = eng_ratio),
    y_position = c(10.4, 9.5, 8.6, 7.7),
    xmin = c(4, 3, 3, 2),
    xmax = c(3, 2, 1, 1),
    annotation = c("***", "", "***", "***")
  ) +
  geom_text(
    mapping = aes(label = format(
      round(eng_ratio, 1), 
      nsmall = 1
    )),
    colour = "white",
    hjust = 1.4
  ) +
  scale_x_discrete(labels = c(
    "DDoS verpest het voor iedereen" = "Social",
    "Een DDoS-aanval is strafbaar" = "Deterrent\n(control)",
    "Wil je een game DDoS-en?" = "Informative",
    "Play fair; verliezers DDoS-en" = "Reorienting"
  )) +
  coord_flip() +
  labs(
    x = "Ads",
    y = "Engagement ratio"
  ) +
  theme_classic() +
  theme(axis.title.y = element_text(
    angle = 0,
    vjust = .5
  ))

figure_4

# Rename and transform variables for better handling
df_tim <- df_tim %>% 
  rename(
    day = `Day of the week`,
    hour = `Hour of the day`,
    clicks = Clicks,
    impr = Impr.
  ) %>% 
  mutate(
    day = factor(
      x = day,
      levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
    ),
    hour = factor(
      x = hour,
      levels = 0:23,
      labels = c("00", "01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12", "13", "14", "15", "16", "17", "18", "19", "20", "21", "22", "23")
    )
  )

# Figure 6 ----

# Plot user interaction by day of the week
figure_5a <- df_tim %>% 
  select(
    day,
    clicks,
    impr
  ) %>% 
  drop_na() %>% 
  group_by(day) %>% 
  summarise(
    mean_impr = mean(impr),
    sd_impr = sd(impr),
    mean_clicks = mean(clicks),
    sd_clicks = sd(clicks),
    eng_ratio = (mean_clicks / mean_impr) * 100
  ) %>% 
  ungroup() %>% 
  ggplot(mapping = aes(x = day)) +
  geom_col(mapping = aes(
    y = eng_ratio, 
    group = 1,
    fill = "Engagement\nratio (A, B)"
  )) +
  geom_point(mapping = aes(
    y = mean_clicks, 
    colour = "Clicks"
  )) +
  geom_point(mapping = aes(
    y = mean_impr / 10, 
    colour = "Impressions / 10"
  )) +
  scale_x_discrete(labels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")) +
  scale_fill_viridis_d(
    name = NULL, 
    begin = .5
  ) +
  scale_color_viridis_d(name = NULL) +
  labs(
    x = "Day of the week",
    y = "Mean\ninteraction"
  ) +
  theme_classic() +
  theme(axis.title.y = element_text(
    angle = 0,
    vjust = .5
  ))

# Plot user interaction by hour of the day
figure_5b <- df_tim %>% 
  select(
    hour,
    clicks,
    impr
  ) %>% 
  drop_na() %>% 
  group_by(hour) %>% 
  summarise(
    mean_impr = mean(impr),
    sd_impr = sd(impr),
    mean_clicks = mean(clicks),
    sd_clicks = sd (clicks),
    eng_ratio = (mean_clicks / mean_impr) *100
  ) %>% 
  ungroup() %>% 
  ggplot(mapping = aes(x = hour)) +
  geom_col(mapping = aes(
    y = eng_ratio, 
    group = 1,
    fill = "Engagement\nratio"
  )) +
  geom_point(mapping = aes(
    y = mean_clicks, 
    colour = "Clicks"
  )) +
  geom_point(mapping = aes(
    y = mean_impr / 10, 
    colour = "Impressions / 10"
  )) +
  scale_fill_viridis_d(
    name = NULL, 
    begin = .5
  ) +
  scale_color_viridis_d(name = NULL) +
  labs(
    x = "Hour of the day",
    y = "Mean\ninteraction"
  ) +
  theme_classic() +
  theme(
    axis.title.y = element_text(
      angle = 0,
      vjust = .5
    ),
    legend.position = "none"
  )

# Plot user interaction by day of the week and hour of the day
figure_5c <- df_tim %>% 
  select(
    day,
    hour,
    clicks,
    impr
  ) %>% 
  drop_na() %>% 
  group_by(
    day, 
    hour
  ) %>% 
  summarise(
    mean_impr = mean(impr),
    mean_clicks = mean(clicks),
    eng_ratio = (mean_clicks / mean_impr) *100
  ) %>% 
  ungroup() %>%
  # Filter outliers out because of small n
  filter(eng_ratio <  mean(eng_ratio) + 2 * sd(eng_ratio) & eng_ratio > mean(eng_ratio) - 2 * sd(eng_ratio)) %>% 
  ggplot(mapping = aes(
    x = hour,
    y = day,
    fill = eng_ratio
  )) +
  geom_raster() +
  scale_y_discrete(labels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")) +
  scale_fill_viridis_c(name = "Engagement\nratio (C)") +
  labs(
    x = "Hour of the day",
    y = "Day of\nthe week"
  ) +
  theme_classic() +
  theme(axis.title.y = element_text(
    angle = 0,
    vjust = .5
  ))

(figure_5a / 
   figure_5b /
   figure_5c) + 
  plot_layout(guides = "collect") +
  plot_annotation(tag_levels = "A")

# Import data from Google Analytics ----

# For all data: Use new segment (paid traffic + new users)
# Behavior events were in place on Feb 11; video events were added on Feb 24
# To match weekly data from the Google Ads the start date is fixed on the 13th

# Import behavioral data from .xlsx
# Google Analytics > Audience > Behavior > New vs. Returning > Secondary dimension: landing page
df_aud <- read_xlsx(
  path = here("data", "aud_final_300421.xlsx"),
  sheet = 2
)

# Import event data from .xlsx
# Google Analytics > Behavior > Events > Top events > Primary dimension: Event Label > Secondary dimension: Landing Page
df_eve <- read_xlsx(
  path = here("data", "eve2_final_300421.xlsx"),
  sheet = 2
)

# Table 3 ----

# Rename and keep only relevant columns
table_3 <- df_aud %>% 
  filter(
    `Landing Page` == "/changeyourgame/politie/" | `Landing Page` == "/changeyourgame/eslplay/",
    `User Type` == "New Visitor"
    ) %>% 
  select(
    page = "Landing Page",
    users = "Users",
    duration = "Avg. Session Duration",
    bounce = "Bounce Rate"
  ) %>% 
  drop_na() %>% 
  mutate(
    bounce = bounce * 100,
    page = if_else(
      condition = page == "/changeyourgame/politie/",
      true = "Formal",
      false = "Informal"
    )
  ) %>%
  mutate(
    p_users = (users / sum(users)) * 100,
    .after = "users"
  ) %>% 
  arrange(page)

kable(
  x = table_3,
  digits = 1,
  col.names = c("Landing page", "n", "%", "Mean session duration (s)", "Mean bounce rate (%)"),
  caption = "New users behaviour by landing page",
  booktabs = TRUE
) %>% 
  add_header_above(c(" " = 1, "Users" = 2, " " = 2))

# Rename and keep only relevant columns
df_eve <- df_eve %>% 
  select(
    page = "Landing Page",
    event = "Event Label",
    interaction = "Unique Events"
  ) %>% 
  filter(
    (event == "25" | event == "50" | event == "75" | event == "ESL |start" | event == "ESL |pause" | event == "ESL |seek" | event == "ESL |complete" | event == "PNL |start" | event == "PNL |pause" | event == "PNL |seek" | event == "PNL |complete" | event == "Strafrechtelijke vervolging met als consequentie een gevangenisstraf of een werkstraf" | event == "Een alternatief straftraject" | event == "Een bezoek van de politie en inbeslagname van digitale middelen" | event == "Wat is er allemaal strafbaar online" | event == "Ik heb gehackt" | event == "Zelf fouten ontdekken" | event == "Crimediggers" | event == "Dutch Game Garden" | event == "ESL Benelux") & (page == "/changeyourgame/politie/" | page == "/changeyourgame/eslplay/")
  ) %>% 
  drop_na() %>% 
  mutate(ratio_interaction = if_else(
    condition = page == "/changeyourgame/politie/",
    true = interaction / (as.numeric(table_3 %>% 
                                      filter(page == "Formal") %>% 
                                      summarise(sum_users_pol = sum(users))
    )) * 100,
    false = interaction / (as.numeric(table_3 %>% 
                                      filter(page == "Informal") %>% 
                                      summarise(sum_users_pol = sum(users))
    )) * 100
  ))

# Figure 7 ----

# Examine general interaction in both pages
figure_6a <- df_eve %>% 
  mutate(
    action = if_else(
      condition = event == "25" | event == "50" | event == "75",
      true = "scroll",
      false = if_else(
        condition = event == "ESL |start" | event == "ESL |pause" | event == "ESL |seek" | event == "ESL |complete" | event == "PNL |start" | event == "PNL |pause" | event == "PNL |seek" | event == "PNL |complete",
        true = "video",
        false = "url"
      )),
    ratio_interaction = if_else(
      condition = action == "scroll",
      true = ratio_interaction / 10,
      false = ratio_interaction
    )) %>% 
  group_by(
    page,
    action
  ) %>% 
  summarise(mean_ratio_interaction = mean(ratio_interaction)) %>% 
  ungroup() %>% 
  ggplot(aes(
    x = reorder(
      x = action,
      X = - mean_ratio_interaction
      ),
    y = mean_ratio_interaction,
    fill = page
  )) +
  geom_col(position = "dodge") +
  scale_x_discrete(labels = c("Scrolling\nper 1000\nusers", "Video\ninteractions\nper 100\nusers", "URL\nclicks\nper 100\nusers")) +
  scale_fill_viridis_d(labels = c("Informal", "Formal")) +
  labs(
    x = NULL,
    y = "Mean\ninteraction\nrate",
    fill = "Landing page"
  ) +
  theme_classic() +
  theme(
    axis.title.y = element_text(
      angle = 0,
      vjust = .5
    ),
    legend.position = "none"
  )

# Examine scrolling behavior in both pages
figure_6b <- df_eve %>% 
  filter(event == "25" | event == "50" | event == "75") %>% 
  ggplot(aes(
    x = event,
    y = ratio_interaction,
    group = page,
    colour = page
  )) +
  geom_line(linetype = 2) +
  geom_point() +
  scale_y_continuous(limits = c(0, max(df_eve$ratio_interaction) + (.1 * max(df_eve$ratio_interaction)))) +
  scale_color_viridis_d(labels = c("Informal", "Formal")) +
  labs(
    x = "% of page scrolled",
    y = "Unique\nevents\nper 100\nusers",
    colour = "Landing page"
  ) +
  theme_classic() +
  theme(
    axis.title.y = element_text(
      angle = 0,
      vjust = .5
    ),
    legend.position = "none"
  )

# Examine video interaction in both pages
figure_6c <- df_eve %>% 
  filter(event == "ESL |start" | event == "ESL |pause" | event == "ESL |seek" | event == "ESL |complete" | event == "PNL |start" | event == "PNL |pause" | event == "PNL |seek" | event == "PNL |complete") %>% 
  mutate(event_2 = str_extract(
    string = event,
    pattern = "[:lower:]+"
  )) %>% 
  ggplot(aes(
    x = factor(
      x = event_2,
      levels = c("start", "pause", "seek", "complete")
    ),
    y = ratio_interaction,
    group = page,
    fill = page
  )) +
  geom_col(position = "dodge") +
  scale_fill_viridis_d(labels = c("Informal", "Formal")) +
  labs(
    x = "Video interactions",
    y = "Unique\nevents\nper 100\nusers",
    fill = "Landing page"
  ) +
  theme_classic() +
  theme(axis.title.y = element_text(
    angle = 0,
    vjust = .5
  ))

# Examine URL clicking behavior in both pages
figure_6d <- df_eve %>% 
  filter(event == "Strafrechtelijke vervolging met als consequentie een gevangenisstraf of een werkstraf" | event == "Een alternatief straftraject" | event == "Een bezoek van de politie en inbeslagname van digitale middelen" | event == "Wat is er allemaal strafbaar online" | event == "Ik heb gehackt" | event == "Zelf fouten ontdekken" | event == "Crimediggers" | event == "Dutch Game Garden" | event == "ESL Benelux") %>% 
  mutate(event = factor(
    x = event,
    labels = c("Crimediggers", "Dutch Game Garden", "An alternative punishment", "A visit from the police and seizure of digital resources", "ESL Benelux", "I hacked", "Criminal prosecution resulting in imprisonment or community service", "Punishable acts online", "Discovering vulnerabilities yourself")
  )) %>% 
  ggplot(aes(
    x = reorder(
      x = event,
      X = ratio_interaction
    ),
    y = ratio_interaction,
    group = page,
    fill = page
  )) +
  geom_col(position = "dodge") +
  scale_x_discrete(labels = function(event) str_wrap(
    string = event, 
    width = 40
  )) +
  scale_fill_viridis_d(labels = c("Informal", "Formal")) +
  labs(
    x = "URL\ntext",
    y = "Unique events per 100 users",
    fill = "Landing page"
  ) +
  coord_flip() +
  theme_classic() +
  theme(
    axis.title.y = element_text(
      angle = 0,
      vjust = .5
    ),
    legend.position = "none"
  )

layout <- "
AABB
CCCE
DDDD
DDDD
"
wrap_plots(
  A = figure_6a, 
  B = figure_6b, 
  C = figure_6c, 
  D = wrap_elements(full = figure_6d), 
  E = guide_area(), 
  design = layout
) + 
  plot_layout(
    guides = "collect", 
    design = layout) + 
  plot_annotation(tag_levels = "A")

## # Statistical analyses
## 
## # Total interactions (Figure 6A)
## chi_f6a <- df_eve %>%
##   mutate(
##     action = if_else(
##       condition = event == "25" | event == "50" | event == "75",
##       true = "scroll",
##       false = if_else(
##         condition = event == "ESL |start" | event == "ESL |pause" | event == "ESL |seek" | event == "ESL |complete" | event == "PNL |start" | event == "PNL |pause" | event == "PNL |seek" | event == "PNL |complete",
##         true = "video",
##         false = "url"
##       ))
##   ) %>%
##   group_by(
##     page,
##     action
##   ) %>%
##   summarise(mean_ratio_interaction = mean(ratio_interaction)) %>%
##   ungroup() %>%
##   pivot_wider(
##     names_from = action,
##     values_from = mean_ratio_interaction
##   ) %>%
##   select(- page) %>%
##   chisq.test()
## 
## 
## # Percentage of page scrolled (Figure 6B)
## chi_f6b <- df_eve %>%
##   filter(event == "25" | event == "50" | event == "75") %>%
##   select(- interaction) %>%
##   pivot_wider(
##     names_from = event,
##     values_from = ratio_interaction
##   ) %>%
##   select(- page) %>%
##   chisq.test()
## 
## # Video interaction (Figure 6C)
## chi_f6c <- df_eve %>%
##   filter(event == "ESL |start" | event == "ESL |pause" | event == "ESL |seek" | event == "ESL |complete" | event == "PNL |start" | event == "PNL |pause" | event == "PNL |seek" | event == "PNL |complete") %>%
##   mutate(event_2 = str_extract(
##     string = event,
##     pattern = "[:lower:]+"
##   )) %>%
##   select(
##     - interaction,
##     - event
##     ) %>%
##   pivot_wider(
##     names_from = event_2,
##     values_from = ratio_interaction
##   ) %>%
##   select(- page) %>%
##   chisq.test()
## 
## # URL click (Figure 6D)
## chi_f6d <- df_eve %>%
##   filter(event == "Strafrechtelijke vervolging met als consequentie een gevangenisstraf of een werkstraf" | event == "Een alternatief straftraject" | event == "Een bezoek van de politie en inbeslagname van digitale middelen" | event == "Wat is er allemaal strafbaar online" | event == "Ik heb gehackt" | event == "Zelf fouten ontdekken" | event == "Crimediggers" | event == "Dutch Game Garden" | event == "ESL Benelux") %>%
##   select(- interaction) %>%
##   pivot_wider(
##     names_from = event,
##     values_from = ratio_interaction
##   ) %>%
##   select(- page) %>%
##   chisq.test()

# Appendix A ----

# Import the list of keywords from .xlsx
# appendix_a <- read_xlsx(path = here("documents", "keywords", "keywords_total_v1_0.xlsx"))

# Display the keywords in a table
kable(
  x = list(
    appendix_a$keyword[1:31], 
    appendix_a$keyword[32:62],
    appendix_a$keyword[63:93],
    appendix_a$keyword[94:124]
    ),
  col.names = "Keywords",
  caption = "List of 123 keywords related to DDoS attacks",
  booktabs = TRUE
) %>% 
  kable_styling(latex_options = "hold_position")

# Appendix B ----

# Create a data frame with empty room for the ad images
appendix_b <- data.frame(
  tone = c("deterrent (control)", "social", "informative", "reorienting"),
  ad_img = ""
)

# Fill the data frame with the ad images and display it
kable(
  x = appendix_b,
  col.names = c("Tone", "Ad"),
  caption = "Google Ads designs",
  booktabs = TRUE
) %>% 
  kable_styling(latex_options = "hold_position") %>% 
  column_spec(
    column = 1,
    width = "2cm"
  ) %>% 
  column_spec(
    column = 2,
    image = c(here("documents", "ads", "ad_deterrent_q.jpg"), here("documents", "ads", "ad_social_q.jpg"), here("documents", "ads", "ad_informative_q.jpg"), here("documents", "ads", "ad_reorienting_q.jpg")),
    width = "13cm"
  )

# Appendix C ----

# Formal design
include_graphics(
  path = here("documents", "landing_pages", "landing_page_politie.jpeg"),
  auto_pdf = TRUE
)

# Informal design
include_graphics(
  path = here("documents", "landing_pages", "landing_page_esl.jpeg"),
  auto_pdf = TRUE
)

# sessionInfo()
