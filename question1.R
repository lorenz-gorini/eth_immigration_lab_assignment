rm(list = ls())

library(lubridate)
library(dplyr)
library(ggplot2)

# 1. Read in the data
dem_dat <- read.csv("data/dem_dat.csv")
job_dat <- read.csv("data/job_dat.csv")

# Convert YM_start and YM_end columns to Date format
dem_dat$YM_in <- ym(dem_dat$YM_in)
dem_dat$YM_out <- ym(dem_dat$YM_out)

job_dat$YM_start <- ym(job_dat$YM_start)
job_dat$YM_end <- ym(job_dat$YM_end)

# These are the PERS_IDs who are in dem_dat but not in job_dat
diff_pers_ids <- setdiff(dem_dat$PERS_ID, job_dat$PERS_ID)
print(paste0(
  "There are ", length(diff_pers_ids),
  " persons who never found a job (recorded in the dataset) in the country"
))
diff_pers_ids_2 <- setdiff(job_dat$PERS_ID, dem_dat$PERS_ID)
print(paste0(
  "There are ", length(diff_pers_ids_2),
  " unique PERS_IDs in job_dat but not in dem_dat"
))

# Merge the dataframes to get the informations about when the person
# entered or left the country (YM_start and YM_end) for each row of the job
# dataset
merge_df <- dem_dat %>%
  left_join(job_dat, by = "PERS_ID")

# I am assuming that the dataset is consistent, so:
# 1. YM_start of a job is always before the YM_end of the same job
# 2. The person never started a job in the country before getting in the
#   country. This means that YM_start of a job is always after the YM_in of
#   the same person ==> WRONG ASSUMPTION
# 3. The person never started a job in the country after leaving the
#  country. This means that YM_start and YM_end of a job is always before
#  the YM_out ==> WRONG ASSUMPTION
# 4. The person never had multiple jobs contemporarily.



# For each row=job in merge_df, we create 3 new columns:
#  1. column "YM_in_plus_3_years" with YM_in + 3 years
#  2. column with YM_start_3_years = min(YM_in_plus_3_years, YM_start)
#  3. column with YM_end_3_years = min(YM_in_plus_3_years, YM_end)
#  In this way:
#  1. If YM_start > YM_in_plus_3_years -> YM_start_3_years = YM_end_3_years
# = YM_in_plus_3_years , so the job duration is zero, because the person
# started working after 3 years
#  2. If YM_end > YM_in_plus_3_years -> YM_end_3_years = YM_in_plus_3_years

merge_df <- merge_df %>%
  mutate(
    YM_in_plus_3_years = pmin(YM_in + years(3), YM_out),
    YM_start_3_years = pmax(YM_in, YM_start),
    YM_end_3_years = pmin(YM_in_plus_3_years, YM_end)
  )
# filter(YM_end_3_years >= YM_in) %>%
# filter(YM_start_3_years <= YM_out)
merge_df <- merge_df %>% mutate(
  months_worked = interval(YM_start_3_years, YM_end_3_years) %>%
    as.period() %>%
    time_length(unit = "months"),
)
# Fill NA with 0
merge_df$months_worked[is.na(merge_df$months_worked)] <- 0
# Based on how we defined YM_start_3_years and YM_end_3_years, the jobs that
# were started and concluded entirely before or entirely after entering the
# country will result in having "months_worked < 0". We set them to 0.
merge_df$months_worked[merge_df$months_worked < 0] <- 0

# Count the total number of months worked for each person
total_months_per_pers <- merge_df %>%
  group_by(PERS_ID) %>%
  summarize(total_months = sum(months_worked))
# Cap the total_months to 36
total_months_per_pers$total_months[total_months_per_pers$total_months > 36] <-
  36

# Print the result
print(paste0(
  "The mean of months_worked by immigrants in the first 3 years",
  "since they entered the country is: ",
  mean(total_months_per_pers$total_months)
))
print(paste0("The median is: ", median(total_months_per_pers$total_months)))
print(paste0("The maximum is: ", max(total_months_per_pers$total_months)))

# Plot the histogram of total_months_per_pers with ggplot2

ggplot(total_months_per_pers, aes(x = total_months)) +
  geom_histogram(
    aes(y = ..density..),
    binscolour = "black",
    fill = "lightblue"
  ) +
  geom_density(aes(y = ..density..)) +
  ylab("Density") +
  xlab("Worked months per immigrant") +
  ggtitle("Histogram of worked months per immigrant") +
  theme_bw() +
  theme(
    plot.title = element_text(size = 30),
    axis.title.y = element_text(size = 24, vjust = +0.2),
    axis.title.x = element_text(size = 24, vjust = -0.2),
    axis.text.y = element_text(size = 24),
    axis.text.x = element_text(size = 24),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )
ggsave("total_months_per_pers_histogram.png")
# Plot the violin plot of total_months_per_pers with ggplot2
ggplot(total_months_per_pers, aes(x = "immigrants", y = total_months)) +
  geom_violin(fill = "lightblue") +
  geom_jitter(height = 0, width = 0.5, colour = "#6464649d") +
  theme_bw() +
  theme(
    plot.title = element_text(size = 30),
    axis.title.y = element_text(size = 24, vjust = +0.2),
    axis.title.x = element_text(size = 24, vjust = -0.2),
    axis.text.y = element_text(size = 24),
    axis.text.x = element_text(size = 24),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )
ggsave(
  filename = "total_months_per_pers_violin_plot.png",
  plot = last_plot(),
  width = 20,
  height = 40,
  units = "cm"
)
