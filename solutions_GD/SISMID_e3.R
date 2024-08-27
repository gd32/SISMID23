# R version of Exercise 3. This is almost a complete solution, but the outbreak detection
# is only performed for the new_cases. To run the outbreak detection on the rest
# of the data traces, you can loop the code over the other columns, storing the results in a list.

library(tidyverse)
library(lubridate)
library(firatheme)
library(patchwork)

df <- read.csv('../SISMID23/data/covid_traces_WA.csv')
df$date <- mdy(df$date)

signals = c('new_cases', 'upToDate', 'cdc_ili',
            'Twitter_RelatedTweets', 'google_fever',
            'Kinsa_AnomalousFeverAbsolute')
par(mfrow=c(3, 2))
plot(df$date, df[, signals[1]], type='l', ylab=signals[1])
plot(df$date, df[, signals[2]], type='l', ylab=signals[2])
plot(df$date, df[, signals[3]], type='l', ylab=signals[3])
plot(df$date, df[, signals[4]], type='l', ylab=signals[4])
plot(df$date, df[, signals[5]], type='l', ylab=signals[5])
plot(df$date, df[, signals[6]], type='l', ylab=signals[6])


# a) --------------------------------------------------------------------------
# Recode the process in a tidy way

traces = read_csv('../SISMID23/data/covid_traces_WA.csv') %>% mutate(date = mdy(date))

p1 = traces %>%
  ggplot(aes(x = date)) +
  geom_line(aes(y = new_cases), color = "black") +
  theme_fira() +
  labs(x = 'Date', y = 'New Cases')+
  scale_x_date(date_labels ='%b\n%y') 

p2 = traces %>%
  ggplot(aes(x = date)) +
  geom_line(aes(y = upToDate), color = "gold2") +
  theme_fira() +
  labs(x = 'Date', y = 'upToDate searches')+
  scale_x_date(date_labels ='%b\n%y') 

p3 = traces %>%
  ggplot(aes(x = date)) +
  geom_line(aes(y = cdc_ili), color = "red4") +
  theme_fira() +
  labs(x = 'Date', y = 'ILI')+
  scale_x_date(date_labels ='%b\n%y') 

p4 = traces %>%
  ggplot(aes(x = date)) +
  geom_line(aes(y = Twitter_RelatedTweets), color = "dodgerblue2") +
  theme_fira() +
  labs(x = 'Date', y = 'Twitter')+
  scale_x_date(date_labels ='%b\n%y') 

p5 = traces %>%
  ggplot(aes(x = date)) +
  geom_line(aes(y = google_fever), color = "darkgreen") +
  theme_fira() +
  labs(x = 'Date', y = 'Google')+
  scale_x_date(date_labels ='%b\n%y') 

p6 = traces %>%
  ggplot(aes(x = date)) +
  geom_line(aes(y = Kinsa_AnomalousFeverAbsolute), color = 'purple3') +
  theme_fira() +
  labs(x = 'Date', y = 'Kinsa') +
  scale_x_date(date_labels ='%b\n%y') 
  
(p1 + p2 + p3)/(p4 + p5 + p6)

# (b) -------------------------------------------------------------------------

sliding_slopes = numeric(nrow(traces))
for(i in 11:nrow(traces)){
  
  train = traces$new_cases[(i-10):(i-1)]
  to_predict = traces$new_cases[(i-9):i]
  
  model = lm(to_predict ~ train + 0)
  
  alpha = model$coefficients[1]
  
  if (is.na(alpha) == T){
    alpha = 0
  }
  
  sliding_slopes[i] = alpha
}

sliding_slopes
# (c) --------------------------------------------------------------------------

pred_frame = tibble(
  date = traces$date,
  new_cases = traces$new_cases,
  alphas = sliding_slopes,
  detections = ifelse(alphas > 1, 1, 0)
)

q1 = pred_frame %>%
  ggplot(aes(x = date, y = new_cases)) +
  geom_line(color = 'black')

q2 = pred_frame %>%
  ggplot(aes(x = date, y = alphas)) +
  geom_line(color = 'red4')

q3 = pred_frame %>%
  ggplot(aes(x = date, y = detections)) +
  geom_line(color = 'dodgerblue3')
  
q1/q2/q3

## d) --------------------------------------------------------------------------
out_arr <- numeric(nrow(df))
outbreak_already_active <- FALSE
for (i in 10:nrow(df)) {
  outbreak <- 0
  if (sum(as.integer(sliding_slopes[(i-10):i] > 1)) == 10) {
    if (outbreak_already_active) {
      outbreak <- 0
    }
    else {
      outbreak_already_active <- TRUE
      outbreak <- 1
    }
  }
  else if (sum(as.integer(sliding_slopes[(i-10):i] > 1)) == 0) {
    outbreak_already_active <- FALSE
  }
  out_arr[i] <- outbreak
}

outbreak_array
out_arr

# add the binary outbreak detection to the plot data
pred_frame$outbreak = out_arr
outbreak_array

pred_frame %>%
  ggplot(aes(x = date, y = new_cases)) +
  geom_line(color = 'black') +
  geom_point(data = pred_frame %>% filter(outbreak == 1), color = "blue")


