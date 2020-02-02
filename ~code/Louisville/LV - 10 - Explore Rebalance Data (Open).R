##########################################################################
# This script produces visualizations of the rebalancing dataset:
# 1. Line graph of rebalancing for each scooter
#   a. by month and year
#   b. by day of week
# 2. Table of each providers' contribution to rebalancing
##########################################################################

# plot the frequency of rebalancing for each scooter ---- 
# by month and year
LV_rebal_byScooter_monthYear <- LV_rebal_rebalance_only %>%
  group_by(year(occurredAt), month(occurredAt)) %>%
  summarise(count = n(), per = count / length(unique(vehicleId))) %>% 
  rename(year = 'year(occurredAt)',
         month = 'month(occurredAt)') %>% 
  mutate(month = ifelse(month <= 9, 
                        paste('0', month, sep = ''), 
                        as.character(month)),
         date = paste(year, month, sep = ''))

ggplot(data = LV_rebal_byScooter_monthYear, 
       aes(date, per, group = 1))+
  geom_line(size = 1) +
  plotTheme

# by day of week
LV_rebal_byScooter_DOW <- LV_rebal_rebalance_only %>%
  group_by(month(occurredAt), weekdays(occurredAt)) %>%
  summarise(count = n(), per = count / length(unique(vehicleId))) %>% 
  rename(month = 'month(occurredAt)',
         weekdays = 'weekdays(occurredAt)') %>% 
  group_by(weekdays) %>% 
  summarise(perd = mean(per)) %>% 
  mutate(weekdays = factor(weekdays,
                           levels = c('Monday','Tuesday','Wednesday','Thursday','Friday','Saturday','Sunday')))

ggplot(data = LV_rebal_byScooter_DOW, aes(weekdays, perd, group = 1))+
  geom_line(size = 1) +
  plotTheme

# Which companies contribute most to rebalancing? ---- 
LV_company_contribution <- as.data.frame(prop.table(table(LV_rebal_rebalance_only$operators)),
                                         stringsAsFactors = FALSE) %>% 
  rename(Provider = Var1,
         Proportion = Freq) %>% 
  mutate(Proportion = round(as.numeric(Proportion), 2))

LV_company_contribution %>% 
  kable(caption = "Contribution by providers") %>%
  kable_styling("striped", full_width = F,position = "center") %>%
  row_spec(2, color = "white", background = "#33638dff") %>%
  row_spec(4, color = "white", background = "#33638dff")