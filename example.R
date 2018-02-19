library(data.table)
userinputnew$Date <- as.POSIXct(userinputnew$Date); activeenergyburnednew$Date <- as.POSIXct(activeenergyburnednew$Date)
activeenergyburnednew <- setDT(activeenergyburnednew)
res <- setDT(userinputnew)[activeenergyburnednew, roll = -Inf, on = .(Date)][, .(Qty = sum(total)),
                                                                             .(stresslevel)][userinputnew, on = .(stresslevel)][is.na(Qty), Qty := 0]
setcolorder(res, c(names(userinputnew), "Qty"))
View(res)

dffinal <- res %>%
  select(Date, stresslevel, Quantity) %>%
  filter(Quantity > 0)
View(dffinal)


library(data.table)
activeenergyburnednew <- setDT(activeenergyburnednew)
res <- setDT(userinputnew)[activeenergyburnednew, roll = -Inf, on = .(Date)][, .(Qty = sum(total)),
                                                                             .(stresslevel)][userinputnew, on = .(stresslevel)][is.na(Qty), Qty := 0]
setcolorder(res, c(names(userinputnew), "Qty"))
View(res)

library(data.table)
rbindlist(list(userinputnew, activeenergyburnednew))[, sum(total), Date]

library(dplyr)
library(tidyr)

dfA <- full_join(userinputnew, activeenergyburnednew, by = "Date") %>% 
  arrange(Date) %>% 
  fill(stresslevel, .direction = "up") %>% 
  group_by(stresslevel) %>% 
  summarize(Qty = sum(total, na.rm = TRUE)) %>% 
  ungroup() %>% 
  select(Qty) %>% 
  bind_cols(userinputnew, .)

View(dfA)



library(dplyr)

dfA <- bind_rows(userinputnew, activeenergyburnednew) %>%
  group_by(Date) %>%
  summarise_all(sum)

View(dfA)



library(data.table)
library(dplyr)
userinputnew$startdate <- as.POSIXct(userinputnew$startdate); activeenergyburnednew$startdate <- as.POSIXct(activeenergyburnednew$startdate)
res <- setDT(userinputnew)[activeenergyburnednew, roll = -Inf, on = .(Date)][, .(Qty = sum(total)),
                                                                             .(stresslevel)][userinputnew, on = .(stresslevel)][is.na(Qty), Qty := 0]
setcolorder(res, c(names(userinputnew), "Qty"))
View(res)

dffinal <- res %>%
  select(startdate, stresslevel, Quantity) %>%
  filter(Quantity > 0)
View(dffinal)