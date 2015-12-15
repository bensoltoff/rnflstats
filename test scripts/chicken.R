# chicken.R
# Gregg Easterbrook calls out teams which punt on 4th down when they are losing by a significant margin.
# How often do those teams go on to win or lose?
# Compared to teams which go for it - is there a difference in game outcomes?

require(dplyr)
require(readr)
require(ggplot2)
require(rnflstats)
require(Zelig)

rm(list = ls())

# load data
nfl <- src_sqlite("./data/armchair.sqlite")

# pull all 4th down plays where the offense is down by 10 or more in the 4th quarter and runs a play
play <- tbl(nfl, "play") %>%
  mutate(ptsdiff = ptso - ptsd) %>%
  filter(dwn == 4, qtr >= 3, ptsdiff <= -14, type != "NOPL")

# join play with game info to determine eventual winner
play %>%
  left_join(tbl(nfl, "game")) %>%
  # ignore tied outcomes
  filter(ptsh != ptsv) %>%
  # determine who won based on current play: the offensive team or the defensive team
  # collect %>%
  mutate(winh = ifelse(ptsh > ptsv, 1, 0),
         offh = ifelse(off == h, 1, 0),
         wino = winh == offh) %>%
  filter(seas == 2003, off == "BAL", def == "SEA") %>%
  select(off, def, h, v, winh, offh, wino)

# initial stats
play %>%
  group_by(wino, type) %>%
  summarize(n = n()) %>%
  collect



