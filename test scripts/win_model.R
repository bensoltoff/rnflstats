# Probability the home team will win
# 
# relevant factors
# -visitor point spread
# -time
# -down
# -distance to go
# -who has the ball

require(dplyr)
require(readr)
require(ggplot2)
require(rnflstats)
require(Zelig)

rm(list = ls())

# load data
nfl <- load_data()

# assemble data into necessary format
## pbp
play <- tbl(nfl, "play") %>%
  select(gid:def, qtr:zone) %>%
  filter(dwn != 0) %>%
  # calculate total minutes left in game
  mutate(minleft = (abs(qtr - 4) * 15) + min + (sec / 60))

## game
game <- tbl(nfl, "game") %>%
  select(gid, v:h, sprv:ptsh) %>%
  # calculate winner/loser/tie
  mutate(winh = if(ptsh > ptsv) 1 else if(ptsh == ptsv) NA else 0)

# merge necessary data
windata <- left_join(play, game) %>%
  # determine if home team is on offense
  mutate(hoff = if(off == h) 1 else 0) %>%
  # calculate point difference
  mutate(ptsdiff = if(off == h) ptso - ptsd else ptsd - ptso) %>%
  # recalculate down, ytg, yfog based on offensive perspective
#   mutate(dwn = if(off == h) dwn else dwn * -1,
#          ytg = if(off == h) ytg else ytg * -1,
#          yfog = if(off == h) yfog else yfog * -1) %>%
  collect

# fit basic model
vegas <- zelig(winh ~ sprv, model = "logit", data = windata)
summary(vegas)

# more complexity
winmodel <- zelig(winh ~ sprv + (minleft + dwn + ytg + yfog) * hoff, model = "logit", data = windata)
summary(winmodel)

