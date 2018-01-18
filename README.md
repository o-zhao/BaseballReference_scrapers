# BaseballReference_scrapers

For all of the wealth of MLB data available to sabermetricians, there's no analogous manner to obtain players' stats from minor and/or foreign league service time. These scripts use the `rvest` package in R in scrape minor league stats. It also returns personal information (like handedness and birthdate) and draft status for domestic players to help with player identification.

### `bbr_batters_scrape.R` and `bbr_pitchers_scrape.R`
First grabs the names of all players for all designated season-organization pairs. This includes players in all minor league affiliates of a team as well as the major league roster. Players are identified by their Baseball-Reference minor league register ID, a string unique to the player's minor league service time. Note that Baseball-Reference assigns a different major league ID to players with major league service time.

Next scrapes information from each players' minor league page, even for players on the major league roster for that season-organization pair. Results are output in three tables:

  1) <b>Stats</b>: offensive statistics for batters and pitching statistics for pitchers. With easy modifications, you could pull defensive statistics, but for my personal use, there's much more to be refined with defensive stats, and the defensive stats provided on Baseball-Reference aren't of much use. This includes observations for years spent with foreign or independent league teams, though statistics for those observations are often missing.
  2) <b>Roster</b>: roster history (list of teams/affiliates player for). For domestic teams and affiliates, exact service dates are provided. Roster history also includes time with foreign or independent league teams, though service dates are often missing.
  3) <b>Personal Information</b>: personal information. Includes a string variable of their name, position(s) (up to three listed in descending order of games played at each position), batting and throwing handedness, birthdate, birthplace.

All tables are linked by players' Baseball-Reference minor league register ID (variable name `bbrid`)

### `bbr_draft_scrape.R`
Draft information for domestic players is available on their pages and easy to scrape, but parsing the long string variable isn't the cleanest. Instead, you can scrape draft results from tables and merge the information into the above tables using the Baseball-Reference ID `bbrid`.
