* Encoding: UTF-8.

PRESERVE.
SET DECIMAL DOT.

GET DATA  /TYPE=TXT
  /FILE=
    "\\tsclient\C\Users\madou\_GitHub\Stat_Crunch_Competition\Subset_of_Twitch_Streamer_Data_2023.csv"
  /ENCODING='UTF8'
  /DELCASE=LINE
  /DELIMITERS=","
  /QUALIFIER='"'
  /ARRANGEMENT=DELIMITED
  /FIRSTCASE=2
  /DATATYPEMIN PERCENTAGE=95.0
  /VARIABLES=
  Channel A38
  Watchtime AUTO
  Streamtime AUTO
  Peakviewers AUTO
  Averageviewers AUTO
  FollowersPrevYr AUTO
  Followers AUTO
  Followersgained AUTO
  Partnered AUTO
  Mature AUTO
  Meanweeklywatchhours AUTO
  Meanweeklystreamhours AUTO
  Followers100000 AUTO
  @1Averageviewers AUTO
  lnFollowers AUTO
  lnAverageviewers AUTO
  lnMeanweeklywatchhours AUTO
  lnMeanweeklystreamhours AUTO
  /MAP.
RESTORE.

CACHE.
EXECUTE.
DATASET NAME DataSet1 WINDOW=FRONT.
