# sna_fcaR

Project in which we will Analyse and Extract Knowledge from social networks using FCA

# Reddit API

## API Request Parameters
*subreddit*

subreddit on which the get or search information


*sort*	

one of (relevance, hot, top, new, comments)


*limit*	

the maximum number of items desired (default: 25, maximum: 100)


*timeframe*	

one of (hour, day, week, month, year, all)


*listing*

controversial, best, hot, new, random, rising, top


*restrict_sr*

defaults to searching the entirety of reddit. 

By adding the restrict_sr flag, the search will be restricted to the relevant subreddit.

## API Search Request Structure
https://www.reddit.com/r/{SUBREDDIT}/search/?q={QUERY}&restrict_sr={TRUE/FALSE}&sort={SORTBY}&t={TIMEFRAME}

## API GET Request Structure
https://www.reddit.com/r/{SUBREDDIT}/{LISTING}.json?limit={LIMIT}&t={TIME}
