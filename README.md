# sna_fcaR

Project in which we will Analyse and Extract Knowledge from social networks using FCA

# Application

Most of this applications is structured using modules for the UI & Server side, the benefits of this is that we can use reuse certain parts of the application for different cases.

This applications offers two different uses:

1. Download new information from posts
2. Work with the current information present in the database
    2.1. We can obtain basic information from the data
    2.2. We can obtain apply some text mining techniques on the data obtained.
    2.2. We can create a formal context and obtain concepts, implications.


Aswell, there is included a script which can be executed as a scheduled task on windows or linux systems to obtain the top posts per day every 12h.

# Docker

## Shiny

There is a Dockerfile included in the directory docker, which has the basic configuration needed for the shiny application.

The app must be located in a folder called app.

To build this custom image we must use the command: 

    docker build --tag custom-shiny .

To run a container with the custom image we must use the command:

    docker run -d --name rshiny -p 3838:3838 custom-shiny 

## Database

This application is using a MariaDB 10.5 server mounted on a docker container.

The commands used are as follows:

    docker run -p 3307:3306 --name mariadb -e MYSQL_ROOT_PASSWORD=root123 -d mariadb/server && sleep 5 && \
    docker exec -it mariadb mysql -uroot -proot123 -e "create database sna_reddit" && sleep 5 && \
    docker exec -it mariadb mysql -uroot -proot123 -e "CREATE USER 'david'@'%' IDENTIFIED BY 'p1YFHaVBpDmmurLU'; " && sleep 5 &&
    docker exec -it mariadb mysql -uroot -proot123 -e "grant all privileges on sna_reddit.* TO 'david'@'%' identified by 'p1YFHaVBpDmmurLU';FLUSH PRIVILEGES;" && sleep 5 && \
    docker run --name mariadb-phpmyadmin -d --link mariadb:db -p 8081:80 phpmyadmin/phpmyadmin

It is recommended to change the root password to a secure password.

The applications has two different implementations for the database access:

1. Using the standard implementation with DBI and RMariaDB so that for each operation we open a connection to the database and close it.
2. Using the pool implementation with pool so that we use the same connection for all the operations needed, using this option there was a increase in memory usage & sluggishness in the application.

# Reddit API

The API of reddit has a limit rate configured so we can only make a certain number of petitions. At this current moment the max limit is set to 100.

On this application we have two options to extract from the API:

1. Select a list of subreddits and get the top X posts for each subreddit
2. Select a subreddit or a tag like "popular" and get the posts

Depending on the options selected and the number of posts to obtain in each case the operation can take more or less.

For example: 

- To obtain the top 5 rising posts in the current day for the subreddits "WorldNews", "News", "Memes", "AskReddit" can take approximately 5 to 10min.
- To obtain the top 100 popular posts in the current day can take approximately 20 to 30min.

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
