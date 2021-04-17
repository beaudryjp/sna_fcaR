DROP TABLE IF EXISTS `sna_reddit`.`awards_post`;
DROP TABLE IF EXISTS `sna_reddit`.`coin`;
DROP TABLE IF EXISTS `sna_reddit`.`comment`;
DROP TABLE IF EXISTS `sna_reddit`.`post`;
DROP TABLE IF EXISTS `sna_reddit`.`subreddit`;
DROP TABLE IF EXISTS `sna_reddit`.`user`;

CREATE TABLE awards_post (
    post_id   VARCHAR(255) NOT NULL,
    coin_id   VARCHAR(255) NOT NULL,
    quantity  INT
);

ALTER TABLE awards_post ADD CONSTRAINT awards_post_pk PRIMARY KEY ( post_id,
                                                                    coin_id, 
                                                                    quantity );

CREATE TABLE coin (
    id           VARCHAR(255) NOT NULL,
    name         VARCHAR(255),
    description  TEXT,
    coin_price   INT,
    coin_reward  INT
);

ALTER TABLE coin ADD CONSTRAINT coin_pk PRIMARY KEY ( id );

CREATE TABLE comment (
    post_id                 VARCHAR(255) NOT NULL,
    user_id                 VARCHAR(255) NOT NULL,
    structure               VARCHAR(255),
    date                    DATE,
    score                   INT,
    comment                 TEXT
    
);

ALTER TABLE comment ADD CONSTRAINT comment_pk PRIMARY KEY ( post_id, user_id, structure, date);

CREATE TABLE post (
    id                     VARCHAR(255) NOT NULL,
    title                  TEXT,
    selftext               TEXT,
    upvote_ratio           FLOAT,
    ups                    INT,
    total_awards_received  INT,
    score                  INT,
    created                DATETIME,
    date_added             DATETIME,
    permalink              TEXT,
    url                    TEXT,
    domain                 VARCHAR(255),
    subreddit_id           INT NOT NULL,
    user_id                VARCHAR(255) NOT NULL
);

ALTER TABLE post ADD CONSTRAINT post_pk PRIMARY KEY ( id );

CREATE TABLE subreddit (
    id    INT NOT NULL AUTO_INCREMENT,
    name  VARCHAR(255),
    subs  INT,
    CONSTRAINT subreddit_pk PRIMARY KEY (id)
);


CREATE TABLE user (
    id                  VARCHAR(255) NOT NULL,
    name                VARCHAR(255),
    created             DATETIME,
    date_added          DATETIME,
    total_karma         INT,
    comment_karma       INT,
    public_description  TEXT,
    is_employee         BOOLEAN,
    is_gold             BOOLEAN,
    is_mod              BOOLEAN
);

ALTER TABLE user ADD CONSTRAINT user_pk PRIMARY KEY ( id );

ALTER TABLE awards_post
    ADD CONSTRAINT awards_post_coin_fk FOREIGN KEY ( coin_id )
        REFERENCES coin ( id );

ALTER TABLE awards_post
    ADD CONSTRAINT awards_post_post_fk FOREIGN KEY ( post_id )
        REFERENCES post ( id );

ALTER TABLE comment
    ADD CONSTRAINT comment_user_fk FOREIGN KEY ( user_id )
        REFERENCES user ( id );
        
ALTER TABLE comment
    ADD CONSTRAINT comment_post_fk FOREIGN KEY ( post_id )
        REFERENCES post ( id );

ALTER TABLE post
    ADD CONSTRAINT post_subreddit_fk FOREIGN KEY ( subreddit_id )
        REFERENCES subreddit ( id );

ALTER TABLE post
    ADD CONSTRAINT post_user_fk FOREIGN KEY ( user_id )
        REFERENCES user ( id );


CREATE VIEW posts_per_subreddit AS
SELECT
       subreddit.name as subreddit, COUNT(post.id) as posts
FROM
     sna_reddit.subreddit, sna_reddit.post
WHERE
      subreddit.id = post.subreddit_id
GROUP BY subreddit.name
ORDER BY subreddit.name;


