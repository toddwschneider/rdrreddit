# rdrreddit

Materials in support of this post: http://toddwschneider.com/posts/the-reddit-front-page-is-not-a-meritocracy/

There are 3 main components to the repo:

## 1. Rails application that grabs the top 100 items from reddit every 5 minutes

The app is not intended to be used as a web server, just as a clock process and delayed job worker. You can run it with:

`bundle exec foreman start -f Procfile.clockandworker`

The clock dumps a blob of serialized text into the `reddit_observations` table every 5 minutes, then a delayed job worker processes each of those blobs into the `posts` and `observations` tables. Some additional methods cache a few attributes on those tables, and fetch data fromt the Imgur API -- these methods are run manually from the Rails console

## 2. R scripts for data analysis

[reddit_analysis.R](https://github.com/toddwschneider/rdrreddit/blob/master/R/reddit_analysis.R) does the heavy lifting

## 3. Postgres database dump file

[rdr_seed.dump](https://github.com/toddwschneider/rdrreddit/blob/master/dbdump/rdr_seed.dump) contains data from the reddit top 100 between September 15 and October 31, 2014

It includes only the `posts` and `observations` tables -- the raw content in `reddit_observations` table would take up too much space, and none of the analysis depends on that table anyway. You can restore the database on your local machine with `pg_restore` (you have to [install postgres](http://www.postgresql.org/download/) first if you haven't yet):

`pg_restore --verbose --clean --no-acl --no-owner -h localhost -d rdrreddit_development /path/to/rdr_seed.dump`

The dump file is about 25 mb compressed, and will take up 175 mb on disk once fully restored
