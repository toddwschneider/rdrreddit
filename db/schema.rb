# encoding: UTF-8
# This file is auto-generated from the current state of the database. Instead
# of editing this file, please use the migrations feature of Active Record to
# incrementally modify your database, and then regenerate this schema definition.
#
# Note that this schema.rb definition is the authoritative source for your
# database schema. If you need to create the application database on another
# system, you should be using db:schema:load, not running all the migrations
# from scratch. The latter is a flawed and unsustainable approach (the more migrations
# you'll amass, the slower it'll run and the greater likelihood for issues).
#
# It's strongly recommended that you check this file into your version control system.

ActiveRecord::Schema.define(version: 20141019172923) do

  # These are extensions that must be enabled in order to support this database
  enable_extension "plpgsql"

  create_table "delayed_jobs", force: true do |t|
    t.integer  "priority",   default: 0, null: false
    t.integer  "attempts",   default: 0, null: false
    t.text     "handler",                null: false
    t.text     "last_error"
    t.datetime "run_at"
    t.datetime "locked_at"
    t.datetime "failed_at"
    t.string   "locked_by"
    t.string   "queue"
    t.datetime "created_at"
    t.datetime "updated_at"
  end

  add_index "delayed_jobs", ["priority", "run_at"], name: "delayed_jobs_priority", using: :btree

  create_table "observations", force: true do |t|
    t.integer  "post_id"
    t.datetime "observed_at"
    t.integer  "front_page_rank"
    t.integer  "score"
    t.integer  "num_comments"
    t.boolean  "stickied"
    t.integer  "best_previous_front_page_rank"
    t.datetime "created_at"
    t.datetime "updated_at"
  end

  add_index "observations", ["post_id", "observed_at"], name: "index_observations_on_post_id_and_observed_at", unique: true, using: :btree

  create_table "posts", force: true do |t|
    t.string   "name"
    t.string   "title",                                  limit: 1000
    t.string   "permalink"
    t.string   "url",                                    limit: 4000
    t.string   "subreddit"
    t.string   "subreddit_id"
    t.float    "created_utc"
    t.datetime "posted_at"
    t.string   "author"
    t.integer  "best_front_page_rank"
    t.datetime "first_observed_at"
    t.datetime "last_observed_at"
    t.datetime "best_front_page_rank_first_observed_at"
    t.integer  "max_score"
    t.integer  "max_num_comments"
    t.string   "domain"
    t.string   "media_type"
    t.integer  "imgur_views"
    t.datetime "created_at"
    t.datetime "updated_at"
  end

  add_index "posts", ["name"], name: "index_posts_on_name", unique: true, using: :btree

  create_table "reddit_observations", force: true do |t|
    t.text     "response"
    t.datetime "created_at"
    t.datetime "updated_at"
  end

end
