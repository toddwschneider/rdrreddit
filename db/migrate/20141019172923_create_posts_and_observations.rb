class CreatePostsAndObservations < ActiveRecord::Migration
  def change
    create_table :reddit_observations do |t|
      t.text :response
      t.timestamps
    end
    
    create_table :posts do |t|
      t.string :name
      t.string :title, :limit => 1000
      t.string :permalink
      t.string :url, :limit => 4000
      t.string :subreddit
      t.string :subreddit_id
      t.float :created_utc
      t.datetime :posted_at
      t.string :author
      t.integer :best_front_page_rank
      t.datetime :first_observed_at
      t.datetime :last_observed_at
      t.datetime :best_front_page_rank_first_observed_at
      t.integer :max_score
      t.integer :max_num_comments
      t.string :domain
      t.string :media_type
      t.integer :imgur_views
      t.timestamps
    end
    
    add_index :posts, :name, :unique => true
    
    create_table :observations do |t|
      t.integer :post_id
      t.datetime :observed_at
      t.integer :front_page_rank
      t.integer :score
      t.integer :num_comments
      t.boolean :stickied
      t.integer :best_previous_front_page_rank
      t.timestamps
    end
    
    add_index :observations, [:post_id, :observed_at], :unique => true
  end
end
