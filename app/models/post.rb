class Post < ActiveRecord::Base
  has_many :observations, :dependent => :destroy
  
  validates_presence_of :name
  
  class << self
    def cache_post_attributes
      totals_query = <<-SQL
        UPDATE posts
        SET 
          best_front_page_rank = totals.best_front_page_rank,
          first_observed_at = totals.first_observed_at,
          last_observed_at = totals.last_observed_at,
          max_score = totals.max_score,
          max_num_comments = totals.max_num_comments
        FROM
          (SELECT post_id,
                  MIN(front_page_rank) AS best_front_page_rank,
                  MIN(observed_at) AS first_observed_at,
                  MAX(observed_at) AS last_observed_at,
                  MAX(score) AS max_score,
                  MAX(num_comments) AS max_num_comments
           FROM observations
           GROUP BY post_id) AS totals
        WHERE posts.id = totals.post_id
      SQL
      
      connection.execute(totals_query)
      
      best_at_query = <<-SQL
        UPDATE posts
        SET best_front_page_rank_first_observed_at = f.observed_at
        FROM
          (SELECT DISTINCT ON (o.post_id) o.post_id, o.observed_at
           FROM posts p INNER JOIN observations o ON p.id = o.post_id
           WHERE o.front_page_rank = p.best_front_page_rank
           ORDER BY o.post_id, o.observed_at ASC) f
        WHERE
          posts.id = f.post_id
      SQL
      
      connection.execute(best_at_query)
      
      best_prev_rank_query = <<-SQL
        UPDATE observations
        SET best_previous_front_page_rank = prev.best_prev_rank
        FROM
          (SELECT
             o.id, MIN(oprev.front_page_rank) AS best_prev_rank
           FROM
           observations o
             INNER JOIN observations oprev ON o.post_id = oprev.post_id AND o.observed_at >= oprev.observed_at
           GROUP BY o.id) AS prev
        WHERE observations.id = prev.id
      SQL
      
      connection.execute(best_prev_rank_query)
    end
    handle_asynchronously :cache_post_attributes
    
    def trim_posts_and_observations
      raise "Can't only run this in development" unless Rails.env.development?
      
      min_observed_at = RedditObservation.minimum(:created_at)
      max_observed_at = RedditObservation.maximum(:created_at)
      
      incomplete_post_ids = Observation.select("DISTINCT post_id").where("observed_at IN (?)", [min_observed_at, max_observed_at]).map(&:post_id)
      
      Post.where(:id => incomplete_post_ids).destroy_all
    end
  end
  
  scope :imgur, -> { where("domain ILIKE '%imgur.com%'") }
  
  def imgur?
    !!(domain =~ /imgur\.com$/i)
  end
  
  def cache_imgur_views
    return unless imgur?
    
    self.imgur_views = fetch_imgur_data.to_h["data"].to_h["views"]
    save!
  end
  handle_asynchronously :cache_imgur_views
  
  def fetch_imgur_data
    return unless imgur?
    
    image_id = url.split("/").compact.last.split(".").first
    auth = {'Authorization' => "Client-ID #{ENV['IMGUR_CLIENT_ID']}"}
    
    JSON.parse(RestClient.get("https://api.imgur.com/3/image/#{image_id}", auth))
  rescue RestClient::ResourceNotFound
  end
end
