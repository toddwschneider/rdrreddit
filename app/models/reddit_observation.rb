class RedditObservation < ActiveRecord::Base
  validates_presence_of :response
  serialize :response
  
  after_create :create_posts_and_observations
  
  class << self
    def record_observation
      response = JSON.parse(RestClient.get("http://www.reddit.com/.json?limit=100"))
      create! { |ro| ro.response = response }
    end
    handle_asynchronously :record_observation
  end
  
  def create_posts_and_observations
    response["data"]["children"].each_with_index do |obs, ix|
      data = obs["data"]

      post = Post.find_or_create_by!(:name => data["name"]) do |p|
        p.name = data["name"]
        p.title = data["title"]
        p.permalink = data["permalink"]
        p.url = data["url"]
        p.domain = data["domain"]
        p.subreddit = data["subreddit"]
        p.subreddit_id = data["subreddit_id"]
        p.media_type = data["media"].to_h["type"]
        p.created_utc = data["created_utc"]
        p.posted_at = Time.zone.at(data["created_utc"])
        p.author = data["author"]
      end

      begin
        post.observations.create! do |o|
          o.observed_at = created_at
          o.front_page_rank = ix + 1
          o.score = data["score"]
          o.num_comments = data["num_comments"]
          o.stickied = data["stickied"]
        end
      rescue ActiveRecord::RecordNotUnique, ActiveRecord::RecordInvalid, PG::UniqueViolation
        next
      end
    end
  end
  handle_asynchronously :create_posts_and_observations
end
