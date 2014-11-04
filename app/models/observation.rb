class Observation < ActiveRecord::Base
  belongs_to :post
  
  validates_presence_of :post_id, :observed_at, :front_page_rank
  validates_uniqueness_of :observed_at, :scope => :post_id
  validates_numericality_of :front_page_rank, :greater_than => 0
end
