require './config/boot'
require './config/environment'

require 'clockwork'
include Clockwork

every(5.minutes, 'record reddit observation') { RedditObservation.record_observation }
