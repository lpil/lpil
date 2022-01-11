# The Mailing model for the DPD + post deliveries.
class Investor < ActiveRecord::Base
  validates_uniqueness_of :partner_code
  validates_presence_of   :partner_code # TODO More validation
end
