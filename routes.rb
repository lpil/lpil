get '/' do
  "The time at the server is #{Time.now.strftime '%l:%M %P'}."
end

#
# DPD + post delivery tracking
#

get '/deliveries' do
  slim :deliveries_form
end

post '/deliveries' do
  @dpd_url = 'http://www.dpd.co.uk/apps/tracking/?reference='
  @mailing = Mailing.find_by_order_ref(params[:order_ref])
  slim :deliveries_result
end
