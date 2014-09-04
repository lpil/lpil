get '/' do
  "The time at the server is #{Time.now.strftime '%l:%M %P'}."
end

#
# DPD + post delivery tracking
#
get '/delivery.?:format?' do
  return slim :deliveries_form unless params['order_ref']

  @data = Mailing.lookup params[:order_ref]

  case params[:format]
  when /json/i
    content_type 'application/json'
    @data.to_json
  else
    slim :deliveries_result
  end
end

#
# Status info
#
get '/status' do
  slim :status
end

#
# JSONP callback filter
# This should be the final body altering filter for JSON
#
after '*.json' do
  return unless params[:callback]

  content_type 'application/javascript'
  @response.body = "#{params[:callback]}(#{@response.body.first});"
end
