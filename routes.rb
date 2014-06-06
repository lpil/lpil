require 'yaml'

get '/' do
  "The time at the server is #{Time.now.strftime '%l:%M %P'}."
end

#
# DPD + post delivery tracking
#

get '/delivery' do
  if params['order_ref']
    @dpd_url = 'http://www.dpd.co.uk/apps/tracking/?reference='
    @mailing = Mailing.find_by_order_ref(params[:order_ref])
    slim :deliveries_result
  else
    slim :deliveries_form
  end
end

#
# AXA Upload
#

get '/axa_upload' do
  @info = { Review: '00/00/0000' }
  erb :'axa_upload.html'
end

post '/axa_upload' do
  result = AxaUpload.check params
  AxaUpload.email result unless result[:failed]
  @info = result[:failed] ? result : { Review: '00/00/0000', success: true }
  erb :'axa_upload.html'
end

#
# Status info
#

get '/status' do
  slim :status
end
