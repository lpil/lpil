# post '/axa_upload' do
#   result = AxaUpload.parse_form params
#   AxaUpload.email result if result[:success]
#   @info = result
#   erb :'axa_upload.html'
# end

require 'yaml'
require 'pony'

# The AxaUpload model for 'uploading' documents to the store
class AxaUpload
  def self.check(params)
    params.symbolize_keys!
    validations = [[:Name, /.+/, 'Name'],
                   [:Email, /.+@.+\..+/, 'Email'],
                   [:Productname, /.+/, 'Product Name'],
                   [:Team, /.+/, 'Team'],
                   [:Doctype, /.+/, 'Document Type'],
                   [:Contact, /.+/, 'Document Owner'],
                   [:Review, %r{[0-3]\d/[01]\d/2\d\d\d}, 'Expiry Date'],
                   [:Category, /.+/, 'Main Category']]
    invalid = []
    validations.each do |v|
      invalid << v[2] unless params[v[0]] =~ v[1]
    end
    invalid << 'File to be uploaded' unless params[:Upload]

    params[:invalid], params[:failed] = invalid, true if invalid.any?
    params.delete :Upload if invalid.any?
    params
  end

  def self.email(params)
    info = params.except(:'Submit.x', :'Submit.y')
    info[:Upload] = params[:Upload][:filename]

    body = info.reduce('') { |a, e| a << "#{e[0]} => #{e[1]}\n" }

    Pony.mail(
      to: 'axa@perivan.co.uk',
      subject: 'AXA Product Upload Request',
      body: body,
      attachments: {
        'upload_data.yml' => YAML.dump(info),
        params[:Upload][:filename] => File.binread(params[:Upload][:tempfile])
      }
    )
  end
end
