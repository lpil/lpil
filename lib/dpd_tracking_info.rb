#!/usr/bin/env ruby
# encoding: utf-8

require 'httparty'

# Fetches the DPD tracking info for a specific DPD tracking number.
class DpdTrackingInfo
  # Fetches the DPD tracking info for a specific DPD tracking number.
  #
  # @param tracking_num [string] The DPD Tracking number
  # @return [hash] The DPD tracking information
  def self.lookup(tracking_num)
    post_xml = <<-HEREDOC
<?xml version="1.0" encoding="UTF-8"?>
<trackingrequest>
  <user>GEOTRACK</user>
  <password>g30tr4ck</password>
  <trackingnumbers>
    <trackingnumber>#{tracking_num}</trackingnumber>
  </trackingnumbers>
</trackingrequest>
    HEREDOC

    JSON.parse(HTTParty.post(
      'https://test-apps.geopostuk.com/tracking-core/dpd/parcels',
      headers: {
        'Content-Type' => 'text/xml',
        'Accept' => 'application/json'
      },
      body: post_xml
    ).body)
  end
end

# puts DpdTrackingInfo.lookup('1355440686')

# FIXME: username and password in the XML
