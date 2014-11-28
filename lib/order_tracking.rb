require 'active_record'
require 'csv'
require 'net/ftp'
require 'sqlite3'
require 'yaml'

Dir['./lib/*.rb'].each { |lib| require lib }

ftp_config = YAML.load_file 'config/ftp.yml'

FTP_CONFIG = [
  ftp_config[:site],
  ftp_config[:user],
  ftp_config[:pass]
]
