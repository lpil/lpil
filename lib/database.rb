class Database
  def self.establish_connection
    ActiveRecord::Base.establish_connection(
      adapter:  'sqlite3',
      database: 'orders.sqlite3'
    )
  end

  def self.migrate
    Database.establish_connection

    ActiveRecord::Migrator.migrate(
      'migrations', ENV['VERSION'] ? ENV['VERSION'].to_i : nil)
  end
end
