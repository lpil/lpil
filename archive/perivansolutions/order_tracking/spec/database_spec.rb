require_relative 'spec_helper'

describe Database do
  describe 'self.establish_connection' do
    it 'calls ActiveRecord::Base.establish_connection with sqlite3' do
      expect(ActiveRecord::Base)
        .to receive(:establish_connection)
        .with(hash_including adapter: 'sqlite3')

      Database.establish_connection
    end
  end

  describe 'self.migrate' do
    it 'calls ActiveRecord::Migrator.migrate' do
      expect(ActiveRecord::Migrator)
        .to receive(:migrate)

      Database.migrate
    end
  end
end
