class AddDefaultValueToUploaderAndReporter < ActiveRecord::Migration
  def up
    change_column :users, :uploader, :boolean, default: false
    change_column :users, :reporter, :boolean, default: false
  end

  def down
    change_column :users, :uploader, :boolean, default: nil
    change_column :users, :reporter, :boolean, default: nil
  end
end
