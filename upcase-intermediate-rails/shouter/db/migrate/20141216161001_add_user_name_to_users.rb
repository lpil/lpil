class AddUserNameToUsers < ActiveRecord::Migration
  def change
    add_column :users, :username, :string
    add_index :users, :username
  end
end
