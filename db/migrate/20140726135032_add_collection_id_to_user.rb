class AddCollectionIdToUser < ActiveRecord::Migration
  def change
    add_column :users, :collection_id, :integer
    add_index :users, :collection_id
  end
end
