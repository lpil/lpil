class AddLockedBooleanToCategories < ActiveRecord::Migration
  def change
    add_column :categories, :locked, :boolean, default: false
  end
end
