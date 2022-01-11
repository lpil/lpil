class CreateCategories < ActiveRecord::Migration
  def change
    create_table :categories do |t|
      t.string :name
      t.string :ancestry, index: true

      t.references :collection, index: true

      t.timestamps
    end
  end
end
