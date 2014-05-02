class CreateMailings < ActiveRecord::Migration
  def change
    create_table :mailings do |t|
      t.string :order_ref
      t.boolean :is_post
      t.string :dpd_ref
      t.date :date_sent
      t.timestamps
    end
  end
end
