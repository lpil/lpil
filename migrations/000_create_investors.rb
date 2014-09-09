class CreateInvestors < ActiveRecord::Migration
  def change
    create_table :investors do |t|
      t.string :partner_code, null: false
      t.string :name, null: false

      t.string :address0
      t.string :address1
      t.string :address2
      t.string :address3
      t.string :post_code

      t.integer :pim_quantity, null: false, default: 0
      t.integer :qir_quantity, null: false, default: 0
      t.integer :gim_quantity, null: false, default: 0

      t.integer :cd_wallet,            null: false, default: 0
      t.integer :cd_custom_wallet,     null: false, default: 0
      t.integer :cd_custom_wallet_rec, null: false, default: 0

      t.integer :trust_cd_generic,  null: false, default: 0
      t.integer :trust_cd_personal, null: false, default: 0

      t.timestamps
    end

    add_index :investors, :partner_code
  end
end
