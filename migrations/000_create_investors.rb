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

      t.integer :pim_quantity, default: 0
      t.integer :qir_quantity, default: 0
      t.integer :gim_quantity, default: 0

      t.integer :cd_wallet,            default: 0
      t.integer :cd_custom_wallet,     default: 0
      t.integer :cd_custom_wallet_rec, default: 0

      t.integer :trust_cd_generic,  default: 0
      t.integer :trust_cd_personal, default: 0

      t.string :pdf

      t.timestamps
    end

    add_index :investors, :partner_code
  end
end
