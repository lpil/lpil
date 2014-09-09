every :hour do
  rake 'orders:fetch'
  rake 'orders:parse'
end

every :day, at: '12:34am' do
  rake 'orders:clean'
end
