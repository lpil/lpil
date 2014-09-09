every 1.hours do
  rake 'orders:fetch'
  rake 'orders:parse'
  rake 'orders:clean'
end
