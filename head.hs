head1 :: [a] -> a
head1 []     = error "Prelude.head: empty list"
head1 (x:_) = x
