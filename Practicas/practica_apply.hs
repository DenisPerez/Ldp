module Apply where 
    apply :: (a->a)-> a -> a
    apply f x = f x