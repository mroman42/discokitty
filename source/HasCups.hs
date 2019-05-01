module HasCups where
class HasCups m where
  cup :: Int -> m -> m -> m
  cunit :: m
