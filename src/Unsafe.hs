module Unsafe where

import qualified Prelude
import qualified Data.Maybe
import qualified Data.List

unsafeHead :: [a] -> a
unsafeHead x
  | Data.List.length x Prelude.> 0 = Prelude.head x
  | Prelude.otherwise = Prelude.undefined

unsafeTail :: [a] -> [a]
unsafeTail x
  | Data.List.length x Prelude.> 0 = Prelude.tail x
  | Prelude.otherwise = Prelude.undefined

unsafeInit :: [a] -> [a]
unsafeInit x
  | Data.List.length x Prelude.> 0 = Prelude.init x
  | Prelude.otherwise = Prelude.undefined

unsafeLast :: [a] -> a
unsafeLast x
  | Data.List.length x Prelude.> 0 = Prelude.last x
  | Prelude.otherwise = Prelude.undefined

fromJust :: Prelude.Maybe a -> a
fromJust = Data.Maybe.fromJust

unsafeIndex :: [a] -> Prelude.Int -> a
unsafeIndex list index
  | index Prelude.>= 0 Prelude.&& Data.List.length list Prelude.> index = list Data.List.!! index
  | Prelude.otherwise = Prelude.undefined

(!!) :: [a] -> Prelude.Int -> a
(!!) = unsafeIndex
