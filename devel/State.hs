import Control.Monad.State.Strict

data Foo = Foo { value :: Int } deriving (Eq, Show)

myFun :: Int -> StateT Foo IO ()
myFun i = do
  old <- get
  let newFoo = Foo (i + value old)
  put newFoo


main = do
  runStateT (myFun 1) (Foo 1)
