import Test.HUnit

import Control.Exception
import Data.Typeable

errorCall :: ErrorCall -> Maybe b
errorCall (ErrorCall _) = Just undefined
errorCall _             = Nothing

expectError         :: String -> a -> IO ()
expectError msg sut =
  do
    handleJust errorCall (\_ -> return ()) performCall 
      where
      performCall = do
        evaluate sut
        assertFailure msg
  
t = TestCase $ expectError "tail [] must throw an error" (tail [])