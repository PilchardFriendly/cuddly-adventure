import Test.Hspec

import qualified Shokinin20.Spec (spec)

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "Shokinin20"     Shokinin20.Spec.spec