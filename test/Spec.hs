import           Data.Function            (on)
import           Data.Monoid
import qualified Data.Set                 as S
import           Lib                      (SetI (..), intersection, runSetI,
                                           union)
import           Test.Hspec
import           Test.QuickCheck          (Arbitrary, arbitrary, frequency,
                                           property)
import           Test.QuickCheck          (Arbitrary, arbitrary, frequency)
import           Test.QuickCheck.Checkers (EqProp, eq, isCommut, quickBatch,
                                           (=-=))
import           Test.QuickCheck.Classes  (applicative, functor, monoid)

instance (Arbitrary a, Ord a) => Arbitrary (SetI a) where
  arbitrary = Prim <$> arbitrary

instance Ord a => EqProp (SetI a) where
  (=-=) = eq `on` runSetI

main :: IO ()
main = hspec $ do
  describe "Set Implementation" $ do
    it "should satisfy functor laws" $
      quickBatch $ functor (undefined :: SetI (Integer, String, [Integer]))
    it "should satisfy monoid laws" $
      quickBatch $ monoid (undefined :: SetI (Integer, Integer, Integer))
    it "should be a commutative structure for union" $
      property (isCommut (union :: SetI Integer -> SetI Integer -> SetI Integer))
    it "should be a commutative structure for intersection" $
      property (isCommut (intersection :: SetI Integer -> SetI Integer -> SetI Integer))
    -- TODO: test Applicative and Monad laws
