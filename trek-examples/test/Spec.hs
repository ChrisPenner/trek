{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
import Trek
import Trek.Lens
import Data.Aeson hiding ((.=))
import Data.Aeson.Lens
import Control.Lens
import Test.Hspec
import Data.Maybe
import GHC.Generics
import Control.Monad
import Data.Foldable
import Data.Map as M
import Data.HashMap.Lazy as HM
import qualified Data.Text as T

data TestCase = TestCase {input :: Value, output :: Value}
  deriving (Generic, FromJSON)

main :: IO ()
main = hspec . describe "examples" $ do
    evalTest1 "test/examples/test1.json" test1
    evalTest1 "test/examples/test2.json" test2
    evalTest1 "test/examples/test3.json" test3
    evalTest1 "test/examples/test5.json" test5
    execTest "test/examples/test6.json" test6
    evalTest1 "test/examples/test7.json" test7
    evalTest1 "test/examples/test8.json" test8
    execTest "test/examples/test9.json" test9
    -- execTest "test/examples/test10.json" test10

execTest :: FilePath -> Trek Value () -> Spec
execTest path m = it path $ do
    TestCase {input, output} <- decoder path
    let r = execTrek m input
    r `shouldBe` output

evalTest1 :: FilePath -> Trek Value Value -> Spec
evalTest1 path m = it path $ do
    TestCase {input, output} <- decoder path
    let r = evalTrek1 m input
    r `shouldBe` output

evalTest :: FilePath -> Trek Value Value -> Spec
evalTest path m = it path $ do
    TestCase {input, output} <- decoder path
    let r = evalTrek m input
    toJSON r `shouldBe` output



decoder :: FilePath -> IO TestCase
decoder p = eitherDecodeFileStrict' p >>= \case
    Left e -> (fail e)
    Right x -> return x

test1 :: Trek Value Value
test1 = get

test2 :: Trek Value Value
test2 = do
    (m :: Map String String) <- selecting _JSON
    pure . toJSON $ M.fromList (swap <$> M.toList m)
  where
    swap (a, b) = (b, a)

test3 :: Trek Value Value
test3 = do
    k <- selecting (key "key" . _String)
    m <- selecting _Object
    pure . toJSON . M.singleton k $ sans "key" m

test5 :: Trek Value Value
test5 = do
    selecting (key "age")

test6 :: Trek Value ()
test6 = do
    _Object . at "name" .= Nothing

test7 :: Trek Value Value
test7 = do
    selecting $ key "hostinfo" . key "host"

test8 :: Trek Value Value
test8 = do
    selecting (values . key "name")

test9 :: Trek Value ()
test9 = do
    focusing values $ do
        hostInfo <- selecting (key "hostinfo" . _Object)
        _Object %= sans "hostinfo"
        _Object <>= hostInfo

-- test10 :: Trek Value ()
-- test10 = do
--     hostInfo <- selecting (key "hostinfo" . _Object)
--     _Object %= sans "hostinfo"
--     _Object <>= hostInfo

