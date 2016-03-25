{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall -Werror #-}

import Route
import Util
import World
import qualified Data.List as DL
import qualified Data.Maybe as B
import qualified Data.Text as T
import qualified Test.HUnit as HU
import qualified Test.QuickCheck as Q

default (T.Text)

main :: IO ()
main = do
    rl <- readFile "test/routes.txt"
    let test1 = HU.TestCase $
                    HU.assertEqual
                        "default route IP regex works"
                        (B.fromJust $ defaultRouteIP (T.pack rl))
                        "192.168.209.1"

    il <- readFile "test/ifconfig.txt"
    let ifks = [ name i | i <- parseInterfaceList (T.pack il) ]
    let test2 = HU.TestCase $
                    HU.assertBool "parsing ifconfig works" (
                        DL.all
                            (\x -> x `DL.elem`
                                ["lo0", "em0", "iwm0", "enc0", "vether0", "pflog0", "ppp0", "tun0"])
                            ifks)

    let tests = HU.TestList [ HU.TestLabel "test1" test1
                            , HU.TestLabel "test2" test2
                            ]

    let test4 s = T.pack s == T.pack s

    _ <- HU.runTestTT tests
    _ <- Q.quickCheck test4

    return ()
