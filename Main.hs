{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import System.IO
import Pipes as P
import Pipes.HTTP
import Pipes.Parse
import Pipes.ByteString as PB
import Pipes.Binary
import Data.Binary.Get


downloadRedditExample :: IO ()
downloadRedditExample = do
    req <- parseUrl "http://www.reddit.com/r/haskell.json"
    withManager defaultManagerSettings $ \m ->
        withHTTP req m $ \resp ->
            runEffect $ responseBody resp >-> PB.stdout


binHandle :: IO Handle
binHandle = openFile "data/fakecast.bin" ReadMode


fakeCastPipe :: Producer ByteString IO ()
fakeCastPipe = lift binHandle >>= PB.fromHandle


data IcyData = IcyData { audio :: ByteString , icyTag :: T.Text }
    deriving (Show, Eq)


getIcyData :: Int -> Get IcyData
getIcyData n = do
    audiodata <- getByteString n
    icybyte <- getWord8
    let icylen = fromEnum $ icybyte * 16
    icyBS <- getByteString icylen
    let icyText = T.decodeUtf8 icyBS
    return IcyData { audio = audiodata, icyTag = icyText }


suckPipe :: Monad m => (a -> m (Either t b, a)) -> a -> m ([b], a)
suckPipe f = go []
    where
        go z p = do
            (r, p') <- f p
            case r of
                Left _ -> return (reverse z, p')
                Right v -> go (v:z) p'


main :: IO ()
main = do
    let decoder = runStateT $ decodeGet (getIcyData 22)
    (values, unused) <- suckPipe decoder fakeCastPipe
    print values
    runEffect $ unused >-> PB.stdout
