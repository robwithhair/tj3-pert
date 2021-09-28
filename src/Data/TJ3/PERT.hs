{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
module Data.TJ3.PERT
    ( stdInToStdOut
    ) where
import qualified Data.Text.Lazy.IO as LT
import qualified Data.Text.Lazy as LT
import qualified Data.List as L
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Data.Maybe (isNothing)

stdInToStdOut :: IO ()
stdInToStdOut = LT.interact (LT.unlines . map processLine . LT.lines)

isSemicolon = (== ';')

statements :: LT.Text -> [LT.Text]
statements = L.filter (not . LT.null) . LT.split isSemicolon

unstatements :: [LT.Text] -> LT.Text
unstatements = LT.intercalate (LT.singleton ';')

processLine :: LT.Text -> LT.Text
processLine = unstatements . map (processPerts "") . statements

processPerts last "" = last
processPerts last rest | LT.null postPert = last `LT.append` prePert
                       | ignoring = processPerts startIfIgnoring restOut
                       | otherwise = processPerts startIfNotIgnoring restIfNotIgnoring
                       where (prePert, postPert) = LT.breakOn "PERT " rest
                             restOut = LT.drop 5 postPert
                             restIfNotIgnoring = LT.unwords $ drop 4 restWords
                             startIfIgnoring = startIfNotIgnoring `LT.append` "PERT "
                             startIfNotIgnoring = last `LT.append` (LT.fromStrict scenarioRemainder) `LT.append` pertEstimateTextOrNothing
                             pertEstimateTextOrNothing = case maybePert of
                               Just pert -> LT.fromStrict $ pertEstimateText pert
                               Nothing -> ""
                             inQuote = ((LT.count "\"" startIfNotIgnoring) `mod` 2) /= 0
                             ignoring = inQuote || dontHave3Words || isNothing maybePert
                             restWords = LT.words postPert
                             next3Words = Vector.fromList $ take 3 $ drop 1 restWords
                             dontHave3Words = length next3Words /= 3
                             scenario = stringEndToMaybeScenario $ LT.toStrict prePert
                             scenarioRemainder = Text.take (Text.length s - Text.length st) s
                               where st = scenarioText scenario
                                     s = LT.toStrict prePert
                             unpackDuration i = LT.unpack <$> next3Words Vector.!? i >>= duration
                             maybeMinEst = unpackDuration 0
                             maybeProbableEst = unpackDuration 1
                             maybeMaxEst = unpackDuration 2
                             maybePert =
                               maybePERTEstimate $
                               MaybePERTEstimate{ maybeScenario=scenario
                                                , maybeMin=maybeMinEst
                                                , maybeProbable=maybeProbableEst
                                                , maybeMax=maybeMaxEst}

maybePERTEstimate :: MaybePERTEstimate -> Maybe PERTEstimate
maybePERTEstimate MaybePERTEstimate{ maybeScenario=ms
                                    , maybeMin=(Just mn)
                                    , maybeProbable=(Just pb)
                                    , maybeMax=(Just mx)
                                    } =
  Just $ PERTEstimate ms $ ThreePointEstimate{ tpeMin=mn
                                             , tpeProbable=pb
                                             , tpeMax=mx
                                             }
maybePERTEstimate _ = Nothing


data MaybePERTEstimate = MaybePERTEstimate { maybeScenario :: Maybe Text
                                           , maybeMin :: Maybe Duration
                                           , maybeProbable :: Maybe Duration
                                           , maybeMax :: Maybe Duration
                                           }

data PERTEstimate = PERTEstimate (Maybe Scenario) ThreePointEstimate

data ThreePointEstimate = ThreePointEstimate { tpeMin :: Estimate
                                             , tpeProbable :: Estimate
                                             , tpeMax :: Estimate
                                             }

type Estimate = Duration
type Scenario = Text

data Duration = Minutes Double
              | Hours Double
              | Days Double
              | Weeks Double
              | Months Double
              | Years Double
              deriving (Show)

scenarioText :: Maybe Scenario -> Text
scenarioText Nothing = ""
scenarioText (Just s) = s `Text.snoc` ':'

pertEstimateText :: PERTEstimate -> Text
pertEstimateText (PERTEstimate maybeScenarioText tpe) = Text.intercalate " " [effort, mint, probablet, maxt, criticalt, sdt, vart] `a` " "
  where a = (Text.append)
        addScenario t = (scenarioText maybeScenarioText) `a` t
        fromTpe textRep extractFunction = addScenario $ "PERT" `a` textRep `Text.snoc` ' ' `a` ((Text.pack . show) (((round . seconds . extractFunction) tpe) :: Integer))
        mint = fromTpe "Min" tpeMin
        maxt = fromTpe "Max" tpeMax
        probablet = fromTpe "Probable" tpeProbable
        effort = addScenario $ "effort " `a` criticalPatht
        criticalt = addScenario $ "PERTCriticalPath " `a` (Text.pack $ show (round criticalPathSeconds :: Integer))
        fromShow textLabel n = addScenario $ "PERT" `a` textLabel `Text.snoc` ' ' `a` (Text.pack $ show (round n :: Integer))
        criticalPatht = Text.pack . toString $ fromSeconds criticalPathSeconds
        sdt = fromShow "StandardDeviation" sd
        vart = fromShow "Variance" var
        criticalPathSeconds = ((o + (4*m) + p) / 6)
        fromTpeSeconds extractFunction = seconds $ extractFunction tpe
        o = fromTpeSeconds tpeMin
        p = fromTpeSeconds tpeMax
        m = fromTpeSeconds tpeProbable
        sd = (p - o) / 6
        var = sd ^ 2

-- x : number you want rounded, n : number of decimal places you want...
round' :: Int -> Double -> Double
round' n x = (fromIntegral (round (x * t))) / t
    where t = 10^n

duration :: String -> Maybe Duration
duration s | "min" `L.isSuffixOf` s = Just $ Minutes $ read $ take (length s - 3) s
duration s | otherwise = case last s of
                           'h' -> Just $ Hours doubleValue
                           'd' -> Just $ Days doubleValue
                           'w' -> Just $ Weeks doubleValue
                           'm' -> Just $ Months doubleValue
                           'y' -> Just $ Years doubleValue
                           _ -> Nothing
                         where doubleValue = read $ take (length s - 1) s

seconds :: Duration -> Double
seconds (Minutes m) = m * 60
seconds (Hours h) = h * (seconds $ Minutes 60)
seconds (Days d) = d * (seconds $ Hours 24)
seconds (Weeks w) = w * (seconds $ Days 7)
seconds (Years y) = y * (seconds $ Days 365.25)
seconds (Months m) = m * (seconds $ Years (1/12))

fromSeconds :: Double -> Duration
fromSeconds s | s < 3600 = Minutes (s / 60)
fromSeconds s | s < 86400 = Hours (s / 3600)
fromSeconds s | s < 604800 = Days (s / 86400)
fromSeconds s | s < (86400 * 365.25 / 12) = Weeks (s / 604800)
fromSeconds s | s < (86400 * 365.25) = Months (s / (86400 * 365.25 / 12))
fromSeconds s | otherwise = Years (s / (86400 * 365.25))

showMaybeRound (Just n) x = show (round' n x)
showMaybeRound Nothing x = show x

showRound3 = showMaybeRound (Just 3)

showRound3t x t = showRound3 x ++ t

toString :: Duration -> String
toString (Minutes m) = showRound3t m "min"
toString (Hours h) = showRound3t h "h"
toString (Days d) = showRound3t d "d"
toString (Weeks w) = showRound3t w "w"
toString (Months m) = showRound3t m "m"
toString (Years y) = showRound3t y "y"

stringEndToMaybeScenario :: Text -> Maybe Scenario
stringEndToMaybeScenario t = case Text.last t of
  ':' -> Just . Text.dropEnd 1 . Vector.last . Vector.fromList $ Text.words t
  _ -> Nothing
