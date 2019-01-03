module Data.TypedWire.Prelude
    ( module Prelude
    , module Control.Alt
    , module Control.Monad
    , module Data.Either
    , module Data.Generic.Rep
    , module Data.Argonaut
    , module Data.Functor
    , module Data.Maybe
    , module Data.Array
    , decodeJsonMaybe, (.??), eatBool
    , DateTime(..), Day(..), TimeOfDay(..)
    , AsBase64(..)
    )
where

import Control.Alt
import Control.Monad
import Data.Argonaut (class DecodeJson, class EncodeJson, Json, decodeJson, encodeJson, fail)
import Data.Array
import Data.DateTime as PSD
import Data.Either (Either(..), either, fromRight)
import Data.Functor (class Functor)
import Data.Maybe (Maybe(..))
import Data.Time (Hour, Minute, Second)
import Data.Date as D
import Data.Enum (toEnum, fromEnum)
import Data.Generic.Rep (class Generic)
import Data.Int as I
import Data.JSDate as JD
import Data.Nullable (Nullable)
import Data.Nullable as Nullable
import Data.String.Regex as R
import Foreign.Object as M
import Partial.Unsafe (unsafePartial)
import Prelude

foreign import dateToISO :: JD.JSDate -> String
foreign import dateFromISO :: String -> Nullable JD.JSDate

newtype AsBase64 = AsBase64 String
derive instance eqAsBase64 :: Eq AsBase64
derive newtype instance showAsBase64 :: Show AsBase64
derive instance genericAsBase64 :: Generic AsBase64 _

unAsBase64 :: AsBase64 -> String
unAsBase64 (AsBase64 d) = d

instance decodeAsBase64 :: DecodeJson AsBase64 where
    decodeJson json = do
        AsBase64 <$> decodeJson json

instance encodeAsBase64 :: EncodeJson AsBase64 where
    encodeJson = encodeJson <<< unAsBase64

newtype DateTime = DateTime PSD.DateTime

instance showDateTime :: Show DateTime where
    show (DateTime d) = show d

instance eqDateTime :: Eq DateTime where
    eq (DateTime a) (DateTime b) = eq a b

unDateTime :: DateTime -> PSD.DateTime
unDateTime (DateTime d) = d

instance decodeDateTime :: DecodeJson DateTime where
    decodeJson json = do
        str <- decodeJson json
        case JD.toDateTime =<< Nullable.toMaybe (dateFromISO str) of
          Nothing -> fail $ "Invalid datetime: " <> str
          Just d -> pure (DateTime d)

instance encodeDateTime :: EncodeJson DateTime where
  encodeJson = encodeJson <<< dateToISO <<< JD.fromDateTime <<< unDateTime

newtype Day = Day D.Date

instance showDay :: Show Day where
    show (Day d) = show d

instance eqDay :: Eq Day where
    eq (Day a) (Day b) = eq a b

dayRegex :: R.Regex
dayRegex = unsafePartial $ fromRight $ R.regex "([0-9]{4})-([0-9]{1,2})-([0-9]{1,2})" mempty

dayFromString :: String -> Maybe Day
dayFromString str = do
    matches <- R.match dayRegex str
    case (fromFoldable matches) of
        [Just ys, Just ms, Just ds] -> do
            y <- toEnum =<< I.fromString ys
            m <- I.fromString ms >>= monthFromInt
            d <- toEnum =<< I.fromString ds
            pure <<< Day $ D.canonicalDate y m d
        _ -> Nothing

monthFromInt :: Int -> Maybe D.Month
monthFromInt i =
    case i of
        1  -> Just D.January
        2  -> Just D.February
        3  -> Just D.March
        4  -> Just D.April
        5  -> Just D.May
        6  -> Just D.June
        7  -> Just D.July
        8  -> Just D.August
        9  -> Just D.September
        10 -> Just D.October
        11 -> Just D.November
        12 -> Just D.December
        _ -> Nothing

monthToInt :: D.Month -> Int
monthToInt m =
    case m of
        D.January -> 1
        D.February -> 2
        D.March -> 3
        D.April -> 4
        D.May -> 5
        D.June -> 6
        D.July -> 7
        D.August -> 8
        D.September -> 9
        D.October -> 10
        D.November -> 11
        D.December -> 12

dayToString :: Day -> String
dayToString (Day dt) =
    show y <> "-" <> show m <> show d
    where
        y = fromEnum $ D.year dt
        m = monthToInt $ D.month dt
        d = fromEnum $ D.day dt

instance decodeDay :: DecodeJson Day where
    decodeJson json = do
        str <- decodeJson json
        case dayFromString str of
            Nothing -> fail "Invalid day"
            Just d -> pure d

instance encodeDay :: EncodeJson Day where
    encodeJson = encodeJson <<< dayToString

newtype TimeOfDay =
    TimeOfDay
    { hour :: Hour
    , minute :: Minute
    , second :: Second
    }

instance eqTimeOfDay :: Eq TimeOfDay where
    eq (TimeOfDay a) (TimeOfDay b) =
        a.hour == b.hour && a.minute == b.minute && a.second == b.second

instance showTimeOfDay :: Show TimeOfDay where
    show a = "TimeOfDay (" <> timeOfDayToString a <> ")"

timeOfDayRegex :: R.Regex
timeOfDayRegex = unsafePartial $ fromRight $ R.regex "([0-9]{1,2}):([0-9]{1,2}):([0-9]{1,2})" mempty

timeOfDayFromString :: String -> Maybe TimeOfDay
timeOfDayFromString str = do
    matches <- R.match timeOfDayRegex str
    case (fromFoldable matches) of
        [Just hs, Just ms, Just ss] -> do
            hour   <- toEnum =<< I.fromString hs
            minute <- toEnum =<< I.fromString ms
            second <- toEnum =<< I.fromString ss
            pure $ TimeOfDay { hour, minute, second }
        _ -> Nothing

timeOfDayToString :: TimeOfDay -> String
timeOfDayToString (TimeOfDay { hour, minute, second }) =
    show (fromEnum hour) <> ":" <> show (fromEnum minute) <> ":" <> show (fromEnum second)

instance decodeTimeOfDay :: DecodeJson TimeOfDay where
    decodeJson json = do
        str <- decodeJson json
        case timeOfDayFromString str of
            Nothing -> fail "Inavlid time of day"
            Just tod -> pure tod

instance encodeTimeOfDay :: EncodeJson TimeOfDay where
    encodeJson = encodeJson <<< timeOfDayToString

decodeJsonMaybe :: forall a. (DecodeJson a) => M.Object Json -> String -> Either String (Maybe a)
decodeJsonMaybe o s =
    case M.lookup s o of
        Nothing -> pure Nothing
        Just val -> Just <$> decodeJson val
infixl 5 decodeJsonMaybe as .??

eatBool :: Boolean -> Either String Unit
eatBool _ = pure unit
