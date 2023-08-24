module Clock (addDelta, fromHourMin, toString) where


data Clock = Clock Int Int
  deriving Eq


fromHourMin :: Int -> Int -> Clock
fromHourMin hs ms = Clock h' m'
  where
    time = hs * 60 + ms
    dh = (time `div` 60) `mod` 24
    dm = time `mod` 60
    h' = if dh < 0 then 24 - dh else dh
    m' = if dm < 0 then 60 - dm else dm

toString :: Clock -> String
toString (Clock h m) = pad h <> ":" <> pad m
  where
    pad x = if x < 10 then "0" <> show x else show x

addDelta :: Int -> Int -> Clock -> Clock
addDelta h m (Clock h' m') = fromHourMin (h+h') (m+m')

