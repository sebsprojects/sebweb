module Sebweb.View (
    makeRelativeLink
  , makeRevisionedLink
) where


makeRelativeLink :: T.Text -> T.Text -> T.Text
makeRelativeLink pth ln = pref <> "/" <> ln
  where pref = T.intercalate "/" (replicate n "..")
        n = max 0 ((length $ T.splitOn "/" pth ) - 1)

makeRevisionedLink :: T.Text -> T.Text -> T.Text
makeRevisionedLink rev ln = ln <> "?rev=" <> rev
