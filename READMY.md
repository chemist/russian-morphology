    Usage:
    > makeMorph
    create binary fail with morphology base
    > normal <- normalForm
    load binary fail, and return IO Morph
    > let check = normal (pack "есть")
    > check
    ["\1073\1099\1090\1100","\1077\1089\1090\1100","\1077\1089\1090\1100"]
    import modules for show cyrillic in console
    > import Data.ByteString (putStrLn)
    > imported from Data.Text.Encoding (encodeUtf8)
    > mapM_ putStrLn $ map encodeUtf8 check
    быть
    есть
    есть
    > let check = normal (pack "ржи")
    > mapM_ putStrLn $ map encodeUtf8 check
    рожь
    ржа
    ржать
    if normal form not found, will return empty list.
