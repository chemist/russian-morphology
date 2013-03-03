russian-morphology
==================

russian-morphology

Simple morphology for Russian language.

Just make normal form from word.

Install
-------
    > cabal configure
    > cabal build
    > cabal install

Usage:
-----
    >>> import Text.Morphology
    >>> import Data.Text (pack)
    >>> import Data.ByteString.Char8 (putStrLn)
    >>> import Data.Text.Encoding (encodeUtf8)
    >>>
    >>> normal <- normalForm  -- |  load binary file, and return IO Morph
    >>> let check = normal (pack "есть")
    >>> check
    ["\1073\1099\1090\1100","\1077\1089\1090\1100","\1077\1089\1090\1100"]
    >>> mapM_ putStrLn $ map encodeUtf8 check
    быть
    есть
    есть
    >>> let check = normal (pack "ржи")
    >>> mapM_ putStrLn $ map encodeUtf8 check
    рожь
    ржа
    ржать
    >>> -- | if normal form not found, will return empty list.
