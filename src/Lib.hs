{-# LANGUAGE GADTs              #-}
{-# LANGUAGE DeriveFunctor      #-}
{-# LANGUAGE LambdaCase         #-}

module Lib
    ( someFunc
    )
where

import           Control.Monad.Free
import           Control.Monad
import           Data.List

data DSL a where
    Par         :: String -> a -> DSL a
    List        :: Orientation -> String -> [String] -> a -> DSL a
  deriving (Functor)

data Orientation = Horizontal | Vertical

par :: String -> Free DSL ()
par x = liftF (Par x ())

list :: Orientation -> String -> [String] -> Free DSL ()
list x y z = liftF (List x y z ())

evalDSL :: Free DSL a -> IO ()
evalDSL = \case
    Free (Par aString aNext) -> putStrLn (aString ++ "\n") >> evalDSL aNext

    Free (List Horizontal aTitle anItem aNext) -> do
        let aList =
                concat $ aTitle : ": " : intersperse ", " anItem ++ [".\n"]
        putStrLn aList
        evalDSL aNext

    Free (List Vertical aTitle anItem aNext) -> do
        putStrLn $ aTitle ++ ":"
        forM_ anItem $ \anIndent -> putStrLn $ "  - " ++ anIndent ++ ";"
        putStrLn ""
        evalDSL aNext

    Pure _ -> return ()

contacts :: Free DSL ()
contacts = list
    Vertical
    "Контакты"
    [ "телефон/telegram/viber: +7 (978) 122-72-16"
    , "github: https://github.com/vojiranto"
    , "mail: dmitrij.pavluk.hs@gmail.com"
    ]

skills :: Free DSL ()
skills =
    list Horizontal "Навыки" ["Haskell", "C++", "Ruby", "git", "bash", "Linux"]

workAt :: String -> String -> Int -> Maybe Int -> String
workAt aCompany aPosition aFrom aMaybeTo =
    aCompany ++ ": " ++ aPosition ++ " с " ++ show aFrom ++ " по " ++ aTo ++ ""
  where
    aTo = case aMaybeTo of
        Just x  -> show x
        Nothing -> "настоящее время"

experience :: Free DSL ()
experience = list
    Vertical
    "Опыт работы"
    [ workAt "Enecuum" "Haskell-программист"     2018 (Just 2019)
    , workAt "DemLabs" "Программист-разработчик" 2017 (Just 2018)
    ]

education :: Free DSL ()
education =
    list Vertical "Образование" ["2017 - бакалавр, МГУ им. Ломоносова, факультет ВМК"]

someFunc :: IO ()
someFunc = evalDSL $ do
    par "Павлюк Дмитрий Александрович"
    contacts
    skills
    experience
    education