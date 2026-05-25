
-- Этот модуль реализует все манипуляции со временем, которые являются основными в нарезке файлов
module TimesMan where

import Text.CueSheet ( CueTime(..), toMmSsFf ) -- Стоит пояснить, что время в CueTime хранится в общем количестве фреймов
                                               -- В одной секунде 75 фреймов
import Data.List.Split ( splitOn )
import Text.Read ( readMaybe ) 
import System.Process ( readProcess ) 
import Control.Exception (try, SomeException)
import System.Directory ( removeFile ) 

-- Форматирует кол-во фреймов в стандартный человеческий временной формат чч:мм:сс. 
-- Есть небольшая погрешность, но она не больше 1 секунды, что не значительно
formatCueTime :: CueTime -> String
formatCueTime ct = show (m `div` 60) ++ ":" ++ show (m `mod` 60) ++ ":" ++ show (s + f `div` 75) 
    where
        (m, s, f) = toMmSsFf ct


-- Эта функция делает противоположное formatCueTime
toCueTime :: String -> CueTime
toCueTime str = 
    case splitOn ":" str of
        (h:m:s:_) -> CueTime $ (3600 * read h + 60 * read m + read s) * 75
        _ -> error "Try again, time error, and check file duration"


-- Функция для вычисления длины треков: длина текущего = (длина следующего - длина текущего)
cueTimeDurCalc :: CueTime -> CueTime -> CueTime
cueTimeDurCalc (CueTime time1) (CueTime time2) = CueTime (time1 - time2) 


-- Вспомогательная функция для получение длины всего файла, чтобы вычислить длину последнего трека
-- Получаем через производную от ffmpeg
getDuration :: FilePath -> IO (Maybe Double)
getDuration filePath = do
    let cmd = "ffprobe"
        args = [ 
                    "-v", "error", 
                    "-show_entries", "format=duration", 
                    "-of", "default=noprint_wrappers=1:nokey=1", 
                    filePath
               ]
    result <- try (readProcess cmd args "") :: IO (Either SomeException String)
    case result of
        Right output -> return (readMaybe (takeWhile (/= '\n') output))
        Left _ -> return Nothing


-- Обертка над верхней функцией с возвратом форматированного времени
getFileTime :: FilePath -> IO String
getFileTime file = do 
    doubleTime <- getDuration file
    case doubleTime of
        Nothing -> do
            putStrLn "Failed to read audio-file, please make sure that file in the same dir as .cue file"
            removeFile "convertedFile.cue"
            error "File duration not valid or file not in same directory"
        Just time -> do
            let intTime = round time :: Int
                hours   = intTime `div` 3600
                minutes = (intTime `mod` 3600) `div` 60
                seconds = intTime `mod` 60
                format n = if n < 10 then '0' : show n else show n
            return $ format hours ++ ":" ++ format minutes ++ ":" ++ format seconds