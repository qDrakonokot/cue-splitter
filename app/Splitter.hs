{-# LANGUAGE OverloadedStrings #-} -- Штука для автоматического преобразования String в Text без головняка

-- Этот модуль реализует основную логику создания структур с полезной и удобной информацией для простой нарезки через ffmpeg
module Splitter where


import TimesMan ( formatCueTime, toCueTime, cueTimeDurCalc, getFileTime )
import Text.Megaparsec ( ParseErrorBundle, errorBundlePretty ) 
import qualified Data.Text as T
import qualified Data.ByteString as B
import Text.CueSheet
    ( CueText,
      CueTime,
      CueSheet(CueSheet, cueFiles, cuePerformer, cueTitle),
      Eec,
      parseCueSheet,
      unCueText,
      CueFile(cueFileName, cueFileTracks),
      CueTrack(CueTrack, cueTrackIndices, cueTrackTitle) )
import Data.List.NonEmpty ( NonEmpty((:|)) )
import System.Directory ( removeFile ) 


-- Структурка с полями для будущей нарезки
data CutInfo = CutInfo { 
    trackName   :: String, 
    startTime   :: String, 
    duration    :: String, 
    artist      :: String,
    albumTitle  :: String,
    audioFile   :: FilePath
}


-- Основая функция модуля, она получает имя файла для нарезки (на самом деле имя нарезаемого файла, фактически нарезаем всегда конвертированный файл)
-- Возвращаем список инфо-структур в IO обертке
parseSheet :: FilePath -> IO [CutInfo]
parseSheet fileName = do
    byteString <- B.readFile "convertedFile.cue"
    case parseCueSheet fileName byteString of 
        Left err -> do
            writeFile "parseErrorLog.log" "ParseLog:\n"
            writeParseCueErr err
            removeFile "convertedFile.cue"
            error "Parse Error, Check file! WARNING: splitter not support .cue file with several dependings files."
        Right sheet -> do
                let 
                    (file :| _) = cueFiles sheet
                    fName = cueFileName file
                time <- getFileTime fName
                return $ getIntervals sheet time


-- Получает структуру файла-разметки и формирует список готовых файлов для нарезки
getIntervals :: CueSheet -> String -> [CutInfo]
getIntervals (CueSheet {
    cuePerformer = cueArtist,
    cueTitle = cueAlbumTitle,
    cueFiles = (file :| _)
}) = fixDurations forFix
    where 
        tracks = cueFileTracks file
        audio = cueFileName file
        forFix = foldr (\(CueTrack {cueTrackTitle = name, cueTrackIndices = (start :| _)}) acc -> createCutInfo (name, cueArtist, cueAlbumTitle, start, audio) : acc) [] tracks


-- Выправляет время на правильное
fixDurations :: [CutInfo] -> String -> [CutInfo]
fixDurations [] _ = []
fixDurations [x] time = [ x { duration = formatCueTime $ cueTimeDurCalc (toCueTime time) (toCueTime $ startTime x) } ]
fixDurations (x:y:xs) time = x { duration = formatCueTime $ cueTimeDurCalc (toCueTime $ startTime y) (toCueTime $ startTime x) } : fixDurations (y : xs) time


-- Создание структурного файла нарезки
createCutInfo :: (Maybe CueText, Maybe CueText, Maybe CueText, CueTime, FilePath) -> CutInfo
createCutInfo (name, art, albumT, start, audio) = 
    CutInfo {
                trackName = nameM,
                startTime = formatCueTime start,
                duration = "00:30:00",
                artist = artM,
                albumTitle = albumTM,
                audioFile = audio 
            }
                where 
                    nameM = case name of 
                        Just str -> T.unpack $ unCueText str
                        Nothing -> "undefined"
                    artM = case art of 
                        Just str -> T.unpack $ unCueText str
                        Nothing -> "undefined"
                    albumTM = case albumT of 
                        Just str -> T.unpack $ unCueText str
                        Nothing -> "undefined"


-- Красивый вывод ошибок в файл 
writeParseCueErr :: ParseErrorBundle B.ByteString Eec -> IO ()
writeParseCueErr err = appendFile "parseErrorLog.log" (errorBundlePretty err)