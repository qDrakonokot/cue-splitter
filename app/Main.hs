module Main where

import Splitter ( CutInfo(CutInfo, albumTitle), parseSheet )
import System.Process ( callProcess ) 
import System.Environment ( getArgs )
--import Control.Monad ( forM_ )
import Control.Concurrent.Async (mapConcurrently_)
import System.Directory ( createDirectory, removeFile )
import System.FilePath ( (</>) )


main :: IO ()
main = do
    args <- getArgs
    case args of
        [fileName] -> do
            -- Перекодируем файл в UTF-8
            callProcess "sh" ["-c", "iconv -f WINDOWS-1251 -t UTF-8 '" ++ fileName ++ "' > convertedFile.cue"]
            parsedFiles <- parseSheet fileName
            removeFile "convertedFile.cue"
            let parsedDir = albumTitle $ head parsedFiles
                parsedFilesWithOrderList = zip [1 .. length parsedFiles] parsedFiles       
            -- Создаем директорию для разбитых файлов    
            createDirectory parsedDir
            -- Применяем к каждой сруктуре разметки вызов ffmpeg для вырезки из главного файла
            --forM_ parsedFilesWithOrderList runFfmpeg
            mapConcurrently_ runFfmpeg parsedFilesWithOrderList
            -- Рисуем красоту))
            drawPerfectCompletePic
        _ -> putStrLn "Input .cue file for split and make shure that audio file in the same directory"


-- Запускаем ffmpeg с правильными параметрами
runFfmpeg :: (Int, CutInfo) -> IO ()
runFfmpeg (num, CutInfo name start dur art albT audF) = do
    callProcess "ffmpeg" [
                            "-loglevel", "quiet",
                            "-i", audF,
                            "-ss", start,
                            "-t", dur,
                            "-acodec", "flac",
                            "-metadata", "title=" ++ name,
                            "-metadata", "artist=" ++ art,
                            "-metadata", "album=" ++ albT,
                            "-metadata", "track=" ++ show num,
                            albT </> name ++ ".flac"
                        ]


-- Делам красивый вывод)))
drawPerfectCompletePic :: IO ()
drawPerfectCompletePic = do
    putStrLn "                                                                                             "
    putStrLn "                                   @@:                              @@@       @  @       +   "
    putStrLn " %%%%%  #####                     @  @                              @         @  @       @   "
    putStrLn "   %%%%% *####                   :%             -    -    -     -   @         @  @       @   "
    putStrLn "    #%%%%  ##### .**********      @     @  :@ :@:@  @-@  @ @@ @% @ =@= @   @  @  @ @   @ @   "
    putStrLn "      %%%%% .####+ *********      *@@   @  :@ @    @    @   @ @     @  @   @  @  @ @  @  @   "
    putStrLn "     -%%%%+ #######  *******        @@  @  :@ @    @    @@@@@  @@   @  @   @  @  @ *= @  @   "
    putStrLn "    %%%%% .####+##### +*****         @ .@  -@ @    @    @       =@  @  @   @  @  @  @ @  @   "
    putStrLn "  %%%%%  #####   *####               @  @  @@ @    @    @:       @  @  @   @  @ .@  @%       "
    putStrLn ":%%%%# *####       #####         *@@@@  @@@:@  @@@  @@@  @@@@ @@@@  @  -@@@@  @ .@  =@   @   "
    putStrLn "                                                                                     @       "
    putStrLn "                                                                                    @        "
    putStrLn "                                                                                   @@        "
    putStrLn "                                                                                             "








