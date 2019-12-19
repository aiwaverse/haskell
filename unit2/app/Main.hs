module Main where

import Capstone
import System.Clock

main :: IO ()
main = do
   putStrLn "Please enter the message:"
   message <- getLine
   time <- getTime Realtime
   let seconds = sec time
   let pad = randomOTP . fromIntegral $ seconds
   let writePad = take (length message) (show pad)
   putStrLn ("Your pad is " ++ writePad ++ ", write it down")
   let encoded = encode (OTP writePad) message
   putStrLn ("The coded message is " ++ encoded)
   let decoded = decode (OTP writePad) encoded
   putStrLn ("The decoded message is " ++ decoded)
-- this generated some pads/coded messages that can't be copied, therefore can't be pasted therefore can't be used outside this main
-- idk how to solve, and idk if i want to