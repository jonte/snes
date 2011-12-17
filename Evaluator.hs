module Evaluator where
import qualified Data.ByteString as B
import Data.Binary.Get
import Data.Word
import Text.Printf
import Data.Int
import Debug.Trace
import Numeric

type Instruction = Word8
-- State to keep current CPU state, flags etc
data State       = State {pc :: PC, powerOn :: Bool, rom :: ROM, 
                          stack :: [Int], flags :: Flags, ram :: RAM}
    deriving (Show)
-- Program counter
type PC          = Int

type ROM      = [Instruction]
type RAM      = [Instruction]

data Flags       = Flags{ n::Bool, v::Bool, m::Bool, 
                          x::Bool, d::Bool, i::Bool, 
                          z::Bool, c::Bool}
    deriving (Show)

eval :: State -> Instruction -> State
-- SEI
eval s 0x78 = s{flags = (flags s){i = True}}

-- RTI
eval s 0x40 = s

-- BRK
eval s 0x00 = s

-- STZ absolute
eval s 0x9C = do
                let (s', param) = fetch s
                writeRAM s' param (0::Instruction)
-- WDM
eval s 0x42 = s
eval s other = error $ "unknown instruction " ++ (printf "%x" other) ++ "\n" ++
                            "State is " ++ (show (s{rom = take 10 (rom s),
                                                    ram = take 100 (ram s)}))


fetch :: State -> (State, Instruction)
fetch st = (incPC st, (rom st) !! (pc st))

writeRAM :: State -> Instruction -> Instruction -> State
writeRAM s loc ct = s{ram = writeRAM' (ram s) loc ct} 

writeRAM' :: [Instruction] -> Instruction -> Instruction ->[Instruction]
writeRAM' ram loc ct  = do
                            let (left, right) = splitAt ((fromIntegral loc)::Int) ram
                            left++[ct]++(tail right)

incPC :: State -> State
incPC st = st{pc = (pc st)+1}

fetchExecuteLoop :: State -> State
fetchExecuteLoop st     | (powerOn st)  = fetchExecuteLoop nextState
                        | otherwise     = nextState
    where nextState     = eval nextState' instruction 
            where (nextState', instruction) = fetch st

main = do
   input <- B.readFile "rom.smc"
   let unpacked  = B.unpack input
   let ram          = replicate (128*1024) (0 :: Word8)
   let powerOnFlags = Flags False False False False False False False False
   let powerOnState = State {pc=0, powerOn=True, rom = unpacked, 
                            stack = [], flags = powerOnFlags, ram = ram}
   print $ fetchExecuteLoop powerOnState

