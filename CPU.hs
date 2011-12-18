module CPU where
import MMU
import Common
import Control.Monad.State
import qualified Data.ByteString as B
import Text.Printf
import Debug.Trace
import Data.Bits

boot :: MachineState -> [Instruction] -> MachineState
boot s instructions = fetchExecLoop preparedState
    where preparedState = loadBIOS s


fetchExecLoop :: MachineState -> MachineState
fetchExecLoop s | (powered s)   = fetchExecLoop s''
                | otherwise     = s
    where s'' = exec s' instr
            where
                (s', instr) = fetch s

fetch :: MachineState -> (MachineState, Instruction)
fetch s = traceShow ("" ++ (printf "Fetched %x, pc %x" instr (pc s)))  (incPC s, instr)
    where   _pc     = pc (s)
            instr   = readRAM s _pc

fetch2 s = do   let (s',  lo) = fetch s
                let (s'', hi) = fetch s'
                (s'', shift hi 8 + lo)
            



exec :: MachineState -> Instruction -> MachineState
-- LD SP, word
exec s i    | i == 0x31 =  do   let (s', w) = fetch2 s
                                s'{sp = w}
-- XOR A -> A
            | i == 0xAF = do    let _a = (a s)
                                s{a =  xor _a _a}
-- LD HL, word
            | i == 0x21 = do    let (s', w) = fetch2 s
                                s'{hl = w}
-- LDD (Save A to (HL))
            | i == 0x32 = do    let addr = hl s
                                writeRAM s addr (a s)
-- Extended operation
            | i == 0xCB = do    let (s', oper) = fetch s
                                case oper of
                                    0x7C  -> do let _h = (h s')
                                                if (_h .&. 0x7 == 0x1)
                                                    then s'{z = True, n = False}
                                                else s'{z = False}
                                    0x11 -> s'{c = rotateL (c s) 1}

                                    -- RL A
                                    0x17 -> s'{c = rotateL (c s) 1}
                                    -- SET 0, C
                                    0xc1 -> s'{c = 0x1 .|. (c s)}
                                    other -> error $ "Extended instruction "++(printf "%x" oper) ++
                                                    " unknown"
-- JNZ relative
            | i == 0x20 = do    let (s', instr) = fetch s
                                if (z s)
                                    then s{pc = (pc s') + instr}
                                else s'
-- LD 8bit immediate -> C
            | i == 0x0E = do    let (s', instr) = fetch s
                                s'{c = instr}
-- LD 8bit immediate -> A
            | i == 0x3E = do    let (s', instr) = fetch s
                                s'{a = instr}
-- LDH (C), A
            | i == 0xE2 = do    let addr = 0xFF00 + (c s)
                                writeRAM s addr (a s)
-- INC C
            | i == 0x0C = do    s{c = (c s) +1}

-- LD (HL), A
            | i == 0x77 = do    writeRAM s (hl s) (a s)

-- LDH (n), A
            | i == 0xE0 = do    let (s', i') = fetch s
                                writeRAM s' (0xFF00 + i') (a s)
-- LD DE nn
            | i == 0x11 = do    let (s', i') = fetch2 s
                                s'{de = i'}
-- INC B
            | i == 0x04 =       s{b = (b s) +1}
-- INC DE
            | i == 0x13 =       s{de = (de s) +1}
-- INC HL
            | i == 0x23 =       s{hl = (hl s) +1}
-- DEC B
            | i == 0x05 =       s{b = (b s) -1}
-- LD BC nn
            | i == 0x01 = do    let (s', i') = fetch2 s
                                traceShow ("Loading BC with: " ++ (printf "%x" i')) s'{bc = i'}
-- LD A (DE)
            | i == 0x1A = do    s{a = readRAM s (de s)}
-- CALL nn
            |i == 0xCD  = do    let (s', i')    = fetch2 s
                                let s'' = s'{ stack = 0xFF00 .&. (pc s'):0x00FF .&. (pc s'):(stack s'),
                                    pc = i'}
                                traceShow ("Pushed PC, stack is now: " ++ (show (stack s''))) s''

-- LD A C
            | i == 0x4F = do    s{a = c s}
-- LD A E
            | i == 0x7B = do    s{a = e s}
-- LD B n
            | i == 0x06 = do    let (s', i') = fetch s
                                s'{b = i'}
-- PUSH BC
            | i == 0xC5 = do    let s' = s{stack = 0xFF00 .&. (bc s) : 0x00FF .&. (bc s) : stack s}
                                traceShow ("Pushed BC, stack is now: " ++ (show (stack s'))) s'
-- RL A
            | i == 0x17 = do    s{c = rotateL (c s) 1}
-- POP BC, nn -> BC
            | i == 0xC1 = do    let (s',  i')  = pop s
                                let (s'', i'') = pop s'
                                let s''' = s''{bc = (shift i' 8) + i''}
                                traceShow ("POP BC, BC is now: " ++ 
                                            (printf "%x" (bc s''')) ++ " stack is " ++ 
                                            (show (stack s'''))) s'''
-- LDI (HL), A
            | i == 0x22 =       writeRAM s (hl s) (a s)
-- RET
            | i == 0xC9 = do    let (s', i') = pop2 s
                                s'{pc = i'}
-- CP n
--            | i == 0xFE = do    
--                            = do   
--                                let (s', param) = fetch s
--                                let regs'       = registers s'
--                                if (z regs)
 --                                   then s'{registers = regs{pc = (pc regs') + param}}
 --                                   else s'
             | True      = error $ "Unknown instruction: " ++ (printf "%x" i)
            

pop :: MachineState -> (MachineState, Instruction)
pop s = (s{stack = tail (stack s)}, head (stack s))

pop2 :: MachineState -> (MachineState, Address)
pop2 s  = do    let (s',  i')  = pop s
                let (s'', i'') = pop s'
                (s'', (shift i' 8) + i'')


incPC :: MachineState -> MachineState
incPC s = s{pc = (pc s) +1}

main = do
   input <- B.readFile "rom.gb"
   let unpacked  = [1] --B.unpack input
   let initialState = MachineState {pc = 0, z = False, a = 0,
                                    ram = replicate 0xFFFF 0,
                                    rom = [],
                                    powered = True,
                                    sp = 0,
                                    hl = 0,
                                    h = 0,
                                    n = False,
                                    c = 0,
                                    de = 0,
                                    b = 0,
                                    bc = 0,
                                    stack = [],
                                    e = 0}
   print $ boot initialState unpacked

