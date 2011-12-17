-- This file should contain the mnemonical names and their opcode
-- representations
module Mnemonics where
data Mnemonic = Mnemonic Int Int
m_RTI = Mnemonic 0x40 1
m_BRK = Mnemonic 0x00 1
