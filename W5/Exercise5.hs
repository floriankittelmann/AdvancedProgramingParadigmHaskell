data Instruction n = Inc n | Dec n | Stop | GoToInst | ReigsterZero n | RegisterNoZero n deriving Show

execRegister :: Instruction -> Int -- returns result of current instruction
execRegister (Inc n) = n + 1
execRegister (Dec n) = n - 1
execRegister Stop = toBeImplemented

execInstruction :: Instruction -> Int -- returns position in Instruction


execRegisterMachine :: [Instruction] -> [Int] -> Int
