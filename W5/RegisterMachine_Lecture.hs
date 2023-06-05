-- saturated substraction
--(|-) :: Integer -> Integer -> Integer
(|-) :: (Num a, Ord a) => a -> a -> a
x |- y | x < y = 0
       | otherwise  = x - y

type Registers = [Integer]
type RegAddr = Int

--- Recursive way to solve it
incrV1 n (reg:regs)
  | n == 0      = reg + 1 : regs
  | otherwise   = reg : incrV1 (n-1) regs

decrV1 n (reg:regs)
  | n == 0      = reg |- 1 : regs
  | otherwise   = reg : decrV1 (n-1) regs

updateV1 f n (reg:regs)
  | n == 0      = f reg : regs
  | otherwise   = reg : updateV1 f (n-1) regs

incrV2 = updateV1 (+1)
decrV2 = updateV1 (|-1)

--- other way to solve it providing f the operation
updateV2 f n regs = firstRegs ++ f reg:lastRegs
  where (firstRegs, reg:lastRegs) = splitAt n regs

type PC = Int

data Instr
  = Incr RegAddr
  | Decr RegAddr
  | Stop
  | Goto PC
  | GotoIfZero RegAddr PC
  | GotoIfNotZero RegAddr PC

type Program = [Instr]
type State = (PC, Registers)

incr = incrV2
decr = decrV2

execInstr :: Program -> State -> State
execInstr prog (pc, regs) =
  case instr of
    Incr regAddr -> (pc + 1, incr regAddr regs)
    Decr regAddr -> (pc + 1, decr regAddr regs)
    Stop -> (-1, regs)
    Goto target -> (target, regs)
    GotoIfZero regAddr target -> (if regs !! regAddr == 0 then target else pc+1, regs)
    GotoIfNotZero regAddr target -> (if regs !! regAddr /= 0 then target else pc+1, regs)
  where instr = prog !! pc

regMachine prog regs = run initialState
  where
    run (-1, regs) = regs
    run state = run (execInstr prog state)
    initialState = (0, regs)
-- what is the difference of where / case ... of

-- example
exaProg1 = [
  GotoIfZero 1 4,
  Incr 0,
  Decr 1,
  Goto 0,
  Stop
  ]

runProg1 :: Integer -> Integer -> Integer
runProg1 x y = head(regMachine exaProg1 [x,y])