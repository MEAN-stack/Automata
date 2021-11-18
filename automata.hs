import Data.List
import Numeric (showHex, showIntAtBase)

binaryStr n = showIntAtBase 2 (\i -> if i==0 then ' ' else '*') n ""

binStr n = reverse $ take 8 c 
           where b = binaryStr n
                 c = reverse b ++ "        "

ns [] = []
ns (x:[]) = []
ns (x:y:[]) = []
ns z = take 3 z : ns (tail z)

automate n s = ' ':map (gen n) (ns s)++" "

gen rule "***" = (binStr rule)!!0
gen rule "** " = (binStr rule)!!1
gen rule "* *" = (binStr rule)!!2
gen rule "*  " = (binStr rule)!!3
gen rule " **" = (binStr rule)!!4
gen rule " * " = (binStr rule)!!5
gen rule "  *" = (binStr rule)!!6
gen rule "   " = (binStr rule)!!7

auto r n = intercalate "\n" u ++ "\n"
           where s = replicate n ' '
                 t = s++"*"++s
                 u = take n $ iterate (automate r) t

main = putStrLn $ auto 30 500
