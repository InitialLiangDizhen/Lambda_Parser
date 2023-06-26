module LambdaParser where

import Control.Applicative
import Prelude
import Parser
import Data.Lambda
import Data.Builder

-- You can add more imports if you need them

-- Remember that you can (and should) define your own functions, types, and
-- parser combinators. Each of the implementations for the functions below
-- should be fairly short and concise.

-- Terminals
dotP :: Parser Char
dotP = is '.' 

lambP :: Parser Char
lambP = is 'λ' 

lefba :: Parser Char
lefba = is '(' 

rigba :: Parser Char
rigba = is ')'

var :: Parser Char
var = oneof "qwertyuiopasdfghjklzxcvbnm" 

-- Non-terminals
vars :: Parser (Builder -> Builder) -- list of characters
vars = fmap lam var

mvars :: Parser [Builder -> Builder]
mvars = list1 vars

terms :: Parser Builder
terms = fmap term var

mterms :: Parser [Builder]
mterms = list1 terms 

-- blam :: Parser Builder
-- blam = mvars >>=  foldr (<$>) mbterms  --mbterms on right-most for foldr


ld :: Parser Builder
ld = do
    ldb <- blam ||| slam
    return $ ldb

labu :: Parser Builder
labu  = do 
    lefba
    ldb <- ld
    rigba
    return $ ldb

lala :: Parser Builder
lala  = do 
    lefba
    ldb <- ld
    rigba
    lefba
    ldt <- ld
    rigba
    return $ ap ldb ldt

blam :: Parser Builder
blam = do
       bv <- sig
       bt <- tterms
       return $  foldr ($) bt bv -- ($) (<$>) (<*>)

slam :: Parser Builder
slam = do
       bv <- sig
       bl <- labu
       return $ foldr ($) bl bv

sig :: Parser [Builder -> Builder]
sig = do
       lambP
       mp <- mvars
       dotP
       return $ mp

tterms :: Parser Builder
tterms = do
        sp <- termlam ||| sterms ||| endterm --intentionally make it fail
        return $ sp

endterm :: Parser Builder
endterm = do
    mt <- mbterms
    return $ mt
                 
sterms :: Parser Builder
sterms = do
         mt <- mbterms
         lefba
         mbt <- mbterms 
         rigba
         return $ ap mt mbt

termlam :: Parser Builder
termlam = do
         mt <- mbterms
         lab <- labu
         return $ ap mt lab      

mbterms :: Parser Builder
mbterms = mterms >>= pure . (foldl1 ap) -- for >>= can pure whole thing afterward

-- start ::= lala |labu | ld
-- labu ::= lefba ld rigba 
-- ld ::=  blam | slam
-- blam ::= sig tterms
-- slam ::= sig labu
-- sig  ::= lambP mvars dotP
-- mvars ::= vars | var
-- vars ::= var var
-- tterms ::= termlam | sterms | endterm
-- termlam ::= mbterms labu
-- sterms ::= lefba mbterms rigba
-- mbterms ::= mterms | terms
-- mterms ::= terms terms 
-- endterm ::= mbterms
-- terms ::= var 


-- var ::= <all alphabets>
-- lambP ::= "λ"
-- dotP ::= "."
-- lefba ::= "("
-- rigba ::= ")"


{-|
    Part 1
-}

-- | Exercise 1

-- | Parses a string representing a lambda calculus expression in long form
--
-- >>> parse longLambdaP "(λx.xx)"
-- Result >< \x.xx
--
-- >>> parse longLambdaP "(λx.(λy.xy(xx)))"
-- Result >< \xy.xy(xx)
--
-- >>> parse longLambdaP "(λx(λy.x))"
-- UnexpectedChar '('
-- left-hand side should be in parathesis,  otherwise \x. after x everything considered as body
longLambdaP :: Parser Lambda
longLambdaP = do
    ll <- labu 
    return $ build $ ll
    
            
       

-- | Parses a string representing a lambda calculus expression in short form
--
-- >>> parse shortLambdaP "λx.xx"
-- Result >< \x.xx
--
-- >>> parse shortLambdaP "λxy.xy(xx)"
-- Result >< \xy.xy(xx)
--
-- >>> parse shortLambdaP "λx.x(λy.yy)"
-- Result >< \x.x\y.yy
--
-- >>> parse shortLambdaP "(λx.x)(λy.yy)"
-- Result >< (\x.x)\y.yy  
--
-- >>> parse shortLambdaP "λxyz"
-- UnexpectedEof

shortLambdaP :: Parser Lambda
shortLambdaP = do
    sl<- lala ||| labu ||| ld
    return $ build $ sl

-- | Parses a string representing a lambda calculus expression in short or long form
-- >>> parse lambdaP "λx.xx"
-- Result >< \x.xx
--
-- >>> parse lambdaP "(λx.xx)"
-- Result >< \x.xx
--
-- >>> parse lambdaP "λx..x"
-- UnexpectedChar '.'
--

lambdaP :: Parser Lambda
lambdaP = do
    sl<- lala ||| labu ||| ld
    return $ build $ sl

{-|
    Part 2
-}

-- | Exercise 1

-- IMPORTANT: The church encoding for boolean constructs can be found here -> https://tgdwyer.github.io/lambdacalculus/#church-encodings

-- | Parse a logical expression and returns in lambda calculus
-- >>> lamToBool <$> parse logicP "True and False"
-- Result >< Just False
--
-- >>> lamToBool <$> parse logicP "True and False or not False and True"
-- Result >< Just True
--
-- >>> lamToBool <$> parse logicP "not not not False"
-- Result >< Just True
-- 
-- >>> parse logicP "True and False"
-- Result >< (\xy.(\btf.btf)xy\_f.f)(\t_.t)\_f.f
-- >>> parse logicP "not False"
-- Result >< (\x.(\btf.btf)x(\_f.f)\t_.t)\_f.f
-- >>> lamToBool <$> parse logicP "if True and not False then True or True else False"
-- Result >< Just True

--reminder for myself, change logicP into orP 
--Parser Builder -> Parser Lambda
-- return -> build $ 


logicP :: Parser Lambda
logicP = do 
    lop <- chainAP ||| mlp
    return $ build $ lop

chain :: Parser a -> Parser (a -> a -> a) -> Parser a
chain p op = p >>= rest      -- a from this function
 where    -- parse first p
  rest a =
    (do    -- parse operator
        f <- op        -- f <- p = f =<< p
        b <- p  --parse snd p    -- f function = Parser (a -> a -> a) f a b take a b as arguments, chain all as a new parser
        rest (f a b)   --recursive loop   do b parser, a parser then f function, 
      )                --chain all as a new parser as a
      ||| pure a  

chainAP :: Parser Builder
chainAP = chain mlp anorch

anorch :: Parser(Builder -> Builder -> Builder)
anorch = do 
    spaces
    anor <-  chaOr ||| chaAnd
    spaces
    return $  (ap) . (ap anor)

chaAnd :: Parser Builder
chaAnd = do
    asp <- aspa
    return $ asp

chaOr :: Parser Builder
chaOr = do
    osp <- ospa
    return $ osp

mlp :: Parser Builder
mlp = do
    mp <- lobu ||| lod
    return $ mp

lobu :: Parser Builder
lobu  = do 
    lefba
    ldb <- lod
    rigba
    return $ ldb

lod :: Parser Builder
lod = do
    lodb <- ifP ||| notP ||| andP ||| orP ||| tspa ||| fspa
    return $ lodb

notP :: Parser Builder
notP = do
    st <- notN ||| notT ||| notF
    return $ st

notT :: Parser Builder
notT = do
    ns <- nspa
    spaces
    ts <- tspa
    nt <- pure $ ap ns ts
    return $ nt

notF :: Parser Builder
notF = do
    ns <- nspa
    spaces
    fs <- fspa
    nf <- pure $ ap ns fs
    return $ nf

notN :: Parser Builder
notN = do
    mpn <- mbnotN
    tf <- tspa ||| fspa
    return $ ap mpn tf

mnotN :: Parser [Builder]
mnotN = list1 snspa

mbnotN :: Parser Builder
mbnotN = mnotN >>= pure . (foldr1 ap) 

nspa :: Parser Builder
nspa = do
    string "not"
    return $ notB

snspa :: Parser Builder
snspa = do
    string "not"
    spaces
    return $ notB

-- munch1 can solve
andP :: Parser Builder
andP = do
    tf <- notP ||| tspa ||| fspa 
    spaces
    an <- aspa
    spaces
    cs <- notP ||| tspa ||| fspa
    return $ ap (ap an tf) cs

--munch1 can solve
orP :: Parser Builder
orP = do
    tf <- notP ||| tspa ||| fspa
    spaces
    os <- ospa
    spaces
    cs <- notP ||| tspa ||| fspa
    return $ ap (ap os tf) cs


ifP :: Parser Builder
ifP = do
    is <- ispa
    spaces
    ifC <- notP ||| andP ||| orP ||| tspa ||| fspa
    spaces
    string "then"
    spaces
    tn <- notP ||| andP ||| orP ||| tspa ||| fspa
    spaces
    string "else"
    spaces
    el <- notP ||| andP ||| orP ||| tspa ||| fspa
    return $ ap (ap (ap is ifC) tn) el


ispa :: Parser Builder
ispa = do
    string "if"
    return $ ifB

ospa :: Parser Builder
ospa = do
    string "or"
    return $ orB

aspa :: Parser Builder
aspa = do
    string "and"
    return $ andB

tspa :: Parser Builder
tspa = do
    string "True"
    return $ trueB

fspa :: Parser Builder
fspa = do
    string "False"
    return $ falseB


ifB :: Builder
ifB = lam 'b' $ lam 't' $  lam 'f' $ tmbtf
    where
        tmbtf = ap (ap (term 'b') (term 't')) (term 'f')

trueB :: Builder
-- \t_.t in Builder type
trueB = boolToLam True

falseB :: Builder
falseB = boolToLam False

andB :: Builder
andB = lam 'x' $ lam 'y' $ ap (ap (ap ifB (term 'x')) (term 'y')) falseB

orB :: Builder
orB = lam 'x' $ lam 'y' $ ap (ap (ap ifB (term 'x')) trueB) (term 'y')

notB :: Builder
notB = lam 'x' $ ap (ap (ap ifB (term 'x')) falseB) trueB


-- | Exercise 2

-- | The church encoding for arithmetic operations are given below (with x and y being church numerals)

-- | x + y = add = λxy.y succ m
-- | x - y = minus = λxy.y pred x
-- | x * y = multiply = λxyf.x(yf)
-- | x ** y = exp = λxy.yx

-- | The helper functions you'll need are:
-- | succ = λnfx.f(nfx)
-- | pred = λnfx.n(λgh.h(gf))(λu.x)(λu.u)
-- | Note since we haven't encoded negative numbers pred 0 == 0, and m - n (where n > m) = 0

-- | Parse simple arithmetic expressions involving + - and natural numbers into lambda calculus
-- >>> lamToInt <$> parse basicArithmeticP "5 + 4"
-- Result >< Just 9
--
-- >>> lamToInt <$> parse basicArithmeticP "5 + 9 - 3 + 2"
-- Result >< Just 13

basicArithmeticP :: Parser Lambda
basicArithmeticP = do
    at <- arthP
    return $ build at

-- | Parse arithmetic expressions involving + - * ** () and natural numbers into lambda calculus
-- >>> lamToInt <$> parse arithmeticP "5 + 9 * 3 - 2 ** 3"
-- Result >< Just 24
--
-- >>> lamToInt <$> parse arithmeticP "100 - 4 * 2**(4-1)"
-- Result >< Just 68

arithmeticP :: Parser Lambda
arithmeticP = do
    sim <- arthP
    return $ build sim


arthP :: Parser Builder
arthP = do
    art <- siexpr ||| complexpr ||| intP
    return $ art


--w11 workshop
complexpr :: Parser Builder
complexpr = chain arhterm sumOp

arhterm :: Parser Builder
arhterm = chain (group ||| expterm ||| intP ) prodOp 

expterm :: Parser Builder
expterm = chain (group ||| intP) expOp

siexpr :: Parser Builder
siexpr = do
    intp <- intP
    orP <- addP ||| minusP
    intt <- intP
    return $ ap (ap orP intp) intt

-- w11 workshop
group :: Parser Builder
group = do
    lefba
    ce <- complexpr
    rigba
    return $ ce

expOp :: Parser (Builder -> Builder -> Builder)
expOp = do 
    spaces
    ex <- expP
    spaces
    return $ (ap) . (ap ex)

expP :: Parser Builder
expP = do
    string "**"  --string would pase the whole string if the first charcter in the string meet condition
    return $ expB

expB :: Builder
expB = lam 'x' $ lam 'y' $ tms 
    where tms = ap (term 'y') (term 'x')

prodOp :: Parser (Builder -> Builder -> Builder)
prodOp = do 
    spaces
    ml <-  mltiP
    spaces
    return $ (ap) . (ap ml)

sumOp :: Parser (Builder -> Builder -> Builder)
sumOp = do 
    spaces
    plumin <- addP ||| minusP
    spaces
    return $ (ap) . (ap plumin)

mltiP :: Parser Builder
mltiP = do
    is '*'
    return $ multiB

multiB :: Builder 
multiB = lam 'x' $ lam 'y' $ lam 'f' $ tms 
    where tms = ap (term 'x') (ap (term 'y') (term 'f')) 

addP :: Parser Builder
addP = do
    is '+'
    return $ addB

minusP :: Parser Builder
minusP = do
    is '-'
    return $ minusB

intP :: Parser Builder
intP = do
    nump <- int
    lain <- pure $ intToLam nump
    return $ lain

addB :: Builder
addB = lam 'x' $ lam 'y' $ tms 
    where
        tms = ap (ap (term 'y') succB) (term 'x')

minusB :: Builder
minusB = lam 'x' $ lam 'y' $ tms
    where
        tms = ap (ap (term 'y') predB) (term 'x')

succB :: Builder
succB = lam 'n' $ lam 'f' $  lam 'x' $ tmbtf
    where
        tmbtf = ap (term 'f') (ap (ap (term 'n') (term 'f')) (term 'x')) 

predB :: Builder
predB = lam 'n' $ lam 'f' $  lam 'x' $ tmnfx
    where
        tmnfx = ap (ap firt sndt) thirt
        firt = ap (term 'n') (lam 'g' $ lam 'h' $ ghterm)
        ghterm = ap (term 'h') (ap (term 'g') (term 'f'))
        sndt = lam 'u' $ term 'x'
        thirt = lam 'u' $ term 'u'

--tut 11
readInt :: String -> Maybe (Int, String)
readInt s = case reads s of
  [(x, rest)] -> Just (x, rest)
  _           -> Nothing

--tut 11
int :: Parser Int
int = P f
 where
  -- This is okay because the case statement is small
  f "" = Error UnexpectedEof
  f x  = case readInt x of
    Just (v, rest) -> Result rest v   -- Result rest v is just a parser, return it self would call
    Nothing        -> Error $ UnexpectedChar (head x)  --can call the parser again


-- | Exercise 3

-- | The church encoding for comparison operations are given below (with x and y being church numerals)

-- | x <= y = LEQ = λmn.isZero (minus m n)
-- | x == y = EQ = λmn.and (LEQ m n) (LEQ n m)

-- | The helper function you'll need is:
-- | isZero = λn.n(λx.False)True

-- >>> lamToBool <$> parse complexCalcP "9 - 2 <= 3 + 6"
-- Result >< Just True
--
-- >>> lamToBool <$> parse complexCalcP "15 - 2 * 2 != 2**3 + 3 or 5 * 3 + 1 < 9"
-- Result >< Just False

complexCalcP :: Parser Lambda
complexCalcP = do
    com <- complxP
    return $ build $ com
    
complxP :: Parser Builder
complxP = chain (eqNumP ||| arthP) chaAnOr

eqNumP :: Parser Builder
eqNumP = chain arthP eqleqP

eqleqP :: Parser (Builder -> Builder -> Builder)
eqleqP = do
    spaces
    sig <- leqP ||| eqP |||neqP ||| meqP ||| mtaP |||ltaP
    spaces
    return $ (ap) . (ap sig)

chaAnOr :: Parser(Builder -> Builder -> Builder)
chaAnOr = do 
    spaces
    log <- anchaP ||| orchaP
    spaces
    return $ (ap) . (ap log)

anchaP :: Parser Builder
anchaP = do
    tr <- tspa
    return $ tr

orchaP :: Parser Builder
orchaP = do
    ors <- ospa
    return $ ors

leqB :: Builder   --ap notB leqB = mtB (more than builder)   --ap mtB andB eqB
leqB = lam 'm' $ lam 'n' $ eqterms  --ap leqB andB notB eqB = letB (less than builder)
    where eqterms = ap isZeroB (ap (ap minusB (term 'm')) (term 'n'))

leqP :: Parser Builder
leqP = do
    string "<="   -- need other function
    return $ leqB

eqB :: Builder    --ap notB eqB = neqB    
eqB = lam 'm' $ lam 'n' $ eqterms
    where
        eqterms = ap (ap andB (ap (ap leqB (term 'm')) (term 'n'))) (ap (ap leqB (term 'n')) (term 'm'))

eqP :: Parser Builder
eqP = do
    string "==" 
    return $ eqB

neqB :: Builder
neqB = ap notB eqB

neqP :: Parser Builder
neqP = do
    string "!="
    return $ neqB

mtaB :: Builder
mtaB = ap notB leqB

mtaP :: Parser Builder
mtaP = do
    string ">"
    return $ mtaB

meqB :: Builder
meqB = ap (ap andB mtaB) eqB

meqP :: Parser Builder
meqP = do
    string "=>"
    return $ meqB

ltaB :: Builder
ltaB = ap (ap andB leqB) neqB

ltaP :: Parser Builder
ltaP = do
    string "<"
    return $ ltaB


-- neqP :: Parser Builder
-- neqP = do
--     string "!=" 
--     return $ neqB

-- neqB :: Builder 
-- neqB = lam 'm' $ lam 'n' $ eqterms
--     where
--         eqterms = ap (ap andB (ap (ap meqB (term 'm')) (term 'n'))) (ap (ap meqB (term 'n')) (term 'm'))

isZeroB :: Builder
isZeroB = lam 'n' $ ter
    where
        ter = ap (ap (term 'n') (lam 'x' $ falseB)) trueB

-- isNotZeroB :: Builder
-- isNotZeroB = lam 'n' $ ter
--     where
--         ter = ap (ap (term 'n') (lam 'x' $ trueB)) falseB

{-|
    Part 3
-}

-- | Exercise 1

-- | The church encoding for list constructs are given below
-- | [] = null = λcn.n
-- | isNull = λl.l(λht.False) True
-- | cons = λhtcn.ch(tcn)
-- | head = λl.l(λht.h) False
-- | tail = λlcn.l(λhtg.gh(tc))(λt.n)(λht.t)
-- >>> parse listP "[]"
-- Result >< \cn.n

-- >>> parse listP "[True]"
-- Result >< (\htcn.ch(tcn))(\xy.x)\cn.n
--
-- >>> parse listP "[0, 0]"
-- Result >< (\htcn.ch(tcn))(\fx.x)((\htcn.ch(tcn))(\fx.x)\cn.n)
--
-- >>> parse listP "[0, 0"
-- UnexpectedEof
listP :: Parser Lambda
listP = do
    arr <- arrayP
    return $ build $ arr

-- w11 tutorial solution
arrayP :: Parser Builder
arrayP = do
    is '['
    arr <- folSomeP ||| nulLisP
    is ']'
    return $ arr


folSomeP :: Parser Builder
folSomeP = do
    solst <- someP
    let fors =  (ap consB) <$> solst
    let fls = foldr ap nulLisB fors
    return $ fls


someP :: Parser [Builder]
someP = sepby1 (intP ||| tspa ||| fspa) (string ", ") 

nulLisB :: Builder
nulLisB = lam 'c' $ lam 'n' $ term 'n'

nulLisP :: Parser Builder
nulLisP = do
    spaces
    return $ nulLisB


isNullB :: Builder
isNullB = lam 'l' $ nt
    where
        nt = ap (ap (term 'l') (lam 'h' $ lam 't' $ falseB)) trueB

isNullP :: Parser Builder
isNullP = do
    string "isNull"
    spaces
    return $ isNullB


consB :: Builder
consB = lam 'h' $ lam 't' $ lam 'c' $ lam 'n' $ nt
    where
        nt = ap (ap (term 'c') (term 'h')) (ap (ap (term 't') (term 'c')) (term 'n'))

consP :: Parser Builder
consP = do
    string "cons"
    spaces
    return $ consB

headB :: Builder
headB = lam 'l' $ nt
    where
        nt = ap (ap (term 'l') (lam 'h' $ lam 't' $ (term 'h'))) falseB

headP :: Parser Builder
headP = do
    string "head"
    spaces
    return $ headB

tailB :: Builder
tailB = lam 'l' $ lam 'c' $ lam 'n' $ nt
    where
        nt = ap (ap (ap fstt sndt) trdt) fout
        fstt = term 'l'
        sndt = lam 'h' $ lam 't' $ lam 'g' $ ghtc
        ghtc = ap (ap (term 'g') (term 'h')) (ap (term 't') (term 'c')) 
        trdt = lam 't' $ term 'n'
        fout = lam 'h' $ lam 't' $ term 't'

tailP :: Parser Builder
tailP = do
    string "rest" ||| string "tail"
    spaces
    return $ tailB

-- w11 tut solution
sepby1 :: Parser a -> Parser s -> Parser [a]
sepby1 a sep = liftA2 (:) a (list (sep *> a))

-- w11 tut solution
sepby :: Parser a -> Parser s -> Parser [a]
sepby a sep = sepby1 a sep ||| pure []

-- >>> lamToBool <$> parse listOpP "head [True, False, True, False, False]"
-- Result >< Just True
--
-- >>> lamToBool <$> parse listOpP "head rest [True, False, True, False, False]"
-- Result >< Just False          ap headB  (ap restB listB)
--
-- >>> lamToBool <$> parse listOpP "isNull []"
-- Result >< Just True
--
-- >>> lamToBool <$> parse listOpP "isNull [1, 2, 3]"
-- Result >< Just False

listOpP :: Parser Lambda
listOpP = do
    fl <- folConfp
    return $ build $ fl

-- comlsOp :: Parser Builder
-- comlsOp = do
--     fup <- folConfp
--     ls <- arrayP
--     return $ ap fup ls 

-- folConfp :: Parser Builder
-- folConfp = do
--     ls <- mallConfp
--     let fl = foldl1 ap ls
--     return $ fl

folConfp :: Parser Builder
folConfp = do
    pls <- mallConfp
    ls <- arrayP
    let fl = foldr ap ls pls 
    return $ fl

mallConfp :: Parser [Builder]
mallConfp = list1 (isNullP ||| consP ||| headP ||| tailP)





-- | Exercise 2

-- | Implement your function(s) of choice below!
