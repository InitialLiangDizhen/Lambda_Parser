module LambdaParser where

import Prelude
import Parser
import Data.Lambda
import Data.Builder

-- You can add more imports if you need them

-- Remember that you can (and should) define your own functions, types, and
-- parser combinators. Each of the implementations for the functions below
-- should be fairly short and concise.

char :: Parser Char
char = P f
 where
  f ""       = Error UnexpectedEof
  f (x : xs) = Result xs x  

dotP :: Parser Char
dotP = is '.' 

lambP :: Parser Char
lambP = is 'λ' 

lefba :: Parser Char
lefba = is '(' 

rigba :: Parser Char
rigba = is ')'

(=<<) :: Monad m => (a -> m b) -> m a -> m b
(=<<) = flip (>>=)

(<<) :: Monad m => m a -> m b -> m a
(<<) = flip (>>)

var :: Parser Char
var = oneof "qwertyuiopasdfghjklzxcvbnm" 

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

-- start ::= labu | ld
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
    sl<- labu ||| ld
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
    sl<- labu ||| ld
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
--
-- >>> parse logicP "not False"
-- Result >< (\x.(\btf.btf)x(\_f.f)\t_.t)\_f.f
-- >>> lamToBool <$> parse logicP "if True and not False then True or True else False"
-- Result >< Just True

logicP :: Parser Lambda
logicP = undefined

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
basicArithmeticP = undefined

-- | Parse arithmetic expressions involving + - * ** () and natural numbers into lambda calculus
-- >>> lamToInt <$> parse arithmeticP "5 + 9 * 3 - 2**3"
-- Result >< Just 24
--
-- >>> lamToInt <$> parse arithmeticP "100 - 4 * 2**(4-1)"
-- Result >< Just 68
arithmeticP :: Parser Lambda
arithmeticP = undefined


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
complexCalcP = undefined


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
--
-- >>> parse listP "[]"
-- Result >< \cn.n
--
-- >>> parse listP "[True]"
-- Result >< (\htcn.ch(tcn))(\xy.x)\cn.n
--
-- >>> parse listP "[0, 0]"
-- Result >< (\htcn.ch(tcn))(\fx.x)((\htcn.ch(tcn))(\fx.x)\cn.n)
--
-- >>> parse listP "[0, 0"
-- UnexpectedEof
listP :: Parser Lambda
listP = undefined

-- >>> lamToBool <$> parse listOpP "head [True, False, True, False, False]"
-- Result >< Just True
--
-- >>> lamToBool <$> parse listOpP "head rest [True, False, True, False, False]"
-- Result >< Just False
--
-- >>> lamToBool <$> parse listOpP "isNull []"
-- Result >< Just True
--
-- >>> lamToBool <$> parse listOpP "isNull [1, 2, 3]"
-- Result >< Just False
listOpP :: Parser Lambda
listOpP = undefined


-- | Exercise 2

-- | Implement your function(s) of choice below!
