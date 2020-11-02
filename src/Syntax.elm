module Syntax exposing (..)


type alias Program a   = List (ScDefn a)
type alias CoreProgram = Program String

-- | A Supercombinator Definition contains the neame, its arguments and its body expression
type alias ScDefn a   = (String, List a, Expr a)
type alias CoreScDefn = ScDefn String

-- | The base data structure for the core language
type Expr a = EVar String              -- Variables
            | ENum Int                 -- Numbers
            | EConstr Int Int          -- Constructor tag arity
            | EAp (Expr a) (Expr a)    -- Applications
            | ELet                     -- Let (rec) expressions
              IsRec                    --    boolean with True = recursive 
              (List (a, Expr a))          --    Definitions
              (Expr a)                 --    Body of let (rec)
            | ECase                    -- Case expression
              (Expr a)                 --    Expression to scruntinise
              (List (Alter a))              --    Alternatives
            | ELam (List String) (Expr a) -- Lambda abstraction

type alias CoreExpr  = Expr String

type alias Alter a   = (Int, List String, Expr a)
type alias CoreAlter = Alter String
    
-- Little helper functions for ELet Constructor
type alias IsRec = Bool    

recursive : Bool
recursive = True

nonRecursive : Bool
nonRecursive = False

bindersOf : List (a, b) -> List a
bindersOf = List.map Tuple.first

rhssOf : List (a, b) -> List b
rhssOf = List.map Tuple.second

-- | A function to find out whether we have an atomic expression
isAtomicExpr : Expr a -> Bool
isAtomicExpr expr =
    case expr of
        (EVar _ )   -> True
        (ENum _ )   -> True
        _           -> False

-- | Define the Standard Core Prelude
--   I x = x
--   K x y  = x
--   K1 x y = y
--   S f g x = f x (g x)
--   compose f g x = f (g x)
--   twice f = compose f f

preludeDefs : CoreProgram
preludeDefs
  = [ ("I", [ "x" ], EVar "x"),
      ("K", [ "x", "y" ], EVar "x"),
      ("K1", [ "x", "y" ], EVar "y"),
      ("S", [ "f", "g", "x" ], EAp (EAp (EVar "f") (EVar "x"))
                               (EAp (EVar "g") (EVar "x"))),
      ("compose", [ "f", "g", "x" ], EAp (EVar "f")
                                (EAp (EVar "g") (EVar "x"))),
      ("twice", [ "f" ], EAp (EAp (EVar "compose") (EVar "f")) (EVar "f"))
    ]

    
