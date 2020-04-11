module Compiler.JavaScript.AST

%default total

indent : String -> String
indent = id -- TODO: Implement `indent`.

mutual
  public export
  data JsStmt : Type where
      JsEmpty : JsStmt
      JsComment : String -> JsStmt
      JsExprStmt : JsExpr -> JsStmt
      JsFun : String -> List String -> JsStmt -> JsStmt
      JsSeq : JsStmt -> JsStmt -> JsStmt
      JsReturn : JsExpr -> JsStmt
      JsDecVar : String -> JsExpr -> JsStmt
      JsDecConst : String -> JsExpr -> JsStmt
      JsDecLet : String -> JsExpr -> JsStmt
      JsSet : JsExpr -> JsExpr -> JsStmt
      JsIf : JsExpr -> JsStmt -> Maybe JsStmt -> JsStmt
      JsSwitchCase : JsExpr -> List (JsExpr, JsStmt) -> Maybe JsStmt -> JsStmt
      JsError : JsExpr -> JsStmt
      JsForever : JsStmt -> JsStmt
      JsContinue : JsStmt
      JsBreak : JsStmt

  public export
  data JsExpr : Type where
       JsNull : JsExpr
       JsUndefined : JsExpr
       JsThis : JsExpr
       JsLambda : List String -> JsStmt -> JsExpr
       JsApp : JsExpr -> List JsExpr -> JsExpr
       JsNew : JsExpr -> List JsExpr -> JsExpr
       JsPart : JsExpr -> String -> JsExpr
       JsMethod : JsExpr -> String -> List JsExpr -> JsExpr
       JsVar : String -> JsExpr
       JsArrayProj : JsExpr -> JsExpr -> JsExpr
       JsObj : List (String, JsExpr) -> JsExpr
       JsProp : JsExpr -> String -> JsExpr
       JsInt : Int -> JsExpr
       JsBool : Bool -> JsExpr
       JsInteger : Integer -> JsExpr
       JsDouble : Double -> JsExpr
       JsStr : String -> JsExpr
       JsArray : List JsExpr -> JsExpr
       JsErrorExp : JsExpr -> JsExpr
       JsUniOp : String -> JsExpr -> JsExpr
       JsBinOp : String -> JsExpr -> JsExpr -> JsExpr
       JsForeign : String -> List JsExpr -> JsExpr
       JsB2I : JsExpr -> JsExpr
       JsForce : JsExpr -> JsExpr

  showJsArgs : List String -> String
  showJsArgs args = pack $ intercalate (unpack ", ") $ map unpack args

  showJsStmt : JsStmt -> String
  showJsStmt JsEmpty = ""
  showJsStmt (JsComment comment) = unlines $ map ("// " ++) $ lines comment
  showJsStmt (JsExprStmt expr) = concat [showJsExpr expr, ";"]
  showJsStmt (JsReturn expr) = concat ["return", showJsExpr expr, ";"]
  showJsStmt (JsDecVar name expr) =
    concat ["var", name, " = ", showJsExpr expr, ";"]
  showJsStmt (JsDecConst name expr) =
    concat ["const", name, " = ", showJsExpr expr, ";"]
  showJsStmt (JsDecLet name expr) =
    concat ["let", name, " = ", showJsExpr expr, ";"]
  showJsStmt (JsFun name args body) =
    concat
      [ "function "
      , name
      , "("
      , showJsArgs args
      , ") {\n"
      , indent $ showJsStmt body
      , "}\n"
      ]
  -- TODO: The Haskell equivalent was to use an as-pattern to capture the
  -- `else if`, but I couldn't figure out the equivalent syntax in Idris.
  showJsStmt (JsIf cond ifBranch (Just (JsIf a b c))) =
    concat
      [ "if ("
      , showJsExpr cond
      , ") {\n"
      , indent $ showJsStmt ifBranch
      , "} else "
      , showJsStmt (JsIf a b c)
      ]
  showJsStmt (JsIf cond ifBranch (Just elseBranch)) =
    concat
      [ "if ("
      , showJsExpr cond
      , ") {\n"
      , indent $ showJsStmt ifBranch
      , "} else "
      , indent $ showJsStmt elseBranch
      , "}\n"
      ]
  showJsStmt (JsIf cond ifBranch Nothing) =
    concat
      [ "if ("
      , showJsExpr cond
      , ") {\n"
      , indent $ showJsStmt ifBranch
      , "}\n "
      ]
  showJsStmt (JsSwitchCase expr cases default) =
    concat
      [ "switch ("
      , showJsExpr expr
      , ") {\n"
      , indent $ concat $ map showCase cases
      , indent $ showDefault default
      , "}\n"
      ]
    where
      showCase : (JsExpr, JsStmt) -> String
      showCase (expr, body) =
        concat
          [ "case"
          , showJsExpr expr
          , ":\n"
          , indent $ concat [showJsStmt body, "\nbreak;\n"]
          ]

      showDefault : Maybe JsStmt -> String
      showDefault Nothing = ""
      showDefault (Just body) =
        concat ["default:\n", indent $ concat [showJsStmt body, "\nbreak;\n"]]
  showJsStmt (JsError expr) = ?handleError
  showJsStmt (JsForever body) =
    concat ["for(;;) {\n", showJsStmt body, "}\n"]
  showJsStmt JsContinue = "continue;"
  showJsStmt JsBreak = "break;"
  showJsStmt (JsSeq JsEmpty y) = showJsStmt y
  showJsStmt (JsSeq x JsEmpty) = showJsStmt x
  showJsStmt (JsSeq x y) = concat [showJsStmt x, "\n", showJsStmt y]
  showJsStmt (JsSet term expr) =
    concat [showJsExpr term, " = ", showJsExpr expr, ";"]

  showJsExpr : JsExpr -> String
  showJsExpr JsNull = "null"
  showJsExpr JsUndefined = "(void 0)"
  showJsExpr JsThis = "this"
  showJsExpr (JsLambda args body) =
    concat
      [ "(function("
      , showJsArgs args
      , ") {\n"
      , indent $ showJsStmt body
      , "})"
      ]
  showJsExpr (JsApp fn args) =
    concat [showJsExpr fn, "(", showJsArgs $ map showJsExpr args, ")"]
  showJsExpr (JsNew fn args) =
    concat ["new ", showJsExpr fn, "(", showJsArgs $ map showJsExpr args, ")"]
  showJsExpr (JsMethod obj name args) =
    concat
      [ showJsExpr obj
      , "."
      , name
      , "("
      , showJsArgs $ map showJsExpr args
      , ")"
      ]
  showJsExpr (JsPart obj name) =
    concat [showJsExpr obj, "[", name, "]"]
  showJsExpr (JsVar var) = var
  showJsExpr (JsObj properties) =
    concat
      [ "({"
      , pack $ intercalate (unpack ", ") $ map unpack properties'
      , "})"
      ]
    where
      properties' : List String
      properties' =
        map (\(name, value) => concat [name, ": ", showJsExpr value]) properties
  showJsExpr (JsProp obj name) = concat [showJsExpr obj, ".", name]
  showJsExpr (JsArrayProj i expr) =
    concat [showJsExpr expr, "[", showJsExpr i, "]"]
  showJsExpr (JsInt i) = show i
  showJsExpr (JsBool True) = "true"
  showJsExpr (JsBool False) = "false"
  showJsExpr (JsDouble d) = show d
  showJsExpr (JsInteger i) = show i
  showJsExpr (JsStr s) = ?showString
  showJsExpr (JsArray elements) =
    concat ["[", showJsArgs $ map showJsExpr elements, "]"]
  showJsExpr (JsErrorExp expr) = ?showErrorExpr
  showJsExpr (JsBinOp op lhs rhs) =
    concat ["(", showJsExpr lhs, " ", op, " ", showJsExpr rhs, ")"]
  showJsExpr (JsUniOp op expr) = concat ["(", op, showJsExpr expr, ")"]
  showJsExpr (JsForeign code args) = ?showForeign
  showJsExpr (JsB2I expr) = showJsExpr $ JsBinOp "+" expr (JsInt 0)
  showJsExpr (JsForce expr) = ?showForce
