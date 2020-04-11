module Compiler.JavaScript.AST

%default total

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
