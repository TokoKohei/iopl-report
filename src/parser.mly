%{ open Syntax %}

%token LPAREN RPAREN LBRACKET RBRACKET SEMICOLON SEMISEMI (* 3.6.2 *)
%token PLUS MULT LT ANDAND OROR CONS (* 3.2.3 ,3.6.2 *)
%token IF THEN ELSE TRUE FALSE 
%token <int> INTV
%token <Syntax.id> ID 
%token LET REC IN EQ (* 3.3.1 ,3.5.1 *)
%token RARROW FUN (* 3.4.1 *)
%token <string> STRINGV 
%token CONCAT
%token PRINT_STRING 
%token DOT_LBRACKET RBRACKET

%start toplevel
%type <Syntax.program> toplevel
%%

toplevel :
    e=Expr SEMISEMI { Exp e }
  | LET x=ID params=ParamList EQ e=Expr SEMISEMI { 
      (* 3.4.3 let f x1 x2 ... xn = e を fun x1 -> fun x2 -> ... -> fun xn -> e に変換 *)
      let rec make_fun_exp params body = match params with
        | [] -> body
        | p :: rest -> FunExp (p, make_fun_exp rest body)
      in
      Decl (x, make_fun_exp params e) 
    } (* 3.3.1 複数パラメータ対応 *)
  | LET x=ID EQ e=Expr SEMISEMI { Decl (x, e) } (* 3.3.1 従来の単一パラメータ *)
  | LET REC f=ID params=ParamList EQ e=Expr SEMISEMI {
      (* 3.4.3 let rec f x1 x2 ... xn = e の処理 *)
      match params with
        | [] -> failwith "Recursive function must have at least one parameter"
        | x :: rest -> 
          let rec make_fun_exp params body = match params with
            | [] -> body
            | p :: rest -> FunExp (p, make_fun_exp rest body)
          in
          RecDecl (f, x, make_fun_exp rest e)
    } (* 3.5.1 複数パラメータ対応 *)
  | LET REC f=ID EQ FUN x=ID RARROW e=Expr SEMISEMI  
    { RecDecl (f, x, e) } (* 3.5.1 従来のfun記法 *)
  | decls=MultipleDecls SEMISEMI { MultiDecl decls }  (* 3.3.2 複数宣言の処理 *)

(* 3.4.3 パラメータリストの定義 - 1個以上のIDのリスト *)
ParamList :
    x=ID { [x] }
  | x=ID params=ParamList { x :: params }

(* 3.3.2 複数宣言の再帰的定義 - 左再帰でリストを構築 *)
MultipleDecls :
  decl=SingleDecl { [decl] } (* 3.3.2 単一宣言をリストで包む *)
  | decl=SingleDecl decls=MultipleDecls { decl :: decls } (* 3.3.2 宣言を先頭に追加 *)

(* 3.3.2 個別の宣言を定義 - let 文と let rec 文に対応 *)
SingleDecl :
    LET x=ID params=ParamList EQ e=Expr { 
      let rec make_fun_exp params body = match params with
        | [] -> body
        | p :: rest -> FunExp (p, make_fun_exp rest body)
      in
      Decl (x, make_fun_exp params e) 
    }
  | LET x=ID EQ e=Expr { Decl (x, e) }
  | LET REC f=ID params=ParamList EQ e=Expr {
      match params with
        | [] -> failwith "Recursive function must have at least one parameter"
        | x :: rest -> 
          let rec make_fun_exp params body = match params with
            | [] -> body
            | p :: rest -> FunExp (p, make_fun_exp rest body)
          in
          RecDecl (f, x, make_fun_exp rest e)
    }
  | LET REC f=ID EQ FUN x=ID RARROW e=Expr { RecDecl (f, x, e) }

Expr :
    e=IfExpr { e }
  | e=LetExpr { e } (* 3.3.1 *)
  | e=OrExpr { e }
  | e=ConsExpr { e } (* 3.6.2 *)
  | e=FunExpr { e }
  | e=LetRecExpr { e }  (* 3.5.1 *)

(* 3.6.2 Cons演算子の処理を追加 *)
ConsExpr :
    l=OrExpr CONS r=ConsExpr { BinOp (Cons, l, r) }  (* 3.6.2 右結合 *)
  | e=OrExpr { e } 

FunExpr :
    FUN params=ParamList RARROW e=Expr { 
      (* 3.4.3 fun x1 x2 ... xn -> e を fun x1 -> fun x2 -> ... -> fun xn -> e に変換 *)
      let rec make_fun_exp params body = match params with
        | [] -> body
        | p :: rest -> FunExp (p, make_fun_exp rest body)
      in
      make_fun_exp params e
    } (* 3.4.1  複数パラメータ対応 *)

OrExpr :
    l=OrExpr OROR r=AndExpr { BinOp (Or, l, r) } (* 3.2.3 *)
  | e=AndExpr { e }

AndExpr :
    l=AndExpr ANDAND r=LTExpr { BinOp (And, l, r) } (* 3.2.3 *)
  | e=LTExpr { e }

LTExpr :
    l=PExpr LT r=PExpr { BinOp (Lt, l, r) }
  | e=PExpr { e }

LetExpr :
    LET x=ID params=ParamList EQ e1=Expr IN e2=Expr { 
      let rec make_fun_exp params body = match params with
        | [] -> body
        | p :: rest -> FunExp (p, make_fun_exp rest body)
      in
      LetExp (x, make_fun_exp params e1, e2) 
    } (* 3.3.1 複数パラメータ対応 *)
  | LET x=ID EQ e1=Expr IN e2=Expr { LetExp (x, e1, e2) } (* 3.3.1 従来の記法 *)

PExpr :
    l=PExpr PLUS r=CONCATExpr { BinOp (Plus, l, r) }
  | e=Mxpr { e }

CONCATExpr :
    l=CONCATExpr CONCAT r=MExpr { StrConcatExp (l, r) } (* 文字列連結 *)
  | e=MExpr { e } (* 文字列連結がない場合はそのまま *)

MExpr :
    l=MExpr MULT r=AppExpr { BinOp (Mult, l, r) }
  | e=AppExpr { e } (* 3.4.1 *)

AppExpr :
    e1=AppExpr e2=AExpr { AppExp (e1, e2) } (* 3.4.1 *)
  | e1 = AExpr DOT_LBRACKET e2=EExpr RBRACKET { StrGetExp (e1, e2) } 
  | PRINT_STRING e=AExpr { PrintStringExp e } 
  | e=AExpr { e }

LetRecExpr :
    LET REC f=ID params=ParamList EQ e1=Expr IN e2=Expr {
      match params with
        | [] -> failwith "Recursive function must have at least one parameter"
        | x :: rest -> 
          let rec make_fun_exp params body = match params with
            | [] -> body
            | p :: rest -> FunExp (p, make_fun_exp rest body)
          in
          LetRecExp (f, x, make_fun_exp rest e1, e2)
    } (* 3.5.1 複数パラメータ対応 *)
  | LET REC f=ID EQ FUN x=ID RARROW e1=Expr IN e2=Expr { LetRecExp (f, x, e1, e2) } (* 3.5.1 従来の記法 *)

AExpr :
    i=INTV { ILit i }
  | TRUE   { BLit true }
  | FALSE  { BLit false }
  | i=ID   { Var i }
  | LPAREN e=Expr RPAREN { e }
  | e=ListExpr { e } (* 3.6.2 リスト式をAExprに追加 *)
  | s=STRINGV { SLit s } 

(* 3.6.2 リスト式の構文規則 *)
ListExpr :
    LBRACKET RBRACKET { ListExp [] } (* 3.6.2 空リスト *)
  | LBRACKET elems=ListElements RBRACKET { 
      (* 3.6.2 リスト要素をListExpに変換 *)
      ListExp elems 
    }

(* 3.6.2 リスト要素の解析（セミコロン区切り） *)
ListElements :
    e=Expr { [e] } (* 単一要素 *)
  | e=Expr SEMICOLON elems=ListElements { e :: elems } (* 3.6.2 複数要素 *)

IfExpr :
    IF c=Expr THEN t=Expr ELSE e=Expr { IfExp (c, t, e) }