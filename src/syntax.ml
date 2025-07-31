open MySet
(* ML interpreter / type reconstruction *)
type id = string

type binOp = Plus | Mult | Lt | And | Or | Cons (* 3.6.2 新しい二項演算子を追加 *)

type exp =
    Var of id
  | ILit of int
  | BLit of bool
  | BinOp of binOp * exp * exp
  | IfExp of exp * exp * exp
  | LetExp of id * exp * exp (* 3.3.1 exp型に新しいコンストラクタを追加 *)
  | FunExp of id * exp (* 3.4.1 *)
  | AppExp of exp * exp (* 3.4.1 *)
  | LetRecExp of id * id * exp * exp (* 3.5.1 *)
  | ListExp of exp list (* 3.6.2 *)
  | SLit of string
  | StrConcatExp of exp * exp 
  | StrGetExp of exp * exp 
  | PrintStrExp of exp 
  | PairExp of exp * exp
  | Proj1Exp of exp
  | Proj2Exp of exp


type program =
    Exp of exp
  | Decl of id * exp (* 3.3.1 program型に新しいコンストラクタを追加 *)
  | RecDecl of id * id * exp (* 3.5.1 *)
  | MultiDecl of program list (* 3.3.2 *)


type tyvar = int (* 4.3.1  型変数の識別子を整数で表現 *)

  (* 4.2.1 MiniML の型を表す OCaml の値の型 *)
type ty =
    TyInt
  | TyBool
  | TyVar of tyvar (* 4.3.1 型変数型を表すコンストラクタ *)
  | TyFun of ty * ty (* 4.3.1 TyFun(t1,t2) は関数型 t1 -> t2 を表す *)
  | TyList of ty  (* 4.3.1 *)
  | TyString  (* 文字列型を表すコンストラクタ *)
  | TyPair of ty * ty (* ペア型を表すコンストラクタ *)


let fresh_tyvar = (* 4.3.1 呼び出すたびに，他とかぶらない新しい tyvar 型の値を返す関数 *)
  let counter = ref 0 in  (* 4.3.1 次に返すべき tyvar 型の値を参照で持っておく *)
  let body () =
    let v = !counter in
      counter := v + 1; v  (* 4.3.1 呼び出されたら参照をインクリメントして，古い counter の参照先の値を返す *)
  in body

let string_of_tyvar n = (* 4.3.1 型変数の識別子を文字列に変換する関数 *)
  let base = Char.code 'a' in (* 4.3.1 文字 'a' の ASCII コード（97）を取得 *)
  let letter = Char.chr (base + (n mod 26)) in (* 4.3.1 n mod 26 で 0-25 の範囲に正規化 *)
  let suffix = if n < 26 then "" else string_of_int (n / 26) in (* 4.3.1 n が 26 未満なら数字サフィックスなし、n が 26 以上なら n / 26 を数字として追加 *)
  "'" ^ String.make 1 letter ^ suffix (* 4.3.1 最終的な文字列を構築 *)

let rec string_of_ty = function (* 4.3.1 型の内部表現を人間が読める文字列に変換 *)
  | TyInt -> "int" (* 4.3.1 基本型 int をそのまま "int" として返す *)
  | TyBool -> "bool" (* 4.3.1 基本型 bool をそのまま "bool" として返す *)
  | TyVar n -> string_of_tyvar n (* 4.3.1 型変数を string_of_tyvar を使って文字列化 *)
  | TyFun (t1, t2) -> (* 4.3.1 関数型 t1 -> t2 を括弧で囲んで表現し、再帰的に両方の型を文字列化 *)
      "(" ^ string_of_ty t1 ^ " -> " ^ string_of_ty t2 ^ ")"
  | TyList t -> (* 4.3.1 リスト型 t list を括弧で囲んで表現し、要素型を再帰的に文字列化 *)
      "(" ^ string_of_ty t ^ " list)" 
  | TyString -> "string"  (* 文字列型を "string" として返す *)
  | TyPair (t1, t2) -> (* ペア型 t1 * t2 を括弧で囲んで表現し、両方の型を再帰的に文字列化 *)
     "(" ^ string_of_ty t1 ^ " * " ^ string_of_ty t2 ^ ")"

let rec freevar_ty ty = (* 4.3.1 型中に現れる自由型変数（型変数）の集合を収集 *)
  match ty with
  | TyInt -> MySet.empty (* 4.3.1 基本型には型変数が含まれないので空集合を返す *)
  | TyBool -> MySet.empty (* 4.3.1 基本型には型変数が含まれないので空集合を返す *)
  | TyVar id -> MySet.singleton id (* 4.3.1  型変数そのものなので、その ID を含む単一要素集合を返す *)
  | TyFun (t1, t2) -> MySet.union (freevar_ty t1) (freevar_ty t2) (* 4.3.1 関数型では引数型と返り値型の両方から型変数を収集し、両方の結果の和集合を計算 *)
  | TyList t -> freevar_ty t (* 4.3.1 リスト型では要素型から型変数を収集 *) 
  | TyString -> MySet.empty  (* 文字列型には型変数が含まれないので空集合を返す *)
  | TyPair (t1, t2) ->  MySet.union (freevar_ty t1) (freevar_ty t2) (* ペア型では両方の要素型から型変数を収集し、和集合を計算 *)

  (* 4.2.1 ty 型の値のための pretty printer *)
let rec pp_ty typ =
  match typ with
    TyInt -> print_string "int" (* 4.2.1 整数型 *)
  | TyBool -> print_string "bool" (* 4.2.1 真偽値型 *)
  | TyVar n -> print_string (string_of_tyvar n) (* 4.3.1 型変数の識別子を文字列に変換 *)
  | TyFun (t1, t2) -> (* 4.3.1 関数型 t1 -> t2 を (t1 -> t2) として表示 *)
      print_string "(";
      pp_ty t1;
      print_string " -> ";
      pp_ty t2;
      print_string ")"
  | TyList t -> (* 4.3.1 リスト型 t list を (t list) として表示 *)
      print_string "(";
      pp_ty t;
      print_string " list)" 
    | TyPair (t1, t2) -> (* ペア型 t1 * t2 を (t1 * t2) として表示 *)
     print_string "(";
     pp_ty t1;
     print_string " * ";
     pp_ty t2;
     print_string ")"
  | TyString -> print_string "string"  (* 文字列型を "string" として表示 *)
