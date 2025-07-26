open Eval
open Typing
open Syntax

let rec read_eval_print env tyenv = (* 4.2.1 型環境を REPL(Read-Eval-Print Loop) で保持 *)
  print_string "# "; (* 4.2.1 # というプロンプトを表示 *)
  flush stdout; (* 4.2.1 確実に画面に出力 *)
  let decl = Parser.toplevel Lexer.main (Lexing.from_channel stdin) in (* 4.2.1 標準入力から文字列を読み取り、字句解析、構文解析を行う *)
  let (ty, new_tyenv) = ty_decl tyenv decl in  (* 4.2.1 型情報と新しい型環境の両方を受け取る *)
  let (id, newenv, v) = eval_decl env decl in (* 4.2.1 eval_decl関数で宣言を実行、戻り値は、(識別子名, 新しい値環境, 計算結果) である *)
  Printf.printf "val %s : " id;
  pp_ty ty;  (* 4.2.1 タプルの最初の要素（ty）を使用 *)
  print_string " = ";
  pp_val v;   (* 4.2.1 val 変数名 : 型 = 値 の形式で結果を表示 *)
  print_newline();
  read_eval_print newenv new_tyenv  (* 4.2.1 更新された環境で次の入力を待つ、これはlet宣言に対応している *)

let initial_env =
  Environment.extend "i" (IntV 1)
    (Environment.extend "v" (IntV 5)
       (Environment.extend "x" (IntV 10)
          (Environment.extend "ii" (IntV 2) (* 3.2.1 *)
             (Environment.extend "iii" (IntV 3)
                (Environment.extend "iv" (IntV 4) Environment.empty)))))

let initial_tyenv =
  Environment.extend "i" TyInt
    (Environment.extend "v" TyInt
       (Environment.extend "x" TyInt
          (Environment.extend "ii" TyInt
             (Environment.extend "iii" TyInt
                (Environment.extend "iv" TyInt Environment.empty)))))