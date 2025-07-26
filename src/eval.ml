open Syntax

type exval =
    IntV of int
  | BoolV of bool
  | ProcV of id * exp * dnval Environment.t ref (* 3.4.1 クロージャが作成された時点の環境をデータ構造に含めている．& 3.5.1 Changed! 関数閉包内の環境を参照型で保持するように変更 *)
  | ConsV of exval * exval (* 3.6.2 cons cell: (head, tail) *)
  | NilV (* 3.6.2 空リスト *)
and dnval = exval

exception Error of string

let err s = raise (Error s)

(* pretty printing *)
let rec string_of_exval = function
    IntV i -> string_of_int i
  | BoolV b -> string_of_bool b
  | ProcV (_, _, _) -> "<fun>"
  | NilV -> "[]" (* 3.6.2 空リストの文字列表現 *)
  | ConsV (head, tail) ->   (* 3.6.2 cons cellの文字列表現 *)
      let rec list_to_string acc = function
        | NilV -> "[" ^ String.concat "; " (List.rev acc) ^ "]"
        | ConsV (h, t) -> list_to_string (string_of_exval h :: acc) t
        | _ -> (* 3.6.2 不正なリスト構造の場合 *)
            let tail_str = string_of_exval tail in
            "[" ^ String.concat "; " (List.rev acc) ^ " | " ^ tail_str ^ "]"
      in
      list_to_string [string_of_exval head] tail

let pp_val v = print_string (string_of_exval v)

let rec apply_prim op arg1 arg2 = match op, arg1, arg2 with
  Plus, IntV i1 , IntV i2 -> IntV (i1 + i2)
  | Plus, _, _ -> err ("Both arguments must be integer: +")
  | Mult, IntV i1, IntV i2 -> IntV (i1 * i2)
  | Mult, _, _ -> err ("Both arguments must be integer: *")
  | Lt, IntV i1, IntV i2 -> BoolV (i1 < i2)
  | Lt, _, _ -> err ("Both arguments must be integer: <")


let rec eval_exp env = function
    Var x ->
    (try Environment.lookup x env with
       Environment.Not_bound -> err ("Variable not bound: " ^ x))
  | ILit i -> IntV i
  | BLit b -> BoolV b
  | ListExp exps -> 
      (* 3.6.2 リスト内の各式を評価してcons cellsでリスト値を作成 *)
      let vals = List.map (eval_exp env) exps in
      let rec build_list = function
        | [] -> NilV
        | h :: t -> ConsV (h, build_list t)
      in
      build_list vals
  | BinOp (op, exp1, exp2) ->
     (match op with
      | And ->
        let arg1 = eval_exp env exp1 in
        (match arg1 with
          | BoolV false -> BoolV false  (* 3.2.3 短絡評価: 左辺がfalseなら右辺を評価しない *)
          | BoolV true -> 
            let arg2 = eval_exp env exp2 in (* 3.2.3 第一引数がtrueであった場合、第二引数の評価結果を返す *)
            (match arg2 with BoolV b -> BoolV b | _ -> err "Both arguments must be boolean: &&")
          | _ -> err "Both arguments must be boolean: &&")
      | Or ->
        let arg1 = eval_exp env exp1 in
        (match arg1 with
          | BoolV true -> BoolV true   (* 3.2.3 短絡評価: 左辺がtrueなら右辺を評価しない *)
          | BoolV false -> 
            let arg2 = eval_exp env exp2 in (* 3.2.3  第一引数がfalseであった場合、第二引数の評価結果を返す *)
            (match arg2 with BoolV b -> BoolV b | _ -> err "Both arguments must be boolean: ||")
          | _ -> err "Both arguments must be boolean: ||")
      | Cons ->  (* 3.6.2 Cons演算子の処理を追加 *)
        let arg1 = eval_exp env exp1 in
        let arg2 = eval_exp env exp2 in
        ConsV (arg1, arg2)
      | _ ->  (* 3.2.3 その他の二項演算子は通常の評価を行う *)
        let arg1 = eval_exp env exp1 in
        let arg2 = eval_exp env exp2 in
        apply_prim op arg1 arg2)

      
  | IfExp (exp1, exp2, exp3) ->
    let test = eval_exp env exp1 in
    (match test with
       BoolV true -> eval_exp env exp2
     | BoolV false -> eval_exp env exp3
     | _ -> err ("Test expression must be boolean: if"))
  | LetExp (id, exp1, exp2) ->
     (* 3.3.1 現在の環境で exp1 を評価 *)
     let value = eval_exp env exp1 in
     (* 3.3.1 exp1 の評価結果を id の値として環境に追加して exp2 を評価 *)
     eval_exp (Environment.extend id value env) exp2
  | FunExp (id, exp) -> ProcV (id, exp, ref env)
  (* 3.4.1 関数適用式 *)
  | AppExp (exp1, exp2) ->
      (* 3.4.1 関数 exp1 を現在の環境で評価 *)
      let funval = eval_exp env exp1 in
      (* 3.4.1 実引数 exp2 を現在の環境で評価 *)
      let arg = eval_exp env exp2 in
      (* 3.4.1 関数 exp1 の評価結果をパターンマッチで取り出す *)
      (match funval with
          ProcV (id, body, env') -> (* 3.4.1 評価結果が実際にクロージャであれば *)
              (* 3.4.1 クロージャ内の環境を取り出して仮引数に対する束縛で拡張 *)
              let newenv = Environment.extend id arg !env' in
                eval_exp newenv body
        | _ ->
          (* 3.4.1 評価結果がクロージャでなければ，実行時型エラー *)
          err ("Non-function value is applied"))
 | LetRecExp (id, para, exp1, exp2) ->
    (* 3.5.1 ダミーの環境への参照を作る *)
    let dummyenv = ref Environment.empty in
    (* 3.5.1 関数閉包を作り，idをこの関数閉包に写像するように現在の環境envを拡張 *)
    let newenv = Environment.extend id (ProcV (para, exp1, dummyenv)) env in
    (* 3.5.1 ダミーの環境への参照に，拡張された環境を破壊的代入してバックパッチ *)
        dummyenv := newenv;
        eval_exp newenv exp2  
  

let rec eval_decl env = function
    Exp e -> let v = eval_exp env e in ("-", env, v) 
  | Decl (id, e) ->
      let v = eval_exp env e in (id, Environment.extend id v env, v)
  | RecDecl (id, para, e) ->
      let result = eval_exp env (LetRecExp (id, para, e, Var id)) in (* 3.5.1 RecDeclをLetRecExpの特殊ケースとして扱い、式レベルの再帰letとトップレベルの再帰宣言を統一 *)
      (id, Environment.extend id result env, result)
  | MultiDecl decls ->
    (* 3.3.2 複数の宣言を順次処理し、環境を累積的に更新 *)
    let rec process_decls current_env = function
      | [] -> ("", current_env, IntV 0)  (* 空の場合のデフォルト *)
      | decl :: rest ->
        let (id, new_env, v) = eval_decl current_env decl in
        if rest = [] then
          (* 3.3.2 最後の宣言の場合、その結果を返すが環境名は空文字列 *)
          ("", new_env, v)
        else
          (* 3.3.2 中間の宣言の場合、環境を更新して続行 *)
          process_decls new_env rest
    in
    process_decls env decls

(* 3.3.2 トップレベルの評価関数 *)
let eval_program env program =
  eval_decl env program