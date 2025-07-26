open Syntax
open Eval

exception Error of string

let err s = raise (Error s)

(* 4.2.1 Type Environment *)
type tyenv = ty Environment.t

(* 4.3.5 型代入を表す値の型 *)
type subst = (tyvar * ty) list

let rec subst_type (subst : (tyvar * ty) list) (ty : ty) : ty = (* 4.3.2 引数 subst は型変数と型のペアのリスト、引数 ty は置換対象の型、戻り値は置換後の型 *)
  let rec apply_subset (var, ty_subset) ty_target = (* 4.3.2 引数 (var, ty_subset) は1つの置換ペア、引数 ty_target は置換を適用する対象の型 *)
    match ty_target with
    | TyInt |TyBool -> ty_target (* 4.3.2 整数型または真偽値型の場合はそのまま返す *)
    | TyVar id ->
        (if id = var then ty_subset else TyVar id) (* 4.3.2 idが置換対象の型変数varと同じならty_subsetに置換、異なる場合は元の型変数TyVar idをそのまま返す *)
    | TyFun (ty1, ty2) ->
        TyFun (apply_subset (var, ty_subset) ty1, (* 4.3.2 引数型ty1と戻り値型ty2の両方に再帰的に置換を適用 *)
               apply_subset (var, ty_subset) ty2) 
    | TyList t -> 
        TyList (apply_subset (var, ty_subset) t) (* 4.3.2 要素型 t に再帰的に置換を適用 *) 
    in
    List.fold_left (fun acc_ty subst_pair -> apply_subset subst_pair acc_ty) ty subst (* 4.3.2 置換リストsubstを左から順に処理 *)
    | TyString -> TyString

(* 4.3.5 eqs_of_subst : subst -> (ty * ty) list
   型代入を型の等式集合に変換．型の等式制約 ty1 = ty2 は (ty1,ty2) という
   ペアで表現し，等式集合はペアのリストで表現 *)
let eqs_of_subst s = 
  List.map (fun (var, ty) -> (TyVar var, ty)) s 

(* 4.3.5 subst_eqs: subst -> (ty * ty) list -> (ty * ty) list
   型の等式集合に型代入を適用する関数 *)
let subst_eqs s eqs = 
  List.map (fun (l, r) -> (subst_type s l, subst_type s r)) eqs 


let rec freevars ty =
  match ty with
  | TyInt | TyBool -> [] (* 4.3.3 TyInt や TyBool には型変数は含まれないので空リスト [] を返す *)
  | TyVar id -> [id] (* 4.3.3 型変数 TyVar id が出てきたら、その変数名 id をリストにして返す *)
  | TyFun (t1, t2) -> freevars t1 @ freevars t2 (* 4.3.3 関数型 t1 -> t2 の場合、それぞれの型の自由変数を再帰的に求めて結合する *)
  | TyList t -> freevars t (* 4.3.3 リスト型 t list の要素型の自由変数を求める *)

let rec unify eqs = (* 4.3.3 制約リスト eqs が空かどうか、またはペアの形を見てパターンマッチする *)
  match eqs with
  | [] -> [] (* 4.3.3 制約リストが空ならば空リストを返す *)
  | (TyInt, TyInt) :: rest 
  | (TyBool, TyBool) :: rest ->
      unify rest (* 4.3.3 同じ型どうしの制約は解決済みなので削除して残りを処理 *)
  | (TyVar id, ty) :: rest
  | (ty, TyVar id) :: rest -> (* 4.3.3 型変数と他の型のペアの場合 *)
      if ty = TyVar id then unify rest (* 4.3.3 型変数と同じ型ならば、制約リストからそのペアを削除して再帰的に処理 *)
      else if List.mem id (freevars ty) then
        raise (Error "occur check") (* 4.3.3 型変数が他の型に現れる場合はエラーを発生させる(オカーチェック) *)
      else
        let rest' = List.map (fun (l, r) -> (subst_type [(id, ty)] l, subst_type [(id, ty)] r)) rest in (* 4.3.3 残りの制約リストの各ペアに対して、型変数 id を ty に置換 *)
        (id, ty) :: unify rest'
  | (TyFun (a1, a2), TyFun (b1, b2)) :: rest -> (* 4.3.3 関数型のペアの場合 *)
      unify ((a1, b1) :: (a2, b2) :: rest) (* 4.3.3 関数型の引数と戻り値の型をペアにして残りの制約リストに追加 *)
  | (TyList ty1, TyList ty2) :: rest -> (* 4.3.3 リスト型のペアの場合 *)
      unify ((ty1, ty2) :: rest)  (* 4.3.3 リスト型の要素型をペアにして残りの制約リストに追加 *)
  | (TyString, TyString) :: rest -> 
      unify rest 
  | _ -> raise (Error "unification failed")

  (* 4.3.5 型環境 tyenv と式 exp を受け取って，型代入と exp の型のペアを返す *)
let ty_prim op ty1 ty2 = match op with
  | Plus -> ([(ty1, TyInt); (ty2, TyInt)], TyInt)
  | Mult -> ([(ty1, TyInt); (ty2, TyInt)], TyInt)
  | Lt -> ([(ty1, TyInt); (ty2, TyInt)], TyBool)
  | And -> ([(ty1, TyBool); (ty2, TyBool)], TyBool)
  | Or -> ([(ty1, TyBool); (ty2, TyBool)], TyBool)

(* 4.3.5 型環境 tyenv と式 exp を受け取って，型代入と exp の型のペアを返す *)
let rec ty_exp tyenv exp =
  match exp with
  | Var x ->
      (try ([], Environment.lookup x tyenv) with (* 4.3.5 tyenv から変数 x の型を探し，なければエラー *)
          Environment.Not_bound -> err ("variable not bound: " ^ x))
  | ILit _ -> ([], TyInt)   (* 4.3.5 整数リテラルなら型は TyInt *)
  | BLit _ -> ([], TyBool)    (* 4.3.5 真偽値リテラルなら型は TyBool *)
  | BinOp (op, exp1, exp2) ->
      let (s1, ty1) = ty_exp tyenv exp1 in (* 4.3.5 exp1 の型推論を行う *)
      let (s2, ty2) = ty_exp tyenv exp2 in  (* 4.3.5 exp2 の型推論を行う *)
      let (eqs3, ty) = ty_prim op ty1 ty2 in    (* 4.3.5 演算子 op に対して型制約と結果型を得る *)
      let eqs = (eqs_of_subst s1) @ (eqs_of_subst s2) @ eqs3 in   (* 4.3.5 s1 と s2 を等式制約の集合に変換し，eqs3 と合わせて全制約を得る *)
      let s3 = unify eqs in    (* 4.3.5 全制約を単一化し，型代入 s3 を得る *)
      (s3, subst_type s3 ty)  (* 4.3.5 型代入 s3 と，それを適用した結果型 ty を返す *)
  | IfExp (exp1, exp2, exp3) -> 
      let (s1, ty1) = ty_exp tyenv exp1 in (* 4.3.5 条件式 exp1 の型推論 *)
      let (s2, ty2) = ty_exp tyenv exp2 in (* 4.3.5 then 節 exp2 の型推論 *)
      let (s3, ty3) = ty_exp tyenv exp3 in (* 4.3.5 else 節 exp3 の型推論 *)
      (* 4.3.5 各代入 s1,s2,s3 を制約集合に変換し，型制約を追加 *)
      let eqs = (eqs_of_subst s1) @ (eqs_of_subst s2) @ (eqs_of_subst s3) @  
                [(ty1, TyBool); (ty2, ty3)] in
      let s4 = unify eqs in (* 4.3.5 全制約を単一化し，型代入 s4 を得る *)
      (s4, subst_type s4 ty2) (* 4.3.5 型代入 s4 と，それを適用した then 節の型 ty2 を返す *)
  | LetExp (id, exp1, exp2) ->
      let (s1, ty1) = ty_exp tyenv exp1 in (* 4.3.5 exp1 の型推論を行い，型代入 s1 と型 ty1 を得る *)
      let new_tyenv = Environment.extend id (subst_type s1 ty1) tyenv in (* 4.3.5 s1 を型環境に適用してから，id に ty1 を関連付けた環境を作成 *)
      let (s2, ty2) = ty_exp new_tyenv exp2 in (* 4.3.5 拡張した環境のもとで exp2 の型推論を行い，型代入 s2 と型 ty2 を得る *)
      let eqs = (eqs_of_subst s1) @ (eqs_of_subst s2) in (* 4.3.5 s1 と s2 を制約集合に変換し，まとめて単一化する *)
      let s3 = unify eqs in
      (s3, subst_type s3 ty2) (* 4.3.5 型代入 s3 と，それを適用した型 ty2 を返す *)
  | FunExp (id, exp) ->
      let domty = TyVar (fresh_tyvar ()) in  (* 4.3.5 引数 id の型として新しい型変数を生成 *)
      let s, ranty = (* 4.3.5 id:domty を型環境に追加し，そのもとで本体 exp の型推論 *)
        ty_exp (Environment.extend id domty tyenv) exp in
      (s, TyFun (subst_type s domty, ranty)) (* 4.3.5 型代入 s と，関数型 (引数の型 → 本体の型) を返す *)
  | AppExp (exp1, exp2) ->
      let (s1, ty1) = ty_exp tyenv exp1 in (* 4.3.5 関数部 exp1 の型推論 *)
      let (s2, ty2) = ty_exp tyenv exp2 in (* 4.3.5 引数部 exp2 の型推論 *)
      let ranty = TyVar (fresh_tyvar ()) in (* 4.3.5 関数の戻り値の型として新しい型変数を生成 *)
      let eqs = (eqs_of_subst s1) @ (eqs_of_subst s2) @ (* 4.3.5 s1, s2 の型代入を制約集合に変換し，関数型 (ty2 → ranty) と ty1 が一致する制約を追加 *)
                [(ty1, TyFun (ty2, ranty))] in
      let s3 = unify eqs in (* 4.3.5 全制約を単一化し，型代入 s3 を得る *)
      (s3, subst_type s3 ranty) (* 4.3.5 型代入 s3 と，それを適用した戻り値型 ranty を返す *)
  | SLit _ -> ([], TyString) 
  | StrConcatExp (exp1, exp2) -> (* 文字列連結の型推論 *)
      let (s1, ty1) = ty_exp tyenv exp1 in (* 4.3.5 exp1 の型推論 *)
      let (s2, ty2) = ty_exp tyenv exp2 in (* 4.3.5 exp2 の型推論 *)
      let eqs = (eqs_of_subst s1) @ (eqs_of_subst s2) @ (* 4.3.5 s1, s2 の型代入を制約集合に変換し，両方が文字列型である制約を追加 *)
                [(ty1, TyString); (ty2, TyString)] in
      let s3 = unify eqs in (* 4.3.5 全制約を単一化し，型代入 s3 を得る *)
      (s3, TyString) (* 4.3.5 型代入 s3 と、結果の型 TyString を返す *)
  | StrGetEXP (exp1, exp2) -> (* 文字列のインデックス取得の型推論 *)
      let (s1, ty1) = ty_exp tyenv exp1 in (* 4.3.5 exp1 の型推論 *)
      let (s2, ty2) = ty_exp tyenv exp2 in (* 4.3.5 exp2 の型推論 *)
      let eqs = (eqs_of_subst s1) @ (eqs_of_subst s2) @ (* 4.3.5 s1, s2 の型代入を制約集合に変換し，exp1 が文字列型、exp2 が整数型である制約を追加 *)
                [(ty1, TyString); (ty2, TyInt)] in
      let s3 = unify eqs in (* 4.3.5 全制約を単一化し，型代入 s3 を得る *)
      (s3, TyString) 
  | PrintStrExp exp -> (* 文字列の出力の型推論 *)
      let (s, ty) = ty_exp tyenv exp in (* 4.3.5 exp の型推論 *)
      let ty_res  = TyVar (fresh_tyvar ()) in (* 4.3.5 出力の戻り値の型として新しい型変数を生成 *)
      let eqs = (eqs_of_subst s) @ [(ty, TyString)] in
      let s2 = unify eqs in (* 4.3.5 全制約を単一化し，型代入 s2 を得る *)
      (s2, TyString) (* 4.3.5 型代入 s
  | _ -> err "Not Implemented!"


let ty_decl tyenv = function
  | Exp e ->   
      let (s, ty) = ty_exp tyenv e in (* 4.3.5 ty_expが型代入sと型tyのペアを返す *)
      (subst_type s ty, tyenv) (* 4.3.5 subst_type s tyで型代入を適用して最終的な型を計算 *)
  | Decl (id, e) -> 
      let (s, ty) = ty_exp tyenv e in 
      let final_ty = subst_type s ty in
      let new_tyenv = Environment.extend id final_ty tyenv in
      (final_ty, new_tyenv)
  | _ -> err "Not Implemented!"