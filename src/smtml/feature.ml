let of_unop (unop : Ty.Unop.t) : Feature_map.feat =
  match unop with
  | Neg -> Neg
  | Not -> Not
  | Clz -> Clz
  | Ctz -> Ctz
  | Popcnt -> Popcnt
  (* Float *)
  | Abs -> Abs
  | Sqrt -> Sqrt
  | Is_normal -> Is_normal
  | Is_subnormal -> Is_subnormal
  | Is_negative -> Is_negative
  | Is_positive -> Is_positive
  | Is_infinite -> Is_infinite
  | Is_nan -> Is_nan
  | Is_zero -> Is_zero
  | Ceil -> Ceil
  | Floor -> Floor
  | Trunc -> Trunc
  | Nearest -> Nearest
  | Head -> Head
  | Tail -> Tail
  | Reverse -> Reverse
  | Length -> Length
  (* String *)
  | Trim -> Trim
  (* RegExp *)
  | Regexp_star -> Regexp_star
  | Regexp_loop _ -> Regexp_loop
  | Regexp_plus -> Regexp_plus
  | Regexp_opt -> Regexp_opt
  | Regexp_comp -> Regexp_comp
  | Rotl _ -> Rotl
  | Rotr _ -> Rotr

let of_binop (binop : Ty.Binop.t) : Feature_map.feat =
  match binop with
  | Add -> Add
  | Sub -> Sub
  | Mul -> Mul
  | Div -> Div
  | DivU -> DivU
  | Rem -> Rem
  | RemU -> RemU
  | Shl -> Shl
  | ShrA -> ShrA
  | ShrL -> ShrL
  | And -> And
  | Or -> Or
  | Xor -> Xor
  | Implies -> Implies
  | Pow -> Pow
  | Min -> Min
  | Max -> Max
  | Copysign -> Copysign
  | Ext_rotl -> Ext_rotl
  | Ext_rotr -> Ext_rotr
  | At -> At
  | List_cons -> List_cons
  | List_append -> List_append
  (* Array *)
  | Select -> Select
  (* String *)
  | String_prefix -> String_prefix
  | String_suffix -> String_suffix
  | String_contains -> String_contains
  | String_last_index -> String_last_index
  | String_in_re -> String_in_re
  (* Regexp *)
  | Regexp_range -> Regexp_range
  | Regexp_inter -> Regexp_inter
  | Regexp_diff -> Regexp_diff
  | Mod -> Mod

let of_triop (triop : Ty.Triop.t) : Feature_map.feat =
  match triop with
  | Ite -> Ite
  | List_set -> List_set
  (* Array *)
  | Store -> Store
  (* String *)
  | String_extract -> String_extract
  | String_replace -> String_replace
  | String_index -> String_index
  | String_replace_all -> String_replace_all
  | String_replace_re -> String_replace_re
  | String_replace_re_all -> String_replace_re_all

let of_relop (relop : Ty.Relop.t) : Feature_map.feat =
  match relop with
  | Eq -> Eq
  | Ne -> Ne
  | Lt -> Lt
  | LtU -> LtU
  | Le -> Le
  | LeU -> LeU

let of_cvtop (cvtop : Ty.Cvtop.t) : Feature_map.feat =
  match cvtop with
  | ToString -> ToString
  | OfString -> OfString
  | ToBool -> ToBool
  | OfBool -> OfBool
  | Reinterpret_int -> Reinterpret_int
  | Reinterpret_float -> Reinterpret_float
  | DemoteF64 -> DemoteF64
  | PromoteF32 -> PromoteF32
  | ConvertSI32 -> ConvertSI32
  | ConvertUI32 -> ConvertUI32
  | ConvertSI64 -> ConvertSI64
  | ConvertUI64 -> ConvertUI64
  | TruncSF32 -> TruncSF32
  | TruncUF32 -> TruncUF32
  | TruncSF64 -> TruncSF64
  | TruncUF64 -> TruncUF64
  | Trunc_sat_f32_s -> Trunc_sat_f32_s
  | Trunc_sat_f32_u -> Trunc_sat_f32_u
  | Trunc_sat_f64_s -> Trunc_sat_f64_s
  | Trunc_sat_f64_u -> Trunc_sat_f64_u
  | WrapI64 -> WrapI64
  | Sign_extend _ -> Sign_extend
  | Zero_extend _ -> Zero_extend
  (* String *)
  | String_to_code -> String_to_code
  | String_from_code -> String_from_code
  | String_to_int -> String_to_int
  | String_from_int -> String_from_int
  | String_to_float -> String_to_float
  | String_to_re -> String_to_re

let of_ty (ty : Ty.t) : Feature_map.feat =
  match ty with
  | Ty_app -> Ty_app
  | Ty_array _ -> Ty_array
  | Ty_bitv _ -> Ty_bitv
  | Ty_bool -> Ty_bool
  | Ty_fp _ -> Ty_fp
  | Ty_int -> Ty_int
  | Ty_list -> Ty_list
  | Ty_none -> Ty_none
  | Ty_real -> Ty_real
  | Ty_str -> Ty_str
  | Ty_unit -> Ty_unit
  | Ty_regexp -> Ty_regexp
  | Ty_roundingMode -> Ty_roundingMode

let of_width (ty : Ty.t) : Feature_map.feat option =
  match ty with
  | Ty_bitv 1 -> Some Bitv_1
  | Ty_bitv 8 -> Some Bitv_8
  | Ty_bitv 16 -> Some Bitv_16
  | Ty_bitv 32 -> Some Bitv_32
  | Ty_bitv 64 -> Some Bitv_64
  | Ty_bitv 128 -> Some Bitv_128
  | Ty_bitv _ -> Some Bitv_other
  | Ty_fp 32 -> Some Fp_32
  | Ty_fp 64 -> Some Fp_64
  | Ty_fp _ -> Some Fp_other
  | Ty_app | Ty_array _ | Ty_bool | Ty_int | Ty_list | Ty_none | Ty_real
  | Ty_str | Ty_unit | Ty_regexp | Ty_roundingMode ->
    None

let ty_param (ty : Ty.t) : Feature_map.ty_param =
  match ty with
  | Ty_bool -> Bool
  | Ty_int -> Int
  | Ty_real -> Real
  | Ty_bitv _ -> Bitv
  | Ty_fp _ -> Fp
  | Ty_str -> Str
  | Ty_app | Ty_array _ | Ty_list | Ty_none | Ty_unit | Ty_regexp
  | Ty_roundingMode ->
    Other

let all_ty_params : Feature_map.ty_param list =
  [ Bool; Int; Real; Bitv; Fp; Str; Other ]

(* Polymorphic operators which need to be differentiated by their type param *)
let typed_ops : Feature_map.feat list =
  [ (* Unops *)
    Neg
  ; Not
  ; Abs
  ; Sqrt
  ; Ceil
  ; Floor
  ; (* Binops *)
    Add
  ; Sub
  ; Mul
  ; Div
  ; Rem
  ; Pow
  ; Min
  ; Max
  ; And
  ; Or
  ; Xor
  ; (* Triops *)
    Ite
  ; (* Relops *)
    Eq
  ; Ne
  ; Lt
  ; Le
  ; (* Naryops *)
    Distinct
  ]

let all_typed_feats =
  List.concat_map
    (fun op ->
      List.map (fun ty_param -> Feature_map.Typed (op, ty_param)) all_ty_params )
    typed_ops

let of_op (ty : Ty.t) (op : Feature_map.feat) : Feature_map.feat list =
  let typed =
    if List.mem op typed_ops then [ Feature_map.Typed (op, ty_param ty) ]
    else []
  in
  match of_width ty with
  | Some width -> op :: of_ty ty :: width :: typed
  | None -> op :: of_ty ty :: typed

let of_naryop (naryop : Ty.Naryop.t) : Feature_map.feat =
  match naryop with
  | Logand -> Logand
  | Logor -> Logor
  | Concat -> Concat_nary
  | Regexp_union -> Regexp_union
  | Distinct -> Distinct

let of_expr_kind (e : Expr.expr) _ty : Feature_map.feat =
  match e with
  | Val _ -> Val
  | Ptr _ -> Ptr
  | Symbol _ -> Symbol
  | List _ -> List_expr
  | App _ -> App
  | Unop _ -> Unop_expr
  | Binop _ -> Binop_expr
  | Triop _ -> Triop_expr
  | Relop _ -> Relop_expr
  | Cvtop _ -> Cvtop_expr
  | Naryop _ -> Naryop_expr
  | Extract _ -> Extract
  | Concat _ -> Concat
  | Binder _ -> Binder

let all_feats : Feature_map.feat array =
  Array.append
    Feature_map.
      [| (* Metadata *)
         Max_depth
       ; Mean_depth
       ; Nb_queries
       ; Time
       ; (* Expr kinds *)
         Val
       ; Ptr
       ; Symbol
       ; List_expr
       ; App
       ; Unop_expr
       ; Binop_expr
       ; Triop_expr
       ; Relop_expr
       ; Cvtop_expr
       ; Naryop_expr
       ; Extract
       ; Concat
       ; Binder
       ; (* Unops *)
         Neg
       ; Not
       ; Clz
       ; Ctz
       ; Popcnt
       ; Abs
       ; Sqrt
       ; Is_normal
       ; Is_subnormal
       ; Is_negative
       ; Is_positive
       ; Is_infinite
       ; Is_nan
       ; Is_zero
       ; Ceil
       ; Floor
       ; Trunc
       ; Nearest
       ; Head
       ; Tail
       ; Reverse
       ; Length
       ; Trim
       ; Regexp_star
       ; Regexp_loop
       ; Regexp_plus
       ; Regexp_opt
       ; Regexp_comp
       ; (* Binops *)
         Add
       ; Sub
       ; Mul
       ; Div
       ; DivU
       ; Rem
       ; RemU
       ; Mod
       ; Shl
       ; ShrA
       ; ShrL
       ; And
       ; Or
       ; Xor
       ; Implies
       ; Pow
       ; Min
       ; Max
       ; Copysign
       ; Rotl
       ; Rotr
       ; Ext_rotl
       ; Ext_rotr
       ; At
       ; List_cons
       ; List_append
       ; Select
       ; String_prefix
       ; String_suffix
       ; String_contains
       ; String_last_index
       ; String_in_re
       ; Regexp_range
       ; Regexp_inter
       ; Regexp_diff
       ; (* Triops *)
         Ite
       ; List_set
       ; Store
       ; String_extract
       ; String_replace
       ; String_index
       ; String_replace_all
       ; String_replace_re
       ; String_replace_re_all
       ; (* Relops *)
         Eq
       ; Ne
       ; Lt
       ; LtU
       ; Le
       ; LeU
       ; (* Cvtops *)
         ToString
       ; OfString
       ; ToBool
       ; OfBool
       ; Reinterpret_int
       ; Reinterpret_float
       ; DemoteF64
       ; PromoteF32
       ; ConvertSI32
       ; ConvertUI32
       ; ConvertSI64
       ; ConvertUI64
       ; TruncSF32
       ; TruncUF32
       ; TruncSF64
       ; TruncUF64
       ; Trunc_sat_f32_s
       ; Trunc_sat_f32_u
       ; Trunc_sat_f64_s
       ; Trunc_sat_f64_u
       ; WrapI64
       ; Sign_extend
       ; Zero_extend
       ; String_to_code
       ; String_from_code
       ; String_to_int
       ; String_from_int
       ; String_to_float
       ; String_to_re
       ; (* Naryops *)
         Logand
       ; Logor
       ; Regexp_union
       ; Distinct
       ; Concat_nary
       ; (* Types *)
         Ty_app
       ; Ty_array
       ; Ty_bitv
       ; Ty_bool
       ; Ty_fp
       ; Ty_int
       ; Ty_list
       ; Ty_none
       ; Ty_real
       ; Ty_str
       ; Ty_unit
       ; Ty_regexp
       ; Ty_roundingMode
       ; (* Widths *)
         Bitv_1
       ; Bitv_8
       ; Bitv_16
       ; Bitv_32
       ; Bitv_64
       ; Bitv_128
       ; Bitv_other
       ; Fp_32
       ; Fp_64
       ; Fp_other
      |]
    (Array.of_list all_typed_feats)

let all_feature_names =
  "solver" :: "model"
  :: Array.fold_right
       (fun feat acc -> Feature_map.feat_to_string feat :: acc)
       all_feats []

let feats_to_str solver_name model feats =
  let values =
    solver_name :: Bool.to_string model
    :: Array.fold_right
         (fun feat acc -> string_of_int (Feature_map.get_feat feat feats) :: acc)
         all_feats []
  in
  String.cat (String.concat "," values) "\n"
