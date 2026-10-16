val of_unop : Ty.Unop.t -> Feature_map.feat

val of_binop : Ty.Binop.t -> Feature_map.feat

val of_triop : Ty.Triop.t -> Feature_map.feat

val of_relop : Ty.Relop.t -> Feature_map.feat

val of_cvtop : Ty.Cvtop.t -> Feature_map.feat

val of_ty : Ty.t -> Feature_map.feat

val of_naryop : Ty.Naryop.t -> Feature_map.feat

val of_op : Ty.t -> Feature_map.feat -> Feature_map.feat list

val add_nonlinear_mul :
  Ty.t -> Ty.Binop.t -> Expr.t -> Expr.t -> Feature_map.t -> Feature_map.t

val of_expr_kind : Expr.expr -> 'a -> Feature_map.feat

val feats_to_strings : Feature_map.t -> string list

val all_feature_names : string list
