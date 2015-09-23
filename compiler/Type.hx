import Ast;
enum Field_kind {
	Var(value: Var_kind);
	Method(value: Method_kind);
};
typedef Var_kind = {
	v_read : Var_access,
	v_write : Var_access
};
enum Var_access {
	AccNormal;
	AccNo;
	AccNever;
	AccResolve;
	AccCall;
	AccInline;
};
enum Method_kind {
	MethNormal;
	MethInline;
	MethDynamic;
	MethMacro;
};
enum T {
	TAnon(value: Tanon);
	TDynamic(value: T);
	TLazy(value: Ref < Unit -> T > );
};
enum Tconstant {
	TInt(value: Int32);
	TFloat(value: String);
	TString(value: String);
	TBool(value: Bool);
	TNull;
	TThis;
	TSuper;
};
typedef Tvar = {
	v_id : Int,
	v_name : String,
	v_type : T,
	v_capture : Bool,
	v_meta : Metadata
};
typedef Tfunc = {
	tf_type : T,
	tf_expr : Texpr
};
enum Anon_status {
	Closed;
	Opened;
	Const;
	Extend(value: List<T>);
	Statics(value: Tclass);
	EnumStatics(value: Tenum);
	AbstractStatics(value: Tabstract);
};
typedef Tanon = {
	a_status : Ref<Anon_status>
};
enum Texpr_expr {
	TConst(value: Tconstant);
	TLocal(value: Tvar);
	TTypeExpr(value: Module_type);
	TParenthesis(value: Texpr);
	TArrayDecl(value: List<Texpr>);
	TFunction(value: Tfunc);
	TBlock(value: List<Texpr>);
	TReturn(value: Option<Texpr>);
	TBreak;
	TContinue;
	TThrow(value: Texpr);
};
enum Tfield_access {
	FAnon(value: Tclass_field);
	FDynamic(value: String);
};
typedef Texpr = {
	eexpr : Texpr_expr,
	etype : T,
	epos : Ast.Pos
};
typedef Tclass_field = {
	cf_name : String,
	cf_type : T,
	cf_public : Bool,
	cf_pos : Pos,
	cf_doc : Ast.Documentation,
	cf_meta : Metadata,
	cf_kind : Field_kind,
	cf_params : Type_params,
	cf_expr : Option<Texpr>,
	cf_overloads : List<Tclass_field>
};
enum Tclass_kind {
	KNormal;
	KTypeParameter(value: List<T>);
	KExpr(value: Ast.Expr);
	KGeneric;
	KMacroType;
	KGenericBuild(value: List<Class_field>);
	KAbstractImpl(value: Tabstract);
};
typedef Metadata = Ast.Metadata;
typedef Tinfos = {
	mt_path : Path,
	mt_module : Module_def,
	mt_pos : Ast.Pos,
	mt_private : Bool,
	mt_doc : Ast.Documentation,
	mt_meta : Metadata,
	mt_params : Type_params
};
typedef Tclass = {
	cl_path : Path,
	cl_module : Module_def,
	cl_pos : Ast.Pos,
	cl_private : Bool,
	cl_doc : Ast.Documentation,
	cl_meta : Metadata,
	cl_params : Type_params,
	cl_kind : Tclass_kind,
	cl_extern : Bool,
	cl_interface : Bool,
	cl_ordered_statics : List<Tclass_field>,
	cl_ordered_fields : List<Tclass_field>,
	cl_dynamic : Option<T>,
	cl_array_access : Option<T>,
	cl_constructor : Option<Tclass_field>,
	cl_init : Option<Texpr>,
	cl_overrides : List<Tclass_field>,
	cl_build : Unit -> Bool,
	cl_restore : Unit -> Unit
};
typedef Tenum_field = {
	ef_name : String,
	ef_type : T,
	ef_pos : Ast.Pos,
	ef_doc : Ast.Documentation,
	ef_index : Int,
	ef_params : Type_params,
	ef_meta : Metadata
};
typedef Tenum = {
	e_path : Path,
	e_module : Module_def,
	e_pos : Ast.Pos,
	e_private : Bool,
	e_doc : Ast.Documentation,
	e_meta : Metadata,
	e_params : Type_params,
	e_type : Tdef,
	e_extern : Bool,
	e_constrs : PMap<String, Tenum_field>,
	e_names : List<String>
};
typedef Tdef = {
	t_path : Path,
	t_module : Module_def,
	t_pos : Ast.Pos,
	t_private : Bool,
	t_doc : Ast.Documentation,
	t_meta : Metadata,
	t_params : Type_params,
	t_type : T
};
typedef Tabstract = {
	a_path : Path,
	a_module : Module_def,
	a_pos : Ast.Pos,
	a_private : Bool,
	a_doc : Ast.Documentation,
	a_meta : Metadata,
	a_params : Type_params,
	a_impl : Option<Tclass>,
	a_this : T,
	a_from : List<T>,
	a_to : List<T>,
	a_array : List<Tclass_field>,
	a_resolve : Option<Tclass_field>
};
enum Module_type {
	TClassDecl(value: Tclass);
	TEnumDecl(value: Tenum);
	TTypeDecl(value: Tdef);
	TAbstractDecl(value: Tabstract);
};
typedef Module_def = {
	m_id : Int,
	m_path : Path,
	m_types : List<Module_type>,
	m_extra : Module_def_extra
};
typedef Module_def_extra = {
	m_file : String,
	m_sign : String,
	m_time : Float,
	m_dirty : Bool,
	m_added : Int,
	m_mark : Int,
	m_deps : PMap<Int, Module_def>,
	m_processed : Int,
	m_kind : Module_kind,
	m_binded_res : PMap<String, String>,
	m_macro_calls : List<String>,
	m_features : Hashtbl<String, Bool>
};
enum Module_kind {
	MCode;
	MMacro;
	MFake;
	MSub;
	MExtern;
};
enum Dt {
	DTGoto(value: Int);
	DTExpr(value: Texpr);
};
typedef Decision_tree = {
	dt_dt_lookup : Array<Dt>,
	dt_first : Int,
	dt_type : T,
	dt_is_complex : Bool
};
enum Unify_error {
	Invalid_visibility(value: String);
	Not_matching_optional(value: String);
	Cant_force_optional;
	Constraint_failure(value: String);
	Unify_custom(value: String);
};
enum Eq_kind {
	EqStrict;
	EqCoreType;
	EqRightDynamic;
	EqBothDynamic;
	EqDoNotFollowNull;
};
