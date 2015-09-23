typedef Pos = {
	pfile : String,
	pmin : Int,
	pmax : Int
};
enum Keyword {
	Function;
	Class;
	Var;
	If;
	Else;
	While;
	Do;
	For;
	Break;
	Continue;
	Return;
	Extends;
	Implements;
	Import;
	Switch;
	Case;
	Default;
	Static;
	Public;
	Private;
	Try;
	Catch;
	New;
	This;
	Throw;
	Extern;
	Enum;
	In;
	Interface;
	Untyped;
	Cast;
	Override;
	Typedef;
	Dynamic;
	Package;
	Inline;
	Using;
	Null;
	True;
	False;
	Abstract;
	Macro;
};
enum Binop {
	OpAdd;
	OpMult;
	OpDiv;
	OpSub;
	OpAssign;
	OpEq;
	OpNotEq;
	OpGt;
	OpGte;
	OpLt;
	OpLte;
	OpAnd;
	OpOr;
	OpXor;
	OpBoolAnd;
	OpBoolOr;
	OpShl;
	OpShr;
	OpUShr;
	OpMod;
	OpAssignOp(value: Binop);
	OpInterval;
	OpArrow;
};
enum Unop {
	Increment;
	Decrement;
	Not;
	Neg;
	NegBits;
};
enum Constant {
	Int(value: String);
	Float(value: String);
	String(value: String);
	Ident(value: String);
	Regexp(value: String, p:String);
};
enum Token {
	Eof;
	Const(value: Constant);
	Kwd(value: Keyword);
	Comment(value: String);
	CommentLine(value: String);
	Binop(value: Binop);
	Unop(value: Unop);
	Semicolon;
	Comma;
	BrOpen;
	BrClose;
	BkOpen;
	BkClose;
	POpen;
	PClose;
	Dot;
	DblDot;
	Arrow;
	IntInterval(value: String);
	Sharp(value: String);
	Question;
	At;
	Dollar(value: String);
};
enum Unop_flag {
	Prefix;
	Postfix;
};
enum While_flag {
	NormalWhile;
	DoWhile;
};
typedef Type_path = {
	tpackage : List<String>,
	tname : String,
	tparams : List<Type_param_or_const>,
	tsub : Option<String>
};
enum Type_param_or_const {
	TPType(value: Complex_type);
	TPExpr(value: Expr);
};
enum Complex_type {
	CTPath(value: Type_path);
	CTAnonymous(value: List<Class_field>);
	CTParent(value: Complex_type);
	CTOptional(value: Complex_type);
};
typedef Func = {
	f_params : List<Type_param>,
	f_type : Option<Complex_type>,
	f_expr : Option<Expr>
};
enum Expr_def {
	EConst(value: Constant);
	EParenthesis(value: Expr);
	EArrayDecl(value: List<Expr>);
	EBlock(value: List<Expr>);
	EBreak;
	EContinue;
	EUntyped(value: Expr);
	EThrow(value: Expr);
	EDisplayNew(value: Type_path);
};
typedef Type_param = {
	tp_name : String,
	tp_params : List<Type_param>,
	tp_constraints : List<Complex_type>,
	tp_meta : Metadata
};
enum Access {
	APublic;
	APrivate;
	AStatic;
	AOverride;
	ADynamic;
	AInline;
	AMacro;
};
enum Class_field_kind {
	FFun(value: Func);
};
typedef Class_field = {
	cff_name : String,
	cff_doc : Documentation,
	cff_pos : Pos,
	cff_meta : Metadata,
	cff_access : List<Access>,
	cff_kind : Class_field_kind
};
enum Enum_flag {
	EPrivate;
	EExtern;
};
enum Class_flag {
	HInterface;
	HExtern;
	HPrivate;
	HExtends(value: Type_path);
	HImplements(value: Type_path);
};
enum Abstract_flag {
	APrivAbstract;
	AFromType(value: Complex_type);
	AToType(value: Complex_type);
	AIsType(value: Complex_type);
};
typedef Enum_constructor = {
	ec_name : String,
	ec_doc : Documentation,
	ec_meta : Metadata,
	ec_args : List<Tuple<String, Bool, Complex_type>>,
	ec_pos : Pos,
	ec_params : List<Type_param>,
	ec_type : Option<Complex_type>
};
enum Import_mode {
	INormal;
	IAsName(value: String);
	IAll;
};
enum Type_def {
	EImport(value: Import);
	EUsing(value: Type_path);
};
