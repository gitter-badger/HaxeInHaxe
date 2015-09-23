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
