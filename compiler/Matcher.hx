import Ast;
import Common;
import Type;
import Typecore;
enum Con_def {
	CConst(value: Tconstant);
	CAny;
	CType(value: Module_type);
	CArray(value: Int);
	CExpr(value: Texpr);
};
typedef Con = {
	c_def : Con_def,
	c_type : T,
	c_pos : Pos
};
enum St_def {
	SVar(value: Tvar);
};
typedef St = {
	st_def : St_def,
	st_type : T,
	st_pos : Pos
};
enum Dt {
	Goto(value: Int);
	Expr(value: Int);
};
enum Pat_def {
	PAny;
	PVar(value: Pvar);
	PTuple(value: Array<Pat>);
};
typedef Pat = {
	p_def : Pat_def,
	p_type : T,
	p_pos : Pos
};
typedef Out = {
	o_pos : Pos,
	o_id : Int,
	o_catch_all : Bool,
	o_num_paths : Int
};
typedef Pattern_ctx = {
	pc_locals : PMap<String, Pvar>,
	pc_reify : Bool,
	pc_is_complex : Bool
};
typedef Matcher = {
	ctx : Typer,
	need_val : Bool,
	dt_lut : DynArray<Dt>,
	dt_cache : Hashtbl<Dt, Int>,
	dt_count : Int,
	outcomes : List<Out>,
	toplevel_or : Bool,
	has_extractor : Bool,
	is_exhaustive : Bool
};
enum Type_finiteness {
	Infinite;
	CompileTimeFinite;
	RunTimeFinite;
};
class Matcher {
