import Ast;
import Type;
import Common;
import Typecore;
typedef In_local = {
	i_var : Tvar,
	i_subst : Tvar,
	i_captured : Bool,
	i_write : Bool,
	i_read : Int,
	i_force_temp : Bool
};
enum Inline_kind {
	IKNone;
};
typedef Compl_locals = {
};
