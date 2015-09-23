import Ast;
import Common;
import Type;
typedef Dce = {
	com : Context,
	full : Bool,
	std_dirs : List<String>,
	debug : Bool,
	follow_expr : Dce -> Texpr -> Unit,
	curclass : Tclass,
	marked_fields : List<Tclass_field>,
	marked_maybe_fields : List<Tclass_field>,
	t_stack : List<T>,
	ts_stack : List<T>,
};
