import Ast;
import Type;
import Common;
import Typecore;
typedef Generic_context = {
	ctx : Typer,
	subst : List<Tuple<T, T>>,
	name : String,
	p : Pos,
	mg : Option<Module_def>
};
typedef Stack_context = {
	stack_var : String,
	stack_exc_var : String,
	stack_pos_var : String,
	stack_pos : Pos,
	stack_expr : Texpr,
	stack_pop : Texpr,
	stack_save_pos : Texpr,
	stack_restore : List<Texpr>,
	stack_push : Tclass -> String -> Texpr,
	stack_return : Texpr -> Texpr
};
