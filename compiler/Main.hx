import Printf;
import Ast;
import Genswf;
import Common;
import Type;
typedef Context = {
	com : Common.Context,
	flush : Unit -> Unit,
	setup : Unit -> Unit,
	messages : List<String>,
	has_next : Bool,
	has_error : Bool
};
typedef Cache = {
};
