import Ast;
import Common;
import Type;
import Typecore;
enum Usage {
	Declare(value: Tvar);
	Use(value: Tvar);
	Assign(value: Tvar);
};
