import Ast;
import Type;
import Common;
import Typecore;
enum Switch_mode {
	CExpr(value: Texpr);
};
enum Access_mode {
	MGet;
	MSet;
	MCall;
};
enum Identifier_type {
	ITLocal(value: Tvar);
	ITType(value: Module_type);
	ITPackage(value: String);
};
enum Display_field_kind {
	FKVar;
	FKMethod;
	FKType;
	FKPackage;
};
enum Access_kind {
	AKNo(value: String);
	AKExpr(value: Texpr);
};
enum Type_class {
	KInt;
	KFloat;
	KString;
	KUnk;
	KDyn;
	KOther;
	KParam(value: T);
};
enum State {
	Generating;
	Done;
	NotYet;
};
enum Macro_arg_type {
	MAExpr;
	MAFunction;
	MAOther;
};
