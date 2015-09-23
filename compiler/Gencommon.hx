import Unix;
import Ast;
import Type;
import Common;
import Option;
import Printf;
import ExtString;
import ExprHashtblHelper;
import ClosuresToClass;
enum Priority {
	PFirst;
	PLast;
	PZero;
	PCustom(value: Float);
};
typedef Generator_ctx = {
	gcon : Common.Context,
	gclasses : Gen_classes,
	gtools : Gen_tools,
	gmk_internal_name : String -> String -> String,
};
typedef Gen_classes = {
	cl_reflect : Tclass,
	cl_type : Tclass,
	cl_dyn : Tclass,
	t_iterator : Tdef,
	nativearray_len : Texpr -> Pos -> Texpr,
	nativearray_type : Type -> Type,
	nativearray : Type -> Type
};
typedef Gen_tools = {
	r_create_empty : Texpr -> T -> Texpr,
	r_fields : Bool -> Texpr -> Texpr,
	r_set_field : T -> Texpr -> Texpr -> Texpr -> Texpr,
	r_field : Bool -> T -> Texpr -> Texpr -> Texpr,
	rf_create_empty : Tclass -> Tparams -> Pos -> Texpr
};
enum Tfield_access {
	FAnonField(value: Tclass_field);
	FDynamicField(value: T);
	FNotFound;
};
enum T_dependency {
	DAfter(value: Float);
	DBefore(value: Float);
};
