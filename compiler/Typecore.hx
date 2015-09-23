import Common;
import Type;
enum With_type {
	NoValue;
	Value;
	WithType(value: T);
	WithTypeResume(value: T);
};
typedef Type_patch = {
	tp_type : Option<Ast.Complex_type>,
	tp_remove : Bool,
	tp_meta : Ast.Metadata
};
enum Current_fun {
	FunMember;
	FunStatic;
	FunConstructor;
	FunMemberAbstract;
	FunMemberClassLocal;
	FunMemberAbstractLocal;
};
enum Macro_mode {
	MExpr;
	MBuild;
	MMacroType;
};
enum Typer_pass {
	PBuildModule;
	PBuildClass;
	PTypeField;
	PCheckConstraint;
	PForce;
	PFinal;
};
typedef Typer_globals = {
	types_module : Hashtbl<Path, Path>,
	modules : Hashtbl<Path, Module_def>,
	doinline : Bool,
	core_api : Option<Typer>,
	std : Module_def,
	hook_generate : List < Unit -> Unit >,
	delayed_macros : DynArray < Unit -> Unit >,
	global_using : List<Tclass>,
	do_create : Common.Context -> Typer,
	do_optimize : Typer -> Texpr -> Texpr,
};
typedef Typer_module = {
	curmod : Module_def,
	module_types : List<Module_type>,
	module_using : List<Tclass>,
	module_imports : List<Ast.Import>
};
typedef Typer = {
	com : Context,
	t : Basic_types,
	g : Typer_globals,
	meta : Metadata,
	this_stack : List<Texpr>,
	with_type_stack : List<With_type>,
	pass : Typer_pass,
	m : Typer_module,
	curclass : Tclass,
	tthis : T,
	curfield : Tclass_field,
	untyped : Bool,
	in_super_call : Bool,
	in_loop : Bool,
	in_display : Bool,
	in_macro : Bool,
	macro_depth : Int,
	curfun : Current_fun,
	ret : T,
	locals : PMap<String, Tvar>,
	opened : List<Anon_status>,
	vthis : Option<Tvar>,
	on_error : Typer -> String -> Pos -> Unit
};
enum Call_error {
	Too_many_arguments;
	Could_not_unify(value: Error_msg);
	Cannot_skip_non_nullable(value: String);
};
enum Error_msg {
	Module_not_found(value: Path);
	Unify(value: List<Unify_error>);
	Custom(value: String);
	Unknown_ident(value: String);
	Call_error(value: Call_error);
};
