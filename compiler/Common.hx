import Ast;
import Type;
enum Package_rule {
	Forbidden;
	Directory(value: String);
	Remap(value: String);
};
typedef Pos = Ast.Pos;
typedef Basic_types = {
	tvoid : T,
	tint : T,
	tfloat : T,
	tbool : T,
	tnull : T -> T,
	tstring : T,
	tarray : T -> T
};
typedef Stats = {
	s_files_parsed : Ref<Int>,
	s_classes_built : Ref<Int>,
	s_methods_typed : Ref<Int>,
	s_macros_called : Ref<Int>
};
enum Platform {
	Cross;
	Js;
	Neko;
	Flash;
	Php;
	Cpp;
	Cs;
	Java;
	Python;
};
enum Capture_policy {
	CPNone;
	CPWrapRef;
	CPLoopVars;
};
typedef Platform_config = {
	pf_static : Bool,
	pf_sys : Bool,
	pf_locals_scope : Bool,
	pf_captured_scope : Bool,
	pf_unique_locals : Bool,
	pf_capture_policy : Capture_policy,
	pf_pad_nulls : Bool,
	pf_add_final_return : Bool,
	pf_overload : Bool,
	pf_pattern_matching : Bool,
	pf_can_skip_non_nullable_argument : Bool,
	pf_reserved_type_paths : List<Path>
};
enum Display_mode {
	DMNone;
	DMDefault;
	DMUsage;
	DMPosition;
	DMToplevel;
	DMResolve(value: String);
	DMType;
};
typedef Context = {
	version : Int,
	args : List<String>,
	sys_args : List<String>,
	display : Display_mode,
	debug : Bool,
	verbose : Bool,
	foptimize : Bool,
	platform : Platform,
	config : Platform_config,
	std_path : List<String>,
	class_path : List<String>,
	main_class : Option<Type.Path>,
	defines : PMap<String, String>,
	package_rules : PMap<String, Package_rule>,
	error : String -> Pos -> Unit,
	warning : String -> Pos -> Unit,
	file : String,
	flash_version : Float,
	features : Hashtbl<String, Bool>,
	modules : List<Type.Module_def>,
	php_front : Option<String>,
	php_lib : Option<String>,
	php_prefix : Option<String>,
	net_std : List<String>,
	c_args : List<String>,
	js_gen : Option < Unit -> Unit >,
	basic : Basic_types,
	memory_marker : Array<Float>
};
typedef Timer_infos = {
	name : String,
	start : List<Float>,
	total : Float
};
