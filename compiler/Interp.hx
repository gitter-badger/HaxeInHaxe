import Common;
import Nast;
import Unix;
import Type;
import Ast;
enum Value {
	VNull;
	VBool(value: Bool);
	VInt(value: Int);
	VFloat(value: Float);
	VString(value: String);
	VObject(value: Vobject);
	VArray(value: Array<Value>);
	VAbstract(value: Vabstract);
	VFunction(value: Vfunction);
	VInt32(value: Int32);
};
typedef Vobject = {
	oproto : Option<Vobject>
};
enum Vabstract {
	ADeallocated(value: Ref<Int>);
	AKind(value: Vabstract);
	AHash(value: Hashtbl<Value, Value>);
	ARandom(value: Ref<Random.State>);
	ABuffer(value: Buffer);
	APos(value: Ast.Pos);
	AFWrite(value: Out_channel);
	AReg(value: Regexp);
	AZipI(value: Zlib);
	AZipD(value: Zlib);
	AUtf8(value: UTF8.Buf.Buf);
	ASocket(value: Unix.File_descr);
	ATDecl(value: Module_type);
	AUnsafe(value: Obj);
	ANekoAbstract(value: Extc.Value);
	ANekoBuffer(value: Value);
	ACacheRef(value: Value);
	AInt32Kind;
	ATls(value: Ref<Value>);
	AProcess(value: Process.Process);
};
enum Vfunction {
	Fun0(value: Unit -> Value);
	Fun1(value: Value -> Value);
	Fun2(value: Value -> Value -> Value);
	Fun3(value: Value -> Value -> Value -> Value);
	Fun4(value: Value -> Value -> Value -> Value -> Value);
	Fun5(value: Value -> Value -> Value -> Value -> Value -> Value);
	FunVar(value: List<Value> -> Value);
};
typedef Regexp = {
	r : Str.Regexp,
	r_string : String,
};
typedef Zlib = {
	z : Extc.Zstream,
	z_flush : Extc.Zflush
};
enum Cmp {
	CEq;
	CSup;
	CInf;
	CUndef;
};
typedef Extern_api = {
	pos : Ast.Pos,
	get_com : Unit -> Common.Context,
	get_module : String -> List<Type>,
	type_macro_expr : Ast.Expr -> Type.Texpr,
	store_typed_expr : Type.Texpr -> Ast.Expr,
	get_display : String -> String,
	allow_package : String -> Unit,
};
typedef Callstack = {
	cpos : Pos,
	cthis : Value,
	cstack : Int,
	cenv : Array<Value>
};
typedef Context = {
	gen : Genneko.Context,
	types : Hashtbl<Type.Path, Int>,
	fields_cache : Hashtbl<Int, String>,
	error : Bool,
	error_proto : Vobject,
	do_string : Value -> String,
	do_loadprim : Value -> Value -> Value,
	do_compare : Value -> Value -> Cmp,
	loader : Value,
	exports : Value,
	stack : DynArray<Value>,
	callstack : List<Callstack>,
	callsize : Int,
	exc : List<Pos>,
	vthis : Value,
	venv : Array<Value>,
	curapi : Extern_api,
	on_reused : List < Unit -> Bool >,
	is_reused : Bool,
	locals_map : PMap<String, Int>,
	locals_count : Int,
	locals_barrier : Int,
	locals_env : DynArray<String>,
};
enum Access {
	AccThis;
	AccLocal(value: Int);
	AccGlobal(value: Ref<Value>);
	AccEnv(value: Int);
};
typedef Neko_context = {
	load : String -> Int -> Primitive,
	call : Primitive -> List<Value> -> Value
};
enum Enum_index {
	IExpr;
	IBinop;
	IUnop;
	IConst;
	ITParam;
	ICType;
	IField;
	IType;
	IFieldKind;
	IMethodKind;
	IVarAccess;
	IAccess;
	IClassKind;
	ITypedExpr;
	ITConstant;
	IModuleType;
	IFieldAccess;
	IAnonStatus;
	IImportMode;
};
