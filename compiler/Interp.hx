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
	VClosure(value: List<Value>List<Value> -> List<Value> -> Value);
	VInt32(value: Int32);
};

typedef Vobject = {
	ofields : Array<Tuple<Int, Value>>,
	oproto : Option<Vobject>
};

enum Vabstract {
	ADeallocated(value: Ref<Int>);
	AKind(value: Vabstract);
	AHash(value: Hashtbl<Value, Value>);
	ARandom(value: Ref<Random.State>);
	ABuffer(value: Buffer);
	APos(value: Ast.Pos);
	AFRead(value: Tuple<In_channel, Ref<Bool>>);
	AFWrite(value: Out_channel);
	AReg(value: Regexp);
	AZipI(value: Zlib);
	AZipD(value: Zlib);
	AUtf8(value: UTF8.Buf.Buf);
	ASocket(value: Unix.File_descr);
	ATDecl(value: Module_type);
	AUnsafe(value: Obj);
	ALazyType(value: Ref < Unit -> Type > );
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
	r_groups : Array<Tuple<Int, Int>>
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
	get_type : String -> Option<Type>,
	get_module : String -> List<Type>,
	on_generate : List<Type> -> Unit -> Unit,
	after_generate : Unit -> Unit -> Unit,
	on_type_not_found : String -> Value -> Unit,
	parse_string : String -> Ast.Pos -> Bool -> Ast.Expr,
	type_expr : Ast.Expr -> Type.Texpr,
	type_macro_expr : Ast.Expr -> Type.Texpr,
	store_typed_expr : Type.Texpr -> Ast.Expr,
	get_display : String -> String,
	allow_package : String -> Unit,
	type_patch : String -> String -> Bool -> Option<String> -> Unit,
	meta_patch : String -> String -> Option<String> -> Bool -> Unit,
	set_js_generator : Value -> Unit -> Unit,
	get_local_type : Unit -> Option<T>,
	get_expected_type : Unit -> Option<T>,
	get_call_arguments : Unit -> Option<Ast.Expr>,
	get_local_method : Unit -> String,
	get_local_imports : Unit -> List<Ast.Import>,
	get_local_using : Unit -> List<Tclass>,
	get_local_vars : Unit -> PMap<String, Type.Tvar>,
	get_build_fields : Unit -> Value,
	get_pattern_locals : Ast.Expr -> Type -> PMap<String, Tuple<Type.Tvar, Ast.Pos>>,
	define_type : Value -> Unit,
	define_module : String -> List<Value> -> List<Tuple<List<Tuple<String, Ast.Pos>>, Ast.Import_mode>> -> List<Ast.Type_path> -> Unit,
	module_dependency : String -> String -> Bool -> Unit,
	current_module : Unit -> Module_def,
	delayed_macro : Int -> Unit -> Unit -> Value,
	use_cache : Unit -> Bool,
	format_string : String -> Ast.Pos -> Ast.Expr,
	cast_or_unify : Type -> Texpr -> Ast.Pos -> Type.Texpr,
	add_global_metadata : String -> String -> Tuple<Bool, Bool, Bool> -> Unit
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
	prototypes : Hashtbl<String, Vobject>,
	fields_cache : Hashtbl<Int, String>,
	error : Bool,
	error_proto : Vobject,
	enums : Array<Tuple<Value, String>>,
	do_call : Value -> Value -> List<Value> -> Pos -> Value,
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
	globals : PMap<String, Value>
};

enum Access {
	AccThis;
	AccLocal(value: Int);
	AccGlobal(value: Ref<Value>);
	AccEnv(value: Int);
	AccField(value: Unit -> ValueString);
	AccArray(value: Unit -> ValueUnit -> Value);
};

class /*exception*/ Runtime {

};

class /*exception*/ Builtin_error {

};

class /*exception*/ Error {

};

class /*exception*/ Abort {

};

class /*exception*/ Continue {

};

class /*exception*/ Break {

};

class /*exception*/ Return {

};

class /*exception*/ Invalid_expr {

};

class /*exception*/ Sys_exit {

};

typedef Primitive = Tuple<String, Extc.Value, Int>;

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
class Interp {
	public static var get_ctx_ref = ref(function []: assert False);

	public static var encode_complex_type_ref = ref(function t: assert False);

	public static var encode_type_ref = ref(function t: assert False);

	public static var decode_type_ref = ref(function t: assert False);

	public static var encode_expr_ref = ref(function e: assert False);

	public static var decode_expr_ref = ref(function e: assert False);

	public static var encode_texpr_ref = ref(function e: assert False);

	public static var decode_texpr_ref = ref(function e: assert False);

	public static var encode_clref_ref = ref(function c: assert False);

	public static var enc_hash_ref = ref(function h: assert False);

	public static var enc_array_ref = ref(function l: assert False);

	public static var dec_array_ref = ref(function v: assert False);

	public static var enc_string_ref = ref(function s: assert False);

	public static var encode_tvar_ref = ref(function _: assert False);

	public static var decode_path_ref = ref(function _: assert False);

	public static var decode_import_ref = ref(function _: assert False);

	public static var encode_import_ref = ref(function _: assert False);

	public static var eval_expr_ref = ref(function _: function _: assert False) : ref(context -> texpr -> option(value));

	public static function get_ctx([]) return {
		get_ctx_ref.val([]);
	};

	public static function enc_array(llist(value)) return {
		enc_array_ref.val(l) : value;
	};

	public static function dec_array(lvalue) return {
		dec_array_ref.val(l) : list(value);
	};

	public static function encode_complex_type(tAst.complex_type) return {
		encode_complex_type_ref.val(t) : value;
	};

	public static function encode_type(tType.t) return {
		encode_type_ref.val(t) : value;
	};

	public static function decode_type(vvalue) return {
		decode_type_ref.val(v) : Type.t;
	};

	public static function encode_expr(eAst.expr) return {
		encode_expr_ref.val(e) : value;
	};

	public static function decode_expr(evalue) return {
		decode_expr_ref.val(e) : Ast.expr;
	};

	public static function encode_texpr(eType.texpr) return {
		encode_texpr_ref.val(e) : value;
	};

	public static function decode_texpr(vvalue) return {
		decode_texpr_ref.val(v) : Type.texpr;
	};

	public static function encode_clref(ctclass) return {
		encode_clref_ref.val(c) : value;
	};

	public static function enc_hash(hHashtbl.t(a)(b)) return {
		enc_hash_ref.val(h) : value;
	};

	public static function enc_string(sstring) return {
		enc_string_ref.val(s) : value;
	};

	public static function encode_tvar(vtvar) return {
		encode_tvar_ref.val(v) : value;
	};

	public static function decode_path(vvalue) return {
		decode_path_ref.val(v) : Ast.type_path;
	};

	public static function encode_import(iAst.import) return {
		encode_import_ref.val(i) : value;
	};

	public static function decode_import(vvalue) return {
		decode_import_ref.val(v) : Ast.import;
	};

	public static function to_int(f) return {
		Int32.of_float(mod_float(f, 2147483648.0));
	};

	public static function need_32_bits(i) return {
		<>(Int32.compare(Int32.logand(Int32.add(i, ), ), Int32.zero), 0);
	};

	public static function best_int(i) return {
		if (need_32_bits(i)) {
			VInt32(i);
		} else {
			VInt(Int32.to_int(i));
		};
	};

	public static function make_pos(p) return {
		var low = land(p.pline, 0xFFFFF);
		{
			() with Ast.pfile = p.psource;
			Ast.pmin = low;
			Ast.pmax = +(low, lsr(p.pline, 20))
		};
	};

	public static function warn(ctx, msg, p) return {
		ctx.curapi.get_com([]).Common.warning(msg, make_pos(p));
	};

	public static function pop(ctx, n) return {
		if ( > (n, 0)) {
			DynArray.delete_last(ctx.stack);
			pop(ctx, -(n, 1));
		} else {
			[];
		};
	};

	public static function pop_ret(ctx, f, n) return {
		var v = f([]);
		pop(ctx, n);
		v;
	};

	public static function push(ctx, v) return {
		DynArray.add(ctx.stack, v);
	};

	public static function hash(f) return {
		var h = ref(0);
		for (i in /*to*/0... - (String.length(f), 1)) {
			h.val = +( * (h.val, 223), int_of_char(String.unsafe_get(f, i)));
		};
		if ( = (Sys.word_size, 64)) {
			Int32.to_int(Int32.shift_right(Int32.shift_left(Int32.of_int(h.val), 1), 1));
		} else {
			h.val;
		};
	};

	public static var constants = var h = Hashtbl.create(0);
	List.iter(function f: Hashtbl.add(h, hash(f), f), ::("done", ::("read", ::("write", ::("min", ::("max", ::("file",
			  ::("args", ::("loadprim", ::("loadmodule", ::("__a", ::("__s", ::("h", ::("tag", ::("index", ::("length", ::("message",
										   ::("pack", ::("name", ::("params", ::("sub", ::("doc", ::("kind", ::("meta", ::("access", ::("constraints", ::("opt",
												   ::("type", ::("value", ::("ret", ::("expr", ::("field", ::("values", ::("get", ::("__string", ::("toString", ::("$",
														   ::("add", ::("remove", ::("has", ::("__t", ::("module", ::("isPrivate", ::("isPublic", ::("isExtern", ::("isInterface",
																   ::("exclude", ::("constructs", ::("names", ::("superClass", ::("interfaces", ::("fields", ::("statics", ::("constructor",
																		   ::("init", ::("t", ::("gid", ::("uid", ::("atime", ::("mtime", ::("ctime", ::("dev", ::("ino", ::("nlink", ::("rdev",
																				   ::("size", ::("mode", ::("pos", ::("len", ::("binops", ::("unops", ::("from", ::("to", ::("array", ::("op", ::("isPostfix",
																						   ::("impl", ::("resolve", ::("id", ::("capture", ::("extra", ::("v", ::("ids", ::("vars", ::("en", ::("overrides",
																								   ::("status", ::("overloads",
																										   ::("path", [])))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))));
	h;

	public static var h_get = hash("__get");

	public static var h_set = hash("__set");

	public static var h_add = hash("__add");

	public static var h_radd = hash("__radd");

	public static var h_sub = hash("__sub");

	public static var h_rsub = hash("__rsub");

	public static var h_mult = hash("__mult");

	public static var h_rmult = hash("__rmult");

	public static var h_div = hash("__div");

	public static var h_rdiv = hash("__rdiv");

	public static var h_mod = hash("__mod");

	public static var h_rmod = hash("__rmod");

	public static var h_string = hash("__string");

	public static var h_compare = hash("__compare");

	public static var h_constructs = hash("__constructs__");

	public static var h_a = hash("__a");

	public static var h_s = hash("__s");

	public static var h_class = hash("__class__");

	public static function exc(v) return {
		raise(Runtime(v));
	};

	public static function hash_field(ctx, f) return {
		var h = hash(f);
		try {
			var f2 = Hashtbl.find(ctx.fields_cache, h);
			if (<>(f, f2)) {
				exc(VString( ^ ("Field conflict between ", ^ (f, ^ (" and ", f2)))));
			} else {
				[];
			};
		} catch (e: Not_found) {
			Hashtbl.add(ctx.fields_cache, h, f);
		};
		h;
	};

	public static function field_name(ctx, fid) return {
		try {
			Hashtbl.find(ctx.fields_cache, fid);
		} catch (e: Not_found) {
			"???";
		};
	};

	public static function obj(hash, fields) return {
		var fields = Array.of_list(List.map(function (k, v): (new Tuple(hash(k), v)), fields));
		Array.sort(function (k1, _): function (k2, _): compare(k1, k2), fields);
		{
			() with ofields = fields;
			oproto = None
		};
	};

	public static function parse_int(s) return {
		function loop_hex(i) return {
			if ( = (i, String.length(s))) {
				s;
			} else {
				switch (String.unsafe_get(s, i)) {
				case '0' .. '9' | 'a' .. 'f' | 'A' .. 'F': loop_hex(+(i, 1));
				case _: String.sub(s, 0, i);
				};
			};
		};
		function loop(sp, i) return {
			if ( = (i, String.length(s))) {
				if ( = (sp, 0)) {
					s;
				} else {
					String.sub(s, sp, -(i, sp));
				};
			} else {
				switch (String.unsafe_get(s, i)) {
				case '0' .. '9': loop(sp, +(i, 1));
				case ' ' if (=(sp, i)): loop(+(sp, 1), +(i, 1));
				case '-' if (=(i, 0)): loop(sp, +(i, 1));
				case 'x' | 'X' if (&&(=(i, 1), =(String.get(s, 0), '0'))): loop_hex(+(i, 1));
				case _: String.sub(s, sp, -(i, sp));
				};
			};
		};
		best_int(Int32.of_string(loop(0, 0)));
	};

	public static function parse_float(s) return {
		function loop(sp, i) return {
			if ( = (i, String.length(s))) {
				if ( = (sp, 0)) {
					s;
				} else {
					String.sub(s, sp, -(i, sp));
				};
			} else {
				switch (String.unsafe_get(s, i)) {
				case ' ' if (=(sp, i)): loop(+(sp, 1), +(i, 1));
				case '0' .. '9' | '-' | '+' | 'e' | 'E' | '.': loop(sp, +(i, 1));
				case _: String.sub(s, sp, -(i, sp));
				};
			};
		};
		float_of_string(loop(0, 0));
	};

	public static function find_sub(str, sub, start) return {
		var sublen = String.length(sub);
		if ( = (sublen, 0)) {
			0;
		} else {
			var found = ref(0);
			var len = String.length(str);
			try {
				for (i in /*to*/start... - (len, sublen)) {
					var j = ref(0);
					= (String.unsafe_get(str, +(i, j.val)), String.unsafe_get(sub, j.val))incr(j);
					if ( = (j.val, sublen)) {
						found.val = i;
						raise(Exit);
					} else {
						[];
					};
				};
				raise(Not_found);
			} catch (e: Exit) {
				found.val;
			};
		};
	};

	public static function nargs(match) return switch (match) {
	case Fun0(_): 0;
	case Fun1(_): 1;
	case Fun2(_): 2;
	case Fun3(_): 3;
	case Fun4(_): 4;
	case Fun5(_): 5;
	case FunVar(_): -1;
	};

	public static function get_field(o, fid) return {
		function loop(min, max) return {
			if ( < (min, max)) {
				var mid = lsr(+(min, max), 1);
				var Tuple(cid, v) = Array.unsafe_get(o.ofields, mid);
				if ( < (cid, fid)) {
					loop(+(mid, 1), max);
				} else {
					if ( > (cid, fid)) {
						loop(min, mid);
					} else {
						v;
					};
				};
			} else {
				switch (o.oproto) {
				case None: VNull;
				case Some(p): get_field(p, fid);
				};
			};
		};
		loop(0, Array.length(o.ofields));
	};

	public static function set_field(o, fid, v) return {
		function loop(min, max) return {
			var mid = lsr(+(min, max), 1);
			if ( < (min, max)) {
				var Tuple(cid, _) = Array.unsafe_get(o.ofields, mid);
				if ( < (cid, fid)) {
					loop(+(mid, 1), max);
				} else {
					if ( > (cid, fid)) {
						loop(min, mid);
					} else {
						Array.unsafe_set(o.ofields, mid, (new Tuple(cid, v)));
					};
				};
			} else {
				var fields = Array.make(+(Array.length(o.ofields), 1), (new Tuple(fid, v)));
				Array.blit(o.ofields, 0, fields, 0, mid);
				Array.blit(o.ofields, mid, fields, +(mid, 1), -(Array.length(o.ofields), mid));
				o.ofields = fields;
			};
		};
		loop(0, Array.length(o.ofields));
	};

	public static function remove_field(o, fid) return {
		function loop(min, max) return {
			var mid = lsr(+(min, max), 1);
			if ( < (min, max)) {
				var Tuple(cid, v) = Array.unsafe_get(o.ofields, mid);
				if ( < (cid, fid)) {
					loop(+(mid, 1), max);
				} else {
					if ( > (cid, fid)) {
						loop(min, mid);
					} else {
						var fields = Array.make(-(Array.length(o.ofields), 1), (new Tuple(fid, VNull)));
						Array.blit(o.ofields, 0, fields, 0, mid);
						Array.blit(o.ofields, +(mid, 1), fields, mid, -(-(Array.length(o.ofields), mid), 1));
						o.ofields = fields;
						True;
					};
				};
			} else {
				False;
			};
		};
		loop(0, Array.length(o.ofields));
	};

	public static function get_field_opt(o, fid) return {
		function loop(min, max) return {
			if ( < (min, max)) {
				var mid = lsr(+(min, max), 1);
				var Tuple(cid, v) = Array.unsafe_get(o.ofields, mid);
				if ( < (cid, fid)) {
					loop(+(mid, 1), max);
				} else {
					if ( > (cid, fid)) {
						loop(min, mid);
					} else {
						Some(v);
					};
				};
			} else {
				switch (o.oproto) {
				case None: None;
				case Some(p): get_field_opt(p, fid);
				};
			};
		};
		loop(0, Array.length(o.ofields));
	};

	public static function catch_errors(ctx, ? : (final = function []: []), f) return {
		var n = DynArray.length(ctx.stack);
		try {
			var v = f([]);
			final([]);
			Some(v);
		} catch (e: T) {
			McOr(McArr(PaApp(PaId(IdUid(<...>)), PaId(IdLid(<...>))), ExNil, ExSeq(ExSem(ExApp(<...>, <...>), ExSem(<...>, <...>)))),
		McArr(PaId(IdUid(Abort)), ExNil, ExSeq(ExSem(ExApp(<...>, <...>), ExSem(<...>, <...>)))))			case Runtime(v): pop(ctx,
			-(DynArray.length(ctx.stack), n));
			final([]);
			function loop(o) return {
				if ( == (o, ctx.error_proto)) {
					True;
				} else {
					switch (o.oproto) {
					case None: False;
					case Some(p): loop(p);
					};
				};
			};
			switch (v) {
			case VObject(o) if (loop(o)):
				switch ((new Tuple(get_field(o, hash("message")), get_field(o, hash("pos"))))) {
				case (VObject(msg), VAbstract(APos(pos))): switch (get_field(msg, h_s)) {
					case VString(msg): raise(Typecore.Error(Typecore.Custom(msg), pos));
					case _: [];
					};
				case _: [];
				};
			case _: [];
			};
			raise(Error(ctx.do_string(v), List.map(function s: make_pos(s.cpos), ctx.callstack)));
		case Abort: pop(ctx, -(DynArray.length(ctx.stack), n));
			final([]);
			None;
		};
	};

	public static function make_library(fl) return {
		var h = Hashtbl.create(0);
		List.iter(function (n, f): Hashtbl.add(h, n, f), fl);
		h;
	};

	public static var neko = var is_win = || ( = (Sys.os_type, "Win32"), = (Sys.os_type, "Cygwin"));
	var neko = Extc.dlopen(if (is_win) {
	"neko.dll";
} else {
	"libneko.so";
});
	var null = Extc.dlint(0);
	var neko = if ( && ( == (Obj.magic(neko), null), !(is_win))) {
		Extc.dlopen("libneko.dylib");
	} else {
		neko;
	};
	if ( == (Obj.magic(neko), null)) {
		None;
	} else {
		function load(v) return {
			var s = Extc.dlsym(neko, v);
			if ( == (Obj.magic(s), null)) {
				failwith( ^ ("Could not load neko.", v));
			} else {
				[];
			};
			s;
		};
		ignore(Extc.dlcall0(load("neko_global_init")));
		var vm = Extc.dlcall1(load("neko_vm_alloc"), null);
		ignore(Extc.dlcall1(load("neko_vm_select"), vm));
		var loader = Extc.dlcall2(load("neko_default_loader"), null, null);
		var loadprim = var l1 = load("neko_val_field");
		var l2 = Extc.dlcall1(load("neko_val_id"), Extc.dlstring("loadprim"));
		Extc.dlcall2(l1, loader, l2);
		var callN = load("neko_val_callN");
		var callEx = load("neko_val_callEx");
		var copy_string = load("neko_copy_string");
		var alloc_root = load("neko_alloc_root");
		var free_root = load("neko_free_root");
		function alloc_root(v) return {
			var r = Extc.dlcall1(alloc_root, Extc.dlint(1));
			Extc.dlsetptr(r, v);
			r;
		};
		function free_root(r) return {
			ignore(Extc.dlcall1(free_root, r));
		};
		ignore(alloc_root(vm));
		ignore(alloc_root(loader));
		ignore(alloc_root(loadprim));
		function alloc_string(s) return {
			Extc.dlcall2(copy_string, Extc.dlstring(s), Extc.dlint(String.length(s)));
		};
		function alloc_int(iint) return {
			Obj.magic(i) : Extc.value;
		};
		function loadprim(n, args) return {
			var exc = ref(null);
			var vargs = [alloc_string(n);
			alloc_int(args)];
			var p = Extc.dlcall5(callEx, loader, loadprim, Obj.magic(vargs), Extc.dlint(2), Obj.magic(exc));
			if ( != (exc.val, null)) {
				failwith( ^ ("Failed to load ", ^ (n, ^ (":", string_of_int(args)))));
			} else {
				[];
			};
			ignore(alloc_root(p));
			(new Tuple(n, p, args));
		};
		function call_raw_prim(Tuple(_, p, nargs), argsarray(Extc.value)) return {
			Extc.dlcall3(callN, p, Obj.magic(args), Extc.dlint(nargs));
		};
		var unser = try {
			loadprim("std@unserialize", 2);
		} catch (e: _) {
			(new Tuple("", null, 0));
		};
		if (switch (unser) {
		case (, _, _): True;
			case _: False;
			}) {
			None;
		} else {
			var val_true = call_raw_prim(unser, [alloc_string("T");
												 loader]);
			var val_false = call_raw_prim(unser, [alloc_string("F");
												  loader]);
			var val_null = call_raw_prim(unser, [alloc_string("N");
												 loader]);
			var is_64 = == (call_raw_prim(loadprim("std@sys_is64", 0), []), val_true);
			var Tuple(alloc_i32, is_v2) = try {
				(new Tuple(load("neko_alloc_int32"), True));
			} catch (e: _) {
				(new Tuple(Obj.magic(0), False));
			};
			var alloc_i32 = if (is_v2) {
				function i: Extc.dlcall1(alloc_i32, Extc.dlint32(i));
			} else {
				function i: alloc_int(Int32.to_int(if ( < (Int32.compare(i, Int32.zero), 0)) {
				Int32.logand(i, );
				} else {
					Int32.logor(i, );
				}));
			};
			var tag_bits = if (is_v2) {
				4;
			} else {
				3;
			};
			var tag_mask = -(lsl(1, tag_bits), 1);
			var ptr_size = if (is_64) {
				8;
			} else {
				4;
			};
			function val_field(v, i) return {
				Extc.dladdr(v, * (+(i, 1), ptr_size));
			};
			function val_str(v) return {
				Extc.dladdr(v, 4);
			};
			function val_fun_env(v) return {
				Extc.dladdr(v, +(8, ptr_size));
			};
			var alloc_function = load("neko_alloc_function");
			var alloc_array = load("neko_alloc_array");
			var alloc_float = load("neko_alloc_float");
			var alloc_object = load("neko_alloc_object");
			var alloc_field = load("neko_alloc_field");
			var alloc_abstract = load("neko_alloc_abstract");
			var val_gc = load("neko_val_gc");
			var val_field_name = load("neko_val_field_name");
			var val_iter_fields = load("neko_val_iter_fields");
			var gen_callback = Extc.dlcaml_callback(2);
			var on_abstract_gc = Extc.dlcaml_callback(1);
			var root_index = ref(0);
			var roots = Hashtbl.create(0);
			Callback.register("dlcallb1", function a: var index = Obj.magic(Extc.dlptr(val_field(a, 1))) : int;
							  Hashtbl.remove(roots, index);
							  null);
			function copy_string(v) return {
				var head = Extc.dltoint(Extc.dlptr(v));
				var size = asr(head, tag_bits);
				var s = String.create(size);
				Extc.dlmemcpy(Extc.dlstring(s), val_str(v), size);
				s;
			};
			var buffers = ref([]);
			function value_neko( ? : (obj = VNull)) return {
			case VNull: val_null;
			case VBool(b): if (b) {
					val_true;
				} else {
					val_false;
				};
			case VInt(i): alloc_int(i);
			case VAbstract(ANekoAbstract(a)): a;
			case VAbstract(ANekoBuffer(VString(buf))): var v = value_neko(VString(buf));
				buffers.val = ::((new Tuple(buf, v)), buffers.val);
				v;
			case VString(s): var v = alloc_string(s);
				ignore(copy_string(v));
				v;
			case VObject(o) = obj: var vo = Extc.dlcall1(alloc_object, null);
				Array.iter(function (id, v): ignore(Extc.dlcall3(alloc_field, vo, Extc.dlint(id), value_neko(obj = , v))), o.ofields);
				vo;
			case VClosure(_): failwith("Closure not supported");
			case VFunction(f): var callb = Extc.dlcall3(alloc_function, gen_callback, Extc.dlint(-1), Obj.magic("<callback>"));
				var index = root_index.val;
				incr(root_index);
				Hashtbl.add(roots, index, (new Tuple(f, obj)));
				var a = Extc.dlcall2(alloc_abstract, null, Obj.magic(index));
				if ( != (Extc.dlptr(val_field(a, 1)), Obj.magic(index))) {
					assert False;
				} else {
					[];
				};
				ignore(Extc.dlcall2(val_gc, a, on_abstract_gc));
				Extc.dlsetptr(val_fun_env(callb), a);
				callb;
			case VArray(a): var va = Extc.dlcall1(alloc_array, Extc.dlint(Array.length(a)));
				Array.iteri(function i: function v: Extc.dlsetptr(val_field(va, i), value_neko(v)), a);
				va;
			case VFloat(f): Extc.dlcall1(alloc_float, Obj.magic(f));
			case VAbstract(_): failwith("Abstract not supported");
			case VInt32(i): alloc_i32(i);
			};
			var obj_r = ref([]);
			function obj_fun(v, id) return {
				obj_r.val = ::((new Tuple(v, id)), obj_r.val);
				val_null;
			};
			function neko_value(vExtc.value) return {
				if (Obj.is_int(Obj.magic(v))) {
					VInt(Obj.magic(v));
				} else {
					var head = Extc.dltoint(Extc.dlptr(v));
					switch (land(head, tag_mask)) {
					case 0: VNull;
					case 2: VBool( == (v, val_true));
					case 3: VString(copy_string(v));
					case 4: ignore(Extc.dlcall3(val_iter_fields, v, Extc.dlcallback(2), Obj.magic(obj_fun)));
						var r = obj_r.val;
						obj_r.val = [];
						var ctx = get_ctx([]);
						var fields = List.rev_map(function (v, id): var iid = Extc.dltoint(id);
						if (!(Hashtbl.mem(ctx.fields_cache, iid))) {
						var name = copy_string(Extc.dlcall1(val_field_name, id));
							ignore(hash_field(ctx, name));
						} else {
							[];
						};
						(new Tuple(iid, neko_value(v))), r);
						VObject({ () with ofields = Array.of_list(fields);
								  oproto = None
								});
					case 5: VArray(Array.init(asr(head, tag_bits), function i: neko_value(Extc.dlptr(val_field(v, i)))));
					case 7: var r = alloc_root(v);
						var a = ANekoAbstract(v);
						Gc.finalise(function _: free_root(r), a);
						VAbstract(a);
					case t: failwith( ^ ("Unsupported Neko value tag ", string_of_int(t)));
					};
				};
			};
			Callback.register("dlcallb2", function args: function nargs: var env = Extc.dlptr(Extc.dladdr(vm, * (2, ptr_size)));
							  var index = Obj.magic(Extc.dlptr(val_field(env, 1))) : int;
			var Tuple(f, obj) = try {
				Hashtbl.find(roots, index);
			} catch (e: Not_found) {
				assert False;
			};
			var nargs = Extc.dltoint(nargs);
			function loop(i) return {
				if ( = (i, nargs)) {
					[];
				} else {
					::(neko_value(Extc.dlptr(Extc.dladdr(args, * (i, ptr_size)))), loop(+(i, 1)));
				};
			};
			var v = get_ctx([]).do_call(obj, VFunction(f), loop(0), { () with psource = "<callback>";
										pline = 0
																	});
					value_neko(v));
			function callprim(Tuple(n, p, nargs), args) return {
				var arr = Array.of_list(List.map(value_neko, args));
				var exc = ref(null);
				if (<>(Array.length(arr), nargs)) {
					failwith(n);
				} else {
					[];
				};
				var ret = Extc.dlcall5(callEx, val_null, p, Obj.magic(arr), Extc.dlint(nargs), Obj.magic(exc));
				if ( != (exc.val, null)) {
					raise(Runtime(neko_value(exc.val)));
				} else {
					[];
				};
				switch (buffers.val) {
				case []: [];
				case l: buffers.val = [];
					List.iter(function (buf, v): Extc.dlmemcpy(Extc.dlstring(buf), val_str(v), String.length(buf)), l);
				};
				neko_value(ret);
			};
			Some({ () with load = loadprim;
				   call = callprim
				 });
		};
	};

	public static var builtins = var p = { () with psource = "<builtin>";
										   pline = 0
										 };
	function error([]) return {
		raise(Builtin_error);
	};
	function vint(match) return switch (match) {
	case VInt(n): n;
	case _: error([]);
	};
	function varray(match) return switch (match) {
	case VArray(a): a;
	case _: error([]);
	};
	function vstring(match) return switch (match) {
	case VString(s): s;
	case _: error([]);
	};
	function vobj(match) return switch (match) {
	case VObject(o): o;
	case _: error([]);
	};
	function vfun(match) return switch (match) {
	case VFunction(f): f;
	case VClosure(cl, f): FunVar(f(cl));
	case _: error([]);
	};
	function vhash(match) return switch (match) {
	case VAbstract(AHash(h)): h;
	case _: error([]);
	};
	function build_stack(sl) return {
		function make(p) return {
			var p = make_pos(p);
			VArray([VString(p.Ast.pfile);
			VInt(Lexer.get_error_line(p))]);
		};
		VArray(Array.of_list(List.map(make, sl)));
	};
	function do_closure(args, args2) return {
		switch (args) {
		case ::(f, ::(obj, args)): get_ctx([]).do_call(obj, f, @(args, args2), p);
		case _: assert False;
		};
	};
	var funcs = ::((new Tuple("array", FunVar(function vl: VArray(Array.of_list(vl))))), ::((new Tuple("amake",
				   Fun1(function v: VArray(Array.create(vint(v), VNull))))), ::((new Tuple("acopy",
						   Fun1(function a: VArray(Array.copy(varray(a)))))), ::((new Tuple("asize",
								   Fun1(function a: VInt(Array.length(varray(a)))))), ::((new Tuple("asub",
										   Fun3(function a: function p: function l: VArray(Array.sub(varray(a), vint(p), vint(l)))))), ::((new Tuple("ablit",
												   Fun5(function dst: function dstp: function src: function p: function l: Array.blit(varray(src), vint(p), varray(dst),
														   vint(dstp), vint(l));
														   VNull))), ::((new Tuple("aconcat", Fun1(function arr: var arr = Array.map(varray, varray(arr));
																   VArray(Array.concat(Array.to_list(arr)))))), ::((new Tuple("string", Fun1(function v: VString(get_ctx([]).do_string(v))))),
																		   ::((new Tuple("smake", Fun1(function l: VString(String.make(vint(l), '\000'))))), ::((new Tuple("ssize",
																				   Fun1(function s: VInt(String.length(vstring(s)))))), ::((new Tuple("scopy",
																						   Fun1(function s: VString(String.copy(vstring(s)))))), ::((new Tuple("ssub",
																								   Fun3(function s: function p: function l: VString(String.sub(vstring(s), vint(p), vint(l)))))), ::((new Tuple("sget",
																										   Fun2(function s: function p:
	try {
		VInt(int_of_char(String.get(vstring(s), vint(p))));
	} catch (e: Invalid_argument(_)) {
		VNull;
	}))), ::((new Tuple("sset", Fun3(function s: function p: function c: var c = char_of_int(land(vint(c), 0xFF));
	try {
		String.set(vstring(s), vint(p), c);
		VInt(int_of_char(c));
	} catch (e: Invalid_argument(_)) {
		VNull;
	}))), ::((new Tuple("sblit", Fun5(function dst: function dstp: function src: function p: function l: String.blit(vstring(
										  src), vint(p), vstring(dst), vint(dstp), vint(l));
									  VNull))), ::((new Tuple("sfind", Fun3(function src: function pos: function pat:
	try {
		VInt(find_sub(vstring(src), vstring(pat), vint(pos)));
	} catch (e: Not_found) {
		VNull;
	}))), ::((new Tuple("new", Fun1(function o:
	switch (o) {
case VNull: VObject({ () with ofields = [];
						  oproto = None
						});
	case VObject(o): VObject({ () with ofields = Array.copy(o.ofields);
								   oproto = o.oproto
								 });
	case _: error([]);
	}))), ::((new Tuple("objget", Fun2(function o: function f:
	switch (o) {
case VObject(o): get_field(o, vint(f));
	case _: VNull;
	}))), ::((new Tuple("objset", Fun3(function o: function f: function v:
	switch (o) {
case VObject(o): set_field(o, vint(f), v);
		v;
	case _: VNull;
	}))), ::((new Tuple("objcall", Fun3(function o: function f: function pl:
	switch (o) {
case VObject(oo): get_ctx([]).do_call(o, get_field(oo, vint(f)), Array.to_list(varray(pl)), p);
	case _: VNull;
	}))), ::((new Tuple("objfield", Fun2(function o: function f:
	switch (o) {
case VObject(o): var p = o.oproto;
		o.oproto = None;
		var v = get_field_opt(o, vint(f));
		o.oproto = p;
		VBool(<>(v, None));
	case _: VBool(False);
	}))), ::((new Tuple("objremove", Fun2(function o: function f: VBool(remove_field(vobj(o), vint(f)))))),
			 ::((new Tuple("objfields", Fun1(function o: VArray(Array.map(function (fid, _): VInt(fid), vobj(o).ofields))))),
				::((new Tuple("hash", Fun1(function v: VInt(hash_field(get_ctx([]), vstring(v)))))), ::((new Tuple("fasthash",
						Fun1(function v: VInt(hash(vstring(v)))))), ::((new Tuple("field", Fun1(function v:
	try {
		VString(Hashtbl.find(get_ctx([]).fields_cache, vint(v)));
	} catch (e: Not_found) {
		VNull;
	}))), ::((new Tuple("objsetproto", Fun2(function o: function p: var o = vobj(o);
	switch (p) {
case VNull: o.oproto = None;
case VObject(p): o.oproto = Some(p);
	case _: error([]);
	};
	VNull))), ::((new Tuple("objgetproto", Fun1(function o:
	switch (vobj(o).oproto) {
case None: VNull;
case Some(p): VObject(p);
	}))), ::((new Tuple("nargs", Fun1(function f: VInt(nargs(vfun(f)))))), ::((new Tuple("call",
			 Fun3(function f: function o: function args: get_ctx([]).do_call(o, f, Array.to_list(varray(args)), p)))),
			 ::((new Tuple("closure", FunVar(function vl:
	switch (vl) {
case ::(VFunction(f), ::(_, _)): VClosure(vl, do_closure);
	case _: exc(VString("Can't create closure : value is not a function"));
	}))), ::((new Tuple("apply", FunVar(function vl:
	switch (vl) {
case ::(f, args): var f = vfun(f);
		VFunction(FunVar(function args2: get_ctx([]).do_call(VNull, VFunction(f), @(args, args2), p)));
	case _: exc(VString("Invalid closure arguments number"));
	}))), ::((new Tuple("varargs", Fun1(function f:
	switch (f) {
case VFunction(FunVar(_)) | VFunction(Fun1(_)) | VClosure(_): VFunction(FunVar(function vl: get_ctx([]).do_call(VNull, f,
				::(VArray(Array.of_list(vl)), []), p)));
	case _: error([]);
	}))), ::((new Tuple("isnan", Fun1(function f:
	switch (f) {
case VFloat(f): VBool(<>(f, f));
	case _: VBool(False);
	}))), ::((new Tuple("isinfinite", Fun1(function f:
	switch (f) {
case VFloat(f): VBool( || ( = (f, infinity), = (f, neg_infinity)));
	case _: VBool(False);
	}))), ::((new Tuple("int", Fun1(function v:
	switch (v) {
case VInt(_) | VInt32(_): v;
	case VFloat(f): best_int(to_int(f));
	case VString(s): try {
			parse_int(s);
		} catch (e: _) {
			VNull;
		};
	case _: VNull;
	}))), ::((new Tuple("float", Fun1(function v:
	switch (v) {
case VInt(i): VFloat(float_of_int(i));
	case VInt32(i): VFloat(Int32.to_float(i));
	case VFloat(_): v;
	case VString(s): try {
			VFloat(parse_float(s));
		} catch (e: _) {
			VNull;
		};
	case _: VNull;
	}))), ::((new Tuple("getkind", Fun1(function v:
	switch (v) {
case VAbstract(a): VAbstract(AKind(a));
	case VInt32(_): VAbstract(AKind(AInt32Kind));
	case _: error([]);
	}))), ::((new Tuple("iskind", Fun2(function v: function k:
	switch ((new Tuple(v, k))) {
case (VAbstract(a), VAbstract(AKind(k))): VBool( = (Obj.tag(Obj.repr(a)), Obj.tag(Obj.repr(k))));
	case (VInt32(_), VAbstract(AKind(AInt32Kind))): VBool(True);
	case (_, VAbstract(AKind(_))): VBool(False);
	case _: error([]);
	}))), ::((new Tuple("hkey", Fun1(function v: VInt(Hashtbl.hash(v))))), ::((new Tuple("hnew",
	Fun1(function v: VAbstract(AHash(switch (v) {
case VNull: Hashtbl.create(0);
	case VInt(n): Hashtbl.create(n);
	case _: error([]);
	}))))), ::((new Tuple("hresize", Fun1(function v: VNull))), ::((new Tuple("hget",
			   Fun3(function h: function k: function cmp:
	if (<>(cmp, VNull)) {
	assert False;
} else {
	[];
	};
	try {
		Hashtbl.find(vhash(h), k);
	} catch (e: Not_found) {
		VNull;
	}))), ::((new Tuple("hmem", Fun3(function h: function k: function cmp:
	if (<>(cmp, VNull)) {
	assert False;
} else {
	[];
	};
	VBool(Hashtbl.mem(vhash(h), k))))), ::((new Tuple("hremove", Fun3(function h: function k: function cmp:
	if (<>(cmp, VNull)) {
	assert False;
} else {
	[];
	};
	var h = vhash(h);
			var old = Hashtbl.mem(h, k);
	if (old) {
	Hashtbl.remove(h, k);
	} else {
		[];
	};
	VBool(old)))), ::((new Tuple("hset", Fun4(function h: function k: function v: function cmp:
	if (<>(cmp, VNull)) {
	assert False;
} else {
	[];
	};
	var h = vhash(h);
			var old = Hashtbl.mem(h, k);
			Hashtbl.replace(h, k, v);
			VBool(!(old))))), ::((new Tuple("hadd", Fun4(function h: function k: function v: function cmp:
	if (<>(cmp, VNull)) {
	assert False;
} else {
	[];
	};
	var h = vhash(h);
			var old = Hashtbl.mem(h, k);
			Hashtbl.add(h, k, v);
			VBool(!(old))))), ::((new Tuple("hiter",
											Fun2(function h: function f: Hashtbl.iter(function k: function v: ignore(get_ctx([]).do_call(VNull, f, ::(k, ::(v, [])),
													p)), vhash(h));
													VNull))), ::((new Tuple("hcount", Fun1(function h: VInt(Hashtbl.length(vhash(h)))))), ::((new Tuple("hsize",
															Fun1(function h: VInt(Hashtbl.length(vhash(h)))))), ::((new Tuple("print",
																	FunVar(function vl: List.iter(function v: var ctx = get_ctx([]);
																			var com = ctx.curapi.get_com([]);
																			com.print(ctx.do_string(v)), vl);
																			VNull))), ::((new Tuple("throw", Fun1(function v: exc(v)))), ::((new Tuple("rethrow",
																					Fun1(function v: var ctx = get_ctx([]);
																							ctx.callstack = @(List.rev(List.map(function p: {
																										() with cpos = p;
																										cthis = ctx.vthis;
																										cstack = DynArray.length(ctx.stack);
																										cenv = ctx.venv
																									}, ctx.exc)), ctx.callstack);
																							exc(v)))), ::((new Tuple("istrue", Fun1(function v:
	switch (v) {
case VNull | VInt(0) | VBool(False) | VInt32(undefined): VBool(False);
	case _: VBool(True);
	}))), ::((new Tuple("not", Fun1(function v:
	switch (v) {
case VNull | VInt(0) | VBool(False) | VInt32(undefined): VBool(True);
	case _: VBool(False);
	}))), ::((new Tuple("typeof", Fun1(function v: VInt(switch (v) {
case VNull: 0;
case VInt(_) | VInt32(_): 1;
	case VFloat(_): 2;
	case VBool(_): 3;
	case VString(_): 4;
	case VObject(_): 5;
	case VArray(_): 6;
	case VFunction(_) | VClosure(_): 7;
	case VAbstract(_): 8;
	})))), ::((new Tuple("compare", Fun2(function a: function b:
	switch (get_ctx([]).do_compare(a, b)) {
case CUndef: VNull;
case CEq: VInt(0);
	case CSup: VInt(1);
	case CInf: VInt(-1);
	}))), ::((new Tuple("pcompare", Fun2(function a: function b: assert False))), ::((new Tuple("excstack",
			 Fun0(function []: build_stack(get_ctx([]).exc)))), ::((new Tuple("callstack",
					 Fun0(function []: build_stack(List.map(function s: s.cpos, get_ctx([]).callstack))))), ::((new Tuple("version",
							 Fun0(function []: VInt(200)))), ::((new Tuple("use_neko_dll", Fun0(function []: VBool(<>(neko,
									 None))))), []))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))));
	var vals = ::((new Tuple("tnull", VInt(0))), ::((new Tuple("tint", VInt(1))), ::((new Tuple("tfloat", VInt(2))),
				  ::((new Tuple("tbool", VInt(3))), ::((new Tuple("tstring", VInt(4))), ::((new Tuple("tobject", VInt(5))),
						  ::((new Tuple("tarray", VInt(6))), ::((new Tuple("tfunction", VInt(7))), ::((new Tuple("tabstract", VInt(8))), [])))))))));
	var h = Hashtbl.create(0);
	List.iter(function (n, f): Hashtbl.add(h, n, VFunction(f)), funcs);
	List.iter(function (n, v): Hashtbl.add(h, n, v), vals);
	h;

	public static function free_abstract(a) return {
		switch (a) {
		case VAbstract(vp): Obj.set_tag(Obj.repr(vp), 0);
		case _: assert False;
		};
	};

	public static var std_lib = var p = { () with psource = "<stdlib>";
										  pline = 0
										};
	function error([]) return {
		raise(Builtin_error);
	};
	function make_list(l) return {
		function loop(acc) return {
		case []: acc;
		case ::(x, l): loop(VArray([x;
			acc]), l);
		};
		loop(VNull, List.rev(l));
	};
	function num(match) return switch (match) {
	case VInt(i): float_of_int(i);
	case VInt32(i): Int32.to_float(i);
	case VFloat(f): f;
	case _: error([]);
	};
	function make_date(f) return {
		VInt32(Int32.of_float(f));
	};
	function date(match) return switch (match) {
	case VInt32(i): Int32.to_float(i);
	case VInt(i): float_of_int(i);
	case _: error([]);
	};
	function make_i32(i) return {
		VInt32(i);
	};
	function int32(match) return switch (match) {
	case VInt(i): Int32.of_int(i);
	case VInt32(i): i;
	case _: error([]);
	};
	function vint(match) return switch (match) {
	case VInt(n): n;
	case _: error([]);
	};
	function vstring(match) return switch (match) {
	case VString(s): s;
	case _: error([]);
	};
	function int32_addr(h) return {
		var base = Int32.to_int(Int32.logand(h, ));
		var str = Printf.sprintf("%ld.%d.%d.%d", Int32.shift_right_logical(h, 24), lsr(base, 16), land(lsr(base, 8), 0xFF), land(base, 0xFF));
		Unix.inet_addr_of_string(str);
	};
	function int32_op(op) return {
		Fun2(function a: function b: make_i32(op(int32(a), int32(b))));
	};
	make_library(@(::((new Tuple("math_atan2", Fun2(function a: function b: VFloat(atan2(num(a), num(b)))))),
					  ::((new Tuple("math_pow", Fun2(function a: function b: VFloat( * * (num(a), num(b)))))), ::((new Tuple("math_abs",
							  Fun1(function v:
	switch (v) {
case VInt(i): VInt(abs(i));
	case VInt32(i): VInt32(Int32.abs(i));
	case VFloat(f): VFloat(abs_float(f));
	case _: error([]);
	}))), ::((new Tuple("math_ceil", Fun1(function v:
	switch (v) {
case VInt(_) | VInt32(_): v;
	case _: best_int(to_int(ceil(num(v))));
	}))), ::((new Tuple("math_floor", Fun1(function v:
	switch (v) {
case VInt(_) | VInt32(_): v;
	case _: best_int(to_int(floor(num(v))));
	}))), ::((new Tuple("math_round", Fun1(function v:
	switch (v) {
case VInt(_) | VInt32(_): v;
	case _: best_int(to_int(floor(+.(num(v), 0.5))));
	}))), ::((new Tuple("math_pi", Fun0(function []: VFloat( * .(4.0, atan(1.0)))))), ::((new Tuple("math_sqrt",
			 Fun1(function v: VFloat(sqrt(num(v)))))), ::((new Tuple("math_atan", Fun1(function v: VFloat(atan(num(v)))))),
					 ::((new Tuple("math_cos", Fun1(function v: VFloat(cos(num(v)))))), ::((new Tuple("math_sin",
							 Fun1(function v: VFloat(sin(num(v)))))), ::((new Tuple("math_tan", Fun1(function v: VFloat(tan(num(v)))))),
									 ::((new Tuple("math_log", Fun1(function v: VFloat(Pervasives.log(num(v)))))), ::((new Tuple("math_exp",
											 Fun1(function v: VFloat(exp(num(v)))))), ::((new Tuple("math_acos", Fun1(function v: VFloat(acos(num(v)))))),
													 ::((new Tuple("math_asin", Fun1(function v: VFloat(asin(num(v)))))), ::((new Tuple("math_fceil",
															 Fun1(function v: VFloat(ceil(num(v)))))), ::((new Tuple("math_ffloor", Fun1(function v: VFloat(floor(num(v)))))),
																	 ::((new Tuple("math_fround", Fun1(function v: VFloat(floor(+.(num(v), 0.5)))))), ::((new Tuple("math_int",
																			 Fun1(function v:
	switch (v) {
case VInt(_) | VInt32(_): v;
	case VFloat(f): best_int(to_int(if ( < (f, 0.)) {
		ceil(f);
		} else {
			floor(f);
		}));
	case _: error([]);
	}))), ::((new Tuple("buffer_new", Fun0(function []: VAbstract(ABuffer(Buffer.create(0)))))), ::((new Tuple("buffer_add",
			 Fun2(function b: function v:
	switch (b) {
case VAbstract(ABuffer(b)): Buffer.add_string(b, get_ctx([]).do_string(v));
		VNull;
	case _: error([]);
	}))), ::((new Tuple("buffer_add_char", Fun2(function b: function v:
	switch ((new Tuple(b, v))) {
case (VAbstract(ABuffer(b)), VInt(n)) if (&&(>=(n, 0), <(n, 256))): Buffer.add_char(b, char_of_int(n));
		VNull;
	case _: error([]);
	}))), ::((new Tuple("buffer_add_sub", Fun4(function b: function s: function p: function l:
	switch ((new Tuple(b, s, p, l))) {
case (VAbstract(ABuffer(b)), VString(s), VInt(p), VInt(l)): try {
			Buffer.add_substring(b, s, p, l);
			VNull;
		} catch (e: _) {
			error([]);
		};
	case _: error([]);
	}))), ::((new Tuple("buffer_string", Fun1(function b:
	switch (b) {
case VAbstract(ABuffer(b)): VString(Buffer.contents(b));
	case _: error([]);
	}))), ::((new Tuple("buffer_reset", Fun1(function b:
	switch (b) {
case VAbstract(ABuffer(b)): Buffer.reset(b);
		VNull;
	case _: error([]);
	}))), ::((new Tuple("buffer_get_length", Fun1(function b:
	switch (b) {
case VAbstract(ABuffer(b)): VInt(Buffer.length(b));
	case _: error([]);
	}))), ::((new Tuple("date_now", Fun0(function []: make_date(Unix.time([]))))), ::((new Tuple("date_new",
	Fun1(function v: make_date(switch (v) {
case VNull: Unix.time([]);
	case VString(s): switch (String.length(s)) {
		case 19: var r =
				Str.regexp("^\\[[0-9][0-9][0-9][0-9]\\]-\\[[0-9][0-9]\\]-\\[[0-9][0-9]\\] \\[[0-9][0-9]\\]:\\[[0-9][0-9]\\]:\\[[0-9][0-9]\\]$");
			if (!(Str.string_match(r, s, 0))) {
				exc(VString( ^ ("Invalid date format : ", s)));
			} else {
				[];
			};
			var t = Unix.localtime(Unix.time([]));
			var t = { (t) with tm_year = -(int_of_string(Str.matched_group(1, s)), 1900);
					  tm_mon = -(int_of_string(Str.matched_group(2, s)), 1);
					  tm_mday = int_of_string(Str.matched_group(3, s));
					  tm_hour = int_of_string(Str.matched_group(4, s));
					  tm_min = int_of_string(Str.matched_group(5, s));
					  tm_sec = int_of_string(Str.matched_group(6, s))
					};
			fst(Unix.mktime(t));
		case 10: assert False;
		case 8: assert False;
		case _: exc(VString( ^ ("Invalid date format : ", s)));
		};
	case _: error([]);
	})))), ::((new Tuple("date_set_hour", Fun4(function d: function h: function m: function s: var d = date(d);
						 var t = Unix.localtime(d);
						 make_date(fst(Unix.mktime({ (t) with tm_hour = vint(h);
									   tm_min = vint(m);
									   tm_sec = vint(s)
												   })))))), ::((new Tuple("date_set_day", Fun4(function d: function y: function m: function da: var d = date(d);
														   var t = Unix.localtime(d);
														   make_date(fst(Unix.mktime({ (t) with tm_year = -(vint(y), 1900);
																   tm_mon = -(vint(m), 1);
																   tm_mday = vint(da)
																					 })))))), ::((new Tuple("date_format", Fun2(function d: function fmt:
	switch (fmt) {
case VNull: var t = Unix.localtime(date(d));
		VString(Printf.sprintf("%.4d-%.2d-%.2d %.2d:%.2d:%.2d", +(t.tm_year, 1900), +(t.tm_mon, 1), t.tm_mday, t.tm_hour, t.tm_min,
							   t.tm_sec));
	case VString(%w): var t = Unix.localtime(date(d));
		VString(string_of_int(t.tm_wday));
	case VString(_): exc(VString("Custom date format is not supported"));
	case _: error([]);
	}))), ::((new Tuple("date_get_hour", Fun1(function d: var t = Unix.localtime(date(d));
						var o = obj(hash_field(get_ctx([])), ::((new Tuple("h", VInt(t.tm_hour))), ::((new Tuple("m", VInt(t.tm_min))),
									::((new Tuple("s", VInt(t.tm_sec))), []))));
						VObject(o)))), ::((new Tuple("date_get_day", Fun1(function d: var t = Unix.localtime(date(d));
										   var o = obj(hash_field(get_ctx([])), ::((new Tuple("d", VInt(t.tm_mday))), ::((new Tuple("m", VInt(+(t.tm_mon, 1)))),
												   ::((new Tuple("y", VInt(+(t.tm_year, 1900)))), []))));
	VObject(o)))), ::((new Tuple("string_split", Fun2(function s: function d: make_list(switch ((new Tuple(s, d))) {
case (VString(), VString(_)): ::(VString(""), []);
	case (VString(s), VString()): Array.to_list(Array.init(String.length(s), function i: VString(String.make(1, String.get(s,
				i)))));
	case (VString(s), VString(d)): List.map(function s: VString(s), ExtString.String.nsplit(s, d));
	case _: error([]);
	})))), ::((new Tuple("url_encode", Fun1(function s: var s = vstring(s);
											var b = Buffer.create(0);
											var hex = "0123456789ABCDEF";
	for (i in /*to*/0... - (String.length(s), 1)) {
	var c = String.unsafe_get(s, i);
		switch (c) {
		case 'A' .. 'Z' | 'a' .. 'z' | '0' .. '9' | '_' | '-' | '.': Buffer.add_char(b, c);
		case _: Buffer.add_char(b, '%');
			Buffer.add_char(b, String.unsafe_get(hex, lsr(int_of_char(c), 4)));
			Buffer.add_char(b, String.unsafe_get(hex, land(int_of_char(c), 0xF)));
		};
	};
	VString(Buffer.contents(b))))), ::((new Tuple("url_decode", Fun1(function s : var s = vstring(s);
										var b = Buffer.create(0);
										var len = String.length(s);
	function decode(c) return {
		switch (c) {
		case '0' .. '9': Some(-(int_of_char(c), int_of_char('0')));
		case 'a' .. 'f': Some(+(-(int_of_char(c), int_of_char('a')), 10));
		case 'A' .. 'F': Some(+(-(int_of_char(c), int_of_char('A')), 10));
		case _: None;
		};
	};
	function loop(i) return {
		if ( = (i, len)) {
			[];
		} else {
			var c = String.unsafe_get(s, i);
			switch (c) {
			case '%': var p1 = try {
					decode(String.get(s, +(i, 1)));
				} catch (e: _) {
					None;
				};
				var p2 = try {
					decode(String.get(s, +(i, 2)));
				} catch (e: _) {
					None;
				};
				switch ((new Tuple(p1, p2))) {
				case (Some(c1), Some(c2)): Buffer.add_char(b, char_of_int(lor(lsl(c1, 4), c2)));
					loop(+(i, 3));
				case _: loop(+(i, 1));
				};
			case '+': Buffer.add_char(b, ' ');
				loop(+(i, 1));
			case c: Buffer.add_char(b, c);
				loop(+(i, 1));
			};
		};
	};
	loop(0);
	VString(Buffer.contents(b))))), ::((new Tuple("base_encode", Fun2(function s : function b :
	switch ((new Tuple(s, b))) {
case (VString(s), VString(0123456789abcdef)) if (=(String.length(s), 16)): VString(Digest.to_hex(s));
	case (VString(s), VString(b)): if (<>(String.length(b), 64)) {
			assert False;
		} else {
			[];
		};
		var tbl = Array.init(64, String.unsafe_get(b));
		VString(Base64.str_encode(tbl = , s));
	case _: error([]);
	}))), ::((new Tuple("base_decode", Fun2(function s: function b: var s = vstring(s);
											var b = vstring(b);
	if (<>(String.length(b), 64)) {
	assert False;
} else {
	[];
	};
	var tbl = Array.init(64, String.unsafe_get(b));
			  VString(Base64.str_decode(tbl = Base64.make_decoding_table(tbl), s))))), ::((new Tuple("make_md5",
					  Fun1(function s: VString(Digest.string(vstring(s)))))), ::((new Tuple("int32_new", Fun1(function v:
	switch (v) {
case VInt32(_): v;
	case VInt(i): make_i32(Int32.of_int(i));
	case VFloat(f): make_i32(Int32.of_float(f));
	case _: error([]);
	}))), ::((new Tuple("int32_to_int", Fun1(function v: var v = int32(v);
						var i = Int32.to_int(v);
	if (<>(Int32.compare(Int32.of_int(i), v), 0)) {
	error([]);
	} else {
		[];
	};
	VInt(i)))), ::((new Tuple("int32_to_float", Fun1(function v: VFloat(Int32.to_float(int32(v)))))),
				   ::((new Tuple("int32_compare", Fun2(function a: function b: VInt(Int32.compare(int32(a), int32(b)))))),
					  ::((new Tuple("int32_add", int32_op(Int32.add))), ::((new Tuple("int32_sub", int32_op(Int32.sub))),
							  ::((new Tuple("int32_mul", int32_op(Int32.mul))), ::((new Tuple("int32_div", int32_op(Int32.div))),
									  ::((new Tuple("int32_shl", int32_op(function a: function b: Int32.shift_left(a, Int32.to_int(b))))),
										 ::((new Tuple("int32_shr", int32_op(function a: function b: Int32.shift_right(a, Int32.to_int(b))))),
											::((new Tuple("int32_ushr", int32_op(function a: function b: Int32.shift_right_logical(a, Int32.to_int(b))))),
													::((new Tuple("int32_mod", int32_op(Int32.rem))), ::((new Tuple("int32_or", int32_op(Int32.logor))),
															::((new Tuple("int32_and", int32_op(Int32.logand))), ::((new Tuple("int32_xor", int32_op(Int32.logxor))),
																	::((new Tuple("int32_neg", Fun1(function v: make_i32(Int32.neg(int32(v)))))), ::((new Tuple("int32_complement",
																			Fun1(function v: make_i32(Int32.lognot(int32(v)))))), ::((new Tuple("same_closure",
	Fun2(function a: function b: VBool(switch ((new Tuple(a, b))) {
case (VClosure(la, fa), VClosure(lb, fb)): && ( == (fa, fb),
				&& ( = (List.length(la), List.length(lb)), List.for_all2(function a: function b: = (get_ctx([]).do_compare(a, b), CEq), la,
						lb)));
	case (VFunction(a), VFunction(b)): == (a, b);
	case _: False;
	})))), ::((new Tuple("double_bytes", Fun2(function f: function big: var f = switch (f) {
case VFloat(f): f;
	case VInt(i): float_of_int(i);
	case _: error([]);
	};
	switch (big) {
case VBool(big): var ch = IO.output_string([]);
		if (big) {
			IO.BigEndian.write_double(ch, f);
		} else {
			IO.write_double(ch, f);
		};
		VString(IO.close_out(ch));
	case _: error([]);
	}))), ::((new Tuple("float_bytes", Fun2(function f: function big: var f = switch (f) {
case VFloat(f): f;
	case VInt(i): float_of_int(i);
	case _: error([]);
	};
	switch (big) {
case VBool(big): var ch = IO.output_string([]);
		var i = Int32.bits_of_float(f);
		if (big) {
			IO.BigEndian.write_real_i32(ch, i);
		} else {
			IO.write_real_i32(ch, i);
		};
		VString(IO.close_out(ch));
	case _: error([]);
	}))), ::((new Tuple("double_of_bytes", Fun2(function s: function big:
	switch ((new Tuple(s, big))) {
case (VString(s), VBool(big)) if (=(String.length(s), 8)): var ch = IO.input_string(s);
		VFloat(if (big) {
		IO.BigEndian.read_double(ch);
		} else {
			IO.read_double(ch);
		});
	case _: error([]);
	}))), ::((new Tuple("float_of_bytes", Fun2(function s: function big:
	switch ((new Tuple(s, big))) {
case (VString(s), VBool(big)) if (=(String.length(s), 4)): var ch = IO.input_string(s);
		VFloat(Int32.float_of_bits(if (big) {
		IO.BigEndian.read_real_i32(ch);
		} else {
			IO.read_real_i32(ch);
		}));
	case _: error([]);
	}))), ::((new Tuple("random_new", Fun0(function []: VAbstract(ARandom(ref(Random.State.make_self_init([]))))))),
			 ::((new Tuple("random_set_seed", Fun2(function r: function s:
	switch ((new Tuple(r, s))) {
case (VAbstract(ARandom(r)), VInt(seed)): r.val = Random.State.make([seed]);
		VNull;
	case (VAbstract(ARandom(r)), VInt32(seed)): r.val = Random.State.make([Int32.to_int(seed)]);
		VNull;
	case _: error([]);
	}))), ::((new Tuple("random_int", Fun2(function r: function s:
	switch ((new Tuple(r, s))) {
case (VAbstract(ARandom(r)), VInt(max)): VInt(Random.State.int(r.val, if ( <= (max, 0)) {
		1;
	} else {
		max;
	}));
	case _: error([]);
	}))), ::((new Tuple("random_float", Fun1(function r:
	switch (r) {
case VAbstract(ARandom(r)): VFloat(Random.State.float(r.val, 1.0));
	case _: error([]);
	}))), ::((new Tuple("file_open", Fun2(function f: function r:
	switch ((new Tuple(f, r))) {
case (VString(f), VString(r)): var perms = 0o666;
		VAbstract(switch (r) {
	case r: AFRead(open_in_gen(::(Open_rdonly, []), 0, f), ref(False));
		case rb: AFRead(open_in_gen(::(Open_rdonly, ::(Open_binary, [])), 0, f), ref(False));
		case w: AFWrite(open_out_gen(::(Open_wronly, ::(Open_creat, ::(Open_trunc, []))), perms, f));
		case wb: AFWrite(open_out_gen(::(Open_wronly, ::(Open_creat, ::(Open_trunc, ::(Open_binary, [])))), perms, f));
		case a: AFWrite(open_out_gen(::(Open_append, []), perms, f));
		case ab: AFWrite(open_out_gen(::(Open_append, ::(Open_binary, [])), perms, f));
		case _: error([]);
		});
	case _: error([]);
	}))), ::((new Tuple("file_close", Fun1(function vf:
	switch (vf) {
case VAbstract(AFRead(f, _)): close_in(f);
		free_abstract(vf);
	case VAbstract(AFWrite(f)): close_out(f);
		free_abstract(vf);
	case _: error([]);
	};
	VNull))), ::((new Tuple("file_write", Fun4(function f: function s: function p: function l:
	switch ((new Tuple(f, s, p, l))) {
case (VAbstract(AFWrite(f)), VString(s), VInt(p), VInt(l)): output(f, s, p, l);
		VInt(l);
	case _: error([]);
	}))), ::((new Tuple("file_read", Fun4(function f: function s: function p: function l:
	switch ((new Tuple(f, s, p, l))) {
case (VAbstract(AFRead(f, r)), VString(s), VInt(p), VInt(l)): var n = input(f, s, p, l);
		if ( = (n, 0)) {
			r.val = True;
			exc(VArray([VString("file_read")]));
		} else {
			[];
		};
		VInt(n);
	case _: error([]);
	}))), ::((new Tuple("file_write_char", Fun2(function f: function c:
	switch ((new Tuple(f, c))) {
case (VAbstract(AFWrite(f)), VInt(c)): output_char(f, char_of_int(c));
		VNull;
	case _: error([]);
	}))), ::((new Tuple("file_read_char", Fun1(function f:
	switch (f) {
case VAbstract(AFRead(f, r)): VInt(int_of_char(try {
			input_char(f);
		} catch (e: _) {
			r.val = True;
			exc(VArray([VString("file_read_char")]));
		}));
	case _: error([]);
	}))), ::((new Tuple("file_seek", Fun3(function f: function pos: function mode:
	switch ((new Tuple(f, pos, mode))) {
case (VAbstract(AFRead(f, r)), VInt(pos), VInt(mode)): r.val = False;
		seek_in(f, switch (mode) {
	case 0: pos;
	case 1: +(pos_in(f), pos);
		case 2: +(in_channel_length(f), pos);
		case _: error([]);
		});
		VNull;
	case (VAbstract(AFWrite(f)), VInt(pos), VInt(mode)): seek_out(f, switch (mode) {
	case 0: pos;
	case 1: +(pos_out(f), pos);
		case 2: +(out_channel_length(f), pos);
		case _: error([]);
		});
		VNull;
	case _: error([]);
	}))), ::((new Tuple("file_tell", Fun1(function f:
	switch (f) {
case VAbstract(AFRead(f, _)): VInt(pos_in(f));
	case VAbstract(AFWrite(f)): VInt(pos_out(f));
	case _: error([]);
	}))), ::((new Tuple("file_eof", Fun1(function f:
	switch (f) {
case VAbstract(AFRead(f, r)): VBool(r.val);
	case _: error([]);
	}))), ::((new Tuple("file_flush", Fun1(function f:
	switch (f) {
case VAbstract(AFWrite(f)): flush(f);
	case _: error([]);
	};
	VNull))), ::((new Tuple("file_contents", Fun1(function f:
	switch (f) {
case VString(f): VString(Std.input_file(bin = True, f));
	case _: error([]);
	}))), ::((new Tuple("file_stdin", Fun0(function []: VAbstract(AFRead(Pervasives.stdin, ref(False)))))),
			 ::((new Tuple("file_stdout", Fun0(function []: VAbstract(AFWrite(Pervasives.stdout))))), ::((new Tuple("file_stderr",
					 Fun0(function []: VAbstract(AFWrite(Pervasives.stderr))))), ::((new Tuple("socket_init", Fun0(function []: VNull))),
							 ::((new Tuple("socket_new", Fun1(function v:
	switch (v) {
case VBool(b): VAbstract(ASocket(Unix.socket(PF_INET, if (b) {
		SOCK_DGRAM;
	} else {
		SOCK_STREAM;
	}, 0)));
	case _: error([]);
	}))), ::((new Tuple("socket_close", Fun1(function vs:
	switch (vs) {
case VAbstract(ASocket(s)): Unix.close(s);
		free_abstract(vs);
		VNull;
	case _: error([]);
	}))), ::((new Tuple("socket_send_char", Fun2(function s: function c:
	switch ((new Tuple(s, c))) {
case (VAbstract(ASocket(s)), VInt(c)) if (&&(>=(c, 0), <=(c, 255))): ignore(Unix.send(s, String.make(1, char_of_int(c)), 0,
				1, []));
		VNull;
	case _: error([]);
	}))), ::((new Tuple("socket_send", Fun4(function s: function buf: function pos: function len:
	switch ((new Tuple(s, buf, pos, len))) {
case (VAbstract(ASocket(s)), VString(buf), VInt(pos), VInt(len)): VInt(Unix.send(s, buf, pos, len, []));
	case _: error([]);
	}))), ::((new Tuple("socket_recv", Fun4(function s: function buf: function pos: function len:
	switch ((new Tuple(s, buf, pos, len))) {
case (VAbstract(ASocket(s)), VString(buf), VInt(pos), VInt(len)): VInt(Unix.recv(s, buf, pos, len, []));
	case _: error([]);
	}))), ::((new Tuple("socket_recv_char", Fun1(function s:
	switch (s) {
case VAbstract(ASocket(s)): var buf = String.make(1, '\000');
		ignore(Unix.recv(s, buf, 0, 1, []));
		VInt(int_of_char(String.unsafe_get(buf, 0)));
	case _: error([]);
	}))), ::((new Tuple("socket_write", Fun2(function s: function str:
	switch ((new Tuple(s, str))) {
case (VAbstract(ASocket(s)), VString(str)): var pos = ref(0);
		var len = ref(String.length(str));
		> (len.val, 0)var k = Unix.send(s, str, pos.val, len.val, []);
		pos.val = +(pos.val, k);
		len.val = -(len.val, k);
		VNull;
	case _: error([]);
	}))), ::((new Tuple("socket_read", Fun1(function s:
	switch (s) {
case VAbstract(ASocket(s)): var tmp = String.make(1024, '\000');
		var buf = Buffer.create(0);
		function loop([]) return {
			var k = try {
				Unix.recv(s, tmp, 0, 1024, []);
			} catch (e: Unix_error(_)) {
				0;
			};
			if ( > (k, 0)) {
				Buffer.add_substring(buf, tmp, 0, k);
				loop([]);
			} else {
				[];
			};
		};
		loop([]);
		VString(Buffer.contents(buf));
	case _: error([]);
	}))), ::((new Tuple("host_resolve", Fun1(function s: var h = try {
		Unix.gethostbyname(vstring(s));
	} catch (e: Not_found) {
		error([]);
	};
	var addr = Unix.string_of_inet_addr(h.h_addr_list0);
			   var Tuple(a, b, c, d) = Scanf.sscanf(addr, "%d.%d.%d.%d", function a: function b: function c: function d: (new Tuple(a, b,
									   c, d)));
			   VInt32(Int32.logor(Int32.shift_left(Int32.of_int(a), 24), Int32.of_int(lor(lor(d, lsl(c, 8)), lsl(b, 16)))))))),
	::((new Tuple("host_to_string", Fun1(function h:
	switch (h) {
case VInt32(h): VString(Unix.string_of_inet_addr(int32_addr(h)));
	case _: error([]);
	}))), ::((new Tuple("host_reverse", Fun1(function h:
	switch (h) {
case VInt32(h): VString(gethostbyaddr(int32_addr(h)).h_name);
	case _: error([]);
	}))), ::((new Tuple("host_local", Fun0(function []: VString(Unix.gethostname([]))))), ::((new Tuple("socket_connect",
			 Fun3(function s: function h: function p:
	switch ((new Tuple(s, h, p))) {
case (VAbstract(ASocket(s)), VInt32(h), VInt(p)): Unix.connect(s, ADDR_INET(int32_addr(h), p));
		VNull;
	case _: error([]);
	}))), ::((new Tuple("socket_listen", Fun2(function s: function l:
	switch ((new Tuple(s, l))) {
case (VAbstract(ASocket(s)), VInt(l)): Unix.listen(s, l);
		VNull;
	case _: error([]);
	}))), ::((new Tuple("socket_set_timeout", Fun2(function s: function t:
	switch (s) {
case VAbstract(ASocket(s)): var t = switch (t) {
		case VNull: 0.;
		case VInt(t): float_of_int(t);
		case VFloat(f): f;
		case _: error([]);
		};
		Unix.setsockopt_float(s, SO_RCVTIMEO, t);
		Unix.setsockopt_float(s, SO_SNDTIMEO, t);
		VNull;
	case _: error([]);
	}))), ::((new Tuple("socket_shutdown", Fun3(function s: function r: function w:
	switch ((new Tuple(s, r, w))) {
case (VAbstract(ASocket(s)), VBool(r), VBool(w)): Unix.shutdown(s, switch ((new Tuple(r, w))) {
	case (True, True): SHUTDOWN_ALL;
		case (True, False): SHUTDOWN_RECEIVE;
		case (False, True): SHUTDOWN_SEND;
		case _: error([]);
		});
		VNull;
	case _: error([]);
	}))), ::((new Tuple("get_env", Fun1(function v:
	try {
		VString(Unix.getenv(vstring(v)));
	} catch (e: _) {
		VNull;
	}))), ::((new Tuple("put_env", Fun2(function e: function v: Unix.putenv(vstring(e), vstring(v));
										VNull))), ::((new Tuple("sys_sleep", Fun1(function f:
	switch (f) {
case VFloat(f): ignore(Unix.select([], [], [], f));
		VNull;
	case _: error([]);
	}))), ::((new Tuple("set_time_locale", Fun1(function l:
	switch (l) {
case VString(s): VBool(False);
	case _: error([]);
	}))), ::((new Tuple("get_cwd", Fun0(function []: var dir = Unix.getcwd([]);
										var l = String.length(dir);
	VString(if ( = (l, 0)) {
	"./";
} else {
	switch (dir - (l, 1)) {
		case '/' | '\\': dir;
		case _: ^ (dir, "/");
		};
	})))), ::((new Tuple("set_cwd", Fun1(function s: Unix.chdir(vstring(s));
										 VNull))), ::((new Tuple("sys_string", var cached_sys_name = ref(None);
	Fun0(function []: VString(switch (Sys.os_type) {
case Unix: switch (cached_sys_name.val) {
		case Some(n): n;
		case None: var ic = Unix.open_process_in("uname");
			var uname = switch (input_line(ic)) {
			case Darwin: "Mac";
			case n: n;
			};
			close_in(ic);
			cached_sys_name.val = Some(uname);
			uname;
		};
	case Win32 | Cygwin: "Windows";
	case s: s;
	})))), ::((new Tuple("sys_is64", Fun0(function []: VBool( = (Sys.word_size, 64))))), ::((new Tuple("sys_command",
			  Fun1(function cmd: VInt(get_ctx([]).curapi.get_com([]).run_command(vstring(cmd)))))), ::((new Tuple("sys_exit",
					  Fun1(function code:
	if (get_ctx([]).curapi.use_cache([])) {
	raise(Typecore.Fatal_error("", Ast.null_pos));
	} else {
		[];
	};
	raise(Sys_exit(vint(code)))))), ::((new Tuple("sys_exists", Fun1(function file: VBool(Sys.file_exists(vstring(file)))))),
									   ::((new Tuple("file_delete", Fun1(function file: Sys.remove(vstring(file));
											   VNull))), ::((new Tuple("sys_rename", Fun2(function file: function target: Sys.rename(vstring(file), vstring(target));
													   VNull))), ::((new Tuple("sys_stat", Fun1(function file: var s = Unix.stat(vstring(file));
															   VObject(obj(hash_field(get_ctx([])), ::((new Tuple("gid", VInt(s.st_gid))), ::((new Tuple("uid", VInt(s.st_uid))),
																	   ::((new Tuple("atime", VInt32(Int32.of_float(s.st_atime)))), ::((new Tuple("mtime", VInt32(Int32.of_float(s.st_mtime)))),
																			   ::((new Tuple("ctime", VInt32(Int32.of_float(s.st_ctime)))), ::((new Tuple("dev", VInt(s.st_dev))), ::((new Tuple("ino",
																					   VInt(s.st_ino))), ::((new Tuple("nlink", VInt(s.st_nlink))), ::((new Tuple("rdev", VInt(s.st_rdev))), ::((new Tuple("size",
																							   VInt(s.st_size))), ::((new Tuple("mode", VInt(s.st_perm))), [])))))))))))))))), ::((new Tuple("sys_file_type",
	Fun1(function file: VString(switch (Unix.stat(vstring(file)).st_kind) {
case S_REG: "file";
case S_DIR: "dir";
case S_CHR: "char";
case S_BLK: "block";
case S_LNK: "symlink";
case S_FIFO: "fifo";
case S_SOCK: "sock";
})))), ::((new Tuple("sys_create_dir", Fun2(function dir: function mode: Unix.mkdir(vstring(dir), vint(mode));
					 VNull))), ::((new Tuple("sys_remove_dir", Fun1(function dir: Unix.rmdir(vstring(dir));
								   VNull))), ::((new Tuple("sys_time", Fun0(function []: VFloat(Unix.gettimeofday([]))))), ::((new Tuple("sys_cpu_time",
										   Fun0(function []: VFloat(Sys.time([]))))), ::((new Tuple("sys_read_dir",
												   Fun1(function dir: var d = Sys.readdir(vstring(dir));
function loop(acc, i) return {
	if ( < (i, 0)) {
			acc;
		} else {
			loop(VArray([VString(di);
			acc]), -(i, 1));
		};
	};
	loop(VNull, -(Array.length(d), 1))))), ::((new Tuple("file_full_path", Fun1(function file: VString(try {
		Extc.get_full_path(vstring(file));
	} catch (e: _) {
		error([]);
	})))), ::((new Tuple("sys_exe_path", Fun0(function []: VString(Sys.argv0)))), ::((new Tuple("sys_env",
			  Fun0(function []: var env = Unix.environment([]);
	function loop(acc, i) return {
		if ( < (i, 0)) {
			acc;
		} else {
			var Tuple(e, v) = ExtString.String.split(envi, "=");
			loop(VArray([VString(e);
			VString(v);
			acc]), -(i, 1));
		};
	};
	loop(VNull, -(Array.length(env), 1))))), ::((new Tuple("sys_getch", Fun1(function echo:
	switch (echo) {
case VBool(b): VInt(Extc.getch(b));
	case _: error([]);
	}))), ::((new Tuple("sys_get_pid", Fun0(function []: VInt(Unix.getpid([]))))), ::((new Tuple("utf8_buf_alloc",
			 Fun1(function v: VAbstract(AUtf8(UTF8.Buf.create(vint(v))))))), ::((new Tuple("utf8_buf_add", Fun2(function b: function c:
	switch (b) {
case VAbstract(AUtf8(buf)): UTF8.Buf.add_char(buf, UChar.chr_of_uint(vint(c)));
		VNull;
	case _: error([]);
	}))), ::((new Tuple("utf8_buf_content", Fun1(function b:
	switch (b) {
case VAbstract(AUtf8(buf)): VString(UTF8.Buf.contents(buf));
	case _: error([]);
	}))), ::((new Tuple("utf8_buf_length", Fun1(function b:
	switch (b) {
case VAbstract(AUtf8(buf)): VInt(UTF8.length(UTF8.Buf.contents(buf)));
	case _: error([]);
	}))), ::((new Tuple("utf8_buf_size", Fun1(function b:
	switch (b) {
case VAbstract(AUtf8(buf)): VInt(String.length(UTF8.Buf.contents(buf)));
	case _: error([]);
	}))), ::((new Tuple("utf8_validate", Fun1(function s: VBool(try {
		UTF8.validate(vstring(s));
		True;
	} catch (e: UTF8.Malformed_code) {
		False;
	})))), ::((new Tuple("utf8_length", Fun1(function s: VInt(UTF8.length(vstring(s)))))), ::((new Tuple("utf8_sub",
			  Fun3(function s: function p: function l: var buf = UTF8.Buf.create(0);
				   var pos = ref(-1);
				   var p = vint(p);
				   var l = vint(l);
				   UTF8.iter(function c: incr(pos);
	if ( && ( >= (pos.val, p), < (pos.val, +(p, l)))) {
	UTF8.Buf.add_char(buf, c);
	} else {
		[];
	}, vstring(s));
	VString(UTF8.Buf.contents(buf))))), ::((new Tuple("utf8_get", Fun2(function s: function p: VInt(UChar.uint_code(try {
		UTF8.get(vstring(s), vint(p));
	} catch (e: _) {
		error([]);
	}))))), ::((new Tuple("utf8_iter", Fun2(function s: function f: var ctx = get_ctx([]);
											UTF8.iter(function c: ignore(ctx.do_call(VNull, f, ::(VInt(UChar.uint_code(c)), []), p)), vstring(s));
											VNull))), ::((new Tuple("utf8_compare", Fun2(function s1: function s2: VInt(UTF8.compare(vstring(s1), vstring(s2)))))),
													::((new Tuple("thread_create", Fun2(function f: function p: exc(VString("Can't create thread from within a macro"))))),
															::((new Tuple("tls_create", Fun0(function []: VAbstract(ATls(ref(VNull)))))), ::((new Tuple("tls_get", Fun1(function t:
	switch (t) {
case VAbstract(ATls(r)): r.val;
	case _: error([]);
	}))), ::((new Tuple("tls_set", Fun2(function t: function v:
	switch (t) {
case VAbstract(ATls(r)): r.val = v;
		VNull;
	case _: error([]);
	}))), ::((new Tuple("process_run", Fun2(function p: function args:
	switch ((new Tuple(p, args))) {
case (VString(p), VArray(args)): VAbstract(AProcess(Process.run(p, Array.map(vstring, args))));
	case _: error([]);
	}))), ::((new Tuple("process_stdout_read", Fun4(function p: function str: function pos: function len:
	switch ((new Tuple(p, str, pos, len))) {
case (VAbstract(AProcess(p)), VString(str), VInt(pos), VInt(len)): VInt(Process.read_stdout(p, str, pos, len));
	case _: error([]);
	}))), ::((new Tuple("process_stderr_read", Fun4(function p: function str: function pos: function len:
	switch ((new Tuple(p, str, pos, len))) {
case (VAbstract(AProcess(p)), VString(str), VInt(pos), VInt(len)): VInt(Process.read_stderr(p, str, pos, len));
	case _: error([]);
	}))), ::((new Tuple("process_stdin_write", Fun4(function p: function str: function pos: function len:
	switch ((new Tuple(p, str, pos, len))) {
case (VAbstract(AProcess(p)), VString(str), VInt(pos), VInt(len)): VInt(Process.write_stdin(p, str, pos, len));
	case _: error([]);
	}))), ::((new Tuple("process_stdin_close", Fun1(function p:
	switch (p) {
case VAbstract(AProcess(p)): Process.close_stdin(p);
		VNull;
	case _: error([]);
	}))), ::((new Tuple("process_exit", Fun1(function p:
	switch (p) {
case VAbstract(AProcess(p)): VInt(Process.exit(p));
	case _: error([]);
	}))), ::((new Tuple("process_pid", Fun1(function p:
	switch (p) {
case VAbstract(AProcess(p)): VInt(Process.pid(p));
	case _: error([]);
	}))), ::((new Tuple("process_close", Fun1(function vp:
	switch (vp) {
case VAbstract(AProcess(p)): Process.close(p);
		free_abstract(vp);
		VNull;
	case _: error([]);
	}))), ::((new Tuple("parse_xml", switch (neko) {
case None: Fun2(function str: function o: switch ((new Tuple(str, o))) {
	case (VString(str), VObject(events)): var ctx = get_ctx([]);
			var p = { () with psource = "parse_xml";
					  pline = 0
					};
			var xml = get_field(events, hash("xml"));
			var don = get_field(events, hash("done"));
			var pcdata = get_field(events, hash("pcdata"));
			function loop(match) return switch (match) {
			case Xml.Element(node, attribs, children): ignore(ctx.do_call(o, xml, ::(VString(node), ::(VObject(obj(hash_field(ctx),
						List.map(function (a, v): (new Tuple(a, VString(v))), attribs))), [])), p));
				List.iter(loop, children);
				ignore(ctx.do_call(o, don, [], p));
			case Xml.PCData(s): ignore(ctx.do_call(o, pcdata, ::(VString(s), []), p));
			};
			var x = XmlParser.make([]);
			XmlParser.check_eof(x, False);
			loop(try {
				XmlParser.parse(x, XmlParser.SString(str));
			} catch (e: T) {
				McOr(McArr(PaApp(PaId(IdAcc(<...>, <...>)), PaId(IdLid(<...>))), ExNil, ExApp(ExId(IdLid(<...>)), ExApp(ExApp(<...>,
						   <...>), ExApp(<...>, <...>)))), McArr(PaId(IdLid(e)), ExNil, ExApp(ExId(IdLid(<...>)), ExApp(ExApp(<...>, <...>),
							   ExApp(<...>, <...>)))))						case Xml.Error(e): failwith( ^ ("Parser failure [", ^ (Xml.error(e), "]")));
			case e: failwith( ^ ("Parser failure [", ^ (Printexc.to_string(e), "]")));
			});
			VNull;
		case _: error([]);
		});
	case Some(neko): var parse_xml = neko.load("std@parse_xml", 2);
		Fun2(function str: function o: neko.call(parse_xml, ::(str, ::(o, []))));
	})), [])))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))),
	switch (neko) {
case None: [];
	case Some(neko): var win_ec = try {
			Some(neko.load("std@win_env_changed", 0));
		} catch (e: _) {
			None;
		};
		::((new Tuple("win_env_changed", Fun0(function []:
		switch (win_ec) {
	case None: error([]);
		case Some(f): neko.call(f, []);
		}))), []);
	}));

	public static var reg_lib = 	function error([]) return {
		raise(Builtin_error);
	};
	var neko = switch (neko) {
	case None: None;
	case Some(neko): try {
			ignore(neko.load("regexp@regexp_new_options", 2));
			Some(neko);
		} catch (e: _) {
			None;
		};
	};
	switch (neko) {
	case None: make_library(::((new Tuple("regexp_new_options", Fun2(function str: function opt: switch ((new Tuple(str,
		opt))) {
	case (VString(str), VString(opt)): var case_sensitive = ref(True);
		List.iter(function case 'm': [];
					  case 'i': case_sensitive.val = False;
						  case c: failwith( ^ ("Unsupported regexp option '", ^ (String.make(1, c), "'"))), ExtString.String.explode(opt));
			var buf = Buffer.create(0);
			function loop(prev, esc) return {
			case []: [];
			case ::(c, l) if (esc):
				switch (c) {
				case 'n': Buffer.add_char(buf, '\n');
				case 'r': Buffer.add_char(buf, '\r');
				case 't': Buffer.add_char(buf, '\t');
				case 'd': Buffer.add_string(buf, "[0-9]");
				case '\\': Buffer.add_string(buf, "\\\\");
				case '[' | ']': Buffer.add_char(buf, c);
				case '1' .. '9' | '+' | '$' | '^' | '*' | '?' | '.' | '[' | ']': Buffer.add_char(buf, '\\');
					Buffer.add_char(buf, c);
				case _: failwith( ^ ("Unsupported escaped char '", ^ (String.make(1, c), "'")));
				};
				loop(c, False, l);
			case ::(c, l): switch (c) {
				case '\\': loop(prev, True, l);
				case '[' | '|' | ']': Buffer.add_char(buf, '\\');
					Buffer.add_char(buf, c);
					loop(c, False, l);
				case '?' if (&&(=(prev, '['), switch (l) {
						case ::(':', _): True;
							case _: False;
							})): failwith("Non capturing groups '[?:' are not supported in macros");
				case '?' if (=(prev, '*')): failwith("Ungreedy *? are not supported in macros");
				case _: Buffer.add_char(buf, c);
					loop(c, False, l);
				};
			};
			loop('\000', False, ExtString.String.explode(str));
			var str = Buffer.contents(buf);
			var r = { () with r = if (case_sensitive.val) {
			Str.regexp(str);
			} else {
				Str.regexp_case_fold(str);
			};
			r_string = "";
					   r_groups = []
					};
			VAbstract(AReg(r));
		case _: error([]);
		}))), ::((new Tuple("regexp_match", Fun4(function r: function str: function pos: function len:
		switch ((new Tuple(r, str, pos, len))) {
	case (VAbstract(AReg(r)), VString(str), VInt(pos), VInt(len)): var Tuple(nstr, npos, delta) = if ( = (len,
			-(String.length(str), pos))) {
				(new Tuple(str, pos, 0));
			} else {
				(new Tuple(String.sub(str, pos, len), 0, pos));
			};
			try {
				ignore(Str.search_forward(r.r, nstr, npos));
				function loop(n) return {
					if ( = (n, 9)) {
						[];
					} else {
						try {
							::(Some(+(Str.group_beginning(n), delta), +(Str.group_end(n), delta)), loop(+(n, 1)));
						} catch (e: T) {
							McOr(McArr(PaId(IdUid(Not_found)), ExNil, ExApp(ExApp(ExId(<...>), ExId(<...>)), ExApp(ExId(<...>), ExApp(<...>, <...>)))),
						McArr(PaApp(PaId(IdUid(<...>)), PaAny), ExNil, ExId(IdUid([]))))									case Not_found: ::(None, loop(+(n, 1)));
						case Invalid_argument(_): [];
						};
					};
				};
				r.r_string = str;
				r.r_groups = Array.of_list(loop(0));
				VBool(True);
			} catch (e: Not_found) {
				VBool(False);
			};
		case _: error([]);
		}))), ::((new Tuple("regexp_matched", Fun2(function r: function n:
		switch ((new Tuple(r, n))) {
	case (VAbstract(AReg(r)), VInt(n)): switch (try {
				r.r_groupsn;
			} catch (e: _) {
				failwith( ^ ("Invalid group ", string_of_int(n)));
				}) {
			case None: VNull;
			case Some(pos, pend): VString(String.sub(r.r_string, pos, -(pend, pos)));
			};
		case _: error([]);
		}))), ::((new Tuple("regexp_matched_pos", Fun2(function r: function n:
		switch ((new Tuple(r, n))) {
	case (VAbstract(AReg(r)), VInt(n)): switch (try {
				r.r_groupsn;
			} catch (e: _) {
				failwith( ^ ("Invalid group ", string_of_int(n)));
				}) {
			case None: VNull;
			case Some(pos, pend): VObject(obj(hash_field(get_ctx([])), ::((new Tuple("pos", VInt(pos))), ::((new Tuple("len",
												  VInt(-(pend, pos)))), []))));
			};
		case _: error([]);
		}))), [])))));
	case Some(neko): var regexp_new_options = neko.load("regexp@regexp_new_options", 2);
		var regexp_match = neko.load("regexp@regexp_match", 4);
		var regexp_matched = neko.load("regexp@regexp_matched", 2);
		var regexp_matched_pos = neko.load("regexp@regexp_matched_pos", 2);
		make_library(::((new Tuple("regexp_new_options", Fun2(function str: function opt: neko.call(regexp_new_options, ::(str,
								   ::(opt, [])))))), ::((new Tuple("regexp_match",
										   Fun4(function r: function str: function pos: function len: neko.call(regexp_match, ::(r, ::(str, ::(pos,
												   ::(len, [])))))))), ::((new Tuple("regexp_matched", Fun2(function r: function n: neko.call(regexp_matched, ::(r,
														   ::(n, [])))))), ::((new Tuple("regexp_matched_pos", Fun2(function r: function n: neko.call(regexp_matched_pos, ::(r,
																   ::(n, [])))))), [])))));
	};

	public static var z_lib = 	function error([]) return {
		raise(Builtin_error);
	};
	make_library(::((new Tuple("inflate_init", Fun1(function f: var z = Extc.zlib_inflate_init2(switch (f) {
case VNull: 15;
case VInt(i): i;
	case _: error([]);
	});
	VAbstract(AZipI({ () with z = z;
					  z_flush = Extc.Z_NO_FLUSH
	}))))), ::((new Tuple("deflate_init", Fun1(function f: var z = Extc.zlib_deflate_init(switch (f) {
case VInt(i): i;
	case _: error([]);
	});
	VAbstract(AZipD({ () with z = z;
					  z_flush = Extc.Z_NO_FLUSH
					}))))), ::((new Tuple("deflate_end", Fun1(function vz:
	switch (vz) {
case VAbstract(AZipD(z)): Extc.zlib_deflate_end(z.z);
		free_abstract(vz);
		VNull;
	case _: error([]);
	}))), ::((new Tuple("inflate_end", Fun1(function vz:
	switch (vz) {
case VAbstract(AZipI(z)): Extc.zlib_inflate_end(z.z);
		free_abstract(vz);
		VNull;
	case _: error([]);
	}))), ::((new Tuple("set_flush_mode", Fun2(function z: function f:
	switch ((new Tuple(z, f))) {
case (VAbstract(AZipI(z) | AZipD(z)), VString(s)): z.z_flush = switch (s) {
		case NO: Extc.Z_NO_FLUSH;
		case SYNC: Extc.Z_SYNC_FLUSH;
		case FULL: Extc.Z_FULL_FLUSH;
		case FINISH: Extc.Z_FINISH;
		case BLOCK: Extc.Z_PARTIAL_FLUSH;
		case _: error([]);
		};
		VNull;
	case _: error([]);
	}))), ::((new Tuple("inflate_buffer", Fun5(function z: function src: function pos: function dst: function dpos:
	switch ((new Tuple(z, src, pos, dst, dpos))) {
case (VAbstract(AZipI(z)), VString(src), VInt(pos), VString(dst), VInt(dpos)): var r = Extc.zlib_inflate(z.z, src, pos,
				-(String.length(src), pos), dst, dpos, -(String.length(dst), dpos), z.z_flush);
		VObject(obj(hash_field(get_ctx([])), ::((new Tuple("done", VBool(r.Extc.z_finish))), ::((new Tuple("read",
												VInt(r.Extc.z_read))), ::((new Tuple("write", VInt(r.Extc.z_wrote))), [])))));
	case _: error([]);
	}))), ::((new Tuple("deflate_buffer", Fun5(function z: function src: function pos: function dst: function dpos:
	switch ((new Tuple(z, src, pos, dst, dpos))) {
case (VAbstract(AZipD(z)), VString(src), VInt(pos), VString(dst), VInt(dpos)): var r = Extc.zlib_deflate(z.z, src, pos,
				-(String.length(src), pos), dst, dpos, -(String.length(dst), dpos), z.z_flush);
		VObject(obj(hash_field(get_ctx([])), ::((new Tuple("done", VBool(r.Extc.z_finish))), ::((new Tuple("read",
												VInt(r.Extc.z_read))), ::((new Tuple("write", VInt(r.Extc.z_wrote))), [])))));
	case _: error([]);
	}))), ::((new Tuple("deflate_bound", Fun2(function z: function size:
	switch ((new Tuple(z, size))) {
case (VAbstract(AZipD(z)), VInt(size)): VInt(+(size, 1024));
	case _: error([]);
	}))), [])))))))));

	public static function haxe_float(f, p) return {
		var std = (new Tuple(Ast.EConst(Ast.Ident("std")), p));
		var math = (new Tuple(Ast.EField(std, "Math"), p));
		if ( = (f, infinity)) {
			(new Tuple(Ast.EField(math, "POSITIVE_INFINITY"), p));
		} else {
			if ( = (f, neg_infinity)) {
				(new Tuple(Ast.EField(math, "NEGATIVE_INFINITY"), p));
			} else {
				if (<>(f, f)) {
					(new Tuple(Ast.EField(math, "NaN"), p));
				} else {
					(new Tuple(Ast.EConst(Ast.Float(float_repres(f))), p));
				};
			};
		};
	};

	public static var macro_lib = 	function error([]) return {
		raise(Builtin_error);
	};
	function ccom([]) return {
		get_ctx([]).curapi.get_com([]);
	};
	make_library(::((new Tuple("curpos", Fun0(function []: VAbstract(APos(get_ctx([]).curapi.pos))))), ::((new Tuple("error",
					Fun2(function msg: function p:
	switch ((new Tuple(msg, p))) {
case (VString(s), VAbstract(APos(p))): ccom([]).Common.error(s, p);
		raise(Abort);
	case _: error([]);
	}))), ::((new Tuple("fatal_error", Fun2(function msg: function p:
	switch ((new Tuple(msg, p))) {
case (VString(s), VAbstract(APos(p))): raise(Typecore.Fatal_error(s, p));
	case _: error([]);
	}))), ::((new Tuple("warning", Fun2(function msg: function p:
	switch ((new Tuple(msg, p))) {
case (VString(s), VAbstract(APos(p))): ccom([]).warning(s, p);
		VNull;
	case _: error([]);
	}))), ::((new Tuple("class_path", Fun0(function []: VArray(Array.of_list(List.map(function s: VString(s),
										   ccom([]).class_path)))))), ::((new Tuple("resolve", Fun1(function file:
	switch (file) {
case VString(s): VString(try {
			Common.find_file(ccom([]), s);
		} catch (e: Not_found) {
			failwith( ^ ("File not found '", ^ (s, "'")));
		});
	case _: error([]);
	}))), ::((new Tuple("define", Fun1(function s:
	switch (s) {
case VString(s): Common.raw_define(ccom([]), s);
		VNull;
	case _: error([]);
	}))), ::((new Tuple("defined", Fun1(function s:
	switch (s) {
case VString(s): VBool(Common.raw_defined(ccom([]), s));
	case _: error([]);
	}))), ::((new Tuple("defined_value", Fun1(function s:
	switch (s) {
case VString(s): try {
			VString(Common.raw_defined_value(ccom([]), s));
		} catch (e: Not_found) {
			VNull;
		};
	case _: error([]);
	}))), ::((new Tuple("get_defines", Fun0(function []: var defines = ccom([]).defines;
											var h = Hashtbl.create(0);
											PMap.iter(function n: function v: Hashtbl.replace(h, VString(n), VString(v)), defines);
											enc_hash(h)))), ::((new Tuple("get_type", Fun1(function s:
	switch (s) {
case VString(s): switch (get_ctx([]).curapi.get_type(s)) {
		case None: failwith( ^ ("Type not found '", ^ (s, "'")));
		case Some(t): encode_type(t);
		};
	case _: error([]);
	}))), ::((new Tuple("get_module", Fun1(function s:
	switch (s) {
case VString(s): enc_array(List.map(encode_type, get_ctx([]).curapi.get_module(s)));
	case _: error([]);
	}))), ::((new Tuple("on_generate", Fun1(function f:
	switch (f) {
case VFunction(Fun1(_)) | VClosure(_): var ctx = get_ctx([]);
		ctx.curapi.on_generate(function tl: ignore(catch_errors(ctx, function []: ctx.do_call(VNull, f,
							   ::(enc_array(List.map(encode_type, tl)), []), null_pos))));
		VNull;
	case _: error([]);
	}))), ::((new Tuple("after_generate", Fun1(function f:
	switch (f) {
case VFunction(Fun0(_)): var ctx = get_ctx([]);
		ctx.curapi.after_generate(function []: ignore(catch_errors(ctx, function []: ctx.do_call(VNull, f, [], null_pos))));
		VNull;
	case _: error([]);
	}))), ::((new Tuple("on_type_not_found", Fun1(function f:
	switch (f) {
case VFunction(Fun1(_)): var ctx = get_ctx([]);
		ctx.curapi.on_type_not_found(function path:
		switch (catch_errors(ctx, function []: ctx.do_call(VNull, f, ::(enc_string(path), []), null_pos))) {
	case Some(v): v;
		case None: VNull;
		});
		VNull;
	case _: error([]);
	}))), ::((new Tuple("parse", Fun3(function s: function p: function b:
	switch ((new Tuple(s, p, b))) {
case (VString(s), VAbstract(APos(p)), VBool(b)) if (<>(s, "")):
		try {
			encode_expr(get_ctx([]).curapi.parse_string(s, p, b));
		} catch (e: Invalid_expr) {
			error([]);
		};
	case _: error([]);
	}))), ::((new Tuple("make_expr", Fun2(function v: function p:
	switch (p) {
case VAbstract(APos(p)): var h_enum = hash("__enum__");
		var h_et = hash("__et__");
		var h_ct = hash("__ct__");
		var h_tag = hash("tag");
		var h_args = hash("args");
		var h_length = hash("length");
		var ctx = get_ctx([]);
		function error(v) return {
			failwith( ^ ("Unsupported value ", ctx.do_string(v)));
		};
		function make_path(t) return {
			function loop(match) return switch (match) {
			case []: assert False;
			case ::(name, []): (new Tuple(Ast.EConst(Ast.Ident(name)), p));
			case ::(name, l): (new Tuple(Ast.EField(loop(l), name), p));
			};
			var t = t_infos(t);
			loop(List.rev(if ( = (t.mt_module.m_path, t.mt_path)) {
			@(fst(t.mt_path), ::(snd(t.mt_path), []));
			} else {
				@(fst(t.mt_module.m_path), ::(snd(t.mt_module.m_path), ::(snd(t.mt_path), [])));
			}));
		};
		function loop(match) return switch (match) {
		case VNull: (new Tuple(Ast.EConst(Ast.Ident("null")), p));
		case VBool(b): (new Tuple(Ast.EConst(Ast.Ident(if (b) {
			"true";
		} else {
			"false";
		})), p));
		case VInt(i): (new Tuple(Ast.EConst(Ast.Int(string_of_int(i))), p));
		case VInt32(i): (new Tuple(Ast.EConst(Ast.Int(Int32.to_string(i))), p));
		case VFloat(f): haxe_float(f, p);
		case VAbstract(APos(p)): (new Tuple(Ast.EObjectDecl(::((new Tuple("fileName",
												(new Tuple(Ast.EConst(Ast.String(p.Ast.pfile)), p)))), ::((new Tuple("lineNumber",
														(new Tuple(Ast.EConst(Ast.Int(string_of_int(Lexer.get_error_line(p)))), p)))), ::((new Tuple("className",
																(new Tuple(Ast.EConst(Ast.String("")), p)))), [])))), p));
		case VString(_) | VArray(_) | VAbstract(_) | VFunction(_) | VClosure(_) = v: error(v);
		case VObject(o) = v: switch (o.oproto) {
			case None: switch (get_field_opt(o, h_ct)) {
				case Some(VAbstract(ATDecl(t))): make_path(t);
				case _: var fields = List.fold_left(function acc: function (fid, v): ::((new Tuple(field_name(ctx, fid), loop(v))),
														acc), [], Array.to_list(o.ofields));
					(new Tuple(Ast.EObjectDecl(fields), p));
				};
			case Some(proto): switch ((new Tuple(get_field_opt(proto, h_enum), get_field_opt(o, h_a), get_field_opt(o, h_s),
													 get_field_opt(o, h_length)))) {
				case (_, Some(VArray(a)), _, Some(VInt(len))): (new Tuple(Ast.EArrayDecl(List.map(loop, Array.to_list(Array.sub(a, 0,
							len)))), p));
				case (_, _, Some(VString(s)), _): (new Tuple(Ast.EConst(Ast.String(s)), p));
				case (Some(VObject(en)), _, _, _): switch ((new Tuple(get_field(en, h_et), get_field(o, h_tag)))) {
					case (VAbstract(ATDecl(t)), VString(tag)): var e = (new Tuple(Ast.EField(make_path(t), tag), p));
						switch (get_field_opt(o, h_args)) {
						case Some(VArray(args)): var args = List.map(loop, Array.to_list(args));
							(new Tuple(Ast.ECall(e, args), p));
						case _: e;
						};
					case _: error(v);
					};
				case _: error(v);
				};
			};
		};
		encode_expr(loop(v));
	case _: error([]);
	}))), ::((new Tuple("signature", Fun1(function v: var cache = ref([]);
										  var cache_count = ref(0);
										  var hfiles = Hashtbl.create(0);
	function get_file(f) return {
		try {
			Hashtbl.find(hfiles, f);
		} catch (e: Not_found) {
			var ff = Common.unique_full_path(f);
			Hashtbl.add(hfiles, f, ff);
			ff;
		};
	};
	function do_cache(vvalue, v2value) return {
		var vt = Obj.repr(v);
		var old = Obj.tag(vt);
		var old_val = Obj.field(vt, 0);
		var abstract_tag = 7;
		Obj.set_tag(vt, abstract_tag);
		Obj.set_field(vt, 0, Obj.repr(ACacheRef(v2)));
		cache.val = ::((new Tuple(vt, old, old_val)), cache.val);
		incr(cache_count);
	};
	function loop(v) return {
		switch (v) {
		case VNull | VBool(_) | VInt(_) | VFloat(_) | VString(_) | VInt32(_): v;
		case VObject(o): var o2 = {
				() with ofields = [];
				oproto = None
			};
			var v2 = VObject(o2);
			do_cache(v, v2);
			Array.iter(function (f, v): if (<>(f, h_class)) {
			set_field(o2, f, loop(v));
			} else {
				[];
			}, o.ofields);
			switch (o.oproto) {
			case None: [];
			case Some(p): switch (loop(VObject(p))) {
				case VObject(p2): o2.oproto = Some(p2);
				case _: assert False;
				};
			};
			v2;
		case VArray(a): var a2 = Array.create(Array.length(a), VNull);
			var v2 = VArray(a2);
			do_cache(v, v2);
			for (i in /*to*/0... - (Array.length(a), 1)) {
				a2i = loop(ai);
			};
			v2;
		case VFunction(f): var v2 = VFunction(Obj.magic(cache_count.val));
			do_cache(v, v2);
			v2;
		case VClosure(vl, f): var rl = ref([]);
			var v2 = VClosure(Obj.magic(rl), Obj.magic(cache_count.val));
			var vl = ::(VNull, vl);
			do_cache(v, v2);
			rl.val = List.map(loop, vl);
			v2;
		case VAbstract(APos(p)): VAbstract(APos({ (p) with Ast.pfile = get_file(p.Ast.pfile) }));
		case VAbstract(ACacheRef(v)): v;
		case VAbstract(AHash(h)): var h2 = Hashtbl.create(0);
			var v2 = VAbstract(AHash(h2));
			do_cache(v, v2);
			Hashtbl.iter(function k: function v: Hashtbl.add(h2, k, loop(v)), h2);
			v2;
		case VAbstract(_): var v2 = VAbstract(Obj.magic(cache_count.val));
			do_cache(v, v2);
			v2;
		};
	};
	var v = loop(v);
			List.iter(function (vt, tag, field): Obj.set_tag(vt, tag);
					  Obj.set_field(vt, 0, field), cache.val);
			VString(Digest.to_hex(Digest.string(Marshal.to_string(v, ::(Marshal.Closures, [])))))))), ::((new Tuple("to_complex",
					Fun1(function v:
	try {
		encode_complex_type(TExprToExpr.convert_type(decode_type(v)));
	} catch (e: Exit) {
		VNull;
	}))), ::((new Tuple("unify", Fun2(function t1: function t2: var e1 = mk(TObjectDecl([]), decode_type(t1), Ast.null_pos);
	try {
		ignore(get_ctx([]).curapi.cast_or_unify(decode_type(t2), e1, Ast.null_pos));
		VBool(True);
	} catch (e: Typecore.Error(Typecore.Unify(_))(_)) {
		VBool(False);
	}))), ::((new Tuple("typeof", Fun1(function v: encode_type(get_ctx([]).curapi.type_expr(decode_expr(v)).etype)))),
			 ::((new Tuple("type_expr", Fun1(function v: encode_texpr(get_ctx([]).curapi.type_expr(decode_expr(v)))))),
				::((new Tuple("s_type", Fun1(function v: VString(Type.s_type(print_context([]), decode_type(v)))))),
	::((new Tuple("s_expr", Fun2(function v: function b: var f = switch (b) {
case VBool(True): Type.s_expr_pretty("");
	case _: Type.s_expr_ast(True, "");
	};
	VString(f(Type.s_type(print_context([])), decode_texpr(v)))))), ::((new Tuple("is_fmt_string", Fun1(function v:
	switch (v) {
case VAbstract(APos(p)): VBool(Lexer.is_fmt_string(p));
	case _: VNull;
	}))), ::((new Tuple("format_string", Fun2(function s: function p:
	switch ((new Tuple(s, p))) {
case (VString(s), VAbstract(APos(p))): encode_expr(get_ctx([]).curapi.format_string(s, p));
	case _: VNull;
	}))), ::((new Tuple("display", Fun1(function v:
	switch (v) {
case VString(s): VString(get_ctx([]).curapi.get_display(s));
	case _: error([]);
	}))), ::((new Tuple("allow_package", Fun1(function v:
	switch (v) {
case VString(s): get_ctx([]).curapi.allow_package(s);
		VNull;
	case _: error([]);
	}))), ::((new Tuple("type_patch", Fun4(function t: function f: function s: function v: var p =
			get_ctx([]).curapi.type_patch;
	switch ((new Tuple(t, f, s, v))) {
case (VString(t), VString(f), VBool(s), VString(v)): p(t, f, s, Some(v));
	case (VString(t), VString(f), VBool(s), VNull): p(t, f, s, None);
	case _: error([]);
	};
	VNull))), ::((new Tuple("meta_patch",
							Fun4(function m: function t: function f: function s: var p = get_ctx([]).curapi.meta_patch;
	switch ((new Tuple(m, t, f, s))) {
case (VString(m), VString(t), VString(f), VBool(s)): p(m, t, Some(f), s);
	case (VString(m), VString(t), VNull, VBool(s)): p(m, t, None, s);
	case _: error([]);
	};
	VNull))), ::((new Tuple("add_global_metadata", Fun5(function v1: function v2: function v3: function v4: function v5:
	switch ((new Tuple(v1, v2, v3, v4, v5))) {
case (VString(s1), VString(s2), VBool(b1), VBool(b2), VBool(b3)): get_ctx([]).curapi.add_global_metadata(s1, s2,
				(new Tuple(b1, b2, b3)));
		VNull;
	case _: error([]);
	}))), ::((new Tuple("custom_js", Fun1(function f:
	switch (f) {
case VFunction(Fun1(_)): var ctx = get_ctx([]);
		ctx.curapi.set_js_generator(function api: ignore(catch_errors(ctx, function []: ctx.do_call(VNull, f, ::(api, []),
									null_pos))));
		VNull;
	case _: error([]);
	}))), ::((new Tuple("get_pos_infos", Fun1(function p:
	switch (p) {
case VAbstract(APos(p)): VObject(obj(hash_field(get_ctx([])), ::((new Tuple("min", VInt(p.Ast.pmin))), ::((new Tuple("max",
											 VInt(p.Ast.pmax))), ::((new Tuple("file", VString(p.Ast.pfile))), [])))));
	case _: error([]);
	}))), ::((new Tuple("make_pos", Fun3(function min: function max: function file:
	switch ((new Tuple(min, max, file))) {
case (VInt(min), VInt(max), VString(file)): VAbstract(APos({ () with Ast.pmin = min;
				Ast.pmax = max;
				Ast.pfile = file
																   }));
	case _: error([]);
	}))), ::((new Tuple("add_resource", Fun2(function name: function data:
	switch ((new Tuple(name, data))) {
case (VString(name), VString(data)): Hashtbl.replace(ccom([]).resources, name, data);
		var m = get_ctx([]).curapi.current_module([]);
		m.m_extra.m_binded_res = PMap.add(name, data, m.m_extra.m_binded_res);
		VNull;
	case _: error([]);
	}))), ::((new Tuple("get_resources", Fun0(function []: var res = ccom([]).resources;
						var h = Hashtbl.create(0);
						Hashtbl.iter(function n: function v: Hashtbl.replace(h, VString(n), VString(v)), res);
						enc_hash(h)))), ::((new Tuple("local_module", Fun0(function []: var m = get_ctx([]).curapi.current_module([]);
											VString(Ast.s_type_path(m.m_path))))), ::((new Tuple("local_type", Fun0(function []:
	switch (get_ctx([]).curapi.get_local_type([])) {
case None: VNull;
case Some(t): encode_type(t);
	}))), ::((new Tuple("expected_type", Fun0(function []:
	switch (get_ctx([]).curapi.get_expected_type([])) {
case None: VNull;
case Some(t): encode_type(t);
	}))), ::((new Tuple("call_arguments", Fun0(function []:
	switch (get_ctx([]).curapi.get_call_arguments([])) {
case None: VNull;
case Some(el): enc_array(List.map(encode_expr, el));
	}))), ::((new Tuple("local_method", Fun0(function []: VString(get_ctx([]).curapi.get_local_method([]))))),
			 ::((new Tuple("local_using", Fun0(function []: enc_array(List.map(encode_clref,
						   get_ctx([]).curapi.get_local_using([])))))), ::((new Tuple("local_imports",
								   Fun0(function []: enc_array(List.map(encode_import, get_ctx([]).curapi.get_local_imports([])))))),
	::((new Tuple("local_vars", Fun1(function as_var: var as_var = switch (as_var) {
case VNull | VBool(False): False;
	case VBool(True): True;
	case _: error([]);
	};
	var vars = get_ctx([]).curapi.get_local_vars([]);
			   var h = Hashtbl.create(0);
	if (as_var) {
	PMap.iter(function n: function v: Hashtbl.replace(h, VString(n), encode_tvar(v)), vars);
	} else {
		PMap.iter(function n: function v: Hashtbl.replace(h, VString(n), encode_type(v.v_type)), vars);
	};
	enc_hash(h)))), ::((new Tuple("follow_with_abstracts", Fun2(function v: function once: var t = decode_type(v);
	function follow_once(t) return {
		switch (t) {
		case TMono(r): switch (r.val) {
			case None: t;
			case Some(t): t;
			};
		case TAbstract(a, tl) if (!(Ast.Meta.has(Ast.Meta.CoreType, a.a_meta))): Abstract.get_underlying_type(a, tl);
		case TAbstract(_) | TEnum(_) | TInst(_) | TFun(_) | TAnon(_) | TDynamic(_): t;
		case TType(t, tl): apply_params(t.t_params, tl, t.t_type);
		case TLazy(f): f.val([]);
		};
	};
	encode_type(switch (once) {
case VNull | VBool(False): Abstract.follow_with_abstracts(t);
	case VBool(True): follow_once(t);
	case _: error([]);
	})))), ::((new Tuple("follow", Fun2(function v: function once: var t = decode_type(v);
	function follow_once(t) return {
		switch (t) {
		case TMono(r): switch (r.val) {
			case None: t;
			case Some(t): t;
			};
		case TAbstract(_) | TEnum(_) | TInst(_) | TFun(_) | TAnon(_) | TDynamic(_): t;
		case TType(t, tl): apply_params(t.t_params, tl, t.t_type);
		case TLazy(f): f.val([]);
		};
	};
	encode_type(switch (once) {
case VNull | VBool(False): follow(t);
	case VBool(True): follow_once(t);
	case _: error([]);
	})))), ::((new Tuple("build_fields", Fun0(function []: get_ctx([]).curapi.get_build_fields([])))),
			  ::((new Tuple("define_type", Fun1(function v: get_ctx([]).curapi.define_type(v);
							VNull))), ::((new Tuple("define_module", Fun4(function p: function v: function i: function u:
	switch ((new Tuple(p, v, i, u))) {
case (VString(path), VArray(vl), VArray(ui), VArray(ul)): get_ctx([]).curapi.define_module(path, Array.to_list(vl),
				List.map(decode_import, Array.to_list(ui)), List.map(decode_path, Array.to_list(ul)));
		VNull;
	case _: error([]);
	}))), ::((new Tuple("add_class_path", Fun1(function v:
	switch (v) {
case VString(cp): var com = ccom([]);
		com.class_path = ::(Common.normalize_path(cp), com.class_path);
		Hashtbl.clear(com.file_lookup_cache);
		VNull;
	case _: error([]);
	}))), ::((new Tuple("add_native_lib", Fun1(function v:
	switch (v) {
case VString(file): var com = ccom([]);
		switch (com.platform) {
		case Flash: Genswf.add_swf_lib(com, file, False);
		case Java: Genjava.add_java_lib(com, file, False);
		case Cs: var Tuple(file, is_std) = switch (ExtString.String.nsplit(file, "@")) {
			case ::(file, []): (new Tuple(file, False));
			case ::(file, ::(std, [])): (new Tuple(file, True));
			case _: failwith( ^ ("unsupported file@`std` format: ", file));
			};
			Gencs.add_net_lib(com, file, is_std);
		case _: failwith("Unsupported platform");
		};
		VNull;
	case _: error([]);
	}))), ::((new Tuple("add_native_arg", Fun1(function v:
	switch (v) {
case VString(arg): var com = ccom([]);
		switch (com.platform) {
		case Java | Cs | Cpp: com.c_args = ::(arg, com.c_args);
		case _: failwith("Unsupported platform");
		};
		VNull;
	case _: error([]);
	}))), ::((new Tuple("module_dependency", Fun2(function m: function file:
	switch ((new Tuple(m, file))) {
case (VString(m), VString(file)): get_ctx([]).curapi.module_dependency(m, file, False);
		VNull;
	case _: error([]);
	}))), ::((new Tuple("module_reuse_call", Fun2(function m: function mcall:
	switch ((new Tuple(m, mcall))) {
case (VString(m), VString(mcall)): get_ctx([]).curapi.module_dependency(m, mcall, True);
		VNull;
	case _: error([]);
	}))), ::((new Tuple("get_typed_expr", Fun1(function e: var e = decode_texpr(e);
	encode_expr(TExprToExpr.convert_expr(e))))), ::((new Tuple("store_typed_expr", Fun1(function e: var e = try {
		decode_texpr(e);
	} catch (e: Invalid_expr) {
		error([]);
	};
	encode_expr(get_ctx([]).curapi.store_typed_expr(e))))), ::((new Tuple("get_output",
			Fun0(function []: VString(ccom([]).file)))), ::((new Tuple("set_output", Fun1(function s:
	switch (s) {
case VString(s): ccom([]).file = s;
		VNull;
	case _: error([]);
	}))), ::((new Tuple("get_display_pos", Fun0(function []: var p = Parser.resume_display.val;
	if ( = (p, Ast.null_pos)) {
	VNull;
} else {
	VObject(obj(hash_field(get_ctx([])), ::((new Tuple("file", VString(p.Ast.pfile))), ::((new Tuple("pos",
											VInt(p.Ast.pmin))), []))));
	}))), ::((new Tuple("pattern_locals",
						Fun2(function e: function t: var loc = get_ctx([]).curapi.get_pattern_locals(decode_expr(e), decode_type(t));
							 var h = Hashtbl.create(0);
							 PMap.iter(function n: function (v, _): Hashtbl.replace(h, VString(n), encode_type(v.v_type)), loc);
							 enc_hash(h)))), ::((new Tuple("macro_context_reused", Fun1(function c:
	switch (c) {
case VFunction(Fun0(_)): var ctx = get_ctx([]);
		ctx.on_reused = ::(function []: = (catch_errors(ctx, function []: ctx.do_call(VNull, c, [], null_pos)), Some(VBool(True))),
						   ctx.on_reused);
		VNull;
	case _: error([]);
	}))), ::((new Tuple("apply_params", Fun3(function tpl: function tl: function t: var tpl = List.map(function v:
	switch (v) {
case VObject(o): var name = switch (get_field(o, hash("name"))) {
		case VString(s): s;
		case _: assert False;
		};
		var t = decode_type(get_field(o, hash("t")));
		(new Tuple(name, t));
	case _: assert False;
	}, dec_array(tpl));
	var tl = List.map(decode_type, dec_array(tl));
	function map(t) return {
		switch (t) {
		case TInst({ cl_kind = KTypeParameter(_) }, _):
			try {
				snd(List.find(function (_, t2): type_iseq(t, t2), tpl));
			} catch (e: Not_found) {
				Type.map(map, t);
			};
		case _: Type.map(map, t);
		};
	};
	encode_type(apply_params(tpl, tl, map(decode_type(t))))))), ::((new Tuple("eval", Fun1(function v: var e = decode_expr(v);
			var e = get_ctx([]).curapi.type_macro_expr(e);
	switch (eval_expr_ref.val(get_ctx([]), e)) {
case Some(v): v;
	case None: VNull;
	}))), ::((new Tuple("include_file", Fun2(function file: function position:
	switch ((new Tuple(file, position))) {
case (VString(file), VString(position)): var file = if (Sys.file_exists(file)) {
			file;
		} else {
			try {
				Common.find_file(ccom([]), file);
			} catch (e: Not_found) {
				failwith( ^ ("unable to find file for inclusion: ", file));
			};
		};
		ccom([]).include_files = ::((new Tuple(file, position)), ccom([]).include_files);
		VNull;
	case _: error([]);
	}))), [])))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))));

	public static function throw(ctx, p, msg) return {
		ctx.callstack = ::({
			() with cpos = p;
			cthis = ctx.vthis;
			cstack = DynArray.length(ctx.stack);
			cenv = ctx.venv
		}, ctx.callstack);
		exc(VString(msg));
	};

	public static function declare(ctx, var) return {
		ctx.locals_map = PMap.add(var, ctx.locals_count, ctx.locals_map);
		ctx.locals_count = +(ctx.locals_count, 1);
	};

	public static function save_locals(ctx) return {
		var Tuple(old, oldcount) = (new Tuple(ctx.locals_map, ctx.locals_count));
		function []: var n = -(ctx.locals_count, oldcount);
		ctx.locals_count = oldcount;
		ctx.locals_map = old;
		n;
	};

	public static function get_ident(ctx, s) return {
		try {
			var index = PMap.find(s, ctx.locals_map);
			if ( >= (index, ctx.locals_barrier)) {
				AccLocal(-(ctx.locals_count, index));
			} else {
				try {
					AccEnv(DynArray.index_of(function s2: = (s, s2), ctx.locals_env));
				} catch (e: Not_found) {
					var index = DynArray.length(ctx.locals_env);
					DynArray.add(ctx.locals_env, s);
					AccEnv(index);
				};
			};
		} catch (e: Not_found) {
			try {
				AccGlobal(PMap.find(s, ctx.globals));
			} catch (e: Not_found) {
				var g = ref(VNull);
				ctx.globals = PMap.add(s, g, ctx.globals);
				AccGlobal(g);
			};
		};
	};

	public static var no_env = [];

	public static function eval_expr(ctx, e) return {
		var e = Genneko.gen_expr(ctx.gen, e);
		catch_errors(ctx, function []: eval(ctx, e, []));
	};

	public static function eval(ctx, Tuple(e, p)) return {
		switch (e) {
		case EConst(c): switch (c) {
			case  True: function []: VBool(True);
			case  False: function []: VBool(False);
			case Null: function []: VNull;
			case This: function []: ctx.vthis;
			case Int(i): function []: VInt(i);
			case Int32(i): function []: VInt32(i);
			case Float(f): var f = float_of_string(f);
				function []: VFloat(f);
			case String(s): function []: VString(s);
			case Builtin(loader): function []: ctx.loader;
			case Builtin(exports): function []: ctx.exports;
			case Builtin(s): var b = try {
					Hashtbl.find(builtins, s);
				} catch (e: Not_found) {
					throw(ctx, p, ^ ("Builtin not found '", ^ (s, "'")));
				};
				function []: b;
			case Ident(s): acc_get(ctx, p, get_ident(ctx, s));
			};
		case EBlock(el): var old = save_locals(ctx);
			var el = List.map(eval(ctx), el);
			var n = old([]);
			function loop(match) return switch (match) {
			case []: VNull;
			case ::(e, []): e([]);
			case ::(e, l): ignore(e([]));
				loop(l);
			};
			function []: var v = loop(el);
			pop(ctx, n);
			v;
		case EParenthesis(e): eval(ctx, e);
		case EField(e, f): var e = eval(ctx, e);
			var h = hash_field(ctx, f);
			function []:
			switch (e([])) {
			case VObject(o): get_field(o, h);
			case _: throw(ctx, p, ^ ("Invalid field access : ", f));
			};
		case ECall((EConst(Builtin(mk_pos)), _), ::((ECall(_, ::((EConst(String(file)), _), [])), _), ::((EConst(Int(min)), _), ::((EConst(Int(max)), _), []))))
				: var pos = VAbstract(APos({ () with Ast.pfile = file;
										 Ast.pmin = min;
										 Ast.pmax = max
									   }));
			function []: pos;
		case ECall((EConst(Builtin(typewrap)), _), ::(t, [])): function []: VAbstract(ATDecl(Obj.magic(t)));
		case ECall((EConst(Builtin(delay_call)), _), ::((EConst(Int(index)), _), [])): var f = ctx.curapi.delayed_macro(index);
			var fbuild = ref(None);
			var old = { (ctx) with gen = ctx.gen };
			function compile_delayed_call([]) return {
				var Tuple(oldl, oldc, oldb, olde) = (new Tuple(ctx.locals_map, ctx.locals_count, ctx.locals_barrier, ctx.locals_env));
				ctx.locals_map = old.locals_map;
				ctx.locals_count = old.locals_count;
				ctx.locals_barrier = old.locals_barrier;
				ctx.locals_env = DynArray.copy(old.locals_env);
				var save = save_locals(ctx);
				var e = f([]);
				var n = save([]);
				var e = if ( = (DynArray.length(ctx.locals_env), DynArray.length(old.locals_env))) {
					e;
				} else {
					var n = DynArray.get(ctx.locals_env, -(DynArray.length(ctx.locals_env), 1));
					function []: exc(VString( ^ ("Macro-in-macro call can't access to closure variable '", ^ (n, "'"))));
				};
				ctx.locals_map = oldl;
				ctx.locals_count = oldc;
				ctx.locals_barrier = oldb;
				ctx.locals_env = olde;
				function []: var v = e([]);
				pop(ctx, n);
				v;
			};
			function []: var e = switch (fbuild.val) {
			case Some(e): e;
			case None: var e = compile_delayed_call([]);
				fbuild.val = Some(e);
				e;
			};
			e([]);
		case ECall(e, el): var el = List.map(eval(ctx), el);
			switch (fst(e)) {
			case EField(e, f): var e = eval(ctx, e);
				var h = hash_field(ctx, f);
				function []: var pl = List.map(function f: f([]), el);
				var o = e([]);
				var f = switch (o) {
				case VObject(o): get_field(o, h);
				case _: throw(ctx, p, ^ ("Invalid field access : ", f));
				};
				call(ctx, o, f, pl, p);
			case _: var e = eval(ctx, e);
				function []: var pl = List.map(function f: f([]), el);
				call(ctx, ctx.vthis, e([]), pl, p);
			};
		case EArray(e1, e2): var e1 = eval(ctx, e1);
			var e2 = eval(ctx, e2);
			var acc = AccArray(e1, e2);
			acc_get(ctx, p, acc);
		case EVars(vl): var vl = List.map(function (v, eo): var eo = switch (eo) {
		case None: function []: VNull;
			case Some(e): eval(ctx, e);
			};
			declare(ctx, v);
			eo, vl);
			function []: List.iter(function e: push(ctx, e([])), vl);
			VNull;
		case EWhile(econd, e, NormalWhile): var econd = eval(ctx, econd);
			var e = eval(ctx, e);
			function loop(st) return {
				switch (econd([])) {
				case VBool(True): var v = try {
						ignore(e([]));
						None;
					} catch (e: T) {
						McOr(McArr(PaId(IdUid(Continue)), ExNil, ExSeq(ExSem(ExApp(<...>, <...>), ExId(<...>)))), McArr(PaApp(PaId(IdUid(<...>)),
								PaId(IdLid(<...>))), ExNil, ExSeq(ExSem(ExApp(<...>, <...>), ExApp(<...>,
									<...>)))))																		case Continue: pop(ctx, -(DynArray.length(ctx.stack), st));
						None;
					case Break(v): pop(ctx, -(DynArray.length(ctx.stack), st));
						Some(v);
					};
					switch (v) {
					case None: loop(st);
					case Some(v): v;
					};
				case _: VNull;
				};
			};
			function []:
			try {
				loop(DynArray.length(ctx.stack));
			} catch (e: Sys.Break) {
				throw(ctx, p, "Ctrl+C");
			};
		case EWhile(econd, e, DoWhile): var e = eval(ctx, e);
			var econd = eval(ctx, econd);
			function loop(st) return {
				var v = try {
					ignore(e([]));
					None;
				} catch (e: T) {
					McOr(McArr(PaId(IdUid(Continue)), ExNil, ExSeq(ExSem(ExApp(<...>, <...>), ExId(<...>)))), McArr(PaApp(PaId(IdUid(<...>)),
					PaId(IdLid(<...>))), ExNil, ExSeq(ExSem(ExApp(<...>, <...>), ExApp(<...>,
				<...>)))))																	case Continue: pop(ctx, -(DynArray.length(ctx.stack), st));
					None;
				case Break(v): pop(ctx, -(DynArray.length(ctx.stack), st));
					Some(v);
				};
				switch (v) {
				case Some(v): v;
				case None: switch (econd([])) {
					case VBool(True): loop(st);
					case _: VNull;
					};
				};
			};
			function []: loop(DynArray.length(ctx.stack));
		case EIf(econd, eif, eelse): var econd = eval(ctx, econd);
			var eif = eval(ctx, eif);
			var eelse = switch (eelse) {
			case None: function []: VNull;
			case Some(e): eval(ctx, e);
			};
			function []:
			switch (econd([])) {
			case VBool(True): eif([]);
			case _: eelse([]);
			};
		case ETry(e, exc, ecatch): var old = save_locals(ctx);
			var e = eval(ctx, e);
			var n1 = old([]);
			declare(ctx, exc);
			var ecatch = eval(ctx, ecatch);
			var n2 = old([]);
			function []: var vthis = ctx.vthis;
			var venv = ctx.venv;
			var stack = ctx.callstack;
			var csize = ctx.callsize;
			var size = DynArray.length(ctx.stack);
			try {
				pop_ret(ctx, e, n1);
			} catch (e: Runtime(v)) {
				function loop(n, l) return {
					if ( = (n, 0)) {
						List.map(function s: s.cpos, l);
					} else {
						switch (l) {
						case []: [];
						case ::(_, l): loop(-(n, 1), l);
						};
					};
				};
				ctx.exc = loop(List.length(stack), List.rev(ctx.callstack));
				ctx.callstack = stack;
				ctx.callsize = csize;
				ctx.vthis = vthis;
				ctx.venv = venv;
				pop(ctx, -(DynArray.length(ctx.stack), size));
				push(ctx, v);
				pop_ret(ctx, ecatch, n2);
			};
		case EFunction(pl, e): var old = save_locals(ctx);
			var Tuple(oldb, oldenv) = (new Tuple(ctx.locals_barrier, ctx.locals_env));
			ctx.locals_barrier = ctx.locals_count;
			ctx.locals_env = DynArray.create([]);
			List.iter(declare(ctx), pl);
			var e = eval(ctx, e);
			ignore(old([]));
			var env = ctx.locals_env;
			ctx.locals_barrier = oldb;
			ctx.locals_env = oldenv;
			var env = DynArray.to_array(DynArray.map(function s: acc_get(ctx, p, get_ident(ctx, s)), env));
			var init_env = if ( = (Array.length(env), 0)) {
				function []: no_env;
			} else {
				function []: Array.map(function e: e([]), env);
			};
			switch (pl) {
			case []: function []: var env = init_env([]);
				VFunction(Fun0(function []: ctx.venv = env;
							   e([])));
			case ::(a, []): function []: var env = init_env([]);
				VFunction(Fun1(function v: ctx.venv = env;
							   push(ctx, v);
							   e([])));
			case ::(a, ::(b, [])): function []: var env = init_env([]);
				VFunction(Fun2(function va: function vb: ctx.venv = env;
							   push(ctx, va);
							   push(ctx, vb);
							   e([])));
			case ::(a, ::(b, ::(c, []))): function []: var env = init_env([]);
				VFunction(Fun3(function va: function vb: function vc: ctx.venv = env;
							   push(ctx, va);
							   push(ctx, vb);
							   push(ctx, vc);
							   e([])));
			case ::(a, ::(b, ::(c, ::(d, [])))): function []: var env = init_env([]);
				VFunction(Fun4(function va: function vb: function vc: function vd: ctx.venv = env;
							   push(ctx, va);
							   push(ctx, vb);
							   push(ctx, vc);
							   push(ctx, vd);
							   e([])));
			case ::(a, ::(b, ::(c, ::(d, ::(pe, []))))): function []: var env = init_env([]);
				VFunction(Fun5(function va: function vb: function vc: function vd: function ve: ctx.venv = env;
							   push(ctx, va);
							   push(ctx, vb);
							   push(ctx, vc);
							   push(ctx, vd);
							   push(ctx, ve);
							   e([])));
			case _: function []: var env = init_env([]);
				VFunction(FunVar(function vl:
				if ( != (List.length(vl), List.length(pl))) {
				exc(VString("Invalid call"));
				} else {
					[];
				};
				ctx.venv = env;
						   List.iter(push(ctx), vl);
						   e([])));
			};
		case EBinop(op, e1, e2): eval_op(ctx, op, e1, e2, p);
		case EReturn(None): function []: raise(Return(VNull));
		case EReturn(Some(e)): var e = eval(ctx, e);
			function []: raise(Return(e([])));
		case EBreak(None): function []: raise(Break(VNull));
		case EBreak(Some(e)): var e = eval(ctx, e);
			function []: raise(Break(e([])));
		case EContinue: function []: raise(Continue);
		case ENext(e1, e2): var e1 = eval(ctx, e1);
			var e2 = eval(ctx, e2);
			function []: ignore(e1([]));
			e2([]);
		case EObject(fl): var fl = List.map(function (f, e): (new Tuple(hash_field(ctx, f), eval(ctx, e))), fl);
			var fields = Array.of_list(List.map(function (f, _): (new Tuple(f, VNull)), fl));
			Array.sort(function (f1, _): function (f2, _): compare(f1, f2), fields);
			function []: var o = { () with ofields = Array.copy(fields);
								   oproto = None
								 };
			List.iter(function (f, e): set_field(o, f, e([])), fl);
			VObject(o);
		case ELabel(l): assert False;
		case ESwitch(e1, el, eo): var e1 = eval(ctx, e1);
			var el = List.map(function (cond, e): (new Tuple(cond, eval(ctx, cond), eval(ctx, e))), el);
			var eo = switch (eo) {
			case None: function []: VNull;
			case Some(e): eval(ctx, e);
			};
			var cases = try {
				var max = ref(-1);
				var ints = List.map(function (cond, _, e):
				switch (fst(cond)) {
			case EConst(Int(i)): if ( < (i, 0)) {
						raise(Exit);
					} else {
						[];
					};
					if ( > (i, max.val)) {
						max.val = i;
					} else {
						[];
					};
					(new Tuple(i, e));
				case _: raise(Exit);
				}, el);
				var a = Array.create(+(max.val, 1), eo);
				List.iter(function (i, e): ai = e, List.rev(ints));
				Some(a);
			} catch (e: Exit) {
				None;
			};
			function def(v) return {
				function loop(match) return switch (match) {
				case []: eo([]);
				case ::((_, c, e), l): if ( = (ctx.do_compare(v, c([])), CEq)) {
						e([]);
					} else {
						loop(l);
					};
				};
				loop(el);
			};
			switch (cases) {
			case None: function []: def(e1([]));
			case Some(t): function []: switch (e1([])) {
				case VInt(i): if ( && ( >= (i, 0), < (i, Array.length(t)))) {
						ti([]);
					} else {
						eo([]);
					};
				case v: def(v);
				};
			};
		case ENeko(_): throw(ctx, p, "Inline neko code unsupported");
		};
	};

	public static function eval_oop(ctx, p, o, field, paramslist(value)) return {
		switch (get_field_opt(o, field)) {
		case None: None;
		case Some(f): Some(call(ctx, VObject(o), f, params, p));
		};
	};

	public static function eval_access(ctx, Tuple(e, p)) return {
		switch (e) {
		case EField(e, f): var v = eval(ctx, e);
			AccField(v, f);
		case EArray(e, eindex): var v = eval(ctx, e);
			var idx = eval(ctx, eindex);
			AccArray(v, idx);
		case EConst(Ident(s)): get_ident(ctx, s);
		case EConst(This): AccThis;
		case _: throw(ctx, p, "Invalid assign");
		};
	};

	public static function eval_access_get_set(ctx, Tuple(e, p)) return {
		switch (e) {
		case EField(e, f): var v = eval(ctx, e);
			var cache = ref(VNull);
			(new Tuple(AccField(function []: cache.val = v([]);
			cache.val, f), AccField(function []: cache.val, f)));
		case EArray(e, eindex): var v = eval(ctx, e);
			var idx = eval(ctx, eindex);
			var vcache = ref(VNull);
			var icache = ref(VNull);
			(new Tuple(AccArray(function []: vcache.val = v([]);
			vcache.val, function []: icache.val = idx([]);
			icache.val), AccArray(function []: vcache.val, function []: icache.val)));
		case EConst(Ident(s)): var acc = get_ident(ctx, s);
			(new Tuple(acc, acc));
		case EConst(This): (new Tuple(AccThis, AccThis));
		case _: throw(ctx, p, "Invalid assign");
		};
	};

	public static function acc_get(ctx, p) return {
	case AccField(v, f): var h = hash_field(ctx, f);
		function []:
		switch (v([])) {
		case VObject(o): get_field(o, h);
		case _: throw(ctx, p, ^ ("Invalid field access : ", f));
		};
	case AccArray(e, index): function []: var e = e([]);
		var index = index([]);
		switch ((new Tuple(index, e))) {
		case (VInt(i), VArray(a)): try {
				Array.get(a, i);
			} catch (e: _) {
				VNull;
			};
		case (VInt32(_), VArray(_)): VNull;
		case (_, VObject(o)): switch (eval_oop(ctx, p, o, h_get, ::(index, []))) {
			case None: throw(ctx, p, "Invalid array access");
			case Some(v): v;
			};
		case _: throw(ctx, p, "Invalid array access");
		};
	case AccLocal(i): function []: DynArray.get(ctx.stack, -(DynArray.length(ctx.stack), i));
	case AccGlobal(g): function []: g.val;
	case AccThis: function []: ctx.vthis;
	case AccEnv(i): function []: ctx.venvi;
	};

	public static function acc_set(ctx, p, acc, value) return {
		switch (acc) {
		case AccField(v, f): var h = hash_field(ctx, f);
			function []: var v = v([]);
			var value = value([]);
			switch (v) {
			case VObject(o): set_field(o, h, value);
				value;
			case _: throw(ctx, p, ^ ("Invalid field access : ", f));
			};
		case AccArray(e, index): function []: var e = e([]);
			var index = index([]);
			var value = value([]);
			switch ((new Tuple(index, e))) {
			case (VInt(i), VArray(a)): try {
					Array.set(a, i, value);
					value;
				} catch (e: _) {
					value;
				};
			case (VInt32(_), VArray(_)): value;
			case (_, VObject(o)): switch (eval_oop(ctx, p, o, h_set, ::(index, ::(value, [])))) {
				case None: throw(ctx, p, "Invalid array access");
				case Some(_): value;
				};
			case _: throw(ctx, p, "Invalid array access");
			};
		case AccLocal(i): function []: var value = value([]);
			DynArray.set(ctx.stack, -(DynArray.length(ctx.stack), i), value);
			value;
		case AccGlobal(g): function []: var value = value([]);
			g.val = value;
			value;
		case AccThis: function []: var value = value([]);
			ctx.vthis = value;
			value;
		case AccEnv(i): function []: var value = value([]);
			ctx.venvi = value;
			value;
		};
	};

	public static function number_op(ctx, p, sop, iop, fop, oop, rop, v1, v2, []) return {
		var v1 = v1([]);
		var v2 = v2([]);
		exc_number_op(ctx, p, sop, iop, fop, oop, rop, v1, v2);
	};

	public static function exc_number_op(ctx, p, sop, iop, fop, oop, rop, v1, v2) return {
		switch ((new Tuple(v1, v2))) {
		case (VInt(a), VInt(b)): best_int(iop(Int32.of_int(a), Int32.of_int(b)));
		case (VInt32(a), VInt(b)): best_int(iop(a, Int32.of_int(b)));
		case (VInt(a), VInt32(b)): best_int(iop(Int32.of_int(a), b));
		case (VFloat(a), VInt(b)): VFloat(fop(a, float_of_int(b)));
		case (VFloat(a), VInt32(b)): VFloat(fop(a, Int32.to_float(b)));
		case (VInt(a), VFloat(b)): VFloat(fop(float_of_int(a), b));
		case (VInt32(a), VFloat(b)): VFloat(fop(Int32.to_float(a), b));
		case (VFloat(a), VFloat(b)): VFloat(fop(a, b));
		case (VInt32(a), VInt32(b)): best_int(iop(a, b));
		case (VObject(o), _): switch (eval_oop(ctx, p, o, oop, ::(v2, []))) {
			case Some(v): v;
			case None: switch (v2) {
				case VObject(o): switch (eval_oop(ctx, p, o, rop, ::(v1, []))) {
					case Some(v): v;
					case None: throw(ctx, p, sop);
					};
				case _: throw(ctx, p, sop);
				};
			};
		case (_, VObject(o)): switch (eval_oop(ctx, p, o, rop, ::(v1, []))) {
			case Some(v): v;
			case None: throw(ctx, p, sop);
			};
		case _: throw(ctx, p, sop);
		};
	};

	public static function int_op(ctx, p, op, iop, v1, v2, []) return {
		var v1 = v1([]);
		var v2 = v2([]);
		switch ((new Tuple(v1, v2))) {
		case (VInt(a), VInt(b)): best_int(iop(Int32.of_int(a), Int32.of_int(b)));
		case (VInt32(a), VInt(b)): best_int(iop(a, Int32.of_int(b)));
		case (VInt(a), VInt32(b)): best_int(iop(Int32.of_int(a), b));
		case (VInt32(a), VInt32(b)): best_int(iop(a, b));
		case _: throw(ctx, p, op);
		};
	};

	public static function base_op(ctx, op, v1, v2, p) return {
		switch (op) {
		case +: function []: var v1 = v1([]);
			var v2 = v2([]);
			switch ((new Tuple(v1, v2))) {
			case (VInt(_) | VInt32(_), VInt(_) | VInt32(_)) | (VInt(_) | VInt32(_), VFloat(_)) | (VFloat(_), VInt(_) | VInt32(_)) | (VFloat(_), VFloat(_)) | (VObject(_), _) | (_, VObject(_))
					: exc_number_op(ctx, p, op, Int32.add, +., h_add, h_radd, v1, v2);
			case (VString(a), _): VString( ^ (a, ctx.do_string(v2)));
			case (_, VString(b)): VString( ^ (ctx.do_string(v1), b));
			case _: throw(ctx, p, op);
			};
		case -: number_op(ctx, p, op, Int32.sub, -., h_sub, h_rsub, v1, v2);
		case *: number_op(ctx, p, op, Int32.mul, * ., h_mult, h_rmult, v1, v2);
		case /: function []: var v1 = v1([]);
			var v2 = v2([]);
			switch ((new Tuple(v1, v2))) {
			case (VInt(i), VInt(j)): VFloat( / .(float_of_int(i), float_of_int(j)));
			case (VInt(i), VInt32(j)): VFloat( / .(float_of_int(i), Int32.to_float(j)));
			case (VInt32(i), VInt(j)): VFloat( / .(Int32.to_float(i), float_of_int(j)));
			case (VInt32(i), VInt32(j)): VFloat( / .(Int32.to_float(i), Int32.to_float(j)));
			case _: exc_number_op(ctx, p, op, Int32.div, / ., h_div, h_rdiv, v1, v2);
			};
		case %: number_op(ctx, p, op, function x: function y: if ( = (y, )) {
			throw(ctx, p, op);
			} else {
				[];
			};
			Int32.rem(x, y), mod_float, h_mod, h_rmod, v1, v2);
		case &: int_op(ctx, p, op, Int32.logand, v1, v2);
		case |: int_op(ctx, p, op, Int32.logor, v1, v2);
		case ^: int_op(ctx, p, op, Int32.logxor, v1, v2);
		case < <: int_op(ctx, p, op, function x: function y: Int32.shift_left(x, Int32.to_int(y)), v1, v2);
		case > >: int_op(ctx, p, op, function x: function y: Int32.shift_right(x, Int32.to_int(y)), v1, v2);
		case > > >: int_op(ctx, p, op, function x: function y: Int32.shift_right_logical(x, Int32.to_int(y)), v1, v2);
		case _: throw(ctx, p, op);
		};
	};

	public static function eval_op(ctx, op, e1, e2, p) return {
		switch (op) {
		case =: var acc = eval_access(ctx, e1);
			var v = eval(ctx, e2);
			acc_set(ctx, p, acc, v);
		case ==: var v1 = eval(ctx, e1);
			var v2 = eval(ctx, e2);
			function []: var v1 = v1([]);
			var v2 = v2([]);
			switch (ctx.do_compare(v1, v2)) {
			case CEq: VBool(True);
			case _: VBool(False);
			};
		case !=: var v1 = eval(ctx, e1);
			var v2 = eval(ctx, e2);
			function []: var v1 = v1([]);
			var v2 = v2([]);
			switch (ctx.do_compare(v1, v2)) {
			case CEq: VBool(False);
			case _: VBool(True);
			};
		case >: var v1 = eval(ctx, e1);
			var v2 = eval(ctx, e2);
			function []: var v1 = v1([]);
			var v2 = v2([]);
			switch (ctx.do_compare(v1, v2)) {
			case CSup: VBool(True);
			case _: VBool(False);
			};
		case >=: var v1 = eval(ctx, e1);
			var v2 = eval(ctx, e2);
			function []: var v1 = v1([]);
			var v2 = v2([]);
			switch (ctx.do_compare(v1, v2)) {
			case CSup | CEq: VBool(True);
			case _: VBool(False);
			};
		case <: var v1 = eval(ctx, e1);
			var v2 = eval(ctx, e2);
			function []: var v1 = v1([]);
			var v2 = v2([]);
			switch (ctx.do_compare(v1, v2)) {
			case CInf: VBool(True);
			case _: VBool(False);
			};
		case <=: var v1 = eval(ctx, e1);
			var v2 = eval(ctx, e2);
			function []: var v1 = v1([]);
			var v2 = v2([]);
			switch (ctx.do_compare(v1, v2)) {
			case CInf | CEq: VBool(True);
			case _: VBool(False);
			};
		case + | - | * | / | % | | | & | ^ | < < | > > | > > >: var v1 = eval(ctx, e1);
			var v2 = eval(ctx, e2);
			base_op(ctx, op, v1, v2, p);
		case += | -= | *= | /= | %= | < <= | > >= | > > >= | |= | &= | ^=: var Tuple(aset, aget) = eval_access_get_set(ctx, e1);
			var v1 = acc_get(ctx, p, aget);
			var v2 = eval(ctx, e2);
			var v = base_op(ctx, String.sub(op, 0, -(String.length(op), 1)), v1, v2, p);
			acc_set(ctx, p, aset, v);
		case &&: var e1 = eval(ctx, e1);
			var e2 = eval(ctx, e2);
			function []:
			switch (e1([])) {
			case VBool(False) = v: v;
			case _: e2([]);
			};
		case ||: var e1 = eval(ctx, e1);
			var e2 = eval(ctx, e2);
			function []:
			switch (e1([])) {
			case VBool(True) = v: v;
			case _: e2([]);
			};
		case ++= | --=: var Tuple(aset, aget) = eval_access_get_set(ctx, e1);
			var v1 = acc_get(ctx, p, aget);
			var v2 = eval(ctx, e2);
			var vcache = ref(VNull);
			var v = base_op(ctx, String.sub(op, 0, 1), function []: vcache.val = v1([]);
							vcache.val, v2, p);
			var set = acc_set(ctx, p, aset, v);
			function []: ignore(set([]));
			vcache.val;
		case _: throw(ctx, p, ^ ("Unsupported ", op));
		};
	};

	public static function call(ctx, vthis, vfun, pl, p) return {
		var oldthis = ctx.vthis;
		var stackpos = DynArray.length(ctx.stack);
		var oldstack = ctx.callstack;
		var oldsize = ctx.callsize;
		var oldenv = ctx.venv;
		ctx.vthis = vthis;
		ctx.callstack = ::({
			() with cpos = p;
			cthis = oldthis;
			cstack = stackpos;
			cenv = oldenv
		}, ctx.callstack);
		ctx.callsize = +(oldsize, 1);
		if ( > (oldsize, 600)) {
			exc(VString("Stack overflow"));
		} else {
			[];
		};
		var ret = try {
			switch (vfun) {
			case VClosure(vl, f): f(vl, pl);
			case VFunction(f): switch ((new Tuple(pl, f))) {
				case ([], Fun0(f)): f([]);
				case (::(a, []), Fun1(f)): f(a);
				case (::(a, ::(b, [])), Fun2(f)): f(a, b);
				case (::(a, ::(b, ::(c, []))), Fun3(f)): f(a, b, c);
				case (::(a, ::(b, ::(c, ::(d, [])))), Fun4(f)): f(a, b, c, d);
				case (::(a, ::(b, ::(c, ::(d, ::(e, []))))), Fun5(f)): f(a, b, c, d, e);
				case (_, FunVar(f)): f(pl);
				case _: exc(VString(Printf.sprintf("Invalid call [%d args instead of %d]", List.length(pl), nargs(f))));
				};
			case VAbstract(ALazyType(f)): encode_type(f.val([]));
			case _: exc(VString("Invalid call"));
			};
		} catch (e: T) {
			McOr(McArr(PaApp(PaId(IdUid(<...>)), PaId(IdLid(<...>))), ExNil, ExId(IdLid(v))), McOr(McArr(PaId(IdUid(<...>)), ExNil,
					ExApp(ExId(<...>), ExApp(<...>, <...>))), McOr(McArr(PaOrp(<...>, <...>), ExNil, ExApp(<...>, <...>)), McOr(McArr(<...>,
						<...>, <...>), McArr(<...>, <...>, <...>)))))			case Return(v): v;
		case Stack_overflow: exc(VString("Compiler Stack overflow"));
		case Sys_error(msg) | Failure(msg): exc(VString(msg));
		case Unix.Unix_error(_, cmd, msg): exc(VString( ^ ("Error ", ^ (cmd, ^ (" ", msg)))));
		case Builtin_error | Invalid_argument(_): exc(VString("Invalid call"));
		};
		ctx.vthis = oldthis;
		ctx.venv = oldenv;
		ctx.callstack = oldstack;
		ctx.callsize = oldsize;
		pop(ctx, -(DynArray.length(ctx.stack), stackpos));
		ret;
	};

	public static function to_string(ctx, n, v) return {
		if ( > (n, 5)) {
			"<...>";
		} else {
			var n = +(n, 1);
			switch (v) {
			case VNull: "null";
			case VBool(True): "true";
			case VBool(False): "false";
			case VInt(i): string_of_int(i);
			case VInt32(i): Int32.to_string(i);
			case VFloat(f): var s = float_repres(f);
				var len = String.length(s);
				if ( = (String.unsafe_get(s, -(len, 1)), '.')) {
					String.sub(s, 0, -(len, 1));
				} else {
					s;
				};
			case VString(s): s;
			case VArray(vl): ^ ("[", ^ (String.concat(",", Array.to_list(Array.map(to_string(ctx, n), vl))), "]"));
			case VAbstract(a): switch (a) {
				case APos(p): ^ ("#pos[", ^ (Lexer.get_error_pos(Printf.sprintf("%s:%d:"), p), "]"));
				case _: "#abstract";
				};
			case VFunction(f): ^ ("#function:", string_of_int(nargs(f)));
			case VClosure(_): "#function:-1";
			case VObject(o): switch (eval_oop(ctx, null_pos, o, h_string, [])) {
				case Some(VString(s)): s;
				case _: var b = Buffer.create(0);
					var first = ref(True);
					Buffer.add_char(b, '{');
					Array.iter(function (f, v):
					if (first.val) {
					Buffer.add_char(b, ' ');
						first.val = False;
					} else {
						Buffer.add_string(b, ", ");
					};
					Buffer.add_string(b, field_name(ctx, f));
					Buffer.add_string(b, " => ");
					Buffer.add_string(b, to_string(ctx, n, v)), o.ofields);
					Buffer.add_string(b, if (first.val) {
					"}";
				} else {
					" }";
				});
					Buffer.contents(b);
				};
			};
		};
	};

	public static function compare(ctx, a, b) return {
		function fcmp(afloat, b) return {
			if ( = (a, b)) {
				CEq;
			} else {
				if ( < (a, b)) {
					CInf;
				} else {
					CSup;
				};
			};
		};
		function scmp(astring, b) return {
			if ( = (a, b)) {
				CEq;
			} else {
				if ( < (a, b)) {
					CInf;
				} else {
					CSup;
				};
			};
		};
		function icmp(aint32, b) return {
			var l = Int32.compare(a, b);
			if ( = (l, 0)) {
				CEq;
			} else {
				if ( < (l, 0)) {
					CInf;
				} else {
					CSup;
				};
			};
		};
		switch ((new Tuple(a, b))) {
		case (VNull, VNull): CEq;
		case (VInt(a), VInt(b)): if ( = (a, b)) {
				CEq;
			} else {
				if ( < (a, b)) {
					CInf;
				} else {
					CSup;
				};
			};
		case (VInt32(a), VInt32(b)): icmp(a, b);
		case (VInt(a), VInt32(b)): icmp(Int32.of_int(a), b);
		case (VInt32(a), VInt(b)): icmp(a, Int32.of_int(b));
		case (VFloat(a), VFloat(b)): fcmp(a, b);
		case (VFloat(a), VInt(b)): fcmp(a, float_of_int(b));
		case (VFloat(a), VInt32(b)): fcmp(a, Int32.to_float(b));
		case (VInt(a), VFloat(b)): fcmp(float_of_int(a), b);
		case (VInt32(a), VFloat(b)): fcmp(Int32.to_float(a), b);
		case (VBool(a), VBool(b)): if ( = (a, b)) {
				CEq;
			} else {
				if (a) {
					CSup;
				} else {
					CInf;
				};
			};
		case (VString(a), VString(b)): scmp(a, b);
		case (VInt(_), VString(s)) | (VInt32(_), VString(s)) | (VFloat(_), VString(s)) | (VBool(_), VString(s)): scmp(to_string(
						ctx, 0, a), s);
		case (VString(s), VInt(_)) | (VString(s), VInt32(_)) | (VString(s), VFloat(_)) | (VString(s), VBool(_)): scmp(s,
					to_string(ctx, 0, b));
		case (VObject(oa), VObject(ob)): if ( == (oa, ob)) {
				CEq;
			} else {
				switch (eval_oop(ctx, null_pos, oa, h_compare, ::(b, []))) {
				case Some(VInt(i)): if ( = (i, 0)) {
						CEq;
					} else {
						if ( < (i, 0)) {
							CInf;
						} else {
							CSup;
						};
					};
				case _: CUndef;
				};
			};
		case (VAbstract(a), VAbstract(b)): if ( == (a, b)) {
				CEq;
			} else {
				CUndef;
			};
		case (VArray(a), VArray(b)): if ( == (a, b)) {
				CEq;
			} else {
				CUndef;
			};
		case (VFunction(a), VFunction(b)): if ( == (a, b)) {
				CEq;
			} else {
				CUndef;
			};
		case (VClosure(la, fa), VClosure(lb, fb)): if ( && ( == (la, lb), == (fa, fb))) {
				CEq;
			} else {
				CUndef;
			};
		case _: CUndef;
		};
	};

	public static function select(ctx) return {
		get_ctx_ref.val = function []: ctx;
	};

	public static function load_prim(ctx, f, n) return {
		switch ((new Tuple(f, n))) {
		case (VString(f), VInt(n)): var Tuple(lib, fname) = try {
				ExtString.String.split(f, "@");
			} catch (e: _) {
				(new Tuple("", f));
			};
			try {
				var f = switch (lib) {
				case std: Hashtbl.find(std_lib, fname);
				case macro: Hashtbl.find(macro_lib, fname);
				case regexp: Hashtbl.find(reg_lib, fname);
				case zlib: Hashtbl.find(z_lib, fname);
				case _: failwith( ^ ("You cannot use the library '", ^ (lib, "' inside a macro")));
				};
				if (<>(nargs(f), n)) {
					raise(Not_found);
				} else {
					[];
				};
				VFunction(f);
			} catch (e: Not_found) {
				VFunction(FunVar(function _: exc(VString( ^ ("Primitive not found ", ^ (f, ^ (":", string_of_int(n))))))));
			};
		case _: exc(VString("Invalid call"));
		};
	};

	public static function create(com, api) return {
		var loader = obj(hash, ::((new Tuple("args", VArray(Array.of_list(List.map(function s: VString(s), com.sys_args))))), ::((new Tuple("loadprim", VFunction(Fun2(function a: function b: get_ctx([]).do_loadprim(a, b))))), ::((new Tuple("loadmodule", VFunction(Fun2(function a: function b: assert False)))), []))));
		var ctx = {
			() with gen = Genneko.new_context(com, 2, True);
			types = Hashtbl.create(0);
			error = False;
			error_proto = {
				() with ofields = [];
				oproto = None
			};
			prototypes = Hashtbl.create(0);
			enums = [];
			locals_map = PMap.empty;
			locals_count = 0;
			locals_barrier = 0;
			locals_env = DynArray.create([]);
			globals = PMap.empty;
			callstack = [];
			callsize = 0;
			stack = DynArray.create([]);
			exc = [];
			vthis = VNull;
			venv = [];
			fields_cache = Hashtbl.copy(constants);
			do_call = Obj.magic([]);
			do_string = Obj.magic([]);
			do_loadprim = Obj.magic([]);
			do_compare = Obj.magic([]);
			curapi = api;
			loader = VObject(loader);
			on_reused = [];
			is_reused = True;
			exports = VObject({ () with ofields = [];
								oproto = None
							  })
		};
		ctx.do_call = call(ctx);
		ctx.do_string = to_string(ctx, 0);
		ctx.do_loadprim = load_prim(ctx);
		ctx.do_compare = compare(ctx);
		select(ctx);
		List.iter(function e: ignore(eval(ctx, e, [])), Genneko.header([]));
		ctx;
	};

	public static function do_reuse(ctx, api) return {
		ctx.is_reused = False;
		ctx.curapi = api;
	};

	public static function can_reuse(ctx, types) return {
		function has_old_version(t) return {
			var inf = Type.t_infos(t);
			try {
				<>(Hashtbl.find(ctx.types, inf.mt_path), inf.mt_module.m_id);
			} catch (e: Not_found) {
				False;
			};
		};
		if (List.exists(has_old_version, types)) {
			False;
		} else {
			if (ctx.is_reused) {
				True;
			} else {
				if (!(List.for_all(function f: f([]), ctx.on_reused))) {
					False;
				} else {
					ctx.is_reused = True;
					True;
				};
			};
		};
	};

	public static function add_types(ctx, types, ready) return {
		var types = List.filter(function t: var path = Type.t_path(t);
		if (Hashtbl.mem(ctx.types, path)) {
		False;
	} else {
		Hashtbl.add(ctx.types, path, Type.t_infos(t).mt_module.m_id);
			True;
		}, types);
		List.iter(ready, types);
		var e = (new Tuple(EBlock(Genneko.build(ctx.gen, types)), null_pos));
		ignore(catch_errors(ctx, function []: ignore(eval(ctx, e, []))));
	};

	public static function get_path(ctx, path, p) return {
		function loop(match) return switch (match) {
		case []: assert False;
		case ::(x, []): (new Tuple(EConst(Ident(x)), p));
		case ::(x, l): (new Tuple(EField(loop(l), x), p));
		};
		eval(ctx, loop(List.rev(path)), []);
	};

	public static function set_error(ctx, e) return {
		ctx.error = e;
	};

	public static function call_path(ctx, path, f, vl, api) return {
		if (ctx.error) {
			None;
		} else {
			var old = ctx.curapi;
			ctx.curapi = api;
			var p = Genneko.pos(ctx.gen, api.pos);
			catch_errors(ctx, final = function []: ctx.curapi = old, function []:
			switch (get_path(ctx, path, p)) {
		case VObject(o): var f = get_field(o, hash(f));
				call(ctx, VObject(o), f, vl, p);
			case _: assert False;
			});
		};
	};

	public static function enum_name(match) return switch (match) {
	case IExpr: "ExprDef";
	case IBinop: "Binop";
	case IUnop: "Unop";
	case IConst: "Constant";
	case ITParam: "TypeParam";
	case ICType: "ComplexType";
	case IField: "FieldType";
	case IType: "Type";
	case IFieldKind: "FieldKind";
	case IMethodKind: "MethodKind";
	case IVarAccess: "VarAccess";
	case IAccess: "Access";
	case IClassKind: "ClassKind";
	case ITypedExpr: "TypedExprDef";
	case ITConstant: "TConstant";
	case IModuleType: "ModuleType";
	case IFieldAccess: "FieldAccess";
	case IAnonStatus: "AnonStatus";
	case IImportMode: "ImportMode";
	};

	public static function init(ctx) return {
		var enums = ::(IExpr, ::(IBinop, ::(IUnop, ::(IConst, ::(ITParam, ::(ICType, ::(IField, ::(IType, ::(IFieldKind, ::(IMethodKind, ::(IVarAccess, ::(IAccess, ::(IClassKind, ::(ITypedExpr, ::(ITConstant, ::(IModuleType, ::(IFieldAccess, ::(IAnonStatus, ::(IImportMode, [])))))))))))))))))));
		function get_enum_proto(e) return {
			switch (get_path(ctx, ::("haxe", ::("macro", ::(enum_name(e), []))), null_pos)) {
			case VObject(e): switch (get_field(e, h_constructs)) {
				case VObject(cst): switch (get_field(cst, h_a)) {
					case VArray(a): Array.map(function s: switch (s) {
					case VObject(s): switch (get_field(s, h_s)) {
							case VString(s): (new Tuple(get_field(e, hash(s)), s));
							case _: assert False;
							};
						case _: assert False;
						}, a);
					case _: assert False;
					};
				case _: assert False;
				};
			case _: failwith( ^ ("haxe.macro.", ^ (enum_name(e), " does not exists")));
			};
		};
		ctx.enums = Array.of_list(List.map(get_enum_proto, enums));
		ctx.error_proto = switch (get_path(ctx, ::("haxe", ::("macro", ::("Error", ::("prototype", [])))), null_pos)) {
		case VObject(p): p;
		case _: failwith("haxe.macro.Error does not exists");
		};
	};

	public static function null(f) return {
	case None: VNull;
	case Some(v): f(v);
	};

	public static function encode_pos(p) return {
		VAbstract(APos(p));
	};

	public static function enc_inst(path, fields) return {
		var ctx = get_ctx([]);
		var p = try {
			Hashtbl.find(ctx.prototypes, path);
		} catch (e: Not_found) {
			try {
				switch (get_path(ctx, @(path, ::("prototype", [])), Nast.null_pos)) {
				case VObject(o): Hashtbl.add(ctx.prototypes, path, o);
					o;
				case _: raise(Runtime(VNull));
				};
			} catch (e: Runtime(_)) {
				failwith( ^ ("Prototype not found ", String.concat(".", path)));
			};
		};
		var o = obj(hash, fields);
		o.oproto = Some(p);
		VObject(o);
	};

	public static function enc_array(l) return {
		var a = Array.of_list(l);
		enc_inst(::("Array", []), ::((new Tuple("__a", VArray(a))), ::((new Tuple("length", VInt(Array.length(a)))), [])));
	};

	public static function enc_string(s) return {
		enc_inst(::("String", []), ::((new Tuple("__s", VString(s))), ::((new Tuple("length", VInt(String.length(s)))), [])));
	};

	public static function enc_hash(h) return {
		enc_inst(::("haxe", ::("ds", ::("StringMap", []))), ::((new Tuple("h", VAbstract(AHash(h)))), []));
	};

	public static function enc_obj(l) return {
		VObject(obj(hash, l));
	};

	public static function enc_enum(ienum_index, index, pl) return {
		var eindex = Obj.magic(i) : int;
		var edef = get_ctx([]).enumseindex;
		if ( = (pl, [])) {
			fst(edefindex);
		} else {
			enc_inst(::("haxe", ::("macro", ::(enum_name(i), []))), ::((new Tuple("tag", VString(snd(edefindex)))), ::((new Tuple("index", VInt(index))), ::((new Tuple("args", VArray(Array.of_list(pl)))), []))));
		};
	};

	public static function compiler_error(msg, pos) return {
		exc(enc_inst(::("haxe", ::("macro", ::("Error", []))), ::((new Tuple("message", enc_string(msg))), ::((new Tuple("pos", encode_pos(pos))), []))));
	};

	public static function encode_const(c) return {
		var Tuple(tag, pl) = switch (c) {
		case Int(s): (new Tuple(0, ::(enc_string(s), [])));
		case Float(s): (new Tuple(1, ::(enc_string(s), [])));
		case String(s): (new Tuple(2, ::(enc_string(s), [])));
		case Ident(s): (new Tuple(3, ::(enc_string(s), [])));
		case Regexp(s, opt): (new Tuple(4, ::(enc_string(s), ::(enc_string(opt), []))));
		};
		enc_enum(IConst, tag, pl);
	};

	public static function encode_binop(op) return {
		var Tuple(tag, pl) = switch (op) {
		case OpAdd: (new Tuple(0, []));
		case OpMult: (new Tuple(1, []));
		case OpDiv: (new Tuple(2, []));
		case OpSub: (new Tuple(3, []));
		case OpAssign: (new Tuple(4, []));
		case OpEq: (new Tuple(5, []));
		case OpNotEq: (new Tuple(6, []));
		case OpGt: (new Tuple(7, []));
		case OpGte: (new Tuple(8, []));
		case OpLt: (new Tuple(9, []));
		case OpLte: (new Tuple(10, []));
		case OpAnd: (new Tuple(11, []));
		case OpOr: (new Tuple(12, []));
		case OpXor: (new Tuple(13, []));
		case OpBoolAnd: (new Tuple(14, []));
		case OpBoolOr: (new Tuple(15, []));
		case OpShl: (new Tuple(16, []));
		case OpShr: (new Tuple(17, []));
		case OpUShr: (new Tuple(18, []));
		case OpMod: (new Tuple(19, []));
		case OpAssignOp(op): (new Tuple(20, ::(encode_binop(op), [])));
		case OpInterval: (new Tuple(21, []));
		case OpArrow: (new Tuple(22, []));
		};
		enc_enum(IBinop, tag, pl);
	};

	public static function encode_unop(op) return {
		var tag = switch (op) {
		case Increment: 0;
		case Decrement: 1;
		case Not: 2;
		case Neg: 3;
		case NegBits: 4;
		};
		enc_enum(IUnop, tag, []);
	};

	public static function encode_import(Tuple(path, mode)) return {
		var Tuple(tag, pl) = switch (mode) {
		case INormal: (new Tuple(0, []));
		case IAsName(s): (new Tuple(1, ::(enc_string(s), [])));
		case IAll: (new Tuple(2, []));
		};
		var mode = enc_enum(IImportMode, tag, pl);
		enc_obj(::((new Tuple("path", enc_array(List.map(function (name, p): enc_obj(::((new Tuple("pos", encode_pos(p))), ::((new Tuple("name", enc_string(name))), []))), path)))), ::((new Tuple("mode", mode)), [])));
	};

	public static function encode_path(t) return {
		var fields = ::((new Tuple("pack", enc_array(List.map(enc_string, t.tpackage)))), ::((new Tuple("name", enc_string(t.tname))), ::((new Tuple("params", enc_array(List.map(encode_tparam, t.tparams)))), [])));
		enc_obj(switch (t.tsub) {
	case None: fields;
	case Some(s): ::((new Tuple("sub", enc_string(s))), fields);
		});
	};

	public static function encode_tparam(match) return switch (match) {
	case TPType(t): enc_enum(ITParam, 0, ::(encode_ctype(t), []));
	case TPExpr(e): enc_enum(ITParam, 1, ::(encode_expr(e), []));
	};

	public static function encode_access(a) return {
		var tag = switch (a) {
		case APublic: 0;
		case APrivate: 1;
		case AStatic: 2;
		case AOverride: 3;
		case ADynamic: 4;
		case AInline: 5;
		case AMacro: 6;
		};
		enc_enum(IAccess, tag, []);
	};

	public static function encode_meta_entry(Tuple(m, ml, p)) return {
		enc_obj(::((new Tuple("name", enc_string(fst(MetaInfo.to_string(m))))), ::((new Tuple("params", enc_array(List.map(encode_expr, ml)))), ::((new Tuple("pos", encode_pos(p))), []))));
	};

	public static function encode_meta_content(m) return {
		enc_array(List.map(encode_meta_entry, m));
	};

	public static function encode_field(fclass_field) return {
		var Tuple(tag, pl) = switch (f.cff_kind) {
		case FVar(t, e): (new Tuple(0, ::(null(encode_ctype, t), ::(null(encode_expr, e), []))));
		case FFun(f): (new Tuple(1, ::(encode_fun(f), [])));
		case FProp(get, set, t, e): (new Tuple(2, ::(enc_string(get), ::(enc_string(set), ::(null(encode_ctype, t),
			::(null(encode_expr, e), []))))));
		};
		enc_obj(::((new Tuple("name", enc_string(f.cff_name))), ::((new Tuple("doc", null(enc_string, f.cff_doc))), ::((new Tuple("pos", encode_pos(f.cff_pos))), ::((new Tuple("kind", enc_enum(IField, tag, pl))), ::((new Tuple("meta", encode_meta_content(f.cff_meta))), ::((new Tuple("access", enc_array(List.map(encode_access, f.cff_access)))), [])))))));
	};

	public static function encode_ctype(t) return {
		var Tuple(tag, pl) = switch (t) {
		case CTPath(p): (new Tuple(0, ::(encode_path(p), [])));
		case CTFunction(pl, r): (new Tuple(1, ::(enc_array(List.map(encode_ctype, pl)), ::(encode_ctype(r), []))));
		case CTAnonymous(fl): (new Tuple(2, ::(enc_array(List.map(encode_field, fl)), [])));
		case CTParent(t): (new Tuple(3, ::(encode_ctype(t), [])));
		case CTExtend(tl, fields): (new Tuple(4, ::(enc_array(List.map(encode_path, tl)), ::(enc_array(List.map(encode_field,
			fields)), []))));
		case CTOptional(t): (new Tuple(5, ::(encode_ctype(t), [])));
		};
		enc_enum(ICType, tag, pl);
	};

	public static function encode_tparam_decl(tp) return {
		enc_obj(::((new Tuple("name", enc_string(tp.tp_name))), ::((new Tuple("params", enc_array(List.map(encode_tparam_decl, tp.tp_params)))), ::((new Tuple("constraints", enc_array(List.map(encode_ctype, tp.tp_constraints)))), ::((new Tuple("meta", encode_meta_content(tp.tp_meta))), [])))));
	};

	public static function encode_fun(f) return {
		enc_obj(::((new Tuple("params", enc_array(List.map(encode_tparam_decl, f.f_params)))), ::((new Tuple("args", enc_array(List.map(function (n, opt, t, e): enc_obj(::((new Tuple("name", enc_string(n))), ::((new Tuple("opt", VBool(opt))), ::((new Tuple("type", null(encode_ctype, t))), ::((new Tuple("value", null(encode_expr, e))), []))))), f.f_args)))), ::((new Tuple("ret", null(encode_ctype, f.f_type))), ::((new Tuple("expr", null(encode_expr, f.f_expr))), [])))));
	};

	public static function encode_expr(e) return {
		function loop(Tuple(e, p)) return {
			var Tuple(tag, pl) = switch (e) {
			case EConst(c): (new Tuple(0, ::(encode_const(c), [])));
			case EArray(e1, e2): (new Tuple(1, ::(loop(e1), ::(loop(e2), []))));
			case EBinop(op, e1, e2): (new Tuple(2, ::(encode_binop(op), ::(loop(e1), ::(loop(e2), [])))));
			case EField(e, f): (new Tuple(3, ::(loop(e), ::(enc_string(f), []))));
			case EParenthesis(e): (new Tuple(4, ::(loop(e), [])));
			case EObjectDecl(fl): (new Tuple(5, ::(enc_array(List.map(function (f, e): enc_obj(::((new Tuple("field", enc_string(f))),
				::((new Tuple("expr", loop(e))), []))), fl)), [])));
			case EArrayDecl(el): (new Tuple(6, ::(enc_array(List.map(loop, el)), [])));
			case ECall(e, el): (new Tuple(7, ::(loop(e), ::(enc_array(List.map(loop, el)), []))));
			case ENew(p, el): (new Tuple(8, ::(encode_path(p), ::(enc_array(List.map(loop, el)), []))));
			case EUnop(op, flag, e): (new Tuple(9, ::(encode_unop(op), ::(VBool(switch (flag) {
			case Prefix: False;
			case Postfix: True;
			}), ::(loop(e), [])))));
			case EVars(vl): (new Tuple(10, ::(enc_array(List.map(function (v, t, eo): enc_obj(::((new Tuple("name", enc_string(v))),
												  ::((new Tuple("type", null(encode_ctype, t))), ::((new Tuple("expr", null(loop, eo))), [])))), vl)), [])));
			case EFunction(name, f): (new Tuple(11, ::(null(enc_string, name), ::(encode_fun(f), []))));
			case EBlock(el): (new Tuple(12, ::(enc_array(List.map(loop, el)), [])));
			case EFor(e, eloop): (new Tuple(13, ::(loop(e), ::(loop(eloop), []))));
			case EIn(e1, e2): (new Tuple(14, ::(loop(e1), ::(loop(e2), []))));
			case EIf(econd, e, eelse): (new Tuple(15, ::(loop(econd), ::(loop(e), ::(null(loop, eelse), [])))));
			case EWhile(econd, e, flag): (new Tuple(16, ::(loop(econd), ::(loop(e), ::(VBool(switch (flag) {
			case NormalWhile: True;
			case DoWhile: False;
			}), [])))));
			case ESwitch(e, cases, eopt): (new Tuple(17, ::(loop(e), ::(enc_array(List.map(function (ecl, eg,
											   e): enc_obj(::((new Tuple("values", enc_array(List.map(loop, ecl)))), ::((new Tuple("guard", null(loop, eg))),
													   ::((new Tuple("expr", null(loop, e))), [])))), cases)), ::(null(encode_null_expr, eopt), [])))));
			case ETry(e, catches): (new Tuple(18, ::(loop(e), ::(enc_array(List.map(function (v, t, e): enc_obj(::((new Tuple("name",
												  enc_string(v))), ::((new Tuple("type", encode_ctype(t))), ::((new Tuple("expr", loop(e))), [])))), catches)), []))));
			case EReturn(eo): (new Tuple(19, ::(null(loop, eo), [])));
			case EBreak: (new Tuple(20, []));
			case EContinue: (new Tuple(21, []));
			case EUntyped(e): (new Tuple(22, ::(loop(e), [])));
			case EThrow(e): (new Tuple(23, ::(loop(e), [])));
			case ECast(e, t): (new Tuple(24, ::(loop(e), ::(null(encode_ctype, t), []))));
			case EDisplay(e, flag): (new Tuple(25, ::(loop(e), ::(VBool(flag), []))));
			case EDisplayNew(t): (new Tuple(26, ::(encode_path(t), [])));
			case ETernary(econd, e1, e2): (new Tuple(27, ::(loop(econd), ::(loop(e1), ::(loop(e2), [])))));
			case ECheckType(e, t): (new Tuple(28, ::(loop(e), ::(encode_ctype(t), []))));
			case EMeta(m, e): (new Tuple(29, ::(encode_meta_entry(m), ::(loop(e), []))));
			};
			enc_obj(::((new Tuple("pos", encode_pos(p))), ::((new Tuple("expr", enc_enum(IExpr, tag, pl))), [])));
		};
		loop(e);
	};

	public static function encode_null_expr(e) return {
		switch (e) {
		case None: enc_obj(::((new Tuple("pos", VNull)), ::((new Tuple("expr", VNull)), [])));
		case Some(e): encode_expr(e);
		};
	};

	public static function opt(f, v) return {
		switch (v) {
		case VNull: None;
		case _: Some(f(v));
		};
	};

	public static function opt_list(f, v) return {
		switch (v) {
		case VNull: [];
		case _: f(v);
		};
	};

	public static function decode_pos(match) return switch (match) {
	case VAbstract(APos(p)): p;
	case _: raise(Invalid_expr);
	};

	public static function field(v, f) return {
		switch (v) {
		case VObject(o): get_field(o, hash(f));
		case _: raise(Invalid_expr);
		};
	};

	public static function decode_enum(v) return {
		switch ((new Tuple(field(v, "index"), field(v, "args")))) {
		case (VInt(i), VNull): (new Tuple(i, []));
		case (VInt(i), VArray(a)): (new Tuple(i, Array.to_list(a)));
		case _: raise(Invalid_expr);
		};
	};

	public static function dec_bool(match) return switch (match) {
	case VBool(b): b;
	case _: raise(Invalid_expr);
	};

	public static function dec_string(v) return {
		switch (field(v, "__s")) {
		case VString(s): s;
		case _: raise(Invalid_expr);
		};
	};

	public static function dec_array(v) return {
		switch ((new Tuple(field(v, "__a"), field(v, "length")))) {
		case (VArray(a), VInt(l)): Array.to_list(if ( = (Array.length(a), l)) {
			a;
		} else {
			Array.sub(a, 0, l);
			});
		case _: raise(Invalid_expr);
		};
	};

	public static function decode_const(c) return {
		switch (decode_enum(c)) {
		case (0, ::(s, [])): Int(dec_string(s));
		case (1, ::(s, [])): Float(dec_string(s));
		case (2, ::(s, [])): String(dec_string(s));
		case (3, ::(s, [])): Ident(dec_string(s));
		case (4, ::(s, ::(opt, []))): Regexp(dec_string(s), dec_string(opt));
		case (5, ::(s, [])): Ident(dec_string(s));
		case _: raise(Invalid_expr);
		};
	};

	public static function decode_op(op) return {
		switch (decode_enum(op)) {
		case (0, []): OpAdd;
		case (1, []): OpMult;
		case (2, []): OpDiv;
		case (3, []): OpSub;
		case (4, []): OpAssign;
		case (5, []): OpEq;
		case (6, []): OpNotEq;
		case (7, []): OpGt;
		case (8, []): OpGte;
		case (9, []): OpLt;
		case (10, []): OpLte;
		case (11, []): OpAnd;
		case (12, []): OpOr;
		case (13, []): OpXor;
		case (14, []): OpBoolAnd;
		case (15, []): OpBoolOr;
		case (16, []): OpShl;
		case (17, []): OpShr;
		case (18, []): OpUShr;
		case (19, []): OpMod;
		case (20, ::(op, [])): OpAssignOp(decode_op(op));
		case (21, []): OpInterval;
		case (22, []): OpArrow;
		case _: raise(Invalid_expr);
		};
	};

	public static function decode_unop(op) return {
		switch (decode_enum(op)) {
		case (0, []): Increment;
		case (1, []): Decrement;
		case (2, []): Not;
		case (3, []): Neg;
		case (4, []): NegBits;
		case _: raise(Invalid_expr);
		};
	};

	public static function decode_import_mode(t) return {
		switch (decode_enum(t)) {
		case (0, []): INormal;
		case (1, ::(alias, [])): IAsName(dec_string(alias));
		case (2, []): IAll;
		case _: raise(Invalid_expr);
		};
	};

	public static function decode_import(t) return {
		(new Tuple(List.map(function o: (new Tuple(dec_string(field(o, "name")), decode_pos(field(o, "pos")))), dec_array(field(t, "path"))), decode_import_mode(field(t, "mode"))));
	};

	public static function decode_path(t) return {
		{
			() with tpackage = List.map(dec_string, dec_array(field(t, "pack")));
			tname = dec_string(field(t, "name"));
			tparams = switch (field(t, "params")) {
			case VNull: [];
			case a: List.map(decode_tparam, dec_array(a));
			};
			tsub = opt(dec_string, field(t, "sub"))
		};
	};

	public static function decode_tparam(v) return {
		switch (decode_enum(v)) {
		case (0, ::(t, [])): TPType(decode_ctype(t));
		case (1, ::(e, [])): TPExpr(decode_expr(e));
		case _: raise(Invalid_expr);
		};
	};

	public static function decode_tparams(match) return switch (match) {
	case VNull: [];
	case a: List.map(decode_tparam_decl, dec_array(a));
	};

	public static function decode_tparam_decl(v) return {
		{
			() with tp_name = dec_string(field(v, "name"));
			tp_constraints = switch (field(v, "constraints")) {
			case VNull: [];
			case a: List.map(decode_ctype, dec_array(a));
			};
			tp_params = decode_tparams(field(v, "params"));
			tp_meta = decode_meta_content(field(v, "meta"))
		};
	};

	public static function decode_fun(v) return {
		{
			() with f_params = decode_tparams(field(v, "params"));
			f_args = List.map(function o: (new Tuple(dec_string(field(o, "name")), switch (field(o, "opt")) {
		case VNull: False;
		case v: dec_bool(v);
			}, opt(decode_ctype, field(o, "type")), opt(decode_expr, field(o, "value")))), dec_array(field(v, "args")));
			f_type = opt(decode_ctype, field(v, "ret"));
			f_expr = opt(decode_expr, field(v, "expr"))
		};
	};

	public static function decode_access(v) return {
		switch (decode_enum(v)) {
		case (0, []): APublic;
		case (1, []): APrivate;
		case (2, []): AStatic;
		case (3, []): AOverride;
		case (4, []): ADynamic;
		case (5, []): AInline;
		case (6, []): AMacro;
		case _: raise(Invalid_expr);
		};
	};

	public static function decode_meta_entry(v) return {
		(new Tuple(MetaInfo.from_string(dec_string(field(v, "name"))), switch (field(v, "params")) {
	case VNull: [];
		case a: List.map(decode_expr, dec_array(a));
		}, decode_pos(field(v, "pos"))));
	};

	public static function decode_meta_content(match) return switch (match) {
	case VNull: [];
	case v: List.map(decode_meta_entry, dec_array(v));
	};

	public static function decode_field(v) return {
		var fkind = switch (decode_enum(field(v, "kind"))) {
		case (0, ::(t, ::(e, []))): FVar(opt(decode_ctype, t), opt(decode_expr, e));
		case (1, ::(f, [])): FFun(decode_fun(f));
		case (2, ::(get, ::(set, ::(t, ::(e, []))))): FProp(dec_string(get), dec_string(set), opt(decode_ctype, t),
			opt(decode_expr, e));
		case _: raise(Invalid_expr);
		};
		{
			() with cff_name = dec_string(field(v, "name"));
			cff_doc = opt(dec_string, field(v, "doc"));
			cff_pos = decode_pos(field(v, "pos"));
			cff_kind = fkind;
			cff_access = List.map(decode_access, opt_list(dec_array, field(v, "access")));
			cff_meta = opt_list(decode_meta_content, field(v, "meta"))
		};
	};

	public static function decode_ctype(t) return {
		switch (decode_enum(t)) {
		case (0, ::(p, [])): CTPath(decode_path(p));
		case (1, ::(a, ::(r, []))): CTFunction(List.map(decode_ctype, dec_array(a)), decode_ctype(r));
		case (2, ::(fl, [])): CTAnonymous(List.map(decode_field, dec_array(fl)));
		case (3, ::(t, [])): CTParent(decode_ctype(t));
		case (4, ::(tl, ::(fl, []))): CTExtend(List.map(decode_path, dec_array(tl)), List.map(decode_field, dec_array(fl)));
		case (5, ::(t, [])): CTOptional(decode_ctype(t));
		case _: raise(Invalid_expr);
		};
	};

	public static function decode_expr(v) return {
		function loop(v) return {
			(new Tuple(decode(field(v, "expr")), decode_pos(field(v, "pos"))));
		};
		function decode(e) return {
			switch (decode_enum(e)) {
			case (0, ::(c, [])): EConst(decode_const(c));
			case (1, ::(e1, ::(e2, []))): EArray(loop(e1), loop(e2));
			case (2, ::(op, ::(e1, ::(e2, [])))): EBinop(decode_op(op), loop(e1), loop(e2));
			case (3, ::(e, ::(f, []))): EField(loop(e), dec_string(f));
			case (4, ::(e, [])): EParenthesis(loop(e));
			case (5, ::(a, [])): EObjectDecl(List.map(function o: (new Tuple(dec_string(field(o, "field")), loop(field(o, "expr")))),
				dec_array(a)));
			case (6, ::(a, [])): EArrayDecl(List.map(loop, dec_array(a)));
			case (7, ::(e, ::(el, []))): ECall(loop(e), List.map(loop, dec_array(el)));
			case (8, ::(t, ::(el, []))): ENew(decode_path(t), List.map(loop, dec_array(el)));
			case (9, ::(op, ::(VBool(f), ::(e, [])))): EUnop(decode_unop(op), if (f) {
				Postfix;
			} else {
				Prefix;
			}, loop(e));
			case (10, ::(vl, [])): EVars(List.map(function v: (new Tuple(dec_string(field(v, "name")), opt(decode_ctype, field(v,
													  "type")), opt(loop, field(v, "expr")))), dec_array(vl)));
			case (11, ::(fname, ::(f, []))): EFunction(opt(dec_string, fname), decode_fun(f));
			case (12, ::(el, [])): EBlock(List.map(loop, dec_array(el)));
			case (13, ::(e1, ::(e2, []))): EFor(loop(e1), loop(e2));
			case (14, ::(e1, ::(e2, []))): EIn(loop(e1), loop(e2));
			case (15, ::(e1, ::(e2, ::(e3, [])))): EIf(loop(e1), loop(e2), opt(loop, e3));
			case (16, ::(e1, ::(e2, ::(VBool(flag), [])))): EWhile(loop(e1), loop(e2), if (flag) {
				NormalWhile;
			} else {
				DoWhile;
			});
			case (17, ::(e, ::(cases, ::(eo, [])))): var cases = List.map(function c: (new Tuple(List.map(loop, dec_array(field(c,
						"values"))), opt(loop, field(c, "guard")), opt(loop, field(c, "expr")))), dec_array(cases));
				ESwitch(loop(e), cases, opt(decode_null_expr, eo));
			case (18, ::(e, ::(catches, []))): var catches = List.map(function c: (new Tuple(dec_string(field(c, "name")),
						decode_ctype(field(c, "type")), loop(field(c, "expr")))), dec_array(catches));
				ETry(loop(e), catches);
			case (19, ::(e, [])): EReturn(opt(loop, e));
			case (20, []): EBreak;
			case (21, []): EContinue;
			case (22, ::(e, [])): EUntyped(loop(e));
			case (23, ::(e, [])): EThrow(loop(e));
			case (24, ::(e, ::(t, []))): ECast(loop(e), opt(decode_ctype, t));
			case (25, ::(e, ::(f, []))): EDisplay(loop(e), dec_bool(f));
			case (26, ::(t, [])): EDisplayNew(decode_path(t));
			case (27, ::(e1, ::(e2, ::(e3, [])))): ETernary(loop(e1), loop(e2), loop(e3));
			case (28, ::(e, ::(t, []))): ECheckType(loop(e), decode_ctype(t));
			case (29, ::(m, ::(e, []))): EMeta(decode_meta_entry(m), loop(e));
			case (30, ::(e, ::(f, []))): EField(loop(e), dec_string(f));
			case _: raise(Invalid_expr);
			};
		};
		try {
			loop(v);
		} catch (e: Stack_overflow) {
			raise(Invalid_expr);
		};
	};

	public static function decode_null_expr(v) return {
		switch (field(v, "expr")) {
		case VNull: None;
		case _: Some(decode_expr(v));
		};
	};

	public static function encode_ref(v, convert, tostr) return {
		enc_obj(::((new Tuple("get", VFunction(Fun0(function []: convert(v))))), ::((new Tuple("__string", VFunction(Fun0(function []: VString(tostr([])))))), ::((new Tuple("toString", VFunction(Fun0(function []: enc_string(tostr([])))))), ::((new Tuple("$", VAbstract(AUnsafe(Obj.repr(v))))), [])))));
	};

	public static function decode_ref(v) return {
		switch (field(v, "$")) {
		case VAbstract(AUnsafe(t)): Obj.obj(t);
		case _: raise(Invalid_expr);
		} : a;
	};

	public static function encode_pmap(convert, m) return {
		var h = Hashtbl.create(0);
		PMap.iter(function k: function v: Hashtbl.add(h, VString(k), convert(v)), m);
		enc_hash(h);
	};

	public static function encode_pmap_array(convert, m) return {
		var l = ref([]);
		PMap.iter(function _: function v: l.val = @(l.val, ::(convert(v), [])), m);
		enc_array(l.val);
	};

	public static function encode_array(convert, l) return {
		enc_array(List.map(convert, l));
	};

	public static function encode_meta(m, set) return {
		var meta = ref(m);
		enc_obj(::((new Tuple("get", VFunction(Fun0(function []: encode_meta_content(meta.val))))), ::((new Tuple("add", VFunction(Fun3(function k: function vl: function p:
		try {
			var el = List.map(decode_expr, dec_array(vl));
			meta.val = ::((new Tuple(MetaInfo.from_string(dec_string(k)), el, decode_pos(p))), meta.val);
			set(meta.val);
		} catch (e: Invalid_expr) {
			failwith("Invalid expression");
		};
		VNull)))), ::((new Tuple("extract", VFunction(Fun1(function k: var k = MetaInfo.from_string(try {
			dec_string(k);
		} catch (e: Invalid_expr) {
			raise(Builtin_error);
		});
		encode_array(encode_meta_entry, List.filter(function (m, _, _): = (m, k), meta.val)))))), ::((new Tuple("remove", VFunction(Fun1(function k: var k = MetaInfo.from_string(try {
			dec_string(k);
		} catch (e: Invalid_expr) {
			raise(Builtin_error);
		});
		meta.val = List.filter(function (m, _, _): <>(m, k), meta.val);
		set(meta.val);
		VNull)))), ::((new Tuple("has", VFunction(Fun1(function k: var k = MetaInfo.from_string(try {
			dec_string(k);
		} catch (e: Invalid_expr) {
			raise(Builtin_error);
		});
		VBool(List.exists(function (m, _, _): = (m, k), meta.val)))))), []))))));
	};

	public static function encode_mtype(t, fields) return {
		var i = t_infos(t);
		enc_obj(@(::((new Tuple("__t", VAbstract(ATDecl(t)))), ::((new Tuple("pack", enc_array(List.map(enc_string, fst(i.mt_path))))), ::((new Tuple("name", enc_string(snd(i.mt_path)))), ::((new Tuple("pos", encode_pos(i.mt_pos))), ::((new Tuple("module", enc_string(s_type_path(i.mt_module.m_path)))), ::((new Tuple("isPrivate", VBool(i.mt_private))), ::((new Tuple("meta", encode_meta(i.mt_meta, function m: i.mt_meta = m))), ::((new Tuple("doc", null(enc_string, i.mt_doc))), ::((new Tuple("params", encode_type_params(i.mt_params))), []))))))))), fields));
	};

	public static function encode_type_params(tl) return {
		enc_array(List.map(function (n, t): enc_obj(::((new Tuple("name", enc_string(n))), ::((new Tuple("t", encode_type(t))), []))), tl));
	};

	public static function encode_tenum(e) return {
		encode_mtype(TEnumDecl(e), ::((new Tuple("isExtern", VBool(e.e_extern))), ::((new Tuple("exclude", VFunction(Fun0(function []: e.e_extern = True;
		VNull)))), ::((new Tuple("constructs", encode_pmap(encode_efield, e.e_constrs))), ::((new Tuple("names", enc_array(List.map(enc_string, e.e_names)))), [])))));
	};

	public static function encode_tabstract(a) return {
		encode_mtype(TAbstractDecl(a), ::((new Tuple("type", encode_type(a.a_this))), ::((new Tuple("impl", switch (a.a_impl) {
	case None: VNull;
	case Some(c): encode_clref(c);
		})), ::((new Tuple("binops", enc_array(List.map(function (op, cf): enc_obj(::((new Tuple("op", encode_binop(op))), ::((new Tuple("field", encode_cfield(cf))), []))), a.a_ops)))), ::((new Tuple("unops", enc_array(List.map(function (op, postfix, cf): enc_obj(::((new Tuple("op", encode_unop(op))), ::((new Tuple("isPostfix", VBool(switch (postfix) {
	case Postfix: True;
	case Prefix: False;
	}))), ::((new Tuple("field", encode_cfield(cf))), [])))), a.a_unops)))), ::((new Tuple("from", enc_array(@(List.map(function t: enc_obj(::((new Tuple("t", encode_type(t))), ::((new Tuple("field", VNull)), []))), a.a_from), List.map(function (t, cf): enc_obj(::((new Tuple("t", encode_type(t))), ::((new Tuple("field", encode_cfield(cf))), []))), a.a_from_field))))), ::((new Tuple("to", enc_array(@(List.map(function t: enc_obj(::((new Tuple("t", encode_type(t))), ::((new Tuple("field", VNull)), []))), a.a_to), List.map(function (t, cf): enc_obj(::((new Tuple("t", encode_type(t))), ::((new Tuple("field", encode_cfield(cf))), []))), a.a_to_field))))), ::((new Tuple("array", enc_array(List.map(encode_cfield, a.a_array)))), ::((new Tuple("resolve", switch (a.a_resolve) {
	case None: VNull;
	case Some(cf): encode_cfref(cf);
		})), [])))))))));
	};

	public static function encode_efield(f) return {
		enc_obj(::((new Tuple("name", enc_string(f.ef_name))), ::((new Tuple("type", encode_type(f.ef_type))), ::((new Tuple("pos", encode_pos(f.ef_pos))), ::((new Tuple("index", VInt(f.ef_index))), ::((new Tuple("meta", encode_meta(f.ef_meta, function m: f.ef_meta = m))), ::((new Tuple("doc", null(enc_string, f.ef_doc))), ::((new Tuple("params", encode_type_params(f.ef_params))), []))))))));
	};

	public static function encode_cfield(f) return {
		enc_obj(::((new Tuple("name", enc_string(f.cf_name))), ::((new Tuple("type", switch (f.cf_kind) {
	case Method(_): encode_lazy_type(f.cf_type);
		case _: encode_type(f.cf_type);
		})), ::((new Tuple("isPublic", VBool(f.cf_public))), ::((new Tuple("params", encode_type_params(f.cf_params))), ::((new Tuple("meta", encode_meta(f.cf_meta, function m: f.cf_meta = m))), ::((new Tuple("expr", VFunction(Fun0(function []: ignore(follow(f.cf_type));
		switch (f.cf_expr) {
	case None: VNull;
	case Some(e): encode_texpr(e);
		})))), ::((new Tuple("kind", encode_field_kind(f.cf_kind))), ::((new Tuple("pos", encode_pos(f.cf_pos))), ::((new Tuple("doc", null(enc_string, f.cf_doc))), ::((new Tuple("overloads", encode_ref(f.cf_overloads, encode_array(encode_cfield), function []: "overloads"))), [])))))))))));
	};

	public static function encode_field_kind(k) return {
		var Tuple(tag, pl) = switch (k) {
		case Type.Var(v): (new Tuple(0, ::(encode_var_access(v.v_read), ::(encode_var_access(v.v_write), []))));
		case Method(m): (new Tuple(1, ::(encode_method_kind(m), [])));
		};
		enc_enum(IFieldKind, tag, pl);
	};

	public static function encode_var_access(a) return {
		var Tuple(tag, pl) = switch (a) {
		case AccNormal: (new Tuple(0, []));
		case AccNo: (new Tuple(1, []));
		case AccNever: (new Tuple(2, []));
		case AccResolve: (new Tuple(3, []));
		case AccCall: (new Tuple(4, []));
		case AccInline: (new Tuple(5, []));
		case AccRequire(s, msg): (new Tuple(6, ::(enc_string(s), ::(null(enc_string, msg), []))));
		};
		enc_enum(IVarAccess, tag, pl);
	};

	public static function encode_method_kind(m) return {
		var Tuple(tag, pl) = switch (m) {
		case MethNormal: (new Tuple(0, []));
		case MethInline: (new Tuple(1, []));
		case MethDynamic: (new Tuple(2, []));
		case MethMacro: (new Tuple(3, []));
		};
		enc_enum(IMethodKind, tag, pl);
	};

	public static function encode_class_kind(k) return {
		var Tuple(tag, pl) = switch (k) {
		case KNormal: (new Tuple(0, []));
		case KTypeParameter(pl): (new Tuple(1, ::(encode_tparams(pl), [])));
		case KExtension(cl, params): (new Tuple(2, ::(encode_clref(cl), ::(encode_tparams(params), []))));
		case KExpr(e): (new Tuple(3, ::(encode_expr(e), [])));
		case KGeneric: (new Tuple(4, []));
		case KGenericInstance(cl, params): (new Tuple(5, ::(encode_clref(cl), ::(encode_tparams(params), []))));
		case KMacroType: (new Tuple(6, []));
		case KAbstractImpl(a): (new Tuple(7, ::(encode_abref(a), [])));
		case KGenericBuild(cfl): (new Tuple(8, []));
		};
		enc_enum(IClassKind, tag, pl);
	};

	public static function encode_tclass(c) return {
		ignore(c.cl_build([]));
		encode_mtype(TClassDecl(c), ::((new Tuple("kind", encode_class_kind(c.cl_kind))), ::((new Tuple("isExtern", VBool(c.cl_extern))), ::((new Tuple("exclude", VFunction(Fun0(function []: c.cl_extern = True;
		c.cl_init = None;
		VNull)))), ::((new Tuple("isInterface", VBool(c.cl_interface))), ::((new Tuple("superClass", switch (c.cl_super) {
	case None: VNull;
	case Some(c, pl): enc_obj(::((new Tuple("t", encode_clref(c))), ::((new Tuple("params", encode_tparams(pl))), [])));
		})), ::((new Tuple("interfaces", enc_array(List.map(function (c, pl): enc_obj(::((new Tuple("t", encode_clref(c))), ::((new Tuple("params", encode_tparams(pl))), []))), c.cl_implements)))), ::((new Tuple("fields", encode_ref(c.cl_ordered_fields, encode_array(encode_cfield), function []: "class fields"))), ::((new Tuple("statics", encode_ref(c.cl_ordered_statics, encode_array(encode_cfield), function []: "class fields"))), ::((new Tuple("constructor", switch (c.cl_constructor) {
	case None: VNull;
	case Some(cf): encode_cfref(cf);
		})), ::((new Tuple("init", switch (c.cl_init) {
	case None: VNull;
	case Some(e): encode_texpr(e);
		})), ::((new Tuple("overrides", enc_array(List.map(encode_cfref, c.cl_overrides)))), []))))))))))));
	};

	public static function encode_ttype(t) return {
		encode_mtype(TTypeDecl(t), ::((new Tuple("isExtern", VBool(False))), ::((new Tuple("exclude", VFunction(Fun0(function []: VNull)))), ::((new Tuple("type", encode_type(t.t_type))), []))));
	};

	public static function encode_tanon(a) return {
		enc_obj(::((new Tuple("fields", encode_pmap_array(encode_cfield, a.a_fields))), ::((new Tuple("status", encode_anon_status(a.a_status.val))), [])));
	};

	public static function encode_anon_status(s) return {
		var Tuple(tag, pl) = switch (s) {
		case Closed: (new Tuple(0, []));
		case Opened: (new Tuple(1, []));
		case Type.Const: (new Tuple(2, []));
		case Extend(tl): (new Tuple(3, ::(encode_ref(tl, function tl: enc_array(List.map(encode_type, tl)),
			function []: "<extended types>"), [])));
		case Statics(cl): (new Tuple(4, ::(encode_clref(cl), [])));
		case EnumStatics(en): (new Tuple(5, ::(encode_enref(en), [])));
		case AbstractStatics(ab): (new Tuple(6, ::(encode_abref(ab), [])));
		};
		enc_enum(IAnonStatus, tag, pl);
	};

	public static function encode_tparams(pl) return {
		enc_array(List.map(encode_type, pl));
	};

	public static function encode_clref(c) return {
		encode_ref(c, encode_tclass, function []: s_type_path(c.cl_path));
	};

	public static function encode_enref(en) return {
		encode_ref(en, encode_tenum, function []: s_type_path(en.e_path));
	};

	public static function encode_cfref(cf) return {
		encode_ref(cf, encode_cfield, function []: cf.cf_name);
	};

	public static function encode_abref(ab) return {
		encode_ref(ab, encode_tabstract, function []: s_type_path(ab.a_path));
	};

	public static function encode_type(t) return {
		function loop(match) return switch (match) {
		case TMono(r): switch (r.val) {
			case None: (new Tuple(0, ::(encode_ref(r, function r: switch (r.val) {
			case None: VNull;
			case Some(t): encode_type(t);
				}, function []: "<mono>"), [])));
			case Some(t): loop(t);
			};
		case TEnum(e, pl): (new Tuple(1, ::(encode_ref(e, encode_tenum, function []: s_type_path(e.e_path)),
												::(encode_tparams(pl), []))));
		case TInst(c, pl): (new Tuple(2, ::(encode_clref(c), ::(encode_tparams(pl), []))));
		case TType(t, pl): (new Tuple(3, ::(encode_ref(t, encode_ttype, function []: s_type_path(t.t_path)),
												::(encode_tparams(pl), []))));
		case TFun(pl, ret): var pl = List.map(function (n, o, t): enc_obj(::((new Tuple("name", enc_string(n))),
												  ::((new Tuple("opt", VBool(o))), ::((new Tuple("t", encode_type(t))), [])))), pl);
			(new Tuple(4, ::(enc_array(pl), ::(encode_type(ret), []))));
		case TAnon(a): (new Tuple(5, ::(encode_ref(a, encode_tanon, function []: "<anonymous>"), [])));
		case TDynamic(tsub) = t: if ( == (t, t_dynamic)) {
				(new Tuple(6, ::(VNull, [])));
			} else {
				(new Tuple(6, ::(encode_type(tsub), [])));
			};
		case TLazy(f): loop(f.val([]));
		case TAbstract(a, pl): (new Tuple(8, ::(encode_abref(a), ::(encode_tparams(pl), []))));
		};
		var Tuple(tag, pl) = loop(t);
		enc_enum(IType, tag, pl);
	};

	public static function encode_lazy_type(t) return {
		function loop(match) return switch (match) {
		case TMono(r): switch (r.val) {
			case Some(t): loop(t);
			case _: encode_type(t);
			};
		case TLazy(f): enc_enum(IType, 7, ::(VAbstract(ALazyType(f)), []));
		case _: encode_type(t);
		};
		loop(t);
	};

	public static function decode_type(t) return {
		switch (decode_enum(t)) {
		case (0, ::(r, [])): TMono(decode_ref(r));
		case (1, ::(e, ::(pl, []))): TEnum(decode_ref(e), List.map(decode_type, dec_array(pl)));
		case (2, ::(c, ::(pl, []))): TInst(decode_ref(c), List.map(decode_type, dec_array(pl)));
		case (3, ::(t, ::(pl, []))): TType(decode_ref(t), List.map(decode_type, dec_array(pl)));
		case (4, ::(pl, ::(r, []))): TFun(List.map(function p: (new Tuple(dec_string(field(p, "name")), dec_bool(field(p, "opt")),
			decode_type(field(p, "t")))), dec_array(pl)), decode_type(r));
		case (5, ::(a, [])): TAnon(decode_ref(a));
		case (6, ::(VNull, [])): t_dynamic;
		case (6, ::(t, [])): TDynamic(decode_type(t));
		case (7, ::(VAbstract(ALazyType(f)), [])): TLazy(f);
		case (8, ::(a, ::(pl, []))): TAbstract(decode_ref(a), List.map(decode_type, dec_array(pl)));
		case _: raise(Invalid_expr);
		};
	};

	public static function decode_tdecl(v) return {
		switch (v) {
		case VObject(o): switch (get_field(o, hash("__t"))) {
			case VAbstract(ATDecl(t)): t;
			case _: raise(Invalid_expr);
			};
		case _: raise(Invalid_expr);
		};
	};

	public static function vopt(f, v) return {
		switch (v) {
		case None: VNull;
		case Some(v): f(v);
		};
	};

	public static function encode_tconst(c) return {
		var Tuple(tag, pl) = switch (c) {
		case TInt(i): (new Tuple(0, ::(best_int(i), [])));
		case TFloat(f): (new Tuple(1, ::(enc_string(f), [])));
		case TString(s): (new Tuple(2, ::(enc_string(s), [])));
		case TBool(b): (new Tuple(3, ::(VBool(b), [])));
		case TNull: (new Tuple(4, []));
		case TThis: (new Tuple(5, []));
		case TSuper: (new Tuple(6, []));
		};
		enc_enum(ITConstant, tag, pl);
	};

	public static function encode_tvar(v) return {
		function f_extra(Tuple(pl, e)) return {
			enc_obj(::((new Tuple("params", encode_type_params(pl))), ::((new Tuple("expr", vopt(encode_texpr, e))), [])));
		};
		enc_obj(::((new Tuple("id", VInt(v.v_id))), ::((new Tuple("name", enc_string(v.v_name))), ::((new Tuple("t", encode_type(v.v_type))), ::((new Tuple("capture", VBool(v.v_capture))), ::((new Tuple("extra", vopt(f_extra, v.v_extra))), ::((new Tuple("meta", encode_meta_content(v.v_meta))), ::((new Tuple("$", VAbstract(AUnsafe(Obj.repr(v))))), []))))))));
	};

	public static function encode_module_type(mt) return {
		var Tuple(tag, pl) = switch (mt) {
		case TClassDecl(c): (new Tuple(0, ::(encode_clref(c), [])));
		case TEnumDecl(e): (new Tuple(1, ::(encode_enref(e), [])));
		case TTypeDecl(t): (new Tuple(2, ::(encode_ref(t, encode_ttype, function []: s_type_path(t.t_path)), [])));
		case TAbstractDecl(a): (new Tuple(3, ::(encode_abref(a), [])));
		};
		enc_enum(IModuleType, tag, pl);
	};

	public static function encode_tfunc(func) return {
		enc_obj(::((new Tuple("args", enc_array(List.map(function (v, c): enc_obj(::((new Tuple("v", encode_tvar(v))), ::((new Tuple("value", switch (c) {
	case None: VNull;
	case Some(c): encode_tconst(c);
		})), []))), func.tf_args)))), ::((new Tuple("t", encode_type(func.tf_type))), ::((new Tuple("expr", encode_texpr(func.tf_expr))), []))));
	};

	public static function encode_field_access(fa) return {
		function encode_instance(c, tl) return {
			enc_obj(::((new Tuple("c", encode_clref(c))), ::((new Tuple("params", encode_tparams(tl))), [])));
		};
		var Tuple(tag, pl) = switch (fa) {
		case FInstance(c, tl, cf): (new Tuple(0, ::(encode_clref(c), ::(encode_tparams(tl), ::(encode_cfref(cf), [])))));
		case FStatic(c, cf): (new Tuple(1, ::(encode_clref(c), ::(encode_cfref(cf), []))));
		case FAnon(cf): (new Tuple(2, ::(encode_cfref(cf), [])));
		case FDynamic(s): (new Tuple(3, ::(enc_string(s), [])));
		case FClosure(co, cf): (new Tuple(4, ::(switch (co) {
		case Some(c, tl): encode_instance(c, tl);
			case None: VNull;
			}, ::(encode_cfref(cf), []))));
		case FEnum(en, ef): (new Tuple(5, ::(encode_enref(en), ::(encode_efield(ef), []))));
		};
		enc_enum(IFieldAccess, tag, pl);
	};

	public static function encode_texpr(e) return {
		function loop(e) return {
			var Tuple(tag, pl) = switch (e.eexpr) {
			case TConst(c): (new Tuple(0, ::(encode_tconst(c), [])));
			case TLocal(v): (new Tuple(1, ::(encode_tvar(v), [])));
			case TArray(e1, e2): (new Tuple(2, ::(loop(e1), ::(loop(e2), []))));
			case TBinop(op, e1, e2): (new Tuple(3, ::(encode_binop(op), ::(loop(e1), ::(loop(e2), [])))));
			case TField(e1, fa): (new Tuple(4, ::(loop(e1), ::(encode_field_access(fa), []))));
			case TTypeExpr(mt): (new Tuple(5, ::(encode_module_type(mt), [])));
			case TParenthesis(e1): (new Tuple(6, ::(loop(e1), [])));
			case TObjectDecl(fl): (new Tuple(7, ::(enc_array(List.map(function (f, e): enc_obj(::((new Tuple("name", enc_string(f))),
				::((new Tuple("expr", loop(e))), []))), fl)), [])));
			case TArrayDecl(el): (new Tuple(8, ::(encode_texpr_list(el), [])));
			case TCall(e1, el): (new Tuple(9, ::(loop(e1), ::(encode_texpr_list(el), []))));
			case TNew(c, pl, el): (new Tuple(10, ::(encode_clref(c), ::(encode_tparams(pl), ::(encode_texpr_list(el), [])))));
			case TUnop(op, flag, e1): (new Tuple(11, ::(encode_unop(op), ::(VBool( = (flag, Postfix)), ::(loop(e1), [])))));
			case TFunction(func): (new Tuple(12, ::(encode_tfunc(func), [])));
			case TVar(v, eo): (new Tuple(13, ::(encode_tvar(v), ::(vopt(encode_texpr, eo), []))));
			case TBlock(el): (new Tuple(14, ::(encode_texpr_list(el), [])));
			case TFor(v, e1, e2): (new Tuple(15, ::(encode_tvar(v), ::(loop(e1), ::(loop(e2), [])))));
			case TIf(eif, ethen, eelse): (new Tuple(16, ::(loop(eif), ::(loop(ethen), ::(vopt(encode_texpr, eelse), [])))));
			case TWhile(econd, e1, flag): (new Tuple(17, ::(loop(econd), ::(loop(e1), ::(VBool( = (flag, NormalWhile)), [])))));
			case TSwitch(e1, cases, edef): (new Tuple(18, ::(loop(e1), ::(enc_array(List.map(function (el,
				e): enc_obj(::((new Tuple("values", encode_texpr_list(el))), ::((new Tuple("expr", loop(e))), []))), cases)),
				::(vopt(encode_texpr, edef), [])))));
			case TTry(e1, catches): (new Tuple(19, ::(loop(e1), ::(enc_array(List.map(function (v, e): enc_obj(::((new Tuple("v",
				encode_tvar(v))), ::((new Tuple("expr", loop(e))), []))), catches)), []))));
			case TReturn(e1): (new Tuple(20, ::(vopt(encode_texpr, e1), [])));
			case TBreak: (new Tuple(21, []));
			case TContinue: (new Tuple(22, []));
			case TThrow(e1): (new Tuple(23, ::(loop(e1), [])));
			case TCast(e1, mt): (new Tuple(24, ::(loop(e1), ::(switch (mt) {
			case None: VNull;
			case Some(mt): encode_module_type(mt);
				}, []))));
			case TMeta(m, e1): (new Tuple(25, ::(encode_meta_entry(m), ::(loop(e1), []))));
			case TEnumParameter(e1, ef, i): (new Tuple(26, ::(loop(e1), ::(encode_efield(ef), ::(VInt(i), [])))));
			};
			enc_obj(::((new Tuple("pos", encode_pos(e.epos))), ::((new Tuple("expr", enc_enum(ITypedExpr, tag, pl))), ::((new Tuple("t", encode_type(e.etype))), []))));
		};
		loop(e);
	};

	public static function encode_texpr_list(el) return {
		enc_array(List.map(encode_texpr, el));
	};

	public static function decode_tconst(c) return {
		switch (decode_enum(c)) {
		case (0, ::(s, [])): TInt(switch (s) {
		case VInt(i): Int32.of_int(i);
			case VInt32(i): i;
			case _: raise(Invalid_expr);
			});
		case (1, ::(s, [])): TFloat(dec_string(s));
		case (2, ::(s, [])): TString(dec_string(s));
		case (3, ::(s, [])): TBool(dec_bool(s));
		case (4, []): TNull;
		case (5, []): TThis;
		case (6, []): TSuper;
		case _: raise(Invalid_expr);
		};
	};

	public static function decode_type_params(v) return {
		List.map(function v: (new Tuple(dec_string(field(v, "name")), decode_type(field(v, "t")))), dec_array(v));
	};

	public static function decode_tvar(v) return {
		switch (field(v, "$")) {
		case VAbstract(AUnsafe(t)): Obj.obj(t);
		case _: raise(Invalid_expr);
		};
	};

	public static function decode_var_access(v) return {
		switch (decode_enum(v)) {
		case (0, []): AccNormal;
		case (1, []): AccNo;
		case (2, []): AccNever;
		case (3, []): AccResolve;
		case (4, []): AccCall;
		case (5, []): AccInline;
		case (6, ::(s1, ::(s2, []))): AccRequire(dec_string(s1), opt(dec_string, s2));
		case _: raise(Invalid_expr);
		};
	};

	public static function decode_method_kind(v) return {
		switch (decode_enum(v)) {
		case (0, []): MethNormal;
		case (1, []): MethInline;
		case (2, []): MethDynamic;
		case (3, []): MethMacro;
		case _: raise(Invalid_expr);
		};
	};

	public static function decode_field_kind(v) return {
		switch (decode_enum(v)) {
		case (0, ::(vr, ::(vw, []))): Type.Var({
				() with v_read = decode_var_access(vr);
				v_write = decode_var_access(vw)
			});
		case (1, ::(m, [])): Method(decode_method_kind(m));
		case _: raise(Invalid_expr);
		};
	};

	public static function decode_cfield(v) return {
		{
			() with cf_name = dec_string(field(v, "name"));
			cf_type = decode_type(field(v, "type"));
			cf_public = dec_bool(field(v, "isPublic"));
			cf_pos = decode_pos(field(v, "pos"));
			cf_doc = opt(dec_string, field(v, "doc"));
			cf_meta = [];
			cf_kind = decode_field_kind(field(v, "kind"));
			cf_params = decode_type_params(field(v, "params"));
			cf_expr = None;
			cf_overloads = decode_ref(field(v, "overloads"))
		};
	};

	public static function decode_efield(v) return {
		{
			() with ef_name = dec_string(field(v, "name"));
			ef_type = decode_type(field(v, "type"));
			ef_pos = decode_pos(field(v, "pos"));
			ef_index = switch (field(v, "index")) {
			case VInt(i): i;
			case _: raise(Invalid_expr);
			};
			ef_meta = [];
			ef_doc = opt(dec_string, field(v, "doc"));
			ef_params = decode_type_params(field(v, "params"))
		};
	};

	public static function decode_field_access(v) return {
		switch (decode_enum(v)) {
		case (0, ::(c, ::(tl, ::(cf, [])))): var c = decode_ref(c);
			FInstance(c, List.map(decode_type, dec_array(tl)), decode_ref(cf));
		case (1, ::(c, ::(cf, []))): FStatic(decode_ref(c), decode_ref(cf));
		case (2, ::(cf, [])): FAnon(decode_ref(cf));
		case (3, ::(s, [])): FDynamic(dec_string(s));
		case (4, ::(co, ::(cf, []))): var co = switch (co) {
			case VNull: None;
			case _: Some(decode_ref(field(co, "c")), List.map(decode_type, dec_array(field(co, "params"))));
			};
			FClosure(co, decode_ref(cf));
		case (5, ::(e, ::(ef, []))): FEnum(decode_ref(e), decode_efield(ef));
		case _: raise(Invalid_expr);
		};
	};

	public static function decode_module_type(v) return {
		switch (decode_enum(v)) {
		case (0, ::(c, [])): TClassDecl(decode_ref(c));
		case (1, ::(en, [])): TEnumDecl(decode_ref(en));
		case (2, ::(t, [])): TTypeDecl(decode_ref(t));
		case (3, ::(a, [])): TAbstractDecl(decode_ref(a));
		case _: raise(Invalid_expr);
		};
	};

	public static function decode_tfunc(v) return {
		{
			() with tf_args = List.map(function v: (new Tuple(decode_tvar(field(v, "v")), opt(decode_tconst, field(v, "value")))), dec_array(field(v, "args")));
			tf_type = decode_type(field(v, "t"));
			tf_expr = decode_texpr(field(v, "expr"))
		};
	};

	public static function decode_texpr(v) return {
		function loop(v) return {
			mk(decode(field(v, "expr")), decode_type(field(v, "t")), decode_pos(field(v, "pos")));
		};
		function decode(e) return {
			switch (decode_enum(e)) {
			case (0, ::(c, [])): TConst(decode_tconst(c));
			case (1, ::(v, [])): TLocal(decode_tvar(v));
			case (2, ::(v1, ::(v2, []))): TArray(loop(v1), loop(v2));
			case (3, ::(op, ::(v1, ::(v2, [])))): TBinop(decode_op(op), loop(v1), loop(v2));
			case (4, ::(v1, ::(fa, []))): TField(loop(v1), decode_field_access(fa));
			case (5, ::(mt, [])): TTypeExpr(decode_module_type(mt));
			case (6, ::(v1, [])): TParenthesis(loop(v1));
			case (7, ::(v, [])): TObjectDecl(List.map(function v: (new Tuple(dec_string(field(v, "name")), loop(field(v, "expr")))),
				dec_array(v)));
			case (8, ::(vl, [])): TArrayDecl(List.map(loop, dec_array(vl)));
			case (9, ::(v1, ::(vl, []))): TCall(loop(v1), List.map(loop, dec_array(vl)));
			case (10, ::(c, ::(tl, ::(vl, [])))): TNew(decode_ref(c), List.map(decode_type, dec_array(tl)), List.map(loop,
				dec_array(vl)));
			case (11, ::(op, ::(pf, ::(v1, [])))): TUnop(decode_unop(op), if (dec_bool(pf)) {
				Postfix;
			} else {
				Prefix;
			}, loop(v1));
			case (12, ::(f, [])): TFunction(decode_tfunc(f));
			case (13, ::(v, ::(eo, []))): TVar(decode_tvar(v), opt(loop, eo));
			case (14, ::(vl, [])): TBlock(List.map(loop, dec_array(vl)));
			case (15, ::(v, ::(v1, ::(v2, [])))): TFor(decode_tvar(v), loop(v1), loop(v2));
			case (16, ::(vif, ::(vthen, ::(velse, [])))): TIf(loop(vif), loop(vthen), opt(loop, velse));
			case (17, ::(vcond, ::(v1, ::(b, [])))): TWhile(loop(vcond), loop(v1), if (dec_bool(b)) {
				NormalWhile;
			} else {
				DoWhile;
			});
			case (18, ::(v1, ::(cl, ::(vdef, [])))): TSwitch(loop(v1), List.map(function v: (new Tuple(List.map(loop,
						dec_array(field(v, "values"))), loop(field(v, "expr")))), dec_array(cl)), opt(loop, vdef));
			case (19, ::(v1, ::(cl, []))): TTry(loop(v1), List.map(function v: (new Tuple(decode_tvar(field(v, "v")), loop(field(v,
													"expr")))), dec_array(cl)));
			case (20, ::(vo, [])): TReturn(opt(loop, vo));
			case (21, []): TBreak;
			case (22, []): TContinue;
			case (23, ::(v1, [])): TThrow(loop(v1));
			case (24, ::(v1, ::(mto, []))): TCast(loop(v1), opt(decode_module_type, mto));
			case (25, ::(m, ::(v1, []))): TMeta(decode_meta_entry(m), loop(v1));
			case (26, ::(v1, ::(ef, ::(i, [])))): TEnumParameter(loop(v1), decode_efield(ef), switch (i) {
			case VInt(i): i;
				case _: raise(Invalid_expr);
				});
			case (i, el): Printf.printf("%i %i\n", i, List.length(el));
				raise(Invalid_expr);
			};
		};
		try {
			loop(v);
		} catch (e: Stack_overflow) {
			raise(Invalid_expr);
		};
	};

	public static function decode_type_def(v) return {
		var pack = List.map(dec_string, dec_array(field(v, "pack")));
		var name = dec_string(field(v, "name"));
		var meta = decode_meta_content(field(v, "meta"));
		var pos = decode_pos(field(v, "pos"));
		var isExtern = switch (field(v, "isExtern")) {
		case VNull: False;
		case v: dec_bool(v);
		};
		var fields = List.map(decode_field, dec_array(field(v, "fields")));
		function mk(fl, dl) return {
			{
				() with d_name = name;
				d_doc = None;
				d_params = decode_tparams(field(v, "params"));
				d_meta = meta;
				d_flags = fl;
				d_data = dl
			};
		};
		var tdef = switch (decode_enum(field(v, "kind"))) {
		case (0, []): 	function conv(f) return {
				function loop(Tuple(n, opt, t, _)) return {
					switch (t) {
					case None: raise(Invalid_expr);
					case Some(t): (new Tuple(n, opt, t));
					};
				};
				var Tuple(args, params, t) = switch (f.cff_kind) {
				case FVar(t, None): (new Tuple([], [], t));
				case FFun(f): (new Tuple(List.map(loop, f.f_args), f.f_params, f.f_type));
				case _: raise(Invalid_expr);
				};
				{
					() with ec_name = f.cff_name;
					ec_doc = f.cff_doc;
					ec_meta = f.cff_meta;
					ec_pos = f.cff_pos;
					ec_args = args;
					ec_params = params;
					ec_type = t
				};
			};
			EEnum(mk(if (isExtern) {
			::(EExtern, []);
			} else {
				[];
			}, List.map(conv, fields)));
		case (1, []): ETypedef(mk(if (isExtern) {
			::(EExtern, []);
			} else {
				[];
			}, CTAnonymous(fields)));
		case (2, ::(ext, ::(impl, ::(interf, [])))): var flags = if (isExtern) {
				::(HExtern, []);
			} else {
				[];
			};
			var flags = switch (interf) {
			case VNull | VBool(False): flags;
			case VBool(True): ::(HInterface, flags);
			case _: raise(Invalid_expr);
			};
			var flags = switch (opt(decode_path, ext)) {
			case None: flags;
			case Some(t): ::(HExtends(t), flags);
			};
			var flags = switch (opt(function v: List.map(decode_path, dec_array(v)), impl)) {
			case None: flags;
			case Some(l): @(List.map(function t: HImplements(t), l), flags);
			};
			EClass(mk(flags, fields));
		case (3, ::(t, [])): ETypedef(mk(if (isExtern) {
			::(EExtern, []);
			} else {
				[];
			}, decode_ctype(t)));
		case (4, ::(tthis, ::(tfrom, ::(tto, [])))): var flags = switch (opt(dec_array, tfrom)) {
			case None: [];
			case Some(ta): List.map(function t: AFromType(decode_ctype(t)), ta);
			};
			var flags = switch (opt(dec_array, tto)) {
			case None: flags;
			case Some(ta): @(List.map(function t: AToType(decode_ctype(t)), ta), flags);
			};
			var flags = switch (opt(decode_ctype, tthis)) {
			case None: flags;
			case Some(t): ::(AIsType(t), flags);
			};
			EAbstract(mk(flags, fields));
		case _: raise(Invalid_expr);
		};
		var Tuple(pack, name) = switch (List.rev(pack)) {
		case ::(last, l) if (!(is_lower_ident(last))): (new Tuple(List.rev(l), last));
		case _: (new Tuple(pack, name));
		};
		(new Tuple((new Tuple(pack, name)), tdef, pos));
	};

	public static function make_const(e) return {
		switch (e.eexpr) {
		case TConst(c): switch (c) {
			case TInt(i): best_int(i);
			case TFloat(s): VFloat(float_of_string(s));
			case TString(s): enc_string(s);
			case TBool(b): VBool(b);
			case TNull: VNull;
			case TThis | TSuper: raise(Exit);
			};
		case TParenthesis(e) | TMeta(_, e): make_const(e);
		case TObjectDecl(el): VObject(obj(hash_field(get_ctx([])), List.map(function (f, e): (new Tuple(f, make_const(e))), el)));
		case TArrayDecl(al): enc_array(List.map(make_const, al));
		case _: raise(Exit);
		};
	};

	public static function __init__() {
		encode_complex_type_ref.val = encode_ctype;
		enc_array_ref.val = enc_array;
		dec_array_ref.val = dec_array;
		encode_type_ref.val = encode_type;
		decode_type_ref.val = decode_type;
		encode_expr_ref.val = encode_expr;
		decode_expr_ref.val = decode_expr;
		encode_clref_ref.val = encode_clref;
		enc_string_ref.val = enc_string;
		enc_hash_ref.val = enc_hash;
		encode_texpr_ref.val = encode_texpr;
		decode_texpr_ref.val = decode_texpr;
		encode_tvar_ref.val = encode_tvar;
		decode_path_ref.val = decode_path;
		encode_import_ref.val = encode_import;
		decode_import_ref.val = decode_import;
		eval_expr_ref.val = eval_expr;
		encode_import_ref.val = encode_import;
	}
}
;
